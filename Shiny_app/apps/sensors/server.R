

###################################
### Prep variables for server use

# Work from a copy of the database
app_data <- app_db

# Columns to remove from the database (not needed in the app)
drop_cols <- c('google_id', 'addit_contact_email', 'addit_contact_person', 'author_email', 'author_orcid_id',
               'author_PersonName', 'coarse_tot', 'curator_email', 'curator_organization', 'curator_PersonName', 'data_file',
               'experiments', 'gradient', 'header_row', 'key_version', 'location_name', 'merge_align', 'modification_date',
               'NA_1', 'NA_2', 'network', 'site_code', 'time_series', 'sample_collector') 

######################################
### SERVER STARTS HERE ###
function(input, output, session) {

 
### Sensor tab map starts here  
  output$sensormap <- renderLeaflet({
    leaflet() %>% 
      
      # Add basemap options
      addProviderTiles("Stamen.Terrain", group = "Stamen.Terrain") %>%
      addProviderTiles("Esri.WorldTopoMap", group = "Esri.WorldTopoMap") %>%     
      addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery",
                       options = providerTileOptions(opacity = 1)) %>%
      addProviderTiles("Stamen.Toner", group = "Stamen.Toner") %>%
      addProviderTiles("Esri.WorldStreetMap", group = "Esri.WorldStreetMap") %>%
      
      # Add basemap layers control options
      addLayersControl(
        baseGroups = c(
          "Esri.WorldTopoMap",
          "Esri.WorldImagery", 
          "Stamen.Terrain", 
          "Stamen.Toner",
          "Esri.WorldStreetMap"#, 
        ),
        position = "topleft",
        options = layersControlOptions(collapsed = TRUE, autoZIndex = FALSE)) %>%
      setView(lng = -116.75, lat = 43.16, zoom = 10) #Set the default map location
      
      ### Base map elements to start with... %>%
      

  })
  

### Sensor graphs start here
  
  # Get sensor filenames based on group
  senID_choices <- reactive({
    if(input$sensor_site == "Reynolds Creek") {
      sensor_file_names <- gsub(".dat", "", list.files("./data/Sensors"))
      sensor_file_names[grepl("RC", sensor_file_names)] 
    } #else if...for other sensor groups
  })
  
  #When sensor group changes, update sensor file dropdown options
  observe({
    updateSelectInput(session, "sensor_ID1", choices = senID_choices(), selected = senID_choices()[1])
    updateSelectInput(session, "sensor_ID2", choices = senID_choices(), selected = senID_choices()[1])
  })
  
  # Get file path for selected sensor 1
  sensor_file1 <- reactive({
    #c(input$sensor_ID1, input$sensor_ID2)
    if(input$sensor_ID1 == "") {
      paste0("./data/Sensors/", senID_choices()[1], ".dat")
    } else {
      paste0("./data/Sensors/", input$sensor_ID1, ".dat")
    }
  })
  
  # Get file path for selected sensor 2
  sensor_file2 <- reactive({
    if(input$sensor_ID2 == "") {
      paste0("./data/Sensors/", senID_choices()[1], ".dat")
    } else {
      paste0("./data/Sensors/", input$sensor_ID2, ".dat")
    }
  })
  
  # Load data from selected sensor 1
  sensor1_df <- reactive({
    
    # UNIQUE FORMATTING BY SENSOR GROUP
    if(input$sensor_site == "Reynolds Creek") {
    df <- read.table(sensor_file1(), sep=",", skip=1, fill=T, header=T, na.strings = c("NAN", "NA"))
    df <- df %>% distinct()
    df$TIMESTAMP <- as.POSIXct(df$TIMESTAMP, format = "%Y-%m-%d %H:%M:%OS", optional=T)
    df <- df %>% filter(!is.na(TIMESTAMP)) %>% arrange(TIMESTAMP)
    df <- df %>% mutate_at(c(2:ncol(df)), as.numeric)
    } #else if...for other sensor groups
    
    df
    }) 

  # Update analyte options for sensor 1
  observe({
    #input$sensor_ID1
    all_fields <- colnames(sensor1_df())
    fields <- c(all_fields[3], all_fields[grepl("Avg", all_fields)])
    
    updateSelectInput(session, "sensor1_analyte1", choices = fields, selected = fields[5])
    updateSelectInput(session, "sensor1_analyte2", choices = c("None",fields), selected = "None")
    updateSelectInput(session, "sensor1_analyte3", choices = c("None",fields), selected = "None")
  })
  
  # Load data from selected sensor 2
  sensor2_df <- reactive({
    
    # UNIQUE FORMATTING BY SENSOR GROUP
    if(input$sensor_site == "Reynolds Creek") {
      df <- read.table(sensor_file2(), sep=",", skip=1, fill=T, header=T, na.strings = c("NAN", "NA"))
      df <- df %>% distinct()
      df$TIMESTAMP <- as.POSIXct(df$TIMESTAMP, format = "%Y-%m-%d %H:%M:%OS", optional=T)
      df <- df %>% filter(!is.na(TIMESTAMP)) %>% arrange(TIMESTAMP)
      df <- df %>% mutate_at(c(2:ncol(df)), as.numeric)
    } #else if...for other sensor groups
    
    df
  }) 
  
  # Update analyte options for sensor 1
  observe({
    #input$sensor_ID2
    all_fields <- colnames(sensor2_df())
    fields <- c(all_fields[3], all_fields[grepl("Avg", all_fields)])
    
    updateSelectInput(session, "sensor2_analyte1", choices = fields, selected = fields[8])
    updateSelectInput(session, "sensor2_analyte2", choices = c("None",fields), selected = "None")
    updateSelectInput(session, "sensor2_analyte3", choices = c("None",fields), selected = "None")
  })

  #Create sensor plot
  output$plot_sens1 <- renderPlotly({
    df <- sensor1_df()
    fig <- plot_ly(type = 'scatter', mode = 'lines+markers')
    fig <- fig %>% add_trace(x = df[,'TIMESTAMP'], y = df[,input$sensor1_analyte1], name = input$sensor1_analyte1, 
                             fill="tozeroy", fillcolor='rgba(26,150,65,0.5)',               
                             line = list(color = 'rgba(26,150,65,0.7)', width = 2),
                             marker = list(color = 'rgba(26,150,65,0.8)', size = 3))
    
    if(input$sensor1_analyte2 != "None"){
      fig <- fig %>% add_trace(x = df[,'TIMESTAMP'], y = df[,input$sensor1_analyte2], name = input$sensor1_analyte2, 
                               fill="tozeroy", fillcolor='rgba(16,110,25,0.5)',               
                               line = list(color = 'rgba(16,110,25,0.7)', width = 2),
                               marker = list(color = 'rgba(16,110,25,0.8)', size = 5))
    }
    
    if(input$sensor1_analyte3 != "None"){
      fig <- fig %>% add_trace(x = df[,'TIMESTAMP'], y = df[,input$sensor1_analyte3], name = input$sensor1_analyte3, 
                               fill="tozeroy", fillcolor='rgba(8,50,5,0.5)',               
                               line = list(color = 'rgba(8,50,5,0.7)', width = 2),
                               marker = list(color = 'rgba(8,50,5,0.8)', size = 3))
    }
    
    fig <- fig %>% layout(xaxis = list(title = 'Date',
                                       rangeslider = list(type = "date", thickness=0.1)),
                          yaxis = list(title = '')) #Y-Axis Label
    fig
    })
  
  output$plot_sens2 <- renderPlotly({  
    df <- sensor2_df()
    fig <- plot_ly(type = 'scatter', mode = 'lines+markers')
    fig <- fig %>% add_trace(x = df[,'TIMESTAMP'], y = df[,input$sensor2_analyte1], name = input$sensor2_analyte1, 
                             fill="tozeroy", fillcolor='rgba(26,150,185,0.5)',               
                             line = list(color = 'rgba(26,150,185,0.7)', width = 2),
                             marker = list(color = 'rgba(26,150,185,0.8)', size = 3))
    
    if(input$sensor2_analyte2 != "None"){
      fig <- fig %>% add_trace(x = df[,'TIMESTAMP'], y = df[,input$sensor2_analyte2], name = input$sensor2_analyte2, 
                               fill="tozeroy", fillcolor='rgba(16,110,145,0.5)',               
                               line = list(color = 'rgba(16,110,145,0.7)', width = 2),
                               marker = list(color = 'rgba(16,110,145,0.8)', size = 3))
    }
    
    if(input$sensor2_analyte3 != "None"){
      fig <- fig %>% add_trace(x = df[,'TIMESTAMP'], y = df[,input$sensor2_analyte3], name = input$sensor2_analyte3, 
                               fill="tozeroy", fillcolor='rgba(8,50,125,0.5)',               
                               line = list(color = 'rgba(8,50,125,0.7)', width = 2),
                               marker = list(color = 'rgba(8,50,125,0.8)', size = 3))
    }
    
    fig <- fig %>% layout(xaxis = list(title = 'Date',
                                       rangeslider = list(type = "date", thickness=0.1)),
                          yaxis = list(title = '')) #Y-Axis Label
    fig
  })
  
  
  ### Sensor DataTable
  
  # Get sensor filenames based on group
  sensor_tbl_choices <- reactive({
    if(input$sensor_tbl_group == "Reynolds Creek") {
      sensor_filenames[grepl("geomicro", sensor_filenames)]
    } #else if...for other sensor groups
  })
  
  # Update sensor file dropdown options
  observe({
    updateSelectInput(session, "sensor_tbl_ID", choices = sensor_tbl_choices(), selected = sensor_tbl_choices()[1])
  })
  
  # Change sensor file if plot 1 changes
  # observe({
  #   c(input$sensor_ID1)
  #   updateSelectInput(session, "sensor_tbl_ID", selected = input$sensor_ID1)
  #   })
  
  # Get file path for selected sensor 2
  sensor_tbl_file <- reactive({
    if(input$sensor_tbl_ID == "") {
      paste0("./data/Sensors/", sensor_tbl_choices()[1], ".dat")
    } else {
      paste0("./data/Sensors/", input$sensor_tbl_ID, ".dat")
    }
  })
  
  # Load data from selected sensor 2
  sensor_tbl_df <- reactive({
    
    # UNIQUE FORMATTING BY SENSOR GROUP
    if(input$sensor_site == "Reynolds Creek") {
      df <- read.table(sensor_tbl_file(), sep=",", skip=1, fill=T, header=T, na.strings = c("NAN", "NA"))
      df <- df %>% distinct()
      df$TIMESTAMP <- as.POSIXct(df$TIMESTAMP, format = "%Y-%m-%d %H:%M:%OS", optional=T)
      df <- df %>% filter(!is.na(TIMESTAMP)) %>% arrange(TIMESTAMP)
      df <- df %>% mutate_at(c(2:ncol(df)), as.numeric)
    } #else if...for other sensor groups
    
    df
  }) 
  
  # Create DataTable
  output$sensor_tbl <- DT::renderDataTable({    
    DT::datatable(sensor_tbl_df(), 
                  rownames = FALSE, 
                  options = list(dom = 't', 
                                 lengthMenu = c(50, 100, 500), 
                                 pageLength = 100), 
                  escape = FALSE, 
                  class = "display nowrap")
  })
  
  # Serve plot data to download button
  output$downloadSensor <- downloadHandler(
    filename = function() { 
      paste("RCrk_Sensor-",input$sensor_tbl_ID, ".csv", sep="")
    },
    content = function(file) {
      write.csv(sensor_tbl_df(), file, row.names = FALSE)
    }
  )
}

  