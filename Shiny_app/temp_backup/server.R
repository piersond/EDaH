

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

  
### PLOT EXPLORER STARTS HERE ###  
  observeEvent(c(input$plot_x, input$plot_y, input$plot_color), {
    
    # Make a smaller dataframe to speed up highchart render
    plot_df <- data.frame(uniqueID = app_data["uniqueID"],
                          lat = app_data["lat"],
                          long= app_data["long"],
                          x_data = app_data[input$plot_x],
                          y_data = app_data[input$plot_y],
                          col_data = app_data[input$plot_color])
    colnames(plot_df) <- c("uniqueID","lat", "long", "x_data", "y_data", "col_data")
    
    #Plotly plot
    output$chart1 <- renderPlotly({
      p1 <- ggplot(data=plot_df, aes(x=x_data, y=y_data, color=col_data)) +
              geom_point() +
              xlab(names(num_vars)[num_vars == input$plot_x]) +
              ylab(names(num_vars)[num_vars == input$plot_y]) +
              labs(color=names(num_vars)[num_vars == input$plot_color]) +
              scale_color_viridis(discrete=FALSE) +
              theme_minimal()
      
      p2 <- ggplotly(p1)
      p2
    })
    
    #Create plot datatable
    plot_dt_df <- plot_df %>% 
      mutate(Action = paste('<a class="go-map" href="" data-lat="', lat, '" data-long="', long, '" data-zip="', uniqueID, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, plot_dt_df, outputId = "ziptable")
    
    colnames(plot_dt_df) <- c("uniqueID","lat", "long", input$plot_x, input$plot_y, input$plot_color, "Action")
    
    output$plotTBL <- DT::renderDataTable({
      DT::datatable(plot_dt_df, 
                  options = list(ajax = list(url = action), 
                                 lengthMenu = c(25, 50, 100), 
                                 pageLength = 25,
                                 columnDefs = list(list(className = 'dt-center', targets = 0:4))), 
                  escape = FALSE, 
                  class = "display nowrap")
    })
    
    # Serve plot data to download button
    output$downloadPlotData <- downloadHandler(
      filename = function() { 
        paste("RCrk_plot_data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(plot_dt_df[,-ncol(plot_dt_df)], file, row.names = FALSE)
      }
    )
  })


  
## Interactive Map Starts Here ###
  
  # Create the Leaflet basemap and basemap options
  output$map <- renderLeaflet({
    leaflet() %>% 
      
      # Add basemap options
      addProviderTiles("Stamen.Terrain", group = "Stamen.Terrain") %>%
      addProviderTiles("Esri.WorldTopoMap", group = "Esri.WorldTopoMap") %>%     
      addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery",
                       options = providerTileOptions(opacity = 1)) %>%
      addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>% #Group = layer name
      addProviderTiles("Stamen.Toner", group = "Stamen.Toner") %>%
      addProviderTiles("Esri.WorldStreetMap", group = "Esri.WorldStreetMap") %>%
      #addProviderTiles("Wikimedia", group = "Wikimedia") %>%
      #addProviderTiles("CartoDB.Positron", group = "CartoDB.Positron") %>%
      
      # Add basemap layers control options
      addLayersControl(
        baseGroups = c(
          "Esri.WorldTopoMap",
          "Esri.WorldImagery", 
          "Stamen.Terrain", 
          "OpenStreetMap", 
          "Stamen.Toner",
          "Esri.WorldStreetMap"#, 
         # "Wikimedia", 
         # "CartoDB.Positron"
         ),
        position = "topleft",
        #overlayGroups = c(""),
        options = layersControlOptions(collapsed = TRUE, autoZIndex = FALSE)) %>%
        setView(lng = -93.4, lat = 35.5, zoom = 4) %>% #Set the default map location
  
  ### Base map elements to start with
      
      # Add RCrk Boundary
      # addPolygons(data=app_boundary,
      #             col = 'black',
      #             stroke = TRUE, 
      #             weight=3,
      #             opacity=1,
      #             fillOpacity = 0, 
      #             smoothFactor = 2,
      #             layerId = 'RCbnd') %>%
      
      # Add shapefile overlay of watershed boundaries
      # addPolygons(data=app_watersheds,
      #             group = "watersheds",
      #             col = '#120046',
      #             stroke = TRUE, 
      #             weight=2,
      #             opacity=1,
      #             fillColor= "grey90",
      #             fillOpacity = 0, 
      #             smoothFactor = 2) %>%
    
      # Add CZCN soil pit markers
      addMarkers(data=app_CZCN_pits, ~long, ~lat, 
                 icon = ~ favicons['pit'],
                 popup = ~as.character(location_name), 
                 label = ~as.character(L1),
                 group = "CZCN_pits") #%>%
      
      # # Add ARS met station points
      # addMarkers(data=app_met, ~long, ~lat, 
      #             icon = ~ favicons['met'],
      #             popup = ~as.character(Description), 
      #             label = ~as.character(StationID),
      #             group = "met_markers") %>%
      # 
      # # Add ARS stream weirs
      # addMarkers(data=app_weir, ~long, ~lat, 
      #             icon = ~ favicons['weir'],
      #             popup = ~as.character(Description), 
      #             label = ~as.character(StationID),
      #             group = "weir_markers")
  })
  
  
### Implement point popups ###
  
  # Create function to show a popup at the given point location
  showPointPopup <- function(uniqueID, lat, lng) {

    #Only proceed if a point is selected
    if (!is.null(uniqueID)){
      
      # Grab data for the selected point
      selectedPoint <- app_data[app_data$uniqueID == uniqueID,]
      
      # Create the content to show in the popup
      content <- as.character(tagList(
      # Create HTML tag list to display in popup
        tags$h4(if(input$point_var != 'none') {HTML(sprintf("%s = %s", # Use sprintf to display variable info as html text
                                                            input$point_var,
                                                            as.numeric(selectedPoint[input$point_var])))
          } else {
            "No analyte selected."
          }), 
        sprintf("Soil depth: %s - %s cm", selectedPoint$layer_top, selectedPoint$layer_bot),
        tags$br(),
        sprintf("Longitude: %s", selectedPoint$long),
        tags$br(),
        sprintf("Latitude: %s", selectedPoint$lat),
        tags$br(),
        sprintf("Data ID: %s", selectedPoint$uniqueID)
      ))
      
      # Add a popup element to the leaflet map object
      leafletProxy("map") %>% addPopups(lng, lat, content, layerId = uniqueID)
    }
  }

  # When map is clicked, show popup info
  observe({leafletProxy("map") %>% # Observe for a leaflet map event
            clearPopups() # Clear any existing popups
    
    # Grab the click event properties
    event <- input$map_shape_click
    
    # Do nothing is the click event data is NULL
    if (is.null(event))
      return()

    # Call popup info function, pass in the event properties 
    isolate({
      showPointPopup(event$id, event$lat, event$lng)
    })
  })

# Observe for table crosshair click
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.1
      zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      showPointPopup(zip, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })

  
### Data Explorer Table Starts Here ###  
  output$databaseTBL <- DT::renderDataTable({
    df <- app_data %>% 
      .[,setdiff(names(.),drop_cols)] %>%

    mutate(Action = paste('<a class="go-map" href="" data-lat="', lat, '" data-long="', long, '" data-zip="', uniqueID, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df, outputId = "ziptable")
    
    DT::datatable(df, 
                  options = list(ajax = list(url = action), 
                                 lengthMenu = c(10, 50, 100), 
                                 pageLength = 50,
                                 columnDefs = list(list(
                                   targets = 0:1, visible= TRUE)), 
                                 buttons = c('colvis'), dom = 'Bfrtip'),
                  extensions = 'Buttons', 
                  escape = FALSE, 
                  class = "display nowrap")
    })

  # Serve plot data to download button
  output$downloadDatabase <- downloadHandler(
    filename = function() { 
      paste("RCrk_database-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(app_data %>% .[,setdiff(names(.),drop_cols)], file, row.names = FALSE)
    }
  )
  
  
  ## Data Key ####################################################
  ## Var info summary tables
  # Location var info tbl
  var_loc.tbl <- var_info %>% filter(level == "location")
  output$var_info.loc = renderDT(
    var_loc.tbl,
    options = list(lengthChange = TRUE,
                   pageLength = 100),
    rownames= FALSE,
    class = 'white-space: nowrap'
  )
  
  # Profile var info tbl
  var_prof.tbl <- var_info %>% filter(level != "location")
  output$var_info.prof = renderDT(
    var_prof.tbl,
    options = list(lengthChange = TRUE,
                   pageLength = 200),
    rownames= FALSE,
    class = 'white-space: nowrap'
  )

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
      setView(lng = -93.4, lat = 30.0, zoom = 3) %>% #Set the default map location
      
      ### Base map elements to start with
      
      # #Add RCrk Boundary
      # addPolygons(data=app_boundary,
      #             col = 'black',
      #             stroke = TRUE,
      #             weight=3,
      #             opacity=1,
      #             fillOpacity = 0,
      #             smoothFactor = 2,
      #             layerId = 'RCbnd') %>%
      # 
      # # Add shapefile  overlay of watershed boundaries
      # addPolygons(data=app_watersheds,
      #             group = "watersheds",
      #             col = '#120046',
      #             stroke = TRUE,
      #             weight=2,
      #             opacity=1,
      #             fillColor= "grey90",
      #             fillOpacity = 0,
      #             smoothFactor = 2) %>%

      # Add CZCN soil pit markers
      addMarkers(data=app_CZCN_pits, ~long, ~lat,
                 icon = ~ favicons['pit'],
                 popup = ~as.character(location_name), 
                 label = ~as.character(L1),
                 group = "CZCN_pits") #%>%
      # 
      # # Add ARS met station points
      # addMarkers(data=app_met, ~long, ~lat, 
      #            icon = ~ favicons['met'],
      #            popup = ~as.character(Description), 
      #            label = ~as.character(StationID),
      #            group = "met_markers") %>%
      # 
      # # Add ARS stream weirs
      # addMarkers(data=app_weir, ~long, ~lat, 
      #            icon = ~ favicons['weir'],
      #            popup = ~as.character(Description), 
      #            label = ~as.character(StationID),
      #            group = "weir_markers")
  })
  

### Sensor graphs start here
  
  # Get list of location names
  senID_loc <- unique(sensor_df$Site)
  updateSelectInput(session, "sensor1_ID_loc", choices = senID_loc)

  # Update list of site at the selected location
  senID_site <- reactive({
    df <- sensor_df %>% filter(Site == input$sensor1_ID_loc)  
    unique(df$Subsite)
  })
  
  # Update list of pits at the selected site
  senID_pos <- reactive({
    df <- sensor_df %>% filter(Site == input$sensor1_ID_loc) %>% filter(Subsite == input$sensor1_ID_site)  
    unique(df$Pit)
  })
  
  #When sensor group changes, update sensor file dropdown options
  observe({
    updateSelectInput(session, "sensor1_ID_site", choices = senID_site())#, selected = senID_site()[1])
    updateSelectInput(session, "sensor1_ID_pos", choices = senID_pos())#, selected = senID_pos()[1])
  })
  
  #Create sensor plot
  output$plot_sens1 <- renderPlotly({
    
    df <- sensor_df %>% filter(Site == input$sensor1_ID_loc) %>%
                        filter(Subsite == input$sensor1_ID_site) %>%
                        filter(Pit == input$sensor1_ID_pos) %>%
                        arrange(TIMESTAMP)
                        
    df_surf <- df %>% filter(Depth == "Shallow")
    df_mid <- df %>% filter(Depth == "Middle")
    df_deep <- df %>% filter(Depth == "Deep")
    
    plot_analyte <- input$sensor1_analyte
    
    fig <- plot_ly(type = 'scatter', mode = 'markers')#mode = 'lines+markers')
  
    if(input$surf_show){
      fig <- fig %>% add_trace(x = df_surf$TIMESTAMP, y = df_surf[[`plot_analyte`]], name = paste0(plot_analyte, " Shallow"), 
                               #fill="tozeroy", fillcolor='rgba(26,150,65,0.5)',               
                               #line = list(color = 'rgba(26,150,65,0.7)', width = 2),
                               marker = list(color = 'rgba(26,150,65,0.8)', size = 3))
    }
    
    if(input$mid_show){
      fig <- fig %>% add_trace(x = df_mid$TIMESTAMP, y = df_mid[[`plot_analyte`]], name = paste0(plot_analyte, " Middle"), 
                               #fill="tozeroy", fillcolor='rgba(16,110,25,0.5)',               
                               #line = list(color = 'rgba(16,110,25,0.7)', width = 2),
                               marker = list(color = 'rgba(16,110,25,0.8)', size = 5))
    }
    
    if(input$deep_show){
      fig <- fig %>% add_trace(x = df_deep$TIMESTAMP, y = df_deep[[`plot_analyte`]], name = paste0(plot_analyte, " Deep"), 
                               #fill="tozeroy", fillcolor='rgba(8,50,5,0.5)',               
                               #line = list(color = 'rgba(8,50,5,0.7)', width = 2),
                               marker = list(color = 'rgba(8,50,5,0.8)', size = 3))
    }
    

    fig <- fig %>% layout(xaxis = list(title = 'Date'),#,
                                       #rangeslider = list(type = "date", thickness=0.1)),
                          yaxis = list(title = '')) #Y-Axis Label
    fig
    })
  
  # output$plot_sens2 <- renderPlotly({  
  #   df <- sensor2_df()
  #   fig <- plot_ly(type = 'scatter', mode = 'lines+markers')
  #   fig <- fig %>% add_trace(x = df[,'TIMESTAMP'], y = df[,input$sensor2_analyte1], name = input$sensor2_analyte1, 
  #                            fill="tozeroy", fillcolor='rgba(26,150,185,0.5)',               
  #                            line = list(color = 'rgba(26,150,185,0.7)', width = 2),
  #                            marker = list(color = 'rgba(26,150,185,0.8)', size = 3))
  #   
  #   if(input$sensor2_analyte2 != "None"){
  #     fig <- fig %>% add_trace(x = df[,'TIMESTAMP'], y = df[,input$sensor2_analyte2], name = input$sensor2_analyte2, 
  #                              fill="tozeroy", fillcolor='rgba(16,110,145,0.5)',               
  #                              line = list(color = 'rgba(16,110,145,0.7)', width = 2),
  #                              marker = list(color = 'rgba(16,110,145,0.8)', size = 3))
  #   }
  #   
  #   if(input$sensor2_analyte3 != "None"){
  #     fig <- fig %>% add_trace(x = df[,'TIMESTAMP'], y = df[,input$sensor2_analyte3], name = input$sensor2_analyte3, 
  #                              fill="tozeroy", fillcolor='rgba(8,50,125,0.5)',               
  #                              line = list(color = 'rgba(8,50,125,0.7)', width = 2),
  #                              marker = list(color = 'rgba(8,50,125,0.8)', size = 3))
  #   }
  #   
  #   fig <- fig %>% layout(xaxis = list(title = 'Date',
  #                                      rangeslider = list(type = "date", thickness=0.1)),
  #                         yaxis = list(title = '')) #Y-Axis Label
  #   fig
  # })
  # 
  
  ### Sensor DataTable
  
  # # Get sensor filenames based on group
  # sensor_tbl_choices <- reactive({
  #   if(input$sensor_tbl_group == "CZCN Soil Pits") {
  #     sensor_filenames[grepl("geomicro", sensor_filenames)]
  #   } #else if...for other sensor groups
  # })
  # 
  # # Update sensor file dropdown options
  # observe({
  #   updateSelectInput(session, "sensor_tbl_ID", choices = sensor_tbl_choices(), selected = sensor_tbl_choices()[1])
  # })
  # 
  # # Change sensor file if plot 1 changes
  # # observe({
  # #   c(input$sensor_ID1)
  # #   updateSelectInput(session, "sensor_tbl_ID", selected = input$sensor_ID1)
  # #   })
  # 
  # # Get file path for selected sensor 2
  # sensor_tbl_file <- reactive({
  #   if(input$sensor_tbl_ID == "") {
  #     paste0("./data/Sensors/", sensor_tbl_choices()[1], ".dat")
  #   } else {
  #     paste0("./data/Sensors/", input$sensor_tbl_ID, ".dat")
  #   }
  # })
  # 
  # # Load data from selected sensor 2
  # sensor_tbl_df <- reactive({
  #   
  #   # UNIQUE FORMATTING BY SENSOR GROUP
  #   if(input$sensor_group == "CZCN Soil Pits") {
  #     df <- read.table(sensor_tbl_file(), sep=",", skip=1, fill=T, header=T, na.strings = c("NAN", "NA"))
  #     df <- df %>% distinct()
  #     df$TIMESTAMP <- as.POSIXct(df$TIMESTAMP, format = "%Y-%m-%d %H:%M:%OS", optional=T)
  #     df <- df %>% filter(!is.na(TIMESTAMP)) %>% arrange(TIMESTAMP)
  #     df <- df %>% mutate_at(c(2:ncol(df)), as.numeric)
  #   } #else if...for other sensor groups
  #   
  #   df
  # }) 
  # 
  # Create DataTable
  output$sensor_tbl <- DT::renderDataTable({
    
    df <- sensor_df %>% filter(Site == input$sensor1_ID_loc) %>%
      filter(Subsite == input$sensor1_ID_site) %>%
      filter(Pit == input$sensor1_ID_pos) %>%
      arrange(TIMESTAMP)
    
    DT::datatable(df,
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

  