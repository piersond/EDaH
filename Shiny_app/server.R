library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

# Prep variables for server use

# Work from a copy of the database
RC_data <- RC_database

# Columns to remove from the database (not needed in the app)
drop_cols <- c('google_id', 'addit_contact_email', 'addit_contact_person', 'author_email', 'author_orcid_id',
               'author_PersonName', 'coarse_tot', 'curator_email', 'curator_organization', 'curator_PersonName', 'data_file',
               'experiments', 'gradient', 'header_row', 'key_version', 'location_name', 'merge_align', 'modification_date',
               'NA_1', 'NA_2', 'network', 'site_code', 'time_series', 'sample_collector') 

######################################
### SERVER STARTS HERE ###
function(input, output, session) {

  
### PLOT HIGHCHART STARTS HERE ###  
  observeEvent(c(input$plot_x, input$plot_y, input$plot_color), {
    
    # Make a smaller dataframe to speed up highchart render
    plot_df <- data.frame(uniqueID = RC_data["uniqueID"],
                          lat = RC_data["lat"],
                          long= RC_data["long"],
                          x_data = RC_data[input$plot_x],
                          y_data = RC_data[input$plot_y],
                          col_data = RC_data[input$plot_color])
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
  })

  
## Interactive Map Starts Here ###
  
  # Create the Leaflet basemap and basemap options
  output$map <- renderLeaflet({
    leaflet() %>%
      # Add basemap options
      addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>%
      addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>% #Group = layer name
      addProviderTiles("Stamen.Toner", group = "Stamen.Toner") %>%
      addProviderTiles("Stamen.Terrain", group = "Stamen.Terrain") %>%
      addProviderTiles("Esri.WorldStreetMap", group = "Esri.WorldStreetMap") %>%
      addProviderTiles("Wikimedia", group = "Wikimedia") %>%
      addProviderTiles("CartoDB.Positron", group = "CartoDB.Positron") %>%
      # Add basemap layers control options
      addLayersControl(
        baseGroups = c(
          "Esri.WorldImagery", "OpenStreetMap", "Stamen.Toner","Stamen.Terrain", 
          "Esri.WorldStreetMap", "Wikimedia", "CartoDB.Positron"),
        position = "topleft",
        overlayGroups = "overlay",
        options = layersControlOptions(collapsed = TRUE, autoZIndex = FALSE)) %>%
        setView(lng = -116.75, lat = 43.16, zoom = 11) %>% #Set the default map location
  
  ### Base map elements to start with
      
      # Add RCrk Boundary
      addPolygons(data=rc_boundary,
                  col = 'black',
                  stroke = TRUE, 
                  weight=3,
                  opacity=1,
                  fillOpacity = 0, 
                  smoothFactor = 2,
                  layerId = 'RCbnd') %>%
      
      # Add shapefile overlay of watershed boundaries
      addPolygons(data=rc_watersheds,
                  group = "watersheds",
                  col = '#120046',
                  stroke = TRUE, 
                  weight=2,
                  opacity=1,
                  fillColor= "grey90",
                  fillOpacity = 0, 
                  smoothFactor = 2)
    })

  
### Observe events related to map point data inputs
  observeEvent(c(input$lyr_top, input$lyr_bot, input$point_var, input$map_zoom), {
    
    # Print log of event    
    print("Point layer event triggered")
    
    if (input$point_var == "none") {
      leafletProxy("map") %>% clearGroup('points') %>% clearGroup('points_init') 
    }
    
    req(input$point_var != "none")
    
    # Require data to proceed
    req(input$lyr_top, input$lyr_bot)
    
    # Process layer depth limits
    if (grepl("-", as.character(input$lyr_top), fixed=TRUE)) {
      lyr_top_min <- as.numeric(strsplit(gsub(" ", "", as.character(input$lyr_top)),"-")[[1]][1])
      lyr_top_max <- as.numeric(strsplit(gsub(" ", "", as.character(input$lyr_top)),"-")[[1]][2])
    } else {
      lyr_top_min <- as.numeric(input$lyr_top)
      lyr_top_max <- as.numeric(input$lyr_top)
    }
    
    if (grepl("-", as.character(input$lyr_bot), fixed=TRUE)) {
      lyr_bot_min <- as.numeric(strsplit( gsub(" ", "", as.character(input$lyr_bot)),"-")[[1]][1])
      lyr_bot_max <- as.numeric(strsplit( gsub(" ", "", as.character(input$lyr_bot)),"-")[[1]][2])
    } else {
      lyr_bot_min <- as.numeric(input$lyr_bot)
      lyr_bot_max <- as.numeric(input$lyr_bot)
    }
    
    # Grab data for map and apply filters
    map_data <- RC_data %>%  
      filter(layer_top >= lyr_top_min & layer_top <= lyr_top_max) %>% #filter by user input soil depth
      filter(layer_bot >= lyr_bot_min & layer_bot <= lyr_bot_max) %>% 
      filter(!!as.symbol(input$point_var) > -1000) %>% # !!as.symbol(input$point_var) turns the input value into a variable name (e.g. a column name as used here) 
      filter(!is.na(lat)) %>%
      filter(!is.na(long)) %>%
      filter(!is.na(uniqueID)) %>%
      filter(!is.na(!!as.symbol(input$point_var))) %>%
      arrange(!!as.symbol(input$point_var)) %>%
      select(!!as.symbol(input$point_var), lat, long, uniqueID)
    
    # Create a colorscale based on quantiles of the point_var_data, which is in column 1 of map_data
    point_pal_qn_breaks <- round(quantile(map_data[,1], c(0.02, 0.15, 0.3, 0.4, 0.5, 0.6, 0.7, 0.85, 0.98), na.rm = TRUE),2)
    point_pal <- colorBin(palette = "viridis", 
                          domain = map_data[,1], 
                          bins = unique(c(min(map_data[,1]), point_pal_qn_breaks, max(map_data[,1]))), 
                          pretty = TRUE, 
                          na.color='transparent') 
    
    # Clear any existing points group layer from the map
    leafletProxy("map") %>% clearGroup('points') %>% clearGroup('points_init') #clearShapes()
    
    # Add points layer
    leafletProxy("map") %>%
      addCircles(data = map_data, ~long, ~lat, 
                 group = "points",
                 layerId=~uniqueID, # Set identifier for each circle
                 stroke=FALSE, # Include shape border
                 fillOpacity=0.85, # Set circle fill opacity
                 fillColor=point_pal(map_data[,1]), # Set circle fill color based on color palette
                 radius = case_when(input$map_zoom >=18 ~5,
                                    input$map_zoom ==17 ~7, #Change circle size based on zoom level
                                    input$map_zoom ==16 ~10,
                                    input$map_zoom ==15 ~20, 
                                    input$map_zoom ==14 ~50, 
                                    input$map_zoom ==13 ~100, 
                                    input$map_zoom ==12 ~300, 
                                    input$map_zoom ==11 ~500, 
                                    input$map_zoom <=10 ~1000)) %>%
      
      # Add point color scale legend
      addLegend("bottomright",
                group="points",
                pal=point_pal, 
                values=map_data[,1], 
                title=input$point_var,
                layerId="pointLegend")
    
  })

### Observe watershed boundary checkbox
  observeEvent(input$watershed_bounds, {  
    if(!input$watershed_bounds) {
      leafletProxy("map") %>% clearGroup('watersheds')
    } else {
      leafletProxy("map") %>%
        addPolygons(data=rc_watersheds,
                    group = "watersheds",
                    col = '#120046',
                    stroke = TRUE, 
                    weight=2,
                    opacity=1,
                    fillColor= "grey90",
                    fillOpacity = 0, 
                    smoothFactor = 2)
    }
  })
      

### Observe events related to raster layers
  observeEvent(input$map_raster, {

    # Print event log
    print("Raster layer event triggered")
    
    ## Add raster image for MAST
    if(input$map_raster == 1) { # MAST RASTER
      
      # Clear off other raster layers
      leafletProxy("map") %>% clearGroup("GEP") %>% removeControl("GEPleg") %>%
                              clearGroup("DEM") %>% removeControl("DEMleg")
      
      # Grab the raster data values
      MAST_vals <- values(raster_MAST)
      
      #Create color palette for raster map
      #raster_qn_breaks <- round(quantile(MAST_vals, c(0.02, 0.15, 0.3, 0.4, 0.5, 0.6, 0.7, 0.85, 0.98), na.rm = TRUE),2)
      MAST_pal <- colorBin(palette = c("#05475e", "#faff78", "#ff9a00", "#ff3500"),
                           domain = MAST_vals,
                           bins = c(-5:15), 
                           pretty = TRUE,
                           na.color = "transparent")
      
      # Add TSOI raster and legend to map object
      
        leafletProxy("map") %>% 
          addRasterImage(raster_MAST, colors = MAST_pal, opacity = 1, group = "MAST") %>% 
          #addGeotiff("./map/tsoi_est2_3857.tif",
          #           colorOptions = colorOptions(
          #            palette = colorRampPalette(c("#05475e", "#faff78", "#ff9a00", "#ff3500"))(100) ,
          #             breaks = c(-5:15),
          #             na.color = "transparent"),
          #           opacity = 1, 
          #           group = "MAST") %>%
          addLegend("bottomleft",
                    layerId = "MASTleg",
                    pal = MAST_pal,
                    values = MAST_vals,
                    title = "Estimated Mean Annual</br>Soil Temperature</br>0-30 cm Depth</br>(deg C)")
        
    } else if (input$map_raster == 2) { # GEP RASTER
      
      # Clear off other raster layers
      leafletProxy("map") %>% clearGroup("MAST") %>% removeControl("MASTleg")
      
      #Grab the raster data values
      GEP_vals <- values(raster_GEP)

      #Create color palette for raster map
      #raster_qn_breaks <- round(quantile(MAST_vals, c(0.02, 0.15, 0.3, 0.4, 0.5, 0.6, 0.7, 0.85, 0.98), na.rm = TRUE),2)
      GEP_pal <- colorBin(palette = c("#ff5454", "#f7ee3c", "#a0d601", "#00a60b", "#004dd0", "#1b0061"),
                           domain = GEP_vals,
                           bins = seq(-400, 2400, 200),
                           pretty = TRUE,
                           na.color = "transparent")

      # Add GEP raster and legend to map object
      leafletProxy("map") %>% addRasterImage(raster_GEP, colors = GEP_pal, opacity = 1, group = "GEP") %>%
        addLegend("bottomleft",
                  layerId = "GEPleg",
                   pal = GEP_pal,
                   values = GEP_vals,
                   title = "Estimated Gross</br>Ecosystem Production</br>(gC m^-2 yr^-1)")

    } else if (input$map_raster == 3) { # GEP RASTER
      
      # Clear off other raster layers
      leafletProxy("map") %>% clearGroup("MAST") %>% removeControl("MASTleg") %>%
                              clearGroup("GEP") %>% removeControl("GEPleg")
      
      #Grab the raster data values
      DEM_vals <- values(raster_DEM)
      
      #Create color palette for raster map
      DEM_pal <- colorBin(palette = c("#febb43", "#d8e335", "#4a7500"),
                          domain = DEM_vals,
                          bins = seq(1100, 2300, 10),
                          pretty = TRUE,
                          na.color = "transparent")

      DEM_pal_legd <- colorBin(palette = c("#febb43", "#d8e335", "#4a7500"),
                          domain = DEM_vals,
                          bins = seq(1100, 2300, 100),
                          pretty = TRUE,
                          na.color = "transparent")
            
      HILLSH_vals <- values(raster_HILLSH)
      HILLSH_pal <- colorBin(palette = c("grey10", "grey90"),
                          domain = HILLSH_vals,
                          bins = seq(0, 254, 1),
                          pretty = TRUE,
                          na.color = "transparent")
      
           
      # Add GEP raster and legend to map object
      leafletProxy("map") %>% 
        addRasterImage(raster_HILLSH, 
                       colors = HILLSH_pal, 
                       opacity = 1, 
                       group = "DEM") %>%        
        addRasterImage(raster_DEM, 
                       colors = DEM_pal, 
                       opacity = 0.5, 
                       group = "DEM") %>%
        addLegend("bottomleft",
                  layerId = "DEMleg",
                  pal = DEM_pal_legd,
                  values = DEM_vals,
                  title = "Elevation (m)")
      
    } else {
      leafletProxy("map") %>% clearGroup("MAST") %>% removeControl("MASTleg") %>%
                              clearGroup("GEP") %>% removeControl("GEPleg") %>%
                              clearGroup("DEM") %>% removeControl("DEMleg")
        
    }
  }) 
  
  
### Implement point popups ###
  
  # Create function to show a popup at the given point location
  showPointPopup <- function(uniqueID, lat, lng) {

    #Only proceed if a point is selected
    if (!is.null(uniqueID)){
      
      # Grab data for the selected point
      selectedPoint <- RC_data[RC_data$uniqueID == uniqueID,]
      
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
    df <- RC_data %>% 
      .[,setdiff(names(.),drop_cols)] %>%

    mutate(Action = paste('<a class="go-map" href="" data-lat="', lat, '" data-long="', long, '" data-zip="', uniqueID, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df, outputId = "ziptable")
    
    DT::datatable(df, 
                  options = list(ajax = list(url = action), 
                                 lengthMenu = c(10, 50, 100), 
                                 pageLength = 50), 
                  escape = FALSE, 
                  class = "display nowrap")
    })

  ### Data Summary Table Starts Here ###  
  output$data_summaryTBL <- DT::renderDataTable({    
    DT::datatable(RC_data_summary %>% select(!Short_Desc), 
                  rownames = FALSE, 
                  options = list(dom = 't', pageLength = nrow(RC_data_summary)), 
                  escape = FALSE, 
                  class = "display nowrap")
  })
}

  