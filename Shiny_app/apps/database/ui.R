
source("C:/github/GeoMicro_shiny/apps/global_common.R")

####################
### BEGIN UI ###

# Create a conditional password panel
fluidPage(### MAIN APP PAGE STARTS HERE ###
  navbarPage(
    "Reynolds Creek Experimental Watershed",
    id = "nav",
    
    ### DATA EXPLORER TAB STARTS HERE ###
    tabPanel(
      "Database",
      #Create a row with columns
      fluidRow(column(
        4,
        # Create dropdown input selection
        selectInput(
          "dataset",
          "Dataset",
          c("All datasets" = "", as.character(unique(app_db$Dataset))),
          multiple = TRUE
        )
      ),),
      #Create second row with columns
      fluidRow(
        column(
          2,
          # Create a numeric input box
          numericInput(
            "minDepth",
            "Layer top limit (cm)",
            min = 0,
            max = 1000,
            value = NA
          )
        ),
        column(
          2,
          # Create a numeric input box
          numericInput(
            "maxDepth",
            "Layer bottom limit (cm)",
            min = 0,
            max = 1000,
            value = NA
          )
        ),
        column(6),
        column(2,
               downloadButton('downloadDatabase', 'Download database'))
      ),
      hr(),
      # Insert a horizontal line break
      
      # Add a custom panel element to add a crosshair button to each data table row
      conditionalPanel("false", icon("crosshair")),
      
      # Insert a datatable object (set up in server.R)
      DT::dataTableOutput("databaseTBL")
    ),
    
    
  ) # Close navbarpage) # Close conditional panel containing the entire app UI
) # Close the UI (fluidpage)
