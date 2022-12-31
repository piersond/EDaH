
source("C:/github/GeoMicro_shiny/apps/global_common.R")

####################
### BEGIN UI ###

# Create a conditional password panel
fluidPage(### MAIN APP PAGE STARTS HERE ###
  navbarPage(
    "Reynolds Creek Experimental Watershed",
    id = "nav",
    
    ### SENSORS TAB STARTS HERE ###
    tabPanel(
      "Sensors",
      h2("Soil Sensors"),
      leafletOutput("sensormap", height = "30vh"),
      br(),
      fluidRow(
        column(
          2,
          style = "background-color:#e4e7eb;",
          selectInput('sensor_site', h3("Site:"), c("Reynolds Creek", "Luquillo", "Southern Sierra"), selected = "Reynolds Creek"),
          hr(),
          selectInput('sensor_ID1', tags$b("Plot 1 Sensor:"), c("")),
          div(selectInput(
            'sensor1_analyte1', "Analyte 1:", c("Analyte 1")
          ), style = "padding-left:25px;"),
          div(selectInput(
            'sensor1_analyte2', "Analyte 2:", c("Analyte 2")
          ), style = "padding-left:25px;"),
          div(selectInput(
            'sensor1_analyte3', "Analyte 3:", c("Analyte 3")
          ), style = "padding-left:25px;"),
          hr(),
          selectInput('sensor_ID2', tags$b("Plot 2 Sensor:"), c("")),
          div(selectInput(
            'sensor2_analyte1', "Analyte 1:", c("Analyte 1")
          ), style = "padding-left:25px;"),
          div(selectInput(
            'sensor2_analyte2', "Analyte 2:", c("Analyte 2")
          ), style = "padding-left:25px;"),
          div(selectInput(
            'sensor2_analyte3', "Analyte 3:", c("Analyte 3")
          ), style = "padding-left:25px;"),
        ),
        column(
          10,
          plotlyOutput("plot_sens1"),
          hr(),
          br(),
          plotlyOutput("plot_sens2"),
        )
      ),
      hr(),
      br(),
      h2("Sensor Data"),
      fluidRow(
        column(
          2,
          style = "background-color:#e4e7eb;",
          selectInput('sensor_tbl_group', h3("Sensor group:"), c("CZCN Soil Pits")),
          hr(),
          selectInput('sensor_tbl_ID', tags$b("Sensor name:"), c("")),
          hr(),
          downloadButton('downloadSensor', 'Download'),
          hr()
        ),
        column(10,
               DT::dataTableOutput("sensor_tbl"))
      )
    )
    
  ) # Close navbarpage) # Close conditional panel containing the entire app UI
) # Close the UI (fluidpage)
