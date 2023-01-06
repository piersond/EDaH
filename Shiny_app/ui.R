

####################
### BEGIN UI ###

# Create a conditional password panel 
fluidPage(
  #Show if password is not entered
  conditionalPanel(id = "myId", condition = "input.password != 'czngd'", #<-- Change password here, 1 of 2
    h1("Critical Zone Network: Geomicrobiology Database"),
    passwordInput("password", h3("Enter password:"), value = ""),
    p("Contact Derek Pierson (derek.pierson@usda.gov) for access to database.")
  ), 
  #Show if password is entered correctly (entire app)
  conditionalPanel(condition = "input.password == 'czngd'", #<-- Change password here, 2 of 2  

### MAIN APP PAGE STARTS HERE ###                   
  navbarPage("CZN Geomicrobiology Database", id="nav",
    # Create a tab panel for the map
### Welcome HTML START PAGE HERE ###
    tabPanel("Overview",
         includeHTML("welcome.html")),
    
### MAP TAB STARTS HERE ###   
    tabPanel("Interactive Map",
      div(class="outer",
        tags$head(
          # Include custom CSS and JS for styling
          includeCSS("styles.css"),
          includeScript("gomap.js")
        ),

      # Create map element and set size (use % if using CSS, otherwise number)
      leafletOutput("map", width="100%", height="100%"),

      # Create a panel over-top of the map
      absolutePanel(
        id = "controls", 
        class = "panel panel-default", 
        fixed = TRUE,
        draggable = TRUE, 
        top = 150, 
        left = "auto", 
        right = 240, 
        bottom = "auto",
        width = 320, 
        height = "auto",
        h2("Map Explorer"), #Panel title

        # Create an input for the panel
        selectInput("point_var", HTML("<b>Map Analyte</b>"), num_vars, 
                    selected = "ph_h2o"),
        
        # Create a 1x2 panel with 2 input boxes
        column(width=12, 
               fixedRow(
                column(6,
                  textInput("lyr_top", "Soil layer top:", value=0)),
                column(6,
                  textInput("lyr_bot", "Soil layer bottom:", value="5-10")))),
        helpText("Enter a single depth value or range (cm)"),         
        hr(),
        # div(style = 'padding-top:0px;'),
        # selectInput("map_raster", HTML("<b>Show map layer:</b>"), 
        #             choices=raster_lyrs),
        # helpText("Map layers can take ~1 minute to load."),
        # hr(),
        h5(HTML("<b>Map Elements</b>")),
        div(style = 'padding-top:0px;'),        
        checkboxInput("pit_show", "Soil pits", TRUE),
        checkboxInput("met_show", "Weather stations", TRUE),
        #checkboxInput("czcn_sites_show", "CZCN GeoMicrobiology sites", TRUE),
        #checkboxInput("met_show", "Meteorologic stations", TRUE),
        #checkboxInput("weir_show", "Stream flow weir", TRUE),
        #checkboxInput("watershed_bounds", "Watershed boundaries", TRUE)
        
        # Add plot to panel
        #plotOutput("histCentile", height = 200), (set up in server.R)
      ),
      
      # Add citation statement in lower left corner (placement based on css)
      tags$div(id="cite",
        'Data compiled for ', tags$em('Critical Zone Network: Geomicrobiology cluster'), ' by Derek Pierson.'
      )
    )
  ),

### DATA PLOT TAB STARTS HERE ### 
tabPanel("Plot Explorer",
         fluidRow(
           column(2, style = "background-color:#e4e7eb;",
                  br(),
                  wellPanel(
                    h4("Data Filter"),
                    selectInput(
                      'plot_site',
                      'Sites:',
                      choices = c("ALL", site_names),
                      selected = "ALL",
                      multiple = T
                    ),
                    sliderInput("plot_depth", label = "Soil depth range:", min = 0, 
                                max = max_depth, value = c(0, max_depth))
                  ),
                  br(),
                  wellPanel(
                  h4("Plot Setup"),
                  #selectInput("plot_type", "Plot type:", c("Scatter")),#,"Bar","Box", "Time Series")),
                  selectInput("plot_x", "X-axis:", num_vars,
                              selected = "lf_13c"),
                  selectInput("plot_y", "Y-axis:", num_vars,
                              selected = "enz_n_lnTrans"),
                  selectInput("plot_color", "Color by:", all_vars,
                              selected = "enz_n_lnTrans"),
                  selectInput("facet_by", "Split plot by:", cat_vars,
                              selected = "enz_n_lnTrans")
                  ),
                  hr(),
                  helpText("Notes:")
           ),
           column(10,
                  div(style = 'padding-top:20px; padding-left:40px;',  
                  # Create plotly UI element
                  plotlyOutput("chart1",
                               width = "1000px",
                               height = "600px"))
                  )
           ),
         div(style = 'padding-top:10px;'),
         hr(),
         fluidRow(
           column(2),
           column(6,
                  h2("Plot data")),
           # column(2,
           #        downloadButton('downloadPlotData', 'Download data'))
           ),
         fluidRow(         
           column(2),
           column(8,
                  # Insert a datatable object (set up in server.R)
                  DT::dataTableOutput("plotTBL"))
         )
         ),


### SENSORS TAB STARTS HERE ###   
tabPanel("Sensors",
         h2("Sensors"),
         leafletOutput("sensormap", height="30vh"),
         br(),
         fluidRow(
           column(2, style = "background-color:#e4e7eb;",
                  #selectInput('sensor_group', h3("Sensor group:"), c("CZCN Soil Pits")),
                  #hr(),
                  selectInput('sensor1_ID_loc', tags$b("Location:"), c("")),
                  selectInput('sensor1_ID_site', tags$b("Site:"), c("")),
                  selectInput('sensor1_ID_pos', tags$b("Landscape position:"), c("")),
                  selectInput('sensor1_analyte', "Analyte:", sensor_analytes),
                  hr(),
                  checkboxInput("surf_show", "Shallow", TRUE),
                  checkboxInput("mid_show", "Middle", FALSE),
                  checkboxInput("deep_show", "Deep", FALSE),
                  
                  #div(selectInput('sensor1_analyte1_depth', "Analyte 1:", c("Analyte 1")), style = "padding-left:25px;"),
                  
                  #div(selectInput('sensor1_analyte2', "Analyte 2:", c("Analyte 2")), style = "padding-left:25px;"),
                  #div(selectInput('sensor1_analyte2_depth', "Analyte 2:", c("Analyte 2")), style = "padding-left:25px;"),
                  
                  #div(selectInput('sensor1_analyte3', "Analyte 3:", c("Analyte 3")), style = "padding-left:25px;"),
                  #div(selectInput('sensor1_analyte3_depth', "Analyte 3:", c("Analyte 3")), style = "padding-left:25px;"),
                  hr()#,
                  #selectInput('sensor_ID2', tags$b("Plot 2 Sensor:"), c("")),
                  #div(selectInput('sensor2_analyte1', "Analyte 1:", c("Analyte 1")), style = "padding-left:25px;"),
                  #div(selectInput('sensor2_analyte2', "Analyte 2:", c("Analyte 2")), style = "padding-left:25px;"),
                  #div(selectInput('sensor2_analyte3', "Analyte 3:", c("Analyte 3")), style = "padding-left:25px;"),
           ),
           column(10,
                  plotlyOutput("plot_sens1"),
                  hr()#,
                  # br(),
                  # plotlyOutput("plot_sens2"),
           )
         ),
         hr(),
         #br(),
         h2("Sensor Data"),
         fluidRow(
           # column(2, style = "background-color:#e4e7eb;",
           #        selectInput('sensor_tbl_group', h3("Sensor group:"), c("CZCN Soil Pits")),
           #        hr(),
           #        selectInput('sensor_tbl_ID', tags$b("Sensor name:"), c("")),
           #        hr(),
           #        downloadButton('downloadSensor', 'Download'),
           #        hr()),
           column(10,
                  DT::dataTableOutput("sensor_tbl"))
         )
),


### DATA EXPLORER TAB STARTS HERE ###   
    tabPanel("Database",
      #Create a row with columns
      # fluidRow(
      #   column(4,
      #     # Create dropdown input selection      
      #     selectInput("dataset", "Dataset", c("All datasets"="", as.character(unique(app_db$Dataset))), multiple=TRUE)
      #   ),
      # ),
      #Create second row with columns
      # fluidRow(
      #   column(2,
      #     # Create a numeric input box
      #     numericInput("minDepth", "Layer top limit (cm)", min=0, max=1000, value=NA)
      #   ),
      #   column(2,
      #     # Create a numeric input box
      #     numericInput("maxDepth", "Layer bottom limit (cm)", min=0, max=1000, value=NA)
      #   ),
      #   column(6),
      #   column(2,
      #          downloadButton('downloadDatabase', 'Download database'))
      # ),
      hr(), # Insert a horizontal line break
      
      # Add a custom panel element to add a crosshair button to each data table row 
      conditionalPanel("false", icon("crosshair", verify_fa = FALSE)), 

      # Insert a datatable object (set up in server.R)
      DT::dataTableOutput("databaseTBL")
      ),


### DATA KEY TAB STARTS HERE ### 
  tabPanel("Data Key",
           div(tabsetPanel(
             tabPanel(tags$b("Location"),
                      DTOutput('var_info.loc')),
             tabPanel(tags$b("Profile"),
                      DTOutput('var_info.prof'))
           )), style = "padding-right:600px;"),
         
### SOURCES TAB STARTS HERE ###   
  tabPanel("Resources"),

### SOURCES TAB STARTS HERE ###   
  tabPanel("Contact")

  ) # Close navbarpage
  ) # Close conditional panel containing the entire app UI
) # Close the UI (fluidpage)


