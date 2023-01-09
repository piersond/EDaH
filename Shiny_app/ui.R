

####################
### BEGIN UI ###
options(spinner.type=6, spinner.size=1)

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
                  wellPanel(#style = "background: #b5cacf",
                    h4("Data Filter"),
                    selectInput(
                      'plot_site',
                      'Sites:',
                      choices = c("ALL", site_names),
                      selected = "ALL",
                      multiple = T,
                      selectize = T
                    ),
                    selectInput(
                      'plot_pos',
                      'Landscape position:',
                      choices = c("ALL", landscape_positions, ""),
                      selected = c("ALL", ""),
                      multiple = T,
                      selectize = T
                    ),
                    sliderInput("plot_depth", label = "Soil depth range:", min = 0, 
                                max = max_depth, value = c(0, max_depth))
                  ),
                  br(),
                  wellPanel(
                  h3("Plot Setup"),
                  tags$hr(style="border-color: #c7c7c7;"),
                  #checkboxInput("smrz_plot_data", tags$b("Average across sampling dates"), FALSE),
                  radioButtons("smrz_plot_data", h4("Plot data structure:"), 
                               choices = list("Sampling date specific" = 1, "Average across sampling dates" = 2), selected = 1),
                  tags$hr(style="border-color: #c7c7c7;"),
                  #selectInput("plot_type", "Plot type:", c("Scatter")),#,"Bar","Box", "Time Series")),
                  selectInput("plot_x", "X-axis:", num_vars,
                              selected = "lf_13c"),
                  selectInput("plot_y", "Y-axis:", num_vars,
                              selected = "enz_n_lnTrans"),
                  selectInput("plot_color", "Color by:", num_vars,
                              selected = "enz_n_lnTrans"),
                  selectInput("facet_by", "Facet wrap:", c("None", "Location", "Landscape position", "Parent Material"),
                              selected = "None"),
                  ),
                  tags$hr(style="border-color: #c7c7c7;"),
                  helpText("Notes:")
           ),
           column(10,
                  div(style = 'padding-top:100px; padding-left:30px; padding-right:120px;',  
                  plotlyOutput("chart1",
                               width = "100%",
                               height = "100%") %>% withSpinner()),
                  hr(),
                  br(),
                  h2("Plot data"),
                  DT::dataTableOutput("plotTBL"))
                  )),
         #div(style = 'padding-top:10px;'),
         #hr(),
         # fluidRow(
         #   column(2),
         #   column(6,
         #          h2("Plot data")),
         #   # column(2,
         #   #        downloadButton('downloadPlotData', 'Download data'))
         #   ),
         # fluidRow(         
         #   column(2),
         #   column(8,
         #          # Insert a datatable object (set up in server.R)
         #          DT::dataTableOutput("plotTBL"))
         # )
         #),


### SENSORS TAB STARTS HERE ###   
tabPanel("Sensors",
         h2("Soil Sensors"),
         leafletOutput("sensormap", height="30vh") %>% withSpinner(),
         br(),
         fluidRow(
           column(2, style = "background-color:#e4e7eb;",
                  #selectInput('sensor_group', h3("Sensor group:"), c("CZCN Soil Pits")),
                  #hr(),
                  br(),
                  selectInput('sensor1_ID_loc', tags$b("Location:"), c("")),
                  selectInput('sensor1_ID_site', tags$b("Site:"), c("")),
                  selectInput('sensor1_ID_pos', tags$b("Landscape position:"), c("")),
                  selectInput('sensor1_analyte', tags$b("Analyte:"), sensor_analytes),
                  tags$b("Sensor depth:"),
                  div(style = "padding-left: 20px",
                  checkboxInput("surf_show", "Shallow", TRUE),
                  checkboxInput("mid_show", "Middle", TRUE),
                  checkboxInput("deep_show", "Deep", TRUE),
                  ),
                  hr(),
                  wellPanel(#style = "background: #b5cacf",
                    radioButtons("sensor_timestep", h4("Sensor timestep"), 
                                 choices = list("Daily" = 1, "15 minute" = 2), selected = 1),
                    radioButtons("sens_plot_style", h4("Plot style"),
                                 choices = list("Marker" = 1, "Line" = 2, "Line + Fill" = 3), selected = 1)
                  ),
           ),
           column(10,
                  br(),
                  plotlyOutput("plot_sens1",
                               width = "100%",
                               height = "80%") %>% withSpinner(), #color = "#0dc5c1",
                  hr(),
                  br(),
                  h2("Sensor Data"),
                  DT::dataTableOutput("sensor_tbl") %>% withSpinner()
           )
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


