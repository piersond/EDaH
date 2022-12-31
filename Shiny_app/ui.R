library(leaflet)




####################
### BEGIN UI ###

# Create a conditional password panel 
fluidPage(
  #Show if password is not entered
  conditionalPanel(id = "myId", condition = "input.password != 'rcdata'", #<-- Change password here, 1 of 2
    h1("Reynolds Creek SoDaH Database"),
    passwordInput("password", h3("Enter password:"), value = ""),
    p("Contact Derek Pierson (derekpierson@isu.edu) for access to database.")
  ), 
  #Show if password is entered correctly (entire app)
  conditionalPanel(condition = "input.password == 'rcdata'", #<-- Change password here, 2 of 2  

### MAIN APP PAGE STARTS HERE ###                   
  navbarPage("Reynolds Creek Experimental Watershed", id="nav",
    # Create a tab panel for the map
    
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
        top = 180, 
        left = "auto", 
        right = 240, 
        bottom = "auto",
        width = 330, 
        height = "auto",
        h2("Map Explorer"), #Panel title

        # Create an input for the panel
        selectInput("point_var", HTML("<b>Map Analyte</b>"), num_vars),
        
        # Create a 1x2 panel with 2 input boxes
        column(width=12, 
               fixedRow(
                column(6,
                  textInput("lyr_top", "Soil layer top:", value=0)),
                column(6,
                  textInput("lyr_bot", "Soil layer bottom:", value=5)))),
        helpText("Enter a single depth value or range (cm)"),         
        hr(),
        div(style = 'padding-top:0px;'),
        selectInput("map_raster", HTML("<b>Show map layer:</b>"), 
                    choices=raster_lyrs),
        helpText("Map layers can take ~1 minute to load."),
        hr(),
        div(style = 'padding-top:0px;'),        
        checkboxInput("watershed_bounds", "Show watershed boundaries", TRUE)
        # Add plot to panel
        #plotOutput("histCentile", height = 200), (set up in server.R)
      ),
      
      # Add citation statement in lower left corner (placement based on css)
      tags$div(id="cite",
        'Data compiled for ', tags$em('Reynolds Creek CZO'), ' by Derek Pierson.'
      )
    )
  ),

### DATA PLOT TAB STARTS HERE ### 
tabPanel("Plot Explorer",
         fluidRow(
           column(2, style = "background-color:#e4e7eb;",
                  h2("Plot options:"),
                  hr(),
                  selectInput("plot_type", "Plot type:", c("Scatter","Bar","Box", "Time Series")),
                  selectInput("plot_x", "X-axis Variable:", num_vars),
                  selectInput("plot_y", "Y-axis Variable:", num_vars),
                  selectInput("plot_color", "Color by:", num_vars),
                  hr(),
                  helpText("Notes go here.")
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
           column(8,
           # Insert a datatable object (set up in server.R)
           DT::dataTableOutput("plotTBL"))
         )
         ),

### DATA EXPLORER TAB STARTS HERE ###   
    tabPanel("Dynamic Table",
      #Create a row with columns
      fluidRow(
        column(4,
          # Create dropdown input selection      
          selectInput("dataset", "Dataset", c("All datasets"="", as.character(unique(RC_database$Dataset))), multiple=TRUE)
        ),
      ),
      #Create second row with columns
      fluidRow(
        column(2,
          # Create a numeric input box
          numericInput("minDepth", "Layer top limit (cm)", min=0, max=1000, value=NA)
        ),
        column(2,
          # Create a numeric input box
          numericInput("maxDepth", "Layer bottom limit (cm)", min=0, max=1000, value=NA)
        )
      ),
      hr(), # Insert a horizontal line break
      
      # Add a custom panel element to add a crosshair button to each data table row 
      conditionalPanel("false", icon("crosshair")), 

      # Insert a datatable object (set up in server.R)
      DT::dataTableOutput("databaseTBL")
      ),

### DATA SUMMARY TAB STARTS HERE ### 
  tabPanel("Data Info",
           h2("Database variable information"),
           h4(uniq_loc_text),
           hr(),
           fluidRow(
             column(6,
                DT::dataTableOutput("data_summaryTBL")))
   )

  ) # Close navbarpage
  ) # Close conditional panel containing the entire app UI
) # Close the UI (fluidpage)


