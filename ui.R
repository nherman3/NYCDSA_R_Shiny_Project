##### SIDEBAR #####
dashboardPage(
  dashboardHeader(title=h4(HTML("NYC Motor<br/>Vehicle Crashes"))),
  dashboardSidebar(
    img(src = "crash_pic.jpeg",width="100%"),
    sidebarUserPanel(HTML('<span style="color: black;">Nick Herman<br/>2023</span>'),
                     image = "NYCDSA.png"),
    sidebarMenu(
      id = "tabs",
      menuItem(tabName = "aboutme",
               HTML('<i class="fa-solid fa-address-card"></i> About Me')),
      menuItem(tabName = "plots",
               HTML('<i class="fa-solid fa-chart-line"></i>  Plots'),
               menuItemOutput("accidents"),
               menuItemOutput("injuries"),
               menuItemOutput("deaths") 
               ),
      menuItemOutput("data"),
      menuItemOutput("maps")
      ),
    selectInput("vehicle_category", "Vehicle Categories to be Included:",
                   choices = c("Passenger Car", "Truck", "Bicycle/Scooter",
                               "E-Bicycle/Scooter", "Motor Bike"),
                   multiple = TRUE),
    checkboxGroupInput(inputId = 'bor',
                       label = "Borough(s)",
                       choices=sort(unique(df$borough)),
                       selected=unique(df$borough[df$borough!="unlisted"])
                       ),
    actionLink("selectallboros","Select/Deselect All"),
    sliderInput(inputId = "range",
                label = "Year Range",
                min = 2012, max = 2023, 
                value = c(2013,2022),
                sep = ""
                )
    ),
##### TABS #####
  dashboardBody(
    useShinyjs(),
    tabItems(
     tabItem(tabName = "aboutme",
             h1(HTML("NYC Motor Vehicle Collision Study")),
             HTML("The city of New York provides open data on many topics.  
             The Motor Vehicle Collisions data table has information from all 
             incidents where there is an injury, death or $1000+ of damage, 
             requiring the NYPD to submit form MV104-AN.<br/><br/>
             Each row in the datatable is references specific crash event and 
             provides location and time information, descriptions of the 
             vehicle(s) involved, potential contributing factors for each 
             vehicle, and the number of injuries/deaths which occurred.<br/><br/>"),
             HTML('<a href="https://data.cityofnewyork.us/Public-Safety/Motor-Vehicle-Collisions-Crashes/h9gi-nx95">Motor Vehicle Collisions - Crashes</a>'),
             HTML("<br/><br/>Additionally from NYC Open Data, a table containing census 
             information on population was merged with the above table in 
             order to study the how the accidents/injuries/deaths were 
             preportional in each borough.<br/><br/>"),
             HTML('<a href="https://data.cityofnewyork.us/City-Government/New-York-City-Population-by-Borough-1950-2040/xywu-7bv9">New York City Population by Borough, 1950 - 2040</a><br/><br/>'),
             img(src = "nyc-open-data-logo.svg",width="50%"),
             HTML("<br/><br/><br/><br/> *Note: The data in this dable begins 
             halfway through the year in 2012 and up to the present.  The year
             range slider starts at 2013 & 2022 because those are years in 
             which the data is available for the entire year<br/><br/>**Note:
             There are many data points where the borough or location information
             is missing and the borough is marked as 'unlisted.' This is 
             by default unchecked but this data can also be viewed")
     ),
     tabItem(tabName = "plots"
     ),
     tabItem(tabName = "accidents",
             selectInput("selected_option", label = h3("Select Data Breakdown:"), 
                         choices = list("Annually" = "Annually", "Monthly" = "Monthly", 
                                        "Weekly" = "Weekly", "Hourly" = "Hourly"), 
                         selected = "Annually"),
                plotOutput("count_plot"),
                plotOutput("percent_plot")
               ),
     tabItem(tabName = "injuries",
             selectInput("selected_option2", label = h3("Select Data Breakdown:"), 
                         choices = list("Annually" = "Annually", "Monthly" = "Monthly", 
                                        "Weekly" = "Weekly", "Hourly" = "Hourly"), 
                         selected = "Annually"),
             plotOutput("injury_count"),
             plotOutput("injury_pct")
             ),
     tabItem(tabName = "deaths",
             selectInput("selected_option3", label = h3("Select Data Breakdown:"), 
                         choices = list("Annually" = "Annually", "Monthly" = "Monthly", 
                                        "Weekly" = "Weekly", "Hourly" = "Hourly"), 
                         selected = "Annually"),
             plotOutput("death_count"),
             plotOutput("death_pct")
     ),
     tabItem(tabName = 'data',
             DT::dataTableOutput('table'), style = "overflow-y: scroll;"
             ),
     tabItem(tabName = "maps",
             fluidRow(column(1),
                      column(11, textOutput("map_count_title"), 
                             style = "font-size: 24px;")),
             fluidRow(column(1),
                      column(11, textOutput("map_subtitle"))),
             fluidRow(
               column(12,
                      leafletOutput("mymap2", height = "300px"),
                      p())),
          box(title = "Variable Selection", solidHeader = TRUE, 
              collapsible = TRUE, collapsed = FALSE, width=12,
             fluidRow(
               column(3,
                 checkboxGroupInput(inputId = "mapboro", label = "Borough(s)",
                              choices = list("Bronx"="bronx", 
                                             "Brooklyn"="brooklyn", 
                                             "Manhattan"="manhattan", 
                                             "Queens"="queens", 
                                             "Staten Island"="staten island")),
                 actionLink("selectallborosmap","Select/Deselect All")),
               column(5,
                 sliderInput(inputId = "maprange", label = "Year Range",
                             min = 2012, max = 2023, 
                             value = c(2013,2022), sep = ""),
                 sliderTextInput(inputId = "mappointcount", 
                                 label = "Number of Locations to Display",
                                 choices = c(1,2,3,5,10,15,25,50,75,100,250,500,1000),
                                 selected = 25,
                                 grid = TRUE)),
               column(4,
                 checkboxGroupInput(inputId = "mapvehicle", label = "Vehicles Involved",
                      choiceValues = c("Bicycle/Scooter", "E-Bicycle/Scooter",
                                       "Motor Bike", "Passenger Car", "Truck"),
                      choiceNames = list(
                        HTML('<i class="fa-solid fa-person-biking" style="color: black;"></i> Bicycle/Scooter'),
                        HTML('<i class="fa-solid fa-bicycle" style="color: red;"></i> E-Bicycle/Scooter'),
                        HTML('<i class="fa-solid fa-motorcycle" style="color: blue;"></i> Motor Bike'),
                        HTML('<i class="fa-solid fa-car-side" style="color: green;"></i> Passenger Car'),
                        HTML('<i class="fa-solid fa-truck" style="color: purple;"></i> Truck'))),
                 actionLink("selectallvehiclesmap","Select/Deselect All"))),
             fluidRow(
               column(1),
               column(6,
                      uiOutput("mapzipcode")
                      ))),
          box(title = "Data Selection", solidHeader = TRUE, 
              collapsible = TRUE, collapsed = TRUE, width=12,
             fluidRow(column(1),
                      column(4, selectInput("mapchoice", "Map Type:",
                                  choices = c("Accidents", "Injuries", "Deaths"),
                                  selected = "Accidents")),
                      column(4, selectInput("inj_dead_choice", "Type of Injured/Dead:",
                                  choices = c("All", "Pedestrian", 
                                              "Cyclist", "Motorist"),
                                  selected = "Any")))
             
          )
      )
    )
  )
)



















