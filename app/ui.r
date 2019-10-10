#ui
ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "NYC Shooting Crime Map"),
                    dashboardSidebar(sidebarMenu(
                      menuItem("Home", tabName = "Home", icon = icon("home")),
                      menuItem("Animation", tabName = "Animation", icon = icon("train")),
                      menuItem("Map", tabName = "Map", icon = icon("map")),
                      menuItem("Report", tabName = "Report", icon = icon("industry")),
                      menuItem("Holidays", tabName = "Holidays", icon = icon("anchor")),
                      menuItem("Source", tabName = "Source", icon = icon("th"))
                    )),
                    dashboardBody(
                      tabItems(
                        #home
                        tabItem(tabName = "Home",
                                fluidPage(
                                  fluidRow(
                                    box(width = 15, title = "Introduction", status = "warning",
                                        solidHeader = TRUE, h3("NYC Shooting Crime Map"),
                                        h4("By Weijia Bao, Kanyan Chen, Tiantian Chu, Chang Xu, Qingyu Zhang"),
                                        h5("In America, which has one of the highest rates of gun homicide in the world, cities experience gun violence at very high rates.  And urban gun violence touches on issues central to American life : safety, equality, opportunity and community. Residents and leaders of America’s cities face few challenges more urgent than gun violence. It takes thousands of lives, depresses the quality of life of whole neighborhoods, drives people to move away, and reduces cities’ attractiveness for newcomers. It makes it harder for schools, businesses, and community institutions to thrive. Urban gun violence also reflects and worsens America’s existing racial and economic disparities. So gun violence is a very severe situation to America, especially to big cities, like New York City."),
                                        h5("Our shiny app is about Shooting Crime Map in NYC, we aim at two types of customers: Residents and Police Departments. For residents, they can use our app to check the shooting crimes in different neighborhoods of New York, providing a good way to find a safer place to live and work. For police departments, we can assist them to reduce crime through a better-informed citizenry. Creating more self-reliance among community members is a great benefit to community oriented policing efforts everywhere and has been proven effective in combating crime."), 
                                        h5("Then, please follow me to use this app!"))),
                                  fluidRow(box(width = 15, title = "User Guide", status = "warning",
                                               solidHeader = TRUE, h3("What Does This Map Do?"),
                                               tags$div(tags$ul(
                                                 tags$li("Animation: This part contains a heatmap and a time series graph, and they are linked with each other. The x-axis of time series graph and the adjustable bar of heatmap represent the 24 hours of a day, and y-axis of time series graph represents the total count number of shooting crimes for the whole 13 years (2006-2018). The darker of the heatmap, the more shooting crimes happened."),
                                                 tags$li("Maps: This part is our search map. There are six filters in total: Boroughs, Start Date, End Date, Race, Gender and Age. Users can select their own choice to understand the shooting crimes in their chosen areas. For example, Amy, a 24 years-old Black girl. She’s going to New York to work, but she has never been to New York. Then she can use our map to find places where she thinks safe to live in. Besides, for different boroughs, we have different pie charts for Race, Gender and Age, which could help users understand the situations in these boroughs more intuitively."),
                                                 tags$li("Report: We have nine graphs for this part in total: Interactive pie-bar charts for different boroughs, Shooting Counts by Year, Season, Week, Boroughs, Murder or not, Race, Age and Gender. All these nine graphs help police departments to better understand the specific factors that drive gun violence."),
                                                 tags$li("Holidays: This part is our interesting finding. We found that on the day of holiday, there were more shooting crimes than other days. There are four holidays users can choose: Independence Day, Halloween, Christmas Day and New Year’s Day. For example, on Christmas Day of 2017, there were 7 shootings in NYC, this was the day with the most shootings from Dec 20, 2017 to Dec 31, 2017. This finding could help police departments to better distribute the polices in important holidays.")
                                               ))))
                                )),
                        #animation
                        tabItem(tabName = "Animation",
                                fluidPage(
                                  fluidRow(leafletOutput("mapAct", height = "800px")),
                                  fluidRow(absolutePanel(top = 150, right = 20,
                                                         sliderInput("animation", "Time(30 minutes)", min = 0, 
                                                                     max = 47, value = 0, step = 1, 
                                                                     animate = animationOptions(interval = 500, loop = FALSE)))),
                                  fluidRow(absolutePanel(plotOutput("line"), top = 80, left = 300,
                                                         width = 360, height = 300, draggable = TRUE)))),
                        #mapMarkers
                        tabItem(tabName = "Map",
                                fluidPage(
                                  fluidRow(column(4,
                                                  selectizeInput("boro1", "Choose the Borough",
                                                                 choices = c("Choose Boro(s)" = "",
                                                                             "BRONX", "BROOKLYN",
                                                                             "MANHATTAN", "QUEENS",
                                                                             "STATEN ISLAND"),
                                                                 multiple = T)),
                                           column(4,
                                                  dateInput("start_date", "Choose start date",
                                                            value = "2006-01-01",
                                                            min = min(Shooting_data$OCCUR_DATE),
                                                            max = max(Shooting_data$OCCUR_DATE))),
                                           column(4,
                                                  dateInput("end_date", "Choose end date",
                                                            value = "2018-12-31",
                                                            min = min(Shooting_data$OCCUR_DATE),
                                                            max = max(Shooting_data$OCCUR_DATE)))),
                                  fluidRow(column(4,
                                                  pickerInput("sex", 'Victim Gender',
                                                              choices = levels(Shooting_data$VIC_SEX),
                                                              options = list(`actions-box` = TRUE),
                                                              multiple = TRUE)),
                                           column(4,
                                                  pickerInput("age", 'Age group',
                                                              choices = levels(Shooting_data$AGE_GROUP),
                                                              options = list(`actions-box` = TRUE),
                                                              multiple = TRUE)),
                                           column(4,
                                                  pickerInput("race", 'Ethnicity',
                                                              choices = levels(Shooting_data$VIC_RACE),
                                                              options = list(`actions-box` = TRUE),
                                                              multiple = TRUE))),
                                  fluidRow(column(6,
                                                  plotOutput("plot2", height = "600px")),
                                           column(6, 
                                                  leafletOutput("mapMarker", height = "600px"))))),
                        
                        #overview
                        tabItem(tabName = "Report",
                                fluidPage(
                                  fluidRow(column(12,
                                                  h3("Interactive Dashboard"),
                                                  "By default, the bar chart shows the sum of segments by year as the height of each bar, and pie chart shows the percentage of total crime shootings in each borough.",
                                                  tags$div(tags$ul(
                                                    tags$li("Hover the mouse over a year bar in histogram will modify the pie chart and legend."),
                                                    tags$li("Hover the mouse over pie slice should change the histogram.")
                                                  )),
                                                  #htmlOutput("d3"))),
                                                  includeHTML("http://bl.ocks.org/wb2326/raw/d2e92fc05d7b437a7a3a56664e3e49ec/"))),
                                  fluidRow(column(width =  12, title = "Shooting Counts from 2006-2018 by year", 
                                                  plotlyOutput("years"))),
                                  fluidRow(column(width =  12, title = "Shooting Counts by season/year", 
                                                  plotlyOutput("seasons"))),
                                  fluidRow(column(width =  12, title = "Shootings Counts by weekday/year", 
                                                  plotlyOutput("weeks"))),
                                  fluidRow(column(width =  12, title = "Shootings Counts by Victims' sex/year", 
                                                  plotlyOutput("sexs"))),
                                  fluidRow(column(width =  12, title = "Shootings Counts by Victims' age/year", 
                                                  plotlyOutput("ages"))),
                                  fluidRow(column(width =  12, title = "Shootings Counts by Victims' race/year", 
                                                  plotlyOutput("races"))),
                                  fluidRow(column(width =  12, title = "Shootings Counts by boro/year", 
                                                  plotlyOutput("boros"))),
                                  fluidRow(column(width =  12, title = "Shootings Counts by murder/year", 
                                                  plotlyOutput("murders"))))),
                        
                        tabItem(tabName = "Holidays",
                                fluidPage(
                                  fluidRow(
                                    column(6,
                                           selectInput("choice2", 'Choose a holiday',
                                                       choices = c("Independence Day",
                                                                   "Halloween", "Christmas", "New Year"))),
                                    column(6,
                                           sliderInput("year1", 'Choose time', min = 2006, max = 2018,
                                                       step = 1, value = 2012))),
                                  fluidRow(plotOutput("plot3", height = "600px"))
                                )),
                        
                        #source
                        tabItem(tabName = "Source",
                                fluidPage(
                                  fluidRow(box(width = 15, title = "Data Source", status = "warning",
                                               solidHeader = TRUE, "The source data for this project is from", 
                                               tags$a(href = "https://data.cityofnewyork.us/Public-Safety/NYPD-Shooting-Incident-Data-Historic-/833y-fsy8", 
                                                      "NYC open data"), ".")),
                                  fluidRow(box(width = 15, title = "Project Code", status = "warning",
                                               solidHeader = TRUE, "The codes for this project are shared at",
                                               tags$a(href = "https://github.com/TZstatsADS/fall2019-proj2--sec2-grp10",
                                                      "Github"), "."))))
                      )))
