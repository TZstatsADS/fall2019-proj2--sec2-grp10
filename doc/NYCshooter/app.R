library(leaflet)
library(geojsonio)
library(lubridate)
library(rmapshaper)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(tidyverse)
library(plotly)

library(shiny)
library(shinydashboard)

# animation
Shooting_data <- read.csv("../../data/NYPD_Shooting_modified1.csv")
Shooting_data$Numeric_time = as.numeric(hms(Shooting_data$OCCUR_TIME))/60
neighborhood_map <- geojsonio::geojson_read("../../data/Neighborhood Tabulation Areas.geojson",what = "sp")
neighborhood_map <- rmapshaper::ms_simplify(neighborhood_map)
neighborhood_vector = neighborhood_map$ntaname
Animation_matrix = matrix(rep(0,length(neighborhood_vector)*48),ncol = length(neighborhood_vector))
colnames(Animation_matrix) = neighborhood_vector
for(i in 1:dim(Shooting_data)[1]){
    time = Shooting_data[i,"Numeric_time"]
    nta = Shooting_data[i,"ntaname"]
    Animation_matrix[(time%/%30+1),nta] = Animation_matrix[(time%/%30+1),nta]+1
}
Animation_array = rowSums(Animation_matrix)
df.line_chart = data.frame(Time = (0:47)/2,Amount = Animation_array)

bins <- c(0, 5, 10, 20, 50, 100)
pal <- colorBin("YlOrRd", bins = bins)

# map with markers
st <- geojsonio::geojson_read("../../data/Neighborhood Tabulation Areas.geojson", what = "sp")

content <- paste("Date:",Shooting_data$OCCUR_DATE,"; ", 
                 "Time:",Shooting_data$OCCUR_TIME, "<br/>",
                 "Statistical murder flag:",Shooting_data$STATISTICAL_MURDER_FLAG, "<br/>",
                 "Precinct:",Shooting_data$PRECINCT,"; ",
                 "Vic age:",Shooting_data$VIC_AGE_GROUP,"<br/>",
                 "Vic gender:",Shooting_data$VIC_SEX,"; ",
                 "Vic race:",Shooting_data$VIC_RACE,"<br/>")


# overview
# plots
Shooting_data$OCCUR_DATE<- format(as.Date(Shooting_data$OCCUR_DATE, format = "%m/%d/%Y"), "%Y-%m-%d")
Shooting_data$OCCUR_DATE <- as.Date(Shooting_data$OCCUR_DATE)
overall_data<- Shooting_data %>% group_by(OCCUR_DATE) %>% summarize(count = n())

## year
Shooting_data$OCCUR_YEAR <- year(Shooting_data$OCCUR_DATE)
year_data <- Shooting_data %>% group_by(OCCUR_YEAR) %>% summarize(count = n())
yearTS <- ggplot(year_data, aes(OCCUR_YEAR, count)) +
        geom_point() + geom_line(color = "grey50") +
        ggtitle("Shooting Counts from 2006-2018 by year") 

## borough
year_boro <- Shooting_data %>% 
    group_by(OCCUR_YEAR, BORO) %>% summarize(count = n())
boroTS <- ggplot(year_boro, aes(OCCUR_YEAR, count, color = BORO))+
    geom_point()+ geom_line(aes(group= BORO))+
    ggtitle("Shootings counts by boro/year") 


## season
Shooting_data$OCCUR_YEAR<-as.factor(Shooting_data$OCCUR_YEAR)
Shooting_data$OCCUR_SEASON<-quarters(Shooting_data$OCCUR_DATE)
season_year <- Shooting_data %>% 
    group_by(OCCUR_SEASON, OCCUR_YEAR) %>% summarize(count = n())
seasonTS <- ggplot(season_year, aes(OCCUR_SEASON, count, color = OCCUR_YEAR)) +
    geom_line(aes(group = OCCUR_YEAR)) + ggtitle("season/year") 

## week
Shooting_data$OCCUR_MONTH<-months(Shooting_data$OCCUR_DATE)
Shooting_data$OCCUR_WEEKDAY <-wday(Shooting_data$OCCUR_DATE, label = TRUE)
overall_data$OCCUR_YEAR <- year(overall_data$OCCUR_DATE)
overall_data$weekday<- wday(overall_data$OCCUR_DATE, label = TRUE)
overall <- overall_data %>% 
    group_by(OCCUR_YEAR, weekday) %>%
    summarize(count = n())
overall$OCCUR_YEAR <- as.factor(overall$OCCUR_YEAR)
weekTS <- ggplot(overall, aes(OCCUR_YEAR, count)) +
    geom_point()+ geom_line(aes(group = OCCUR_YEAR)) +
    facet_grid(. ~ weekday) + ggtitle("year/weekday") + ylim(35, 53) 



# choice
out <- function(choice) {
    if (choice == "Year") print(yearTS)
    if (choice == "Season") print(seasonTS)
    if (choice == "Week") print(weekTS)
    if (choice == "Borough") print(boroTS)
}
    

                            
#ui
ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "Main Title"),
                    dashboardSidebar(sidebarMenu(
                        menuItem("Home", tabName = "Home", icon = icon("home")),
                        menuItem("Animation", tabName = "Animation", icon = icon("train")),
                        menuItem("Map", tabName = "Map", icon = icon("map")),
                        menuItem("Insight", tabName = "Insight", icon = icon("layer-group"),
                                 menuSubItem("Overview", tabName = "Overview", icon = icon("industry")),
                                 menuSubItem("Events", tabName = "Events", icon = icon("table"))),
                        menuItem("Source", tabName = "Source", icon = icon("th"))
                    )),
                    dashboardBody(
                        tabItems(
                            #home
                            tabItem(tabName = "Home",
                                    fluidPage(
                                        fluidRow(
                                            box(width = 15, title = "Introduction", status = "warning",
                                                solidHeader = TRUE, h3("Main Title"), 
                                                h4("By Weijia Bao, Kanyan Chen, Tiantian Chu, Chang Xu, Qingyu Zhang"),
                                                "This interactive project provides ..... about NYC shootings...."))
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
                                        fluidRow(leafletOutput("mapMarker", height = "700px")))),
                            
                            #overview
                            tabItem(tabName = "Overview", 
                                    fluidPage(
                                        fluidRow(
                                            column(6,
                                                   selectInput("choice1", 'Choose a graph',
                                                               choices = c("Choose a graph" = "",
                                                                           "Year", "Season", "Week", "Borough"))),
                                            column(6)),
                                        fluidRow(plotOutput("plot1")))),
                            
                            #source
                            tabItem(tabName = "Source",
                                    fluidPage(
                                        fluidRow(box(width = 15, title = "Data Source", status = "warning",
                                                     solidHeader = TRUE, "The source data for this project is from", 
                                                     tags$a(href = "https://data.cityofnewyork.us/Public-Safety/NYPD-Shooting-Incident-Data-Historic-/833y-fsy8", 
                                                            "NYC open data"))),
                                        fluidRow(box(width = 15, title = "Project Code", status = "warning",
                                                     solidHeader = TRUE, "The codes for this project are shared at",
                                                     tags$a(href = "https://github.com/TZstatsADS/fall2019-proj2--sec2-grp10",
                                                            "Github")))
                                    ))
                        )
                    ))





# server
server <- function(input, output) {
    # animation
    filteredData <- reactive({
        as.vector(Animation_matrix[input$animation+1,])
    })
    filteredData1 <- reactive({
        df.line_chart[1:(input$animation+1),]
    })
    output$mapAct<-renderLeaflet({
        leaflet(options = leafletOptions(minZoom = 0, maxZoom = 18)) %>%
            addTiles('http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png') %>%
            setView(lng = -73.99, lat = 40.72, zoom = 11) %>%
            addLegend(pal = pal,values = bins, position = "topright")
    })
    observe({
        leafletProxy("mapAct") %>%
            addPolygons(data = neighborhood_map, stroke = TRUE, weight = 1, color = "pink", 
                        fillOpacity = .4, fillColor = pal(filteredData()))
        output$line <- renderPlot({
            p = ggplot(filteredData1(), aes(x = Time,y = Amount, group = 1))
            p + geom_line() + 
                scale_x_continuous(limits = c(0,24), expand = c(0,0)) + 
                scale_y_continuous(limits = c(0,1000), expand = c(0,0)) + 
                xlab("Time(hour)") + theme_bw() +
                theme_wsj()+ scale_colour_wsj("colors6")
            })
         })
    
    # mapMarker
    output$mapMarker <- renderLeaflet({
        leaflet(st, options = leafletOptions(minZoom = 0, maxZoom = 18)) %>%
            addTiles('http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png') %>%
            setView(lng = -73.95, lat = 40.72, zoom = 11) %>%
            addCircleMarkers(lng = ~Shooting_data$Longitude, lat = ~Shooting_data$Latitude, 
                             popup = content, clusterOptions = markerClusterOptions())
    })
    
    #overview
    
    output$plot1 <- renderPlot({
        choice <- input$choice1
        out(choice)
    })
   
    
}

shinyApp(ui = ui, server = server)