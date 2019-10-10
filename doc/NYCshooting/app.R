library(leaflet)
library(geojsonio)
library(lubridate)
library(rmapshaper)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(tidyverse)
library(plotly)
library(ggpubr)
library(gridExtra)
library(cowplot)
library(shinyWidgets)
library(htmlwidgets)
library(shinyjs)



library(shiny)
library(shinydashboard)

# animation
Shooting_data <- read.csv("NYPD_Shooting_modified1.csv")
Shooting_data$Numeric_time = as.numeric(hms(Shooting_data$OCCUR_TIME))/60
neighborhood_map <- geojsonio::geojson_read("Neighborhood Tabulation Areas.geojson",what = "sp")
neighborhood_map <- rmapshaper::ms_simplify(neighborhood_map)
neighborhood_vector <- neighborhood_map$ntaname
Animation_matrix <- matrix(rep(0,length(neighborhood_vector)*48),ncol = length(neighborhood_vector))
colnames(Animation_matrix) <- neighborhood_vector
for (i in 1:dim(Shooting_data)[1]) {
    time <- Shooting_data[i,"Numeric_time"]
    nta <- Shooting_data[i,"ntaname"]
    Animation_matrix[(time%/%30+1),nta] <- Animation_matrix[(time%/%30+1),nta]+1
}
Animation_array <- rowSums(Animation_matrix)
df.line_chart <- data.frame(Time = (0:47)/2, Amount = Animation_array)

bins <- c(0, 5, 10, 20, 50, 100)
pal <- colorBin("YlOrRd", bins = bins)

# change date format
Shooting_data$OCCUR_DATE<- format(as.Date(Shooting_data$OCCUR_DATE, format = "%m/%d/%Y"), "%Y-%m-%d")
Shooting_data$OCCUR_DATE <- as.Date(Shooting_data$OCCUR_DATE)
Shooting_data$YEAR <- year(Shooting_data$OCCUR_DATE)
Shooting_data$year <- as.factor(Shooting_data$YEAR)
Shooting_data <- rename(Shooting_data, AGE_GROUP = VIC_AGE_GROUP)

# map shapefile
st <- geojsonio::geojson_read("Neighborhood Tabulation Areas.geojson", what = "sp")

content <- paste("Date:",Shooting_data$OCCUR_DATE,"; ", 
                 "Time:",Shooting_data$OCCUR_TIME, "<br/>",
                 "Murder:",Shooting_data$STATISTICAL_MURDER_FLAG, "; ",
                 "Precinct:",Shooting_data$PRECINCT,"<br/>",
                 "Victim age:",Shooting_data$VIC_AGE_GROUP,"; ",
                 "Victim gender:",Shooting_data$VIC_SEX,"<br/>",
                 "Victim race:",Shooting_data$VIC_RACE)

# plotimage
plotimage = function(x){
    clean_age_shoot = x[x$PERP_AGE_GROUP %in% c("<18","18-24","25-44","45-64","65+","UNKNOWN"),]
    df_age_shoot = as.data.frame(prop.table(table(droplevels(clean_age_shoot$PERP_AGE_GROUP))))
    df_age_shoot = cbind(df_age_shoot,a = rep("shooter",nrow(df_age_shoot)))
    clean_age_vic = Shooting_data[Shooting_data$AGE_GROUP %in% c("<18","18-24","25-44","45-64","65+","UNKNOWN"),]
    df_age_vic = as.data.frame(prop.table(table(droplevels(clean_age_vic$AGE_GROUP))))
    df_age_vic = cbind(df_age_vic,a = rep("victim",nrow(df_age_vic)))
    df_age = rbind(df_age_shoot,df_age_vic)
    colnames(df_age) = c("AGE_GROUP","Proportion","Identity")
    g1 = ggplot(df_age, aes(x="", y=Proportion, fill= AGE_GROUP)) +
        geom_bar(stat="identity", width=1, color="white") +
        scale_fill_brewer(palette = "Pastel1")+
        coord_polar("y", start=0) +
        theme_void()+
        facet_wrap(~Identity)
    
    clean_sex_shoot = x[x$PERP_SEX %in% c("M","F","U"),]
    df_sex_shoot = as.data.frame(prop.table(table(droplevels(clean_sex_shoot$PERP_SEX))))
    df_sex_shoot = cbind(df_sex_shoot,a = rep("shooter",nrow(df_sex_shoot)))
    clean_sex_vic = Shooting_data[Shooting_data$VIC_SEX %in% c("M","F","U"),]
    df_sex_vic = as.data.frame(prop.table(table(droplevels(clean_sex_vic$VIC_SEX))))
    df_sex_vic = cbind(df_sex_vic,a = rep("victim",nrow(df_sex_vic)))
    df_sex = rbind(df_sex_shoot,df_sex_vic)
    colnames(df_sex) = c("SEX","Proportion","Identity")
    g2 = ggplot(df_sex, aes(x="", y=Proportion, fill=SEX)) +
        geom_bar(stat="identity", width=1, color="white") +
        scale_fill_brewer(palette = "Pastel1")+
        coord_polar("y", start=0) +
        theme_void()+
        facet_wrap(~Identity)
    
    clean_race_shoot = x[x$PERP_RACE !="",]
    df_race_shoot = as.data.frame(prop.table(table(droplevels(clean_race_shoot$PERP_RACE))))
    df_race_shoot = cbind(df_race_shoot,a = rep("shooter",nrow(df_race_shoot)))
    clean_race_vic = Shooting_data[Shooting_data$VIC_RACE != "",]
    df_race_vic = as.data.frame(prop.table(table(droplevels(clean_race_vic$VIC_RACE))))
    df_race_vic = cbind(df_race_vic,a = rep("victim",nrow(df_race_vic)))
    df_race = rbind(df_race_shoot,df_race_vic)
    colnames(df_race) = c("RACE","Proportion","Identity")
    g3 = ggplot(df_race, aes(x="", y=Proportion, fill=RACE)) +
        geom_bar(stat="identity", width=1, color="white") +
        scale_fill_brewer(palette = "Pastel1")+
        coord_polar("y", start=0) +
        theme_void()+
        facet_wrap(~Identity)
    
    plot_grid(g1,g2,g3,nrow = 3,ncol = 1,align = "v")
}



# report
## year
year_data <- Shooting_data %>% group_by(YEAR) %>% summarize(count = n())
yearTS <- ggplot(year_data, aes(YEAR, count))+
    geom_point()+
    geom_line(color = "orange")+
    ggtitle("Shooting Counts from 2006-2018 by year")+
    xlab("Year")+
    geom_label(aes(label = count)) 

## borough
year_boro <- Shooting_data %>% 
    group_by(year, BORO) %>% summarize(count = n()) 
boroTS <- ggplot(year_boro, aes(year, count, color = BORO))+
    geom_point()+
    geom_line(aes(group= BORO))+
    xlab("Year")+
    geom_text(aes(x = "2014", y = 700, label = "BROOKLYN", color = "BROOKLYN")) + 
    geom_text(aes(x = "2014", y = 500, label = "BRONX", color = "BRONX")) + 
    geom_text(aes(x = "2014", y = 280, label = "QUEENS", color = "QUEENS")) + 
    geom_text(aes(x = "2014", y = 190, label = "MANHATTAN", color = "MANHATTAN"))+
    geom_text(aes(x = "2014", y = 100, label = "STATEN ISLAND", color = "STATEN ISLAND")) + 
    theme(legend.position="none")+
    ggtitle("Shootings Counts by boro/year") 

## murder
year_murder <- Shooting_data %>% group_by(year, STATISTICAL_MURDER_FLAG) %>%
    summarize(count = n()) %>% rename(Murder = STATISTICAL_MURDER_FLAG)
murderTS <- ggplot(year_murder, aes(year, count, color = Murder))+
    geom_point()+
    geom_line(aes(group= Murder))+
    xlab("Year")+
    geom_text(aes(x = "2014", y = 1300, label = "NOT MURDER", color = "False"))+
    geom_text(aes(x = "2014", y = 400, label = "MURDER", color = "True")) + 
    theme(legend.position="none")+
    ggtitle("Shootings Counts by murder/year") 

## season
Shooting_data$OCCUR_SEASON<-quarters(Shooting_data$OCCUR_DATE)
season_year <- Shooting_data %>% 
    group_by(OCCUR_SEASON, year) %>% summarize(count = n())
seasonTS <- ggplot(season_year, aes(OCCUR_SEASON, count, color = year))+
    geom_line(aes(group = year))+
    ggtitle("Shooting Counts by season/year")+
    xlab("Season")+
    guides(fill = guide_legend(title = "Year")) 

## week
overall_data<- Shooting_data %>% group_by(OCCUR_DATE) %>% summarize(count = n())
weekTS <-ggplot(overall_data, aes(OCCUR_DATE, count))+
    geom_line()+
    facet_grid(.~wday(overall_data$OCCUR_DATE, label = TRUE))+
    ggtitle("Shootings Counts by weekday/year")+
    xlab("")

## sex
year_sex <- Shooting_data %>% group_by(year, VIC_SEX) %>%
    summarize(count = n())
sexTS <-ggplot(year_sex, aes(year, count, color = VIC_SEX))+
    geom_point()+
    geom_line(aes(group = VIC_SEX))+
    xlab("")+
    geom_text(aes(x = "2014", y = 1500, label = "MALE", color = "M"))+
    geom_text(aes(x = "2014", y = 250, label = "FEMALE", color = "F")) + 
    geom_text(aes(x = "2014", y = 10, label = "UNKNOWN", color = "U"))+
    theme(legend.position="none")+
    ggtitle("Shootings Counts by Victims' sex/year")

## age
year_age <- Shooting_data %>% group_by(YEAR, AGE_GROUP) %>%
    summarize(count = n())
ageTS <-ggplot(year_age, aes(YEAR, count, color = AGE_GROUP))+
    geom_point()+
    geom_line(aes(group = AGE_GROUP))+
    xlab("")+
    ggtitle("Shootings Counts by Victims' age/year")

## race
year_race <- Shooting_data %>% group_by(YEAR, VIC_RACE) %>%
    summarize(count = n())
raceTS <-ggplot(year_race, aes(YEAR, count, color = VIC_RACE))+
    geom_point()+
    geom_line(aes(group = VIC_RACE))+
    xlab("")+
    ggtitle("Shootings Counts by Victims' race/year")

# event
eventimage = function (x, i){
    data <- Shooting_data %>%
        filter(OCCUR_DATE >= as.Date(x[i, ]$start_date, format = "%Y-%m-%d")&
                   OCCUR_DATE <= as.Date(x[i, ]$end_date, format = "%Y-%m-%d")) %>%
        group_by(OCCUR_DATE) %>%
        summarize(count = n())
    g <- ggplot(data, aes(OCCUR_DATE, count)) +
        geom_label(aes(label = wday(OCCUR_DATE, label = TRUE))) +
        geom_line(color = "cornflowerblue") +
        xlab("")+
        scale_x_date(date_labels = "%b\n%d",
                     date_breaks = "1 day")
    
    start <- as.Date(x[i, ]$beginning, format = "%Y-%m-%d")
    end <- as.Date(x[i, ]$ending, format = "%Y-%m-%d")
    g+annotate("rect", xmin = start, xmax = end,
               ymin = -Inf, ymax = Inf, fill = "green",
               alpha = .2) +
        annotate("text", x = end + 0.4,
                 y = 10, label = x[i, ]$label,
                 color = "green", hjust = 0) +
        theme_classic()
}

christmas <- data.frame(
    "start_date" = c(
        "2006-12-20","2007-12-20","2008-12-20","2009-12-20","2010-12-20",
        "2011-12-20","2012-12-20","2013-12-20","2014-12-20",
        "2015-12-20","2016-12-20","2017-12-20","2018-12-20"),
    "end_date" = c(
        "2007-01-03","2008-01-03","2009-01-03","2010-01-03","2011-01-03",
        "2012-01-03","2013-01-03","2014-01-03","2015-01-03",
        "2016-01-03","2017-01-03","2018-01-03","2018-01-03"), 
    "beginning" = c(
        "2006-12-24","2007-12-24","2008-12-24","2009-12-24","2010-12-24",
        "2011-12-24","2012-12-24","2013-12-24","2014-12-24",
        "2015-12-24","2016-12-24","2017-12-24", "2018-12-24"),
    "ending" = c(
        "2006-12-27", "2007-12-27","2008-12-27","2009-12-27","2010-12-27",
        "2011-12-27","2012-12-27","2013-12-27","2014-12-27",
        "2015-12-27","2016-12-27","2017-12-27","2018-12-27"),
    "label" = rep("Dec 24 - Dec 27", 13)
)

new_year<-data.frame(
    "start_date" = c(
        "2006-12-22","2007-12-22","2008-12-22","2009-12-22","2010-12-22",
        "2011-12-22","2012-12-22","2013-12-22","2014-12-22",
        "2015-12-22","2016-12-22","2017-12-22","2018-12-22"),
    "end_date" = c(
        "2007-01-10","2008-01-10","2009-01-10","2010-01-10","2011-01-10",
        "2012-01-10","2013-01-10","2014-01-10","2015-01-10",
        "2016-01-10","2017-01-10","2018-01-10","2019-01-10"), 
    "beginning" = c(
        "2006-12-31","2007-12-31","2008-12-31","2009-12-31","2010-12-31",
        "2011-12-31","2012-12-31","2013-12-31","2014-12-31",
        "2015-12-31","2016-12-31","2017-12-31", "2018-12-28"),
    "ending" = c(
        "2007-01-03", "2008-01-03","2009-01-03","2010-01-03","2011-01-03",
        "2012-01-03","2013-01-03","2014-01-03","2015-01-03",
        "2016-01-03","2017-01-03","2018-01-03", "2019-01-03"),
    "label" = rep("Dec 31 - Jan 03",13)
)

Independence_day<-data.frame(
    "start_date" = c(
        "2006-06-30","2007-06-30","2008-06-30","2009-06-30","2010-06-30",
        "2011-06-30","2012-06-30","2013-06-30","2014-06-30",
        "2015-06-30","2016-06-30","2017-06-30","2018-06-30"),
    "end_date" = c(
        "2006-07-13","2007-07-13","2008-07-13","2009-07-13","2010-07-13",
        "2011-07-13","2012-07-13","2013-07-13","2014-07-13",
        "2015-07-13","2016-07-13","2017-07-13","2018-07-13"), 
    "beginning" = c(
        "2006-07-03","2007-07-03","2008-07-03","2009-07-03","2010-07-03",
        "2011-07-03","2012-07-03","2013-07-03","2014-07-03",
        "2015-07-03","2016-07-03","2017-07-03", "2018-07-03"),
    "ending" = c(
        "2006-07-06", "2007-07-06","2008-07-06","2009-07-06","2010-07-06",
        "2011-07-06","2012-07-06","2013-07-06","2014-07-06",
        "2015-07-06","2016-07-06","2017-07-06","2018-07-06"),
    "label" = rep("Jul 03 - Jul 06", 13)
)

Halloween<-data.frame(
    "start_date" = c(
        "2006-10-25","2007-10-25","2008-10-25","2009-10-25","2010-10-25",
        "2011-10-25","2012-10-25","2013-10-25","2014-10-25",
        "2015-10-25","2016-10-25","2017-10-25","2018-10-25"),
    "end_date" = c(
        "2006-11-03","2007-11-03","2008-11-03","2009-11-03","2010-11-03",
        "2011-11-03","2012-11-03","2013-11-03","2014-11-03",
        "2015-11-03","2016-11-03","2017-11-03","2018-11-03"), 
    "beginning" = c(
        "2006-10-30","2007-10-30","2008-10-30","2009-10-30","2010-10-30",
        "2011-10-30","2012-10-30","2013-10-30","2014-10-30",
        "2015-10-30","2016-10-30","2017-10-30", "2018-10-30"),
    "ending" = c(
        "2006-11-02", "2007-11-02","2008-11-02","2009-11-02","2010-11-02",
        "2011-11-02","2012-11-02","2013-11-02","2014-11-02",
        "2015-11-02","2016-11-02","2017-11-02","2018-11-02"),
    "label" = rep("Oct 30 - Nov 02", 13)
)

co <- function(cho) {
    if (cho == "Independence Day") return(Independence_day)
    if (cho == "Christmas") return(christmas)
    if (cho == "Halloween") return(Halloween)
    if (cho == "New Year") return(new_year)
}

    

                            
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
    

    filteredData8 <- reactive({
        if(is.null(input$boro1)){selected_boro = levels(Shooting_data$BORO)}
        else{selected_boro = input$boro1}
        
        if(is.null(input$sex)){selected_sex = levels(Shooting_data$VIC_SEX)}
        else{selected_sex = input$sex}
        
        if(is.null(input$race)){selected_race = levels(Shooting_data$VIC_RACE)}
        else{selected_race = input$race}
        
        if(is.null(input$age)){selected_age = levels(Shooting_data$AGE_GROUP)}
        else{selected_age = input$age}
        
        Shooting_data %>% filter(OCCUR_DATE >= input$start_date & OCCUR_DATE <= input$end_date) %>%
            filter(BORO %in% selected_boro) %>%
            filter(VIC_SEX %in% selected_sex) %>%
            filter(VIC_RACE %in% selected_race) %>%
            filter(AGE_GROUP %in% selected_age)
    })
    
    
    
    output$mapMarker <- renderLeaflet({
        leaflet(options = leafletOptions(minZoom = 8, maxZoom = 18)) %>%
            addTiles('http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png') %>%
            setView(lng = -73.95, lat = 40.72, zoom = 10) %>%
            addCircleMarkers(lng = Shooting_data$Longitude, lat = Shooting_data$Latitude,
                             popup = content,
                             clusterOptions = markerClusterOptions())
    })
    
    observe({
        df.marker = filteredData8()
        leafletProxy("mapMarker",data = df.marker) %>%
            fitBounds(lat1 = min(df.marker$Latitude), 
                      lng1 = min(df.marker$Longitude), 
                      lat2 = max(df.marker$Latitude), 
                      lng2 = max(df.marker$Longitude))%>%
            clearMarkerClusters()%>%
            clearPopups() %>%
            clearMarkers() %>%
            addCircleMarkers(lng = ~Longitude, lat = ~Latitude, 
                             popup = paste("Date:",df.marker$OCCUR_DATE,"; ",
                                           "Time:",df.marker$OCCUR_TIME, "<br/>",
                                           "Murder:",df.marker$STATISTICAL_MURDER_FLAG, "<br/>",
                                           "Victim age:",df.marker$AGE_GROUP,"<br/>",
                                           "Victim gender:",df.marker$VIC_SEX,"<br/>",
                                           "Victim race:",df.marker$VIC_RACE,"<br/>"),
                             clusterOptions = markerClusterOptions())
    })
    
 
    filteredData2 <- reactive({
        Shooting_data %>% filter(BORO %in% input$boro1 & OCCUR_DATE >= input$start_date & OCCUR_DATE <= input$end_date)
    })
    filteredData3 <- reactive({
        Shooting_data %>% filter(OCCUR_DATE >= input$start_date & OCCUR_DATE <= input$end_date)
    })
    
    output$plot2 <- renderPlot({
        if (input$start_date > input$end_date) {plot(NULL)}
        if (is.null(input$boro1) == TRUE) {plotimage(filteredData3())}
        else {plotimage(filteredData2())}
    })

    output$plot3 <- renderPlot({
        y <- input$year1 - 2005
        cho <- input$choice2
        eventimage(co(cho), y)
    })
    
    
    
    output$years <- renderPlotly({
        ggplotly(yearTS)
    })
    
    output$seasons <- renderPlotly({
        ggplotly(seasonTS)
    })
    
    output$weeks <- renderPlotly({
        ggplotly(weekTS)
    })
    
    output$sexs <- renderPlotly({
        ggplotly(sexTS)
    })
    
    output$ages <- renderPlotly({
        ggplotly(ageTS)
    })
    
    output$races <- renderPlotly({
        ggplotly(raceTS)
    })
    
    output$boros <- renderPlotly({
        ggplotly(boroTS)
    })
    
    output$murders <- renderPlotly({
        ggplotly(murderTS)
    })
    
    
 
}

shinyApp(ui = ui, server = server, options = list(height = 1080))