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
