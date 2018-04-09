

library(shiny)
source("global.R")
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

x <- reactive({
  if(input$sex=='Both')
    dataSet <- patients
  else dataSet <- patients %>% filter(gender==input$sex)
  dataSet
})


output$histo <- renderPlotly({
  x <- req(x())
  varmean <- mean(x[[input$varble]])
  ggplotly(
    ggplot(x(), aes_string(x=input$varble))+
      geom_histogram(bins=input$bins,show.legend = FALSE,fill="#c0392b", alpha=0.75)+
      geom_vline(aes(xintercept=mean(get(input$varble))),size=1, linetype="dashed")+
      ggtitle(paste("Histogram of ",input$varble))+
      theme_economist()
    
  )
})

output$scatter <- renderPlotly({
  ggplotly(
    ggplot(patients, aes(x=height, y=weight, fill=gender))+
      geom_point()+
      geom_smooth()+
      ggtitle("Scatter Plot of Weight v/s Height")
  )
})

output$map <- renderLeaflet({
  locations <- coords %>% filter(68.1766451354<=lon & lon<=97.4025614766)
  leaflet(locations) %>% 
    addTiles(group = "OSM (default)") %>%
    addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
    addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
    addHeatmap(lng=~lon, lat=~lat,blur=30,max=0.05,radius=15,group = "Heatmap") %>% 
    addMarkers(clusterOptions = markerClusterOptions(),group = "Points") %>% 
    addLayersControl(
      baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
      overlayGroups = c("Heatmap", "Points"),
      options = layersControlOptions(collapsed = FALSE)
    )
})


  
})
