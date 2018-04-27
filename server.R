

library(shiny)
source("global.R")
#source("Clustering.R")
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

############################################################
#Plots for demog tab
  
#Reactive table based on user selection of gender
x <- reactive({
  if(input$sex=='Both')
    dataSet <- patients
  else dataSet <- patients %>% filter(gender==input$sex)
  dataSet
})

#Histogram plot
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

#Scatter Plot
output$scatter <- renderPlotly({
  ggplotly(
    ggplot(patients, aes(x=height, y=weight, fill=gender))+
      geom_point()+
      geom_smooth()+
      ggtitle("Scatter Plot of Weight v/s Height")
  )
})

#Leaflet Plot
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

##############################################################################
#Plots for Initial Disease Tab

#Symptoms Plot

output$symptom <- renderPlotly({
  ggplotly(
    ggplot(Symptoms, aes(x=reorder(value,Count),y=Count,fill=gender))+geom_col()+coord_flip()+
      ggtitle("Distribution of Common Initial Symptoms")+
      theme(legend.position="bottom",
            legend.title = element_blank())+
      xlab(NULL),tooltip = "Count"
  )
})

#Drug Usage Plot
output$drugs <- renderPlotly({
  initDetail <- initDetail %>% filter(brandTKI!="" & nameTKI!="")
  initDetail$brandTKI[initDetail$brandTKI=='Imatinate,type="text"'] <- "Imatinate"
  x <- initDetail %>% count(brandTKI)
  ggplotly(
    ggplot(initDetail, aes(x=fct_rev(fct_infreq(brandTKI)), fill=nameTKI,
                           text=paste('Drug Brand :',brandTKI)))+geom_bar()+coord_flip()+
    ggtitle("Drug Brands being Administered")+xlab(NULL)+
      theme(legend.position="bottom",
            legend.title = element_blank()),
    tooltip = c("text")
      
  )
})

#Scatter plot Sokal?Hasford/EUTOS vs BCR
output$BCR <- renderPlot({
  ggplot(NewInitDetails, aes_string(x=NewInitDetails$BCR_ABL, y=input$Score,
                                    color=paste0(input$Score,"Risk")))+
    geom_point()+geom_smooth()+xlab("BCR_ABL")+
    ggtitle("Prognostic Scores compared with BCR-ABL at Patient Registration")
})

#Corr Plot
output$corr <- renderPlot({
  corrplot(cor.mat, type="upper", order="hclust", 
           tl.col="black", tl.srt=45)
})

#Corr Plot with hchart
output$corr <- renderHighchart({
  hchart(cor.mat) %>% hc_add_theme(hc_theme_economist()) %>% 
    hc_title(text="Correlation Matrix") %>% 
    hc_subtitle(text="Hover over the squares to read values")
})


  
})
