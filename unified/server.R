#unified server

shinyServer(function(input, output, session) {
  ## Dem_df is a reactive dataframe. Necessary for when summary/plot/map have common input (Multiple Variables). Not in this project
  
  uni_df <- reactive({
    unified_df <- unified       
    ## Output reactive dataframe
    unified_df
  })
  
  #selected county is necessary for when the user picks a variable that has only county data
  #they will still have the option to choose the municipality
  
  one_county <- reactive({
    df <- uni_df()
    as.character(unique(df$County[df$Municipal %in% input$one_muni]))
  })
  
  multi_county <- reactive({
    df <- uni_df()
    as.character(unique(df$County[df$Municipal %in% input$multi_muni]))
  })
  
  #output$counties <- renderPrint({c(one_county(), multi_county())})
  
  output$map <- renderLeaflet({
    leaflet(MA_map_muni) %>% addTiles() %>% fitBounds(-73.5,41, -70,43) %>%
      addPolygons(stroke = TRUE, smoothFactor = 0.3, fillOpacity = 1,
                  label = ~NAMELSAD10)
  })
  clrs <- c("")
  output$colplot <- renderPlot({
    ggplot(topics, aes(x=x, y=y, label=Labels))+ geom_point(size=4) +
      
      geom_label_repel(aes(x, y, fill= "dark red", label = Labels),
        fontface = 'bold', color = 'white',
        box.padding = unit(0.10, "lines"),
        point.padding = unit(0.15, "lines"),
        segment.color = 'grey50') +
      
    theme(panel.background = element_rect(fill = "white"),legend.position="none",
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(),
          axis.ticks = element_blank()) +
    
    facet_grid(~App)
    
    #adding ylim breaks the interactivity!
    })
  
#geom_text_repel(size=4)+ use instead of geomlabel repel 

selected_variable <- reactive({
  nearPoints(topics, input$plot_click, threshold = 10, maxpoints = 1, addDist = T)$Variables
})
selected_app <- reactive({
  nearPoints(topics, input$plot_click, threshold = 10, maxpoints = 1, addDist = T)$App
})
selected_label <- reactive({
  nearPoints(topics, input$plot_click, threshold = 10, maxpoints = 1, addDist = T)$Labels
})



output$tsplot <- renderPlot({
  
  sel_var <- as.character(selected_variable())
  sel_lab <- as.character(selected_label())
  
  df <- uni_df()
  col_selector <- which(names(df) %in% sel_var)
                              
  if(selected_app()=="Demographics"){
    
    
    plot_df <- df %>% filter(Municipal == input$one_muni) %>% select(1,60,col_selector, c(col_selector+1))
    names(plot_df)[3] <- "Var"
    p <- ggplot(plot_df, aes(x=Year, y=Var))+geom_point()+geom_line()+ylab(sel_lab) +
    theme(panel.background = element_rect(fill = "white"))
  }
  
  
  #validate(is.null(col_selector)==F, message = "Please select a variable")
  p
  
})

})