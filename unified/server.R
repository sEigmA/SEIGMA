#unified server

shinyServer(function(input, output, session) {
  ## Dem_df is a reactive dataframe. Necessary for when summary/plot/map have common input (Multiple Variables). Not in this project
  
  unified_df <- reactive({
    unified_df <- unified       
    ## Output reactive dataframe
    unified_df
   

  })

  munis_p <- reactive({
    
    munis_p2 <- input$one_muni
    
    #MA
    if(input$MA_mean==T && any(grepl(x=munis_p2, pattern = "MA"))==F){
      return(c("MA", munis_p2[!(munis_p2 =="MA")])) ## US only ## MA only
    }else if(input$MA_mean==T && any(grepl(x=munis_p2, pattern = "MA"))==T){
      return(c("MA", munis_p2[!(munis_p2 =="MA")])) ## US only ## MA only
    }
    else if(input$MA_mean==F && any(grepl(x=munis_p2, pattern = "MA"))==T){
      return(munis_p2[!(munis_p2 =="MA")]) ## remove MA
    } else if(input$MA_mean==F && any(grepl(x=munis_p2, pattern = "MA"))==F){
      return(munis_p2[!(munis_p2 =="MA")]) ## remove MA
    }
  })
  
  munis_pfinal <- reactive({
    munis_p3 <- munis_p()
    #AMERICA FWURST
    if(input$US_mean==T && any(grepl(x=munis_p3, pattern = "United States"))==F){
      return(c("United States", munis_p3[!(munis_p3 =="United States")])) ##  United States
    }else if(input$US_mean==T && any(grepl(x=munis_p3, pattern = "United States"))==T){
      return(c("United States", munis_p3[!(munis_p3 =="United States")])) ## US  United States
    }
    else if(input$US_mean==F && any(grepl(x=munis_p3, pattern = "United States"))==T){
      return(munis_p3[!(munis_p3 =="United States")]) ## remove United States
    } else if(input$US_mean==F && any(grepl(x=munis_p3, pattern = "United States"))==F){
      return(munis_p3[!(munis_p3 =="United States")]) ## remove  United States
    }
    
    
  })
  
  
  #selected county is necessary for when the user picks a variable that has only county data
  #they will still have the option to choose the municipality
  

  # one_county <- reactive({
  #   df <- unified_df()
  #   as.character(unique(df$County[df$Municipal %in% input$one_muni]))
  # })
  # 
  # multi_county <- reactive({
  #   df <- unified_df()
  #   as.character(unique(df$County[df$Municipal %in% input$multi_muni]))
  # })
  # 
  # output$counties <- renderPrint({c(one_county(), multi_county())})
  
  output$map <- renderLeaflet({
    leaflet(MA_map_muni) %>% addTiles() %>% fitBounds(-73.5,41, -70,43) %>%
      addPolygons(stroke = TRUE, smoothFactor = 0.3, fillOpacity = 1,
                  label = ~NAMELSAD10)
  })
  clrs <- c("")
  output$colplot <- renderPlot({
    ggplot(topics, aes(x=x, y=y, label=Labels))+ 
      # geom_point() +
      geom_label(colour="dark red", check_overlap=TRUE) +
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
  
  plot_uni_df <- reactive({
    
  selmun <- munis_pfinal()
  plot_uni_df <- unified %>%
    filter(Region %in% selmun)
  return(plot_uni_df[order(match(plot_uni_df$Region, selmun)),])
  
  })

  
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
  
 df <- plot_uni_df()
  
  #     browser( )

  col_selector <- which(names(df) %in% sel_var)
                              
  if(selected_app()=="Demographics"){
    
  
    plot_df <- df %>% 
        select(4,60,col_selector, c(col_selector+1))
                             names(plot_df)[3] <- "Var"
    
                             p <- ggplot(plot_df, aes(x=Year, y=Var, colour=Region))+ geom_point()+ geom_line() + ylab(sel_lab) +
    theme(panel.background = element_rect(fill = "white"))
  }
  
  #validate(is.null(col_selector)==F, message = "Please select a variable")
  p
  
})

})