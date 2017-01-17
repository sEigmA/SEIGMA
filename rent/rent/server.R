#######################################
## Title: RENT   server.R            ##
## Author(s): JWB, BF                ## 
## Date Created:  12/01/2016         ##
#######################################

shinyServer(function(input, output, session) {
  ## rent_df is a reactive dataframe. Necessary for when summary/plot/map have common input (Multiple Variables). Not in this project
  rent_df <- reactive({
    ## Filter the data by the chosen Five Year Range 
    rent_df <- rent %>%
      filter(Five.Year.Range == input$sum_year) %>%
      select(c(1:5))

    ## Output reactive dataframe
    rent_df    
  })
  
  ## Create summary table
  output$summary <- renderDataTable({
    ## Make reactive dataframe into regular dataframe
    rent_df <- rent_df()
    
    ## make municipals a vector based on input variable
    if(!is.null(input$sum_muni))
      munis <- input$sum_muni
    ## if none selected, put all municipals in vector
    if(is.null(input$sum_muni))
      munis <- MA_municipals
    
    ## if the user checks the meanUS box or the meanMA box, add those to counties vector
    if(input$US_mean){
      if(input$MA_mean){
        munis <- c("USA", "MA", munis) ## US and MA  
      } else{
        munis <- c("USA", munis) ## US only
      }
    } else{
      if(input$MA_mean){
        munis <- c("MA", munis) ## US only ## MA only
      }
    }
    #     browser( )
    ## create a dataframe consisting only of counties in vector
    rent_df <- rent_df %>%
      filter(Municipal %in% munis) %>%
      select(c(2,1,3,4,5))
    
    colnames(rent_df) <- gsub(".", " ", colnames(rent_df), fixed=T)
    
    
#     rent_df[,3] <- prettyNum(rent_df[,3], big.mark=",")
#     rent_df[,4] <- prettyNum(rent_df[,4], big.mark=",")
#     
    return(rent_df)
  }, options = list(searching = FALSE, orderClasses = TRUE)) # there are a bunch of options to edit the appearance of datatables, this removes one of the ugly features
  
  
#   
  ## create the plot of the data

  plot_rent_df <- reactive({
    munis_p<-input$plot_muni
    
    if(input$US_mean_p){
      if(input$MA_mean_p){
        munis_p <- c("USA", "MA", munis_p) ## US and MA  
      } else{
        munis_p <- c("USA", munis_p) ## US only
      }
    } else{
      if(input$MA_mean_p){
        munis_p <- c("MA", munis_p) ## US only ## MA only
      }
    }
    
    ## Filter the data by the chosen Five Year Range
    if(is.null(munis_p)){munis_p <-"MA"}
      
    plot_rent_df <- rent %>%
      filter(Municipal %in% munis_p) %>%
      select(c(1,3,4,5)) 
    # %>%
    #   spread(Municipal, Median.Rent)
    # 
    plot_rent_df$Year <- as.numeric(sapply(strsplit(as.character(plot_rent_df$Five.Year.Range), split="-"), FUN=function(x){x[1]}))+2
    
    
    #add ribbons
    #plot_rent_df_e <- rent %>%
    #  filter(Municipal %in% munis_p) %>%
    #  select(c(1,3,5)) %>%
    #  spread(Municipal, Rent.Margin.of.Error)
    #plot_rent_df_e<-plot_rent_df_e[,-1]
    #names(plot_rent_df_e)<-paste(names(plot_rent_df_e), "error", sep="_")
    #plot_rent_df<-cbind(plot_rent_df, plot_rent_df_e)
    
    ## Output reactive dataframe
    plot_rent_df
  })

  
  
  
  
  
  ## for the Google charts plot
  output$plot <- renderPlot({
    ## make reactive dataframe into regular dataframe
    
# 
#     county <- as.character(plot_rent_df$County[which(plot_rent_df$Municipal==input$plot_muni)])
# 
#     ## make counties a vector based on input variable
#     munis <- c(input$plot_muni, county, "MA", "United States")
# 
#     muni_index <- c()
# 
#     for(i in 1:length(munis)){
#       muni_index[i] <- match(munis[i], plot_rent_df$Region)
#     }
#     #     browser()
#     plot_df <- plot_rent_df[muni_index,] %>%
#       select(Region, Median.Rent)
# 
#     colnames(plot_df) <- gsub("_", " ", colnames(plot_df))

    #     plot_df[,"pop.html.tooltip"] <- paste0("$", prettyNum(plot_df[,2], big.mark = ","))

    # list(
    #   data=googleDataTable(plot_rent_df()))
    # 
    pdf <- plot_rent_df()
    ap=0.5
    sz=1
    
    p=ggplot(pdf, aes(x=Year, y=Median.Rent, colour=Municipal))+
      geom_errorbarh(aes(xmax = Year + 2, xmin = Year - 2, height = 0,colour=Municipal),alpha=ap/2, size=sz/2)+
      geom_errorbar(aes(ymin = Median.Rent-Rent.Margin.of.Error, ymax = Median.Rent+Rent.Margin.of.Error,colour=Municipal),alpha=ap,size=sz, width=0.125)+
      ylab("Median Rent ($)")+
      scale_color_manual(values=cbbPalette, guide="legend")+
      geom_point(aes(colour=Municipal),size=4,alpha=1)+
      geom_line(aes(colour=Municipal),size=2,alpha=1)+
      theme_bw() + 
      theme(plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank() )+
      theme(panel.border= element_blank())+
      theme(axis.line.x = element_line(color="black", size = 0.5),
            axis.line.y = element_line(color="black", size = 0.5))+
      theme(axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14))+
      theme(legend.title=element_text(size=16),
            legend.text=element_text(size=14))

      #guides(colour = guide_legend(override.aes = list(colour = NA)))+
      #guides(colour = guide_legend(override.aes = list(colour = cbbPalette[1:length(unique(pdf$Municipal))])))
    p
    
    
  })
  
  ##### interactive stuff on the plot
  # output$event <- renderPrint({
  #   d <- event_data("plotly_hover")
  #   if (is.null(d)) "Hover on a point!" else d
  # })
  # 

  #####################################MAP CREATION##############

  map_rent_df <- reactive({
    ## Filter the data by the chosen Five Year Range
    map_rent_df <- rent %>%
      filter(Five.Year.Range == input$map_year) %>%
      select(1:4, Five.Year.Range, Median.Rent, Rent.Margin.of.Error)

    ## Output reactive dataframe
    map_rent_df
  })


  ## set map colors
  map_dat <- reactive({

    ## Browser command - Stops the app right when it's about to break
    ## make reactive dataframe into regular dataframe
    map_rent_df <- map_rent_df()

    ## take US, MA, and counties out of map_dat
    map_dat <- map_rent_df %>%
      filter(!is.na(Municipal))

    ## assign colors to each entry in the data frame
    color <- as.integer(cut2(map_dat[,"Median.Rent"],cuts=cuts))
    map_dat <- cbind.data.frame(map_dat, color)
    map_dat$color <- ifelse(is.na(map_dat$color), length(map_colors),
                            map_dat$color)
    map_dat$opacity <- 0.7

    ## find missing counties in data subset and assign NAs to all values
    missing_munis <- setdiff(leftover_munis_map, map_dat$Municipal)
    missing_df <- data.frame(Municipal = missing_munis, County = NA, Five.Year.Range = input$map_year,
                             Median.Rent = NA, Rent.Margin.of.Error = NA,
                             color=length(map_colors), opacity = 0)

    na_munis <- setdiff(MA_municipals_map, map_dat$Municipal)
    na_munis <- na_munis[na_munis!= "County subdivisions not defined"]
    na_df <- data.frame(Municipal = na_munis, County = NA, Five.Year.Range = input$map_year,
                        Median.Rent = NA, Rent.Margin.of.Error = NA,
                        color=length(map_colors), opacity = 0.7)


    # combine data subset with missing counties data
    map_dat <- rbind.data.frame(map_dat, missing_df, na_df)
    map_dat$color <- map_colors[map_dat$color]
    return(map_dat)
  })

  values <- reactiveValues(selectedFeature=NULL, highlight=c())

  #############################################
  # observe({
  #   values$highlight <- input$map_shape_mouseover$id
  #   #browser()
  # })

  # # Dynamically render the box in the upper-right
  # output$countyInfo <- renderUI({
  #
  #   if (is.null(values$highlight)) {
  #     return(tags$div("Hover over a county"))
  #   } else {
  #     # Get a properly formatted state name
  #     countyName <- names(MAcounties)[values$highlight]
  #     return(tags$div(
  #       tags$strong(countyName),
  #       tags$div(density[countyName], HTML("people/m<sup>2</sup>"))
  #     ))
  #   }
  # })

  # lastHighlighted <- c()
  # # When values$highlight changes, unhighlight the old state (if any) and
  # # highlight the new state
  # observe({
  # #   if (length(lastHighlighted) > 0)
  # #     drawStates(getStateName(lastHighlighted), FALSE)
  #   lastHighlighted <<- values$highlight
  #
  #   if (is.null(values$highlight))
  #     return()
  #
  #   isolate({
  #     drawStates(getStateName(values$highlight), TRUE)
  #   })
  # })

  ###########################################

  # draw leaflet map
  map <- createLeafletMap(session, "map")

  ## the functions within observe are called when any of the inputs are called

  ## Does nothing until called (done with action button)
  observe({
    input$action

    ## load in relevant map data
    map_dat <- map_dat()

    ## All functions which are isolated, will not run until the above observe function is activated
    isolate({
      ## Duplicate MAmap to x
      x <- MA_map_muni
      #     browser()
      ## for each county in the map, attach the Crude Rate and colors associated
      for(i in 1:length(x$features)){
        ## Each feature is a county
        x$features[[i]]$properties["Median.Rent"] <-
          map_dat[match(x$features[[i]]$properties$NAMELSAD10, map_dat$Municipal), "Median.Rent"]
        ## Style properties
        x$features[[i]]$properties$style <- list(
          fill=TRUE,
          ## Fill color has to be equal to the map_dat color and is matched by county
          fillColor = map_dat$color[match(x$features[[i]]$properties$NAMELSAD10, map_dat$Municipal)],
          ## "#000000" = Black, "#999999"=Grey,
          weight=1, stroke=TRUE,
          opacity=map_dat$opacity[match(x$features[[i]]$properties$NAMELSAD10, map_dat$Municipal)],
          color="#000000",
          fillOpacity=map_dat$opacity[match(x$features[[i]]$properties$NAMELSAD10, map_dat$Municipal)])
      }

      map$addGeoJSON(x) # draw map
    })
  })

  observe({
    ## EVT = Mouse Click
    evt <- input$map_click
    if(is.null(evt))
      return()

    isolate({
      values$selectedFeature <- NULL
    })
  })

  observe({
    evt <- input$map_geojson_click
    if(is.null(evt))
      return()
    map_dat <- map_dat()
    isolate({
      values$selectedFeature <- evt$properties
      clickmuni <- evt$properties$NAMELSAD10
      values$selectedFeature["Median.Rent"] <- map_dat[match(clickmuni, map_dat$Municipal), "Median.Rent"]
      values$selectedFeature["Rent.Margin.of.Error"] <- map_dat[match(clickmuni, map_dat$Municipal), "Rent.Margin.of.Error"]
    })
  })
  ##  This function is what creates info box
  output$details <- renderText({

    ## Before a county is clicked, display a message
    if(is.null(values$selectedFeature)){
      return(as.character(tags$div(
        tags$div(
          h4("Click on a town or city"))
      )))
    }
    #     browser()
    muni_name <- values$selectedFeature$NAMELSAD10
    muni_value <- prettyNum(values$selectedFeature["Median.Rent"], big.mark = ",")
    muni_margin<- prettyNum(values$selectedFeature["Rent.Margin.of.Error"], big.mark = ",")

    ## If clicked county has no crude rate, display a message
    if(muni_value == "NA"){
      return(as.character(tags$div(
        tags$h5("Median Annual Household Income in ", muni_name, "is not available for this timespan"))))
    }
    ## For a single year when county is clicked, display a message
    as.character(tags$div(
      tags$h4("Median Annual Household Income in ", muni_name, " for ", input$map_year),
      tags$h5("$",muni_value, "+-", muni_margin)
    ))
  })

  output$legend1 <- renderPlot({
    paint.brush = colorRampPalette(colors=c("white", "#009E73"))
    cols <- paint.brush(length(map_colors)-1)
    leg_dat<- data_frame(y = seq(min_val, max_val, length.out = (length(map_colors)-1)), x = 1, col = cols)

    q<- ggplot(data = leg_dat) +
      geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
      scale_y_continuous(limits = c(min_val, max_val), breaks = round(seq(min_val, max_val, length.out = 5),0)) +
      scale_fill_manual(values = leg_dat$col) + theme_bw() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.x = element_blank(),
            panel.border = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank())

    return(q)

  })

})