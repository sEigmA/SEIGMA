########################################
## Title: Poverty server.R            ##
## Author(s): Emily Ramos, Xuelian Li ##
##            Arvind Ramakrishnan,    ##
##            Jenna Kiridly           ## 
## Date Created:  02/24/2015          ##
## Date Modified: 03/05/2015 ER       ##
########################################

shinyServer(function(input, output, session) {
  ## labor_df is a reactive dataframe. Necessary for when summary/plot/map have common input (Multiple Variables). Not in this project
  labor_df <- reactive({
    #     browser()
    ## Filter the data by the chosen Five Year Range 
    labor_df <- labor %>%
      select(1:4, Five_Year_Range,Total_Pop, Pov_Pop, Percent_Pov, Margin_Error_Percent)
    
    ## Output reactive dataframe
    labor_df    
  })
  
  ## Create summary table
  output$summary <- renderDataTable({
    ## Make reactive dataframe into regular dataframe
    labor_df <- labor_df() %>%
    filter(Five_Year_Range == input$sum_year)
    
    ## make municipals a vector based on input variable
    if(!is.null(input$sum_muni))
      munis <- input$sum_muni
    ## if none selected, put all municipals in vector
    if(is.null(input$sum_muni))
      munis <- MA_municipals
    
    ## if the user checks the meanUS box or the meanMA box, add those to counties vector
    if(input$US_mean){
      if(input$MA_mean){
        munis <- c("United States", "MA", munis) ## US and MA  
      } else{
        munis <- c("United States", munis) ## US only
      }
    } else{
      if(input$MA_mean){
        munis <- c("MA", munis) ## US only ## MA only
      }
    }
    
    ## create a dataframe consisting only of counties in vector
    sum_df <- labor_df %>%
      filter(Region %in% munis) %>%
      select(4:length(colnames(labor_df)))
    
    colnames(sum_df) <- c("Region","Five Year Range", "Total Population", "Population Below Poverty Level", 
                          "Percent Below Poverty Level", "Margin of Error")
    
    return(sum_df)
  }, options = list(searching = FALSE, orderClasses = TRUE)) # there are a bunch of options to edit the appearance of datatables, this removes one of the ugly features
  
  ## create the plot of the data
  ## for the Google charts plot
  output$plot <- reactive({
    ## make reactive dataframe into regular dataframe
    labor_df <- labor_df() %>%
    filter(Five_Year_Range == input$plot_year)
    
    
    county <- as.character(labor_df$County[which(labor_df$Municipal==input$plot_muni)])
    
    ## make counties a vector based on input variable
    munis <- c(input$plot_muni, county, "MA", "United States")
    
    muni_index <- c()
    
    for(i in 1:length(munis)){
      muni_index[i] <- match(munis[i], labor_df$Region)
    }
    #     browser()
    plot_df <- labor_df[muni_index,] %>%
      select(Region, Percent_Pov)
    
    colnames(plot_df) <- gsub("Percent_Pov", "Poverty Rate", colnames(plot_df))
    
    #     plot_df[,"pop.html.tooltip"] <- paste0("$", prettyNum(plot_df[,2], big.mark = ","))
    
    list(
      data=googleDataTable(plot_df))
  })
  
  
  ###################MAP CREATION##############
  
  ## set map colors
  map_dat <- reactive({
    #     browser()
    ## Browser command - Stops the app right when it's about to break
    ## make reactive dataframe into regular dataframe
    labor_df <- labor_df()%>%
    filter(Five_Year_Range == input$map_year)%>%
    select(1:4, Total_Pop, Pov_Pop, Percent_Pov, Margin_Error_Percent)
    
    ## take US, MA, and counties out of map_dat
    map_dat <- labor_df %>%
      filter(!is.na(Municipal))
    
    ## assign colors to each entry in the data frame
    color <- as.integer(cut2(map_dat[,"Percent_Pov"],cuts=cuts))
    map_dat <- cbind.data.frame(map_dat, color)
    map_dat$color <- ifelse(is.na(map_dat$color), length(map_colors), 
                            map_dat$color)
    map_dat$opacity <- 0.7
    
    ## find missing counties in data subset and assign NAs to all values
    missing_munis <- setdiff(leftover_munis_map, map_dat$Region)
    missing_df <- data.frame(Municipal = missing_munis, County = NA, State = "MA", 
                             Region = missing_munis, Total_Pop = NA, Pov_Pop = NA,
                             Percent_Pov = NA, Margin_Error_Percent = NA,
                             color=length(map_colors), opacity = 0)
    na_munis <- setdiff(MA_municipals_map, map_dat$Region)
    na_df <- data.frame(Municipal = na_munis, County = NA, State = "MA", 
                        Region = na_munis, Total_Pop = NA, Pov_Pop = NA,
                        Percent_Pov = NA, Margin_Error_Percent = NA,
                        color=length(map_colors), opacity = 0.7)
    
    # combine data subset with missing counties data
    map_dat <- rbind.data.frame(map_dat, missing_df, na_df)
    map_dat$color <- map_colors[map_dat$color]
    return(map_dat)
  })
  
  values <- reactiveValues(selectedFeature=NULL, highlight=c())
  
  ## draw leaflet map
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
        x$features[[i]]$properties["Percent_Pov"] <- 
          map_dat[match(x$features[[i]]$properties$NAMELSAD10, map_dat$Region), "Percent_Pov"]
        ## Style properties
        x$features[[i]]$properties$style <- list(
          fill=TRUE, 
          ## Fill color has to be equal to the map_dat color and is matched by county
          fillColor = map_dat$color[match(x$features[[i]]$properties$NAMELSAD10, map_dat$Region)], 
          ## "#000000" = Black, "#999999"=Grey, 
          weight=1, stroke=TRUE, 
          opacity=map_dat$opacity[match(x$features[[i]]$properties$NAMELSAD10, map_dat$Region)], 
          color="#000000", 
          fillOpacity=map_dat$opacity[match(x$features[[i]]$properties$NAMELSAD10, map_dat$Region)])
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
      region <- evt$properties$NAMELSAD10
      values$selectedFeature["Percent_Pov"] <- map_dat[match(region, map_dat$Region), "Percent_Pov"]
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
    muni_value <- prettyNum(values$selectedFeature["Percent_Pov"], big.mark = ",")
    
    ## If clicked county has no crude rate, display a message
    if(muni_value == "NULL"){
      return(as.character(tags$div(
        tags$h5("Poverty Rate in", muni_name, "is not available for this timespan"))))
    }
    ## For a single year when county is clicked, display a message
    as.character(tags$div(
      tags$h4("Poverty Rate in", muni_name, " for ", input$map_year),
      tags$h5(muni_value, "%")
    ))
  })
  
  output$legend1 <- renderPlot({  
      paint.brush = colorRampPalette(colors=c("white", "darkmagenta"))
      cols <- paint.brush(length(map_colors)-1)
      leg_dat<- data_frame(y = seq(povmin.val, povmax.val, length.out = (length(map_colors)-1)), x = 1, col = cols)
      
      q<- ggplot(data = leg_dat) +
        geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
        scale_y_continuous(limits = c(povmin.val, povmax.val), breaks = round(seq(povmin.val, povmax.val, length.out = 5),1)) +
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