#######################################
## Title: Income server.R            ##
## Author(s): Emily Ramos, Arvind    ##
##            Ramakrishnan, Jenna    ##
##            Kiridly, Steve Lauer   ## 
## Date Created:  11/05/2014         ##
## Date Modified: 02/24/2014 ER      ##
#######################################

shinyServer(function(input, output, session) {
  ## inc_df is a reactive dataframe. Necessary for when summary/plot/map have common input (Multiple Variables). Not in this project
  inc_df <- reactive({
    ## Filter the data by the chosen Five Year Range 
    inc_df <- inc_data %>%
      filter(Five_Year_Range == input$year) %>%
      select(1:4, Five_Year_Range, Median_Household_Income, Margin_Error_Median)
    
    ## Output reactive dataframe
    inc_df    
  })
  
  ## Create summary table
  output$summary <- renderDataTable({
    ## Make reactive dataframe into regular dataframe
    inc_df <- inc_df()
    
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
    #     browser( )
    ## create a dataframe consisting only of counties in vector
    inc_df <- inc_df %>%
      filter(Region %in% munis) %>%
      select(4:length(colnames(inc_df)))
    
    colnames(inc_df) <- gsub("_", " ", colnames(inc_df))
    
    inc_df[,3] <- prettyNum(inc_df[,3], big.mark=",")
    inc_df[,4] <- prettyNum(inc_df[,4], big.mark=",")
    
    return(inc_df)
  }, options = list(searching = FALSE, orderClasses = TRUE)) # there are a bunch of options to edit the appearance of datatables, this removes one of the ugly features
  
  ## create the plot of the data
  ## for the Google charts plot
  output$plot <- reactive({
    ## make reactive dataframe into regular dataframe
    inc_df <- inc_df()
    
    county <- as.character(inc_df$County[which(inc_df$Municipal==input$plot_muni)])
    
    ## make counties a vector based on input variable
    munis <- c(input$plot_muni, county, "MA", "United States")
    
    muni_index <- c()
    
    for(i in 1:length(munis)){
      muni_index[i] <- match(munis[i], inc_df$Region)
    }
#     browser()
    plot_df <- inc_df[muni_index,] %>%
      select(Region, Median_Household_Income)
    
    colnames(plot_df) <- gsub("_", " ", colnames(plot_df))
    
#     plot_df[,"pop.html.tooltip"] <- paste0("$", prettyNum(plot_df[,2], big.mark = ","))
    
    list(
      data=googleDataTable(plot_df))
  })
  
  
  #####################################MAP CREATION##############
  
  ## set map colors
  map_dat <- reactive({
    
    ## Browser command - Stops the app right when it's about to break
    ## make reactive dataframe into regular dataframe
    inc_df <- inc_df()
    
    ## take US, MA, and counties out of map_dat
    map_dat <- inc_df %>%
      filter(!is.na(Municipal))
    
    ## assign colors to each entry in the data frame
    color <- as.integer(cut2(map_dat[,"Median_Household_Income"],cuts=cuts))
    map_dat <- cbind.data.frame(map_dat, color)
    map_dat$color <- ifelse(is.na(map_dat$color), length(map_colors), 
                            map_dat$color)
    map_dat$opacity <- 0.7
    
    ## find missing counties in data subset and assign NAs to all values
    missing_munis <- setdiff(leftover_munis_map, map_dat$Region)
    missing_df <- data.frame(Municipal = missing_munis, County = NA, State = "MA", 
                             Region = missing_munis, Five_Year_Range = input$year, 
                             Median_Household_Income = NA, Margin_Error_Median = NA,
                             color=length(map_colors), opacity = 0)
    
    na_munis <- setdiff(MA_municipals_map, map_dat$Region)
    na_df <- data.frame(Municipal = na_munis, County = NA, State = "MA", 
                             Region = na_munis, Five_Year_Range = input$year, 
                             Median_Household_Income = NA, Margin_Error_Median = NA,
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
        x$features[[i]]$properties["Median_Household_Income"] <- 
          map_dat[match(x$features[[i]]$properties$NAMELSAD10, map_dat$Region), "Median_Household_Income"]
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
      values$selectedFeature["Median_Household_Income"] <- map_dat[match(region, map_dat$Region), "Median_Household_Income"]
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
    muni_value <- prettyNum(values$selectedFeature["Median_Household_Income"], big.mark = ",")
    
    ## If clicked county has no crude rate, display a message
    if(muni_value == "NULL"){
      return(as.character(tags$div(
        tags$h5("Median Household Income in ", muni_name, "is not available for this timespan"))))
    }
    ## For a single year when county is clicked, display a message
    as.character(tags$div(
      tags$h4("Median Household Income in ", muni_name, " for ", input$year),
      tags$h5("$",muni_value)
    ))
  })
  
})