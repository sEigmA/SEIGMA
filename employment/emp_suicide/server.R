#######################################
## Title: Employment server.R          ##
## Author(s): Emily Ramos, Arvind    ##
##            Ramakrishnan, Jenna    ##
##            Kiridly, Steve Lauer   ## 
## Date Created:  01/07/2015         ##
## Date Modified: 01/08/2015         ##
#######################################

shinyServer(function(input, output, session) {
  # emp_df is a reactive dataframe. Necessary for when summary/plot/map have common input (Multiple Variables). Not in this project
  emp_df <- reactive({
    
    ## Filter the data by the chosen Year 
    emp_df <- empdata %>%
      filter(Year == input$year) %>%
      select(1:4, 6:9)
    
    ## Output reactive dataframe
    emp_df    
  })
  
  ## Create summary table
  output$summary <- renderDataTable({
    ## Make reactive dataframe into regular dataframe
    emp_df <- emp_df()
    
    ## if a user chooses Single Year, display only data from that year (dpylr)
    if(input$timespan == "sing.yr"){
      df <- filter(emp_df, Year==input$year)
    }
    
    ## if a user chooses Multiple Years, display data from all years in range
    if(input$timespan == "mult.yrs"){
      range <- seq(min(input$range), max(input$range), 1)
      df <- c()
      
      ####**********RBIND.Data.frame -DO Not Match
      for(i in 1:length(range)){
        bbb <- subset(emp_df, Year==range[i])
        df <- rbind.data.frame(df, bbb)
      }
    }
    
    
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
    sum_df <- emp_df %>%
      filter(Region %in% munis) %>%
      select(4:length(colnames(emp_df)))
    
    colnames(sum_df) <- c("Municipal", "State", "Year", "Region", "Industry Title", "Average Monthly Employment")
    
    return(sum_df)
  }, options = list(searching = FALSE, orderClasses = TRUE)) # there are a bunch of options to edit the appearance of datatables, this removes one of the ugly features
  
  ##############################################  
  
  ## create the plot of the data
  ## for the Google charts plot
  output$plot <- reactive({
    ## make reactive dataframe into regular dataframe
    emp_df <- emp_df()
    
    ## make region a vector based on input variable
    munis <- input$munis
    
    ## put data into form that googleCharts understands (this unmelts the dataframe)
    df <- dcast(emp_df, Year ~ munis, value.var="Average_Monthly_Employment")
    
    ## if no counties have been selected, just show the US average
    if(is.null(input$region)){
      ## %>% = then
      g <- df %>%
        select(Year, US)
    }
    
    ## if counties are selected and MA or US mean boxes are selected, add those to dataframe
    if(!is.null(input$munis)){
      if(input$meanMA)
        munis <- c(munis, "MA")
      if(input$meanUS)
        munis <- c(munis, "US")
      
      g <- df[,c("Year", munis)]
    }
    
    ## this outputs the google data to be used in the UI to create the dataframe
    list(
      data=googleDataTable(g))
  })
  
  #################################################  
  #   ## for the Google charts plot
  #   output$plot <- reactive({
  #     ## make reactive dataframe into regular dataframe
  #     emp_df <- emp_df()
  #     
  #     county <- as.character(emp_df$County[which(emp_df$Municipal==input$plot_muni)])
  #     
  #     ## make counties a vector based on input variable
  #     munis <- c(input$plot_muni, county, "MA", "United States")
  #     
  #     muni_index <- c()
  #     
  #     for(i in 1:length(munis)){
  #       muni_index[i] <- match(munis[i], emp_df$Region)
  #     }
  #     
  #     plot_df <- emp_df[muni_index,] %>%
  #       select(Region, Percent_Vet)
  #     
  #     colnames(plot_df) <- gsub("_", " ", colnames(plot_df))
  #     
  # #   plot_df[,"pop.html.tooltip"] <- paste0("$", prettyNum(plot_df[,2], big.mark = ","))
  #     
  #     list(
  #       data=googleDataTable(plot_df))
  #   })
  
  ###################MAP CREATION##############
  
  ## set map colors
  map_dat <- reactive({
    ## Browser command - Stops the app right when it's about to break
    ## make reactive dataframe into regular dataframe
    emp_df <- emp_df()
    
    ## take US, MA, and counties out of map_dat
    map_dat <- emp_df %>%
      filter(!is.na(Municipal))
    ######################################################    
    ## for single year maps...
    if(input$timespan == "sing.yr"){
      
      ## subset the data by the year selected
      emp_df <- filter(emp_df, Year==input$year)
      
      ## assign colors to each entry in the data frame
      color <- as.integer(cut2(emp_df$Average_Monthly_Employment,cuts=scuts))
      emp_df <- cbind.data.frame(emp_df,color)
      emp_df$color <- ifelse(is.na(emp_df$color), length(smap.colors), 
                               emp_df$color)
      #       ## This line is important. Formats county name (ie Franklin County)
      #       suidf$County <- paste(as.character(suidf$County), "County")
      
      ## find missing counties in data subset and assign NAs to all values
      missing.munis <- setdiff(MAmunis, emp_df$Municipal)
      df <- data.frame(Municipal=missing.munis, County=County, State="MA", Country="US", 
                       Year=input$year, Average_Monthly_Employment=NA,
                       color=length(smap.colors))
      
      ## combine data subset with missing counties data
      emp_df <- rbind.data.frame(emp_df, df)
      emp_df$color <- smap.colors[emp_df$color]
      return(emp_df)
    } 
    
    if(input$timespan=="mult.yrs"){
      
      ## create dataframes for the max and min year of selected data
      min.year <- min(input$range)
      max.year <- max(input$range)
      min.df <- subset(emp_df, Year==min.year)
      max.df <- subset(emp_df, Year==max.year)
      
      ## merge data and take difference between the data of the min year and the max year
      diff.df <- within(merge(min.df, max.df, by="Municipal"),{
        Average_Monthly_Employment <- round(Average_Monthly_Employment.y - Average_Monthly_Employment.x, 3)
      })[,c("Municipal", "Average_Monthly_Employment")]
      
      diff.df$Municipal <- paste(as.character(diff.df$Municipal), "Municipal")
      
      ## assign colors to each entry in the data frame
      color <- as.integer(cut2(map_dat[,"Average_Monthly_Employment"],cuts=cuts))
      map_dat <- cbind.data.frame(map_dat, color)
      map_dat$color <- ifelse(is.na(map_dat$color), length(map_colors), 
                              map_dat$color)
      map_dat$opacity <- 0.7
      
      ## find missing counties in data subset and assign NAs to all values
      missing_munis <- setdiff(leftover_munis_map, map_dat$Region)
      missing_df <- data.frame(Municipal = missing_munis, County = NA, State = "MA", 
                               Region = missing_munis, Average_Monthly_Employment = NA,
                               color=length(map_colors), opacity = 0)
      na_munis <- setdiff(MA_municipals_map, map_dat$Region)
      na_df <- data.frame(Municipal = na_munis, County = NA, State = "MA", 
                          Region = na_munis, Average_Monthly_Employment = NA, color=length(map_colors), opacity = 0.7)
      
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
        x$features[[i]]$properties["Average_Monthly_Employment"] <- 
          map_dat[match(x$features[[i]]$properties$NAMELSAD10, map_dat$Region), "Average_Monthly_Employment"]
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
    
    isolate({
      values$selectedFeature <- evt$properties
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
    muni_name <- values$selectedFeature$NAMELSAD10
    muni_value <- prettyNum(values$selectedFeature["Average_Monthly_Employment"], big.mark = ",")
    
    ## If clicked county has no crude rate, display a message
    if(muni_value == "NULL"){
      return(as.character(tags$div(
        tags$h5("Average Monthly Employment for", muni_name, "is not available for this timespan"))))
    }
    ## For a single year when county is clicked, display a message
    as.character(tags$div(
      tags$h4("Average Monthly Employment for", muni_name, " for ", input$year),
      tags$h5(muni_value)
    ))
  })
  
  })