#######################################
## Title: Employment server.R        ##
## Author(s): Emily Ramos, Arvind    ##
##            Ramakrishnan, Jenna    ##
##            Kiridly, Steve Lauer   ## 
## Date Created:  01/07/2015         ##
## Date Modified: 02/05/2015         ##
#######################################

shinyServer(function(input, output, session) {
  # emp_df is a reactive dataframe. Necessary for when summary/plot/map have common input (Multiple Variables). Not in this project
  
  
  #   sum_df <- emp_df %>%
  #     filter(Region %in% munis) %>%
  #     select(4:length(colnames(df)))
  
  emp_df <- reactive({
    ## Filter the data by the chosen Year
    emp_df <- emp_data ## %>%
    
    ##  filter(Year == input$year) %>%
    ##  select(1:4, 6:9)
    
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
      
      for(i in 1:length(range)){
        bbb <- subset(emp_df, Year==range[i])
        df <- rbind.data.frame(df, bbb)
      }
    }
    
    ## make municipals a vector based on input variable
    ## For when something is clicked
    
    if(!is.null(input$sum_muni))
      munis <- input$sum_muni
    ## if none selected, put all municipals in vector
    ## For nothing clicked
    if(is.null(input$sum_muni))
      munis <- MA_municipals

    ## create a dataframe consisting only of counties in vector
    sum_df <- df %>%
      filter(Municipal %in% munis) %>%
      select(Municipal, Year, Average_Monthly_Employment)
    
    colnames(sum_df) <- c("Municipal","Year","Average Monthly Employment")

    return(sum_df)
  }, options = list(searching = FALSE, orderClasses = TRUE)) 
  
  # there are a bunch of options to edit the appearance of datatables, this removes one of the ugly features
  
  
  ##############################################  
  
  
  ## create the plot of the data
  ## for the Google charts plot
  output$plot <- reactive({
  
    ## make reactive dataframe into regular dataframe
    emp_df <- emp_df()
    
    ## make region a vector based on input variable
    munis <- input$plot_muni
    
    ## if counties are selected and MA or US mean boxes are selected, add those to dataframe
#     if(!is.null(input$plot_muni)){
#       if(input$MA_mean)
#         munis <- c(plot_muni, "MA")
#       
#       if(input$US_mean)
#         munis <- c(munis, "United States")
#     }
    
    ## if no counties have been selected, just show the US average
#     if(is.null(input$plot_muni)){
#       ## make region a vector based on input variable
#       munis <- "MA"
#     }
    
    ## put data into form that googleCharts understands (this unmelts the dataframe)
    # g <- dcast(emp_df, Year ~ munis, value.var="Average_Monthly_Employment")
    
    g <- emp_df %>%
      filter(Municipal %in% munis) %>%
      select( Municipal, Year, Average_Monthly_Employment) %>%
      spread(Municipal, Average_Monthly_Employment)
    
    
    ## this outputs the google data to be used in the UI to create the dataframe
    list(
      data=googleDataTable(g))
  })
 
  
  
  ###################MAP CREATION##############
  
  ## set map colors
  map_dat <- reactive({
    ## Browser command - Stops the app right when it's about to break
    ## make reactive dataframe into regular dataframe
    emp_df <- emp_df()
    
    ## take US, MA, and counties out of map_dat
#     map_dat <- emp_df %>%
#       filter(!is.na(Municipal))
#     
    
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
      emp_df$opacity = 0.7
      
      #       ## This line is important. Formats county name (ie Franklin County)
      #       suidf$County <- paste(as.character(suidf$County), "County")
      
      ## find missing counties in data subset and assign NAs to all values
      missing.munis <- setdiff(leftover_munis_map,emp_df$Municipal)
      
      
      if(length(missing.munis) > 0){
        df <- data.frame(Municipal=missing.munis, Year=input$year, Average_Monthly_Employment=NA,
                         color=length(smap.colors), opacity = 0)
        
        ## combine data subset with missing counties data
        emp_df <- rbind.data.frame(emp_df, df)
      }
      emp_df$color <- smap.colors[emp_df$color]
      return(emp_df)
    } 
    
    ######################################MULTIPLE YEARS   
    
    
    if(input$timespan=="mult.yrs"){
      
      ## create dataframes for the max and min year of selected data
      min.year <- min(input$range)
      max.year <- max(input$range)
      min.df <- subset(emp_df, Year==min.year)
      max.df <- subset(emp_df, Year==max.year)
      
      ## merge data and take difference between the data of the min year and the max year
      diff.df <- within(merge(min.df, max.df, by="Municipal"),{
        Average_Monthly_Employment <- (Average_Monthly_Employment.y - Average_Monthly_Employment.x)
      })[,c("Municipal", "Average_Monthly_Employment")]
      
      #diff.df$Municipal <- paste(as.character(diff.df$Municipal), "Municipal")
      
      ## assign colors to each entry in the data frame
      color <- as.integer(cut2(diff.df[,2],cuts=mcuts))
      diff.df <- cbind.data.frame(diff.df,color)
      diff.df$color <- ifelse(is.na(diff.df$color), length(mmap.colors), diff.df$color)
      diff.df$opacity <- 0.7
   
      
      ## find missing munis in data subset and assign NAs to all values
      missing.munis <- setdiff(leftover_munis_map, diff.df$Municipal)
      df <- data.frame(Municipal=missing.munis, Average_Monthly_Employment=NA,
                       color=length(mmap.colors))
      df$opacity <- 0
      
      ## combine data subset with missing counties data
      diff.df <- rbind.data.frame(diff.df, df)
      diff.df$color <- mmap.colors[diff.df$color]
      return(diff.df)
    }
    
  })
  
  values <- reactiveValues(selectedFeature=NULL, highlight=c())
  
  

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
        ## Each feature is a Municipal
        x$features[[i]]$properties["Average_Monthly_Employment"] <- map_dat[match(x$features[[i]]$properties$NAMELSAD10, map_dat$Municipal), "Average_Monthly_Employment"]
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
    
    if(input$timespan=="sing.yr"){
    
    as.character(tags$div(
      tags$h4("Average Monthly Employment for", muni_name, " for ", input$year),
      tags$h5(muni_value)
    ))
    }
    if(input$timespan=="mult.yrs"){
      
      as.character(tags$div(
        tags$h4("Average Monthly Employment for", muni_name, " for ", input$range),
        tags$h5(muni_value)
      ))
    }
    
  })
  
})
