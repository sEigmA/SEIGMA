#######################################
## Title: Marital server.R          ##
## Author(s): Emily Ramos, Arvind    ##
##            Ramakrishnan, Jenna    ##
##            Kiridly, Steve Lauer   ## 
## Date Created:  10/22/2014         ##
## Date Modified: 10/22/2014         ##
#######################################

shinyServer(function(input, output, session) {
  ## edu_df is a reactive dataframe. Necessary for when summary/plot/map have common input (Multiple Variables). Not in this project
  edu_df <- reactive({
    ## Filter the data by the chosen Five Year Range 
    edu_df <- mar_data %>%
      filter(Five_Year_Range == input$year) %>%
      select(1:4, Gender, Five_Year_Range, Population, Inc_HS_Pct, HS_Pct,
             Some_College_Pct, Bachelors_Pct, Masters_Pct, PhD_Pct) %>%
      arrange(Region, Gender)
    ## Output reactive dataframe
    edu_df    
  })
  
  ## Create summary table
  output$summary <- renderDataTable({
    ## Make reactive dataframe into regular dataframe
    edu_df <- edu_df()
    
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
    edu_df <- edu_df %>%
      filter(Region %in% munis) %>%
      select(4:length(colnames(edu_df)))
    
    colnames(edu_df) <- gsub("_", " ", colnames(edu_df))
    colnames(edu_df) <- gsub("Pct", "%", colnames(edu_df))
    
    return(edu_df)
  }, options=list(searching = FALSE, orderClasses = TRUE)) # there are a bunch of options to edit the appearance of datatables, this removes one of the ugly features
  
  ## create the plot of the data
  ## for the Google charts plot
  output$plot <- reactive({
    ## make reactive dataframe into regular dataframe
    edu_df <- edu_df()
    
    ## find the county of the municipal
    county <- edu_df$County[match(input$plot_muni, edu_df$Municipal)]
    
    ## make counties a vector based on input variable
    munis <- c(input$plot_muni, county, "MA", "US")
    
    plot_df <- edu_df %>%
      filter(Region %in% munis)
    
    ## put data into form that googleCharts understands (this unmelts the dataframe)
    melted_plot_df <- melt(plot_df, id.vars = "Region", 
                           measure.vars = c("Inc_HS_Pct", "HS_Pct", "Some_College_Pct", 
                                            "Bachelors_Pct", "Masters_Pct", "PhD_Pct"),
                           variable.name = "Education_Attainment",
                           value.name = "Population_Pct")
    
    g <- dcast(melted_plot_df, Education_Attainment ~ Region, 
               value.var = "Population_Pct")
    
    g$Education_Attainment <- gsub("_", " ", g$Education_Attainment)
    g$Education_Attainment <- gsub("Pct", "", g$Education_Attainment)
    
    ## this outputs the google data to be used in the UI to create the dataframe
    list(
      data = googleDataTable(g), options = list(
        title = paste0("Education Attainment Statistics for ", munis[1], ", ", munis[2], ", Massachusetts, and the United States")))
  })
  
  ## set map colors
  map_dat <- reactive({
    
    #browser()
    ## Browser command - Stops the app right when it's about to break
    ## make reactive dataframe into regular dataframe
    edu_df <- edu_df()
    
    ## take US, MA, and counties out of map_dat
    map_dat <- edu_df %>%
      filter(!is.na(Municipal))
    
    ## assign colors to each entry in the data frame
    color <- as.integer(cut2(map_dat[,input$var],cuts=cuts))
    map_dat <- cbind.data.frame(map_dat, color)
    map_dat$color <- ifelse(is.na(map_dat$color), length(map_colors), 
                            map_dat$color)
    map_dat$opacity <- 0.7
    
    ## find missing counties in data subset and assign NAs to all values
    missing_munis <- setdiff(leftover_munis_map, map_dat$Region)
    missing_df <- data.frame(Municipal = missing_munis, County = NA, State = "MA", 
                             Region = missing_munis,
                             Five_Year_Range = input$year, Population = NA, 
                             Inc_HS_Pct = NA,
                             HS_Pct = NA, Some_College_Pct = NA, Bachelor_Pct = NA, 
                             Masters_Pct = NA, PhD_Pct = NA, color=length(map_colors), 
                             opacity = 0)
    
    # combine data subset with missing counties data
    map_dat <- rbind.data.frame(map_dat, missing_df)
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
      
      ## for each county in the map, attach the Crude Rate and colors associated
      for(i in 1:length(x$features)){
        ## Each feature is a county
        x$features[[i]]$properties[input$var] <- 
          map_dat[match(x$features[[i]]$properties$NAMELSAD10, map_dat$Region), input$var]
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
    muni_value <- values$selectedFeature[input$var]
    var_select <- gsub("_", " ", input$var)
    var_select <- gsub("Pct", "", var_select)
    
    ## If clicked county has no crude rate, display a message
    if(is.null(values$selectedFeature[input$var])){
      return(as.character(tags$div(
        tags$h5(var_select, "% in ", muni_name, "is not available for this timespan"))))
    }
    ## For a single year when county is clicked, display a message
    as.character(tags$div(
      tags$h4(var_select, "% in ", muni_name, " for ", input$year),
      tags$h5(muni_value, "%")
    ))
  })
  
})