#######################################
## Title: Bankruptices server.R      ##
## Author(s): Xuelian Li, Jenna      ##
##            Kiridly                ## 
## Date Created:  08/15/15           ##
## Date Modified: 08/16/15 XL        ##
#######################################


shinyServer(function(input, output, session) {
  ## suidf is a reactive dataframe. Necessary for when summary/plot/map have common input (Multiple Variables). Not in this project
  bank_df <- reactive({
    bank_df <- bankdata
    bank_df    
  })
  
  ## Create summary table
  output$summary <- renderDataTable({
    ## Make reactive dataframe into regular dataframe
    bank_df <- bank_df()
    
    ## make municipals a vector based on input variable
    if(!is.null(input$sum_county)){
      munis <- input$sum_county
    }
    ## if none selected, put all municipals in vector
    else {
      munis <- MAcounties
    }
    ## if the user checks the meanUS box or the meanMA box, add those to counties vector
    if(input$sum_US){
      if(input$sum_MA){
        munis <- c("United States", "MA", munis) ## US and MA
      } else{
        munis <- c("United States", munis) ## US only
      }
    } else{
      if(input$sum_MA){
        munis <- c("MA", munis) ## US only ## MA only
      }
    }
    ##select columns according input$radio
    sel_col_num<-c()
    if (input$sum_radio=="Total Filings") {
      sel_col_num<-c(2:6)
    } else if (input$sum_radio=="Business Filings") {
      sel_col_num<-c(7:11)
    } else {sel_col_num<-c(12:15)}
    
    ## create a dataframe consisting only of counties in vector
    bank_df <- bank_df %>%
      filter(Region %in% munis,Year == input$sum_year) %>%
      select(1, 16, sel_col_num)
    
    colnames(bank_df) <- gsub("_", " ", colnames(Dem_df))
        
    return(bank_df)
  }, options=list(searching = FALSE, orderClasses = TRUE)) # there are a bunch of options to edit the appearance of datatables, this removes one of the ugly features
  ## create the plot of the data
  plot_dat<- reactive({
    plot_df<-bank_df()
    counties <- input$plot_county
    if(input$plot_US){
      if(input$plot_MA){
        counties <- c("United States", "MA", counties) ## US and MA
      } else{
        counties <- c("United States", counties) ## US only
      }
    } else{
      if(input$plot_MA){
        counties <- c("MA", counties)  ## MA only
      }
    }
    ##Choose column according input
    if (input$plot_radio == "Business Filings") {
      col<-input$plot_bus_display
    }
    else {
      col<-input$plot_nonbus_display
    }
    
    ## assign colors to each entry in the data frame
    col_sel_num1<-which( colnames(bank_df)==col )
    plot_dat <- plot_df %>%
      filter(Region %in% munis,Year == input$sum_year) %>%
      select(1, 16, col_sel_num1)%>%
      spread(Region, col)
    return(plot_dat)
  })
  
  ## for the Google charts plot (Business Filings plot)
  output$Bus_plot <- reactive({
    ## make reactive dataframe into regular dataframe
    bus_df <- plot_dat()
    list(
      data=googleDataTable(bus_df))
  })
  
  ##NonBusiness Filings Plot
  output$NonBus_plot <- reactive({
    ## make reactive dataframe into regular dataframe
    nonbus_df <- plot_dat()
    list(
      data=googleDataTable(nonbus_df))
  })
  
  ##proportion by chapter in Business Filings
  output$Pro_Bus_plot <- reactive({
    ## make reactive dataframe into regular dataframe
    pro_bus_df <- plot_dat()
    list(
      data=googleDataTable(pro_bus_df))
  })
  ##proportion by chapter in NonBusiness Filings
  output$Pro_NonBus_plot <- reactive({
    ## make reactive dataframe into regular dataframe
    pro_nonbus_df <- plot_dat()
    list(
      data=googleDataTable(pro_nonbus_df))
  })
   
  ##map
  map_dat <- reactive({
    ## make reactive dataframe into regular dataframe
    bank_dat <- bank_df()%>%
      filter(Year == input$map_year)%>%
      ## take US, MA, and counties out of map_dat
      filter(!is.na(Municipal) )
    
    ## get column name and cuts based on input
    if (input$map_radio == "Age") {
      col<-input$var_age
      cuts<-agecuts
    }
    else if (input$map_radio == "Gender"){
      col<-input$var_gen
      cuts<-gencuts
    }
    else if (input$map_radio == "Race"){
      col<-input$var_rac
      cuts<-racecuts
    }
    else {
      col<-input$var_eth
      cuts<-racecuts 
    }
    
    ## assign colors to each entry in the data frame
    col_sel_num1<-which( colnames(Dem_dat)==col )
    map_dat <- select(Dem_dat,c(1:6,col_sel_num1))
    color <- as.integer(cut2(map_dat[,col],cuts=cuts))
    map_dat <- cbind.data.frame(map_dat, color)
    map_dat$color <- ifelse(is.na(map_dat$color), length(map_colors),
                            map_dat$color)
    map_dat$opacity <- 0.7
    
    ## find missing counties in data subset and assign NAs to all values
    missing_munis <- setdiff(leftover_munis_map, map_dat$Region)
    missing_df <- data.frame(Municipal = NA, County = NA, State = "MA",
                             Region = missing_munis, Five_Year_Range = input$map_year,
                             Total_Population = NA,
                             Map_var = NA, color=length(map_colors), opacity = 0)
    colnames(missing_df)[7]<-col
    # combine data subset with missing counties data
    map_dat <- rbind.data.frame(map_dat, missing_df)
    map_dat$color <- map_colors[map_dat$color]
    return(map_dat)
  })
  
  values <- reactiveValues(selectedFeature=NULL, highlight=c())
  ## draw leaflet map
  map <- createLeafletMap(session, "map")
  # browser()
  ## the functions within observe are called when any of the inputs are called
  
  ## Does nothing until called (done with action button)
  observe({
    input$action
    
    ## load in relevant map data
    map_dat <- map_dat()
    col_name<-colnames(map_dat)[7]
    ## All functions which are isolated, will not run until the above observe function is activated
    isolate({
      ## Duplicate MAmap to x
      x <- MA_map_muni
      
      ## for each county in the map, attach the Crude Rate and colors associated
      for(i in 1:length(x$features)){
        ## Each feature is a county
        x$features[[i]]$properties[col_name] <-
          map_dat[match(x$features[[i]]$properties$NAMELSAD10, map_dat$Region), col_name]
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
    col_name<-colnames(map_dat)[7]
    isolate({
      values$selectedFeature <- evt$properties
      region <- evt$properties$NAMELSAD10
      values$selectedFeature[col_name] <- map_dat[match(region, map_dat$Region), col_name]
    })
  })
  ##  This function is what creates info box
  output$details <- renderText({
    if (input$map_radio == "Age") {
      col<-input$var_age
    }
    else if (input$map_radio == "Gender"){
      col<-input$var_gen
    }
    else if (input$map_radio == "Race"){
      col<-input$var_rac
    }
    else{
      col<-input$var_eth 
    }
    ## Before a county is clicked, display a message
    if(is.null(values$selectedFeature)){
      return(as.character(tags$div(
        tags$div(
          h4("Click on a town or city"))
      )))
    }
    
    muni_name <- values$selectedFeature$NAMELSAD10
    muni_value <- values$selectedFeature[col]
    var_select <- gsub("Pct", "", col)
    var_select <- gsub("_", " ", var_select)
    var_select <- gsub("plot", "", var_select)
    
    ## If clicked county has no crude rate, display a message
    if(is.null(values$selectedFeature[col])){
      return(as.character(tags$div(
        tags$h5("Percentage of", var_select, "in ", muni_name, "is not available for this timespan"))))
    }
    ## For a single year when county is clicked, display a message
    as.character(tags$div(
      tags$h4("Percentage of", var_select, "in ", muni_name, " for ", input$map_year),
      tags$h5(muni_value, "%")
    ))
  })
  ##########################################################
})

  ## Gets crazy  
  map_dat <- reactive({
    
    # browser()
    ## Browser command - Stops the app right when it's about to break
    ## make reactive dataframe into regular dataframe
    suidf <- suidf()
    
    ## subset US data into own dataframe
    US <- suidf[which(suidf$County=="US"),]
    
    ## subset MA data into own dataframe
    MA <- suidf[which(suidf$County=="MA"),]
    
    ## take US and MA out of suidf
    suidf <- subset(suidf, County!="MA")
    suidf <- subset(suidf, County!="US")
    
    ## for single year maps...
    if(input$map_timespan == "sing.yr"){
      
      ## subset the data by the year selected
      suidf <- filter(suidf, Year==input$map_year)
      
      ## assign colors to each entry in the data frame
      color <- as.integer(cut2(suidf$Age.Adjusted.Rate,cuts=scuts))
      suidf <- cbind.data.frame(suidf,color)
      suidf$color <- ifelse(is.na(suidf$color), length(smap.colors), 
                            suidf$color)
      ## This line is important. Formats county name (ie Franklin County)
      suidf$County <- paste(as.character(suidf$County), "County")
      
      ## find missing counties in data subset and assign NAs to all values
      missing.counties <- setdiff(MAcounties, suidf$County)
      df <- data.frame(County=missing.counties, State="MA", Country="US", 
                       Year=input$map_year, Suicides=NA, Population=NA, 
                       Age.Adjusted.Rate=NA, Age.Adjusted.Rate.Lower.Bound=NA,
                       Age.Adjusted.Rate.Upper.Bound=NA, 
                       Age.Adjusted.Rate.Standard.Error=NA,Crude.Rate=NA,
                       color=length(smap.colors))
      
      ## combine data subset with missing counties data
      suidf <- rbind.data.frame(suidf, df)
      suidf$color <- smap.colors[suidf$color]
      return(suidf)
    }
    
    if(input$map_timespan=="mult.yrs"){
      
      ## create dataframes for the max and min year of selected data
      min.year <- min(input$map_range)
      max.year <- max(input$map_range)
      min.df <- subset(suidf, Year==min.year)
      max.df <- subset(suidf, Year==max.year)
      
      ## merge data and take difference between the data of the min year and the max year
      diff.df <- within(merge(min.df, max.df, by="County"),{
        Age.Adjusted.Rate <- round(Age.Adjusted.Rate.y - Age.Adjusted.Rate.x, 3)
      })[,c("County", "Age.Adjusted.Rate")]
      
      diff.df$County <- paste(as.character(diff.df$County), "County")
      
      ## assign colors to each entry in the data frame
      color <- as.integer(cut2(diff.df[,2],cuts=mcuts))
      diff.df <- cbind.data.frame(diff.df,color)
      diff.df$color <- ifelse(is.na(diff.df$color), length(mmap.colors), diff.df$color)
      
      ## find missing counties in data subset and assign NAs to all values
      missing.counties <- setdiff(MAcounties, diff.df$County)
      df <- data.frame(County=missing.counties, Age.Adjusted.Rate=NA,
                       color=length(mmap.colors))
      
      ## combine data subset with missing counties data
      diff.df <- rbind.data.frame(diff.df, df)
      diff.df$color <- mmap.colors[diff.df$color]
      return(diff.df)
    }
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
    x <- MAmap
    
    ## for each county in the map, attach the Crude Rate and colors associated
    for(i in 1:length(x$features)){
      ## Each feature is a county
      x$features[[i]]$properties$Age.Adjusted.Rate <- map_dat$Age.Adjusted.Rate[match(x$features[[i]]$properties$County, map_dat$County)]
      ## Style properties
      x$features[[i]]$properties$style <- list(
        fill=TRUE, 
        ## Fill color has to be equal to the map_dat color and is matched by county
        fillColor = map_dat$color[match(x$features[[i]]$properties$County, map_dat$County)], 
        ## "#000000" = Black, "#999999"=Grey, 
        weight=1, stroke=TRUE, opacity=1, color="#000000", fillOpacity=0.7)
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
    input$map_year
    if(is.null(evt))
      return()
    
    ## load in relevant map data
    map_dat <- map_dat()
    
    isolate({
      values$selectedFeature <- evt$properties
      county <- evt$properties$County
      values$selectedFeature$Age.Adjusted.Rate <- map_dat$Age.Adjusted.Rate[match(county, map_dat$County)]
    })
  })
  ##  This function is what creates info box
  output$details <- renderText({
  ## Before a county is clicked, display a message
    if(is.null(values$selectedFeature)){
      return(as.character(tags$div(
        tags$div(
          h4("Click on a County"))
      )))
    }
  
    ## If clicked county has no crude rate, display a message
    if(is.null(values$selectedFeature$Age.Adjusted.Rate)){
      return(as.character(tags$div(
        tags$h5("Age-adjusted suicide rate in ", values$selectedFeature$County, "is not available for this timespan"))))
    }
    else{
    ## For a single year when county is clicked, display a message
    if(input$map_timespan=="sing.yr"){
    return(as.character(tags$div(
      tags$h4(input$map_year, "Age-adjusted suicide rate in ", values$selectedFeature$County),
      tags$h5(values$selectedFeature$Age.Adjusted.Rate, "per 100,000 in population")
    )))}
   ## For multiple years when county is clicked, display a message
    if(input$map_timespan=="mult.yrs"){
      return(as.character(tags$div(
        tags$h4("Change in Age-adjusted suicide rate from ", min(input$map_range), " to ", max(input$map_range), " in ", values$selectedFeature$County),
        tags$h5("Increased by "[values$selectedFeature$Age.Adjusted.Rate>=0],
                "Decreased by"[values$selectedFeature$Age.Adjusted.Rate<0],
                abs(values$selectedFeature$Age.Adjusted.Rate), "per 100,000 in population")
      )))}
    }
  })
  
})