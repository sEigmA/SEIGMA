#######################################
## Title: Suicide server.R           ##
## Author(s): Emily Ramos, Arvind    ##
##            Ramakrishnan, Jenna    ##
##            Kiridly, Xuelian Li    ##
##            Steve Lauer            ## 
## Date Created:                     ##
## Date Modified: 04/13/15 XL        ##
#######################################

## load necessary libraries
require(dplyr)
require(sp)
require(maptools)
require(rgeos)
require(Hmisc)
require(reshape2)

shinyServer(function(input, output, session) {
  ## suidf is a reactive dataframe. Necessary for when summary/plot/map have common input (Multiple Variables). Not in this project
  suidf <- reactive({
    suidf <- suidata
    suidf    
  })
  
  ## Create summary table
  output$summary <- renderDataTable({
    
    ## Make reactive dataframe into regular dataframe
    suidf <- suidf()
    
    ## if a user chooses Single Year, display only data from that year (dpylr)
    if(input$sum_timespan == "sing.yr"){
      df <- filter(suidf, Year==input$sum_year)
    }
    
    ## if a user chooses Multiple Years, display data from all years in range
    if(input$sum_timespan == "mult.yrs"){
      range <- seq(min(input$sum_range), max(input$sum_range), 1)
      df <- c()
      
####**********RBIND.Data.frame -DO Not Match
      for(i in 1:length(range)){
        bbb <- subset(suidf, Year==range[i])
        df <- rbind.data.frame(df, bbb)
      }
    }
    
    ## make counties a vector based on input variable
    if(!is.null(input$sum_county))
      counties <- input$sum_county
    ## if none selected, put all counties in vector
    if(is.null(input$sum_county))
      counties <- names(table(suidata[,1]))[c(1:7, 9:12,14)]
    
    ## if the user checks the meanUS box or the meanMA box, add those to counties vector
    if(input$sum_meanUS){
      if(input$sum_meanMA){
        counties <- c("US", "MA", counties) ## US and MA  
      } else{
        counties <- c("US", counties) ## US only
      }
    } else{
      if(input$sum_meanMA){
        counties <- c("MA", counties) ## US only ## MA only
      }
    }
    
    ## create a dataframe consisting only of counties in vector
    df2 <- c()
      for(i in 1:length(counties)){
        bbb <- subset(df, County==counties[i])
        df2 <- rbind.data.frame(df2, bbb)
      }
    
    ## make column names more pretty (i.e. no periods)
    colnames(df2)[7:11] <- c("Age-adjusted suicide Rate (per 100,000)", 
                            "Age-adjusted suicide Rate Lower Bound", 
                            "Age-adjusted suicide Rate Upper Bound", 
                            "Age-adjusted suicide Rate Standard Error", "Crude Rate (per 100,000  )")
    
    return(df2)
  }, options=list(searching = FALSE, orderClasses = TRUE)) # there are a bunch of options to edit the appearance of datatables, these make them pretty
  
  

  ## create the plot of the data
  ## for the Google charts plot
  output$plot <- reactive({
    # browser()
  
    ## make reactive dataframe into regular dataframe
    suidf <- suidf()
    
    ## make counties a vector based on input variable
    counties <- input$plot_county
    
    ## put data into form that googleCharts understands (this unmelts the dataframe)
    df <- dcast(suidf, Year ~ County, value.var="Age.Adjusted.Rate")
    
    ## if no counties have been selected, just show the US average
    if(is.null(input$plot_county)){
      ## %>% = then
      g <- df %>%
        select(Year, US)
    }
    
    ## if counties are selected and MA or US mean boxes are selected, add those to dataframe
    if(!is.null(input$plot_county)){
      if(input$plot_meanMA)
        counties <- c(counties, "MA")
      if(input$plot_meanUS)
        counties <- c(counties, "US")
      
      g <- df[,c("Year", counties)]
    }
    
    ## this outputs the google data to be used in the UI to create the dataframe
    list(
      data=googleDataTable(g))
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
  
  output$legend1 <- renderPlot({
    if(input$map_timespan == 'sing.yr') {
    paint.brush = colorRampPalette(colors=c("white", "red3"))
    cols <- paint.brush(length(smap.colors)-1)
    leg_dat<- data_frame(y = seq(smin.val, smax.val, length.out = (length(smap.colors)-1)), x = 1, col = cols)
    
    q<- ggplot(data = leg_dat) +
      geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
      scale_y_continuous(limits = c(smin.val, smax.val), breaks = seq(smin.val, smax.val, length.out = 5)) +
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
    } 
  })
  
  output$legend2<- renderPlot({
    if(input$map_timespan == "mult.yrs"){
      
      paint.brush = colorRampPalette(colors=c(cbbPalette[6], "white",cbbPalette[7]))
      cols <- paint.brush(25)
      leg_dat <- data_frame(y = seq(mmin.val, mmax.val,length.out=25), x = 1, col = cols)
      
      b <- ggplot(data = leg_dat) +
        geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
        scale_y_continuous(limits = c(mmin.val, mmax.val), breaks = seq(mmin.val, mmax.val, length.out = 5)) +
        scale_fill_manual(values = leg_dat$col) + theme_bw() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_text(size = 12),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks.x = element_blank(),
              panel.border = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank())
      
    }
    return(b)
  })
})