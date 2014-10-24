#######################################
## Title: Marriage server.R          ##
## Author(s): Emily Ramos, Arvind    ##
##            Ramakrishnan, Jenna    ##
##            Kiridly, Steve Lauer   ## 
## Date Created:  10/22/2014         ##
## Date Modified: 10/22/2014         ##
#######################################

shinyServer(function(input, output, session) {
  ## mar_df is a reactive dataframe. Necessary for when summary/plot/map have common input (Multiple Variables). Not in this project
  mar_df <- reactive({
   ## Filter the data by the chosen Five Year Range 
   mar_df <- mar_data %>%
    filter(Five_Year_Range == input$year) %>%
    select(1:4, Gender, Five_Year_Range, Population, Never_Married_Pct, Now_Married_Pct,
           Separated_Pct, Widowed_Pct, Divorced_Pct) %>%
     arrange(Region, Gender)
   ## Output reactive dataframe
   mar_df    
  })
  
  ## Create summary table
  output$summary <- renderDataTable({
    ## Make reactive dataframe into regular dataframe
    mar_df <- mar_df()
        
    ## make gender a vector based on input variable
    if(!is.null(input$sum_gender))
     genders <- input$sum_gender
    ## if none selected, put all genders in vector
    if(is.null(input$sum_gender))
     genders <- c("Female", "Male")
    
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
    mar_df <- mar_df %>%
     filter(Gender %in% genders, Region %in% munis) %>%
      select(4:length(colnames(mar_df)))
    
    colnames(mar_df) <- gsub("_", " ", colnames(mar_df))
    colnames(mar_df) <- gsub("Pct", "%", colnames(mar_df))
    
    return(mar_df)
  }, options=list(bFilter=FALSE)) # there are a bunch of options to edit the appearance of datatables, this removes one of the ugly features
  
  ## create the plot of the data
  ## for the Google charts plot
  output$plot_US <- reactive({
   ## make reactive dataframe into regular dataframe
   mar_df <- mar_df()
   
   ## make counties a vector based on input variable
   munis <- "United States"
   
   plot_df <- mar_df %>%
    filter(Region %in% munis)
   
   ## put data into form that googleCharts understands (this unmelts the dataframe)
   melted_plot_df <- melt(plot_df, id.vars = "Gender", 
                          measure.vars = c("Never_Married_Pct", "Now_Married_Pct", "Separated_Pct", 
                                           "Widowed_Pct", "Divorced_Pct"),
                          variable.name = "Marital_Status", value.name = "Population_Pct")
   
   g <- dcast(melted_plot_df, Marital_Status ~ Gender, 
              value.var = "Population_Pct")
   
   g$Marital_Status <- gsub("_", " ", g$Marital_Status)
   g$Marital_Status <- gsub("Pct", "", g$Marital_Status)
   
   ## this outputs the google data to be used in the UI to create the dataframe
   list(
    data=googleDataTable(g))
  })
  
  ## create the plot of the MA data
  output$plot_MA <- reactive({
    ## make reactive dataframe into regular dataframe
    mar_df <- mar_df()
    
    ## make counties a vector based on input variable
    munis <- "MA"
    
    plot_df <- mar_df %>%
      filter(Region %in% munis)
    
    ## put data into form that googleCharts understands (this unmelts the dataframe)
    melted_plot_df <- melt(plot_df, id.vars = "Gender", 
                           measure.vars = c("Never_Married_Pct", "Now_Married_Pct", "Separated_Pct", 
                                            "Widowed_Pct", "Divorced_Pct"),
                           variable.name = "Marital_Status", value.name = "Population_Pct")
    
    g <- dcast(melted_plot_df, Marital_Status ~ Gender, 
               value.var = "Population_Pct")
    
    g$Marital_Status <- gsub("_", " ", g$Marital_Status)
    g$Marital_Status <- gsub("Pct", "", g$Marital_Status)
    
    ## this outputs the google data to be used in the UI to create the dataframe
    list(
      data=googleDataTable(g))
  })
  
  ## create the plot of the MA data
  output$plot_county <- reactive({
    ## make reactive dataframe into regular dataframe
    mar_df <- mar_df()
    
    ## find the county of the municipal
    county <- mar_df$County[which(mar_df$Municipal==input$plot_muni)]
    ## make counties a vector based on input variable
    munis <- mar_df$Region[match(county, mar_df$Region)]
    
    plot_df <- mar_df %>%
      filter(Region %in% munis)
    
    ## put data into form that googleCharts understands (this unmelts the dataframe)
    melted_plot_df <- melt(plot_df, id.vars = "Gender", 
                           measure.vars = c("Never_Married_Pct", "Now_Married_Pct", "Separated_Pct", 
                                            "Widowed_Pct", "Divorced_Pct"),
                           variable.name = "Marital_Status", value.name = "Population_Pct")
    
    g <- dcast(melted_plot_df, Marital_Status ~ Gender, 
               value.var = "Population_Pct")
    
    g$Marital_Status <- gsub("_", " ", g$Marital_Status)
    g$Marital_Status <- gsub("Pct", "", g$Marital_Status)
    
    ## this outputs the google data to be used in the UI to create the dataframe
    list(
      data=googleDataTable(g))
  })
  
  ## create the plot of the MA data
  output$plot_muni <- reactive({
    ## make reactive dataframe into regular dataframe
    mar_df <- mar_df()
    
    ## make counties a vector based on input variable
    munis <- input$plot_muni
    
    plot_df <- mar_df %>%
      filter(Region %in% munis)
    
    ## put data into form that googleCharts understands (this unmelts the dataframe)
    melted_plot_df <- melt(plot_df, id.vars = "Gender", 
                           measure.vars = c("Never_Married_Pct", "Now_Married_Pct", "Separated_Pct", 
                                            "Widowed_Pct", "Divorced_Pct"),
                           variable.name = "Marital_Status", value.name = "Population_Pct")
    
    g <- dcast(melted_plot_df, Marital_Status ~ Gender, 
               value.var = "Population_Pct")
    
    g$Marital_Status <- gsub("_", " ", g$Marital_Status)
    g$Marital_Status <- gsub("Pct", "", g$Marital_Status)
    
    ## this outputs the google data to be used in the UI to create the dataframe
    list(
      data=googleDataTable(g))
  })
  
#   ## Gets crazy  
#   map_dat <- reactive({
#     
#     # browser()
#     ## Browser command - Stops the app right when it's about to break
#     ## make reactive dataframe into regular dataframe
#     suidf <- suidf()
#     
#     ## subset US data into own dataframe
#     US <- suidf[which(suidf$County=="US"),]
#     
#     ## subset MA data into own dataframe
#     MA <- suidf[which(suidf$County=="MA"),]
#     
#     ## take US and MA out of suidf
#     suidf <- subset(suidf, County!="MA")
#     suidf <- subset(suidf, County!="US")
#     
#     ## for single year maps...
#     if(input$timespan == "sing.yr"){
#       
#       ## subset the data by the year selected
#       suidf <- filter(suidf, Year==input$year)
#       
#       ## assign colors to each entry in the data frame
#       color <- as.integer(cut2(suidf$Crude.Rate,cuts=scuts))
#       suidf <- cbind.data.frame(suidf,color)
#       suidf$color <- ifelse(is.na(suidf$color), length(smap.colors), 
#                             suidf$color)
#       ## This line is important. Formats county name (ie Franklin County)
#       suidf$County <- paste(as.character(suidf$County), "County")
#       
#       ## find missing counties in data subset and assign NAs to all values
#       missing.counties <- setdiff(MAcounties, suidf$County)
#       df <- data.frame(County=missing.counties, State="MA", Country="US", 
#                        Year=input$year, Suicides=NA, Population=NA, 
#                        Crude.Rate=NA, Crude.Rate.Lower.Bound=NA,
#                        Crude.Rate.Upper.Bound=NA, 
#                        Crude.Rate.Standard.Error=NA,
#                        color=length(smap.colors))
#       
#       ## combine data subset with missing counties data
#       suidf <- rbind.data.frame(suidf, df)
#       suidf$color <- smap.colors[suidf$color]
#       return(suidf)
#     }
#     
#     if(input$timespan=="mult.yrs"){
#       
#       ## create dataframes for the max and min year of selected data
#       min.year <- min(input$range)
#       max.year <- max(input$range)
#       min.df <- subset(suidf, Year==min.year)
#       max.df <- subset(suidf, Year==max.year)
#       
#       ## merge data and take difference between the data of the min year and the max year
#       diff.df <- within(merge(min.df, max.df, by="County"),{
#         Crude.Rate <- round(Crude.Rate.y - Crude.Rate.x, 3)
#       })[,c("County", "Crude.Rate")]
#       
#       diff.df$County <- paste(as.character(diff.df$County), "County")
#       
#       ## assign colors to each entry in the data frame
#       color <- as.integer(cut2(diff.df[,2],cuts=mcuts))
#       diff.df <- cbind.data.frame(diff.df,color)
#       diff.df$color <- ifelse(is.na(diff.df$color), length(mmap.colors), diff.df$color)
#       
#       ## find missing counties in data subset and assign NAs to all values
#       missing.counties <- setdiff(MAcounties, diff.df$County)
#       df <- data.frame(County=missing.counties, Crude.Rate=NA,
#                        color=length(mmap.colors))
#       
#       ## combine data subset with missing counties data
#       diff.df <- rbind.data.frame(diff.df, df)
#       diff.df$color <- mmap.colors[diff.df$color]
#       return(diff.df)
#     }
#   })
#   
# values <- reactiveValues(selectedFeature=NULL, highlight=c())
# 
# #############################################
# # observe({
# #   values$highlight <- input$map_shape_mouseover$id
# #   #browser()
# # })
# 
# # # Dynamically render the box in the upper-right
# # output$countyInfo <- renderUI({
# #   
# #   if (is.null(values$highlight)) {
# #     return(tags$div("Hover over a county"))
# #   } else {
# #     # Get a properly formatted state name
# #     countyName <- names(MAcounties)[values$highlight]
# #     return(tags$div(
# #       tags$strong(countyName),
# #       tags$div(density[countyName], HTML("people/m<sup>2</sup>"))
# #     ))
# #   }
# # })
# 
# # lastHighlighted <- c()
# # # When values$highlight changes, unhighlight the old state (if any) and
# # # highlight the new state
# # observe({
# # #   if (length(lastHighlighted) > 0)
# # #     drawStates(getStateName(lastHighlighted), FALSE)
# #   lastHighlighted <<- values$highlight
# #   
# #   if (is.null(values$highlight))
# #     return()
# #   
# #   isolate({
# #     drawStates(getStateName(values$highlight), TRUE)
# #   })
# # })
# 
# ###########################################
# 
#   ## draw leaflet map
#   map <- createLeafletMap(session, "map")
#   
#   ## the functions within observe are called when any of the inputs are called
# 
#   ## Does nothing until called (done with action button)
#   observe({
#     input$action
#     
#     ## load in relevant map data
#     map_dat <- map_dat()
#     
#     ## All functions which are isolated, will not run until the above observe function is activated
#     isolate({
#     ## Duplicate MAmap to x
#     x <- MAmap
#     
#     ## for each county in the map, attach the Crude Rate and colors associated
#     for(i in 1:length(x$features)){
#       ## Each feature is a county
#       x$features[[i]]$properties$Crude.Rate <- map_dat$Crude.Rate[match(x$features[[i]]$properties$County, map_dat$County)]
#       ## Style properties
#       x$features[[i]]$properties$style <- list(
#         fill=TRUE, 
#         ## Fill color has to be equal to the map_dat color and is matched by county
#         fillColor = map_dat$color[match(x$features[[i]]$properties$County, map_dat$County)], 
#         ## "#000000" = Black, "#999999"=Grey, 
#         weight=1, stroke=TRUE, opacity=1, color="#000000", fillOpacity=0.7)
#     }
#     
#     map$addGeoJSON(x) # draw map
#      })
#   })
#  
#   observe({
#     ## EVT = Mouse Click
#     evt <- input$map_click
#     if(is.null(evt))
#       return()
#     
#     isolate({
#       values$selectedFeature <- NULL
#     })
#   })
#   
#   observe({
#     evt <- input$map_geojson_click
#     if(is.null(evt))
#       return()
#     
#     isolate({
#       values$selectedFeature <- evt$properties
#     })
#   })
#   ##  This function is what creates info box
#   output$details <- renderText({
#   ## Before a county is clicked, display a message
#     if(is.null(values$selectedFeature)){
#       return(as.character(tags$div(
#         tags$div(
#           h4("Click on a County"))
#       )))
#     }
#   
#     ## If clicked county has no crude rate, display a message
#     if(is.null(values$selectedFeature$Crude.Rate)){
#       return(as.character(tags$div(
#         tags$h5("Crude suicide rate in ", values$selectedFeature$County, "is not available for this timespan"))))
#     }
#     ## For a single year when county is clicked, display a message
#     if(input$timespan=="sing.yr"){
#     return(as.character(tags$div(
#       tags$h4(input$year, "crude suicide rate in ", values$selectedFeature$County),
#       tags$h5(values$selectedFeature$Crude.Rate, "per 100,000 in population")
#     )))}
#    ## For multiple years when county is clicked, display a message
#     if(input$timespan=="mult.yrs"){
#       return(as.character(tags$div(
#         tags$h4("Change in crude suicide rate from ", min(input$range), " to ", max(input$range), " in ", values$selectedFeature$County),
#         tags$h5("Increased by "[values$selectedFeature$Crude.Rate>=0],
#                 "Decreased by"[values$selectedFeature$Crude.Rate<0],
#                 abs(values$selectedFeature$Crude.Rate), "per 100,000 in population")
#       )))}
#   })
#   
})