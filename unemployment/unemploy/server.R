#######################################
## Title: Unemploy server.R          ##
## Author(s): Emily Ramos, Arvind    ##
##            Ramakrishnan, Jenna    ##
##            Kiridly, Steve Lauer   ##
## Date Created:  01/07/2015         ##
## Date Modified: 01/08/2015         ##
#######################################

shinyServer(function(input, output, session){
# unemp_df is a reactive dataframe. Necessary for when summary/plot/map have common input (Multiple Variables). Not in this project
 unemp_df <- reactive({
   unemp_df <- unemp_data
    ## Output reactive dataframe
    unemp_df
  })

  ## Create summary table
  output$summary <- renderDataTable({
    ## Make reactive dataframe into regular dataframe
    unemp_df <- unemp_df()

    ## if a user chooses Single Year, display only data from that year (dpylr)
    if(input$timespan == "sing.yr"){
      df <- filter(unemp_df, Year==input$year)
    }

    ## if a user chooses Multiple Years, display data from all years in range
    if(input$timespan == "mult.yrs"){
      range <- seq(min(input$range), max(input$range), 1)
      df <- c()

      ####**********RBIND.Data.frame -DO Not Match
      for(i in 1:length(range)){
        bbb <- subset(unemp_df, Year==range[i])
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
    sum_df <- unemp_df %>%
      filter(Region %in% munis) %>%
      select(4:length(colnames(unemp_df)))

    colnames(sum_df) <- c("Region", "Unemployment Rate", "Number Unemployed", "Number Employed", "Number in Labor Force")

    return(sum_df)
  }, options = list(searching = FALSE, orderClasses = TRUE)) # there are a bunch of options to edit the appearance of datatables, this removes one of the ugly features

##############################################

  ## create the plot of the data
    ## for the Google charts plot
  output$plot <- reactive({
#     browser()
    ## make reactive dataframe into regular dataframe
    unemp_df <- unemp_df()

    ## make region a vector based on input variable
    munis <- input$plot_muni

    ## if counties are selected and MA or US mean boxes are selected, add those to dataframe
    if(!is.null(input$plot_muni)){
      if(input$MA_mean)
        munis <- c(munis, "MA")
      if(input$US_mean)
        munis <- c(munis, "United States")
    }

    ## if no counties have been selected, just show the US average
    if(is.null(input$plot_muni)){
      ## make region a vector based on input variable
      munis <- "MA"
    }

    ## put data into form that googleCharts understands (this unmelts the dataframe)
    g <- unemp_df %>%
        filter(Region %in% munis) %>%
        select(Year, Region, Unemployment.Rate.Avg) %>%
        spread(Region, Unemployment.Rate.Avg)

    ## this outputs the google data to be used in the UI to create the dataframe
    list(
      data=googleDataTable(g))
  })

###################MAP CREATION##############

  ## set map colors
  map_dat <- reactive({
    ## Browser command - Stops the app right when it's about to break
    ## make reactive dataframe into regular dataframe
    unemp_df <- unemp_df()

    ## take US, MA, and counties out of map_dat
    map_dat <- unemp_df %>%
      filter(!is.na(Municipal))



######################################################
    ## for single year maps...
    if(input$timespan == "sing.yr"){
     browser()
      ## subset the data by the year selected
      unemp_df <- filter(unemp_df, Year==input$year)

      ## assign colors to each entry in the data frame
      color <- as.integer(cut2(unemp_df$Unemployment.Rate.Avg, cuts=scuts))
      unemp_df <- cbind.data.frame(unemp_df, color)
      unemp_df$color <- ifelse(is.na(unemp_df$color), length(smap.colors),
                               unemp_df$color)
#       ## This line is important. Formats county name (ie Franklin County)
#       suidf$County <- paste(as.character(suidf$County), "County")

      ## find missing counties in data subset and assign NAs to all values
      missing.munis <- setdiff(MA_municipals, unemp_df$Municipal)
      df <- data.frame(Municipal=missing.munis, County=County, State="MA", Country="US",
                       Year=input$year, Unemployment.Rate.Avg=NA, No.Unemployed.Avg=NA,
                       No.Employed.Avg=NA, No.Labor.Avg=NA,
                       color=length(smap.colors))

      ## combine data subset with missing counties data
      unemp_df <- rbind.data.frame(unemp_df, df)
      unemp_df$color <- smap.colors[unemp_df$color]
      return(unemp_df)
    }

if(input$timespan=="mult.yrs"){

  ## create dataframes for the max and min year of selected data
  min.year <- min(input$range)
  max.year <- max(input$range)
  min.df <- subset(unemp_df, Year==min.year)
  max.df <- subset(unemp_df, Year==max.year)

  ## merge data and take difference between the data of the min year and the max year
  diff.df <- within(merge(min.df, max.df, by="Municipal"),{
    Unemployment.Rate.Avg <- round(Unemployment.Rate.Avg.y - Unemployment.Rate.Avg.x, 3)
  })[,c("Municipal", "Unemployment.Rate.Avg")]

  diff.df$Municipal <- paste(as.character(diff.df$Municipal), "Municipal")

    ## assign colors to each entry in the data frame
    color <- as.integer(cut2(map_dat[,"Unemployment.Rate.Avg"], cuts=cuts))
    map_dat <- cbind.data.frame(map_dat, color)
    map_dat$color <- ifelse(is.na(map_dat$color), length(map_colors),
                            map_dat$color)
    map_dat$opacity <- 0.7

    ## find missing counties in data subset and assign NAs to all values
    missing_munis <- setdiff(leftover_munis_map, map_dat$Region)
    missing_df <- data.frame(Municipal = missing_munis, County = NA, State = "MA",
                             Region = missing_munis, Unemployment.Rate.Avg = NA,
                             No.Unemployed.Avg = NA, No.Employed.Avg = NA,
                             No.Labor.Avg = NA,
                             color=length(map_colors), opacity = 0)
    na_munis <- setdiff(MA_municipals_map, map_dat$Region)
    na_df <- data.frame(Municipal = na_munis, County = NA, State = "MA",
                             Region = na_munis, Unemployment.Rate.Avg = NA,
                             No.Unemployed.Avg = NA, No.Employed.Avg = NA,
                             No.Labor.Avg = NA, color=length(map_colors), opacity = 0.7)

    # combine data subset with missing counties data
    map_dat <- rbind.data.frame(map_dat, missing_df, na_df)
    map_dat$color <- map_colors[map_dat$color]
    return(map_dat)
  }

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
        x$features[[i]]$properties["Unemployment.Rate.Avg"] <-
          map_dat[match(x$features[[i]]$properties$NAMELSAD10, map_dat$Region), "Unemployment.Rate.Avg"]
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
    muni_value <- prettyNum(values$selectedFeature["Unemployment.Rate.Avg"], big.mark = ",")

    ## If clicked county has no crude rate, display a message
    if(muni_value == "NULL"){
      return(as.character(tags$div(
        tags$h5("Average Rate of Unemployment for", muni_name, "is not available for this timespan"))))
    }
    ## For a single year when county is clicked, display a message
    as.character(tags$div(
      tags$h4("Average Rate of Unemployment for", muni_name, " for ", input$year),
      tags$h5(muni_value)
    ))
   })
  })
})