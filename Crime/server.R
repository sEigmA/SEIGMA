#######################################
## Title: Crime server.R             ##
## Author(s): Heather Weaver,        ##
##            Valerie Evans          ##
## Date Created:  6/27/2019          ##
## Date Modified:                    ##
#######################################

shinyServer(function(input, output, session) {
  ## crime_df is a reactive dataframe. Necessary for when summary/plot/map have common input (Multiple Variables). Not in this project
  crime_df <- reactive({
    ## Filter the data by the chosen Five Year Range 
    crime_df <- crime_data %>%
     arrange(Region)
    ## Output reactive dataframe
    crime_df    
  })
  
  
  ## Create summary table
  output$summary <- renderDataTable({
    #     browser()
    ## Make reactive dataframe into regular dataframe
    crime_df <- crime_df()
    
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
    crime_df <- crime_df %>%
      filter(Year == input$sum_year) %>%
      filter(Region %in% munis) %>%
      select(2,4,5,7,9,11,13,15,17,19,21,23,25) 
    
    names(crime_df) <- c("Region", "Year", "Population", "Violent crime Rate", "Murder and Nonnegligent Manslaughter Rate",
                           "Rape Rate", "Robbery Rate", "Aggravated assault Rate",
                           "Property crime Rate", "Burglary Rate",
                           "Larceny-theft Rate", "Motor vehicle theft Rate", "Arson Rate")
    

    return(crime_df)
  }, options=list(searching = FALSE, orderClasses = TRUE )) # there are a bunch of options to edit the appearance of datatables, this removes one of the ugly features
  
  ## create the plot of the data
  ## for the Google charts plot
  output$plot <- reactive({
    #browser()
    ## make reactive dataframe into regular dataframe
    crime_df <- crime_df()%>%
    filter(Year == input$plot_year) 
    
    ## find the county of the municipal
    #county <- as.character(crime_df$County[match(input$plot_muni, crime_df$Municipal)])
    
    ## make counties a vector based on input variable
    munis <- c(input$plot_muni, "MA", "United States")
    
    muni_index <- c()
        for(i in 1:length(munis)){
      muni_index[i] <- match(munis[i], crime_df$Region)
    }
    
    crime_df$Region <- factor(crime_df$Region, levels = c(munis, as.character(crime_df$Region)[-muni_index]))
    
    munis_df <- crime_df[muni_index,]
    
    ## put data into form that googleCharts understands (this unmelts the dataframe)
    melted_munis_df <- melt(munis_df, id.vars = "Region", 
                            measure.vars = c("Violent_crime_Rate","Murder_and_nonnegligent_manslaughter_Rate", "Rape_Rate", "Robbery_Rate", 
                                             "Aggravated_assault_Rate", "Property_crime_Rate", "Burglary_Rate",
                                             "Larceny_theft_Rate", "Motor_vehicle_theft_Rate", "Arson_Rate"),
                            variable.name = "Crime_Rate",
                            value.name = "Rate")
  
    
    levels(melted_munis_df$Region)[1:3] <- munis
    
    plot_df <- melted_munis_df %>%
      arrange(Region)
    
    g <- dcast(plot_df, Crime_Rate ~ Region, 
               value.var = "Rate")
    
    g$Crime_Rate <- c("Violent crime Rate", "Murder and Nonnegligent Manslaughter Rate",
                      "Rape Rate", "Robbery Rate", "Aggravated assault Rate",
                      "Property crime Rate", "Burglary Rate",
                      "Larceny-theft Rate", "Motor vehicle theft Rate", "Arson Rate")
    
    ## this outputs the google data to be used in the UI to create the dataframe
    list(
      data = googleDataTable(g))
  })
 
##########################################################
  
  ## set map colors
  map_dat <- reactive({
    
        #browser()
    ## Browser command - Stops the app right when it's about to break
    ## make reactive dataframe into regular dataframe
    crime_df <- crime_df()
    
    ## take US, MA, and counties out of map_dat
    map_dat <- crime_df %>%
      filter(Year == input$map_year)%>%
      filter(!is.na(Municipal))
    
    #Select only the crime rates, region, year and population columns
    
    map_dat <- map_dat %>%
      select(2,4,5,7,9,11,13,15,17,19,21,23,25:length(colnames(map_dat)))
    
    #Have map change color range based on selected variable
    ## get column name and cuts based on input
    if (input$var == "Violent Crime"){
        col<-"Violent_crime_Rate"  
        cuts<-violentcuts
      }
      else if (input$var == "Murder and Nonnegligent Manslaughter"){
        col<-"Murder_and_nonnegligent_manslaughter_Rate"
        cuts<-murdercuts
      }
    else if (input$var == "Rape"){
      col<-"Rape_Rate"
      cuts<-rapecuts
    }
    else if (input$var == "Robbery"){
      col<-"Robbery_Rate"
      cuts<-robberycuts
    }
    else if (input$var == "Aggravated Assault"){
      col<-"Aggravated_assault_Rate"
      cuts<-assaultcuts
    }
    else if (input$var == "Property Crime"){
      col<-"Property_crime_Rate"
      cuts<-propertycuts
    }
    else if (input$var == "Burglary"){
      col<-"Burglary_Rate"
      cuts<-burglarycuts
    }
    else if (input$var == "Larceny-theft"){
      col<-"Larceny_theft_Rate"
      cuts<-larcenycuts
    }
    else if (input$var == "Motor vehicle Theft"){
      col<-"Motor_vehicle_theft_Rate"
      cuts<-motorcuts
    }
      else {
        col<-"Arson_Rate"
        cuts<-arsoncuts
      }
      
    
    col_sel_num1<-which(colnames(map_dat)==col)
    map_dat <- select(map_dat,c(1:3,col_sel_num1))
    color <- as.integer(cut2(map_dat[,col],cuts=cuts))
    map_dat <- cbind.data.frame(map_dat, color)
    map_dat$color <- ifelse(is.na(map_dat$color), length(map_colors),
                            map_dat$color)
    map_dat$opacity <- 0.7
    
    ## assign colors to each entry in the data frame
    #color <- as.integer(cut2(map_dat[,col],cuts=cuts))
    #map_dat <- cbind.data.frame(map_dat, color)
    #map_dat$color <- ifelse(is.na(map_dat$color), length(map_colors), 
                            #map_dat$color)
    #map_dat$opacity <- 0.7
    
    ## find missing counties in data subset and assign NAs to all values
    missing_munis <- setdiff(leftover_munis_map, map_dat$Region)
    missing_df <-data.frame(Region=missing_munis, Year=input$map_year, Population=NA, Map_var=NA,
                            color=length(map_colors), opacity = 0)
    colnames(missing_df)[4]<-col
    
    
    # combine data subset with missing counties data
    map_dat <- rbind.data.frame(map_dat, missing_df)
    map_dat$color <- map_colors[map_dat$color]
    return(map_dat)
  })
  
  values <- reactiveValues(selectedFeature=NULL, highlight=c())
  
############################################################
  
  ## draw leaflet map
  map <- createLeafletMap(session, "map")
  
  
  
  ## Does nothing until called (done with action button)
  observe({
    input$action
    
    ## load in relevant map data
    map_dat <- map_dat()
    col_name<-colnames(map_dat)[4]
    
    ## All functions which are isolated, will not run until the above observe function is activated
    isolate({
      ## Duplicate MAmap to x
      x <- MA_map_muni
      #     browser()
      ## for each county in the map, attach the Crude Rate and colors associated
      for(i in 1:length(x$features)){
        ## Each feature is a Municipal
        x$features[[i]]$properties[col_name] <- map_dat[match(x$features[[i]]$properties$NAMELSAD10, map_dat$Municipal), col_name]
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
    col_name<-colnames(map_dat)[4]
    
    isolate({
      values$selectedFeature <- evt$properties
      region <- evt$properties$NAMELSAD10
      values$selectedFeature[col_name] <- map_dat[match(region, map_dat$Region), col_name]
    })
  })
  ##  This function is what creates info box
  
  output$details <- renderText({
    map_dat <- map_dat()
    col_name<-colnames(map_dat)[4]
    
    ## Before a county is clicked, display a message
    if(is.null(values$selectedFeature)){
      return(as.character(tags$div(
        tags$div(
          h4("Click on a town or city"))
      )))
    }
    
    muni_name <- values$selectedFeature$NAMELSAD10
    muni_value <- values$selectedFeature[col_name]
    var_select <- gsub("_", " ", input$var)
    var_select <- gsub("Rate", "", var_select)
    
    ## If clicked county has no crude rate, display a message
    if(is.null(values$selectedFeature[input$var])){
      return(as.character(tags$div(
        tags$h5(" ",var_select, " in ", muni_name, "is not available for this year"))))
    }
    ## For a single year when county is clicked, display a message
    as.character(tags$div(
      tags$h4(var_select, " rate estimate in ", muni_name, " for ", input$map_year, " is ", muni_value, "per 100,000 people")
    ))
  })
  
  output$legend1 <- renderPlot({  
    paint.brush = colorRampPalette(colors=c("white", "darkblue"))
    ## Cuts based on entire dataset so that the legends are the same for all years
    pctmax.val<- max(crime_data$Violent_crime_Rate, na.rm=TRUE)
    pctmin.val<- 0
    pctcuts <- seq(pctmin.val, pctmax.val, length.out = length(map_colors))
    cols <- paint.brush(length(map_colors)-1)
    leg_dat<- data_frame(y = seq(pctmin.val, pctmax.val,length.out=(length(map_colors)-1)), x = 1, col = cols)
    
    b <- ggplot(data = leg_dat) +
      geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
      scale_fill_manual(values = leg_dat$col) + theme_bw() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.x = element_blank(),
            panel.border = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank())
    return(b)
    
  })
  
  
  output$legend2 <- renderPlot({  
      paint.brush = colorRampPalette(colors=c("white", "darkblue"))
      pctmax.val<- max(crime_data$Murder_and_nonnegligent_manslaughter_Rate, na.rm=TRUE)
      pctmin.val<- 0
      pctcuts <- seq(pctmin.val, pctmax.val, length.out = length(map_colors))
      cols <- paint.brush(length(map_colors)-1)
      leg_dat<- data_frame(y = seq(pctmin.val, pctmax.val,length.out=(length(map_colors)-1)), x = 1, col = cols)
    
      b <- ggplot(data = leg_dat) +
        geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
        scale_fill_manual(values = leg_dat$col) + theme_bw() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_text(size = 12),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks.x = element_blank(),
              panel.border = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank())
    
    return(b)
    
  })
  
  output$legend3 <- renderPlot({  
    paint.brush = colorRampPalette(colors=c("white", "darkblue"))
    pctmax.val<- max(crime_data$Rape_Rate, na.rm=TRUE)
    pctmin.val<- 0
    pctcuts <- seq(pctmin.val, pctmax.val, length.out = length(map_colors))
    cols <- paint.brush(length(map_colors)-1)
    leg_dat<- data_frame(y = seq(pctmin.val, pctmax.val,length.out=(length(map_colors)-1)), x = 1, col = cols)
    
    
    b <- ggplot(data = leg_dat) +
      geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
      scale_fill_manual(values = leg_dat$col) + theme_bw() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.x = element_blank(),
            panel.border = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank())
    return(b)
    
  })
  
  output$legend4 <- renderPlot({  
    paint.brush = colorRampPalette(colors=c("white", "darkblue"))
    pctmax.val<- max(crime_data$Robbery_Rate, na.rm=TRUE)
    pctmin.val<- 0
    pctcuts <- seq(pctmin.val, pctmax.val, length.out = length(map_colors))
    cols <- paint.brush(length(map_colors)-1)
    leg_dat<- data_frame(y = seq(pctmin.val, pctmax.val,length.out=(length(map_colors)-1)), x = 1, col = cols)
    
    
    b <- ggplot(data = leg_dat) +
      geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
      scale_fill_manual(values = leg_dat$col) + theme_bw() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.x = element_blank(),
            panel.border = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank())
    return(b)
    
  })
  
  output$legend5 <- renderPlot({  
    paint.brush = colorRampPalette(colors=c("white", "darkblue"))
    pctmax.val<- max(crime_data$Aggravated_assault_Rate, na.rm=TRUE)
    pctmin.val<- 0
    pctcuts <- seq(pctmin.val, pctmax.val, length.out = length(map_colors))
    cols <- paint.brush(length(map_colors)-1)
    leg_dat<- data_frame(y = seq(pctmin.val, pctmax.val,length.out=(length(map_colors)-1)), x = 1, col = cols)
    
    
    b <- ggplot(data = leg_dat) +
      geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
      scale_fill_manual(values = leg_dat$col) + theme_bw() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.x = element_blank(),
            panel.border = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank())
    return(b)
    
  })
  
  output$legend6 <- renderPlot({  
    paint.brush = colorRampPalette(colors=c("white", "darkblue"))
    pctmax.val<- max(crime_data$Property_crime_Rate, na.rm=TRUE)
    pctmin.val<- 0
    pctcuts <- seq(pctmin.val, pctmax.val, length.out = length(map_colors))
    cols <- paint.brush(length(map_colors)-1)
    leg_dat<- data_frame(y = seq(pctmin.val, pctmax.val,length.out=(length(map_colors)-1)), x = 1, col = cols)
    
    
    b <- ggplot(data = leg_dat) +
      geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
      scale_fill_manual(values = leg_dat$col) + theme_bw() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.x = element_blank(),
            panel.border = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank())
    return(b)
    
  })
  
  output$legend7 <- renderPlot({  
    paint.brush = colorRampPalette(colors=c("white", "darkblue"))
    pctmax.val<- max(crime_data$Burglary_Rate, na.rm=TRUE)
    pctmin.val<- 0
    pctcuts <- seq(pctmin.val, pctmax.val, length.out = length(map_colors))
    cols <- paint.brush(length(map_colors)-1)
    leg_dat<- data_frame(y = seq(pctmin.val, pctmax.val,length.out=(length(map_colors)-1)), x = 1, col = cols)
    
    
    b <- ggplot(data = leg_dat) +
      geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
      scale_fill_manual(values = leg_dat$col) + theme_bw() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.x = element_blank(),
            panel.border = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank())
    return(b)
    
  })
  
  output$legend8 <- renderPlot({  
    paint.brush = colorRampPalette(colors=c("white", "darkblue"))
    pctmax.val<- max(crime_data$Larceny_theft_Rate, na.rm=TRUE)
    pctmin.val<- 0
    pctcuts <- seq(pctmin.val, pctmax.val, length.out = length(map_colors))
    cols <- paint.brush(length(map_colors)-1)
    leg_dat<- data_frame(y = seq(pctmin.val, pctmax.val,length.out=(length(map_colors)-1)), x = 1, col = cols)
    
    
    b <- ggplot(data = leg_dat) +
      geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
      scale_fill_manual(values = leg_dat$col) + theme_bw() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.x = element_blank(),
            panel.border = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank())
    return(b)
    
  })
  
  output$legend9 <- renderPlot({  
    paint.brush = colorRampPalette(colors=c("white", "darkblue"))
    pctmax.val<- max(crime_data$Motor_vehicle_theft_Rate, na.rm=TRUE)
    pctmin.val<- 0
    pctcuts <- seq(pctmin.val, pctmax.val, length.out = length(map_colors))
    cols <- paint.brush(length(map_colors)-1)
    leg_dat<- data_frame(y = seq(pctmin.val, pctmax.val,length.out=(length(map_colors)-1)), x = 1, col = cols)
    
    b <- ggplot(data = leg_dat) +
      geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
      scale_fill_manual(values = leg_dat$col) + theme_bw() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.x = element_blank(),
            panel.border = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank())
    return(b)
    
  })
  
  output$legend10 <- renderPlot({  
    paint.brush = colorRampPalette(colors=c("white", "darkblue"))
    pctmax.val<- max(crime_data$Arson_Rate, na.rm=TRUE)
    pctmin.val<- 0
    pctcuts <- seq(pctmin.val, pctmax.val, length.out = length(map_colors))
    cols <- paint.brush(length(map_colors)-1)
    leg_dat<- data_frame(y = seq(pctmin.val, pctmax.val,length.out=(length(map_colors)-1)), x = 1, col = cols)
    
    b <- ggplot(data = leg_dat) +
      geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
      scale_fill_manual(values = leg_dat$col) + theme_bw() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.x = element_blank(),
            panel.border = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank())
    return(b)
    
  })
  
})