#######################################
## Title: Crime server.R             ##
## Author(s): Heather Weaver,        ##
##            Valerie Evans          ##
## Date Created:  06/27/2019         ##
## Date Modified: 10/30/2019 VE      ##
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
    #browser()
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
      } else {
        munis <- c("United States", munis) ## US only
      }
    } else {
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
                         "Rape Rate", "Robbery Rate", "Aggravated assault Rate", "Property crime Rate", "Burglary Rate", 
                         "Larceny-theft Rate", "Motor vehicle theft Rate", "Arson Rate")
    return(crime_df)
  }, options = list(searching = FALSE, orderClasses = TRUE )) # there are a bunch of options to edit the appearance of datatables, this removes one of the ugly features
  
  ###########################################################
  
  ## create the plot of the data
  ## for the Google charts plot
  
  plot_dat <- reactive({
    crime_df <- crime_df()
    munis <- input$plot_muni
    
    ## if counties are selected and MA or US mean boxes are selected, add those to dataframe
    if(!is.null(input$plot_muni)){
      if(input$plot_var == "Violent_crime_Rate"){
        if(input$MA_violent){
          munis <- c(munis, "MA")
        }
        if(input$US_violent){
          munis <- c(munis, "United States")
        }
      }  
      else if (input$plot_var == "Murder_and_nonnegligent_manslaughter_Rate"){
        if(input$MA_murder){
          munis <- c(munis, "MA")  
        }
        if(input$US_murder){
          munis <- c(munis, "United States")
        }
      }  
      else if (input$plot_var == "Rape_Rate"){
        if(input$MA_rape){
          munis <- c(munis, "MA")  
        }
        if(input$US_rape){
          munis <- c(munis, "United States")
        }
      }  
      else if (input$plot_var == "Robbery_Rate"){
        if(input$MA_robbery){
          munis <- c(munis, "MA")  
        }
        if(input$US_robbery){
          munis <- c(munis, "United States")
        }
      }  
      else if (input$plot_var == "Aggravated_assault_Rate"){
        if(input$MA_assault){
          munis <- c(munis, "MA")  
        }
        if(input$US_assault){
          munis <- c(munis, "United States")
        }
      }  
      else if (input$plot_var == "Property_crime_Rate"){
        if(input$MA_property){
          munis <- c(munis, "MA")  
        }
        if(input$US_property){
          munis <- c(munis, "United States")
        }
      }  
      else if (input$plot_var == "Burglary_Rate"){
        if(input$MA_burglary){
          munis <- c(munis, "MA")  
        }
        if(input$US_burglary){
          munis <- c(munis, "United States")
        }
      }  
      else if (input$plot_var == "Larceny_theft_Rate"){
        if(input$MA_larceny){
          munis <- c(munis, "MA")  
        }
        if(input$US_larceny){
          munis <- c(munis, "United States")
        }
      }  
      else if (input$plot_var == "Motor_vehicle_theft_Rate"){
        if(input$MA_motor){
          munis <- c(munis, "MA")  
        }
        if(input$US_motor){
          munis <- c(munis, "United States")
        }
      } 
      else if (input$plot_var == "Arson_Rate"){
        if(input$MA_arson){
          munis <- c(munis, "MA")
        }
        if(input$US_arson){
          munis <- c(munis, "United States")
        }
      }
    }
    
    ##Choose column according input
    if (input$plot_var == "Violent_crime_Rate") {
      col <- "Violent_crime_Rate"
    }
    else if (input$plot_var == "Murder_and_nonnegligent_manslaughter_Rate") {
      col <- "Murder_and_nonnegligent_manslaughter_Rate"
    }
    else if (input$plot_var == "Rape_Rate") {
      col <- "Rape_Rate"
    }
    else if (input$plot_var == "Robbery_Rate") {
      col <- "Robbery_Rate"
    }
    else if (input$plot_var == "Aggravated_assault_Rate") {
      col <- "Aggravated_assault_Rate"
    }
    else if (input$plot_var == "Property_crime_Rate") {
      col <- "Property_crime_Rate"
    }
    else if (input$plot_var == "Burglary_Rate") {
      col <- "Burglary_Rate"
    }
    else if (input$plot_var == "Larceny_theft_Rate") {
      col <- "Larceny_theft_Rate"
    }
    else if (input$plot_var == "Motor_vehicle_theft_Rate") {
      col <- "Motor_vehicle_theft_Rate"
    }
    else {
      col <- "Arson_Rate"
    }
    #browser()
    ## assign column to each entry in the data frame
    col_sel_num1 <- which(colnames(crime_df) == col)
    plot_dat1 <- crime_df %>%
      filter(Region %in% munis) %>%
      select(2, 4, col_sel_num1)
    plot_dat <- plot_dat1 %>%
      spread_("Region", col)
    return(plot_dat)
    
  })
  
  ## for the Google charts plots 
  output$plot_violent <- reactive({
    ## make reactive dataframe into regular dataframe
    violent_df <- plot_dat()
    list(
      data = googleDataTable(violent_df))
  })
  
  output$plot_murder <- reactive({
    ## make reactive dataframe into regular dataframe
    murder_df <- plot_dat()
    list(
      data = googleDataTable(murder_df))
  })
  
  output$plot_rape <- reactive({
    ## make reactive dataframe into regular dataframe
    rape_df <- plot_dat()
    list(
      data = googleDataTable(rape_df))
  })
  
  output$plot_robbery <- reactive({
    ## make reactive dataframe into regular dataframe
    robbery_df <- plot_dat()
    list(
      data = googleDataTable(robbery_df))
  })
  
  output$plot_assault <- reactive({
    ## make reactive dataframe into regular dataframe
    assault_df <- plot_dat()
    list(
      data = googleDataTable(assault_df))
  })
  
  output$plot_property <- reactive({
    ## make reactive dataframe into regular dataframe
    property_df <- plot_dat()
    list(
      data = googleDataTable(property_df))
  })
  
  output$plot_burglary <- reactive({
    ## make reactive dataframe into regular dataframe
    burglary_df <- plot_dat()
    list(
      data = googleDataTable(burglary_df))
  })
  
  output$plot_larceny <- reactive({
    ## make reactive dataframe into regular dataframe
    larceny_df <- plot_dat()
    list(
      data = googleDataTable(larceny_df))
  })
  
  output$plot_motor <- reactive({
    ## make reactive dataframe into regular dataframe
    motor_df <- plot_dat()
    list(
      data = googleDataTable(motor_df))
  })
  
  output$plot_arson <- reactive({
    ## make reactive dataframe into regular dataframe
    arson_df <- plot_dat()
    list(
      data = googleDataTable(arson_df))
  })
  
  output$text <- renderText({
    input$MA_arson
    paste0('Arson Rates are not available for MA or the US.')
  })
    

  ###########################################################
  
  ## set map colors
  map_dat <- reactive({
    
    #browser()
    ## Browser command - Stops the app right when it's about to break
    ## make reactive dataframe into regular dataframe
    crime_df <- crime_df()
    
    ## take US, MA, and counties out of map_dat
    crime_df <- crime_df() %>%
      filter(Year == input$map_year) %>%
      filter(!is.na(Municipal))
    
    #Select only the crime rates, region, year and population columns
    crime_df <- crime_df %>%
      select(2,4,5,7,9,11,13,15,17,19,21,23,25:length(colnames(crime_df)))
    
    #Have map change color range based on selected variable
    ## get column name and cuts based on input
    if (input$var == "Violent_crime_Rate"){
      col <- "Violent_crime_Rate"
      cuts <- violentcuts
    }
    else if (input$var == "Murder_and_nonnegligent_manslaughter_Rate"){
      col <- "Murder_and_nonnegligent_manslaughter_Rate"
      cuts <- murdercuts
    }
    else if (input$var == "Rape_Rate"){
      col <- "Rape_Rate"
      cuts <- rapecuts
    }
    else if (input$var == "Robbery_Rate"){
      col <- "Robbery_Rate"
      cuts <- robberycuts
    }
    else if (input$var == "Aggravated_assault_Rate"){
      col <- "Aggravated_assault_Rate"
      cuts <- assaultcuts
    }
    else if (input$var == "Property_crime_Rate"){
      col <- "Property_crime_Rate"
      cuts <- propertycuts
    }
    else if (input$var == "Burglary_Rate"){
      col <- "Burglary_Rate"
      cuts <- burglarycuts
    }
    else if (input$var == "Larceny_theft_Rate"){
      col <- "Larceny_theft_Rate"
      cuts <- larcenycuts
    }
    else if (input$var == "Motor_vehicle_theft_Rate"){
      col <- "Motor_vehicle_theft_Rate"
      cuts <- motorcuts
    }
    else {
      col <- "Arson_Rate"
      cuts <- arsoncuts
    }
    
    #browser()
    ## assign colors to each entry in the data frame
    col_sel_num1 <- which(colnames(crime_df) == col)
    crime_df <- select(crime_df, c(1:3, col_sel_num1))
    color <- as.integer(cut2(crime_df[, col], cuts = cuts))
    crime_df <- cbind.data.frame(crime_df, color)
    crime_df$color <- ifelse(is.na(crime_df$color), length(map_colors), crime_df$color)
    crime_df$opacity <- 0.7
    
    ## find missing counties in data subset and assign NAs to all values
    missing_munis <- setdiff(leftover_munis_map, crime_df$Region)
    missing_df <- data.frame(Region = missing_munis, Year = input$map_year, Population = NA, 
                             Map_var = NA, color = length(map_colors), opacity = 0)
    colnames(missing_df)[4] <- col
    
    # combine data subset with missing counties data
    map_dat <- rbind.data.frame(crime_df, missing_df)
    map_dat$color <- map_colors[map_dat$color]
    return(map_dat)
  })
  
  values <- reactiveValues(selectedFeature = NULL, highlight = c())
  
  ############################################################
  
  ## draw leaflet map
  map <- createLeafletMap(session, "map")
  
  ## Does nothing until called (done with action button)
  observe({
    input$action
    
    ## load in relevant map data
    map_dat <- map_dat()
    col_name <- colnames(map_dat)[4]
    
    ## All functions which are isolated, will not run until the above observe function is activated
    isolate({
      ## Duplicate MAmap to x
      x <- MA_map_muni
      
      ## for each county in the map, attach the Crude Rate and colors associated
      for(i in 1:length(x$features)){
        ## Each feature is a Municipal
        x$features[[i]]$properties[col_name] <- map_dat[match(x$features[[i]]$properties$NAMELSAD10, map_dat$Region), col_name]
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
    input$map_year
    map_dat <- map_dat()
    col_name <- colnames(map_dat)[4]
    
    isolate({
      values$selectedFeature <- evt$properties
      region <- evt$properties$NAMELSAD10
      values$selectedFeature[col_name] <- map_dat[match(region, map_dat$Region), col_name]
    })
  })
  ##  This function is what creates info box
  
  output$details <- renderText({
    map_dat <- map_dat()
    col_name <- colnames(map_dat)[4]
    
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
        tags$h5(" ", var_select, " in ", muni_name, "is not available for this year"))))
    }
    ## For a single year when county is clicked, display a message
    as.character(tags$div(
      tags$h4(var_select, " rate estimate in ", muni_name, " for ", input$map_year, " is ", muni_value, "per 100,000 people")
    ))
  })
  
  output$legend1 <- renderPlot({  
    paint.brush = colorRampPalette(colors = c("white", "darkblue"))
    ## Cuts based on entire dataset so that the legends are the same for all years
    cols <- paint.brush(length(map_colors) - 1)
    leg_dat <- data_frame(y = seq(0, 2000, length.out = (length(map_colors)-1)), x = 1, col = cols)
    
    b <- ggplot(data = leg_dat) +
      geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
      scale_y_continuous(limits = c(0, 2000), breaks = round(seq(0, 2000, length.out = 5), 0)) +
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
    paint.brush = colorRampPalette(colors = c("white", "darkblue"))
    cols <- paint.brush(length(map_colors) - 1)
    leg_dat <- data_frame(y = seq(0, 40, length.out = (length(map_colors)-1)), x = 1, col = cols)
    
    b <- ggplot(data = leg_dat) +
      geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
      scale_y_continuous(limits = c(0, 40), breaks = round(seq(0, 40, length.out = 5), 0)) +
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
    cols <- paint.brush(length(map_colors)-1)
    leg_dat<- data_frame(y = seq(0, 350,length.out=(length(map_colors)-1)), x = 1, col = cols)
    
    b <- ggplot(data = leg_dat) +
      geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
      scale_y_continuous(limits = c(0, 350), breaks = round(seq(0, 350, length.out = 5), 0)) +
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
    cols <- paint.brush(length(map_colors)-1)
    leg_dat<- data_frame(y = seq(0, 710,length.out=(length(map_colors)-1)), x = 1, col = cols)
    
    b <- ggplot(data = leg_dat) +
      geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
      scale_y_continuous(limits = c(0, 710), breaks = round(seq(0, 710, length.out = 5), 0)) +
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
    cols <- paint.brush(length(map_colors)-1)
    leg_dat<- data_frame(y = seq(0, 1300,length.out=(length(map_colors)-1)), x = 1, col = cols)
    
    b <- ggplot(data = leg_dat) +
      geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
      scale_y_continuous(limits = c(0, 1300), breaks = round(seq(0, 1300, length.out = 5), 0)) +
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
    cols <- paint.brush(length(map_colors)-1)
    leg_dat<- data_frame(y = seq(0, 7000,length.out=(length(map_colors)-1)), x = 1, col = cols)
    
    b <- ggplot(data = leg_dat) +
      geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
      scale_y_continuous(limits = c(0, 7000), breaks = round(seq(0, 7000, length.out = 5), 0)) +
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
    cols <- paint.brush(length(map_colors)-1)
    leg_dat<- data_frame(y = seq(0, 2800,length.out=(length(map_colors)-1)), x = 1, col = cols)
    
    b <- ggplot(data = leg_dat) +
      geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
      scale_y_continuous(limits = c(0, 2800), breaks = round(seq(0, 2800, length.out = 5), 0)) +
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
    cols <- paint.brush(length(map_colors)-1)
    leg_dat<- data_frame(y = seq(0, 6000,length.out=(length(map_colors)-1)), x = 1, col = cols)
    
    b <- ggplot(data = leg_dat) +
      geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
      scale_y_continuous(limits = c(0, 6000), breaks = round(seq(0, 6000, length.out = 5), 0)) +
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
    cols <- paint.brush(length(map_colors)-1)
    leg_dat<- data_frame(y = seq(0, 1400,length.out=(length(map_colors)-1)), x = 1, col = cols)
    
    b <- ggplot(data = leg_dat) +
      geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
      scale_y_continuous(limits = c(0, 1400), breaks = round(seq(0, 1400, length.out = 5), 0)) +
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
    cols <- paint.brush(length(map_colors)-1)
    leg_dat<- data_frame(y = seq(0, 120,length.out=(length(map_colors)-1)), x = 1, col = cols)
    
    b <- ggplot(data = leg_dat) +
      geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
      scale_y_continuous(limits = c(0, 120), breaks = round(seq(0, 120, length.out = 5), 0)) +
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
