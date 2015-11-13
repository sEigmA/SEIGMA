#######################################
## Title: Unemploy server.R          ##
## Author(s): Emily Ramos, Arvind    ##
##            Ramakrishnan, Jenna    ##
##            Kiridly, Xuelian Li,   ##
##            Steve Lauer            ##
## Date Created:  01/07/2015         ##
## Date Modified: 04/05/2015 XL      ##
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
    if(input$sum_timespan == "sing.yr"){
      df <- filter(unemp_df, Year==input$sum_year)
    }
    
    ## if a user chooses Multiple Years, display data from all years in range
    if(input$sum_timespan == "mult.yrs"){
      range <- seq(min(input$sum_range), max(input$sum_range), 1)
      df <- c()
      
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
    
    ## if the user checks the the meanMA box, add those to counties vector
    if(input$sum_MA_mean){
      munis <- c("MA", munis) ## MA only
      } 
    
    ## create a dataframe consisting only of counties in vector
    sum_df <- df %>%
      filter(Region %in% munis) %>%
      select(Region, Year, Unemployment_Rate_Avg, No_Unemployed_Avg, No_Employed_Avg, No_Labor_Avg)
    
    colnames(sum_df) <- c("Region","Year", "Unemployment Rate", "Average Number Unemployed", "Average Number Employed", "Average Number in Labor Force")
    
    return(sum_df)
  }, options = list(searching = FALSE, orderClasses = TRUE)) # there are a bunch of options to edit the appearance of datatables, this removes one of the ugly features
  
  ##############################################
  
  ## create the plot of the data
  ## for the Google charts plot
  plot_dat<- reactive({
    ## make reactive dataframe into regular dataframe
    unemp_df <- unemp_df()
    
    ## make region a vector based on input variable
    munis <- input$plot_muni
    
    ## if counties are selected and MA or US mean boxes are selected, add those to dataframe
    if(!is.null(input$plot_muni)){
      if(input$plot_MA ||input$plot2_MA)
        munis <- c(munis, "MA")
      }
    
    ##Choose column according input
    if (input$plot_radio == "Unemployment_Rate_Avg") {
      col<-input$plot_radio
    }
    else {
      col<-input$plot_display_radio
    }
    
    ## assign column to each entry in the data frame
    col_sel_num1<-which( colnames(unemp_df)==col )
    plot_dat1 <- unemp_df %>%
      filter(Region %in% munis) %>%
      select(4, 5, col_sel_num1)
    plot_dat<-plot_dat1%>%
      spread_("Region", col)
    return(plot_dat)

  })
  
  ## for the Google charts plot (Business Filings plot)
  output$une_plot1 <- reactive({
    ## make reactive dataframe into regular dataframe
    une_df <- plot_dat()
    list(
      data=googleDataTable(une_df))
  })
  
  ##Labor Force Plot
  output$lab_plot1 <- reactive({
    ## make reactive dataframe into regular dataframe
    lab_df <- plot_dat()
    list(
      data=googleDataTable(lab_df))
  })
  
  output$lab_pct_plot <- reactive({
    ## make reactive dataframe into regular dataframe
    lab_pct_df <- plot_dat()
    list(
      data=googleDataTable(lab_pct_df))
  })
  ###################MAP CREATION##############
  map_dat <- reactive({
    ## make reactive dataframe into regular dataframe
    unemp_dat <- unemp_df()%>%
      filter(Year == input$map_year)%>%
      ## take US, MA, and counties out of map_dat
      filter(Region!="MA")
    
    
    ## get column name and cuts based on input
    ##Choose column according input
    if (input$map_radio == "Unemployment_Rate_Avg") {
      col<-input$map_radio
      cuts<-unecuts}
    else{
      col<-input$map_display_radio
      if (input$map_display_radio=="No_Labor_Avg"){
        cuts<- labcuts
      }
      else {
        cuts<-pctcuts
      }
   }
    
    ## assign colors to each entry in the data frame
    col_sel_num1<-which( colnames(unemp_dat)==col )
    map_dat <- select(unemp_dat,c(4,5,col_sel_num1))
    color <- as.integer(cut2(map_dat[,col],cuts=cuts))
    map_dat <- cbind.data.frame(map_dat, color)
    map_dat$color <- ifelse(is.na(map_dat$color), length(map_colors),
                            map_dat$color)
    map_dat$opacity <- 0.7
   ## find missing counties in data subset and assign NAs to all values
   missing.munis <- setdiff(leftover_munis_map,unemp_dat$Region)
   missing_df <-data.frame(Region=missing.munis, Year=input$map_year, Map_var=NA,
                           color=length(map_colors), opacity = 0)
   colnames(missing_df)[3]<-col
   # combine data subset with missing counties data
   map_dat <- rbind.data.frame(map_dat, missing_df)
    map_dat$color <- map_colors[map_dat$color]
    return(map_dat)
  })
 
  
  values <- reactiveValues(selectedFeature=NULL, highlight=c())
  
  ## draw leaflet map
  map <- createLeafletMap(session, "map")
  
  ## the functions within observe are called when any of the inputs are called
  
  ## Does nothing until called (done with action button)
  observe({
    input$action
    
    ## load in relevant map data
    map_dat <- map_dat()
    col_name<-colnames(map_dat)[3]
    
    ## All functions which are isolated, will not run until the above observe function is activated
    isolate({
      ## Duplicate MAmap to x
      x <- MA_map_muni
      ## for each county in the map, attach the Crude Rate and colors associated
      for(i in 1:length(x$features)){
        ## Each feature is a Muni
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
    col_name<-colnames(map_dat)[3]
    isolate({
      values$selectedFeature <- evt$properties
      region <- evt$properties$NAMELSAD10
      values$selectedFeature[col_name] <- map_dat[match(region, map_dat$Region), col_name]
    })
  })
  ##  This function is what creates info box
  output$details <- renderText({
    map_dat <- map_dat()
    col_name<-colnames(map_dat)[3]
    ## Before a county is clicked, display a message
    if(is.null(values$selectedFeature)){
      return(as.character(tags$div(
        tags$div(
          h4("Click on a town or city"))
      )))
    }
    muni_name <- values$selectedFeature$NAMELSAD10
    if(input$map_display_radio == "No_Labor_Avg"){
      muni_value <- prettyNum(values$selectedFeature[col_name], big.mark = ",")
    }
    if(input$map_display_radio == "Labor_Pct_Change"|| input$map_radio == "Unemployment_Rate_Avg"){
      muni_value <- prettyNum(values$selectedFeature[col_name],digits=2)
    }
        
    ## If clicked county has no crude rate, display a message
    if(muni_value == "NULL"){
      return(as.character(tags$div(
        tags$h5("Average Rate of Unemployment for", muni_name, "is not available for this timespan"))))
    }
    ## For a single year when county is clicked, display a message
    if (input$map_radio == "Unemployment_Rate_Avg") {
      return(as.character(tags$div(
        tags$h4("Average Rate of Unemployment in", muni_name, " for ", input$map_year),
        tags$h5(muni_value,"%")
      )))
    }
    else{
    if(input$map_display_radio == "No_Labor_Avg"){
      return(as.character(tags$div(
        tags$h4("Average Labor Force in", muni_name, " for ", input$map_year),
        tags$h5(muni_value)
      )))
    }
    else{
      return(as.character(tags$div(
        tags$h4("Change in Labor Force in", muni_name, " for ", input$map_year,"compared to year 2003"),
        tags$h5(muni_value,"%")
      )))
    }}
  })
  
  ## map legend
  output$legend1 <- renderPlot({
    if(input$map_radio == "Unemployment_Rate_Avg"){
      
      paint.brush = colorRampPalette(colors=c("darkgreen", "white", "maroon"))
      cols <- paint.brush(26)
      leg_dat <- data_frame(y = seq(0, 25), x = 1, col = cols)
      
      p <- ggplot(data = leg_dat) +
        geom_tile(aes(y = y, fill = reorder(col,y), x = x), show_guide = FALSE) +
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
    return(p)
  })
  output$legend3<- renderPlot({
    if(input$map_display_radio == "Labor_Pct_Change"){
      
      paint.brush = colorRampPalette(colors=c("darkgreen", "white", "maroon"))
      cols <- paint.brush(25)
      leg_dat <- data_frame(y = seq(pctmin.val, pctmax.val,length.out=(length(map_colors)-1)), x = 1, col = cols)
      
      b <- ggplot(data = leg_dat) +
        geom_tile(aes(y = y, fill = reorder(col, y), x = x), show_guide = FALSE) +
        scale_y_continuous(limits = c(pctmin.val, pctmax.val), breaks = seq(pctmin.val, pctmax.val, length.out = 5)) +
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
  output$legend2 <- renderPlot({  
    if(input$map_display_radio == "No_Labor_Avg"){
      paint.brush = colorRampPalette(colors=c("darkgreen", "white", "maroon"))
      cols <- paint.brush(length(map_colors)-1)
      leg_dat<- data_frame(y = seq(labmin.val, labmax.val,length.out=(length(map_colors)-1)), x = 1, col = cols)
      
      q<- ggplot(data = leg_dat) +
        geom_tile(aes(y = y, fill = reorder(col, y), x = x), show_guide = FALSE) +
        scale_y_continuous(limits = c(labmin.val, labmax.val), breaks = seq(labmin.val, labmax.val, length.out = 5)) +
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
    
    return(q)
    
  })
})