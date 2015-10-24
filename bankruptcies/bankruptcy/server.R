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
      filter(Region %in% munis,Year %in% input$sum_year) %>%
      select(1, 16, sel_col_num)
    
    colnames(bank_df) <- gsub("_", " ", colnames(bank_df))
        
    return(bank_df)
  }, options=list(searching = FALSE, orderClasses = TRUE)) # there are a bunch of options to edit the appearance of datatables, this removes one of the ugly features
  ## create the plot of the data
  plot_dat<- reactive({
    plot_df<-bank_df()
    counties <- input$plot_county
    if(input$plot_radio =='Business Filings'){
     if(input$plot_bus_display != 'Business_Filings_Total'){
      if(input$plot_US){
        if(input$plot_MA){
          counties <- c("United States", "MA", counties) ## US and MA
        } else {
          counties <- c("United States", counties) ## US only
        }
      } else{
        if(input$plot_MA){
          counties <- c("MA", counties)  ## MA only
        }
      }
    }
    } else {
     if (input$plot_nonbus_display != 'Personal_Filings_Total') {
       if(input$plot2_US){
         if(input$plot2_MA){
           counties <- c("United States", "MA", counties) ## US and MA
         } else {
           counties <- c("United States", counties) ## US only
         }
       } else{
         if(input$plot2_MA){
           counties <- c("MA", counties)  ## MA only
         }
       } 
     }
    }
    ##Choose column according input
    if (input$plot_radio == "Business Filings") {
      col<-input$plot_bus_display
    }
    else {
      col<-input$plot_nonbus_display
    }
    
    ## assign column to each entry in the data frame
    col_sel_num1<-which( colnames(plot_df)==col )
    plot_dat1 <- plot_df %>%
      filter(Region %in% counties) %>%
      select(1, 16, col_sel_num1)
    plot_dat<-plot_dat1%>%
      spread_("Region", col)
    return(plot_dat)
  })
  
  ## for the Google charts plot (Business Filings plot)
  output$Bus_plot <- reactive({
    ## make reactive dataframe into regular dataframe
    bus_df <- plot_dat()
    Bnames <- gsub("_", " ", input$plot_bus_display)
    list(
      data=googleDataTable(bus_df),options = list(title=paste("Bankruptcies",Bnames,
                                                              "Over Time "),fontSize = 19,bold = TRUE))
  })
  
  ##Personal Filings Plot
  output$NonBus_plot <- reactive({
    ## make reactive dataframe into regular dataframe
    nonbus_df <- plot_dat()
    Nnames <- gsub("_", " ", input$plot_nonbus_display)
    list(
      data=googleDataTable(nonbus_df),options = list(title=paste("Bankruptcies", Nnames,
                                                                 "Over Time "),fontSize = 19,bold = TRUE))
  })
  
  ##proportion by chapter in Business Filings
  output$Pro_Bus_plot <- reactive({
    ## make reactive dataframe into regular dataframe
    pro_bus_df <- plot_dat()
    Bpnames <- gsub("_", " ", input$plot_bus_display)
    list(
      data=googleDataTable(pro_bus_df),options = list(title=paste(Bpnames,
                                                                  "Over Time "),fontSize = 19,bold = TRUE))
  })
  ##proportion by chapter in Personal Filings
  output$Pro_NonBus_plot <- reactive({
    ## make reactive dataframe into regular dataframe
    pro_nonbus_df <- plot_dat()
    Npnames <- gsub("_", " ", input$plot_nonbus_display)
    list(
      data=googleDataTable(pro_nonbus_df),options = list(title=paste(Npnames,
                                                                     "Over Time "),fontSize = 19,bold = TRUE))
  })
   
  ##map
  map_dat <- reactive({
    ## make reactive dataframe into regular dataframe
    bank_dat <- bank_df()%>%
      filter(Year == input$map_year)%>%
      ## take US, MA, and counties out of map_dat
      filter(Region!="MA"&Region!="United States")
    
    
    ## get column name and cuts based on input
    ##Choose column according input
    if (input$map_radio == "Business Filings") {
      col<-input$map_bus_display
      if(input$map_bus_display=="Business_Filings_Total") {
        cuts<-buscuts
      }
      else{
        cuts<-procuts
      }
    }
    else {
      col<-input$map_nonbus_display
      if(input$map_nonbus_display=="Personal_Filings_Total") {
        cuts<-nonbuscuts
      }
      else{
        cuts<-procuts
      }
    }
    
    ## assign colors to each entry in the data frame
    col_sel_num1<-which( colnames(bank_dat)==col )
    map_dat <- select(bank_dat,c(1,16,col_sel_num1))
    color <- as.integer(cut2(map_dat[,col],cuts=cuts))
    map_dat <- cbind.data.frame(map_dat, color)
    map_dat$color <- ifelse(is.na(map_dat$color), length(map_colors),
                            map_dat$color)
    map_dat$opacity <- 0.7
    
    ## find missing counties in data subset and assign NAs to all values
    ##missing_munis <- setdiff(leftover_munis_map, map_dat$Region)
    ##missing_df <- data.frame(Municipal = NA, County = NA, State = "MA",
                             ##Region = missing_munis, Five_Year_Range = input$map_year,
                             ##Total_Population = NA,
                             ##Map_var = NA, color=length(map_colors), opacity = 0)
    ##colnames(missing_df)[7]<-col
    # combine data subset with missing counties data
    ##map_dat <- rbind.data.frame(map_dat, missing_df)
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
    col_name<-colnames(map_dat)[3]
    ## All functions which are isolated, will not run until the above observe function is activated
    isolate({
      ## Duplicate MAmap to x
      x <-  MAmap
      
      ## for each county in the map, attach the Crude Rate and colors associated
      for(i in 1:length(x$features)){
        ## Each feature is a county
        x$features[[i]]$properties[col_name] <-
          map_dat[match(x$features[[i]]$properties$County, map_dat$Region), col_name]
        ## Style properties
        x$features[[i]]$properties$style <- list(
          fill=TRUE,
          ## Fill color has to be equal to the map_dat color and is matched by county
          fillColor = map_dat$color[match(x$features[[i]]$properties$County, map_dat$Region)],
          ## "#000000" = Black, "#999999"=Grey,
          weight=1, stroke=TRUE,
          opacity=map_dat$opacity[match(x$features[[i]]$properties$County, map_dat$Region)],
          color="#000000",
          fillOpacity=map_dat$opacity[match(x$features[[i]]$properties$County, map_dat$Region)])
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
      region <- evt$properties$County
      values$selectedFeature[col_name] <- map_dat[match(region, map_dat$Region), col_name]
    })
  })
  ## map legend
  output$legend <- renderPlot({
    if(input$map_radio =='Personal Filings' && input$map_nonbus_display != 'Personal_Filings_Total'){
      
      paint.brush = colorRampPalette(c("white","red3"))
      cols <- paint.brush(101)
      leg_dat <- data_frame(y = seq(0, 100), x = 1, col = cols)
      
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
  output$legend3 <- renderPlot({
      if(input$map_radio =='Business Filings' && input$map_nonbus_display != 'Business_Filings_Total'){
      
      paint.brush = colorRampPalette(c("white", "red3"))
      cols <- paint.brush(101)
      leg_dat <- data_frame(y = seq(0, 100), x = 1, col = cols)
      
      b <- ggplot(data = leg_dat) +
        geom_tile(aes(y = y, fill = reorder(col, y), x = x), show_guide = FALSE) +
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
  output$legend1 <- renderPlot({  
  if(input$map_radio =='Personal Filings' && input$map_nonbus_display == 'Personal_Filings_Total'){
      paint.brush = colorRampPalette(c("white", "red3"))
      cols <- paint.brush(length(map_colors)-1)
      leg_dat<- data_frame(y = seq(nonbusmin.val, nonbusmax.val,length.out=(length(map_colors)-1)), x = 1, col = cols)
      
      q<- ggplot(data = leg_dat) +
        geom_tile(aes(y = y, fill = reorder(col, y), x = x), show_guide = FALSE) +
        scale_y_continuous(limits = c(nonbusmin.val, nonbusmax.val), breaks = seq(nonbusmin.val, nonbusmax.val, length.out = 5)) +
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
  output$legend2 <- renderPlot({  
    if(input$map_radio =='Business Filings' && input$map_bus_display == 'Business_Filings_Total'){
      paint.brush = colorRampPalette(c("white", "red3"))
      cols <- paint.brush(length(map_colors)-1)
      leg_dat <- data_frame(y = seq(busmin.val, busmax.val, length.out=(length(map_colors)-1)), x = 1, col = cols)
      
      d<- ggplot(data = leg_dat) +
        geom_tile(aes(y = y, fill = reorder(col, y), x = x), show_guide = FALSE) +
        scale_y_continuous(limits = c(busmin.val, busmax.val), breaks = round(seq(busmin.val, busmax.val, length.out = 8)),1) +
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
    
    return(d)
    
  })
  ##  This function is what creates info box
  output$details <- renderText({
    if (input$map_radio == "Business Filings") {
      col<-input$map_bus_display
    }
    else {
      col<-input$map_nonbus_display
    }
    ## Before a county is clicked, display a message
    if(is.null(values$selectedFeature)){
      return(as.character(tags$div(
        tags$div(
          h4("Click on a County"))
      )))
    }
    
    muni_name <- values$selectedFeature$County
    muni_value <- values$selectedFeature[col]
    var_select <- gsub("_", " ", col)
    
    
    ## If clicked county has no crude rate, display a message
    if(is.null(values$selectedFeature[col])){
      return(as.character(tags$div(
        tags$h5(var_select, "in ", muni_name, "is not available for this timespan"))))
    }
    ## For a single year when county is clicked, display a message
    if((input$map_radio=='Business Filings' & input$map_bus_display != 'Business_Filings_Total') | (input$map_radio=='Personal Filings' & input$map_nonbus_display != 'Personal_Filings_Total')){
     return(as.character(tags$div(
      tags$h4(var_select, "in ", muni_name, " for ", input$map_year),
      tags$h5(muni_value,"%")
    )))
    }
    else {
      return(as.character(tags$div(
        tags$h4(var_select, "in ", muni_name, " for ", input$map_year),
        tags$h5(muni_value)
      )))  
    }
  })
  ##########################################################
})

