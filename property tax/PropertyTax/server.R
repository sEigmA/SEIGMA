#######################################
## Title: Property Tax server.R      ##
## Author(s): Xuelian Li, Jenna      ##
##            Kiridly, Zhenning Kang ## 
## Date Created:  01/07/16           ##
## Date Modified: 05/01/18 ZK        ##
##                01/14/19 VE        ##
#######################################

shinyServer(function(input, output, session){
  # tax_df is a reactive dataframe. Necessary for when summary/plot/map have common input (Multiple Variables). Not in this project
  tax_df <- reactive({
    tax_df <- tax_data
    ## Output reactive dataframe
    tax_df
  })
  
  ## Create summary table
  output$summary <- renderDataTable({
    ## Make reactive dataframe into regular dataframe
    tax_df <- tax_df()
    
    ## if a user chooses Single Year, display only data from that year (dpylr)
    if(input$sum_timespan == "sing.yr"){
      df <- filter(tax_df, Year==input$sum_year)
    }
    
    ## if a user chooses Multiple Years, display data from all years in range
    if(input$sum_timespan == "mult.yrs"){
      range <- seq(min(input$sum_range), max(input$sum_range), 1)
      df <- c()
      
      for(i in 1:length(range)){
        bbb <- subset(tax_df, Year==range[i])
        df <- rbind.data.frame(df, bbb)
      }
    }
    
    ## make municipals a vector based on input variable
    if(!is.null(input$sum_muni))
      munis <- input$sum_muni
    ## if none selected, put all municipals in vector
    if(is.null(input$sum_muni))
      munis <- MA_municipals
    
    ## create a dataframe consisting only of counties in vector
    sum_df <- df %>%
      filter(Municipal %in% munis) %>%
      select(Municipal, Year, Inflation_Adjusted_Total_Levy, Inflation_Adjusted_Residential, Inflation_Adjusted_Open_Space, Inflation_Adjusted_Commercial, Inflation_Adjusted_Industrial, Inflation_Adjusted_Personal_Property)
      sum_df[3:8]<-apply(sum_df[3:8],2,function(x)prettyNum(x,big.mark = ","))
    colnames(sum_df) <- c("Municipal","Fiscal Year", "Total Levy (2018 dollars)", "Residential (2018 dollars)", "Open Space (2018 dollars) ", "Commercial (2018 dollars)", "Industrial (2018 dollars)", "Personal Property (2018 dollars)")
    
    return(sum_df)
  }, options = list(searching = FALSE, orderClasses = TRUE)) # there are a bunch of options to edit the appearance of datatables, this removes one of the ugly features
  
  ##############################################
  
  ## create the plot of the data
  ## for the Google charts plot
  plot_dat<- reactive({
    ## make reactive dataframe into regular dataframe
    
    tax_df <- tax_df()
    
    ##Choose column according input
    ##col_sel_num<-c()
    if (input$plot_radio == "Total_Levy") {
      ## make region a vector based on input variable
      munis <- input$plot_muni
      col<-input$plot_display_radio
      col_sel_num<-which( colnames(tax_df)==col )
    }
    else {
      ## make region a vector based on input variable
      munis <- input$plot_muni2
      col_sel_num<-c(22,23,25:27)
    }
    
    ## assign column to each entry in the data frame
    ##col_sel_num1<-which( colnames(tax_df)==col )
    plot_dat <- tax_df %>%
      filter(Municipal %in% munis) %>%
      select(1, 2, col_sel_num)
    
    return(plot_dat)
    
  })
  

  ## for the Google charts plot (Total Tax plot)
  output$TotTax_plot1 <- reactive({
    ## make reactive dataframe into regular dataframe
    if (input$plot_radio == "Total_Levy"& input$plot_display_radio=="Total_Levy_Million"){
    TotTax_df <- plot_dat()
    TotTax_df<-TotTax_df%>%
      spread("Municipal", "Total_Levy_Million")
    
      list(
        data=googleDataTable(TotTax_df))  
    }
     })
  
  ##change in Total tax Plot
  output$TotTax_plot2 <- reactive({
    if (input$plot_radio == "Total_Levy"& input$plot_display_radio=="Total_Levy_Pct_Change"){
    ## make reactive dataframe into regular dataframe
    TotTax2_df <- plot_dat()
    TotTax2_df<-TotTax2_df%>%
      spread("Municipal", "Total_Levy_Pct_Change")
    list(
      data=googleDataTable(TotTax2_df))
    }
  })
  
  output$pct_plot1 <- reactive({
    if (input$plot_radio == "Percent_Levy") {
    ## make reactive dataframe into regular dataframe
    pct_df <- plot_dat()
    ymax<-max(pct_df[,3])+2
    pct_df[,2]<-as.character(pct_df[,2])
    pct_df1 <- pct_df[,-c(1,3)]
    colnames(pct_df1) <- gsub("_Million", "", colnames(pct_df1))
    list(
      data=googleDataTable(pct_df1),
      options = list(
        ## set fonts
        fontName = "Source Sans Pro",
        fontSize = font_size,
        title = paste("Total Tax Levy and Percent of Levy by Class FY2003-FY2018 (2018 dollars)", 
                       "at ", input$plot_muni2),
        ## set axis titles, ticks, fonts, and ranges
        hAxis = list(
          title = "",
          ticks = seq(2003,2018,1),
          format = "####",
          textStyle = list(
            fontSize = font_size),
          titleTextStyle = list(
            fontSize = font_size+2,
            bold = TRUE,
            italic = FALSE)
        ),
        vAxis = list(
          title = "Tax Levy in $Millions",
          viewWindow = ymax,
          textStyle = list(
            fontSize = font_size),
          titleTextStyle = list(
            fontSize = font_size+2,
            bold = TRUE,
            italic = FALSE)
        ),
        
        ## set legend fonts
        legend = list(
          position = "in"),
        
        ## set chart area padding
        chartArea = list(
          top = 50, left = 75,
          height = "75%", width = "70%"
        ),
        
        ## set colors
        ##colors = cbbPalette[c(3:7)],
        colors=c("cornflowerblue", "gray", "yellow", "blue"),
        ## set point size
        pointSize = 3,
        
        ## set tooltip font size
        ## Hover text font stuff
        tooltip = list(
          textStyle = list(
            fontSize = font_size
          )
        ),
        
        ## stacked column
        isStacked= TRUE
      ))
    }
  })
  ###################MAP CREATION##############
  map_dat <- reactive({
    ## make reactive dataframe into regular dataframe
    tax_df <- tax_df()%>%
      filter(Year == input$map_year)
      
    ## get column name and cuts based on input
    ##Choose column according input
    if (input$map_radio == "Total_Levy") {
      col<-input$map_display_radio
      if (input$map_display_radio=="Inflation_Adjusted_Total_Levy"){
      cuts<-TotTaxcuts 
      }
      else {
        cuts<-TaxChacuts  
      }
      }
    else{
      col<-input$map_class_radio
      cuts<-pctcuts
      }
    
    ## assign colors to each entry in the data frame
    col_sel_num1<-which( colnames(tax_df)==col )
    map_dat <- select(tax_df,c(1,2,col_sel_num1))
    color <- as.integer(cut2(map_dat[,col],cuts=cuts))
    map_dat <- cbind.data.frame(map_dat, color)
    map_dat$color <- ifelse(is.na(map_dat$color), length(map_colors),
                            map_dat$color)
    map_dat$opacity <- 0.7
    ## find missing counties in data subset and assign NAs to all values
    missing.munis <- setdiff(leftover_munis_map,tax_df$Municipal)
    missing_df <-data.frame(Municipal=missing.munis, Year=input$map_year, Map_var=NA,
                            color=length(map_colors), opacity = 0)
    colnames(missing_df)[3]<-col
    # combine data subset with missing counties data
    map_dat <- rbind.data.frame(map_dat, missing_df)
    if (input$map_radio == "Total_Levy" & input$map_display_radio=="Total_Levy_Pct_Change"){
      map_dat$color <- map_colors1[map_dat$color]
    }
    else{
      map_dat$color <- map_colors[map_dat$color]
    }
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
          map_dat[match(x$features[[i]]$properties$NAMELSAD10, map_dat$Municipal), col_name]
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
    col_name<-colnames(map_dat)[3]
    isolate({
      values$selectedFeature <- evt$properties
      region <- evt$properties$NAMELSAD10
      values$selectedFeature[col_name] <- map_dat[match(region, map_dat$Municipal), col_name]
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
    if(input$map_display_radio == "Inflation_Adjusted_Total_Levy"){
      muni_value <- prettyNum(values$selectedFeature[col_name], big.mark = ",")
    }
    if(input$map_display_radio == "Total_Levy_Pct_Change"|| input$map_radio == "Percent_Levy"){
      muni_value <- prettyNum(values$selectedFeature[col_name],digits=2)
    }
    var_select <- gsub("_", " ", col_name)
    
    ## If clicked county has no crude rate, display a message
    if(muni_value == "NULL"){
      return(as.character(tags$div(
        tags$h5("Annual Total Tax Levy for", muni_name, "is not available for this timespan"))))
    }
    ## For a single year when county is clicked, display a message
    if (input$map_radio == "Total_Levy") {
      if(input$map_display_radio == "Inflation_Adjusted_Total_Levy"){
      return(as.character(tags$div(
        tags$h4("Annual Total Tax Levy (2017 dollars) in", muni_name, " for ", input$map_year),
        tags$h5(muni_value)
      )))
    }
    else{
      return(as.character(tags$div(
        tags$h4("Change in Total Tax Levy in", muni_name, " for ", input$map_year,"since year 2003"),
        tags$h5(muni_value,"%")
      )))
    }
  }
    else{
          return(as.character(tags$div(
          tags$h4(var_select, "property tax in", muni_name, " for ", input$map_year),
          tags$h5(muni_value, "%")
        )))
      }
  })
  
  ## map legend
  output$legend1 <- renderPlot({
    if(input$map_radio == "Total_Levy"&input$map_display_radio == "Inflation_Adjusted_Total_Levy"){
      
      paint.brush = colorRampPalette(colors=c("white", "violetred"))
      cols <- paint.brush(25)
      leg_dat <- data_frame(y = seq(TotTaxmin.val, TotTaxmax.val,length.out=(length(map_colors)-1)), x = 1, col = cols)
      
      p <- ggplot(data = leg_dat) +
        geom_tile(aes(y = y, fill = reorder(col,y), x = x), show.legend = FALSE) +
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
    if(input$map_radio == "Total_Levy"&input$map_display_radio == "Total_Levy_Pct_Change"){
      
      paint.brush = colorRampPalette(colors=c("darkgreen", "white", "maroon"))
      cols <- paint.brush(25)
      leg_dat <- data_frame(y = seq(TaxChamin.val, TaxChamax.val,length.out=(length(map_colors1)-1)), x = 1, col = cols)
      
      b <- ggplot(data = leg_dat) +
        geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
        scale_y_continuous(limits = c(TaxChamin.val, TaxChamax.val), breaks = seq(TaxChamin.val, TaxChamax.val, length.out = 5)) +
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
    if(input$map_radio == "Percent_Levy"){
      paint.brush = colorRampPalette(colors=c("white", "violetred"))
      cols <- paint.brush(length(map_colors1)-1)
      leg_dat<- data_frame(y = seq(pctmin.val, pctmax.val,length.out=(length(map_colors)-1)), x = 1, col = cols)
      
      q<- ggplot(data = leg_dat) +
        geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
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
    
    return(q)
    
  })
})