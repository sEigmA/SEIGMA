#######################################
## Title:  Building Permits server.R ##
## Author(s): Xuelian Li, Zhenning   ##
##            Kang                   ## 
## Date Created:  08/10/16           ##
## Date Modified: 02/25/19 VE        ##
#######################################

shinyServer(function(input, output, session){
  # bPermit_df is a reactive dataframe. Necessary for when summary/plot/map have common input (Multiple Variables). Not in this project
  bPermit_df <- reactive({
    bPermit_df <- bPermit_data
    ## Output reactive dataframe
    bPermit_df
  })
  
  ## Create summary table
  output$summary <- renderDataTable({
    ## Make reactive dataframe into regular dataframe
    bPermit_df <- bPermit_df()
    
    ## if a user chooses Single Year, display only data from that year (dpylr)
    if(input$sum_timespan == "sing.yr"){
      df <- filter(bPermit_df, Year==input$sum_year)
    }
    
    ## if a user chooses Multiple Years, display data from all years in range
    if(input$sum_timespan == "mult.yrs"){
      range <- seq(min(input$sum_range), max(input$sum_range), 1)
      df <- c()
      
      for(i in 1:length(range)){
        bbb <- subset(bPermit_df, Year==range[i])
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
      select(1:19)
      sum_df[,c(6,9,12,15,18,19)]<-apply(sum_df[,c(6,9,12,15,18,19)],2,function(x)prettyNum(x,big.mark = ","))
    colnames(sum_df)[3:19] <- c("Number of Months Reported" ,"1-Family Buildings","1-Family Units","1-Family Valuation (2017 dollars)","2-Family Buildings","2-Family Units","2-Family Valuation (2017 dollars)","3-4-Family Buildings","3-4-Family Units","3-4-Family Valuation (2017 dollars)","5+ Family Buildings","5+ Family Units","5+ Family Valuation (2017 dollars)", "Total Buildings", "Total Units", "Total Valuation (2017 dollars)", "Average Valuation (2017 dollars)")
    sum_df1<-sum_df%>%
      select(1:3,16:19, 4:15)
    return(sum_df1)
  }, options = list(searching = FALSE, orderClasses = TRUE)) # there are a bunch of options to edit the appearance of datatables, this removes one of the ugly features
  
  ##############################################
  
  ## create the plot of the data
  ## for the Google charts plot
  plot_dat<- reactive({
    ## make reactive dataframe into regular dataframe
    
    bPermit_df <- bPermit_df()
    
    ##Choose column according input
    ##col_sel_num<-c()
    if (input$plot_radio == "Total_Permits") {
      ## make region a vector based on input variable
      munis <- input$plot_muni
      if (input$plot_MA ){
        munis <- c(munis, "MA")
      }
      col<-input$plot_display_radio
      col_sel_num<-which(colnames(bPermit_df)==col) 
    }
    else {
      ## make region a vector based on input variable
      munis <- input$plot_muni2
      col_sel_num<-c(5,8,11,14)
    }
    

    ## assign column to each entry in the data frame
    ##col_sel_num1<-which( colnames(bPermit_df)==col )
    plot_dat <- bPermit_df %>%
      filter(Region %in% munis) %>%
      select(1, 2, col_sel_num)
    
    return(plot_dat)
    
  })
  

  ## for the Google charts plot (Total Units plot)
  output$TotUni_plot1 <- reactive({
    ## make reactive dataframe into regular dataframe
    if (input$plot_radio == "Total_Permits"& input$plot_display_radio=="Total_Units_Reported_Imputed"){
    TotbPermit_df <- plot_dat()
    ylim_max<-max(TotbPermit_df[,3])+5
    TotbPermit_df<-TotbPermit_df%>%
      spread("Region", "Total_Units_Reported_Imputed")
    
      list(
        data=googleDataTable(TotbPermit_df),
        options = list(
          vAxis = list(
            viewWindow = ylim_max
        )))  
    }
     })
  
  ##change in Total Units since 2000 Plot
#   output$TotUniCha_plot <- reactive({
#     if (input$plot_radio == "Total_Permits"& input$plot_display_radio=="Total_Pct_Change"){
#     ## make reactive dataframe into regular dataframe
#     TotbPermit2_df <- plot_dat()
#     TotbPermit2_df<-TotbPermit2_df%>%
#       spread("Region", "Total_Pct_Change")
#       
#     list(
#       data=googleDataTable(TotbPermit2_df))
#     }
#   })
  
  ##change in Total Units from Previous year Plot
  output$PreUniCha_plot <- reactive({
    if (input$plot_radio == "Total_Permits"& input$plot_display_radio=="Pct_Change_from_previous"){
      ## make reactive dataframe into regular dataframe
      TotbPermit3_df <- plot_dat()
      ylim_pre_max<-max(TotbPermit3_df[,3])+5
      TotbPermit3_df<-TotbPermit3_df%>%
        spread("Region", "Pct_Change_from_previous")
      list(
        data=googleDataTable(TotbPermit3_df),
        options = list(
          
          vAxis = list(
            viewWindow = ylim_pre_max
            )))
    }
  })
  
  ##HOusing Units by structre
  output$pct_plot1 <- reactive({
    if (input$plot_radio == "Percent_Permits") {
    ## make reactive dataframe into regular dataframe
    pct_df <- plot_dat()
    ymax<-max(pct_df[,3])+5
    pct_df[,2]<-as.character(pct_df[,2])
    pct_df1 <- pct_df[,-1]
    colnames(pct_df1) <- gsub("_", "", colnames(pct_df1))
    colnames(pct_df1) <- gsub("I", "", colnames(pct_df1))
    list(
      data=googleDataTable(pct_df1),
      options = list(
        ## set fonts
        fontName = "Source Sans Pro",
        fontSize = font_size,
        title =paste("Number of New Housing Units by Structure Size for 2000-2017", 
                       "in ", input$plot_muni2),
          titleTextStyle = list(
            fontSize = font_size+8,
            bold = TRUE,
            italic = FALSE),
        ## set axis titles, ticks, fonts, and ranges
        hAxis = list(
          title = "",
          ticks = seq(2000,2017,1),
          format = "####",
          textStyle = list(
            fontSize = font_size),
          titleTextStyle = list(
            fontSize = font_size+2,
            bold = TRUE,
            italic = FALSE)
        ),
        vAxis = list(
          title = "Number of new Housing units",
          viewWindow = ymax,
          textStyle = list(
            fontSize = font_size),
          titleTextStyle = list(
            fontSize = font_size+4,
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
        isStacked= F
      ))
    }
  })
  ###################MAP CREATION##############
  map_dat <- reactive({
    ## make reactive dataframe into regular dataframe
    bPermit_df <- bPermit_df()%>%
      filter(Year == input$map_year)
      
    ## get column name and cuts based on input
    ##Choose column according input
    if (input$map_radio == "Total_Permits") {
      col<-input$map_display_radio
      if (input$map_display_radio=="Total_Units_Reported_Imputed"){
      cuts<-TotUnitscuts 
      }
#       else if (input$map_display_radio=="Total_Pct_Change"){
#         cuts<-TotUniChacuts 
#       }
      else {
        cuts<-PreChacuts  
      }
    }
    else if (input$map_radio == "Permits_Per_1000_Population"){
      col<-input$map_radio
      cuts<-UniPerPopcuts
    }
    else{
      col<-input$map_class_radio
      cuts<-pctcuts
      }
    
    ## assign colors to each entry in the data frame
    col_sel_num1<-which( colnames(bPermit_df)==col )
    map_dat <- select(bPermit_df,c(1,2,col_sel_num1))
    color <- as.integer(cut2(map_dat[,col],cuts=cuts))
    map_dat <- cbind.data.frame(map_dat, color)
    map_dat$color <- ifelse(is.na(map_dat$color), length(map_colors),
                            map_dat$color)
    map_dat$opacity <- 0.7
    ## find missing counties in data subset and assign NAs to all values
    missing.munis <- setdiff(leftover_munis_map,bPermit_df$Region)
    missing_df <-data.frame(Region=missing.munis, Year=input$map_year, Map_var=NA,
                            color=length(map_colors), opacity = 0)
    colnames(missing_df)[3]<-col
    # combine data subset with missing counties data
    map_dat <- rbind.data.frame(map_dat, missing_df)
    if (input$map_radio == "Total_Permits" & input$map_display_radio=="Pct_Change_from_previous"){
      map_dat$color <- map_colors1[map_dat$color]
    }
#     else if (input$map_radio == "Total_Permits" & input$map_display_radio=="Total_Pct_Change"){
#       map_dat$color <- map_colors1[map_dat$color]
#     }
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
    if (input$map_radio == 'Total_Permits'&& input$map_display_radio == "Total_Units_Reported_Imputed"){
     muni_value <- prettyNum(values$selectedFeature[col_name], big.mark = ",")
    }
    else {
      muni_value <- prettyNum(values$selectedFeature[col_name],digits=2)
    }
    
    var_select <- gsub("Reported_Imputed", "", col_name)
    var_select <- gsub("_", " ", var_select)
    ## If clicked county has no crude rate, display a message
    if(muni_value == "NULL"){
      return(as.character(tags$div(
        tags$h5("Annual Total Number of New Housing Units for", muni_name, "is not available for this timespan"))))
    }
    ## For a single year when county is clicked, display a message
    if (input$map_radio == "Total_Permits") {
      if(input$map_display_radio == "Total_Units_Reported_Imputed"){
      return(as.character(tags$div(
        tags$h4("Annual Total Number of New Housing Units in", muni_name, " for ", input$map_year),
        tags$h5(muni_value)
      )))
    }
#     else if(input$map_display_radio == "Total_Pct_Change"){
#       return(as.character(tags$div(
#         tags$h4("Change in Total Housing Permits in", muni_name, " for ", input$map_year,"since year 2000"),
#         tags$h5(muni_value,"%")
#       )))
#     }
    else {
      return(as.character(tags$div(
        tags$h4("Change in Total Number of New Housing Units in", muni_name, " for ", input$map_year,"from Previous Year"),
        tags$h5(muni_value,"%")
      )))
    }
    }
    else if (input$map_radio == "Permits_Per_1000_Population"){
      return(as.character(tags$div(
        tags$h4("New Housing Units Per 1000 Population in", muni_name, " for ", input$map_year),
        tags$h5(muni_value)
      )))
    }
    else{
          return(as.character(tags$div(
          tags$h4(var_select, "Permits in", muni_name, " for ", input$map_year),
          tags$h5(muni_value, "%")
        )))
      }
  })
  
  ## map legend
  output$legend1 <- renderPlot({
    if(input$map_radio == "Total_Permits"&input$map_display_radio == "Total_Units_Reported_Imputed"){
      
      paint.brush = colorRampPalette(colors=c("white", "violetred"))
      cols <- paint.brush(22)
      leg_dat <- data_frame(y = TotUnitscuts, x = 1, col = cols)
      
      p <- ggplot(data = leg_dat) +
        geom_tile(aes(y = y, fill = reorder(col,y), x = x), show.legend = FALSE) +
                scale_fill_manual(values = leg_dat$col) + theme_bw() +
        scale_y_log10(limits = c(1, max(TotUnitscuts)), breaks = round(TotUnitscuts,0))+
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
#   output$legend3<- renderPlot({
#     if(input$map_radio == "Total_Permits"&input$map_display_radio == "Total_Pct_Change"){
#       
#       paint.brush = colorRampPalette(colors=c("darkgreen", "white", "maroon"))
#       cols <- paint.brush(25)
#       leg_dat <- data_frame(y = seq(TotUniChamin.val, TotUniChamax.val,length.out=(length(map_colors1)-1)), x = 1, col = cols)
#       
#       b <- ggplot(data = leg_dat) +
#         geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
#         scale_y_continuous(limits = c(TotUniChamin.val, TotUniChamax.val), breaks = seq(TotUniChamin.val, TotUniChamax.val, length.out = 5)) +
#         scale_fill_manual(values = leg_dat$col) + theme_bw() +
#         theme(axis.text.x = element_blank(),
#               axis.text.y = element_text(size = 12),
#               axis.title.x = element_blank(),
#               axis.title.y = element_blank(),
#               axis.ticks.x = element_blank(),
#               panel.border = element_blank(),
#               panel.grid.minor = element_blank(),
#               panel.grid.major = element_blank())
#       
#     }
#     return(b)
#   })
  output$legend2 <- renderPlot({  
    if(input$map_radio == "Percent_Permits"){
      paint.brush = colorRampPalette(colors=c("white", "violetred"))
      cols <- paint.brush(length(map_colors)-1)
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
  
#   output$legend4<- renderPlot({
#     if(input$map_radio == "Total_Permits"&input$map_display_radio == "Pct_Change_from_previous"){
#       
#       paint.brush = colorRampPalette(colors=c("darkgreen", "white", "maroon"))
#       cols <- paint.brush(25)
#       leg_dat <- data_frame(y = unique(c(seq(PreChamin.val, 0, length.out = 13), seq(0, PreChamax.val, length.out = 13))), x = 1, col = cols)
#       
#       d <- ggplot(data = leg_dat) +
#         geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
#         scale_y_continuous(limits = c(PreChamin.val, PreChamax.val), breaks = unique(c(seq(PreChamin.val, 0,length.out = 3),seq(0,PreChamax.val, length.out = 3)))) +
#         scale_fill_manual(values = leg_dat$col) + theme_bw() +
#         theme(axis.text.x = element_blank(),
#               axis.text.y = element_text(size = 12),
#               axis.title.x = element_blank(),
#               axis.title.y = element_blank(),
#               axis.ticks.x = element_blank(),
#               panel.border = element_blank(),
#               panel.grid.minor = element_blank(),
#               panel.grid.major = element_blank())
#       
#     }
#     return(d)
#   })
  
  output$legend5 <- renderPlot({
    if(input$map_radio == "Permits_Per_1000_Population"){
      
      paint.brush = colorRampPalette(colors=c("white", "violetred"))
      cols <- paint.brush(22)
      leg_dat <- data_frame(y = UniPerPopcuts, x = 1, col = cols)
      
      e <- ggplot(data = leg_dat) +
        geom_tile(aes(y = y, fill = reorder(col,y), x = x), show.legend = FALSE) +
        scale_fill_manual(values = leg_dat$col) + theme_bw() +
        scale_y_log10(limits = c(1, max(UniPerPopcuts)), breaks = round(UniPerPopcuts,0))+
        theme(axis.text.x = element_blank(),
              axis.text.y = element_text(size = 12),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks.x = element_blank(),
              panel.border = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank())
      
    }
    return(e)
  })
})