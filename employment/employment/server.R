###########################################
## Title: employment Data Cleaning       ##
## Author(s): Xuelian Li, Emily Ramos,   ## 
##            Arvind Ramakrishnan,       ##
##            Jenna Kiridly, Steve Lauer ## 
## Date Created:  02/04/2015             ##
## Date Modified: 07/13/2015             ##
###########################################


shinyServer(function(input, output, session) {
  # emp_df is a reactive dataframe. Necessary for when summary/plot/map have common input (Multiple Variables). Not in this project


  #   sum_df <- emp_df %>%
  #     filter(Region %in% munis) %>%
  #     select(4:length(colnames(df)))

  emp_df <- reactive({
    ## Filter the data by the chosen Year
    emp_df <- emp_data ## %>%
    ##select(1:6, 9,11,13)

    ## Output reactive dataframe
    emp_df
  })

  ## Create summary table
  output$summary <- renderDataTable({
    ## Make reactive dataframe into regular dataframe
    emp_df <- emp_df()

    ## if a user chooses Single Year, display only data from that year (dpylr)
    if(input$sum_timespan == "sing.yr"){
      df <- filter(emp_df, Year==input$sum_year)
    }

    ## if a user chooses Multiple Years, display data from all years in range
    if(input$sum_timespan == "mult.yrs"){
      range <- seq(min(input$sum_range), max(input$sum_range), 1)
      df <- c()

      for(i in 1:length(range)){
        bbb <- subset(emp_df, Year==range[i])
        df <- rbind.data.frame(df, bbb)
      }
    }

    ## make municipals a vector based on input variable
    ## For when something is clicked

    if(!is.null(input$sum_muni))
      munis <- input$sum_muni
    ## if none selected, put all municipals in vector
    ## For nothing clicked
    if(is.null(input$sum_muni))
      munis <- MA_municipals

    ## create a dataframe consisting only of counties in vector
    sum_df <- df %>%
      filter(Municipal %in% munis) %>%
      arrange(Municipal)%>%
      select(Municipal, Year, Average_Monthly_Employment, Inflation_Adjusted_Average_Weekly_Wage, Number_of_Employer_Establishments)

    colnames(sum_df) <- c("Municipal","Year","Average Monthly Employment","Average Weekly Wage (2012 dollars)", "Number of Business Establishments")

    return(sum_df)
  }, options = list(searching = FALSE, orderClasses = TRUE))

  # there are a bunch of options to edit the appearance of datatables, this removes one of the ugly features


  ## create the plot of the data
  
  output$Wage_plot1<-reactive({
    wage_df<-emp_df()
    munis <- input$plot_muni
    w <- wage_df %>%
      filter(Municipal %in% munis) %>%
      select(Municipal, Year, Inflation_Adjusted_Average_Weekly_Wage) %>%
      spread(Municipal, Inflation_Adjusted_Average_Weekly_Wage)
    list(
      data=googleDataTable(w))
  })
  
  output$Wage_pct_plot<-reactive({
    wage_df<-emp_df()
    munis <- input$plot_muni
    w_pct <- wage_df %>%
      filter(Municipal %in% munis) %>%
      select(Municipal, Year, Average_Weekly_Wage_difference) %>%
      spread(Municipal, Average_Weekly_Wage_difference)
    list(
      data=googleDataTable(w_pct))
  })
    
  ## for the Google charts plot
  output$Emp_plot1 <- reactive({
#   browser()
    ## make reactive dataframe into regular dataframe
    emp_df <- emp_df()

    ## make region a vector based on input variable
    munis <- input$plot_muni

    ## if counties are selected and MA or US mean boxes are selected, add those to dataframe
#     if(!is.null(input$plot_muni)){
#       if(input$MA_mean)
#         munis <- c(plot_muni, "MA")
#
#       if(input$US_mean)
#         munis <- c(munis, "United States")
#     }

    ## if no counties have been selected, just show the US average
#     if(is.null(input$plot_muni)){
#       ## make region a vector based on input variable
#       munis <- "MA"
#     }

    ## put data into form that googleCharts understands (this unmelts the dataframe)
#      g <- dcast(emp_df, Year ~ munis, value.var="Average_Monthly_Employment")
g <- emp_df %>%
    filter(Municipal %in% munis) %>%
    select( Municipal, Year, Average_Monthly_Employment) %>%
    spread(Municipal, Average_Monthly_Employment)

#     g <- emp_df %>%
#       filter(Municipal %in% munis) %>%
#       select( Municipal, Year, Average_Monthly_Employment) %>%
#       spread(Municipal, Average_Monthly_Employment)


    ## this outputs the google data to be used in the UI to create the dataframe
    list(
      data=googleDataTable(g))
  })
output$Est_plot1 <- reactive({
  #   browser()
  ## make reactive dataframe into regular dataframe
  emp_df <- emp_df()
  
  ## make region a vector based on input variable
  munis <- input$plot_muni

  est <- emp_df %>%
    filter(Municipal %in% munis) %>%
    select( Municipal, Year, Number_of_Employer_Establishments) %>%
    spread(Municipal, Number_of_Employer_Establishments)
  
    
  ## this outputs the google data to be used in the UI to create the dataframe
  list(
    data=googleDataTable(est))
})

output$Emp_pct_plot<-reactive({
  emp_df<-emp_df()
  munis <- input$plot_muni
  emp_pct <- emp_df %>%
    filter(Municipal %in% munis) %>%
    select(Municipal, Year, Employment_difference) %>%
    spread(Municipal, Employment_difference)
  list(
    data=googleDataTable(emp_pct))
})

output$Est_pct_plot<-reactive({
  emp_df<-emp_df()
  munis <- input$plot_muni
  est_pct <- emp_df %>%
    filter(Municipal %in% munis) %>%
    select(Municipal, Year, Establishment_difference) %>%
    spread(Municipal, Establishment_difference)
  list(
    data=googleDataTable(est_pct))
})



  ###################MAP CREATION##############

  ## set map colors
  map_dat <- reactive({
    ## Browser command - Stops the app right when it's about to break
    ## make reactive dataframe into regular dataframe
    emp_df <- emp_df()
    ## subset the data by the year selected
    emp_df <- filter(emp_df, Year==input$map_year)
    ## get column name and cuts based on input
    if (input$map_display_radio == "Actual Values"){
      if (input$map_radio == "Employment") {
        col<-"Average_Monthly_Employment"  
        cuts<-empcuts
      }
      else if (input$map_radio == "Establishments"){
        col<-"Number_of_Employer_Establishments"
        cuts<-estcuts
      }
      else {
        col<-"Inflation_Adjusted_Average_Weekly_Wage"
        cuts<-wagecuts
      }
   
    ## assign colors to each entry in the data frame
    col_sel_num1<-which( colnames(emp_df)==col )
    map_dat <- select(emp_df,c(1,3,col_sel_num1))
    color <- as.integer(cut2(map_dat[,col],cuts=cuts))
    map_dat <- cbind.data.frame(map_dat, color)
    map_dat$color <- ifelse(is.na(map_dat$color), length(map_colors),
                            map_dat$color)
    map_dat$opacity <- 0.7
    
    ## find missing counties in data subset and assign NAs to all values
    missing.munis <- setdiff(leftover_munis_map,emp_df$Municipal)
    missing_df <-data.frame(Municipal=missing.munis, Year=input$map_year, Map_var=NA,
                            color=length(map_colors), opacity = 0)
    colnames(missing_df)[3]<-col
    # combine data subset with missing counties data
    map_dat <- rbind.data.frame(map_dat, missing_df)
    map_dat$color <- map_colors[map_dat$color]
    return(map_dat)
    } 
    if (input$map_display_radio == "Change_Pct"){
      if (input$map_radio == "Employment") {
        col<-"Employment_difference"  
        cuts<-pctcuts
      }
      else if (input$map_radio == "Establishments"){
        col<-"Establishment_difference"
        cuts<-pctcuts
      }
      else {
        col<-"Average_Weekly_Wage_difference"
        cuts<-pctcuts
      }
      col_sel_num1<-which( colnames(emp_df)==col )
      map_dat <- select(emp_df,c(1,3,col_sel_num1))
      color <- as.integer(cut2(map_dat[,col],cuts=cuts))
      map_dat <- cbind.data.frame(map_dat, color)
      map_dat$color <- ifelse(is.na(map_dat$color), length(pctmap_colors),
                              map_dat$color)
      map_dat$opacity <- 0.7
      
      ## find missing counties in data subset and assign NAs to all values
      missing.munis <- setdiff(leftover_munis_map,emp_df$Municipal)
      missing_df <-data.frame(Municipal=missing.munis, Year=input$map_year, Map_var=NA,
                              color=length(pctmap_colors), opacity = 0)
      colnames(missing_df)[3]<-col
      # combine data subset with missing counties data
      map_dat <- rbind.data.frame(map_dat, missing_df)
      map_dat$color <- pctmap_colors[map_dat$color]
      return(map_dat)
         }
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
    if(input$map_display_radio == "Actual Values"){
    muni_value <- prettyNum(values$selectedFeature[col_name], big.mark = ",")
    }
    if(input$map_display_radio == "Change_Pct"){
      muni_value <- prettyNum(values$selectedFeature[col_name],digits=4)
    }
    var_select <- gsub("Pct", "", col_name)
    var_select <- gsub("_", " ", var_select)
    var_select <- gsub("Employer", "Business", var_select)

    ## If clicked county has no crude rate, display a message
    if(muni_value == "NULL"|| muni_value =="NA"){
      return(as.character(tags$div(
        tags$h4(var_select, muni_name, "is not available for this timespan"))))
    }
    ## For a single year when county is clicked, display a message

    if(input$map_display_radio == "Actual Values"){
      if(input$map_radio =='Wages'){
    return(as.character(tags$div(
      tags$h4("Average Weekly Wage (2012 dollars)", "in", muni_name, " for ", input$map_year),
      tags$h5("$",muni_value)
    )))
    }
    else {
      return(as.character(tags$div(
        tags$h4(var_select, "in", muni_name, " for ", input$map_year),
        tags$h5(muni_value)
      )))
    }
    }
    if(input$map_display_radio == "Change_Pct"){
      
      return(as.character(tags$div(
        tags$h4(var_select, "in", muni_name, " for ", input$map_year,"compared to year 2003"),
        tags$h5(muni_value,"%")
      )))
    }

  })
  
  ## map legend
  output$legend2 <- renderPlot({
    if(input$map_radio=="Establishments" && input$map_display_radio == "Actual Values"){
      
      paint.brush = colorRampPalette(colors=c("white", "violetred"))
      cols <- paint.brush(25)
      leg_dat <- data_frame(y = seq(estmin.val, estmax.val,length.out=(length(map_colors)-1)), x = 1, col = cols)
      
      p <- ggplot(data = leg_dat) +
        geom_tile(aes(y = y, fill = reorder(col,y), x = x), show.legend = FALSE) +
        scale_y_continuous(limits = c(estmin.val, estmax.val), breaks = seq(estmin.val, estmax.val, length.out = 5)) +
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
  
  output$legend1 <- renderPlot({  
    if(input$map_radio=="Employment" && input$map_display_radio == "Actual Values"){
      paint.brush = colorRampPalette(colors=c("white", "violetred"))
      cols <- paint.brush(length(map_colors)-1)
      leg_dat<- data_frame(y = seq(empmin.val, empmax.val,length.out=(length(map_colors)-1)), x = 1, col = cols)
      
      q<- ggplot(data = leg_dat) +
        geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
        scale_y_continuous(limits = c(empmin.val, empmax.val), breaks = seq(empmin.val, empmax.val, length.out = 5)) +
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

output$legend3 <- renderPlot({  
  if(input$map_radio=="Wages" && input$map_display_radio == "Actual Values"){
    paint.brush = colorRampPalette(colors=c("white", "violetred"))
    cols <- paint.brush(length(map_colors)-1)
    leg_dat<- data_frame(y = seq(wagemin.val, wagemax.val,length.out=(length(map_colors)-1)), x = 1, col = cols)
    
    q<- ggplot(data = leg_dat) +
      geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
      scale_y_continuous(limits = c(wagemin.val, wagemax.val), breaks = seq(wagemin.val, wagemax.val, length.out = 5)) +
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
output$legend4<- renderPlot({
  if(input$map_display_radio == "Change_Pct"){
    
    paint.brush = colorRampPalette(colors=c(cbbPalette[5], "white",cbbPalette[8]))
    cols <- paint.brush(25)
    leg_dat <- data_frame(y = seq(pctmin.val2, pctmax.val,length.out=25), x = 1, col = cols)
    
    b <- ggplot(data = leg_dat) +
      geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
      scale_y_continuous(limits = c(pctmin.val, pctmax.val), breaks = unique(c(seq(pctmin.val, 0,length.out = 3),seq(0,pctmax.val, length.out = 3)))) +
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
