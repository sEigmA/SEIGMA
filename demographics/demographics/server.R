#######################################
## Title: Demographics server.R      ##
## Author(s): Xuelian Li, Arvind     ##
##            Ramakrishnan, Jenna    ##
##            Kiridly, Emily Ramos   ##
## Date Created:  02/28/2014         ##
## Date Modified: 03/01/2015  ER     ##
#######################################

shinyServer(function(input, output, session) {
  ## Dem_df is a reactive dataframe. Necessary for when summary/plot/map have common input (Multiple Variables). Not in this project
  
  Dem_df <- reactive({
       Dem_df <- Dem_data       
    ## Output reactive dataframe
    Dem_df
  })
  
  
  ## Create summary table
  output$summary <- renderDataTable({
    ## Make reactive dataframe into regular dataframe
    Dem_df <- Dem_df()
         
    ## make municipals a vector based on input variable
    if(!is.null(input$sum_muni)){
      munis <- input$sum_muni
    }
    ## if none selected, put all municipals in vector
    else {
      munis <- MA_municipals
    }
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
    ##select columns according input$radio
    sel_col_num<-c()
    if (input$sum_radio=="Age") {
      sel_col_num<-c(12:37)
    } else if (input$sum_radio=="Gender") {
      sel_col_num<-c(8:11)
    } else if (input$sum_radio=="Race") {
      sel_col_num<-c(38:49)
    } else {sel_col_num<-c(50:53)}
    
    ## create a dataframe consisting only of counties in vector
    Dem_df <- Dem_df %>%
      filter(Region %in% munis,Five_Year_Range == input$sum_year) %>%
      select(4:7, sel_col_num)
    
    colnames(Dem_df) <- gsub("_", " ", colnames(Dem_df))
    colnames(Dem_df) <- gsub("Pct", "Percentage", colnames(Dem_df))
    
    return(Dem_df)
  }, options=list(searching = FALSE, orderClasses = TRUE)) # there are a bunch of options to edit the appearance of datatables, this removes one of the ugly features
  ## create the plot of the data
  ## for the Google charts plot

  munis_df <- reactive({
    #     browser()
    ## make reactive dataframe into regular dataframe
    Dem_df <- Dem_df()%>%
    filter(Five_Year_Range == input$plot_year) 
    ## find the county of the municipal
    county <- as.character(Dem_df$County[match(input$plot_muni, Dem_df$Municipal)])
    
    ## make counties a vector based on input variable
    munis <- c(input$plot_muni, county, "MA", "United States")
    
    muni_index <- c()
    for(i in 1:length(munis)){
      muni_index[i] <- match(munis[i], Dem_df$Region)
    }
    
    Dem_df$Region <- factor(Dem_df$Region, levels = c(munis, as.character(Dem_df$Region)[-muni_index]))
    
    sel_col_num1<-c()
    if (input$plot_radio=="Age") {
      sel_col_num1<-c(54, 55, 56, 57, 58, 59)
    } else if (input$plot_radio=="Gender") {
      sel_col_num1<-c(8,10)
    } else if (input$plot_radio=="Race") {
      sel_col_num1<-c(38,40,42,44,46,48)
    } else {sel_col_num1<-c(50,52)}
    
    ## create a dataframe consisting only of counties in vector
    munis_df <- Dem_df %>%
      filter(Region %in% munis) %>%
      arrange(Region)%>%
      select(4, sel_col_num1)
    colnames(munis_df) <- gsub("_", " ", colnames(munis_df))
    colnames(munis_df) <- gsub("Pct", "", colnames(munis_df))
    colnames(munis_df) <- gsub("plot", "", colnames(munis_df))
    return(munis_df)
})
    output$Plot_age<-reactive({list(
      data = googleDataTable(munis_df()), options = list(title=paste(input$plot_radio, "as a Percentage of the Population by Region ", input$plot_muni,
                                                                   "over selected five years ", input$plot_year)))
    
  })
output$Plot_gender<-reactive({list(
  data = googleDataTable(munis_df()), options = list(title=paste(input$plot_radio, "as a Percentage of the Population by Region ", input$plot_muni,
                                                                 "over selected five years ", input$plot_year)))
  
})

  ## set map colors
  map_dat <- reactive({
    ## make reactive dataframe into regular dataframe
    Dem_dat <- Dem_df()%>%
      filter(Five_Year_Range == input$map_year)%>%
      ## take US, MA, and counties out of map_dat
      filter(!is.na(Municipal) )
          
    ## get column name and cuts based on input
    if (input$map_radio == "Age") {
      col<-input$var_age
      cuts<-agecuts
    }
    else if (input$map_radio == "Gender"){
      col<-input$var_gen
      cuts<-gencuts
    }
    else if (input$map_radio == "Race"){
      col<-input$var_rac
      cuts<-racecuts
    }
    else {
      col<-input$var_eth
      cuts<-racecuts 
    }
    
    ## assign colors to each entry in the data frame
    col_sel_num1<-which( colnames(Dem_dat)==col )
    map_dat <- select(Dem_dat,c(1:6,col_sel_num1))
    color <- as.integer(cut2(map_dat[,col],cuts=cuts))
    map_dat <- cbind.data.frame(map_dat, color)
    map_dat$color <- ifelse(is.na(map_dat$color), length(map_colors),
                            map_dat$color)
    map_dat$opacity <- 0.7
    
    ## find missing counties in data subset and assign NAs to all values
    missing_munis <- setdiff(leftover_munis_map, map_dat$Region)
    missing_df <- data.frame(Municipal = NA, County = NA, State = "MA",
                             Region = missing_munis, Five_Year_Range = input$map_year,
                             Total_Population = NA,
                             Map_var = NA, color=length(map_colors), opacity = 0)
    colnames(missing_df)[7]<-col
    # combine data subset with missing counties data
    map_dat <- rbind.data.frame(map_dat, missing_df)
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
    col_name<-colnames(map_dat)[7]
    ## All functions which are isolated, will not run until the above observe function is activated
    isolate({
      ## Duplicate MAmap to x
      x <- MA_map_muni
      
      ## for each county in the map, attach the Crude Rate and colors associated
      for(i in 1:length(x$features)){
        ## Each feature is a county
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
    col_name<-colnames(map_dat)[7]
    isolate({
      values$selectedFeature <- evt$properties
      region <- evt$properties$NAMELSAD10
      values$selectedFeature[col_name] <- map_dat[match(region, map_dat$Region), col_name]
    })
  })
  ##  This function is what creates info box
  output$details <- renderText({
    if (input$map_radio == "Age") {
      col<-input$var_age
     }
    else if (input$map_radio == "Gender"){
      col<-input$var_gen
     }
    else if (input$map_radio == "Race"){
      col<-input$var_rac
    }
    else{
      col<-input$var_eth 
    }
    ## Before a county is clicked, display a message
    if(is.null(values$selectedFeature)){
      return(as.character(tags$div(
        tags$div(
          h4("Click on a town or city"))
      )))
    }
    
    muni_name <- values$selectedFeature$NAMELSAD10
    muni_value <- values$selectedFeature[col]
    var_select <- gsub("Pct", "", col)
    var_select <- gsub("_", " ", var_select)
    var_select <- gsub("plot", "", var_select)
    
    ## If clicked county has no crude rate, display a message
    if(is.null(values$selectedFeature[col])){
      return(as.character(tags$div(
        tags$h5("Percentage of", var_select, "in ", muni_name, "is not available for this timespan"))))
    }
    ## For a single year when county is clicked, display a message
    as.character(tags$div(
      tags$h4("Percentage of", var_select, "in ", muni_name, " for ", input$map_year),
      tags$h5(muni_value, "%")
    ))
  })
  ##########################################################
  output$legend1 <- renderPlot({  
    paint.brush = colorRampPalette(colors=c("lightgreen","yellow", "red"))
    cols <- paint.brush(length(map_colors)-1)
    if(input$map_radio =='Age'){
      leg_dat<- data_frame(y = seq(agemin.val, agemax.val,length.out=(length(map_colors)-1)), x = 1, col = cols)
      
      q<- ggplot(data = leg_dat) +
        geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
        scale_y_continuous(limits = c(agemin.val, agemax.val), breaks = round(seq(agemin.val, agemax.val, length.out = 5),0)) +
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
    else if(input$map_radio =='Gender'){
      leg_dat<- data_frame(y = seq(genmin.val, genmax.val,length.out=(length(map_colors)-1)), x = 1, col = cols)
      
      q<- ggplot(data = leg_dat) +
        geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
        scale_y_continuous(limits = c(genmin.val, genmax.val), breaks = round(seq(genmin.val, genmax.val, length.out = 5),1)) +
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
    else {
      leg_dat<- data_frame(y = seq(racemin.val, racemax.val,length.out=(length(map_colors)-1)), x = 1, col = cols)
      
      q<- ggplot(data = leg_dat) +
        geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
        scale_y_continuous(limits = c(racemin.val, racemax.val), breaks = round(seq(racemin.val, racemax.val, length.out = 5),1)) +
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
  output$text1<-renderText({
    return(as.character(
      input$map_radio
    ))
  })
})

