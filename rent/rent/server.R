#######################################
## Title: RENT   server.R            ##
## Author(s): JWB, BF                ## 
## Date Created:  12/01/2016         ##
#######################################

shinyServer(function(input, output, session) {
  ## rent_df is a reactive dataframe. Necessary for when summary/plot/map have common input (Multiple Variables). Not in this project
  rent_df <- reactive({
    years <- input$sum_year
    if(is.null(years)==TRUE){years <- paste(c(2005:2011), c(2005:2011)+4, sep="-")}
    
    ## Filter the data by the chosen Five Year Range 
    rent_df <- rent %>%
      filter(Five.Year.Range %in% years) %>%
      select(c(1:5))

    ## Output reactive dataframe
    rent_df    
  })
  
  ## Create summary table
  output$summary <- renderDataTable({
    ## Make reactive dataframe into regular dataframe
    
    rent_df <- rent_df()
    
    ## make municipals a vector based on input variable
    if(!is.null(input$sum_muni))
      munis <- MA_municipals
    ## if none selected, put all municipals in vector
    if(is.null(input$sum_muni))
      munis <- MA_municipals
    
    ## if the user checks the meanUS box or the meanMA box, add those to counties vector
    if(input$US_mean){
      if(input$MA_mean){
        munis <- c("United States", "Massachusetts", munis) ## US and MA  
      } else{
        munis <- c("United States", munis) ## US only
      }
    } else{
      if(input$MA_mean){
        munis <- c("Massachusetts", munis) ## US only ## MA only
      }
    }
    #     browser( )
    ## create a dataframe consisting only of counties in vector
    rent_df <- rent_df %>%
      filter(Municipal %in% munis) %>%
      select(c(2,1,3,4,5))
    
    colnames(rent_df) <- gsub(".", " ", colnames(rent_df), fixed=T)
    colnames(rent_df)[4] <- "Median Rent (2015$)"
    colnames(rent_df)[5] <- "Median Margin of Error (2015$)"
    
    
#     rent_df[,3] <- prettyNum(rent_df[,3], big.mark=",")
#     rent_df[,4] <- prettyNum(rent_df[,4], big.mark=",")
#     
    return(rent_df)
  }, options = list(searching = FALSE, orderClasses = TRUE)) # there are a bunch of options to edit the appearance of datatables, this removes one of the ugly features
  

  ## create the plot of the data

   
  
    munis_p <- reactive({
      
      munis_p2 <- input$plot_muni
      #MA
      if(input$MA_mean_p==T && any(grepl(x=munis_p2, pattern = "Massachusetts"))==F){
        return(c("Massachusetts", munis_p2[!(munis_p2 =="Massachusetts")])) ## US only ## MA only
      }else if(input$MA_mean_p==T && any(grepl(x=munis_p2, pattern = "Massachusetts"))==T){
        return(c("Massachusetts", munis_p2[!(munis_p2 =="Massachusetts")])) ## US only ## MA only
      }
      else if(input$MA_mean_p==F && any(grepl(x=munis_p2, pattern = "Massachusetts"))==T){
        return(munis_p2[!(munis_p2 =="Massachusetts")]) ## remove MA
      } else if(input$MA_mean_p==F && any(grepl(x=munis_p2, pattern = "Massachusetts"))==F){
        return(munis_p2[!(munis_p2 =="Massachusetts")]) ## remove MA
      }
    
   
   })
  
    munis_pfinal <- reactive({
     munis_p3 <- munis_p()
     #AMERICA FWURST
     if(input$US_mean_p==T && any(grepl(x=munis_p3, pattern = "United States"))==F){
       return(c("United States", munis_p3[!(munis_p3 =="United States")])) ##  United States
     }else if(input$US_mean_p==T && any(grepl(x=munis_p3, pattern = "United States"))==T){
       return(c("United States", munis_p3[!(munis_p3 =="United States")])) ## US  United States
     }
     else if(input$US_mean_p==F && any(grepl(x=munis_p3, pattern = "United States"))==T){
       return(munis_p3[!(munis_p3 =="United States")]) ## remove United States
     } else if(input$US_mean_p==F && any(grepl(x=munis_p3, pattern = "United States"))==F){
       return(munis_p3[!(munis_p3 =="United States")]) ## remove  United States
     }
     
     
   })
  
    plot_rent_df <- reactive({
      rent_for_plot <- rent  
      rent_for_plot$Year <- as.numeric(sapply(strsplit(as.character(rent_for_plot$Five.Year.Range), split="-"), FUN=function(x){x[1]}))+2    
     
      selmun <- munis_pfinal()
      rent_for_plot <- rent_for_plot %>%
        filter(Municipal %in% selmun) %>%
        select(c(1,6,4,5)) 
      names(rent_for_plot)[c(3,4)] <- c("Var", "Error")
      
      ## Output reactive dataframe, sorted like selected munis
      #order=unlist(lapply(match(munis_p, plot_mar_df$Region), FUN=function(x){x+0:((nrow(plot_mar_df))/(length(munis_p))-1)}))
      return(rent_for_plot[order(match(rent_for_plot$Municipal, selmun)),])
      
    })
      
  ## for the Google charts plot
  output$plot <- renderPlot({
    ## make reactive dataframe into regular dataframe
    
    pdf <- plot_rent_df()
    row.names(pdf) <- 1:nrow(pdf)
    pdf$Municipal <- factor(pdf$Municipal, levels = unique(pdf$Municipal,ordered = TRUE))
    ap=0.5
    sz=1
    
    p=ggplot(pdf, aes(x=Year, y=Var, colour=Municipal)) +
      geom_errorbarh(aes(xmax = Year + 2, xmin = Year - 2, height = 0,colour=Municipal),alpha=ap/2, size=sz/2)+
      geom_errorbar(aes(ymin = Var-Error, ymax = Var+Error,colour=Municipal),alpha=ap,size=sz, width=0.125)+
      ylab("Median Rent (2015 $)")+
      scale_x_continuous(breaks=c(2006, 2008, 2010, 2012, 2014))+
      scale_color_manual(values=cbbPalette, guide="legend")+
      geom_point(aes(colour=Municipal),size=4,alpha=1)+
      geom_line(aes(colour=Municipal),size=2,alpha=1)+
      theme_bw() +
      theme(plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank() )+
      theme(panel.border= element_blank())+
      theme(axis.line.x = element_line(color="black", size = 0.5),
            axis.line.y = element_line(color="black", size = 0.5))+
      theme(axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14))+
      theme(legend.title=element_text(size=16),
            legend.text=element_text(size=14))

      #guides(colour = guide_legend(override.aes = list(colour = NA)))+
      #guides(colour = guide_legend(override.aes = list(colour = cbbPalette[1:length(unique(pdf$Municipal))])))
    p
    
    
  })
  
  ##### interactive stuff on the plot
  # output$event <- renderPrint({
  #   d <- event_data("plotly_hover")
  #   if (is.null(d)) "Hover on a point!" else d
  # })
  # 

  #####################################MAP CREATION##############

  map_rent_df <- reactive({
    ## Filter the data by the chosen Five Year Range
    map_rent_df <- rent %>%
      filter(Five.Year.Range == input$map_year) %>%
      select(1:4, Five.Year.Range, Median.Rent.2015.Dollar, Rent.Margin.of.Error.2015.Dollar)

    ## Output reactive dataframe
    map_rent_df
  })



  ## set map colors
  map_dat <- reactive({

    ## Browser command - Stops the app right when it's about to break
    ## make reactive dataframe into regular dataframe
    map_rent_df <- map_rent_df()

    ## take US, MA, and counties out of map_dat
    map_dat <- map_rent_df %>%
      filter(!is.na(Municipal))

    ## assign colors to each entry in the data frame
    color <- as.integer(cut2(map_dat[,"Median.Rent.2015.Dollar"],cuts=cuts))
    map_dat <- cbind.data.frame(map_dat, color)
    map_dat$color <- ifelse(is.na(map_dat$color), length(map_colors),
                            map_dat$color)
    map_dat$opacity <- 0.7

    ## find missing counties in data subset and assign NAs to all values
    missing_munis <- setdiff(leftover_munis_map, map_dat$Municipal)
    missing_df <- data.frame(Municipal = missing_munis, County = NA, Five.Year.Range = input$map_year,
                             Median.Rent.2015.Dollar = NA, Rent.Margin.of.Error.2015.Dollar = NA,
                             color=length(map_colors), opacity = 0)

    na_munis <- setdiff(MA_municipals_map, map_dat$Municipal)
    na_munis <- na_munis[na_munis!= "County subdivisions not defined"]
    # input$map_year <- "2005-2009" # Debug
    # County <- "Barnstable" # Debug
    # Median.Rent <- 200 # Debug
    # Rent.Margin.of.Error <- 2 # Debug

    # na_df <- data.frame(Municipal = na_munis, County = NA, Five.Year.Range = input$map_year,
    #                     Median.Rent = NA, Rent.Margin.of.Error = NA,
    #                     color=length(map_colors), opacity = 0.7)


    # combine data subset with missing counties data
    map_dat <- rbind.data.frame(map_dat, missing_df)
    map_dat$color <- map_colors[map_dat$color]
    return(map_dat)
  })

  values <- reactiveValues(selectedFeature=NULL, highlight=c())

  #############################################
  # observe({
  #   values$highlight <- input$map_shape_mouseover$id
  #   #browser()
  # })

  # # Dynamically render the box in the upper-right
  # output$countyInfo <- renderUI({
  #
  #   if (is.null(values$highlight)) {
  #     return(tags$div("Hover over a county"))
  #   } else {
  #     # Get a properly formatted state name
  #     countyName <- names(MAcounties)[values$highlight]
  #     return(tags$div(
  #       tags$strong(countyName),
  #       tags$div(density[countyName], HTML("people/m<sup>2</sup>"))
  #     ))
  #   }
  # })

  # lastHighlighted <- c()
  # # When values$highlight changes, unhighlight the old state (if any) and
  # # highlight the new state
  # observe({
  # #   if (length(lastHighlighted) > 0)
  # #     drawStates(getStateName(lastHighlighted), FALSE)
  #   lastHighlighted <<- values$highlight
  #
  #   if (is.null(values$highlight))
  #     return()
  #
  #   isolate({
  #     drawStates(getStateName(values$highlight), TRUE)
  #   })
  # })

  ###########################################

  # draw leaflet map
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
        x$features[[i]]$properties["Median.Rent"] <-
          map_dat[match(x$features[[i]]$properties$NAMELSAD10, map_dat$Municipal), "Median.Rent"]
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
    isolate({
      values$selectedFeature <- evt$properties
      clickmuni <- evt$properties$NAMELSAD10
      values$selectedFeature["Median.Rent.2015.Dollar"] <- map_dat[match(clickmuni, map_dat$Municipal), "Median.Rent.2015.Dollar"]
      values$selectedFeature["Rent.Margin.of.Error.2015.Dollar"] <- map_dat[match(clickmuni, map_dat$Municipal), "Rent.Margin.of.Error.2015.Dollar"]
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
    #     browser()
    muni_name <- values$selectedFeature$NAMELSAD10
    muni_value <- prettyNum(values$selectedFeature["Median.Rent.2015.Dollar"], big.mark = ",")
    muni_margin<- prettyNum(values$selectedFeature["Rent.Margin.of.Error.2015.Dollar"], big.mark = ",")

    ## If clicked county has no crude rate, display a message
    if(muni_value == "NA"){
      return(as.character(tags$div(
        tags$h5("Median Contract Rent in", muni_name, "is not available for this timespan"))))
    }
    ## For a single year when county is clicked, display a message
    as.character(tags$div(
      tags$h4("Median Contract Rent in ", muni_name, " for ", input$map_year),
      tags$h5("$",muni_value, "+-", muni_margin)
    ))
  })

  output$legend1 <- renderPlot({
    paint.brush = colorRampPalette(colors=c("white", "#009E73"))
    cols <- paint.brush(length(map_colors)-1)
    leg_dat<- data_frame(y = seq(min_val, max_val, length.out = (length(map_colors)-1)), x = 1, col = cols)

    q<- ggplot(data = leg_dat) +
      geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
      scale_y_continuous(limits = c(min_val, max_val), breaks = round(seq(min_val, max_val, length.out = 5),0)) +
      scale_fill_manual(values = leg_dat$col) + theme_bw() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.x = element_blank(),
            panel.border = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank())

    return(q)

  })

})