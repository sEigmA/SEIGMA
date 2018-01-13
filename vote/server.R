###############################
## Title: server.R           ##
## App: SEIGMA VOTE          ##
## Author: Zhenning Kang     ##
## Date Created:  01/12/2018 ##
## Last Modified: 01/13/2018 ##
###############################

library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)
votedata <- cleantable[sample.int(nrow(cleantable), 100),]
# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
votedata <- votedata[order(votedata$yes),]

function(input, output, session) {
  
  ## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -71.65, lat = 42.15, zoom = 9)
  })
  
  # A reactive expression that returns the set of zips that are
  # in bounds right now
  zipsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(votedata[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(votedata,
           lat >= latRng[1] & lat <= latRng[2] &
             lon >= lngRng[1] & lon <= lngRng[2])
  })
  
  # Precalculate the breaks we'll need for the two histograms
  centileBreaks <- hist(plot = FALSE, cleantable$yes, breaks = 20)$breaks
  
  output$histCentile <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)
    
    hist(zipsInBounds()$yes,
         breaks = centileBreaks,
         main = "Voted Yes in Visible Municipalities",
         xlab = "Percentile",
         xlim = range(cleantable$yes),
         col = '#00DD00',
         border = 'white')
  })
  
  output$scatterCollegeIncome <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)
    
    print(xyplot(income ~ bachelor, data = zipsInBounds(), xlim = range(cleantable$bachelor), ylim = range(cleantable$income)))
  })
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    colorBy <- input$color
    sizeBy <- input$size
    
    colorData <- votedata[[colorBy]]
    pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
    radius <- votedata[[sizeBy]] / max(votedata[[sizeBy]]) * 30000
    
    leafletProxy("map", data = votedata) %>%
      clearShapes() %>%
      addCircles(~lon, ~lat, radius=radius, layerId=~muni,
                 stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
      addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                layerId="colorLegend")
  })
  
  # # Show a popup at the given location
  # showMuniPopup <- function(muni, lat, lng) {
  #   selectedMuni <- cleantable[cleantable$muni == muni,]
  #   content <- as.character(tagList(
  #     tags$h4("yes Rate:", as.integer(selectedMuni$yes)),
  #     tags$strong(HTML(sprintf("%s, %s",
  #                              selectedMuni$muni, "MA"
  #     ))), tags$br(),
  #     sprintf("Median household income: %s", dollar(selectedMuni$income)), tags$br(),
  #     sprintf("Percent of adults with BA: %s%%", as.integer(selectedMuni$bachelor)), tags$br(),
  #     sprintf("Unemployment Rate: %s", selectedMuni$unemployment)
  #   ))
  #   leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
  # }
  # 
  # # When map is clicked, show a popup with city info
  # observe({
  #   leafletProxy("map") %>% clearPopups()
  #   event <- input$map_shape_click
  #   if (is.null(event))
  #     return()
  #   
  #   isolate({
  #     showMuniPopup(event$id, event$lat, event$lng)
  #   })
  # })
  # 
  
  ## Data Explorer ###########################################
  
  # observe({
  #   cities <- if (is.null(input$states)) character(0) else {
  #     filter(cleantable, State %in% input$states) %>%
  #       `$`('City') %>%
  #       unique() %>%
  #       sort()
  #   }
  #   stillSelected <- isolate(input$cities[input$cities %in% cities])
  #   updateSelectInput(session, "cities", choices = cities,
  #                     selected = stillSelected)
  # })
  # 
  # observe({
  #   zipcodes <- if (is.null(input$states)) character(0) else {
  #     cleantable %>%
  #       filter(State %in% input$states,
  #              is.null(input$cities) | City %in% input$cities) %>%
  #       `$`('Zipcode') %>%
  #       unique() %>%
  #       sort()
  #   }
  #   stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
  #   updateSelectInput(session, "zipcodes", choices = zipcodes,
  #                     selected = stillSelected)
  # })
  # 
  # observe({
  #   if (is.null(input$goto))
  #     return()
  #   isolate({
  #     map <- leafletProxy("map")
  #     map %>% clearPopups()
  #     dist <- 0.5
  #     zip <- input$goto$zip
  #     lat <- input$goto$lat
  #     lng <- input$goto$lng
  #     showZipcodePopup(zip, lat, lng)
  #     map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
  #   })
  # })
  
  output$votetable <- DT::renderDataTable({
    df <- cleantable %>%
      filter(
        yes >= input$minRate,
        yes <= input$maxRate,
        muni %in% input$muni
      ) %>%
      select(1:6)
    df$population <- format(df$population, big.mark=",", scientific=FALSE)
    df$bachelor <- sapply(df$bachelor,function(x) paste0(x, "%"))
    df$income <- format(df$income, big.mark=",", scientific=FALSE)
    df$income <- sapply(df$income,function(x) paste0("$", x))
    df$unemployment <- sapply(df$unemployment, function(x) paste0(x, "%"))
    df$yes <- round(df$yes, 1)
    df$yes <- sapply(df$yes,function(x) paste0(x, "%"))
    colnames(df) <- c("Municipal", "Total Population(2013)", "Percentage of Bachelors and Higher(2013)", "Median Income(2013)", "Unemplpyment Rate(2012)", "Percentage of Residents Voted Yes(2014)")
    
    action <- DT::dataTableAjax(session, df)
    
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
}