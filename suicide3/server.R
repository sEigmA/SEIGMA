# libraries you need
#require(ggplot2)
require(dplyr)
#require(sp)
require(maptools)
#require(rgeos)
require(Hmisc)
#require(GISTools)
#require(ggvis)
require(reshape2)
require(lattice)
#require(rMaps)
require(foreign)
#require(googleVis)
require(leaflet)
require(rgdal)

MAmap <- readOGR(dsn="County_2010Census_DP1.geojson", layer="OGRGeoJSON")
#MAmap <- readShapeSpatial("countymaps/ma_counties.json")
suidata <- read.csv(file="SASuicidedata.csv")[,-1]
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                "#0072B2", "#D55E00", "#CC79A7")
suidata.cuts <- quantile(suidata$Crude.Rate, seq(0,1,1/6), na.rm=T)

shinyServer(function(input, output, session) {
  data <- reactive({
    data <- suidata
    return(data)    
  })
  
  #   output$summary <- renderDataTable({
  #     data <- data()
  #     US <- data[which(data$County=="US"),]
  #     MA <- data[which(data$County=="MA"),]
  #     data <- subset(data, County!="US")
  #     data <- subset(data, County!="MA")
  #     counties <- input$county
  #     if(is.null(input$county)==FALSE){
  #       df <- c()
  #       for(i in 1:length(counties)){
  #         bbb <- subset(data, County==counties[i])
  #         df <- rbind.data.frame(df, bbb)
  #         #levels(data) <- c("US", "MA", counties)
  #       }
  #       data <- df
  #     }
  #     if(input$meanUS){
  #       if(input$meanMA){
  #         data <- rbind.data.frame(US, MA, data) ## US and MA
  #         
  #       } else{
  #         data <- rbind.data.frame(US, data)## US only
  #         levels(data) <- c("US", counties)
  #       }
  #     } else{
  #       if(input$meanMA){
  #         data <- rbind.data.frame(MA, data) ## MA only
  #         levels(data) <- c("MA", counties)
  #       } else{
  #         data <- data ## no US or MA
  #         levels(data) <- counties
  #       }
  #     }
  #     #levels(data$County) <- c("US", "MA", counties, setdiff(names(table(data$County)), c("US", "MA", counties)))
  #     
  #     if(input$timespan == "sing.yr"){
  #       df <- subset(data, Year==input$year)
  #     }
  #     
  #     if(input$timespan == "mult.yrs"){
  #       range <- seq(min(input$range), max(input$range), 1)
  #       df <- c()
  #       for(i in 1:length(range)){
  #         bbb <- subset(data, Year==range[i])
  #         df <- rbind.data.frame(df, bbb)
  #       }
  #     }
  #     colnames(df)[7:10] <- c("Crude Rate (per 100,000)", 
  #                             "Crude Rate Lower Bound", 
  #                             "Crude Rate Upper Bound", 
  #                             "Crude Rate Standard Error")
  #     #df$County <- factor(df$County, levels = counties)
  #     df
  #   }, options=list(bFilter=FALSE))
  #   
  #   output$plot <- reactive({
  #     data <- data()
  #     counties <- input$county
  #     
  #     df <- dcast(data, Year ~ County, value.var="Crude.Rate")
  #     
  #     if(is.null(input$county)){
  #       g <- df %.%
  #         select(Year, US)
  #     }
  #     
  #     if(!is.null(input$county)){
  #       if(input$meanMA)
  #         counties <- c(counties, "MA")
  #       if(input$meanUS)
  #         counties <- c(counties, "US")
  #       
  #       g <- df[,c("Year", counties)]
  #     }
  #     
  #     list(data=googleDataTable(g))
  #   })
  #   
  
  
  
  
  # ----- Create the cuts
  cuts<-round(quantile(MAmap@data$Population, probs = seq(0, 1, 0.20), na.rm = FALSE), 0)
  cuts[1]<-0 # ----- for this example make first cut zero
  
#   MAmap@data$color <- as.integer(cut2(MAmap@data$Population,cuts=cuts))
#   x <- MAmap@polygons[[i]]@Polygons[[1]]
#   
#   # Draw the given counties, with or without highlighting
#   drawCounties <- function(highlight = FALSE) {
#     map$addPolygon(I(MAmap@polygons[[1]]@Polygons[[1]]@coords[,1]), 
#                    I(MAmap@polygons[[1]]@Polygons[[1]]@coords[,2]), 
#                    I(MAmap@data$County),
#                    I(lapply(MAmap@data$County, function(x) {
#                      x <- x[[1]][1]
#                      list(fillColor = cbbPalette[MAmap@data$color[1]])
#                    })),
#                    I(list(fill=TRUE, fillOpacity=0.7, 
#                           stroke=TRUE, opacity=1, color="white", weight=ifelse(highlight, 4, 1)
#                    ))
#     )
#   }
#   
  

## what I'm working on now
  MAmap@data$color <- as.integer(cut2(MAmap@data$Population,cuts=cuts))
  for(i in 1:length(MAmap@polygons)){
    MAmap@polygons[[i]]@Polygons[[1]] <- c(MAmap@polygons[[i]]@Polygons[[1]], 
                                           style = list(fillColor = cbbPalette[MAmap@data$color[i]], weight=2, color="#000000"))
  }

## what works
  map <- createLeafletMap(session, "map")
  
  session$onFlushed(once=TRUE, function() {
    map$addGeoJSON(MAmap)
  })
  
  values <- reactiveValues(selectedFeature=NULL)
  
  observe({
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
    
    isolate({
      values$selectedFeature <- evt$properties
    })
  })
  
  output$details <- renderText({
    if(is.null(values$selectedFeature))
      return(NULL)
    
    as.character(tags$div(
      tags$h3(values$selectedFeature$GEOID10),
      tags$div(
        "Population:",
        values$selectedFeature$Population)
    ))
  })
  
  #   output$map = renderChart({
  #     paint.brush <- colorRampPalette(colors=c("white", cbbPalette[6]))
  #     map.colors <- c(paint.brush(n=6), "#999999")
  #     
  #     map <- createLeafletMap(session, "map")
  #     
  #     
  #     data <- data()
  #     df <- data %.%
  #       filter(Year==2011 & County!="US" & County !="MA") %.%
  #       select(County, Crude.Rate)
  #     missing.counties <- setdiff(MAmap$COUNTY, toupper(df$County))
  #     mc <- data.frame(County=tolower(missing.counties), Crude.Rate=NA)
  #     df <- rbind.data.frame(df,mc)
  #     df$County <- toupper(as.character(df$County))
  #     df$fillKey=cut(df$Crude.Rate, suidata.cuts, labels=LETTERS[1:6])
  #     df$fillKey <- ifelse(is.na(df$fillKey), LETTERS[7], as.character(df$fillKey))
  #     
  #   )}
  
  #   output$map <- reactive({
  #     data <- data()
  #     data$County <- toupper(as.character(data$County))
  #     df <- data.frame(County = rep(MAmap$COUNTY, 13), Year = rep(1999:2011, each=14))
  #     df2 <- merge(df, data, by=c("County", "Year"), all.x=TRUE)[,c("County", "Year", "Crude.Rate")]
  #     df2[is.na(df2)] <- 0
  #     
  #     #df <- data %.%
  #     #  filter(County!="US" & County !="MA") %.%
  #     #  select(County, Crude.Rate, Year)
  #     #missing.counties <- setdiff(MAmap$COUNTY, toupper(df$County))
  #     #mc <- data.frame(County=missing.counties, Crude.Rate=NA)
  #     #df <- rbind.data.frame(df,mc)
  #     #df$County <- paste(capitalize(tolower(as.character(df$County))), "County")
  #     #df$County <- toupper(as.character(df$County))
  #         #df$fillKey=cut(df$Crude.Rate, suidata.cuts, labels=LETTERS[1:6])
  #         #df$fillKey <- ifelse(is.na(df$fillKey), LETTERS[7], as.character(df$fillKey))
  #     paint.brush <- colorRampPalette(colors=c("white", cbbPalette[6]))
  #     map.colors <- c(paint.brush(n=6), "#999999")
  #     #max.val <- max(data$Crude.Rate, na.rm=TRUE)
  #     #min.val <- min(data$Crude.Rate, na.rm=TRUE)
  #     #cuts <- seq(min.val, max.val, (max.val-min.val)/(length(map.colors)-1))
  #     #color <- as.integer(cut2(df$Crude.Rate,cuts=cuts))
  #     #df <- cbind.data.frame(df,color)
  #     #df$color <- ifelse(is.na(df$color), length(map.colors), df$color)
  #     #df$Crude.Rate <- ifelse(is.na(df$Crude.Rate), 0, df$Crude.Rate)
  #     
  #     codes <- read.dbf(file = "~/Desktop/countymaps/states.dbf")[,c(1,3)]
  #     codes$COUNTY <- iconv(codes$COUNTY, "latin1", "utf-8")
  #     codes$OBJECTID <- as.numeric(codes$OBJECTID)
  #     names(codes) <- c("state_code", "name")
  #     df3 <- merge(codes, df2, by.x = "name", by.y = "County")
  #     #df2$Crude.Rate <- seq(1,length(df2[,1]))
  #     d1 <- ichoropleth(Crude.Rate ~ name, data = df4, ncuts=3, pal = "YlOrRd", animate="Year", map = 'states')
  #     
  #       d1$set(
  #         geographyConfig = list(
  #           dataUrl = "suicide/countymaps/mx_states.json"),
  #         scope = 'states',
  #         setProjection = '#! function( element, options ) {
  #      var projection, path;
  #      projection = d3.geo.mercator()
  #       .center([-72, 42]).scale(element.offsetWidth)
  #       .translate([element.offsetWidth / 2, element.offsetHeight / 2]);
  #   
  #      path = d3.geo.path().projection( projection );
  #      return {path: path, projection: projection};
  #     } !#')
  #   d1$save('rMaps.html', cdn=TRUE)
  #   d1
  #   d1$print('chart4', include_assets = TRUE)
  #   
  #   
  #   
  #   
  #   df3$fillKey=cut(df3$Crude.Rate, suidata.cuts, labels=1:6)
  #   df3$fillKey <- ifelse(is.na(df3$fillKey), 7, as.character(df3$fillKey))
  #   #fills = setNames(
  #   #  c(RColorBrewer::brewer.pal(6, 'YlOrRd'), 'white'),
  #   #  c(LETTERS[1:6], 'defaultFill')
  #   #)
  #   
  #   df4 <- filter(df3, Year==2005)
  #   options(rcharts.cdn = TRUE)
  #   map <- Datamaps$new()
  #   map$set(
  #     geographyConfig = list(
  #       dataUrl = "~/Desktop/countymaps/mx_states.json",
  #     popupTemplate =  "#! function(geography, data) { //this function should just return a string
  #           return '<div class=hoverinfo><strong>' + geography.properties.name + '</strong></div>';
  #     }  !#"),
  #     dom = 'chart_1',
  #     scope = 'states',
  #     bodyattrs = "ng-app ng-controller='rChartsCtrl'",
  #     setProjection = '#! function( element, options ) {
  #   
  #   var projection, path;
  #   
  #   projection = d3.geo.mercator()
  #   .center([-72, 42])
  #   .scale(element.offsetWidth)
  #   .translate([element.offsetWidth / 2, element.offsetHeight / 2]);
  #   
  #   path = d3.geo.path()
  #   .projection( projection );
  #   
  #   return {path: path, projection: projection};
  #   } !#',
  #     fills = map.colors,
  #     data = df4,
  #     legend=TRUE,
  #     labels = TRUE)
  #   map$save('rMaps.html', cdn=TRUE)
  #   map
  #   
  #   #list(data=googleDataTable(df), options = list(
  #   #  region = "US-MA",
  #   #  displayMode: 'regions'))
  #   
  #   #G <- gvisGeoChart(df, locationvar = "County", colorvar = "Crude.Rate", 
  #   #                  options=list(width = 600, height = 400, region = "US", province = "US-MA"))
  #   #plot(G)
  #   })
})