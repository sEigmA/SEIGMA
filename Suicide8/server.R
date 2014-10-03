# libraries you need
#require(ggplot2)
require(dplyr)
#require(sp)
#require(maptools)
#require(rgeos)
require(Hmisc)
#require(GISTools)
#require(ggvis)
#require(reshape2)
#require(lattice)
#require(rMaps)
#require(foreign)
#require(googleVis)
require(leaflet)
#require(rgdal)
require(RJSONIO)

#MAmap <- readOGR(dsn="County_2010Census_DP1.geojson", layer="OGRGeoJSON")

#cuts <- seq(0, 1503085, l=5)

## what I'm working on now
#   MAmap@data$color <- as.integer(cut2(MAmap@data$Population,cuts=cuts))
#   for(i in 1:length(MAmap@polygons)){
#     MAmap@polygons[[i]]@Polygons[[1]] <- c(MAmap@polygons[[i]]@Polygons[[1]], 
#                                            style = list(fillColor = cbbPalette[MAmap@data$color[i]], weight=2, color="#000000"))
#   }


suidata <- read.csv(file="SASuicidedata.csv")[,-1]
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                "#0072B2", "#D55E00", "#CC79A7")
suidata.cuts <- quantile(suidata$Crude.Rate, seq(0,1,1/6), na.rm=T)

shinyServer(function(input, output, session) {
 suidf <- reactive({
  suidf <- suidata
  return(suidf)    
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
 
 # ----- Create the cuts
 # cuts<-round(quantile(MAmap@data$Population, probs = seq(0, 1, 0.20), na.rm = FALSE), 0)
 # cuts[1]<-0 # ----- for this example make first cut zero
 
 map_dat <- reactive({
#   browser()
  ## make reactive dataframe into regular dataframe
  suidf <- suidf()
  
  ## subset US data into own dataframe
  US <- suidf[which(suidf$County=="US"),]
  
  ## subset MA data into own dataframe
  MA <- suidf[which(suidf$County=="MA"),]
  
  ## take US and MA out of suidf
  suidf <- subset(suidf, County!="MA")
  suidf <- subset(suidf, County!="US")
  
  ## for single year maps...
  if(input$timespan == "sing.yr"){
   
   ## set the color to ramp from white to one of the colorblind colors and grey representing NA
   paint.brush <- colorRampPalette(colors=c("white", "red"))
   map.colors <- c(paint.brush(n=6), "#999999")
   
   ## find  max and min values of the variable in the total data and make cuts based on those values
   max.val <- max(suidata$Crude.Rate, na.rm=TRUE)
   min.val <- min(suidata$Crude.Rate, na.rm=TRUE)
   cuts <- seq(min.val, max.val, (max.val-min.val)/(length(map.colors)-1))
   
   ## subset the data by the year selected
   suidf <- filter(suidf, Year==input$year)
  
  ## assign colors to each entry in the data frame
  color <- as.integer(cut2(suidf$Crude.Rate,cuts=cuts))
  suidf <- cbind.data.frame(suidf,color)
  suidf$color <- ifelse(is.na(suidf$color), length(map.colors), 
                        suidf$color)
  suidf$County <- paste(as.character(suidf$County), "County")
  
  ## find missing counties in data subset and assign NAs to all values
  missing.counties <- setdiff(MAcounties, suidf$County)
  df <- data.frame(County=missing.counties, State="MA", Country="US", 
                   Year=input$year, Suicides=NA, Population=NA, 
                   Crude.Rate=NA, Crude.Rate.Lower.Bound=NA,
                   Crude.Rate.Upper.Bound=NA, 
                   Crude.Rate.Standard.Error=NA,
                   color=length(map.colors))
  
  ## combine data subset with missing counties data
  suidf <- rbind.data.frame(suidf, df)
  return(suidf)
  }
  
  if(input$timespan=="mult.yrs"){
   ## colors fade from one color to white to another color, with gray for NAs
   paint.brush <- colorRampPalette(colors=c(cbbPalette[6], "white", cbbPalette[7]))
   map.colors <- c(paint.brush(n=8), "#999999")
   
   ## find max and min values for each county
   bound <- suidf %>%
    group_by(County) %>%
    summarise(max.val = max(Crude.Rate, na.rm=FALSE),
              min.val = min(Crude.Rate, na.rm=FALSE))
   
   ## find the difference between each county's max and min
   bound$diff <- abs(bound$max.val - bound$min.val)
   
   ## set the max and min value (for the legend) at 95% of the largest difference
   max.val <- quantile(bound$diff, .95, na.rm=TRUE)
   min.val <- -1*max.val
   
   ## create dataframes for the max and min year of selected data
   min.year <- min(input$range)
   max.year <- max(input$range)
   min.df <- subset(suidf, Year==min.year)
   max.df <- subset(suidf, Year==max.year)
   
   ## merge data and take difference between the data of the min year and the max year
   diff.df <- within(merge(min.df, max.df, by="County"),{
    Crude.Rate <- round(Crude.Rate.y - Crude.Rate.x, 3)
   })[,c("County", "Crude.Rate")]
   
   diff.df$County <- paste(as.character(diff.df$County), "County")
   
   ## assign colors to each entry in the data frame
   cuts <- seq(min.val, max.val, (max.val-min.val)/(length(map.colors)-1))
   color <- as.integer(cut2(diff.df[,2],cuts=cuts))
   diff.df <- cbind.data.frame(diff.df,color)
   diff.df$color <- ifelse(is.na(diff.df$color), length(map.colors), diff.df$color)
   
   ## find missing counties in data subset and assign NAs to all values
   missing.counties <- setdiff(MAcounties, diff.df$County)
   df <- data.frame(County=missing.counties, Crude.Rate=NA,
                    color=length(map.colors))
   
   ## combine data subset with missing counties data
   diff.df <- rbind.data.frame(diff.df, df)
   return(diff.df)
  }
 })
 
 ## draw leaflet map
 map <- createLeafletMap(session, "map")
 
 ## the functions within observe are called when any of the inputs are called
 observe({
#   browser()
  
  ## when year is changed
  is.null(input$year)
  ## when a range of years are selected
  is.null(input$range)
  
  input$action
  
  ## load in relevant map data
  suidf <- map_dat()
  
  ## assign map to x
  x <- MAmap
  ## for each county in the map, attach the Crude Rate and colors associated
  for(i in 1:length(x$features)){
   x$features[[i]]$properties$Crude.Rate <- suidf$Crude.Rate[match(x$features[[i]]$properties$County, suidf$County)]
   x$features[[i]]$properties$style <- list(fillColor = map.colors[as.integer(cut2(x$features[[i]]$properties$Crude.Rate, cuts=cuts))], weight=1, color="#000000", fillOpacity=0.5)
  }
  #     session$onFlushed(once=FALSE, function() {
  map$addGeoJSON(x)
  # }
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
   tags$h3(values$selectedFeature$County),
   tags$div(
    "Crude Rate:",
    values$selectedFeature$Crude.Rate)
  ))
 })
 # output$map <- renderPrint({
 #   downloaddir <- getwd()
 #   leafdat<-"County_2010Census_DP1.geojson"
 #   load("county_subdat.RData")
 #   
 #   # ----- Create the cuts
 #   cuts<-round(quantile(subdat$Population, probs = seq(0, 1, 0.20), na.rm = FALSE), 0)
 #   cuts[1]<-0 # ----- for this example make first cut zero
 #   
 #   # ----- Fields to include in the popup
 #   popup<-c("County", "Population")
 #   
 #   # ----- Gradulated style based on an attribute
 #   sty<-styleGrad(prop="Population", breaks=cuts, right=FALSE, style.par="col",
 #                  style.val=rev(heat.colors(6)), leg="Population (2010)", lwd=1)
 #   
 #   # ----- Create the map and load into browser
 #   leaflet(data=leafdat, dest=downloaddir, style=sty,
 #                  title="counties", base.map="osm",
 #                  incl.data=TRUE,  popup=popup)
 # 
 # })
 
 
})