#test googlevis

#
library(RCurl)
url <- "https://raw.githubusercontent.com/mages/diesunddas/master/Data/US%20Presidential%20Elections.csv"
dat <- getURL(url, ssl.verifypeer=0L, followlocation=1L)
dat <- read.csv(text=dat)
## Add min and max values to t


datminmax = data.frame(state=rep(c("Min", "Max"),21), 
                       demVote=rep(c(0, 100),21),
                       year=sort(rep(seq(1932,2012,4),2)))
dat <- rbind(dat[,1:3], datminmax)

shinyServer(function(input, output) {
  myYear <- reactive({
    input$Year
  })
  output$year <- renderText({
    paste("Democratic share of the presidential vote in", myYear())
  })
  output$gvis <- renderGvis({
    myData <- subset(dat, 
                     (year > (myYear()-1)) & (year < (myYear()+1)))
    gvisGeoChart(myData,
                 locationvar="state", colorvar="demVote",
                 options=list(region="US", displayMode="regions", 
                              resolution="provinces",
                              width=500, height=400,
                              colorAxis="{colors:['#FFFFFF', '#0000FF']}"
                 ))     
  })
})




G5 <- gvisGeoChart(Andrew, "LatLong", colorvar='Speed_kt',
                   options=list(region="US"))
plot(G5)


G6 <- gvisGeoChart(Andrew, "LatLong", sizevar='Speed_kt',
                   colorvar="Pressure_mb", options=list(region="US"))
