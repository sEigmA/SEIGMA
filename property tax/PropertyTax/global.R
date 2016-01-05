#######################################
## Title: Bankruptices global.R      ##
## Author(s): Xuelian Li, Jenna      ##
##            Kiridly                ## 
## Date Created:  08/11/15           ##
## Date Modified: 08/15/15 XL        ##
#######################################

##First file run - Environment Setup
## load necessary libraries
require(dplyr)
require(maptools)
require(Hmisc)
require(shiny)
require(googleCharts)
require(leaflet)
require(RJSONIO)
require(tidyr)

## load map data
MA_map_muni <- fromJSON("Muni_2010Census_DP1.geojson")

## Load formatted tax data
tax_data <- read.csv(file="taxdata.csv")
colnames(tax_data)[2]<-"Year"

## Find order of municipals in geojson files
## Each municipal is a separate feature
for(i in 1:length(MA_map_muni$features)){
  MA_map_muni$features[[i]]$properties$NAMELSAD10 <- substr(MA_map_muni$features[[i]]$properties$NAMELSAD10, 1, nchar(MA_map_muni$features[[i]]$properties$NAMELSAD10)-5)
}

MA_municipals_map <- c()
for(i in 1:length(MA_map_muni$features)){
  MA_municipals_map <- c(MA_municipals_map, MA_map_muni$features[[i]]$properties$NAMELSAD10)
}

idx_leftovers <- which(!MA_municipals_map %in% tax_data$Municipal)
leftover_munis <- MA_municipals_map[idx_leftovers]
for(i in 1:length(leftover_munis)){
  MA_map_muni$features[[idx_leftovers[i]]]$properties$NAMELSAD10 <-
    substr(leftover_munis[i], 1, nchar(leftover_munis[i])-5)
}

MA_municipals <- c()
for(i in 1:length(MA_map_muni$features)){
  MA_municipals <- c(MA_municipals, MA_map_muni$features[[i]]$properties$NAMELSAD10)
}
idx_leftovers2 <- which(!MA_municipals %in% tax_data$Municipal)
leftover_munis_map <- MA_municipals[idx_leftovers2]
MA_municipals <- sort(MA_municipals[-idx_leftovers2])

## Set graph colors (special for colorblind people)
## In order: black, orange, light blue, green, yellow, dark blue, red, pink
cbbPalette <- c("black", "red", "orange","green",
                "blue", "maroon", "deeppink", "yellow")

## Create maxs and mins for tax total levy plot in googleCharts/Plot tab
xlim <- list(
  min = min(tax_data$Year)-1,
  max = max(tax_data$Year)+1
)
ylim <- list(
  min = 0,
  ##+5 = max rate plus a little extra
  max = max(tax_data$Inflation_Adjusted_Total_Levy, na.rm=T)+5
)

## create ylim for the Percent of Levy by Class 
ylim_pct<-list(
  min = 0,
  
  max = 100
)

#################################################################

