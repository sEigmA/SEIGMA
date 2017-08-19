#######################################
## Title: Unified      global.R      ##
## Author(s): Justin Baldwin         ## 
## Date Created:  07/11/2017         ##
#######################################

require(dplyr)
require(maptools)
require(Hmisc)
require(shiny)
require(leaflet)
require(geojsonio)
require(lubridate)
require(markdown)
require(RJSONIO)
require(ggrepel)
require(ggplot2)

MA_map_county <- fromJSON("County_2010Census_DP1.geojson")
MA_map_muni <- geojson_read("Muni_2010Census_DP1.geojson", what = "sp")
MA_municipals <- unique(MA_map_muni$NAMELSAD10)

MA_municipals <- as.character(MA_municipals)
MA_municipals <- gsub(MA_municipals, pattern = " [Tt]own| city", replacement = "")
MA_municipals <- sort(MA_municipals[-grep(MA_municipals, pattern = "County subdivisions not defined")])


MA_map_muni$NAMELSAD10 <- as.character(MA_map_muni$NAMELSAD10)
MA_map_muni$NAMELSAD10 <- gsub(MA_map_muni$NAMELSAD10, pattern = " [Tt]own| city|County subdivisions not defined", 
                               replacement = "")


MA_municipals_map <- c()
for(i in 1:length(MA_map_muni$features)){
  MA_municipals_map <- c(MA_municipals_map, MA_map_muni$features[[i]]$properties$NAMELSAD10)
}


## Load formatted marital status data
unified <- read.csv(file="unified.csv")

#some unavoidable formatting of column names, can't handle hyphens
colnames(unified)[12:37] <- c("Age_under_5_Pct","Margin_Error_under_5_Pct","Age_5-9_Pct", "Margin_Error_5-9_Pct",
                               "Age_10-14_Pct","Margin_Error_10-14_Pct","Age_15-19_Pct", "Margin_Error_15-19_Pct",
                               "Age_20-24_Pct","Margin_Error_20-24_Pct","Age_25-34_Pct", "Margin_Error_25-34_Pct",
                               "Age_35-44_Pct", "Margin_Error_35-44_Pct","Age_45-54_Pct", "Margin_Error_45-54_Pct",
                               "Age_55-59_Pct", "Margin_Error_55-59_Pct","Age_60-64_Pct", "Margin_Error_60-64_Pct",
                               "Age_65-74_Pct", "Margin_Error_65-74_Pct","Age_75-84_Pct", "Margin_Error_75-84_Pct",
                               "Age_85+Pct", "Margin_Error_85+_Pct")
colnames(unified)[54:59] <- c("Age_under_20_Pct_plot","Age_20-34_Pct_plot","Age_35-54_Pct_plot", "Age_55-64_Pct_plot",
                               "Age_65-74_Pct_plot", "Age_75+Pct_plot")

topics <- read.csv("topics.csv")
