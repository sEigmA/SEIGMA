#######################################
## Title: Marriage global.R          ##
## Author(s): Emily Ramos, Arvind    ##
##            Ramakrishnan, Jenna    ##
##            Kiridly, Steve Lauer   ## 
## Date Created:  10/22/2014         ##
## Date Modified: 10/22/2014         ##
#######################################

##First file run - Environment Setup

require(RJSONIO)
require(dplyr)

## load map data
MA_map_county <- fromJSON("County_2010Census_DP1.geojson")
MA_map_muni <- fromJSON("Muni_2010Census_DP1.geojson")


## Find order of counties in geojson files
## Each county is a separate feature
MA_counties <- c()
for(i in 1:length(MA_map_county$features)){
  MA_counties <- c(MA_counties, MA_map_county$features[[i]]$properties$County)
}

## Find order of municipals in geojson files
## Each municipal is a separate feature
for(i in 1:length(MA_map_muni$features)){
  MA_map_muni$features[[i]]$properties$NAMELSAD10 <- substr(MA_map_muni$features[[i]]$properties$NAMELSAD10, 1, nchar(MA_map_muni$features[[i]]$properties$NAMELSAD10)-5)
}

MA_municipals <- c()
for(i in 1:length(MA_map_muni$features)){
  MA_municipals <- c(MA_municipals, MA_map_muni$features[[i]]$properties$NAMELSAD10)
}


## Load formatted suicide data
## -1 eliminates first column [rows,columns]
mar_data <- read.csv(file="marriagedata.csv")[,-1]

## Set graph colors (special for colorblind people)
## In order: black, orange, light blue, green, yellow, dark blue, red, pink
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                "#0072B2", "#D55E00", "#CC79A7")

## Create maxs and mins for googleCharts/Plot tab
ylim <- list(
  min = 0,
  max = 100
)

## Colors for a single-year legend
paint_brush <- colorRampPalette(colors=c("white", cbbPalette[7]))
map_colors <- c(paint_brush(n=6), "#999999")

## For a single year data, we have a series of percentages (split into quintiles).  Cuts are quintiles of the total data percentages
## Cuts based on entire dataset - not year specific - This keeps colors consistent for maps year-to-year

max_val <- 100
min_val <- 0

## Puts each county year in between the cuts (n colors, n+1 cuts)
## length.out will make that many cuts
cuts <- seq(min_val, max_val, length.out = length(map_colors))

## Construct break ranges for displaying in the legend
## Creates a data frame
## head = scuts takes everything except for the last one, 
## tails = same thing opposite

colorRanges <- data.frame(
  from = head(cuts, length(cuts)-1),
  to = tail(cuts, length(cuts)-1)
)