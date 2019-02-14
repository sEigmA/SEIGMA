#######################################
## Title: Suicide global.R           ##
## Author(s): Emily Ramos, Arvind    ##
##            Ramakrishnan, Jenna    ##
##            Kiridly, Steve Lauer,  ##
##            Justin Baldwin         ## 
## Date Created:                     ##
## Date Modified: 02/24/15 ER        ##
## Date Updted: 02/14/219 VE         ##
#######################################

##First file run - Environment Setup

require(RJSONIO)
require(dplyr)
require(leaflet)

## load map data
MAmap <- fromJSON("County_2010Census_DP1.geojson")


## Find order of counties in geojson files
## Each county is a separate feature
MAcounties <- c()
for(i in 1:length(MAmap$features)){
  MAcounties <- c(MAcounties, MAmap$features[[i]]$properties$County)
}

## Load formatted suicide data
## -1 eliminates first column [rows,columns]
suidata <- read.csv(file="SASuicidedata_Updated2018.csv")[,-1]

#If there is no age adjusted rate, get rid of the bounds and standard errors
suidata$Age.Adjusted.Rate.Lower.Bound[is.na(suidata$Age.Adjusted.Rate)] <- NA
suidata$Age.Adjusted.Rate.Upper.Bound[is.na(suidata$Age.Adjusted.Rate)] <- NA
suidata$Age.Adjusted.Rate.Standard.Error[is.na(suidata$Age.Adjusted.Rate)] <- NA

## Set graph colors (special for colorblind people)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                "#0072B2", "#D55E00", "#CC79A7")

## Create maxs and mins for googleCharts/Plot tab
xlim <- list(
  min = min(suidata$Year)-1,
  max = max(suidata$Year)+1
)
ylim <- list(
  min = 0,
  
  ##+5 = max crude rate plus a little extra
  max = max(suidata$Age.Adjusted.Rate, na.rm=T)+5
)


## Colors for a single-year legend
spaint.brush <- colorRampPalette(colors=c("white", "red3"))
smap.colors <- c(spaint.brush(n=25), "#999999")

## For a single year data, we have a series of crude rate (split into quintiles).  Cuts are quintiles of the total data
## Cuts based on entire dataset - not year specific - This keeps colors consistent for maps year-to-year

smax.val <- max(suidata$Age.Adjusted.Rate, na.rm=TRUE)
smin.val <- min(suidata$Age.Adjusted.Rate, na.rm=TRUE)

## Puts each county year in between the cuts (n colors, n+1 cuts)
## length.out will make that many cuts
# scuts <- seq(smin.val, smax.val, length.out = length(smap.colors))
scuts <- quantile(suidata$Age.Adjusted.Rate, probs = seq(0, 1, length.out = length(smap.colors)), na.rm=TRUE)

## Construct break ranges for displaying in the legend
## Creates a data frame
## head = scuts takes everything except for the last one, 
## tails = same thing opposite

#scolorRanges <- data.frame(
#  from = head(scuts, length(scuts)-1),
#  to = tail(scuts, length(scuts)-1)
#)

## colors fade from one color to white to another color, with gray for NAs
## m-prefix = multiple years
mpaint.brush <- colorRampPalette(colors=c(cbbPalette[6], "white", cbbPalette[7]))
mmap.colors <- c(mpaint.brush(n=25), "#999999")

## find max and min (crude suicide rates) values for each county
bound <- suidata %>%
  group_by(County) %>%
  
  ##n.rm=FALSE = needed 
  summarise(max.val = max(Age.Adjusted.Rate, na.rm=FALSE),
            min.val = min(Age.Adjusted.Rate, na.rm=FALSE))

## find the difference between each county's max and min
bound$diff <- abs(bound$max.val - bound$min.val)

## set the max and min value (for the legend) at 95% of the largest difference
mmax.val <- quantile(bound$diff, .95, na.rm=TRUE)
mmin.val <- -1*mmax.val
mcuts <- seq(mmin.val, mmax.val, length.out = length(mmap.colors))

# Construct break ranges for displaying in the legend

#mcolorRanges <- data.frame(
  #from = head(mcuts, length(mcuts)-1),
 # to = tail(mcuts, length(mcuts)-1)
#)
