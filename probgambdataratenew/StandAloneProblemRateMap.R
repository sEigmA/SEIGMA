




require(RJSONIO)
require(dplyr)
require(leaflet)

MAmap <- fromJSON("County_2010Census_DP1.geojson")


## Find order of counties in geojson files
## Each county is a separate feature
MAcounties <- c()
for(i in 1:length(MAmap$features)){
  MAcounties <- c(MAcounties, MAmap$features[[i]]$properties$County)
}

## Load formatted suicide data
## -1 eliminates first column [rows,columns]
suidata <- read.csv(file="Problem_Gambling_data.csv")[,]


cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                "#0072B2", "#D55E00", "#CC79A7")



## Colors for a single-year legend
spaint.brush <- colorRampPalette(colors=c("white", "red3"))
smap.colors <- c(spaint.brush(n=5), "#999999")

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

scolorRanges <- data.frame(
  from = head(scuts, length(scuts)-1),
  to = tail(scuts, length(scuts)-1)
)





