require(RJSONIO)
require(dplyr)
require(leaflet)

## load map data
# MAmap <- readShapeSpatial("countymaps/COUNTIES_POLYM.shp")
MAmap <- fromJSON("County_2010Census_DP1.geojson")

MAcounties <- c()
for(i in 1:length(MAmap$features)){
  MAcounties <- c(MAcounties, MAmap$features[[i]]$properties$County)
}

## load suicide data
suidata <- read.csv(file="SASuicidedata.csv")[,-1]

## set graph colors (special for colorblind people)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                "#0072B2", "#D55E00", "#CC79A7")

## create maxs and mins for googleCharts
xlim <- list(
  min = min(suidata$Year)-1,
  max = max(suidata$Year)+1
)
ylim <- list(
  min = 0,
  max = max(suidata$Crude.Rate, na.rm=T)+5
)

spaint.brush <- colorRampPalette(colors=c("white", "red"))
smap.colors <- c(spaint.brush(n=6))#, "#999999")

## find  max and min values of the variable in the total data and make cuts based on those values
smax.val <- max(suidata$Crude.Rate, na.rm=TRUE)
smin.val <- min(suidata$Crude.Rate, na.rm=TRUE)
scuts <- seq(smin.val, smax.val, length.out = length(smap.colors)+1)
# Construct break ranges for displaying in the legend
scolorRanges <- data.frame(
  from = head(scuts, length(scuts)-1),
  to = tail(scuts, length(scuts)-1)
)
# scolorRanges <- rbind.data.frame(scolorRanges, c(NA, NA))

## colors fade from one color to white to another color, with gray for NAs
mpaint.brush <- colorRampPalette(colors=c(cbbPalette[6], "white", cbbPalette[7]))
mmap.colors <- c(mpaint.brush(n=6))#, "#999999")

## find max and min values for each county
bound <- suidata %>%
  group_by(County) %>%
  summarise(max.val = max(Crude.Rate, na.rm=FALSE),
            min.val = min(Crude.Rate, na.rm=FALSE))

## find the difference between each county's max and min
bound$diff <- abs(bound$max.val - bound$min.val)

## set the max and min value (for the legend) at 95% of the largest difference
mmax.val <- quantile(bound$diff, .95, na.rm=TRUE)
mmin.val <- -1*mmax.val
mcuts <- seq(mmin.val, mmax.val, length.out = length(mmap.colors)+1)

# Construct break ranges for displaying in the legend
mcolorRanges <- data.frame(
  from = head(mcuts, length(mcuts)-1),
  to = tail(mcuts, length(mcuts)-1)
)
# mcolorRanges <- rbind.data.frame(mcolorRanges, c(NA, NA))
