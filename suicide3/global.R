require(RJSONIO)

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

## set the color to ramp from white to one of the colorblind colors and grey representing NA
paint.brush <- colorRampPalette(colors=c("white", "red"))
map.colors <- c(paint.brush(n=6), "#999999")

## find  max and min values of the variable in the total data and make cuts based on those values
max.val <- max(suidata$Crude.Rate, na.rm=TRUE)
min.val <- min(suidata$Crude.Rate, na.rm=TRUE)
cuts <- seq(min.val, max.val, (max.val-min.val)/(length(map.colors)-1))
