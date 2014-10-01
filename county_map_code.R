#################################
#Emily Ramos
#Map raw geojson file for County
#Began: 10/1/14
#Modified: 10/1/14
#################################

#----- needed packages
library(leafletR)
library(rgdal) #for reading/writing geo files
library(rgeos) #for simplification
library(sp)

#----- Load in GeoJSON and Data files
downloaddir <- getwd()
leafdat<-"County_2010Census_DP1.geojson"
load("county_subdat.RData")

# ----- Create the cuts
cuts<-round(quantile(subdat$Population, probs = seq(0, 1, 0.20), na.rm = FALSE), 0)
cuts[1]<-0 # ----- for this example make first cut zero

# ----- Fields to include in the popup
popup<-c("County", "Population")

# ----- Gradulated style based on an attribute
sty<-styleGrad(prop="Population", breaks=cuts, right=FALSE, style.par="col",
               style.val=rev(heat.colors(6)), leg="Population (2010)", lwd=1)

# ----- Create the map and load into browser
map<-leaflet(data=leafdat, dest=downloaddir, style=sty,
             title="counties", base.map="osm",
             incl.data=TRUE,  popup=popup)

# ----- to look at the map you can use this code
browseURL(map)
