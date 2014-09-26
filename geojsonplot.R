library(leafletR)
library(rgdal) #for reading/writing geo files
library(rgeos) #for simplification
library(sp)


##download and unzip a shapefile from the US Census
# note that this file is somewhat big so it might take a couple
# of minutes to download
url<-"http://www2.census.gov/geo/tiger/TIGER2010DP1/County_2010Census_DP1.zip"
downloaddir <- getwd()
destname<-"tiger.zip"
download.file(url, destname)
unzip(destname, exdir=downloaddir, junkpaths=TRUE)

#bring the shapefile into R for a little formatting. 
filename<-list.files(downloaddir, pattern=".shp", full.names=FALSE)
filename<-gsub(".shp", "", filename)

# ----- Read in shapefile (NAD83 coordinate system)
# ----- this is a fairly big shapefile and takes 1 minute to read
dat<-readOGR(downloaddir, filename) 

# ----- Create a subset of Mass counties
subdat<-dat[substring(dat$GEOID10, 1, 2) == "25",]

# ----- Transform to EPSG 4326 - WGS84 (required)
subdat<-spTransform(subdat, CRS("+init=epsg:4326"))

# ----- change name of field we will map
names(subdat)[names(subdat) == "DP0010001"]<-"Population"

#Simplify shape file
# ----- save the data slot
subdat_data<-subdat@data[,c("GEOID10", "Population")]

# ----- simplification yields a SpatialPolygons class
subdat<-gSimplify(subdat,tol=0.01, topologyPreserve=TRUE)

# ----- to write to geojson we need a SpatialPolygonsDataFrame
subdat<-SpatialPolygonsDataFrame(subdat, data=subdat_data)


##Play with Leaflet
# ----- Write data to GeoJSON
leafdat<-paste(downloaddir, "/", filename, ".geojson", sep="") 

writeOGR(obj=subdat, dsn=leafdat, layer="", driver="GeoJSON")

# ----- Create the cuts
cuts<-round(quantile(subdat$Population, probs = seq(0, 1, 0.20), na.rm = FALSE), 0)
cuts[1]<-0 # ----- for this example make first cut zero


# ----- Fields to include in the popup
popup<-c("GEOID10", "Population")


# ----- Gradulated style based on an attribute
sty<-styleGrad(prop="Population", breaks=cuts, right=FALSE, style.par="col",
               style.val=rev(heat.colors(6)), leg="Population (2010)", lwd=1)


# ----- Create the map and load into browser
map<-leaflet(data=leafdat, dest=downloaddir, style=sty,
             title="index", base.map="osm",
             incl.data=TRUE,  popup=popup)

# ----- to look at the map you can use this code
browseURL(map)



