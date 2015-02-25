
##Creating the Map

require(maps)
require(ggplot2)

require(ggmap)



##data(us.cities)

##Obtaining Problem Gambling Data
pgdat <- read.csv(file="Problem_Gambling_data.csv")[,]
head(pgdat)

#dat <- pgdat[c(1:14),c(1,7)]
dat <-pgdat


#data(unemp)
data(county.fips)
## This gives a fips code for the mass counties
mafips <- county.fips[c(1186:1199),]
mafips

mafips$polyname <- substring(mafips$polyname, first=15)
mafips

##This gives the dataset with fips codes (location), 
newdat <- cbind(mafips,dat)
head(newdat)
names(newdat)

quants <- quantile(newdat$Problem.Gambling.Rate)
quants




##legend("topright", leg.txt, horiz = TRUE, fill = colors)



##
##colnames(unemp)[6:12] <- substring(colnames(unemp[6:12]), first=12)
##mafips <-  substring(colnames(county.fips$[6:12]), first=12)

# Plot unemployment by country
colors = c("#F1EEF6", "#D4B9DA", "#C994C7", "#DF65B0", "#DD1C77", "#980043")



# df<-dat
# data.frame(lapply(df, function(V) {
#   if (is.character(V)) return(tolower(v))
#   else return(v)
# }))



par(mfrow = c(1, 1))
map("county", "MASSACHUSETTS")


dat <- pgdat[c(1:14),c(1,7)]
dat
dat <- cbind(dat,mafips)

mafips
#data(unemp)
#data(county.fips)

# Plot unemployment by country
colors = c("#F1EEF6", "#D4B9DA", "#C994C7", "#DF65B0", "#DD1C77", 
           "#980043")


dat$colorBuckets <- as.numeric(cut(dat$Problem.Gambling.Rate, c(0.39, 0.625, 1.25, 2.4, 3.30)))
colorsmatched <- dat$colorBuckets[match(mafips$fips, dat$fips)]

par(mar=c(2,2,0,0))
map("county", "MASSACHUSETTS", col = colors[colorsmatched], fill = TRUE, resolution = 0, 
    lty = 0, projection = "polyconic")
## Loading required package: mapproj

# Add border around each State
map("county","MASSACHUSETTS", col = "black", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2, 
    projection = "polyconic")
par(mar=c(4,4.5,2,1))
par(oma=c(0,0,0,0))
title("
      Problem Gambling Rates 
      by County in 2014")

#leg.txt <- c("<2%", "2-4%", "4-6%", "6-8%", "8-10%", ">10%")
leg.txt <- c("0.40-0.625%", "0.625-1.25%","1.25-2.40", "2.40-3.30%")

par(mar=c(1,2,0,0))
legend("bottomleft", leg.txt, horiz = FALSE, fill = colors,title="
       Rate by quantile")
 
dev.copy(jpeg,filename="MassCountyProbGambRatesMap.jpg");
dev.off ();

