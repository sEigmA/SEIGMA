
require(maps)
require(ggplot2)

require(ggmap)


map("state", "MASSACHUSETTS")
data(us.cities)

pgdat <- read.csv(file="Problem_Gambling_data.csv")[,]
head(pgdat)

dat <- pgdat[c(1:14),c(1,6)]
dat


#data(unemp)
data(county.fips)
## This gives a fips code for the mass counties
mafips <- county.fips[c(1186:1199),]
mafips

mafips$polyname <- substring(mafips$polyname, first=15)
mafips

##This gives the dataset with fips codes (location), 
newdat <- cbind(mafips,dat)
View(newdat)
names(newdat)

quants <- quantile(newdat$Problem.Gambling.Rate)

leg.txt <- as.data.frame(quants)


leg.txt2 <- c("<2%", "2-4%", "4-6%", "6-8%", "8-10%", ">10%")
leg.txt
leg.txt2

legend("topright", leg.txt, horiz = TRUE, fill = colors)



##
##colnames(unemp)[6:12] <- substring(colnames(unemp[6:12]), first=12)
##mafips <-  substring(colnames(county.fips$[6:12]), first=12)

# Plot unemployment by country
colors = c("#F1EEF6", "#D4B9DA", "#C994C7", "#DF65B0", "#DD1C77", "#980043")

 

#View(county.fips)



unemp$colorBuckets <- as.numeric(cut(unemp$unemp, c(0, 2, 4, 6, 8, 
                                                    10, 100)))
colorsmatched <- unemp$colorBuckets[match(county.fips$fips, unemp$fips)]

map("county", col = colors[colorsmatched], fill = TRUE, resolution = 0, 
    lty = 0, projection = "polyconic")


# Add border around each State
map("state", col = "white", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2, 
    projection = "polyconic")
title("unemployment by county, 2009")

leg.txt <- c("<2%", "2-4%", "4-6%", "6-8%", "8-10%", ">10%")
legend("topright", leg.txt, horiz = TRUE, fill = colors)



