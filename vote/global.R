###############################
## Title: global.R           ##
## App: SEIGMA VOTE          ##
## Author: Zhenning Kang     ##
## Date Created:  01/12/2018 ##
## Last Modified: 02/13/2018 ##
###############################

library(sqldf)

# 2014 vote results
vote <- read.csv(file="data/ACS_14_1yr_BF002.csv")
vote <- vote[-nrow(vote),-1]
colnames(vote)[1] <- "Town"

# 2013 population 
dem_data <- read.csv(file="data/demodata.csv")
dem_data$Year <- as.factor(as.numeric(substr(dem_data$Five_Year_Range, 1, 4))+2)
pop <- sqldf("select municipal as muni, total_population as population
      from dem_data 
      where year = 2013 and municipal is not null")

# 2013 high school rate
edu_data <- read.csv(file="data/edudata.csv")[,-1]
edu_data$Year <- as.factor(as.numeric(substr(edu_data$Five_Year_Range, 1, 4))+2)
bac <- sqldf("select municipal as muni, bachelors_pct as bachelor
      from edu_data 
      where year = 2013 and municipal is not null")

# 2013 median income 
inc_data <- read.csv(file="data/incomedata.csv")[,-1]
inc_data$Year <- as.factor(as.numeric(substr(inc_data$Five_Year_Range, 1, 4))+2)
inc <- sqldf("select municipal as muni, median_annual_household_income as inc
      from inc_data
      where year = 2013 and municipal is not null")
inc$inc <- as.numeric(inc$inc)

# 2012 unemployment rate 
une_data <- read.csv(file="data/unempdata2.csv")
unemp <- sqldf("select municipal as muni, unemployment_rate_avg as unemp
               from une_data
               where year = 2012 and municipal is not null")

# create dataset for map
keyvar <- sqldf("select b.*, unemp.unemp as unemployment
from (select a.*, inc.inc as income
      from (select pop.*, bac.bachelor from pop join bac on pop.muni = bac.muni) a 
      join inc on a.muni = inc.muni) b
join unemp on b.muni = unemp.muni
                  order by b.muni") 
keyvar$yes <- vote$Yes
keyvar$no <- vote$No

# add MA city lat/long
malonlat <- read.csv(file="data/malonlat.csv")[,-1]
keyvar$lon <- malonlat[,2]
keyvar$lat <- malonlat[,3]
ma_muni <- malonlat$muni





### DON'T RUN THE CODES BELOW
### DATA ALREADY SAVED IN CSV

# add missing lat/lon
# library(ggmap)
# nolatlong <- (subset(cleantable, is.na(lat))[,1])
# search <- c()
# for (i in 1:length(nolatlong)){
#   search[i] <- paste0(nolatlong[i]," Massachusetts United States")
# }
# missingll <- cbind(muni=nolatlong, geocode(search))
# temp <- merge(cleantable[,c(1,9,8)], missingll, by="muni", all=TRUE)
# temp[,2] <- ifelse(is.na(temp[,2]), temp[,4], temp[,2])
# temp[,3] <- ifelse(is.na(temp[,3]), temp[,5], temp[,3])
# colnames(temp)[2:3] <- c("lon", "lat")
# write.csv(temp[,1:3], file = "data/malonlat.csv")


# allzips <- readRDS("data/superzip.rds")
# allzips$latitude <- jitter(allzips$latitude)
# allzips$longitude <- jitter(allzips$longitude)
# mazips <-subset(allzips, allzips$state.x == "MA")
# colnames(mazips)[5] <- "city"
# addlatlong <- sqldf("select keyvar.*, mazips.latitude as lat, mazips.longitude as lon
#       from keyvar left join mazips
#       on keyvar.muni = mazips.city
#       order by keyvar.muni")

