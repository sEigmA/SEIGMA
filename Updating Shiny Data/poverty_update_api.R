#######################################
## Title: Poverty Rate Update - API  ##
## Author(s): Valerie Evans          ##
## Date Created:  11/11/2019         ##
## Date Modified: 11/14/2019         ##
#######################################
# based on https://cran.r-project.org/web/packages/censusapi/vignettes/getting-started.html
# Census API: https://api.census.gov/data/2010/acs/acs5/profile.html

setwd("~/Documents/R/SEIGMA/Updating Shiny Data")

####  SETTINGS  ####
library(censusapi)
library(tidyverse)
library(readxl)

## Add census key
# Add key to .Renviron
Sys.setenv(CENSUS_KEY = "8fcb02f2025c2bde7566d120dfb5c77b1f2486c9")
# Reload .Renviron
readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
Sys.getenv("CENSUS_KEY")


####  DATA  ####
## Create function to loop over all 5-year ranges (2006-2017)
## Add year and gender columns
## Separate runs for US, MA, Counties, Municipalities
## Final datasets will have varying column numbers -- need to add geography columns (Municipal, County, State, Region)
## Final column names/order: Municipal, County, State, Region, Five_Year_Range, Total_Pop, Pov_Pop, Percent_Pov, Margin_Error_Percent

variables <- read_excel("ACS Variables.xlsx", sheet = "Poverty Rate")
pov_variables <- variables[,c(1,4)]
pov_variables_s1701 <- pov_variables[c(1:3),]
pov_variables_dp03 <- pov_variables[c(1,4:5),]
yearget <- list(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)
counties <- list("state:25 + county:001", "state:25 + county:003", "state:25 + county:005", "state:25 + county:007", 
                 "state:25 + county:009", "state:25 + county:011", "state:25 + county:013", "state:25 + county:015", 
                 "state:25 + county:017", "state:25 + county:019", "state:25 + county:021", "state:25 + county:023", 
                 "state:25 + county:025", "state:25 + county:027")
geography <- read.csv("geography.csv", stringsAsFactors = FALSE)


## Poverty Status (Need both Table DP02 and Table S1701)
#US
povupdate_s1701_us <- NULL
for (i in yearget) {
  temp <- getCensus(name = "acs/acs5", 
                    vintage = i,  
                    vars = pov_variables_s1701$`ACS CODE 2`, 
                    region = "us:*")
  temp$Year <- i
  povupdate_s1701_us <- rbind(povupdate_s1701_us, temp)
}
povupdate_dp03_us <- NULL
for (i in yearget) {
  temp <- getCensus(name = "acs/acs5/profile", 
                    vintage = i,  
                    vars = pov_variables_dp03$`ACS CODE 2`, 
                    region = "us:*")
  temp$Year <- i
  povupdate_dp03_us <- rbind(povupdate_dp03_us, temp)
}
povupdate_us <- merge(povupdate_s1701_us, povupdate_dp03_us)
povupdate_us <- povupdate_us[,-1]
colnames(povupdate_us) <- c("Region", "Five_Year_Range", "Total_Pop", "Pov_Pop", "Percent_Pov", "Margin_Error_Percent")
povupdate_us$Municipal <- NA
povupdate_us$County <- "NA County"
povupdate_us$State <- NA

#MA
povupdate_s1701_ma <- NULL
for (i in yearget) {
  temp <- getCensus(name = "acs/acs5", 
                    vintage = i,
                    vars = pov_variables_s1701$`ACS CODE 2`, 
                    region = "state:25")
  temp$Year <- i
  povupdate_s1701_ma <- rbind(povupdate_s1701_ma, temp)
}
povupdate_dp03_ma <- NULL
for (i in yearget) {
  temp <- getCensus(name = "acs/acs5/profile", 
                    vintage = i,  
                    vars = pov_variables_dp03$`ACS CODE 2`, 
                    region = "state:25")
  temp$Year <- i
  povupdate_dp03_ma <- rbind(povupdate_dp03_ma, temp)
}
povupdate_ma <- merge(povupdate_s1701_ma, povupdate_dp03_ma)
povupdate_ma <- povupdate_ma[,-1]
colnames(povupdate_ma) <- c("Region", "Five_Year_Range", "Total_Pop", "Pov_Pop", "Percent_Pov", "Margin_Error_Percent")
povupdate_ma$Region <- gsub("Massachusetts", "MA", povupdate_ma$Region)
povupdate_ma$Municipal <- NA
povupdate_ma$County <- "NA County"
povupdate_ma$State <- "MA"

#County
povupdate_s1701_county <- NULL
for (i in yearget) {
  temp <- getCensus(name = "acs/acs5", 
                    vintage = i,  
                    vars = pov_variables_s1701$`ACS CODE 2`, 
                    region = "county:*", 
                    regionin = "state:25")
  temp$Year <- i
  povupdate_s1701_county <- rbind(povupdate_s1701_county, temp)
}
povupdate_dp03_county <- NULL
for (i in yearget) {
  temp <- getCensus(name = "acs/acs5/profile", 
                    vintage = i,  
                    vars = pov_variables_dp03$`ACS CODE 2`, 
                    region = "county:*", 
                    regionin = "state:25")
  temp$Year <- i
  povupdate_dp03_county <- rbind(povupdate_dp03_county, temp)
}
povupdate_county <- merge(povupdate_s1701_county, povupdate_dp03_county)
povupdate_county$NAME <-  gsub("(.*),.*", "\\1", povupdate_county$NAME)
povupdate_county <- merge(povupdate_county, geography, by.x = "NAME", by.y = "Region")
povupdate_county <- povupdate_county[,-c(2:3)]
colnames(povupdate_county) <- c("Region", "Five_Year_Range", "Total_Pop", "Pov_Pop", "Percent_Pov", "Margin_Error_Percent", 
                                "Municipal", "County", "State")

#Muni
povupdate_s1701_muni <- NULL
for (i in yearget) { 
  for (j in counties) {
    temp <- getCensus(name = "acs/acs5",
                      vintage = i,
                      vars = pov_variables_s1701$`ACS CODE 2`,
                      region = "county subdivision:*",
                      regionin = j)
    temp$Year <- i
    povupdate_s1701_muni <- rbind(povupdate_s1701_muni, temp)
  }
}
povupdate_dp03_muni <- NULL
for (i in yearget) { 
  for (j in counties) {
    temp <- getCensus(name = "acs/acs5/profile",
                      vintage = i,
                      vars = pov_variables_dp03$`ACS CODE 2`,
                      region = "county subdivision:*",
                      regionin = j)
    temp$Year <- i
    povupdate_dp03_muni <- rbind(povupdate_dp03_muni, temp)
  }
}
povupdate_muni <- merge(povupdate_s1701_muni, povupdate_dp03_muni)
povupdate_muni$NAME <- gsub("(.*),.*", "\\1", povupdate_muni$NAME)
povupdate_muni$NAME <- gsub("(.*),.*", "\\1", povupdate_muni$NAME)
povupdate_muni <- povupdate_muni[!grepl("County subdivisions not defined", povupdate_muni$NAME),]
povupdate_muni$NAME <- gsub(" city", "", povupdate_muni$NAME)
povupdate_muni$NAME <- gsub(" town", "", povupdate_muni$NAME)
povupdate_muni$NAME <- gsub(" Town", "", povupdate_muni$NAME)
povupdate_muni <- merge(povupdate_muni, geography, by.x = "NAME", by.y = "Region")
povupdate_muni <- povupdate_muni[,-c(2:4)]
colnames(povupdate_muni) <- c("Region", "Five_Year_Range", "Total_Pop", "Pov_Pop", "Percent_Pov", "Margin_Error_Percent", 
                              "Municipal", "County", "State")

## Bind rows for US, MA, counties, and municipalities
povupdate_s1701_dp03 <- bind_rows(povupdate_us, povupdate_ma, povupdate_county, povupdate_muni)

## Change Five_Year_Range
povupdate_s1701_dp03$Five_Year_Range <- gsub("2010", "2006-2010", povupdate_s1701_dp03$Five_Year_Range)
povupdate_s1701_dp03$Five_Year_Range <- gsub("2011", "2007-2011", povupdate_s1701_dp03$Five_Year_Range)
povupdate_s1701_dp03$Five_Year_Range <- gsub("2012", "2008-2012", povupdate_s1701_dp03$Five_Year_Range)
povupdate_s1701_dp03$Five_Year_Range <- gsub("2013", "2009-2013", povupdate_s1701_dp03$Five_Year_Range)
povupdate_s1701_dp03$Five_Year_Range <- gsub("2014", "2010-2014", povupdate_s1701_dp03$Five_Year_Range)
povupdate_s1701_dp03$Five_Year_Range <- gsub("2015", "2011-2015", povupdate_s1701_dp03$Five_Year_Range)
povupdate_s1701_dp03$Five_Year_Range <- gsub("2016", "2012-2016", povupdate_s1701_dp03$Five_Year_Range)
povupdate_s1701_dp03$Five_Year_Range <- gsub("2017", "2013-2017", povupdate_s1701_dp03$Five_Year_Range)
povupdate_s1701_dp03$Five_Year_Range <- gsub("2018", "2014-2018", povupdate_s1701_dp03$Five_Year_Range)

## Final column order
povupdate_s1701_dp03 <- povupdate_s1701_dp03[,c(7:9,1,2:6)]

## Write new csv file to replace old in shiny app
write_csv(povupdate_s1701_dp03, "povupdate_s1701_dp03.csv")

## Compare to original dataset
poverty <- read.csv("original datasets/poverty.csv", stringsAsFactors = FALSE)
poverty <- poverty[,-1]
discrep <- mapply(setdiff, povupdate_s1701_dp03, poverty)
discrep #discrepancies found with MOE but were rounding errors in original dataset
num.discrep <- sapply(discrep, length)
num.discrep
compare <- right_join(povupdate_s1701_dp03, poverty)
write.csv(num.discrep, "poverty_numdiscrep.csv")

