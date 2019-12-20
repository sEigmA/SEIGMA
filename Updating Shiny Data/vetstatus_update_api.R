#########################################
## Title: Veteran Status Update - API  ##
## Author(s): Valerie Evans            ##
## Date Created:  10/31/2019           ##
## Date Modified: 12/20/2019           ##
#########################################
# based on https://cran.r-project.org/web/packages/censusapi/vignettes/getting-started.html
# Census API: https://api.census.gov/data/2010/acs/acs5/profile.html


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
## Create function to loop over all 5-year ranges (2006-2017) and add year column
## Separate runs for US, MA, Counties, Municipalities
## Final datasets will have varying column numbers -- need to add geography columns (Municipal, County, State, Region)
## Final column names/order: Municipal, County, State, Region, Five_Year_Range, Civililan_Pop, Vet_Pop, Percent_Vet, Margin_Error_Percent

yearget <- list(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)
counties <- list("state:25 + county:001", "state:25 + county:003", "state:25 + county:005", "state:25 + county:007", 
                 "state:25 + county:009", "state:25 + county:011", "state:25 + county:013", "state:25 + county:015", 
                 "state:25 + county:017", "state:25 + county:019", "state:25 + county:021", "state:25 + county:023", 
                 "state:25 + county:025", "state:25 + county:027")
variables <- read_excel("ACS Variables.xlsx", sheet = "Veteran")
geography <- read.csv("geography.csv", stringsAsFactors = FALSE)

## Veteran Status (Table DP02)
#US
vetupdate_dp02_us <- NULL
for (i in yearget) {
  temp <- getCensus(name = "acs/acs5/profile", 
                    vintage = i,  
                    vars = variables$`ACS CODE`, 
                    region = "us:*")
  temp$Year <- i
  vetupdate_dp02_us <- rbind(vetupdate_dp02_us, temp)
}
vetupdate_dp02_us$Municipal <- NA
vetupdate_dp02_us$County <- "NA County"
vetupdate_dp02_us$State <- NA
vetupdate_dp02_us <- vetupdate_dp02_us[,-1]

#MA
vetupdate_dp02_ma <- NULL
for (i in yearget) {
  temp <- getCensus(name = "acs/acs5/profile", 
                    vintage = i,  
                    vars = variables$`ACS CODE`, 
                    region = "state:25")
  temp$Year <- i
  vetupdate_dp02_ma <- rbind(vetupdate_dp02_ma, temp)
}
vetupdate_dp02_ma$Municipal <- NA
vetupdate_dp02_ma$County <- "NA County"
vetupdate_dp02_ma$State <- "MA"
vetupdate_dp02_ma$NAME <- gsub("Massachusetts", "MA", vetupdate_dp02_ma$NAME)
vetupdate_dp02_ma <- vetupdate_dp02_ma[,-1]

#County
vetupdate_dp02_county <- NULL
for (i in yearget) {
  temp <- getCensus(name = "acs/acs5/profile", 
                    vintage = i,  
                    vars = variables$`ACS CODE`, 
                    region = "county:*", 
                    regionin = "state:25")
  temp$Year <- i
  vetupdate_dp02_county <- rbind(vetupdate_dp02_county, temp)
}
vetupdate_dp02_county$NAME <-  gsub("(.*),.*", "\\1", vetupdate_dp02_county$NAME)
vetupdate_dp02_county <- merge(vetupdate_dp02_county, geography, by.x = "NAME", by.y = "Region")
vetupdate_dp02_county <- vetupdate_dp02_county[,-c(2:3)]

#Muni
vetupdate_dp02_muni <- NULL
for (i in yearget) { 
  for (j in counties) {
  temp <- getCensus(name = "acs/acs5/profile",
                    vintage = i,
                    vars = variables$`ACS CODE`, 
                    region = "county subdivision:*",
                    regionin = j)
  temp$Year <- i
  vetupdate_dp02_muni <- rbind(vetupdate_dp02_muni, temp)
  }
}
vetupdate_dp02_muni$NAME <-  gsub("(.*),.*", "\\1", vetupdate_dp02_muni$NAME)
vetupdate_dp02_muni$NAME <-  gsub("(.*),.*", "\\1", vetupdate_dp02_muni$NAME)
vetupdate_dp02_muni <- vetupdate_dp02_muni[!grepl("County subdivisions not defined", vetupdate_dp02_muni$NAME),]
vetupdate_dp02_muni$NAME <- gsub(" city", "", vetupdate_dp02_muni$NAME)
vetupdate_dp02_muni$NAME <- gsub(" town", "", vetupdate_dp02_muni$NAME)
vetupdate_dp02_muni$NAME <- gsub(" Town", "", vetupdate_dp02_muni$NAME)
vetupdate_dp02_muni <- merge(vetupdate_dp02_muni, geography, by.x = "NAME", by.y = "Region")
vetupdate_dp02_muni <- vetupdate_dp02_muni[,-c(2:4)]

## Bind rows for US, MA, counties, and municipalities
vetupdate_dp02 <- bind_rows(vetupdate_dp02_us, vetupdate_dp02_ma, vetupdate_dp02_county, vetupdate_dp02_muni)

## Change Year to Five_Year_Range
vetupdate_dp02$Year <- gsub("2010", "2006-2010", vetupdate_dp02$Year)
vetupdate_dp02$Year <- gsub("2011", "2007-2011", vetupdate_dp02$Year)
vetupdate_dp02$Year <- gsub("2012", "2008-2012", vetupdate_dp02$Year)
vetupdate_dp02$Year <- gsub("2013", "2009-2013", vetupdate_dp02$Year)
vetupdate_dp02$Year <- gsub("2014", "2010-2014", vetupdate_dp02$Year)
vetupdate_dp02$Year <- gsub("2015", "2011-2015", vetupdate_dp02$Year)
vetupdate_dp02$Year <- gsub("2016", "2012-2016", vetupdate_dp02$Year)
vetupdate_dp02$Year <- gsub("2017", "2013-2017", vetupdate_dp02$Year)
vetupdate_dp02$Year <- gsub("2018", "2014-2018", vetupdate_dp02$Year)

## Final column names/order: Municipal, County, State, Region, Five_Year_Range, Civililan_Pop, Vet_Pop, Percent_Vet, Margin_Error_Percent
colnames(vetupdate_dp02) <- c(variables$`COLUMN NAME`, "Five_Year_Range", "Municipal", "County", "State")
vetupdate_dp02 <- vetupdate_dp02[,c(7,8,9,1,6,2,3,4,5)]

## Write new csv file to replace old in shiny app
write_csv(vetupdate_dp02, "vetupdate_dp02.csv")

## Compare to original dataset 
vetstatus <- read.csv("original datasets/va_status.csv", stringsAsFactors = FALSE)
vetstatus <- vetstatus[,-1]
discrep <- mapply(setdiff, vetupdate_dp02, vetstatus)
discrep
num.discrep <- sapply(discrep, length)
num.discrep

