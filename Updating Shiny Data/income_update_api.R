########################################
## Title: Median Income Update - API  ##
## Author(s): Valerie Evans           ##
## Date Created:  11/11/2019          ##
## Date Modified: 12/19/2019          ##
########################################
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
## Final column names/order: Municipal, County, State, Region, Five_Year_Range, 

variables <- read_excel("ACS Variables.xlsx", sheet = "Household Income")
yearget <- list(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)
counties <- list("state:25 + county:001", "state:25 + county:003", "state:25 + county:005", "state:25 + county:007", 
                 "state:25 + county:009", "state:25 + county:011", "state:25 + county:013", "state:25 + county:015", 
                 "state:25 + county:017", "state:25 + county:019", "state:25 + county:021", "state:25 + county:023", 
                 "state:25 + county:025", "state:25 + county:027")
geography <- read.csv("geography.csv", stringsAsFactors = FALSE)


## Median Annual Household Income (Table DP03)
#US
incomeupdate_dp03_us <- NULL
for (i in yearget) {
  temp <- getCensus(name = "acs/acs5/profile", 
                    vintage = i,  
                    vars = variables$`ACS CODE`, 
                    region = "us:*")
  temp$Year <- i
  incomeupdate_dp03_us <- rbind(incomeupdate_dp03_us, temp)
}
incomeupdate_dp03_us <- incomeupdate_dp03_us[,-1]
incomeupdate_dp03_us$Municipal <- NA
incomeupdate_dp03_us$County <- "NA County"
incomeupdate_dp03_us$State <- NA

#MA
incomeupdate_dp03_ma <- NULL
for (i in yearget) {
  temp <- getCensus(name = "acs/acs5/profile", 
                    vintage = i,  
                    vars = variables$`ACS CODE`, 
                    region = "state:25")
  temp$Year <- i
  incomeupdate_dp03_ma <- rbind(incomeupdate_dp03_ma, temp)
}
incomeupdate_dp03_ma <- incomeupdate_dp03_ma[,-1]
incomeupdate_dp03_ma$NAME <- gsub("Massachusetts", "MA", incomeupdate_dp03_ma$NAME)
incomeupdate_dp03_ma$Municipal <- NA
incomeupdate_dp03_ma$County <- "NA County"
incomeupdate_dp03_ma$State <- "MA"

#County
incomeupdate_dp03_county <- NULL
for (i in yearget) {
  temp <- getCensus(name = "acs/acs5/profile", 
                    vintage = i,  
                    vars = variables$`ACS CODE`, 
                    region = "county:*", 
                    regionin = "state:25")
  temp$Year <- i
  incomeupdate_dp03_county <- rbind(incomeupdate_dp03_county, temp)
}
incomeupdate_dp03_county$NAME <-  gsub("(.*),.*", "\\1", incomeupdate_dp03_county$NAME)
incomeupdate_dp03_county <- merge(incomeupdate_dp03_county, geography, by.x = "NAME", by.y = "Region")
incomeupdate_dp03_county <- incomeupdate_dp03_county[,-c(2:3)]

#Muni
incomeupdate_dp03_muni <- NULL
for (i in yearget) { 
  for (j in counties) {
    temp <- getCensus(name = "acs/acs5/profile",
                      vintage = i,
                      vars = variables$`ACS CODE`,
                      region = "county subdivision:*",
                      regionin = j)
    temp$Year <- i
    incomeupdate_dp03_muni <- rbind(incomeupdate_dp03_muni, temp)
  }
}
incomeupdate_dp03_muni$NAME <-  gsub("(.*),.*", "\\1", incomeupdate_dp03_muni$NAME)
incomeupdate_dp03_muni$NAME <-  gsub("(.*),.*", "\\1", incomeupdate_dp03_muni$NAME)
incomeupdate_dp03_muni <- incomeupdate_dp03_muni[!grepl("County subdivisions not defined", incomeupdate_dp03_muni$NAME),]
incomeupdate_dp03_muni$NAME <- gsub(" city", "", incomeupdate_dp03_muni$NAME)
incomeupdate_dp03_muni$NAME <- gsub(" town", "", incomeupdate_dp03_muni$NAME)
incomeupdate_dp03_muni$NAME <- gsub(" Town", "", incomeupdate_dp03_muni$NAME)
incomeupdate_dp03_muni <- merge(incomeupdate_dp03_muni, geography, by.x = "NAME", by.y = "Region")
incomeupdate_dp03_muni <- incomeupdate_dp03_muni[,-c(2:4)]


## Bind rows for US, MA, counties, and municipalities
incomeupdate_dp03 <- bind_rows(incomeupdate_dp03_us, incomeupdate_dp03_ma, incomeupdate_dp03_county, incomeupdate_dp03_muni)

## Change Five_Year_Range
incomeupdate_dp03$Year <- gsub("2010", "2006-2010", incomeupdate_dp03$Year)
incomeupdate_dp03$Year <- gsub("2011", "2007-2011", incomeupdate_dp03$Year)
incomeupdate_dp03$Year <- gsub("2012", "2008-2012", incomeupdate_dp03$Year)
incomeupdate_dp03$Year <- gsub("2013", "2009-2013", incomeupdate_dp03$Year)
incomeupdate_dp03$Year <- gsub("2014", "2010-2014", incomeupdate_dp03$Year)
incomeupdate_dp03$Year <- gsub("2015", "2011-2015", incomeupdate_dp03$Year)
incomeupdate_dp03$Year <- gsub("2016", "2012-2016", incomeupdate_dp03$Year)
incomeupdate_dp03$Year <- gsub("2017", "2013-2017", incomeupdate_dp03$Year)
incomeupdate_dp03$Year <- gsub("2018", "2014-2018", incomeupdate_dp03$Year)

## Add Five_Year_Average column 
incomeupdate_dp03$Five_Year_Average <- incomeupdate_dp03$Year
incomeupdate_dp03$Five_Year_Average <- gsub("2006-2010", "2008", incomeupdate_dp03$Five_Year_Average)
incomeupdate_dp03$Five_Year_Average <- gsub("2007-2011", "2009", incomeupdate_dp03$Five_Year_Average)
incomeupdate_dp03$Five_Year_Average <- gsub("2008-2012", "2010", incomeupdate_dp03$Five_Year_Average)
incomeupdate_dp03$Five_Year_Average <- gsub("2009-2013", "2011", incomeupdate_dp03$Five_Year_Average)
incomeupdate_dp03$Five_Year_Average <- gsub("2010-2014", "2012", incomeupdate_dp03$Five_Year_Average)
incomeupdate_dp03$Five_Year_Average <- gsub("2011-2015", "2013", incomeupdate_dp03$Five_Year_Average)
incomeupdate_dp03$Five_Year_Average <- gsub("2012-2016", "2014", incomeupdate_dp03$Five_Year_Average)
incomeupdate_dp03$Five_Year_Average <- gsub("2013-2017", "2015", incomeupdate_dp03$Five_Year_Average)
incomeupdate_dp03$Five_Year_Average <- gsub("2014-2018", "2016", incomeupdate_dp03$Five_Year_Average)

## Final column names and order
colnames(incomeupdate_dp03) <- c(variables$`COLUMN NAME`, "Year", "Municipal", "County", "State", "Five_Year_Average")
incomeupdate_dp03 <- incomeupdate_dp03[,c(5:7,1,4,8,2:3)]

## Replace suppressed data with NA in Margin_Error_Pop (Gosnold data suppressed in 2013-2017)
incomeupdate_dp03$Median_Annual_Household_Income <- gsub("-666666666", NA, incomeupdate_dp03$Median_Annual_Household_Income)
incomeupdate_dp03$Margin_Error_Median <- gsub("-222222222", NA, incomeupdate_dp03$Margin_Error_Median)

## Write new csv file to replace old in shiny app
write_csv(incomeupdate_dp03, "incomeupdate_dp03.csv")

## Compare to original dataset 
income <- read.csv("original datasets/income.csv", stringsAsFactors = FALSE)
income <- income[,-1]
discrep <- mapply(setdiff, incomeupdate_dp03, income)
discrep #Five_Year_Average is not accurate in original dataset, 
num.discrep <- sapply(discrep, length)
num.discrep
write.csv(num.discrep, "income_numdiscrep.csv")
#which(grepl(38750, incomeupdate_dp03$Median_Annual_Household_Income))


