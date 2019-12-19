#########################################
## Title: Marital Status Update - API  ##
## Author(s): Valerie Evans            ##
## Date Created:  11/01/2019           ##
## Date Modified: 12/19/2019           ##
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
## Create function to loop over all 5-year ranges (2006-2018)
## Separate runs for US, MA, Counties, Municipalities by Gender
## Final datasets will have varying column numbers -- need to add geography columns (Municipal, County, State, Region)
## Final column names/order: Municipal, County, State, Region, Five_Year_Range, Population, Never_Married, 
   #Never_Married_pct, Never_Married_pct_error, Now_Married, Now_Married_pct, Now_Married_pct_error, Separated, 
   #Separated_pct, Separated_pct_error, Widowed, Widowed_pct, Widowed_pct_error, Divorced, Divorced_pct,
   #Divorced_pct_error, Gender

yearget <- list(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)
counties <- list("state:25 + county:001", "state:25 + county:003", "state:25 + county:005", "state:25 + county:007", 
                 "state:25 + county:009", "state:25 + county:011", "state:25 + county:013", "state:25 + county:015", 
                 "state:25 + county:017", "state:25 + county:019", "state:25 + county:021", "state:25 + county:023", 
                 "state:25 + county:025", "state:25 + county:027")
geography <- read.csv("geography.csv", stringsAsFactors = FALSE)
colnames <- read_excel("ACS Variables.xlsx", sheet = "Marital")
colnames_male <- colnames[c(1:17),]
colnames_female <- colnames[c(1,18:33),]

## Marital Status (Table DP02)
#US MALE
maritalupdate_dp02_us_male <- NULL
for (i in yearget) {
  temp <- getCensus(name = "acs/acs5/profile", 
                    vintage = i,  
                    vars = colnames_male$`ACS CODE`, 
                    region = "us:*")
  temp$Year <- i
  temp$Gender <- "Male"
  maritalupdate_dp02_us_male <- rbind(maritalupdate_dp02_us_male, temp)
}
maritalupdate_dp02_us_male <- maritalupdate_dp02_us_male[,-1]
colnames(maritalupdate_dp02_us_male) <- c(colnames_male$`COLUMN NAME`, "Five_Year_Range", "Gender")
maritalupdate_dp02_us_male$Municipal <- NA
maritalupdate_dp02_us_male$County <- "NA County"
maritalupdate_dp02_us_male$State <- NA

#US FEMALE
maritalupdate_dp02_us_female <- NULL
for (i in yearget) {
  temp <- getCensus(name = "acs/acs5/profile", 
                    vintage = i,  
                    vars = colnames_female$`ACS CODE`, 
                    region = "us:*")
  temp$Year <- i
  temp$Gender <- "Female"
  maritalupdate_dp02_us_female <- rbind(maritalupdate_dp02_us_female, temp)
}
maritalupdate_dp02_us_female <- maritalupdate_dp02_us_female[,-1]
colnames(maritalupdate_dp02_us_female) <- c(colnames_female$`COLUMN NAME`, "Five_Year_Range", "Gender")
maritalupdate_dp02_us_female$Municipal <- NA
maritalupdate_dp02_us_female$County <- "NA County"
maritalupdate_dp02_us_female$State <- NA

#MA MALE
maritalupdate_dp02_ma_male <- NULL
for (i in yearget) {
  temp <- getCensus(name = "acs/acs5/profile", 
                    vintage = i,  
                    vars = colnames_male$`ACS CODE`, 
                    region = "state:25")
  temp$Year <- i
  temp$Gender <- "Male"
  maritalupdate_dp02_ma_male <- rbind(maritalupdate_dp02_ma_male, temp)
}
maritalupdate_dp02_ma_male <- maritalupdate_dp02_ma_male[,-1]
colnames(maritalupdate_dp02_ma_male) <- c(colnames_male$`COLUMN NAME`, "Five_Year_Range", "Gender")
maritalupdate_dp02_ma_male$Municipal <- NA
maritalupdate_dp02_ma_male$County <- "NA County"
maritalupdate_dp02_ma_male$State <- "MA"
maritalupdate_dp02_ma_male$Region <- gsub("Massachusetts", "MA", maritalupdate_dp02_ma_male$Region)

#MA FEMALE
maritalupdate_dp02_ma_female <- NULL
for (i in yearget) {
  temp <- getCensus(name = "acs/acs5/profile", 
                    vintage = i,  
                    vars = colnames_female$`ACS CODE`, 
                    region = "state:25")
  temp$Year <- i
  temp$Gender <- "Female"
  maritalupdate_dp02_ma_female <- rbind(maritalupdate_dp02_ma_female, temp)
}
maritalupdate_dp02_ma_female <- maritalupdate_dp02_ma_female[,-1]
colnames(maritalupdate_dp02_ma_female) <- c(colnames_female$`COLUMN NAME`, "Five_Year_Range", "Gender")
maritalupdate_dp02_ma_female$Municipal <- NA
maritalupdate_dp02_ma_female$County <- "NA County"
maritalupdate_dp02_ma_female$State <- "MA"
maritalupdate_dp02_ma_female$Region <- gsub("Massachusetts", "MA", maritalupdate_dp02_ma_female$Region)

#County MALE
maritalupdate_dp02_county_male <- NULL
for (i in yearget) {
  temp <- getCensus(name = "acs/acs5/profile", 
                    vintage = i,  
                    vars = colnames_male$`ACS CODE`, 
                    region = "county:*", 
                    regionin = "state:25")
  temp$Year <- i
  temp$Gender <- "Male"
  maritalupdate_dp02_county_male <- rbind(maritalupdate_dp02_county_male, temp)
}
maritalupdate_dp02_county_male$NAME <-  gsub("(.*),.*", "\\1", maritalupdate_dp02_county_male$NAME)
maritalupdate_dp02_county_male <- merge(maritalupdate_dp02_county_male, geography, by.x = "NAME", by.y = "Region")
maritalupdate_dp02_county_male <- maritalupdate_dp02_county_male[,-c(2:3)]
colnames(maritalupdate_dp02_county_male) <- c(colnames_male$`COLUMN NAME`, "Five_Year_Range", "Gender", "Municipal", "County", "State")

#County FEMALE
maritalupdate_dp02_county_female <- NULL
for (i in yearget) {
  temp <- getCensus(name = "acs/acs5/profile", 
                    vintage = i,  
                    vars = colnames_female$`ACS CODE`, 
                    region = "county:*", 
                    regionin = "state:25")
  temp$Year <- i
  temp$Gender <- "Female"
  maritalupdate_dp02_county_female <- rbind(maritalupdate_dp02_county_female, temp)
}
maritalupdate_dp02_county_female$NAME <-  gsub("(.*),.*", "\\1", maritalupdate_dp02_county_female$NAME)
maritalupdate_dp02_county_female <- merge(maritalupdate_dp02_county_female, geography, by.x = "NAME", by.y = "Region")
maritalupdate_dp02_county_female <- maritalupdate_dp02_county_female[,-c(2:3)]
colnames(maritalupdate_dp02_county_female) <- c(colnames_female$`COLUMN NAME`, "Five_Year_Range", "Gender", "Municipal", "County", "State")

#Muni MALE
maritalupdate_dp02_muni_male <- NULL
for (i in yearget) { 
  for (j in counties) {
    temp <- getCensus(name = "acs/acs5/profile",
                      vintage = i,
                      vars = colnames_male$`ACS CODE`, 
                      region = "county subdivision:*",
                      regionin = j)
    temp$Year <- i
    temp$Gender <- "Male"
    maritalupdate_dp02_muni_male <- rbind(maritalupdate_dp02_muni_male, temp)
  }
}
maritalupdate_dp02_muni_male$NAME <-  gsub("(.*),.*", "\\1", maritalupdate_dp02_muni_male$NAME)
maritalupdate_dp02_muni_male$NAME <-  gsub("(.*),.*", "\\1", maritalupdate_dp02_muni_male$NAME)
maritalupdate_dp02_muni_male <- maritalupdate_dp02_muni_male[!grepl("County subdivisions not defined", maritalupdate_dp02_muni_male$NAME),]
maritalupdate_dp02_muni_male$NAME <- gsub(" city", "", maritalupdate_dp02_muni_male$NAME)
maritalupdate_dp02_muni_male$NAME <- gsub(" town", "", maritalupdate_dp02_muni_male$NAME)
maritalupdate_dp02_muni_male$NAME <- gsub(" Town", "", maritalupdate_dp02_muni_male$NAME)
maritalupdate_dp02_muni_male <- merge(maritalupdate_dp02_muni_male, geography, by.x = "NAME", by.y = "Region")
maritalupdate_dp02_muni_male <- maritalupdate_dp02_muni_male[,-c(2:4)]
colnames(maritalupdate_dp02_muni_male) <- c(colnames_male$`COLUMN NAME`, "Five_Year_Range", "Gender", "Municipal", "County", "State")

#Muni FEMALE
maritalupdate_dp02_muni_female <- NULL
for (i in yearget) { 
  for (j in counties) {
    temp <- getCensus(name = "acs/acs5/profile",
                      vintage = i,
                      vars = colnames_female$`ACS CODE`, 
                      region = "county subdivision:*",
                      regionin = j)
    temp$Year <- i
    temp$Gender <- "Female"
    maritalupdate_dp02_muni_female <- rbind(maritalupdate_dp02_muni_female, temp)
  }
}
maritalupdate_dp02_muni_female$NAME <-  gsub("(.*),.*", "\\1", maritalupdate_dp02_muni_female$NAME)
maritalupdate_dp02_muni_female$NAME <-  gsub("(.*),.*", "\\1", maritalupdate_dp02_muni_female$NAME)
maritalupdate_dp02_muni_female <- maritalupdate_dp02_muni_female[!grepl("County subdivisions not defined", maritalupdate_dp02_muni_female$NAME),]
maritalupdate_dp02_muni_female$NAME <- gsub(" city", "", maritalupdate_dp02_muni_female$NAME)
maritalupdate_dp02_muni_female$NAME <- gsub(" town", "", maritalupdate_dp02_muni_female$NAME)
maritalupdate_dp02_muni_female$NAME <- gsub(" Town", "", maritalupdate_dp02_muni_female$NAME)
maritalupdate_dp02_muni_female <- merge(maritalupdate_dp02_muni_female, geography, by.x = "NAME", by.y = "Region")
maritalupdate_dp02_muni_female <- maritalupdate_dp02_muni_female[,-c(2:4)]
colnames(maritalupdate_dp02_muni_female) <- c(colnames_female$`COLUMN NAME`, "Five_Year_Range", "Gender", "Municipal", "County", "State")


## Bind rows for US, MA, counties, and municipalities
maritalupdate_dp02 <- bind_rows(maritalupdate_dp02_us_male, maritalupdate_dp02_us_female, 
                                maritalupdate_dp02_ma_male, maritalupdate_dp02_ma_female, 
                                maritalupdate_dp02_county_male, maritalupdate_dp02_county_female, 
                                maritalupdate_dp02_muni_male, maritalupdate_dp02_muni_female)

## Change Year to Five_Year_Range
maritalupdate_dp02$Five_Year_Range <- gsub("2010", "2006-2010", maritalupdate_dp02$Five_Year_Range)
maritalupdate_dp02$Five_Year_Range <- gsub("2011", "2007-2011", maritalupdate_dp02$Five_Year_Range)
maritalupdate_dp02$Five_Year_Range <- gsub("2012", "2008-2012", maritalupdate_dp02$Five_Year_Range)
maritalupdate_dp02$Five_Year_Range <- gsub("2013", "2009-2013", maritalupdate_dp02$Five_Year_Range)
maritalupdate_dp02$Five_Year_Range <- gsub("2014", "2010-2014", maritalupdate_dp02$Five_Year_Range)
maritalupdate_dp02$Five_Year_Range <- gsub("2015", "2011-2015", maritalupdate_dp02$Five_Year_Range)
maritalupdate_dp02$Five_Year_Range <- gsub("2016", "2012-2016", maritalupdate_dp02$Five_Year_Range)
maritalupdate_dp02$Five_Year_Range <- gsub("2017", "2013-2017", maritalupdate_dp02$Five_Year_Range)
maritalupdate_dp02$Five_Year_Range <- gsub("2018", "2013-2018", maritalupdate_dp02$Five_Year_Range)


## Final column names/order: Municipal, County, State, Region, Five_Year_Range, Population, Never_Married, 
#Never_Married_pct, Never_Married_pct_error, Now_Married, Now_Married_pct, Now_Married_pct_error, Separated, 
#Separated_pct, Separated_pct_error, Widowed, Widowed_pct, Widowed_pct_error, Divorced, Divorced_pct,
#Divorced_pct_error, Gender
maritalupdate_dp02 <- maritalupdate_dp02[,c(20,21,22,1,18,2,3:17,19)]

## Write new csv file to replace old in shiny app
write_csv(maritalupdate_dp02, "maritalupdate_dp02.csv")

## Compare to original dataset 
maritalstatus <- read.csv("original datasets/marital.csv", stringsAsFactors = FALSE)
discrep <- mapply(setdiff, maritalupdate_dp02, maritalstatus)
discrep
num.discrep <- sapply(discrep, length)
num.discrep
