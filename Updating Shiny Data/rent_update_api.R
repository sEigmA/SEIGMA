######################################
## Title: Median Rent Update - API  ##
## Author(s): Valerie Evans         ##
## Date Created:  11/11/2019        ##
## Date Modified: 12/20/2019        ##
######################################
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
## Create function to loop over all 5-year ranges (2006-2017)
## Add year and gender columns
## Separate runs for US, MA, Counties, Municipalities
## Final datasets will have varying column numbers -- need to add geography columns (Municipal, County, State, Region)
## Final column names/order: Municipal, County, Five_Year_Range, Median.Rent.2017.Dollar, Rent.Margin.of.Error.2017.Dollar

variables <- read_excel("ACS Variables.xlsx", sheet = "Rental Housing Costs")
yearget <- list(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)
counties <- list("state:25 + county:001", "state:25 + county:003", "state:25 + county:005", "state:25 + county:007", 
                 "state:25 + county:009", "state:25 + county:011", "state:25 + county:013", "state:25 + county:015", 
                 "state:25 + county:017", "state:25 + county:019", "state:25 + county:021", "state:25 + county:023", 
                 "state:25 + county:025", "state:25 + county:027")
geography <- read.csv("geography.csv", stringsAsFactors = FALSE)
rentnames <- c("Municipal", "County", "State", "Region", "Five.Year.Range", "Median.Rent.2018.Dollar", "Rent.Margin.of.Error.2018.Dollar")
CPI <- read.csv("CPI_2010_2018.csv", stringsAsFactors = FALSE) #if you already have a file saved elsewhere
CPI$inflation_rate <- 251.1070/CPI$average #use current year as numerator

## If you need to create a CPI dataset; you will need to register for an API key to download all years at once: https://data.bls.gov/registrationEngine/
# library(blscrapeR)
# #You'll need to register for an API key to download all years at once: https://data.bls.gov/registrationEngine/
# df_cpi <- bls_api("CUSR0000SA0", startyear = 2010, endyear = 2018,
#                   registrationKey = "a621394b12a84fe0adc76df37ca675b5",
#                   annualaverage = TRUE)
# CPI_annual_avg <- df_cpi %>% group_by(year) %>% summarise(average = mean(value))
# CPI$inflation_rate <- 245.1342/CPI$average #use current year as numerator


## Median Contract Rent (Table B25058) - Create Individual Geography Datasets
#US
rentupdate_b25058_us <- NULL
for (i in yearget) {
  temp <- getCensus(name = "acs/acs5", 
                    vintage = i,  
                    vars = variables$`ACS CODE`, 
                    region = "us:*")
  temp$Year <- i
  rentupdate_b25058_us <- rbind(rentupdate_b25058_us, temp)
}
rentupdate_b25058_us <- rentupdate_b25058_us[,-1]
colnames(rentupdate_b25058_us) <- c("Region", "Median.Rent.2018.Dollar", "Rent.Margin.of.Error.2018.Dollar", "Five_Year_Range")
rentupdate_b25058_us$Municipal <- NA
rentupdate_b25058_us$County <- "NA County"
rentupdate_b25058_us$State <- NA

#MA
rentupdate_b25058_ma <- NULL
for (i in yearget) {
  temp <- getCensus(name = "acs/acs5", 
                    vintage = i,  
                    vars = variables$`ACS CODE`, 
                    region = "state:25")
  temp$Year <- i
  rentupdate_b25058_ma <- rbind(rentupdate_b25058_ma, temp)
}
rentupdate_b25058_ma <- rentupdate_b25058_ma[,-1]
colnames(rentupdate_b25058_ma) <- c("Region", "Median.Rent.2018.Dollar", "Rent.Margin.of.Error.2018.Dollar", "Five_Year_Range")
rentupdate_b25058_ma$Region <- gsub("Massachusetts", "MA", rentupdate_b25058_ma$Region)
rentupdate_b25058_ma$Municipal <- NA
rentupdate_b25058_ma$County <- "NA County"
rentupdate_b25058_ma$State <- "MA"

#County
rentupdate_b25058_county <- NULL
for (i in yearget) {
  temp <- getCensus(name = "acs/acs5", 
                    vintage = i,  
                    vars = variables$`ACS CODE`, 
                    region = "county:*", 
                    regionin = "state:25")
  temp$Year <- i
  rentupdate_b25058_county <- rbind(rentupdate_b25058_county, temp)
}
rentupdate_b25058_county$NAME <-  gsub("(.*),.*", "\\1", rentupdate_b25058_county$NAME)
rentupdate_b25058_county <- merge(rentupdate_b25058_county, geography, by.x = "NAME", by.y = "Region")
rentupdate_b25058_county <- rentupdate_b25058_county[,-c(2:3)]
colnames(rentupdate_b25058_county) <- c("Region", "Median.Rent.2018.Dollar", "Rent.Margin.of.Error.2018.Dollar", "Five_Year_Range", "Municipal", "County", "State")

#Muni
rentupdate_b25058_muni <- NULL
for (i in yearget) { 
  for (j in counties) {
    temp <- getCensus(name = "acs/acs5",
                      vintage = i,
                      vars = variables$`ACS CODE`,
                      region = "county subdivision:*",
                      regionin = j)
    temp$Year <- i
    rentupdate_b25058_muni <- rbind(rentupdate_b25058_muni, temp)
  }
}
rentupdate_b25058_muni$NAME <- gsub("(.*),.*", "\\1", rentupdate_b25058_muni$NAME)
rentupdate_b25058_muni$NAME <- gsub("(.*),.*", "\\1", rentupdate_b25058_muni$NAME)
rentupdate_b25058_muni <- rentupdate_b25058_muni[!grepl("County subdivisions not defined", rentupdate_b25058_muni$NAME),]
rentupdate_b25058_muni$NAME <- gsub(" city", "", rentupdate_b25058_muni$NAME)
rentupdate_b25058_muni$NAME <- gsub(" town", "", rentupdate_b25058_muni$NAME)
rentupdate_b25058_muni$NAME <- gsub(" Town", "", rentupdate_b25058_muni$NAME)
rentupdate_b25058_muni <- merge(rentupdate_b25058_muni, geography, by.x = "NAME", by.y = "Region")
rentupdate_b25058_muni <- rentupdate_b25058_muni[,-c(2:4)]
colnames(rentupdate_b25058_muni) <- c("Region", "Median.Rent.2018.Dollar", "Rent.Margin.of.Error.2018.Dollar", "Five_Year_Range", "Municipal", "County", "State")

## Bind rows for US, MA, counties, and municipalities
rentupdate_b25058 <- bind_rows(rentupdate_b25058_us, rentupdate_b25058_ma, rentupdate_b25058_county, rentupdate_b25058_muni)

## Convert all to most recent $ amount
rentupdate_b25058$IA_Med_Rent <- rep(0, nrow(rentupdate_b25058))
#Adjust median rent for inflation -- end year needs to updated as new data is downloaded
for (i in 2010:2018) {
  rentupdate_b25058$IA_Med_Rent[which(rentupdate_b25058$Five_Year_Range == i)] <- 
    rentupdate_b25058$Median.Rent.2018.Dollar[which(rentupdate_b25058$Five_Year_Range == i)]*CPI$inflation_rate[which(CPI$year==i)]
}
rentupdate_b25058$IA_Med_Rent <- round(rentupdate_b25058$IA_Med_Rent, 0)
#Adjust margin of error for inflation -- end year needs to be updated as new data is downloaded
rentupdate_b25058$IA_Rent_Error <- rep(0, nrow(rentupdate_b25058))
for (i in 2010:2018) {
  rentupdate_b25058$IA_Rent_Error[which(rentupdate_b25058$Five_Year_Range == i)] <- 
    rentupdate_b25058$Rent.Margin.of.Error.2018.Dollar[which(rentupdate_b25058$Five_Year_Range == i)]*CPI$inflation_rate[which(CPI$year==i)]
}
rentupdate_b25058$IA_Rent_Error <- round(rentupdate_b25058$IA_Rent_Error, 0)

## Change Five_Year_Range
rentupdate_b25058$Five_Year_Range <- gsub("2010", "2006-2010", rentupdate_b25058$Five_Year_Range)
rentupdate_b25058$Five_Year_Range <- gsub("2011", "2007-2011", rentupdate_b25058$Five_Year_Range)
rentupdate_b25058$Five_Year_Range <- gsub("2012", "2008-2012", rentupdate_b25058$Five_Year_Range)
rentupdate_b25058$Five_Year_Range <- gsub("2013", "2009-2013", rentupdate_b25058$Five_Year_Range)
rentupdate_b25058$Five_Year_Range <- gsub("2014", "2010-2014", rentupdate_b25058$Five_Year_Range)
rentupdate_b25058$Five_Year_Range <- gsub("2015", "2011-2015", rentupdate_b25058$Five_Year_Range)
rentupdate_b25058$Five_Year_Range <- gsub("2016", "2012-2016", rentupdate_b25058$Five_Year_Range)
rentupdate_b25058$Five_Year_Range <- gsub("2017", "2013-2017", rentupdate_b25058$Five_Year_Range)

## Final column order and names (keep new inflation adjusted columns)
rentupdate_b25058 <- rentupdate_b25058[,c(5:7,1,4,8:9)]
colnames(rentupdate_b25058) <- rentnames

## Write new csv file to replace old in shiny app
write_csv(rentupdate_b25058, "rentupdate_b25058.csv")

