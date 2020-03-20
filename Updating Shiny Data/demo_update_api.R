#########################################
## Title: Demographics Update - API    ##
## Author(s): Valerie Evans            ##
## Date Created:  11/04/2019           ##
## Date Modified: 03/02/2020           ##
#########################################
# Based on https://cran.r-project.org/web/packages/censusapi/vignettes/getting-started.html
# Census API: https://api.census.gov/data/2010/acs/acs5/profile.html

setwd("~/Documents/R/SEIGMA/Updating Shiny Data")

####  SETTINGS  ####
setwd("~/SEIGMA/Updating Shiny Data")
library(censusapi)
library(tidyverse)
library(readxl)
library(naniar)

## Add census key
# Add key to .Renviron
Sys.setenv(CENSUS_KEY = "8fcb02f2025c2bde7566d120dfb5c77b1f2486c9")
# Reload .Renviron
readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
Sys.getenv("CENSUS_KEY")


####  DATA  ####
## Create function to loop over all 5-year ranges (2006-2017)
## Add year column
## Separate runs for US, MA, Counties, Municipalities
## Final datasets will have varying column numbers -- need to add geography columns (Municipal, County, State, Region)
## Final column names/order (59): Municipal, County, State, Region, Five_Year_Range, Total_Population, 
#Margin_Error_Total_Population, Male_Pct,	Margin_Error_Male, Female_Pct, Margin_Error_Female,	
#Age_under_5_Pct, Margin_Error_under_5_Pct, Age_5_9_Pct, Margin_Error_5_9_Pct,	
#Age_10_14_Pct,	Margin_Error_10_14_Pct, Age_15_19_Pct, Margin_Error_15_19_Pct, 
#Age_20_24_Pct, Margin_Error_20_24_Pct, Age_25_34_Pct, Margin_Error_25_34_Pct, 
#Age_35_44_Pct, Margin_Error_35_44_Pct, Age_45_54_Pct, Margin_Error_45_54_Pct, 
#Age_55_59_Pct, Margin_Error_55_59_Pct, Age_60_64_Pct, Margin_Error_60_64_Pct, 
#Age_65_74_Pct, Margin_Error_65_74_Pct, Age_75_84_Pct, Margin_Error_75_84_Pct, 
#Age_over_85_Pct, Margin_Error_85._Pct, White_Pct, Margin_Error_White_Pct, Black_Pct, Margin_Error_Black_Pct, 
#American_Indian_and_Alaska_Native_Pct, Margin_Error_American_Indian_and_Alaska_Native_Pct, 
#Asian_Pct, Margin_Error_Asian_Pct, Hawaiian_and_Other_Pacific_Islander_Pct, Margin_Error_Hawaiian_and_Other_Pacific_Islander_Pct, 
#Others_Pct, Margin_Error_Others_Pct, Hispanic_Pct, Margin_Error_Hispanic_Pct, 
#Not_Hispanic_Pct, Margin_Error_Not_Hispanic_Pct, Age_under_20_Pct_plot, Age_20_34_Pct_plot, 
#Age_35_54_Pct_plot, Age_55_64_Pct_plot, Age_65_74_Pct_plot, Age_over_75_Pct_plot

variables <- read_excel("ACS Variables.xlsx", sheet = "Demo")
yearget <- list(2010, 2011, 2012, 2013, 2014, 2015, 2016) #2017 and 2018 need to be pulled separately
yearget2 <- list(2017, 2018)
counties <- list("state:25 + county:001", "state:25 + county:003", "state:25 + county:005", "state:25 + county:007", 
                 "state:25 + county:009", "state:25 + county:011", "state:25 + county:013", "state:25 + county:015", 
                 "state:25 + county:017", "state:25 + county:019", "state:25 + county:021", "state:25 + county:023", 
                 "state:25 + county:025", "state:25 + county:027")
geography <- read.csv("geography.csv", stringsAsFactors = FALSE)

## Demographics (Table DP05) 
#US
demoupdate_dp05_us <- NULL
for (i in yearget) {
  temp <- getCensus(name = "acs/acs5/profile", 
                    vintage = i,  
                    vars = variables$`ACS CODE`, 
                    region = "us:*")
  temp$Year <- i
  demoupdate_dp05_us <- rbind(demoupdate_dp05_us, temp)
}
demoupdate_dp05_us <- demoupdate_dp05_us[,-1]
colnames(demoupdate_dp05_us) <- c(variables$`COLUMN NAME`, "Five_Year_Range")
#variables are not the same for 2013-2017 and 2014-2018 datasets after first 6 variables 
demoupdate_dp05_us_2 <- NULL
for (i in yearget2) {
  temp <- getCensus(name = "acs/acs5/profile", 
                    vintage = i,  
                    vars = variables$`ACS CODE 2`, 
                    region = "us:*")
  temp$Year <- i
  demoupdate_dp05_us_2 <- rbind(demoupdate_dp05_us_2, temp)
}
demoupdate_dp05_us_2 <- demoupdate_dp05_us_2[,-1]
colnames(demoupdate_dp05_us_2) <- c(variables$`COLUMN NAME`, "Five_Year_Range")

#bind data
demoupdate_dp05_us <- rbind(demoupdate_dp05_us, demoupdate_dp05_us_2)
demoupdate_dp05_us$Municipal <- NA
demoupdate_dp05_us$County <- NA
demoupdate_dp05_us$State <- NA

#MA
demoupdate_dp05_ma <- NULL
for (i in yearget) {
  temp <- getCensus(name = "acs/acs5/profile", 
                    vintage = i,  
                    vars = variables$`ACS CODE`, 
                    region = "state:25")
  temp$Year <- i
  demoupdate_dp05_ma <- rbind(demoupdate_dp05_ma, temp)
}
demoupdate_dp05_ma <- demoupdate_dp05_ma[,-1]
colnames(demoupdate_dp05_ma) <- c(variables$`COLUMN NAME`, "Five_Year_Range")
#variables are not the same for 2013-2017 and 2014-2018 datasets after first 6 variables 
demoupdate_dp05_ma_2 <- NULL
for (i in yearget2) {
  temp <- getCensus(name = "acs/acs5/profile", 
                    vintage = i,  
                    vars = variables$`ACS CODE 2`, 
                    region = "state:25")
  temp$Year <- i
  demoupdate_dp05_ma_2 <- rbind(demoupdate_dp05_ma_2, temp)
}
demoupdate_dp05_ma_2 <- demoupdate_dp05_ma_2[,-1]
colnames(demoupdate_dp05_ma_2) <- c(variables$`COLUMN NAME`, "Five_Year_Range")

#bind data
demoupdate_dp05_ma <- rbind(demoupdate_dp05_ma, demoupdate_dp05_ma_2)
demoupdate_dp05_ma$Region <- gsub("Massachusetts", "MA", demoupdate_dp05_ma$Region)
demoupdate_dp05_ma$Municipal <- NA
demoupdate_dp05_ma$County <- NA
demoupdate_dp05_ma$State <- "MA"

#County
demoupdate_dp05_county <- NULL
for (i in yearget) {
  temp <- getCensus(name = "acs/acs5/profile", 
                    vintage = i,  
                    vars = variables$`ACS CODE`, 
                    region = "county:*", 
                    regionin = "state:25")
  temp$Year <- i
  demoupdate_dp05_county <- rbind(demoupdate_dp05_county, temp)
}
demoupdate_dp05_county <- demoupdate_dp05_county[,-c(1:2)]
colnames(demoupdate_dp05_county) <- c(variables$`COLUMN NAME`, "Five_Year_Range")
#variables are not the same for 2013-2017 and 2014-2018 datasets after first 6 variables 
demoupdate_dp05_county_2 <- NULL
for (i in yearget2) {
  temp <- getCensus(name = "acs/acs5/profile", 
                    vintage = i,  
                    vars = variables$`ACS CODE 2`, 
                    region = "county:*", 
                    regionin = "state:25")
  temp$Year <- i
  demoupdate_dp05_county_2 <- rbind(demoupdate_dp05_county_2, temp)
}
demoupdate_dp05_county_2 <- demoupdate_dp05_county_2[,-c(1:2)]
colnames(demoupdate_dp05_county_2) <- c(variables$`COLUMN NAME`, "Five_Year_Range")

#bind data
demoupdate_dp05_county <- rbind(demoupdate_dp05_county, demoupdate_dp05_county_2)
demoupdate_dp05_county$Region <-  gsub("(.*),.*", "\\1", demoupdate_dp05_county$Region)
demoupdate_dp05_county <- merge(demoupdate_dp05_county, geography, by.x = "Region", by.y = "Region")
colnames(demoupdate_dp05_county) <- c(variables$`COLUMN NAME`, "Five_Year_Range", "Municipal", "County", "State")

#Muni
demoupdate_dp05_muni <- NULL
for (i in yearget) { 
  for (j in counties) {
    temp <- getCensus(name = "acs/acs5/profile",
                      vintage = i,
                      vars = variables$`ACS CODE`,
                      region = "county subdivision:*",
                      regionin = j)
    temp$Year <- i
    demoupdate_dp05_muni <- rbind(demoupdate_dp05_muni, temp)
  }
}
demoupdate_dp05_muni <- demoupdate_dp05_muni[,-c(1:3)]
colnames(demoupdate_dp05_muni) <- c(variables$`COLUMN NAME`, "Five_Year_Range")
#variables are not the same for 2013-2017 and 2014-2018 datasets after first 6 variables 
demoupdate_dp05_muni_2 <- NULL
for (i in yearget2) { 
  for (j in counties) {
    temp <- getCensus(name = "acs/acs5/profile",
                      vintage = i,
                      vars = variables$`ACS CODE 2`,
                      region = "county subdivision:*",
                      regionin = j)
    temp$Year <- i
    demoupdate_dp05_muni_2 <- rbind(demoupdate_dp05_muni_2, temp)
  }
}
demoupdate_dp05_muni_2 <- demoupdate_dp05_muni_2[,-c(1:3)]
colnames(demoupdate_dp05_muni_2) <- c(variables$`COLUMN NAME`, "Five_Year_Range")

#bind data
demoupdate_dp05_muni <- rbind(demoupdate_dp05_muni, demoupdate_dp05_muni_2)
demoupdate_dp05_muni$Region <- gsub("(.*),.*", "\\1", demoupdate_dp05_muni$Region)
demoupdate_dp05_muni$Region <- gsub("(.*),.*", "\\1", demoupdate_dp05_muni$Region)
demoupdate_dp05_muni <- demoupdate_dp05_muni[!grepl("County subdivisions not defined", demoupdate_dp05_muni$Region),]
demoupdate_dp05_muni$Region <- gsub(" city", "", demoupdate_dp05_muni$Region)
demoupdate_dp05_muni$Region <- gsub(" town", "", demoupdate_dp05_muni$Region)
demoupdate_dp05_muni$Region <- gsub(" Town", "", demoupdate_dp05_muni$Region)
demoupdate_dp05_muni <- merge(demoupdate_dp05_muni, geography, by.x = "Region", by.y = "Region")

## Bind rows for US, MA, counties, and municipalities
demoupdate_dp05 <- bind_rows(demoupdate_dp05_us, demoupdate_dp05_ma, demoupdate_dp05_county, demoupdate_dp05_muni)

## Change Five_Year_Range
demoupdate_dp05$Five_Year_Range <- gsub("2010", "2006-2010", demoupdate_dp05$Five_Year_Range)
demoupdate_dp05$Five_Year_Range <- gsub("2011", "2007-2011", demoupdate_dp05$Five_Year_Range)
demoupdate_dp05$Five_Year_Range <- gsub("2012", "2008-2012", demoupdate_dp05$Five_Year_Range)
demoupdate_dp05$Five_Year_Range <- gsub("2013", "2009-2013", demoupdate_dp05$Five_Year_Range)
demoupdate_dp05$Five_Year_Range <- gsub("2014", "2010-2014", demoupdate_dp05$Five_Year_Range)
demoupdate_dp05$Five_Year_Range <- gsub("2015", "2011-2015", demoupdate_dp05$Five_Year_Range)
demoupdate_dp05$Five_Year_Range <- gsub("2016", "2012-2016", demoupdate_dp05$Five_Year_Range)
demoupdate_dp05$Five_Year_Range <- gsub("2017", "2013-2017", demoupdate_dp05$Five_Year_Range)
demoupdate_dp05$Five_Year_Range <- gsub("2018", "2014-2018", demoupdate_dp05$Five_Year_Range)

## Final column order
demoupdate_dp05 <- demoupdate_dp05[,c(51:53,1,50,2:49)]

## Replace missing with NA throughout dataste
demoupdate_dp05 <- demoupdate_dp05 %>% 
  replace_with_na(replace = list(Margin_Error_Total_Population = -555555555, 
                                 Margin_Error_Male = c(-888888888.0, -555555555.0), 
                                 Margin_Error_Female = c(-888888888.0, -555555555.0), 
                                 Margin_Error_under_5_Pct = c(-555555555.0, -888888888.0), 
                                 Margin_Error_25_34_Pct = -888888888.0, 
                                 Margin_Error_35_44_Pct = -888888888.0, 
                                 Margin_Error_45_54_Pct = -888888888.0, 
                                 Margin_Error_65_74_Pct = c(-888888888.0, -555555555.0), 
                                 Margin_Error_Hawaiian_and_Other_Pacific_Islander_Pct = -888888888, 
                                 Margin_Error_Hispanic_Pct = c(-555555555, -888888888), 
                                 Margin_Error_Not_Hispanic_Pct = c(-555555555, -888888888)))

## Add new columns: Age_under_20_Pct_plot, Age_20_34_Pct_plot, Age_35_54_Pct_plot, Age_55_64_Pct_plot, Age_65_74_Pct_plot, Age_over_75_Pct_plot
demoupdate_dp05$Age_under_20_Pct_plot <- demoupdate_dp05$Age_under_5_Pct + demoupdate_dp05$Age_5_9_Pct + demoupdate_dp05$Age_10_14_Pct + demoupdate_dp05$Age_15_19_Pct
demoupdate_dp05$Age_20_34_Pct_plot <- demoupdate_dp05$Age_20_24_Pct + demoupdate_dp05$Age_25_34_Pct
demoupdate_dp05$Age_35_54_Pct_plot <- demoupdate_dp05$Age_35_44_Pct + demoupdate_dp05$Age_45_54_Pct
demoupdate_dp05$Age_55_64_Pct_plot <- demoupdate_dp05$Age_55_59_Pct + demoupdate_dp05$Age_60_64_Pct
demoupdate_dp05$Age_65_74_Pct_plot <- demoupdate_dp05$Age_65_74_Pct
demoupdate_dp05$Age_over_75_Pct_plot <- demoupdate_dp05$Age_75_84_Pct + demoupdate_dp05$Age_over_85_Pct

## Write new csv file to replace old in shiny app
write_csv(demoupdate_dp05, "demoupdate_dp05.csv")

## Compare to original dataset 
#setwd("..")
demographics <- read.csv("original datasets/demographics.csv", stringsAsFactors = FALSE)
demographics_2010_2017 <- demographics %>% filter(Five_Year_Range != "2005-2009")
discrep <- mapply(setdiff, demoupdate_dp05, demographics_2010_2017)
discrep
num.discrep <- sapply(discrep, length)
num.discrep
#which(grepl(xxxx, demoupdate_dp05))

write.csv(num.discrep, "demoupdate_numdiscrep.csv")


