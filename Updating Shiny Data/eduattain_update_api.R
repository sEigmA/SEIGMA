#################################################
## Title: Educational Attainment Update - API  ##
## Author(s): Valerie Evans                    ##
## Date Created:  11/11/2019                   ##
## Date Modified: 12/19/2019                   ##
#################################################
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
## Final column names/order: Municipal, County, State, Region, Five_Year_Range, Pop_25, Margin_Error_Pop, HS_Pct, 
## Margin_Error_HS, Bachelors_Pct, Margin_Error_Bach, Grad_Pct, Margin_Error_Grad


yearget <- list(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)
counties <- list("state:25 + county:001", "state:25 + county:003", "state:25 + county:005", "state:25 + county:007", 
                 "state:25 + county:009", "state:25 + county:011", "state:25 + county:013", "state:25 + county:015", 
                 "state:25 + county:017", "state:25 + county:019", "state:25 + county:021", "state:25 + county:023", 
                 "state:25 + county:025", "state:25 + county:027")
variables <- read_excel("ACS Variables.xlsx", sheet = "Educational Attainment")
geography <- read.csv("geography.csv", stringsAsFactors = FALSE)


## Educational Attainment (Table DP02)
#US
eduupdate_dp02_us <- NULL
for (i in yearget) {
  temp <- getCensus(name = "acs/acs5/profile", 
                    vintage = i,  
                    vars = variables$`ACS CODE`, 
                    region = "us:*")
  temp$Year <- i
  eduupdate_dp02_us <- rbind(eduupdate_dp02_us, temp)
}
eduupdate_dp02_us <- eduupdate_dp02_us[,-1]
colnames(eduupdate_dp02_us) <- c(variables$`COLUMN NAME`, "Five_Year_Range")
eduupdate_dp02_us$Municipal <- NA
eduupdate_dp02_us$County <- "NA County"
eduupdate_dp02_us$State <- NA

#MA
eduupdate_dp02_ma <- NULL
for (i in yearget) {
  temp <- getCensus(name = "acs/acs5/profile", 
                    vintage = i,  
                    vars = variables$`ACS CODE`, 
                    region = "state:25")
  temp$Year <- i
  eduupdate_dp02_ma <- rbind(eduupdate_dp02_ma, temp)
}
eduupdate_dp02_ma <- eduupdate_dp02_ma[,-1]
colnames(eduupdate_dp02_ma) <- c(variables$`COLUMN NAME`, "Five_Year_Range")
eduupdate_dp02_ma$Region <- gsub("Massachusetts", "MA", eduupdate_dp02_ma$Region)
eduupdate_dp02_ma$Municipal <- NA
eduupdate_dp02_ma$County <- "NA County"
eduupdate_dp02_ma$State <- "MA"

#County
eduupdate_dp02_county <- NULL
for (i in yearget) {
  temp <- getCensus(name = "acs/acs5/profile", 
                    vintage = i,  
                    vars = variables$`ACS CODE`, 
                    region = "county:*", 
                    regionin = "state:25")
  temp$Year <- i
  eduupdate_dp02_county <- rbind(eduupdate_dp02_county, temp)
}
eduupdate_dp02_county$NAME <-  gsub("(.*),.*", "\\1", eduupdate_dp02_county$NAME)
eduupdate_dp02_county <- merge(eduupdate_dp02_county, geography, by.x = "NAME", by.y = "Region")
eduupdate_dp02_county <- eduupdate_dp02_county[,-c(2:3)]
colnames(eduupdate_dp02_county) <- c(variables$`COLUMN NAME`, "Five_Year_Range", "Municipal", "County", "State")

#Muni
eduupdate_dp02_muni <- NULL
for (i in yearget) { 
  for (j in counties) {
    temp <- getCensus(name = "acs/acs5/profile",
                      vintage = i,
                      vars = variables$`ACS CODE`,
                      region = "county subdivision:*",
                      regionin = j)
    temp$Year <- i
    eduupdate_dp02_muni <- rbind(eduupdate_dp02_muni, temp)
  }
}
eduupdate_dp02_muni$NAME <- gsub("(.*),.*", "\\1", eduupdate_dp02_muni$NAME)
eduupdate_dp02_muni$NAME <- gsub("(.*),.*", "\\1", eduupdate_dp02_muni$NAME)
eduupdate_dp02_muni <- eduupdate_dp02_muni[!grepl("County subdivisions not defined", eduupdate_dp02_muni$NAME),]
eduupdate_dp02_muni$NAME <- gsub(" city", "", eduupdate_dp02_muni$NAME)
eduupdate_dp02_muni$NAME <- gsub(" town", "", eduupdate_dp02_muni$NAME)
eduupdate_dp02_muni$NAME <- gsub(" Town", "", eduupdate_dp02_muni$NAME)
eduupdate_dp02_muni <- merge(eduupdate_dp02_muni, geography, by.x = "NAME", by.y = "Region")
eduupdate_dp02_muni <- eduupdate_dp02_muni[,-c(2:4)]
colnames(eduupdate_dp02_muni) <- c(variables$`COLUMN NAME`, "Five_Year_Range", "Municipal", "County", "State")

## Bind rows for US, MA, counties, and municipalities
eduupdate_dp02 <- bind_rows(eduupdate_dp02_us, eduupdate_dp02_ma, eduupdate_dp02_county, eduupdate_dp02_muni)

## Change Five_Year_Range
eduupdate_dp02$Five_Year_Range <- gsub("2010", "2006-2010", eduupdate_dp02$Five_Year_Range)
eduupdate_dp02$Five_Year_Range <- gsub("2011", "2007-2011", eduupdate_dp02$Five_Year_Range)
eduupdate_dp02$Five_Year_Range <- gsub("2012", "2008-2012", eduupdate_dp02$Five_Year_Range)
eduupdate_dp02$Five_Year_Range <- gsub("2013", "2009-2013", eduupdate_dp02$Five_Year_Range)
eduupdate_dp02$Five_Year_Range <- gsub("2014", "2010-2014", eduupdate_dp02$Five_Year_Range)
eduupdate_dp02$Five_Year_Range <- gsub("2015", "2011-2015", eduupdate_dp02$Five_Year_Range)
eduupdate_dp02$Five_Year_Range <- gsub("2016", "2012-2016", eduupdate_dp02$Five_Year_Range)
eduupdate_dp02$Five_Year_Range <- gsub("2017", "2013-2017", eduupdate_dp02$Five_Year_Range)
eduupdate_dp02$Five_Year_Range <- gsub("2018", "2014-2018", eduupdate_dp02$Five_Year_Range)

## Final column order
eduupdate_dp02 <- eduupdate_dp02[,c(11:13,1,10,2:9)]

## Replace -555555555 with NA in Margin_Error_Pop
eduupdate_dp02$Margin_Error_Pop <- gsub("-555555555", NA, eduupdate_dp02$Margin_Error_Pop)

## Write new csv file to replace old in shiny app
write_csv(eduupdate_dp02, "eduupdate_dp02.csv")

## Compare to original dataset 
eduattain <- read.csv("original datasets/education.csv", stringsAsFactors = FALSE)
eduattain <- eduattain[,-1]
discrep <- mapply(setdiff, eduupdate_dp02, eduattain)
discrep
num.discrep <- sapply(discrep, length)
num.discrep

