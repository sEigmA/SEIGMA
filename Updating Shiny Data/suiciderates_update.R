###########################################
##  Title: Suicide Rates Update          ##
##  Author(s): Valerie Evans             ## 
##  Date Created:  01/15/2019            ##
##  Date Modified: 01/22/2020            ##
###########################################
# Data can be obtained via this link: 
# https://wonder.cdc.gov/ucd-icd10.html


####  SETTINGS  ####
library(tidyverse)
library(readxl)

####  DATA  ####
## Load column names, geography 
colnames <- read_excel("Shiny App Variables.xlsx", sheet = "Suicide Rates")

## Load downloaded excel files via document "Updating Suicide Rates Data.docx" and save relevant columns/observations
suiciderates_us <- read_tsv("Compressed Mortality, 1999-2016_us.txt", col_names = TRUE)
suiciderates_us <- suiciderates_us[c(1:18),c(2,4:8)]
suiciderates_ma <- read_tsv("Compressed Mortality, 1999-2016_ma.txt", col_names = TRUE)
suiciderates_ma <- suiciderates_ma[c(1:18),c(2,4,6:12)]
suiciderates_ma$State = "MA"
suiciderates_counties <- read_tsv("Compressed Mortality, 1999-2016_counties.txt", col_names = TRUE)
suiciderates_counties <- suiciderates_counties[c(1:252),c(2,4,6:12)]
suiciderates_counties$State <- "MA"
suiciderates_counties$County <- str_replace(suiciderates_counties$County, " County, MA", "")
suiciderates_counties[,c(3,5:9)] <- sapply(suiciderates_counties[,c(3,5:9)],as.numeric)

## Combine datasets
suiciderates <- bind_rows(suiciderates_us, suiciderates_ma, suiciderates_counties)
suiciderates$Country = "United States"

## Final column order and column names
suiciderates <- suiciderates[,c(10:11,7,1:3,5:6,8:9,4)]
colnames(suiciderates) <- colnames$`COLUMN NAME`

## Write new csv file to replace old in shiny app
write_csv(suiciderates, "suiciderates_update.csv")
