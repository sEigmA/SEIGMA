###########################################
## Title: Bankruptcy Data Update         ##
## Author(s): Valerie Evans              ## 
## Date Created:  11/09/2017             ##
## Date Modified: 09/18/2020             ##
###########################################
# Data can be downloaded through a search for tables F-5A ending December 31st via this link: 
# http://www.uscourts.gov/statistics-reports/caseload-statistics-data-tables/

setwd("~/Documents/R/SEIGMA/Updating Shiny Data/bankruptcy")

####  SETTINGS  ####
library(tidyverse)
library(readxl)

####  DATA  ####
## Load column names, geography 
colnames <- read_excel("Shiny App Variables.xlsx", sheet = "Bankruptcy")
colnames_all <- colnames[c(1:16),]
colnames_2018a <- colnames[c(1:4,6:9,11:15),]
colnames_2018b <- colnames[c(1:2,5),]
colnames_2019a <- colnames[c(1:4,6:9,11:15),]
colnames_2019b <- colnames[c(1:2,5),]

## Load downloaded excel files
# bankruptcy_2013 <- read_excel("1213_f5a.xls", skip = 1)
bankruptcy_2014 <- read_excel("1214_f5a.xls", skip = 1)
bankruptcy_2015 <- read_excel("table_f-_5a_filings_dec_2015_0.xls", skip = 1)
bankruptcy_2016 <- read_excel("bf_f5a_1231.2016.xlsx", skip = 1)
bankruptcy_2017 <- read_excel("bf_f5a_1231.2017.xlsx", skip = 1)
bankruptcy_2018a <- read_excel("bf_f5a_1231.2018.xlsx", sheet = 1, skip = 2)
bankruptcy_2018b <- read_excel("bf_f5a_1231.2018.xlsx", sheet = 2, skip = 2)
bankruptcy_2019a <- read_excel("bf_f5a_1231.2019.xlsx", sheet = 1, skip = 2)
bankruptcy_2019b <- read_excel("bf_f5a_1231.2019.xlsx", sheet = 2, skip = 2)

## Select relevant columns and rows
# bankruptcy_2013 <- bankruptcy_2013[c(1,3,51,62:75), c(1,3:16)]
bankruptcy_2014 <- bankruptcy_2014[c(1:2,50,60:73), c(1,3:16)]
bankruptcy_2015 <- bankruptcy_2015[c(1,3,52,59:72), c(1,3:16)]
bankruptcy_2016 <- bankruptcy_2016[c(1,3,43,53:66), c(1,3:16)]
bankruptcy_2017 <- bankruptcy_2017[c(1,3,46,50:63), c(1,3:16)]
bankruptcy_2018a <- bankruptcy_2018a[c(1:3,40,45:58), c(1,3:6,8:11,13:16)]
bankruptcy_2018b <- bankruptcy_2018b[c(3,40,45:58), c(1,3,5)]
bankruptcy_2019a <- bankruptcy_2019a[c(1:3,40,45:58), c(1,3:6,8:11,13:16)]
bankruptcy_2019b <- bankruptcy_2019b[c(3,40,45:58), c(1,3,5)]

## Add MA and US labels
#bankruptcy_2013[2,1] = "United States"
#bankruptcy_2013[3,1] = "MA"
#bankruptcy_2013$Year = "2013"
bankruptcy_2014[2,1] = "United States"
bankruptcy_2014[3,1] = "MA"
bankruptcy_2014$Year = "2014"
bankruptcy_2015[2,1] = "United States"
bankruptcy_2015[3,1] = "MA"
bankruptcy_2015$Year = "2015"
bankruptcy_2016[2,1] = "United States"
bankruptcy_2016$Year = "2016"
bankruptcy_2017[2,1] = "United States"
bankruptcy_2017$Year = "2017"
bankruptcy_2018a[3,1] <- "United States"
bankruptcy_2018b[1,1] <- "United States"
bankruptcy_2019a[3,1] <- "United States"
bankruptcy_2019b[1,1] <- "United States"

## Clean 2018 datasets, duplicate chapter 12 filings for business chapter 12, merge, add year column, and make sure all data columns are numeric
colnames(bankruptcy_2018a) <- colnames_2018a$`COLUMN NAME`
bankruptcy_2018a <- bankruptcy_2018a[-c(1:2),]
colnames(bankruptcy_2018b) <- colnames_2018b$`COLUMN NAME`
bankruptcy_2018b$Business_Filings_Chapter_12 = bankruptcy_2018b$All_Filings_Chapter_12 
bankruptcy_2018 <- merge(bankruptcy_2018a, bankruptcy_2018b)
bankruptcy_2018$Year = "2018"
bankruptcy_2018[,2:15] <- sapply(bankruptcy_2018[,2:15], as.numeric)

## Clean 2019 datasets, duplicate chapter 12 filings for business chapter 12, merge, add year column, and make sure all data columns are numeric
colnames(bankruptcy_2019a) <- colnames_2019a$`COLUMN NAME`
bankruptcy_2019a <- bankruptcy_2019a[-c(1:2),]
colnames(bankruptcy_2019b) <- colnames_2019b$`COLUMN NAME`
bankruptcy_2019b$Business_Filings_Chapter_12 = bankruptcy_2019b$All_Filings_Chapter_12 
bankruptcy_2019 <- merge(bankruptcy_2019a, bankruptcy_2019b)
bankruptcy_2019$Year = "2019"
bankruptcy_2019[,2:15] <- sapply(bankruptcy_2019[,2:15], as.numeric)

## Merge all datasets (except 2018) and make sure all data columns are numeric
bankruptcy_all <- bind_rows(bankruptcy_2014, bankruptcy_2015, bankruptcy_2016, bankruptcy_2017)
colnames(bankruptcy_all) <- colnames_all$`COLUMN NAME`
bankruptcy_all <- bankruptcy_all[!is.na(bankruptcy_all$Region),]
bankruptcy_all[,2:15] <- sapply(bankruptcy_all[,2:15], as.numeric)

## Merge all datasets with 2018 
bankruptcy <- rbind(bankruptcy_all, bankruptcy_2018)

## Merge all datasets with 2019 
bankruptcy <- rbind(bankruptcy, bankruptcy_2019)

## Add percentage columns
bankruptcy$Percentage_of_Chapter_7_in_Business_Filings <- round(bankruptcy$Business_Filings_Chapter_7/bankruptcy$Business_Filings_Total*100, 1)
bankruptcy$Percentage_of_Chapter_11_in_Business_Filings <- round(bankruptcy$Business_Filings_Chapter_11/bankruptcy$Business_Filings_Total*100, 1)
bankruptcy$Percentage_of_Chapter_12_in_Business_Filings <- round(bankruptcy$Business_Filings_Chapter_12/bankruptcy$Business_Filings_Total*100, 1)
bankruptcy$Percentage_of_Chapter_13_in_Business_Filings <- round(bankruptcy$Business_Filings_Chapter_13/bankruptcy$Business_Filings_Total*100, 1)
bankruptcy$Percentage_of_Chapter_7_in_Personal_Filings <- round(bankruptcy$Personal_Filings_Chapter_7/bankruptcy$Personal_Filings_Total*100, 1)
bankruptcy$Percentage_of_Chapter_11_in_Personal_Filings <- round(bankruptcy$Personal_Filings_Chapter_11/bankruptcy$Personal_Filings_Total*100, 1)
bankruptcy$Percentage_of_Chapter_13_in_Personal_Filings <- round(bankruptcy$Personal_Filings_Chapter_13/bankruptcy$Personal_Filings_Total*100, 1)

## Rename counties
bankruptcy$Region <- bankruptcy$Region %>% 
  str_replace("BARNSTABLE", "Barnstable County") %>% 
  str_replace("BERKSHIRE", "Berkshire County") %>% 
  str_replace("BRISTOL", "Bristol County") %>% 
  str_replace("DUKES", "Dukes County") %>% 
  str_replace("ESSEX", "Essex County") %>% 
  str_replace("FRANKLIN", "Franklin County") %>% 
  str_replace("HAMPDEN", "Hampden County") %>% 
  str_replace("HAMPSHIRE", "Hampshire County") %>% 
  str_replace("MIDDLESEX", "Middlesex County") %>% 
  str_replace("NANTUCKET", "Nantucket County") %>% 
  str_replace("NORFOLK", "Norfolk County") %>% 
  str_replace("PLYMOUTH", "Plymouth County") %>% 
  str_replace("SUFFOLK", "Suffolk County") %>% 
  str_replace("WORCESTER", "Worcester County")

## Write new csv file to replace old in shiny app
write_csv(bankruptcy, "bankruptcy_update.csv")

