#########################################
## Title: Employment Update            ##
## Author(s): Valerie Evans            ##
## Date Created:  11/25/2019           ##
## Date Modified: 12/23/2019           ##
#########################################
# Data Source: http://lmi2.detma.org/lmi/town202data.asp 


####  SETTINGS  ####
library(tidyverse)
library(readxl)
library(data.table)


####  DATA  ####
colnames <- read_excel("Shiny App Variables.xlsx", sheet = "Employment")
geography <- read.csv("geography.csv", stringsAsFactors = FALSE)

## CPI file for inflation adjustments
CPI <- read.csv("CPI_2003_2018.csv", stringsAsFactors = FALSE) #if you already have a file saved elsewhere
CPI$inflation_rate <- 251.1070/CPI$average #use current year as numerator

## If you need to create a CPI dataset; you will need to register for an API key to download all years at once: https://data.bls.gov/registrationEngine/
# library(blscrapeR)
# df_cpi <- bls_api("CUSR0000SA0", startyear = 2003, endyear = 2018, 
#                   registrationKey = "a621394b12a84fe0adc76df37ca675b5", 
#                   annualaverage = TRUE)
# CPI_annual_avg <- df_cpi %>% group_by(year) %>% summarise(average = mean(value))
# CPI$inflation_rate <- 245.1342/CPI$average #use current year as numerator

## Employment Data
##Final datasets will have varying column numbers -- need to add geography columns (Municipal, County, State, Region)
##Final column names/order: Municipal, State, Year, Average_Monthly_Employment, Average_Weekly_Wage, 
#Number_of_Employer_Establishments, Total_Wages_Paid_to_All_Workers, Inflation_Adjusted_Average_Weekly_Wage, 
#Change, Employment_Change_Pct, Establishment_Change, Establishment_Change_Pct, Average_Weekly_Wage_Change, 
#Average_Weekly_Wage_Change_Pct, Employment_difference, Establishment_difference, Average_Weekly_Wage_difference

## Download excel files from website (add most recent file to end of list)
setwd("./Employment")
list <- c("http://lmi2.detma.org/lmi/data/2003towntotalEMPwagesbytownallown.xlsx", 
          "http://lmi2.detma.org/lmi/data/2004towntotalEMPwagesbytownallown.xlsx", 
          "http://lmi2.detma.org/lmi/data/2005towntotalEMPwagesbytownallown.xlsx", 
          "http://lmi2.detma.org/lmi/data/2006towntotalEMPwagesbytownallown.xlsx", 
          "http://lmi2.detma.org/lmi/data/2007towntotalEMPwagesbytownallown.xlsx", 
          "http://lmi2.detma.org/lmi/data/2008towntotalEMPwagesbytownallown.xlsx", 
          "http://lmi2.detma.org/lmi/data/2009towntotalEMPwagesbytownallown.xlsx", 
          "http://lmi2.detma.org/lmi/data/2010towntotalEMPwagesbytownallown.xlsx", 
          "http://lmi2.detma.org/lmi/data/2011towntotalEMPwagesbytownallown.xlsx", 
          "http://lmi2.detma.org/lmi/data/2012towntotalEMPwagesbytownallown.xlsx", 
          "http://lmi2.detma.org/lmi/data/2013towntotalEMPwagesbytownallown.xlsx", 
          "http://lmi2.detma.org/lmi/data/2014towntotalEMPwagesbytownallown.xlsx", 
          "http://lmi2.detma.org/lmi/data/2015towntotalEMPwagesbytownallown.xlsx", 
          "http://lmi2.detma.org/lmi/data/2016towntotalEMPwagesbytownallown.xlsx", 
          "http://lmi2.detma.org/lmi/data/2017towntotalEMPwagesbytownallown.xlsx", 
          "http://lmi2.detma.org/lmi/data/2018towntotalEMPwagesbytownallown.xlsx")
for (i in 1:length(list)) {
  download.file(list[i], destfile = paste0("Employment_Wages_", i, ".xlsx"),  mode = "wb")
}

## Create complete dataset with all downloaded files
# Get list of files
file.list <- list.files(path = ".", pattern = "*.xlsx")

# Iterate over files
dt_list <- lapply(seq_along(file.list), function(x) {
  # Read sheet 1 as data.table
  dt <- data.table(read_excel(file.list[x], sheet = 1))
  dt
})
employ <- rbindlist(dt_list, use.names = TRUE)

## Remove unnecessary columns
employment <- employ[,c(3,1,6:7,10,23)]
employment <- employment[order(employment$areaname),]

## Correct missing municipality names (mismatch with bls files and geography file)
#which(grepl("Manchester", employment$areaname)) #find specific rows with these values
employment$areaname[2633] = "Manchester-by-the-Sea" #need to check row # each time you run this file
employment$areaname[253:268] = "Attleboro" #need to check row #s each time you run this file

## Add new columns
# Add inflation adjusted average weekly wage column
employment[, "inflationweeklywage"] <- NA
for (i in 2003:2017) {
  employment$inflationweeklywage[which(employment$periodyear == i)] <- employment$avgwkwage[which(employment$periodyear == i)] * CPI$inflation_rate[i-2002]
}
employment$inflationweeklywage <- round(employment$inflationweeklywage, 0)

# Add change from 2003 (2003 = 1) and employment change percent columns
year_03 <- employment[which(employment$periodyear == 2003),]
employment$change <- employment$avgemp/year_03$avgemp[match(employment$areaname, year_03$areaname)]
##"Leyden"(2006-2017), "Savoy"(2007-2017), "Tolland"(2005-2017) so the changepct for these towns are NA
employment$change[which(employment$areaname == "Leyden")] <- employment$avgemp[which(employment$areaname == "Leyden")]/employment$avgemp[which(employment$areaname == "Leyden" & employment$periodyear == 2006)]
employment$change[which(employment$areaname == "Savoy")] <- employment$avgemp[which(employment$areaname == "Savoy")]/employment$avgemp[which(employment$areaname == "Savoy" & employment$periodyear == 2007)]
employment$change[which(employment$areaname == "Tolland")] <- employment$avgemp[which(employment$areaname == "Tolland")]/employment$avgemp[which(employment$areaname == "Tolland" & employment$periodyear == 2005)]
employment$changepct <- employment$change*100
#which(is.na(employment), arr.ind=TRUE) #determine if/where NAs are in dataset

# Add establishment change and percent columns
employment$estabchange <- employment$estab/year_03$estab[match(employment$areaname, year_03$areaname)]
##"Leyden"(2006-2017), "Savoy"(2007-2017), "Tolland"(2005-2017) so the changepct for these towns are NA
employment$estabchange[which(employment$areaname == "Leyden")] <- employment$estab[which(employment$areaname == "Leyden")]/employment$estab[which(employment$areaname == "Leyden" & employment$periodyear == 2006)]
employment$estabchange[which(employment$areaname == "Savoy")] <- employment$estab[which(employment$areaname == "Savoy")]/employment$estab[which(employment$areaname == "Savoy" & employment$periodyear == 2007)]
employment$estabchange[which(employment$areaname == "Tolland")] <- employment$estab[which(employment$areaname == "Tolland")]/employment$estab[which(employment$areaname == "Tolland" & employment$periodyear == 2005)]
employment$estabchangepct <- employment$estabchange*100
  
# Add average weekly wage change and percent columns
employment$avgweekwagechange <- employment$inflationweeklywage/year_03$inflationweeklywage[match(employment$areaname, year_03$areaname)]
##"Leyden"(2006-2017), "Savoy"(2007-2017), "Tolland"(2005-2017) so the changepct for these towns are NA
employment$avgweekwagechange[which(employment$areaname == "Leyden")] <- employment$inflationweeklywage[which(employment$areaname == "Leyden")]/employment$inflationweeklywage[which(employment$areaname == "Leyden" & employment$periodyear == 2006)]
employment$avgweekwagechange[which(employment$areaname == "Savoy")] <- employment$inflationweeklywage[which(employment$areaname == "Savoy")]/employment$inflationweeklywage[which(employment$areaname == "Savoy" & employment$periodyear == 2007)]
employment$avgweekwagechange[which(employment$areaname == "Tolland")] <- employment$inflationweeklywage[which(employment$areaname == "Tolland")]/employment$inflationweeklywage[which(employment$areaname == "Tolland" & employment$periodyear == 2005)]
employment$avgweekwagechangepct <- employment$avgweekwagechange*100
  
##Calculate the difference since 2003
employment$changediff <- round(employment$changepct-100, 1)
employment$estabdiff <- round(employment$estabchangepct-100, 1)
employment$avgweekwagediff <- round(employment$avgweekwagechangepct-100, 1)

# Add geography columns
employment <- merge(employment, geography, by.x = "areaname", by.y = "Region", all.x = TRUE)

# Final column order
employment <- employment[,c(17,19,2:16)]
  
# Update column names
colnames(employment) <- colnames$`COLUMN NAME`

## Write new csv file to replace old in shiny app
setwd("..")
write_csv(employment, "employment_update.csv")

## Compare to original dataset (there might be differences depending on rounding of inflation adjustment calculations)
setwd("..")
employdata <- read.csv("original datasets/employment.csv", stringsAsFactors = FALSE)
employdata <- employdata[,-1]
discrep <- mapply(setdiff, employment, employdata)
discrep
num.discrep <- sapply(discrep, length)
num.discrep

