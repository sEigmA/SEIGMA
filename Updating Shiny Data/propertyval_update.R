#######################################
## Title: Property Value Update      ##
## Author(s): Valerie Evans          ## 
## Date Created:  12/04/2019         ##
## Date Modified: 12/23/2019         ##
#######################################
# Data can be obtained via this link: 
# https://dlsgateway.dor.state.ma.us/reports/rdPage.aspx?rdReport=PropertyTaxInformation.AssessedValuesbyClass.assessedvaluesbyclass


####  SETTINGS  ####
library(tidyverse)
library(readxl)


####  DATA  ####
## Load column names, geography 
colnames <- read_excel("Shiny App Variables.xlsx", sheet = "Property Value")
geography <- read.csv("geography.csv", stringsAsFactors = FALSE)
geography <- geography[!grepl(" County", geography$Region),] #remove counties from region column
geography <- geography[-1,] #remove US from region column
CPI <- read.csv("CPI_2003_2018.csv", stringsAsFactors = FALSE) #if you already have a file saved elsewhere
CPI$inflation_rate <- 251.1070/CPI$average #use current year as numerator

## If you need to create a CPI dataset; you will need to register for an API key to download all years at once: https://data.bls.gov/registrationEngine/
# library(blscrapeR)
# #You'll need to register for an API key to download all years at once: https://data.bls.gov/registrationEngine/
# df_cpi <- bls_api("CUSR0000SA0", startyear = 2003, endyear = 2018, 
#                   registrationKey = "a621394b12a84fe0adc76df37ca675b5", 
#                   annualaverage = TRUE)
# CPI_annual_avg <- df_cpi %>% group_by(year) %>% summarise(average = mean(value))
# CPI$inflation_rate <- 251.1070/CPI$average #use current year as numerator

## Load downloaded excel files (downloaded .xls files may need to be resaved as .xlsx files to allow import)
assessedvalues_ma <- read_excel("assessedvalues_st_2003_2018.xlsx")
assessedvalues_ma$Municipal = NA
assessedvalues_muni <- read_excel("assessedvalues_2003_2018.xlsx")
assessedvalues_muni <- assessedvalues_muni[,-1]
colnames(assessedvalues_muni)[1] <- "Municipal"
assessedvalues_muni$Municipal[assessedvalues_muni$Municipal == "Manchester By The Sea"] <- "Manchester-by-the-Sea"

## Combine datasets and add geography
assessedvalues <- bind_rows(assessedvalues_ma, assessedvalues_muni)
assessedvalues$State = "MA"
assessedvalues <- left_join(assessedvalues, geography)

## Calculate inflation-adjusted residential property values
assessedvalues$inflation_residential <- NA
for (i in 2003:2018) {
  assessedvalues$inflation_residential[which(assessedvalues$`Fiscal Year` == i)] <- assessedvalues$Residential[which(assessedvalues$`Fiscal Year` == i)] * CPI$inflation_rate[i-2002]
}
assessedvalues$inflation_residential <- round(assessedvalues$inflation_residential, 0)

## Calculate inflation-adjusted open space property values
assessedvalues$inflation_openspace <- NA
for (i in 2003:2018) {
  assessedvalues$inflation_openspace[which(assessedvalues$`Fiscal Year` == i)] <- assessedvalues$`Open Space`[which(assessedvalues$`Fiscal Year` == i)] * CPI$inflation_rate[i-2002]
}
assessedvalues$inflation_openspace <- round(assessedvalues$inflation_openspace, 0)

## Calculate inflation-adjusted commercial property values
assessedvalues$inflation_commercial <- NA
for (i in 2003:2018) {
  assessedvalues$inflation_commercial[which(assessedvalues$`Fiscal Year` == i)] <- assessedvalues$Commercial[which(assessedvalues$`Fiscal Year` == i)] * CPI$inflation_rate[i-2002]
}
assessedvalues$inflation_commercial <- round(assessedvalues$inflation_commercial, 0)

## Calculate inflation-adjusted industrial property values
assessedvalues$inflation_industrial <- NA
for (i in 2003:2018) {
  assessedvalues$inflation_industrial[which(assessedvalues$`Fiscal Year` == i)] <- assessedvalues$Industrial[which(assessedvalues$`Fiscal Year` == i)] * CPI$inflation_rate[i-2002]
}
assessedvalues$inflation_industrial <- round(assessedvalues$inflation_industrial, 0)

## Calculate inflation-adjusted personal property values
assessedvalues$inflation_personal <- NA
for (i in 2003:2018) {
  assessedvalues$inflation_personal[which(assessedvalues$`Fiscal Year` == i)] <- assessedvalues$`Personal Property`[which(assessedvalues$`Fiscal Year` == i)] * CPI$inflation_rate[i-2002]
}
assessedvalues$inflation_personal <- round(assessedvalues$inflation_personal, 0)

## Calculate inflation-adjusted total assessed values
assessedvalues$inflation_total <- NA
for (i in 2003:2018) {
  assessedvalues$inflation_total[which(assessedvalues$`Fiscal Year` == i)] <- assessedvalues$Total[which(assessedvalues$`Fiscal Year` == i)] * CPI$inflation_rate[i-2002]
}
assessedvalues$inflation_total <- round(assessedvalues$inflation_total, 0)

## Calculate percent of assessed by class
assessedvalues$pct_residential <- round(assessedvalues$Residential/assessedvalues$Total * 100, 2)
assessedvalues$pct_openspace <-round(assessedvalues$`Open Space`/assessedvalues$Total * 100, 2)
assessedvalues$pct_commercial <-round(assessedvalues$Commercial/assessedvalues$Total * 100, 2)
assessedvalues$pct_industrial <-round(assessedvalues$Industrial/assessedvalues$Total * 100, 2)
assessedvalues$pct_personal <-round(assessedvalues$`Personal Property`/assessedvalues$Total * 100, 2)

## Calculate total assessed value percent change since 2003
year_2003 <- assessedvalues[which(assessedvalues$`Fiscal Year` == 2003),]
assessedvalues$total_pct_change <- round((assessedvalues$inflation_total/year_2003$inflation_total[match(assessedvalues$Municipal, year_2003$Municipal)]-1) * 100, 1)

## Put the label as x million
assessedvalues$total_million <- round(assessedvalues$inflation_total/1000000, 2)
assessedvalues$residential_million <- round(assessedvalues$inflation_residential/1000000, 2)
assessedvalues$openspace_million <- round(assessedvalues$inflation_openspace/1000000, 2)
assessedvalues$commercial_million <- round(assessedvalues$inflation_commercial/1000000, 2)
assessedvalues$industrial_million <- round(assessedvalues$inflation_industrial/1000000, 2)
assessedvalues$personal_million <- round(assessedvalues$inflation_personal/1000000, 2)

## Final column order and column names
assessedvalues <- assessedvalues[,c(10,12,11,1:9,14:31)]
colnames(assessedvalues) <- colnames$`COLUMN NAME`

## Write new csv file to replace old in shiny app
write_csv(assessedvalues, "propertyvalue_update.csv")

## Compare to original dataset 
propval <- read.csv("original datasets/propertyvalue.csv", stringsAsFactors = FALSE)
propvalues_muni <- assessedvalues[-c(1:16),]
discrep <- mapply(setdiff, assessedvalues, propval)
discrep
num.discrep <- sapply(discrep, length)
num.discrep
#which(grepl(xxxx, propvalues_muni$County))
#propvalues_muni[!complete.cases(propvalues_muni$County), ]
