#######################################
## Title: Property Tax Update        ##
## Author(s): Valerie Evans          ## 
## Date Created:  12/06/2019         ##
## Date Modified: 12/23/2019         ##
#######################################
# Data can be obtained via this link: 
# https://dlsgateway.dor.state.ma.us/reports/rdPage.aspx?rdReport=Dashboard.TrendAnalysisReports.TaxLevyByClass


####  SETTINGS  ####
library(tidyverse)
library(readxl)


####  DATA  ####
## Load column names, geography 
colnames <- read_excel("Shiny App Variables.xlsx", sheet = "Property Tax")
CPI <- read.csv("CPI_2003_2018.csv", stringsAsFactors = FALSE) #if you already have a file saved elsewhere
CPI$inflation_rate <- 251.1070/CPI$average #use current year as numerator

## If you need to create a CPI dataset; you will need to register for an API key to download all years at once: https://data.bls.gov/registrationEngine/
# library(blscrapeR)
# #You'll need to register for an API key to download all years at once: https://data.bls.gov/registrationEngine/
# df_cpi <- bls_api("CUSR0000SA0", startyear = 2003, endyear = 2018, 
#                   registrationKey = "a621394b12a84fe0adc76df37ca675b5", 
#                   annualaverage = TRUE)
# CPI_annual_avg <- df_cpi %>% group_by(year) %>% summarise(average = mean(value))
# CPI$inflation_rate <- 245.1342/CPI$average #use current year as numerator

## Load downloaded tax levy excel file (downloaded .xls files may need to be resaved as .xlsx files to allow import)
taxlevy <- read_excel("Taxlevybyclass_2003_2018.xlsx")
taxlevy <- taxlevy[,-c(1,10:11)]

## Load revenue by source excel file to obtain "Total Budget" (i.e. Total.Receipts) column (downloaded .xls files may need to be resaved as .xlsx files to allow import)
totalbudget <- read_excel("revenuebysource_2003_2018.xlsx")
totalbudget <- totalbudget[,c(2,3,8)]

## Merge propertytax with totalbudget and fix municipality name
propertytax <- merge(taxlevy, totalbudget)
colnames(propertytax)[1] <- "Municipal"
propertytax$Municipal[propertytax$Municipal == "Manchester By The Sea"] <- "Manchester-by-the-Sea"

## Calculate inflation-adjusted residential property values
propertytax$inflation_residential <- NA
for (i in 2003:2018) {
  propertytax$inflation_residential[which(propertytax$`Fiscal Year` == i)] <- propertytax$Residential[which(propertytax$`Fiscal Year` == i)] * CPI$inflation_rate[i-2002]
}
propertytax$inflation_residential <- round(propertytax$inflation_residential, 0)

## Calculate inflation-adjusted open space property values
propertytax$inflation_openspace <- NA
for (i in 2003:2018) {
  propertytax$inflation_openspace[which(propertytax$`Fiscal Year` == i)] <- propertytax$`Open Space`[which(propertytax$`Fiscal Year` == i)] * CPI$inflation_rate[i-2002]
}
propertytax$inflation_openspace <- round(propertytax$inflation_openspace, 0)

## Calculate inflation-adjusted commercial property values
propertytax$inflation_commercial <- NA
for (i in 2003:2018) {
  propertytax$inflation_commercial[which(propertytax$`Fiscal Year` == i)] <- propertytax$Commercial[which(propertytax$`Fiscal Year` == i)] * CPI$inflation_rate[i-2002]
}
propertytaxs$inflation_commercial <- round(propertytax$inflation_commercial, 0)

## Calculate inflation-adjusted industrial property values
propertytax$inflation_industrial <- NA
for (i in 2003:2018) {
  propertytax$inflation_industrial[which(propertytax$`Fiscal Year` == i)] <- propertytax$Industrial[which(propertytax$`Fiscal Year` == i)] * CPI$inflation_rate[i-2002]
}
propertytax$inflation_industrial <- round(propertytax$inflation_industrial, 0)

## Calculate inflation-adjusted personal property values
propertytax$inflation_personal <- NA
for (i in 2003:2018) {
  propertytax$inflation_personal[which(propertytax$`Fiscal Year` == i)] <- propertytax$`Personal Property`[which(propertytax$`Fiscal Year` == i)] * CPI$inflation_rate[i-2002]
}
propertytax$inflation_personal <- round(propertytax$inflation_personal, 0)

## Calculate inflation-adjusted total assessed values
propertytax$inflation_total <- NA
for (i in 2003:2018) {
  propertytax$inflation_total[which(propertytax$`Fiscal Year` == i)] <- propertytax$`Total Levy`[which(propertytax$`Fiscal Year` == i)] * CPI$inflation_rate[i-2002]
}
propertytax$inflation_total <- round(propertytax$inflation_total, 0)

## Calculate percent of levy by class
propertytax$pct_residential <- round(propertytax$Residential/propertytax$`Total Levy` * 100, 2)
propertytax$pct_openspace <-round(propertytax$`Open Space`/propertytax$`Total Levy` * 100, 2)
propertytax$pct_commercial <-round(propertytax$Commercial/propertytax$`Total Levy` * 100, 2)
propertytax$pct_industrial <-round(propertytax$Industrial/propertytax$`Total Levy` * 100, 2)
propertytax$pct_personal <-round(propertytax$`Personal Property`/propertytax$`Total Levy` * 100, 2)

## Calculate levy percent change since 2003
year_2003 <- propertytax[which(propertytax$`Fiscal Year` == 2003),]
propertytax$total_pct_change <- round((propertytax$inflation_total/year_2003$inflation_total[match(propertytax$Municipal, year_2003$Municipal)]-1) * 100, 1)

## Put the label as x million
propertytax$total_million <- round(propertytax$inflation_total/1000000, 2)
propertytax$residential_million <- round(propertytax$inflation_residential/1000000, 2)
propertytax$openspace_million <- round(propertytax$inflation_openspace/1000000, 2)
propertytax$commercial_million <- round(propertytax$inflation_commercial/1000000, 2)
propertytax$industrial_million <- round(propertytax$inflation_industrial/1000000, 2)
propertytax$personal_million <- round(propertytax$inflation_personal/1000000, 2)

## Final column order and column names
propertytax <- propertytax[,c(1:2,9,3:8,10:27)]
colnames(propertytax) <- colnames$`COLUMN NAME`

## Write new csv file to replace old in shiny app
write_csv(propertytax, "propertytax_update.csv")

## Compare to original dataset 
propval <- read.csv("original datasets/propertyvalue.csv", stringsAsFactors = FALSE)
propvalues_muni <- assessedvalues[-c(1:16),]
discrep <- mapply(setdiff, assessedvalues, propval)
discrep
num.discrep <- sapply(discrep, length)
num.discrep
#which(grepl(xxxx, propvalues_muni$County))
#propvalues_muni[!complete.cases(propvalues_muni$County), ]
