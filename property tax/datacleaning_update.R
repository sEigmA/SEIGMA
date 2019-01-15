##############################################
## Title: Property Tax Data Cleaning_Update ##
## Author(s): Valerie Evans                 ## 
## Date Created:   04/25/2018               ##
## Date Modified:  01/14/2019               ##
##############################################


## load libraries
library(dplyr)
library(readxl)

## set working directory
setwd("J:/Projects/Gambling/SEIGMA/Data Management Center/SECONDARY DATA/ACTIVE/SHINY DATA/Economic/Property Tax/")


#### Create Updated Data File ####

## import new data files 2014-2017 removing columns
tax_update_14 <- read.csv("taxlevybyclass_2014.csv")[,2:9]
tax_update_15 <- read.csv("taxlevybyclass_2015.csv")[,2:9]
tax_update_16 <- read.csv("taxlevybyclass_2016.csv")[,2:9]
tax_update_17 <- read.csv("taxlevybyclass_2017.csv")[,2:9]
tax_update_18 <- read.csv("taxlevybyclass_2018.csv")[,2:9]

## merge new data files
tax_update_14_18 <- rbind(tax_update_14, tax_update_15, tax_update_16, tax_update_17, tax_update_18)

## give columns relevant titles 
colnames(tax_update_14_18) <- c("Municipal","Fiscal_Year","Residential","Open_Space","Commercial","Industrial","Personal_Property","Total_Levy")

## select "Total Budget" (i.e. Total.Receipts) column from "Revenue By Source" file
revenue_14_18 <- read.csv("RevenueBySource_2003-2018.csv")[, 2:8]
revenue_14_18[,3:6] <- NULL

## filter for 2014-2018 only
years <- c(2014, 2015, 2016, 2017, 2018)
revenue_14_18 <- revenue_14_18 %>% filter(Fiscal.Year %in% years)
colnames(revenue_14_18) <- c("Municipal", "Fiscal_Year", "Total_Budget")

## merge update with revenue and update column order
tax_update_14_18_2 <- merge(tax_update_14_18, revenue_14_18, by = c("Municipal", "Fiscal_Year"))
tax_update_14_18_2 <- tax_update_14_18_2[,c(1, 2, 9, 3, 4, 5, 6, 7, 8)]
tax_update_14_18_2[, 3:9] <- lapply(3:9, function(x) as.numeric(tax_update_14_18_2[[x]]))

## read in archived data file 2003-2013 and updating column names
tax <- read.csv("taxdata1.csv")
colnames(tax) <- c("Municipal","Fiscal_Year","Total_Budget", "Residential","Open_Space","Commercial","Industrial","Personal_Property","Total_Levy")
tax[, 3:9] <- lapply(3:9, function(x) as.numeric(tax[[x]]))

## merge new and archived data files
tax_update <- rbind(tax_update_14_18_2, tax)

## order the dataset by region, year
tax_update <- tax_update[order(tax_update$Municipal,tax_update$Fiscal_Year),]

## save updated file
write.csv(tax_update, file="tax_update_18.csv",row.names=FALSE)


#### Calculate Inflation Adjustments ####

## calculate the inflation adjusted Residential
Adjusted_index <- data.frame(Year=2003:2018, Annual=c(184.0, 188.9, 195.3, 201.6, 207.342, 215.303, 214.537, 218.056, 224.939, 229.594, 232.957, 236.736, 237.017, 210.007, 245.120, 251.107))
Adjusted_index$Inflation_rate <- 251.107/Adjusted_index$Annual
tax_update$Inflation_Adjusted_Residential <- rep(0,5616)
for (i in 2003:2018) {
  tax_update$Inflation_Adjusted_Residential[which(tax_update$Fiscal_Year==i)] <- tax_update$Residential[which(tax_update$Fiscal_Year==i)]*Adjusted_index$Inflation_rate[i-2002]
}
tax_update$Inflation_Adjusted_Residential <- round(tax_update$Inflation_Adjusted_Residential,0)

## calculate the inflation adjusted Open Space
Adjusted_index <- data.frame(Year=2003:2018, Annual=c(184.0, 188.9, 195.3, 201.6, 207.342, 215.303, 214.537, 218.056, 224.939, 229.594, 232.957, 236.736, 237.017, 210.007, 245.120, 251.107))
Adjusted_index$Inflation_rate <- 251.107/Adjusted_index$Annual
tax_update$Inflation_Adjusted_Open_Space <- rep(0,5616)
for (i in 2003:2018) {
  tax_update$Inflation_Adjusted_Open_Space[which(tax_update$Fiscal_Year==i)] <- tax_update$Open_Space[which(tax_update$Fiscal_Year==i)]*Adjusted_index$Inflation_rate[i-2002]
}
tax_update$Inflation_Adjusted_Open_Space <- round(tax_update$Inflation_Adjusted_Open_Space,0)

## calculate the inflation adjusted Commerical
Adjusted_index <- data.frame(Year=2003:2018, Annual=c(184.0, 188.9, 195.3, 201.6, 207.342, 215.303, 214.537, 218.056, 224.939, 229.594, 232.957, 236.736, 237.017, 210.007, 245.120, 251.107))
Adjusted_index$Inflation_rate <- 251.107/Adjusted_index$Annual
tax_update$Inflation_Adjusted_Commercial <- rep(0,5616)
for (i in 2003:2018) {
  tax_update$Inflation_Adjusted_Commercial[which(tax_update$Fiscal_Year==i)] <- tax_update$Commercial[which(tax_update$Fiscal_Year==i)]*Adjusted_index$Inflation_rate[i-2002]
}
tax_update$Inflation_Adjusted_Commercial <- round(tax_update$Inflation_Adjusted_Commercial,0)

## calculate the inflation adjusted Industrial
Adjusted_index <- data.frame(Year=2003:2018, Annual=c(184.0, 188.9, 195.3, 201.6, 207.342, 215.303, 214.537, 218.056, 224.939, 229.594, 232.957, 236.736, 237.017, 210.007, 245.120, 251.107))
Adjusted_index$Inflation_rate <- 251.107/Adjusted_index$Annual
tax_update$Inflation_Adjusted_Industrial <- rep(0,5616)
for (i in 2003:2018) {
  tax_update$Inflation_Adjusted_Industrial[which(tax_update$Fiscal_Year==i)] <- tax_update$Industrial[which(tax_update$Fiscal_Year==i)]*Adjusted_index$Inflation_rate[i-2002]
}
tax_update$Inflation_Adjusted_Industrial <- round(tax_update$Inflation_Adjusted_Industrial,0)

## calculate the inflation adjusted Personal Property
Adjusted_index <- data.frame(Year=2003:2018, Annual=c(184.0, 188.9, 195.3, 201.6, 207.342, 215.303, 214.537, 218.056, 224.939, 229.594, 232.957, 236.736, 237.017, 210.007, 245.120, 251.107))
Adjusted_index$Inflation_rate <- 251.107/Adjusted_index$Annual
tax_update$Inflation_Adjusted_Personal_Property <- rep(0,5616)
for (i in 2003:2018) {
  tax_update$Inflation_Adjusted_Personal_Property[which(tax_update$Fiscal_Year==i)] <- tax_update$Personal_Property[which(tax_update$Fiscal_Year==i)]*Adjusted_index$Inflation_rate[i-2002]
}
tax_update$Inflation_Adjusted_Personal_Property <- round(tax_update$Inflation_Adjusted_Personal_Property,0)

## calculate the inflation adjusted Total Levy
Adjusted_index <- data.frame(Year=2003:2018, Annual=c(184.0, 188.9, 195.3, 201.6, 207.342, 215.303, 214.537, 218.056, 224.939, 229.594, 232.957, 236.736, 237.017, 210.007, 245.120, 251.107))
Adjusted_index$Inflation_rate <- 251.107/Adjusted_index$Annual
tax_update$Inflation_Adjusted_Total_Levy <- rep(0,5616)
for (i in 2003:2018) {
  tax_update$Inflation_Adjusted_Total_Levy[which(tax_update$Fiscal_Year==i)] <- tax_update$Total_Levy[which(tax_update$Fiscal_Year==i)]*Adjusted_index$Inflation_rate[i-2002]
}
tax_update$Inflation_Adjusted_Total_Levy <- round(tax_update$Inflation_Adjusted_Total_Levy,0)

## save updated file with inflation adjustment
write.csv(tax_update, file="tax_update2.csv",row.names=FALSE)


#### Calculate Percent Levy ####

## calculate the Percent of Levy by Class
tax_update$Percentage_of_Residential <- round(tax_update$Residential/tax_update$Total_Levy*100, 2)
tax_update$Percentage_of_Open_Space <- round(tax_update$Open_Space/tax_update$Total_Levy*100, 2)
tax_update$Percentage_of_Commercial <- round(tax_update$Commercial/tax_update$Total_Levy*100, 2)
tax_update$Percentage_of_Industrial <- round(tax_update$Industrial/tax_update$Total_Levy*100, 2)
tax_update$Percentage_of_Personal_Property <- round(tax_update$Personal_Property/tax_update$Total_Levy*100, 2)

## save updated file with percent of levy 
write.csv(tax_update, file="taxdata_update.csv",row.names=FALSE)

## calculate the Total levy percent change since 2003
year_2003 <- tax_update[which(tax_update$Fiscal_Year==2003),]
tax_update$Total_Levy_Pct_Change <- round((tax_update$Inflation_Adjusted_Total_Levy/year_2003$Inflation_Adjusted_Total_Levy[match(tax_update$Municipal,year_2003$Municipal)]-1)*100,1)

## save updated file with levy percent change  
write.csv(tax_update, file="taxdata2_update.csv",row.names=FALSE)


#### Change to Millions ####

## put the label as x million
tax_update$Total_Levy_Million <- round(tax_update$Inflation_Adjusted_Total_Levy/1000000, 2)
tax_update$Residential_Million <- round(tax_update$Inflation_Adjusted_Residential/1000000, 2)
tax_update$Open_Space_Million <- round(tax_update$Inflation_Adjusted_Open_Space/1000000, 2)
tax_update$Commercial_Million <- round(tax_update$Inflation_Adjusted_Commercial/1000000, 2)
tax_update$Industrial_Million <- round(tax_update$Inflation_Adjusted_Industrial/1000000, 2)
tax_update$Personal_Property_Million <- round(tax_update$Inflation_Adjusted_Personal_Property/1000000, 2)

## save updated file with columns adjusted to millions 
write.csv(tax_update, file="taxdata2_update.csv",row.names=FALSE)
