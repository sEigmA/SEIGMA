#######################################
## Title: Veteran Data Cleaning      ##
## Author(s): Emily Ramos, Arvind    ##
##            Ramakrishnan, Jenna    ##
##            Kiridly, Steve Lauer   ## 
## Date Created:  11/20/2014         ##
## Date Modified: 12/04/2014         ##
#######################################

setwd("va_status")
require(sas7bdat)
require(dplyr)

## load SAS data
va <- read.sas7bdat("ba002_01_5yr.sas7bdat")

## give columns relevant titles
column_titles <- read.csv("BA002_01_5yr_contents.csv", skip=1)
colnames(va)[5:127] <- as.character(column_titles$Label[1:123])

## remove unnecessary characters from column names
colnames(va)[5:127] <- substring(colnames(va[5:127]), first=12)

## grab only columns involving veterans status
va_cols <- grep("VETERAN STATUS", x = colnames(va))
va_data <- va[,c(1:5, va_cols)]

## Replace N/A's with "NA" to remove the slash.
va_data2 <- va_data
va_data2$Region <- replace(va_data2$Region,va_data2$Region=="N/A", NA)
va_data2$County <- replace(va_data2$County,va_data2$County=="N/A", NA)
va_data2$State <- replace(va_data2$State,va_data2$State=="N/A", NA)
va_data2$Municipal <- replace(va_data2$Municipal,va_data2$Municipal=="N/A", NA)

va_data2$Region  <- ifelse(!is.na(va_data2$Region), "United States", "MA")
va_data2$Region  <- ifelse(!is.na(va_data2$County), paste(va_data2$County, "County"), va_data2$Region)
va_data2$Region  <- ifelse(!is.na(va_data2$Municipal),as.character(va_data2$Municipal),as.character(va_data2$Region))

## Add "County" to the end of each County name
va_data2$County <- paste(va_data2$County, "County")

## Take only key variables
va_data3 <- va_data2[, c(1:6, 9, 11:12)]

## save and reload to make factors into numeric (this is faster than the other methods)
write.csv(va_data3, file="vetstatusdata.csv")
va_data3 <- read.csv("vetstatusdata.csv")[,-c(1)]

colnames(va_data3)[c(5:9)] <- c("Five_Year_Range", "Civilian_Pop", "Vet_Pop", "Percent_Vet", "Margin_Error_Percent")

# Organizing by Region
va_data4 <- va_data3 %>%
  arrange(Region)

# Put US and MA at top of data set
idx_edu_MA <- which(va_data4$Region == "MA")
idx_edu_US <- which(va_data4$Region == "United States")
va_data4 <- rbind.data.frame(va_data4[idx_edu_US,],
                              va_data4[idx_edu_MA,], va_data4[-c(idx_edu_MA, idx_edu_US),])

# Save data
write.csv(va_data4, file="va/vetstatusdata.csv")
