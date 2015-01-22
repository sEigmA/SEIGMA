#######################################
## Title: Unemployment Data Cleaning ##
## Author(s): Emily Ramos, Arvind    ##
##            Ramakrishnan, Jenna    ##
##            Kiridly, Steve Lauer   ## 
## Date Created:  01/07/2015         ##
## Date Modified: 01/07/2015         ##
#######################################

require(sas7bdat)
require(dplyr)

## load SAS data
unemp <- read.sas7bdat("at004_01.sas7bdat")

## give columns relevant titles
column_titles <- read.csv("AT004_01_contents.csv", skip=1)
colnames(unemp)[6:12] <- as.character(column_titles$Label[1:7])

## remove unnecessary characters from column names
colnames(unemp)[6:12] <- substring(colnames(unemp[6:12]), first=12)

## grab only columns needed
unemp <- unemp[,-c(7, 12)]

#Keep only Annual average for the year
unemp_data <- unemp[which(unemp$Period=="Annual Average"),]

write.csv(unemp_data, file="unempdata2.csv")

## Replace N/A's with "NA" to remove the slash.
unemp_data2 <- unemp_data
unemp_data2$Region <- replace(unemp_data2$Region,unemp_data2$Region=="N/A", NA)
unemp_data2$County <- replace(unemp_data2$County,unemp_data2$County=="N/A", NA)
unemp_data2$State <- replace(unemp_data2$State,unemp_data2$State=="N/A", NA)
unemp_data2$Municipal <- replace(unemp_data2$Municipal,unemp_data2$Municipal=="N/A", NA)

unemp_data2$Region  <- ifelse(!is.na(unemp_data2$Region), "United States", "MA")
unemp_data2$Region  <- ifelse(!is.na(unemp_data2$County), paste(unemp_data2$County, "County"), unemp_data2$Region)
unemp_data2$Region  <- ifelse(!is.na(unemp_data2$Municipal), as.character(unemp_data2$Municipal), as.character(unemp_data2$Region))

## Add "County" to the end of each County name
unemp_data2$County <- paste(unemp_data2$County, "County")

# grab only the columns that we want: 
unemp_data3 <- unemp_data2[,-6]

## save and reload to make factors into numeric (this is faster than the other methods)
write.csv(unemp_data3, file="unempdata.csv")
unemp_data3 <- read.csv("unempdata.csv")[,-c(1)]

colnames(unemp_data3)[c(5:9)] <- c("Year", "Unemployment Rate Avg", "No Unemployed Avg", "No Employed Avg", "No Labor Avg")

#Organizing by Region
unemp_data4 <- unemp_data3 %>%
  arrange(Region)

# Put US and MA at top of data set
idx_unemp_MA <- which(unemp_data4$Region == "MA")
idx_unemp_US <- which(unemp_data4$Region == "United States")
unemp_data4 <- rbind.data.frame(unemp_data4[idx_unemp_US,], unemp_data4[idx_unemp_MA,], unemp_data4[-c(idx_unemp_MA, idx_unemp_US),])

unemp_data4 <- unemp_data4[-c(1:759),]
  
## save data
write.csv(unemp_data4, file="unemploy/unempdata.csv")
