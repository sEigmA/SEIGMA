#######################################
## Title: Labor Data Cleaning ##
## Author(s): Emily Ramos, Arvind    ##
##            Ramakrishnan, Jenna    ##
##            Kiridly, Steve Lauer   ## 
## Date Created:  02/22/2015         ##
## Date Modified: 02/22/2015         ##
#######################################

require(sas7bdat)
require(dplyr)

## load SAS data
labor <- read.sas7bdat("at004_01.sas7bdat")

## give columns relevant titles
column_titles <- read.csv("AT004_01_contents.csv", skip=1)
colnames(labor)[6:12] <- as.character(column_titles$Label[1:7])

## remove unnecessary characters from column names
colnames(labor)[6:12] <- substring(colnames(labor[6:12]), first=12)

## grab only columns needed
labor <- labor[,-c(7, 12)]

#Keep only Annual average for the year
labor_data <- labor[which(labor$Period=="Annual Average"),]
##9168 observations

write.csv(labor_data, file="labordatawithnas.csv")

## Replace N/A's with "NA" to remove the slash.
labor_data2 <- labor_data
labor_data2$Region <- replace(labor_data2$Region,labor_data2$Region=="N/A", NA)
labor_data2$County <- replace(labor_data2$County,labor_data2$County=="N/A", NA)
labor_data2$State <- replace(labor_data2$State,labor_data2$State=="N/A", NA)
labor_data2$Municipal <- replace(labor_data2$Municipal,labor_data2$Municipal=="N/A", NA)

labor_data2$Region  <- ifelse(!is.na(labor_data2$Region), "United States", "MA")
labor_data2$Region  <- ifelse(!is.na(labor_data2$County), paste(labor_data2$County, "County"), labor_data2$Region)
labor_data2$Region  <- ifelse(!is.na(labor_data2$Municipal), as.character(labor_data2$Municipal), as.character(labor_data2$Region))

## Add "County" to the end of each County name
labor_data2$County <- paste(labor_data2$County, "County")

# grab only the columns that we want: 
labor_data3 <- labor_data2[,-6]

## save and reload to make factors into numeric (this is faster than the other methods)
write.csv(labor_data3, file="labordata.csv")
labor_data3 <- read.csv("labordata.csv")[,-c(1)]

colnames(labor_data3)[c(5:9)] <- c("Year", "Unemployment Rate Avg", "No Unemployed Avg", "No Employed Avg", "No Labor Avg")

#Organizing by Region
labor_data4 <- labor_data3 %>%
  arrange(Region)

# Put US and MA at top of data set
idx_labor_MA <- which(labor_data4$Region == "MA")
idx_labor_US <- which(labor_data4$Region == "United States")
labor_data4 <- rbind.data.frame(labor_data4[idx_labor_US,], labor_data4[idx_labor_MA,], labor_data4[-c(idx_labor_MA, idx_labor_US),])
View(labor_data4)

##Eliminating unnecessary data read from CDC - not required for most other datasets
labor_data4 <- labor_data4[-c(1:759),]
  
## save data
write.csv(labor_data4, file="labor/labordata.csv")
