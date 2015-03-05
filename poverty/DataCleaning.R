########################################
## Title: Poverty Data Cleaning       ##
## Author(s): Emily Ramos, Xuelian Li ##
##            Arvind Ramakrishnan,    ##
##            and Jenna Kiridly       ## 
## Date Created:  02/20/2015          ##
## Date Modified: 03/03/2015 ER       ##
########################################

setwd("poverty")
require(sas7bdat)
require(dplyr)

## load SAS data
pov <- read.sas7bdat("at001_03_5yr.sas7bdat")

#drop variables not needed
pov <- pov[,1:14]

## give columns relevant titles
column_titles <- read.csv("AT001_03_5yr_contents.csv", skip=1)
colnames(pov)[5:14] <- as.character(column_titles$Label[1:10])

## remove unnecessary characters from column names
colnames(pov)[5:14] <- substring(colnames(pov[5:14]), first=11)

## drop more columns
pov <- pov[,c(1:4, 8, 9, 11, 13, 14)]

## Replace N/A's with "NA" to remove the slash.
pov_data2 <- pov
pov_data2$Region <- replace(pov_data2$Region,pov_data2$Region=="N/A", NA)
pov_data2$County <- replace(pov_data2$County,pov_data2$County=="N/A", NA)
pov_data2$State <- replace(pov_data2$State,pov_data2$State=="N/A", NA)
pov_data2$Municipal <- replace(pov_data2$Municipal,pov_data2$Municipal=="N/A", NA)

pov_data2$Region  <- ifelse(!is.na(pov_data2$Region), "United States", "MA")
pov_data2$Region  <- ifelse(!is.na(pov_data2$County), paste(pov_data2$County, "County"), pov_data2$Region)
pov_data2$Region  <- ifelse(!is.na(pov_data2$Municipal),as.character(pov_data2$Municipal),as.character(pov_data2$Region))

## Add "County" to the end of each County name
pov_data2$County <- paste(pov_data2$County, "County")

## save and reload to make factors into numeric (this is faster than the other methods)
write.csv(pov_data2, file="povertyratedata.csv")
pov_data3 <- read.csv("povertyratedata.csv")[,-c(1)]

colnames(pov_data3)[c(5:9)] <- c("Five_Year_Range", "Total_Pop", "Pov_Pop", "Percent_Pov", "Margin_Error_Percent")

# Organizing by Region
pov_data4 <- pov_data3 %>%
  arrange(Region)

# Put US and MA at top of data set
idx_edu_MA <- which(pov_data4$Region == "MA")
idx_edu_US <- which(pov_data4$Region == "United States")
pov_data4 <- rbind.data.frame(pov_data4[idx_edu_US,],
                              pov_data4[idx_edu_MA,], pov_data4[-c(idx_edu_MA, idx_edu_US),])

# Save data
write.csv(pov_data4, file="poverty/povratedata.csv")
