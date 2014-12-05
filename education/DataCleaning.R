#######################################
## Title: Education Data Cleaning    ##
## Author(s): Emily Ramos, Arvind    ##
##            Ramakrishnan, Jenna    ##
##            Kiridly, Steve Lauer   ## 
## Date Created:  11/20/2014         ##
## Date Modified: 12/04/2014         ##
#######################################

setwd("education/")
require(sas7bdat)
require(dplyr)

## load SAS data
edu <- read.sas7bdat("ba002_01_5yr.sas7bdat")

## give columns relevant titles
column_titles <- read.csv("BA002_01_5yr_contents.csv", skip=1)
colnames(edu)[5:127] <- as.character(column_titles$Label[1:123])

## remove unnecessary characters from column names
colnames(edu)[5:127] <- substring(colnames(edu[5:127]), first=12)

## grab only columns involving educational attainment
edu_cols <- grep("EDUCATIONAL ATTAINMENT", x = colnames(edu))
edu_data <- edu[,c(1:5, edu_cols)]

## Replace N/A's with "NA" to remove the slash.
edu_data2 <- edu_data
edu_data2$Region <- replace(edu_data2$Region,edu_data2$Region=="N/A", NA)
edu_data2$County <- replace(edu_data2$County,edu_data2$County=="N/A", NA)
edu_data2$State <- replace(edu_data2$State,edu_data2$State=="N/A", NA)
edu_data2$Municipal <- replace(edu_data2$Municipal,edu_data2$Municipal=="N/A", NA)

edu_data2$Region  <- ifelse(!is.na(edu_data2$Region), "United States", "MA")
edu_data2$Region  <- ifelse(!is.na(edu_data2$County), paste(edu_data2$County, "County"), edu_data2$Region)
edu_data2$Region  <- ifelse(!is.na(edu_data2$Municipal), as.character(edu_data2$Municipal), 
                            as.character(edu_data2$Region))

## Add "County" to the end of each County name
edu_data2$County <- paste(edu_data2$County, "County")

# grab only the columns that we want: % HS+, % Bach+, % Grad/Prof Degree
edu_data3 <- edu_data2[,c(1:7,35,36,39,40,43,44)]

## save and reload to make factors into numeric (this is faster than the other methods)
write.csv(edu_data3, file="edudata.csv")
edu_data3 <- read.csv("edudata.csv")[,-c(1)]

colnames(edu_data3)[c(5:13)] <- c("Five_Year_Range", "Pop_25", "Margin_Error_Pop", "Grad_Pct", "Margin_Error_Grad", "HS_Pct", "Margin_Error_HS", "Bachelors_Pct", "Margin_Error_Bach")

#Organizing by Region and putting columns in order
edu_data4 <- edu_data3 %>%
  select(1:7, HS_Pct, Margin_Error_HS, Bachelors_Pct, Margin_Error_Bach, Grad_Pct, Margin_Error_Grad) %>%
  arrange(Region)

# Put US and MA at top of data set
idx_edu_MA <- which(edu_data4$Region == "MA")
idx_edu_US <- which(edu_data4$Region == "United States")
edu_data4 <- rbind.data.frame(edu_data4[idx_edu_US,],
                              edu_data4[idx_edu_MA,], edu_data4[-c(idx_edu_MA, idx_edu_US),])

## save data
write.csv(edu_data4, file="education/edudata.csv")
