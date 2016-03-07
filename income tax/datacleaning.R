##############################################
## Title: Personal Income Tax Data Cleaning ##
## Author(s): Xuelian Li, Jenna Kiridly     ## 
## Date Created:  12/08/2015                ##
## Date Modified: 01/04/2016 XL             ##
##############################################

require(sas7bdat)
require(dplyr)

## load SAS data
InTax <- read.sas7bdat("aw001_02.sas7bdat")

## give columns relevant titles
column_titles <- read.csv("AW001_02_contents.csv", skip=1)
colnames(InTax)[4:24] <- as.character(column_titles$Label[1:21])

## remove unnecessary characters from column names
colnames(InTax)[4:24] <- substring(colnames(InTax)[4:24], first=12)
write.csv(InTax, file="Incometaxdata1.csv",row.names=FALSE)