#######################################
## Title: Demographic Data Cleaning  ##
## Author(s): Emily Ramos, Arvind    ##
##            Ramakrishnan, Jenna    ##
##            Kiridly, Steve Lauer   ##
##            Xuelian, Li            ##
## Date Created:  01/29/2015         ##
## Date Modified: 01/29/2015         ##
#######################################

require(sas7bdat)

## load SAS data
Demographic <- read.sas7bdat("ba003_01_5yr.sas7bdat")

## give columns relevant titles
column_titles <- read.csv("BA003_01_5yr_contents.csv", skip=1)
colnames(Demographic)[5:133] <- as.character(column_titles$Label[1:129])

## remove unnecessary characters from column names
colnames(Demographic)[5:133] <- substring(colnames(Demographic[5:133]), first=12)
colnames(Demographic)[5:133]<-gsub("; SEX AND AGE", "", colnames(Demographic[5:133]))

Dem_data<-Demographic[,c(-(6:8), -11)]

## Replace N/A's with "NA" to remove the slash.
Dem_data2<-Dem_data
Dem_data2[,1:4]<-apply(Dem_data2[,1:4],2, function(x) replace(x, x=="N/A", NA))

Dem_data2$County  <- ifelse(!is.na(Dem_data2$County), paste(Dem_data2$County, "County"), NA)
Dem_data2$Region  <- ifelse(!is.na(Dem_data2$Region), "United States", "MA")
Dem_data2$Region  <- ifelse(!is.na(Dem_data2$County), Dem_data2$County, Dem_data2$Region)
Dem_data2$Region  <- ifelse(!is.na(Dem_data2$Municipal),as.character(Dem_data2$Municipal),as.character(Dem_data2$Region))