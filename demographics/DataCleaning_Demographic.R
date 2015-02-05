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

##Choose cloumn we need(Percentage of Age and Sex, Race, Ethnicity)
Dem_data<-Demographic[,c(1:5,9,76,81,85,87,92,96,100,104,108,112,116,120,124,128,132)]

## Replace N/A's with "NA" to remove the slash.
Dem_data2<-Dem_data
Dem_data2[,1:4]<-apply(Dem_data2[,1:4],2, function(x) replace(x, x=="N/A", NA))

Dem_data2$County  <- ifelse(!is.na(Dem_data2$County), paste(Dem_data2$County, "County"), NA)
Dem_data2$Region  <- ifelse(!is.na(Dem_data2$Region), "United States", "MA")
Dem_data2$Region  <- ifelse(!is.na(Dem_data2$County), Dem_data2$County, Dem_data2$Region)
Dem_data2$Region  <- ifelse(!is.na(Dem_data2$Municipal),as.character(Dem_data2$Municipal),as.character(Dem_data2$Region))

## Five_Year_Average
Dem_data3<-Dem_data2
Dem_data3$Five_Year_Average<-Dem_data3[,5]
Dem_data3$Five_Year_Average1<-as.numeric(substr(Dem_data3$Five_Year_Average, 1, 4))
Dem_data3$Five_Year_Average2<-as.numeric(substr(Dem_data3$Five_Year_Average, 6, 9))

#Organizing Region
Dem_data4<-Dem_data3[-(1:16),]
Dem_data4<-Dem_data4[order(Dem_data4$Region),]
Dem_data5<-rbind(Dem_data3[1:16,], Dem_data4 )
colnames(Dem_data5)[5:24]<-gsub("Estimate -", "", colnames(Dem_data5[5:24]))


