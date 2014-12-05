#######################################
## Title: Education Data Cleaning    ##
## Author(s): Emily Ramos, Arvind    ##
##            Ramakrishnan, Jenna    ##
##            Kiridly, Steve Lauer   ## 
## Date Created:  11/20/2014         ##
## Date Modified: 12/04/2014         ##
#######################################


require(sas7bdat)

## load SAS data
edu <- read.sas7bdat("ba002_01_5yr.sas7bdat")

## give columns relevant titles
column_titles <- read.csv("BA002_01_5yr_contents.csv", skip=1)
colnames(edu)[5:127] <- as.character(column_titles$Label[1:123])

## remove unnecessary characters from column names
colnames(edu)[5:127] <- substring(colnames(edu[5:127]), first=12)

edu_data <- edu[,c(1:5, 121:127)]

## Replace N/A's with "NA" to remove the slash.
edu_data2 <- edu_data
edu_data2$Region <- replace(edu_data2$Region,edu_data2$Region=="N/A", NA)
edu_data2$County <- replace(edu_data2$County,edu_data2$County=="N/A", NA)
edu_data2$State <- replace(edu_data2$State,edu_data2$State=="N/A", NA)
edu_data2$Municipal <- replace(edu_data2$Municipal,edu_data2$Municipal=="N/A", NA)

edu_data2$Region  <- ifelse(!is.na(edu_data2$Region), "United States", "MA")
edu_data2$Region  <- ifelse(!is.na(edu_data2$County), paste(edu_data2$County, "County"), edu_data2$Region)
edu_data2$Region  <- ifelse(!is.na(edu_data2$Municipal),as.character(edu_data2$Municipal),as.character(edu_data2$Region))

edu_data2$County <- paste(edu_data2$County, "County")

#Five Year Average Variable
edu_data2$Five_Year_Average <- edu_data2[,5]
edu_data2$Five_Year_Average <- as.numeric(substr(edu_data2$Five_Year_Average, 6, 9))

#sorting
edu_data3 <- edu_data2[,c(1:6, 9, 11, 13)]

## save and reload to make factors into numeric (this is faster than the other methods)
write.csv(edu_data3, file="vetstatusdata.csv")
edu_data3 <- read.csv("vetstatusdata.csv")[,-c(1)]

colnames(edu_data3)[c(5:8)] <- c("Five_Year_Range", "Civilian_Pop", "Vet_Pop", "Percent_Vet")

#Organizing Region
edu_data4 <- edu_data3[order(edu_data3$Region),]
edu_data4 <- edu_data4[c(1:684, 689:1253, 1260:1468, 685:688, 1254:1259),]

write.csv(edu_data4, file="edu/vetstatusdata.csv")
