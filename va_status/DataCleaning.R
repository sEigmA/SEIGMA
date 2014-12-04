#######################################
## Title: Income Data Cleaning       ##
## Author(s): Emily Ramos, Arvind    ##
##            Ramakrishnan, Jenna    ##
##            Kiridly, Steve Lauer   ## 
## Date Created:  11/20/2014         ##
## Date Modified: 12/04/2014         ##
#######################################


require(sas7bdat)

## load SAS data
va <- read.sas7bdat("ba002_01_5yr.sas7bdat")

## give columns relevant titles
column_titles <- read.csv("BA002_01_5yr_contents.csv", skip=1)
colnames(va)[5:127] <- as.character(column_titles$Label[1:123])

## remove unnecessary characters from column names
colnames(va)[5:127] <- substring(colnames(va[5:127]), first=12)

va_data <- va[,c(1:5, 121:127)]

## Replace N/A's with "NA" to remove the slash.
va_data2 <- va_data
va_data2$Region <- replace(va_data2$Region,va_data2$Region=="N/A", NA)
va_data2$County <- replace(va_data2$County,va_data2$County=="N/A", NA)
va_data2$State <- replace(va_data2$State,va_data2$State=="N/A", NA)
va_data2$Municipal <- replace(va_data2$Municipal,va_data2$Municipal=="N/A", NA)

va_data2$Region  <- ifelse(!is.na(va_data2$Region), "United States", "MA")
va_data2$Region  <- ifelse(!is.na(va_data2$County), paste(va_data2$County, "County"), va_data2$Region)
va_data2$Region  <- ifelse(!is.na(va_data2$Municipal),as.character(va_data2$Municipal),as.character(va_data2$Region))

va_data2$County <- paste(va_data2$County, "County")

#Five Year Average Variable
va_data2$Five_Year_Average <- va_data2[,5]
va_data2$Five_Year_Average <- as.numeric(substr(va_data2$Five_Year_Average, 6, 9))

#sorting
va_data3 <- va_data2[,c(1:6, 9, 11, 13)]

## save and reload to make factors into numeric (this is faster than the other methods)
write.csv(va_data3, file="vetstatusdata.csv")
va_data3 <- read.csv("vetstatusdata.csv")[,-c(1)]

colnames(va_data3)[c(5:8)] <- c("Five_Year_Range", "Civilian_Pop", "Vet_Pop", "Percent_Vet")

#Organizing Region
va_data4 <- va_data3[order(va_data3$Region),]
va_data4 <- va_data4[c(1:684, 689:1253, 1260:1468, 685:688, 1254:1259),]

write.csv(va_data4, file="va/vetstatusdata.csv")
