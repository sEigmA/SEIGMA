#######################################
## Title: Income Data Cleaning       ##
## Author(s): Emily Ramos, Arvind    ##
##            Ramakrishnan, Jenna    ##
##            Kiridly, Steve Lauer   ## 
## Date Created:  11/5/2014          ##
## Date Modified: 11/6/2014          ##
#######################################


require(sas7bdat)

## load SAS data
income <- read.sas7bdat("at003_03_5yr.sas7bdat")

## give columns relevant titles
column_titles <- read.csv("AT003_03_5yr_contents.csv", skip=1)
colnames(income)[5:32] <- as.character(column_titles$Label[1:28])

## remove unnecessary characters from column names
colnames(income)[5:20] <- substring(colnames(income[5:20]), first=12)

inc_data <- income[,c(1:4, 8:10)]

## Replace N/A's with "NA" to remove the slash.
inc_data2 <- inc_data
inc_data2$Region <- replace(inc_data2$Region,inc_data2$Region=="N/A", NA)
inc_data2$County <- replace(inc_data2$County,inc_data2$County=="N/A", NA)
inc_data2$State <- replace(inc_data2$State,inc_data2$State=="N/A", NA)
inc_data2$Municipal <- replace(inc_data2$Municipal,inc_data2$Municipal=="N/A", NA)

inc_data2$Region  <- ifelse(!is.na(inc_data2$Region), "United States", "MA")
inc_data2$Region  <- ifelse(!is.na(inc_data2$County), paste(inc_data2$County, "County"), inc_data2$Region)
inc_data2$Region  <- ifelse(!is.na(inc_data2$Municipal),as.character(inc_data2$Municipal),as.character(inc_data2$Region))

inc_data2$County <- paste(inc_data2$County, "County")

#Five Year Average Variable
inc_data2$Five_Year_Average <- inc_data2[,5]
inc_data2$Five_Year_Average <- as.numeric(substr(inc_data2$Five_Year_Average, 6, 9))
#sorting
inc_data3 <- inc_data2[,c(1:5,8,6,7)]

## save and reload to make factors into numeric (this is faster than the other methods)
write.csv(inc_data3, file="incomedata.csv")
inc_data3 <- read.csv("incomedata.csv")[,-c(1)]

colnames(inc_data3)[c(5,7:8)] <- c("Five_Year_Range", "Median_Household_Income", "Margin_Error_Median")

#Organizing Region
inc_data4 <- inc_data3[order(inc_data3$Region),]
inc_data4 <- inc_data4[c(1:684, 689:1253, 1260:1468, 685:688, 1254:1259),]


write.csv(inc_data4, file="income/incomedata.csv")

