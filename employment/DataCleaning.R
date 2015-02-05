#######################################
## Title: employment Data Cleaning ##
## Author(s): Emily Ramos, Arvind    ##
##            Ramakrishnan, Jenna    ##
##            Kiridly, Steve Lauer   ## 
## Date Created:  02/04/2015         ##
## Date Modified: 02/05/2015         ##
#######################################

require(sas7bdat)
require(dplyr)

## load SAS data
emp <- read.sas7bdat("ap003_01.sas7bdat")

## give columns relevant titles
column_titles <- read.csv("AP003_01_contents.csv", skip=1)
colnames(emp)[7:8] <- as.character(column_titles$Label[4:5])

## remove unnecessary characters from column names
colnames(emp)[7:8] <- substring(colnames(emp[7:8]), first=12)

## grab only columns needed
emp <- emp[,c(1:3,7:8)]

#Keep only Average Monthly employment for the year
emp_data <- emp[which(emp[,4]=="Total, All Industries"),]

write.csv(emp_data, file="empdata.csv")

## Replace N/A's with "NA" to remove the slash.
emp_data2 <- emp_data

emp_data2$Municipal <- replace(emp_data2$Municipal,emp_data2$Municipal=="N/A", NA)

#emp_data2$Region  <- ifelse(!is.na(emp_data2$Region), "United States", "MA")
emp_data2$Municipal  <- ifelse(!is.na(emp_data2$Municipal), as.character(emp_data2$Municipal), as.character(emp_data2$Region))

# grab only the columns that we want: 
emp_data3 <- emp_data2[,-c(2,4)]

## save and reload to make factors into numeric (this is faster than the other methods)
write.csv(emp_data3, file="empdata.csv")
emp_data3 <- read.csv("empdata.csv")[,-c(1)]

colnames(emp_data3)[c(1:3)] <- c("Municipal" , "Year" , "Average_Monthly_Employment")


#Organizing by Region
emp_data4 <- emp_data3 %>%
  arrange(Municipal)

## save data
write.csv(emp_data4, file="employ/empdata.csv")
