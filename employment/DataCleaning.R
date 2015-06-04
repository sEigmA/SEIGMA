#######################################
## Title: employment Data Cleaning   ##
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
colnames(emp)[4:29] <- as.character(column_titles$Label[1:26])

## remove unnecessary characters from column names
colnames(emp)[4:29] <- substring(colnames(emp[4:29]), first=12)
write.csv(emp, file="empdata1.csv",row.names=FALSE)


## grab only columns needed
emp_data <- emp[,c(1:3,7:9, 13:25)]

#Keep only Average Monthly employment and each monthly employment of all industries for the year
emp_data1 <- emp_data[which(emp_data[,4]=="Total, All Industries"),]

write.csv(emp_data1, file="empdata2.csv")

## Replace N/A's with "NA" to remove the slash.
emp_data2 <- emp_data1
emp_data2[,1:2]<-apply(emp_data2[,1:2],2, function(x) replace(x, x=="N/A", NA))
##No NA in Municipal

# grab only the columns that we want: 
emp_data3 <- emp_data2[,-c(4,19)]


colnames(emp_data3)[c(3:17)] <- c("Year" , "Average_Monthly_Employment","Average_Weekly_Wage","Employment_for_January",
                                  "Employment_for_February", "Employment_for_March","Employment_for_April", 
                                  "Employment_for_May","Employment_for_June","Employment_for_Junly", 
                                  "Employment_for_August", "Employment_for_September","Employment_for_October",
                                  "Employment_for_November","Employment_for_December")


## save data
write.csv(emp_data3, file="employ/empdata1.csv")
