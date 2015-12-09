###########################################
## Title: Property Tax Data Cleaning     ##
## Author(s): Xuelian Li, Jenna Kiridly  ## 
## Date Created:  12/08/2015             ##
## Date Modified:                        ##
###########################################

require(sas7bdat)
require(dplyr)

## load SAS data
tax <- read.sas7bdat("aw002_01.sas7bdat")

## give columns relevant titles
column_titles <- read.csv("AW002_01_contents.csv", skip=1)
colnames(tax)[3:9] <- as.character(column_titles$Label[1:7])

## remove unnecessary characters from column names
colnames(tax)[3:9] <- substring(colnames(tax)[3:9], first=12)
tax[,1:2]<-apply(tax[,1:2],2, function(x) replace(x, x=="N/A", NA))

write.csv(tax, file="taxdata1.csv",row.names=FALSE)

##calculate the inflation adjusted Wage
Adjusted_index<-data.frame(Year=2003:2013, Annual=c(184.0,188.9,195.3,201.6,207.342,215.303,214.537,218.056,224.939,229.594, 232.957
))
Adjusted_index$Inflation_rate<-232.957/Adjusted_index$Annual
tax$Inflation_Adjusted_Residential<-rep(0,3861)
for (i in 2003:2013) {
  tax$Inflation_Adjusted_Residential[which(tax$F_year==i)]<-tax$Residential[which(tax$F_year==i)]*Adjusted_index$Inflation_rate[i-2002]
}
tax$Inflation_Adjusted_Residential<-round(tax$Inflation_Adjusted_Residential,0)
write.csv(tax, file="taxdata2.csv",row.names=FALSE)

