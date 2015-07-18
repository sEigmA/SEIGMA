###########################################
## Title: Bankruptcies Data Cleaning     ##
## Author(s): Xuelian Li,                ##
##            Jenna Kiridly              ## 
## Date Created:  07/17/2015             ##
## Date Modified: 07/17/2015             ##
###########################################

require(sas7bdat)
require(dplyr)

## load SAS data
bank <- read.sas7bdat("ap004_01.sas7bdat")

## give columns relevant titles
column_titles <- read.csv("AP004_01_contents.csv", skip=1)
colnames(bank)[5:20] <- as.character(column_titles$Label[1:16])

## remove unnecessary characters from column names
colnames(bank)[5:20] <- substring(colnames(bank[5:20]), first=12)
## Replace N/A's with "NA" to remove the slash.
bank[,1:4]<-apply(bank[,1:4],2, function(x) replace(x, x=="N/A", NA))
write.csv(bank, file="bankdata1.csv",row.names=FALSE)
bank<-read.csv(file="bankdata1.csv")
bank_data<-bank[which(bank[,2]=="MA"),]
bank_US<-bank[which(bank[,4]=="United States"),]
bank_data1<-rbind.data.frame(bank_US, bank_data)
## grab only columns needed
bank_data2 <- bank_data1[,c(3,5,7:20)]
colnames(bank_data2)[c(2:16)] <- c("Region" , "All_Filings_Total","All_Filings_Chapter_7","All_Filings_Chapter_11",
                                 "All_Filings_Chapter_12", "All_Filings_Chapter_13", "Business_Filings_Total",
                                 "Business_Filings_Chapter_7", "Business_Filings_Chapter_11", "Business_Filings_Chapter_12",
                                 "Business_Filings_Chapter_13", "NonBusiness_Filings_Total","NonBusiness_Filings_Chapter_7",
                                 "NonBusiness_Filings_Chapter_11", "NonBusiness_Filings_Chapter_13")

write.csv(bank_data2, file="bankdata2.csv")

