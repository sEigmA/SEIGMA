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
bank2 <- read.sas7bdat("ap004_01_02.sas7bdat")

## give columns relevant titles
column_titles <- read.csv("AP004_01_contents.csv", skip=1)
colnames(bank)[5:20] <- as.character(column_titles$Label[1:16])
column_titles2 <- read.csv("AP004_01_02contents.csv", skip=1)
colnames(bank2)[5:20] <- as.character(column_titles2$Label[1:16])
## remove unnecessary characters from column names
colnames(bank)[5:20] <- substring(colnames(bank[5:20]), first=12)
colnames(bank2)[5:20] <- substring(colnames(bank2[5:20]), first=12)
## Replace N/A's with "NA" to remove the slash.
bank[,1:4]<-apply(bank[,1:4],2, function(x) replace(x, x=="N/A", NA))
bank2[,1:4]<-apply(bank2[,1:4],2, function(x) replace(x, x=="N/A", NA))
bank2[,7:20]<-apply(bank2[,7:20],2, function(x) replace(x, x=="-", NA))
write.csv(bank, file="bankdata1.csv",row.names=FALSE)
write.csv(bank2, file="bank2data1.csv",row.names=FALSE)
bank<-read.csv(file="bankdata1.csv")
bank2<-read.csv(file="bank2data1.csv")
bank_data<-bank[c(39, 50:63),]
bank_US<-bank[which(bank[,4]=="United States"),]
bank_data1<-rbind.data.frame(bank_US, bank_data)
bank_data1$Year<-rep(2013,16)
bank_data2<-bank_data1
bank_data2$County  <- ifelse(!is.na(bank_data2$County), paste(bank_data2$County, "County"), NA)
bank_data2$Region  <- ifelse(!is.na(bank_data2$Region), "United States", "MA")
bank_data2$Region  <- ifelse(!is.na(bank_data2$County), bank_data2$County, bank_data2$Region)
##bank data 2014
bank2_data<-bank2[c(49, 59:72),]
bank2_data[,2]<-"MA"
bank2_US<-bank2[which(bank2[,4]=="United States"),]
bank2_data1<-rbind.data.frame(bank2_US, bank2_data)
bank2_data1$Year<-rep(2014,16)
bank2_data2<-bank2_data1
bank2_data2$County  <- ifelse(!is.na(bank2_data2$County), paste(bank2_data2$County, "County"), NA)
bank2_data2$Region  <- ifelse(!is.na(bank2_data2$Region), "United States", "MA")
bank2_data2$Region  <- ifelse(!is.na(bank2_data2$County), bank2_data2$County, bank2_data2$Region)
##combined two bank data
bank_data3<-rbind.data.frame(bank_data2, bank2_data2)
## grab only columns needed
bank_data4 <- bank_data3[,c(4,7:21)]
colnames(bank_data4)[2:15] <- c("All_Filings_Total","All_Filings_Chapter_7","All_Filings_Chapter_11",
                                 "All_Filings_Chapter_12", "All_Filings_Chapter_13", "Business_Filings_Total",
                                 "Business_Filings_Chapter_7", "Business_Filings_Chapter_11", "Business_Filings_Chapter_12",
                                 "Business_Filings_Chapter_13", "NonBusiness_Filings_Total","NonBusiness_Filings_Chapter_7",
                                 "NonBusiness_Filings_Chapter_11", "NonBusiness_Filings_Chapter_13")

write.csv(bank_data4, file="bankruptcies/bankdata.csv",row.names=FALSE)
##calculated the proportion of each chapter in business filings
bank_data5<-bank_data4
bank_data5$Proportion_Business_Filings_Chapter_7<-round(bank_data5$Business_Filings_Chapter_7/bank_data5$Business_Filings_Total,3)
bank_data5$Proportion_Business_Filings_Chapter_11<-round(bank_data5$Business_Filings_Chapter_11/bank_data5$Business_Filings_Total,3)
bank_data5$Proportion_Business_Filings_Chapter_12<-round(bank_data5$Business_Filings_Chapter_12/bank_data5$Business_Filings_Total,3)
bank_data5$Proportion_Business_Filings_Chapter_13<-round(bank_data5$Business_Filings_Chapter_13/bank_data5$Business_Filings_Total,3)
##calculated the proportion of each chapter in nonbusiness filings
bank_data5$Proportion_NonBusiness_Filings_Chapter_7<-round(bank_data5$NonBusiness_Filings_Chapter_7/bank_data5$NonBusiness_Filings_Total,3)
bank_data5$Proportion_NonBusiness_Filings_Chapter_11<-round(bank_data5$NonBusiness_Filings_Chapter_11/bank_data5$NonBusiness_Filings_Total,3)
bank_data5$Proportion_NonBusiness_Filings_Chapter_13<-round(bank_data5$NonBusiness_Filings_Chapter_13/bank_data5$NonBusiness_Filings_Total,3)
write.csv(bank_data5, file="bankruptcies/bankdata1.csv",row.names=FALSE)