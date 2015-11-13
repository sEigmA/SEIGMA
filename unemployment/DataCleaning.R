#######################################
## Title: Unemploy data cleaning      ##
## Author(s): Xuelian Li,Jenna Kiridly##
##            Emily Ramos, Arvind     ##
##            Ramakrishnan,           ##
## Date Created:  01/07/2015          ##
## Date Modified: 11/13/2015  XL      ##
#######################################

require(sas7bdat)
require(dplyr)

## load SAS data
unemp <- read.sas7bdat("at004_01.sas7bdat")

## give columns relevant titles
column_titles <- read.csv("AT004_01_contents.csv", skip=1)
colnames(unemp)[6:12] <- as.character(column_titles$Label[1:7])

## remove unnecessary characters from column names
colnames(unemp)[6:12] <- substring(colnames(unemp[6:12]), first=12)

## grab only columns needed
unemp <- unemp[,-c(7, 12)]
write.csv(unemp, file="unempdata1.csv",row.names=FALSE)

#Keep only Annual average for the year
unemp_data <- unemp[which(unemp$Period=="Annual Average"),]

write.csv(unemp_data, file="unempdata2.csv", row.names=FALSE)

## Replace N/A's with "NA" to remove the slash.
unemp_data2 <- unemp_data
unemp_data2[,1:4]<-apply(unemp_data2[,1:4],2, function(x) replace(x, x=="N/A", NA))

unemp_data2$Region  <- ifelse(!is.na(unemp_data2$Region), "United States", "MA")
unemp_data2$Region  <- ifelse(!is.na(unemp_data2$County), paste(unemp_data2$County, "County"), unemp_data2$Region)
unemp_data2$Region  <- ifelse(!is.na(unemp_data2$Municipal), as.character(unemp_data2$Municipal), as.character(unemp_data2$Region))

## Add "County" to the end of each County name
unemp_data2$County <- paste(unemp_data2$County, "County")

# grab only the columns that we want: 
unemp_data3 <- unemp_data2[,-6]

## save and reload to make factors into numeric (this is faster than the other methods)
write.csv(unemp_data3, file="unempdata.csv", row.names=FALSE)
unemp_data3 <- read.csv("unempdata.csv")

colnames(unemp_data3)[c(5:9)] <- c("Year", "Unemployment_Rate_Avg", "No_Unemployed_Avg", "No_Employed_Avg", "No_Labor_Avg")

#Organizing by Region
unemp_data4 <- unemp_data3 %>%
    filter(Year>=1990)%>%
  arrange(Region)

# Put US and MA at top of data set
idx_unemp_MA <- which(unemp_data4$Region == "MA")
idx_unemp_US <- which(unemp_data4$Region == "United States")
MA<-unemp_data4[idx_unemp_MA,]
US<-unemp_data4[idx_unemp_US,]
##Eliminating unnecessary data, US is not the real us data
write.csv(MA, file="MA.csv", row.names=FALSE)
write.csv(US, file="US.csv", row.names=FALSE)
unemp_data4 <- rbind.data.frame(unemp_data4[idx_unemp_MA,], unemp_data4[-c(idx_unemp_MA, idx_unemp_US),])
  
## save data
write.csv(unemp_data4, file="unemployment/unempdata.csv",row.names=FALSE)
##calculate the unemployment rate and labor force percent change since 1990
year_90<-unemp_data4[which(unemp_data4$Year==1990),]
unemp_data4$Unemployment_Rate_Change<-round(unemp_data4$Unemployment_Rate_Avg-year_90$Unemployment_Rate_Avg[match(unemp_data4$Municipal,year_90$Municipal)],1)
##labor force
unemp_data4$Labor_Pct_Change<-round((unemp_data4$No_Labor_Avg/year_90$No_Labor_Avg[match(unemp_data4$Municipal,year_90$Municipal)]-1)*100,1)
write.csv(unemp_data4, file="unemploy/unempdata1.csv",row.names=FALSE)

##calculate the labor force percent change since 2003
unemp_data5<-read.csv("unemployment/unempdata.csv")
unemp_data6 <- unemp_data5 %>%
  filter(Year>=2003)
year_2003<-unemp_data6[which(unemp_data6$Year==2003),]
unemp_data6$Labor_Pct_Change<-round((unemp_data6$No_Labor_Avg/year_2003$No_Labor_Avg[match(unemp_data6$Municipal,year_2003$Municipal)]-1)*100,1)
write.csv(unemp_data6, file="unemployment/unempdata2.csv",row.names=FALSE)
