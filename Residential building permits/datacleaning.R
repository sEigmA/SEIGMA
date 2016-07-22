###########################################
## Title: Property Tax Data Cleaning     ##
## Author(s): Xuelian Li, Jenna Kiridly  ## 
## Date Created:  12/08/2015             ##
## Date Modified: 01/04/2016 XL          ##
###########################################

require(sas7bdat)
require(dplyr)

## load SAS data
building_permit <- read.sas7bdat("ar002_01.sas7bdat")

## give columns relevant titles
column_titles <- read.csv("AR002_01_contents.csv", skip=1)
colnames(building_permit)[5:38] <- as.character(column_titles$Label[1:34])

## remove unnecessary characters from column names
colnames(building_permit)[5:38] <- substring(colnames(building_permit)[5:38], first=12)
building_permit[,1:3]<-apply(building_permit[,1:3],2, function(x) replace(x, x=="N/A", NA))

write.csv(building_permit, file="buildingdata1.csv",row.names=FALSE)

##calculate the inflation adjusted Residential
Adjusted_index<-data.frame(Year=2003:2013, Annual=c(184.0,188.9,195.3,201.6,207.342,215.303,214.537,218.056,224.939,229.594, 232.957
))
Adjusted_index$Inflation_rate<-232.957/Adjusted_index$Annual
tax$Inflation_Adjusted_Residential<-rep(0,3861)
for (i in 2003:2013) {
  tax$Inflation_Adjusted_Residential[which(tax$F_year==i)]<-tax$Residential[which(tax$F_year==i)]*Adjusted_index$Inflation_rate[i-2002]
}
tax$Inflation_Adjusted_Residential<-round(tax$Inflation_Adjusted_Residential,0)

##calculate the inflation adjusted Open.Space
tax$Inflation_Adjusted_Open_Space<-rep(0,3861)
for (i in 2003:2013) {
  tax$Inflation_Adjusted_Open_Space[which(tax$F_year==i)]<-tax$Open.Space[which(tax$F_year==i)]*Adjusted_index$Inflation_rate[i-2002]
}
tax$Inflation_Adjusted_Open_Space<-round(tax$Inflation_Adjusted_Open_Space,0)

##calculate the inflation adjusted Commercial
tax$Inflation_Adjusted_Commercial<-rep(0,3861)
for (i in 2003:2013) {
  tax$Inflation_Adjusted_Commercial[which(tax$F_year==i)]<-tax$Commercial[which(tax$F_year==i)]*Adjusted_index$Inflation_rate[i-2002]
}
tax$Inflation_Adjusted_Commercial<-round(tax$Inflation_Adjusted_Commercial,0)

##calculate the inflation adjusted Industrial
tax$Inflation_Adjusted_Industrial<-rep(0,3861)
for (i in 2003:2013) {
  tax$Inflation_Adjusted_Industrial[which(tax$F_year==i)]<-tax$Industrial[which(tax$F_year==i)]*Adjusted_index$Inflation_rate[i-2002]
}
tax$Inflation_Adjusted_Industrial<-round(tax$Inflation_Adjusted_Industrial,0)

##calculate the inflation adjusted Personal.Property
tax$Inflation_Adjusted_Personal_Property<-rep(0,3861)
for (i in 2003:2013) {
  tax$Inflation_Adjusted_Personal_Property[which(tax$F_year==i)]<-tax$Personal.Property[which(tax$F_year==i)]*Adjusted_index$Inflation_rate[i-2002]
}
tax$Inflation_Adjusted_Personal_Property<-round(tax$Inflation_Adjusted_Personal_Property,0)

##calculate the inflation adjusted Total.Levy
tax$Inflation_Adjusted_Total_Levy<-rep(0,3861)
for (i in 2003:2013) {
  tax$Inflation_Adjusted_Total_Levy[which(tax$F_year==i)]<-tax$Total.Levy[which(tax$F_year==i)]*Adjusted_index$Inflation_rate[i-2002]
}
tax$Inflation_Adjusted_Total_Levy<-round(tax$Inflation_Adjusted_Total_Levy,0)
write.csv(tax, file="taxdata2.csv",row.names=FALSE)

##calculate the Percent of Levy by Class
tax$Percentage_of_Residential<-round(tax$Residential/tax$Total.Levy*100, 2)
tax$Percentage_of_Open_Space<-round(tax$Open.Space/tax$Total.Levy*100, 2)
tax$Percentage_of_Commercial<-round(tax$Commercial/tax$Total.Levy*100, 2)
tax$Percentage_of_Industrial<-round(tax$Industrial/tax$Total.Levy*100, 2)
tax$Percentage_of_Personal_Property<-round(tax$Personal.Property/tax$Total.Levy*100, 2)
write.csv(tax, file="PropertyTax/taxdata.csv",row.names=FALSE)
##calculate the Total levy percent change since 2003
year_2003<-tax[which(tax$F_year==2003),]
tax$Total_Levy_Pct_Change<-round((tax$Inflation_Adjusted_Total_Levy/year_2003$Inflation_Adjusted_Total_Levy[match(tax$Municipal,year_2003$Municipal)]-1)*100,1)
write.csv(tax, file="PropertyTax/taxdata2.csv",row.names=FALSE)
##put the label as x million
tax$Total_Levy_Million<-round(tax$Inflation_Adjusted_Total_Levy/1000000, 2)
tax$Residential_Million<-round(tax$Inflation_Adjusted_Residential/1000000, 2)
tax$Open_Space_Million<-round(tax$Inflation_Adjusted_Open_Space/1000000, 2)
tax$Commercial_Million<-round(tax$Inflation_Adjusted_Commercial/1000000, 2)
tax$Industrial_Million<-round(tax$Inflation_Adjusted_Industrial/1000000, 2)
tax$Personal_Property_Million<-round(tax$Inflation_Adjusted_Personal_Property/1000000, 2)
write.csv(tax, file="PropertyTax/taxdata2.csv",row.names=FALSE)