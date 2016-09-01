###############################################
## Title: Building permits Data Cleaning     ##
## Author(s): Xuelian Li,                    ## 
## Date Created:  08/04/2016                 ##
## Date Modified: 08/11/2016 XL              ##
###############################################

require(sas7bdat)
require(dplyr)
## load SAS data
building_permit <- read.sas7bdat("ar002_01.sas7bdat")

## give columns relevant titles
column_titles <- read.csv("AR002_01_contents.csv", skip=1)
colnames(building_permit)[5:38] <- as.character(column_titles$Label[1:34])

## remove unnecessary characters from column names
colnames(building_permit)[5:38] <- substring(colnames(building_permit)[5:38], first=12)
building_permit[,1:5]<-apply(building_permit[,1:5],2, function(x) replace(x, x=="N/A", NA))
building_permit2 <- building_permit

building_permit2$Region  <- ifelse(!is.na(building_permit2$County), paste(building_permit2$County, "County"), "MA")
building_permit2$Region  <- ifelse(!is.na(building_permit2$Municipal), as.character(building_permit2$Municipal), as.character(building_permit2$Region))

write.csv(building_permit2, file="buildingdata1.csv",row.names=FALSE)

##only have data for year 2000-2011.
building_permit3<-building_permit2%>%
  arrange(Region)

##calculate the inflation adjusted Valuation (2011 dolars)
Adjusted_index<-data.frame(Year=2000:2011, Annual=c(172.2,177.1,179.9,184.0,188.9,195.3,201.6,207.342,215.303,214.537,218.056,224.939))
Adjusted_index$Inflation_rate<-224.939/Adjusted_index$Annual
building_permit3$Inflation_Adjusted_1_Family_Valuation<-rep(0,4392)
for (i in 2000:2011) {
  building_permit3$Inflation_Adjusted_1_Family_Valuation[which(building_permit3$C_year==i)]<-building_permit3[,9][which(building_permit3$C_year==i)]*Adjusted_index$Inflation_rate[i-1999]
}
building_permit3$Inflation_Adjusted_1_Family_Valuation<-round(building_permit3$Inflation_Adjusted_1_Family_Valuation,0)

#2family
building_permit3$Inflation_Adjusted_2_Family_Valuation<-rep(0,4392)
for (i in 2000:2011) {
  building_permit3$Inflation_Adjusted_2_Family_Valuation[which(building_permit3$C_year==i)]<-building_permit3[,12][which(building_permit3$C_year==i)]*Adjusted_index$Inflation_rate[i-1999]
}
building_permit3$Inflation_Adjusted_2_Family_Valuation<-round(building_permit3$Inflation_Adjusted_2_Family_Valuation,0)

##3-4 family
building_permit3$Inflation_Adjusted_3_4_Family_Valuation<-rep(0,4392)
for (i in 2000:2011) {
  building_permit3$Inflation_Adjusted_3_4_Family_Valuation[which(building_permit3$C_year==i)]<-building_permit3[,15][which(building_permit3$C_year==i)]*Adjusted_index$Inflation_rate[i-1999]
}
building_permit3$Inflation_Adjusted_3_4_Family_Valuation<-round(building_permit3$Inflation_Adjusted_3_4_Family_Valuation,0)

##5+Family
building_permit3$Inflation_Adjusted_5_Family_Valuation<-rep(0,4392)
for (i in 2000:2011) {
  building_permit3$Inflation_Adjusted_5_Family_Valuation[which(building_permit3$C_year==i)]<-building_permit3[,18][which(building_permit3$C_year==i)]*Adjusted_index$Inflation_rate[i-1999]
}
building_permit3$Inflation_Adjusted_5_Family_Valuation<-round(building_permit3$Inflation_Adjusted_5_Family_Valuation,0)


#recalculate the total buildings(31=7+10+13+16) and total units(32=8+11+14+17), and total valuation(33=9+12+15+18)(reported+imputed)
building_permit3[,31]<-building_permit3[,7]+building_permit3[,10]+building_permit3[,13]+building_permit3[,16]
building_permit3[,32]<-building_permit3[,8]+building_permit3[,11]+building_permit3[,14]+building_permit3[,17]
building_permit3[,33]<-building_permit3[,9]+building_permit3[,12]+building_permit3[,15]+building_permit3[,18]
building_permit3$Inflation_Adjusted_Total_Valuation<-building_permit3$Inflation_Adjusted_1_Family_Valuation+building_permit3$Inflation_Adjusted_2_Family_Valuation+building_permit3$Inflation_Adjusted_3_4_Family_Valuation+building_permit3$Inflation_Adjusted_5_Family_Valuation
#average valuation per units (reported+imputed)
building_permit3[,34]<-building_permit3[,33]/building_permit3[,32]
building_permit3$Inflation_Adjusted_Average_Valuation<-building_permit3$Inflation_Adjusted_Total_Valuation/building_permit3[,32]
#recalculate the total buildings(35=19+22+25+28) and total units(36=20+23+26+29), and total valuation(37=21+24+27+30)(only reported)
building_permit3[,35]<-building_permit3[,19]+building_permit3[,22]+building_permit3[,25]+building_permit3[,28]
building_permit3[,36]<-building_permit3[,20]+building_permit3[,23]+building_permit3[,26]+building_permit3[,29]
building_permit3[,37]<-building_permit3[,21]+building_permit3[,24]+building_permit3[,27]+building_permit3[,30]
#average valuation per units (only reported)
building_permit3[,38]<-building_permit3[,37]/building_permit3[,36]
colnames(building_permit3)[31:38]<-c("Total_Buildings_Reported_Imputed","Total_Units_Reported_Imputed","Total_Valuation_Reported_Imputed","Average_Valuation_PerUnits_Reported_Imputed","Total_Buildings_Reported","Total_Units_Reported","Total_Valuation_Reported","Average_Valuation_PerUnits_Reported")
write.csv(building_permit3, file="buildingdata2.csv",row.names=FALSE)

#calculated the change in total units since 2000
year_2000<-building_permit3[which(building_permit3$C_year==2000),]
building_permit3$Total_Pct_Change<-round((building_permit3$Total_Units_Reported_Imputed/year_2000$Total_Units_Reported_Imputed[match(building_permit3$Region,year_2000$Region)]-1)*100,2)

#calculated the change in total units from previous year
building_permit4<-building_permit3
  

myFun <- function(x){
  n <- nrow(x)
  x$Change_from_previous <- c(NA,diff(x$Total_Units_Reported_Imputed) / head(x$Total_Units_Reported_Imputed,n-1))
  x
}

building_permit5<-do.call(rbind,by(building_permit4,building_permit4$Region,FUN=myFun))
building_permit5$Pct_Change_from_previous<-round(building_permit5$Change_from_previous*100,2)

##calculate the Percent of units by Class
building_permit5$Percentage_of_1_Family<-round(building_permit5[,8]/building_permit5$Total_Units_Reported_Imputed*100, 2)
building_permit5$Percentage_of_2_Family<-round(building_permit5[,11]/building_permit5$Total_Units_Reported_Imputed*100, 2)
building_permit5$Percentage_of_3_and_4_Family<-round(building_permit5[,14]/building_permit5$Total_Units_Reported_Imputed*100, 2)
building_permit5$Percentage_of_5_Family<-round(building_permit5[,17]/building_permit5$Total_Units_Reported_Imputed*100, 2)
write.csv(building_permit5, file="buildingdata3.csv",row.names=FALSE)

#load population data
population<-read.csv("Population_1930_2014.csv", skip=3, stringsAsFactors=F)
#select data year2000 and 2010
population1<-population%>%
  select(4,13:14)
colnames(population1)<-c("Area_Name","Year2000","Year2010")
population1$Year_2000<-gsub("\\,","",population1$Year2000)
population1$Year_2010<-gsub("\\,","",population1$Year2010)
population1$Area_Name2<-gsub(" town","",population1$Area_Name)
population1$Area_Name2<-gsub(" Town","",population1$Area_Name2)
population1$Area_Name2<-gsub(" city","",population1$Area_Name2)
population1$Area_Name2<-gsub("Massachusetts","MA",population1$Area_Name2)
population1<-population1[-c(381,382:391),]

population2<-population1[!apply(population1 == "", 1, all),]%>%
  arrange(Area_Name2)
population2$Year2000_per1000<-as.numeric(population2$Year_2000)/1000
population2$Year2010_per1000<-as.numeric(population2$Year_2010)/1000
write.csv(population2, file="BuildingPermits/populationdata.csv",row.names=FALSE)

##Calculate the Permits per 1000 population
##for year 2000-2009 using Year2000_per1000, for year2010 and 2011 using Year2010_per1000
building_permit6<-building_permit5
building_permit6$Permits_Per_1000_Population<-round(building_permit6$Total_Units_Reported_Imputed/population2$Year2000_per1000[match(building_permit6$Region,population2$Area_Name2)],2)
building_permit6$Permits_Per_1000_Population[which(building_permit6$C_year=="2010")]<-round(building_permit6$Total_Units_Reported_Imputed[which(building_permit6$C_year=="2010")]/population2$Year2010_per1000[match(building_permit6$Region[which(building_permit6$C_year=="2010")],population2$Area_Name2)],2)
building_permit6$Permits_Per_1000_Population[which(building_permit6$C_year=="2011")]<-round(building_permit6$Total_Units_Reported_Imputed[which(building_permit6$C_year=="2011")]/population2$Year2010_per1000[match(building_permit6$Region[which(building_permit6$C_year=="2011")],population2$Area_Name2)],2)
write.csv(building_permit6, file="buildingdata4.csv",row.names=FALSE)

##Select the varialbe needed
building_permit7<-building_permit6%>%
  select(c(39, 4:5, 7:18, 31:34, 40:53))
#Set Total_Pct_Change==Ifn as NA
idx_ifn<-which(building_permit7$Total_Pct_Change=="Inf")
building_permit7$Total_Pct_Change[idx_ifn]<-NA
building_permit7$Pct_Change_from_previous[which(building_permit7$Pct_Change_from_previous=="Inf")]<-NA
building_permit7$Percentage_of_1_Family[which(building_permit7$Total_Units_Reported_Imputed==0)]<-NA
building_permit7$Percentage_of_2_Family[which(building_permit7$Total_Units_Reported_Imputed==0)]<-NA
building_permit7$Percentage_of_3_and_4_Family[which(building_permit7$Total_Units_Reported_Imputed==0)]<-NA
building_permit7$Percentage_of_5_Family[which(building_permit7$Total_Units_Reported_Imputed==0)]<-NA
# Put MA at top of data set
idx_MA <- which(building_permit7$Region == "MA")
MA<-building_permit7[idx_MA,]
write.csv(building_permit7, file="BuildingPermits/buildingdata.csv",row.names=FALSE)

##################################################
