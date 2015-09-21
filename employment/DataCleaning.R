###########################################
## Title: employment Data Cleaning       ##
## Author(s): Xuelian Li, Emily Ramos,   ## 
##            Arvind Ramakrishnan,       ##
##            Jenna Kiridly, Steve Lauer ## 
## Date Created:  02/04/2015             ##
## Date Modified: 07/13/2015             ##
###########################################

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
emp<-read.csv(file="empdata1.csv")

## grab only columns needed
emp_data <- emp[,c(1:3,7:9,12,25)]

#Keep only Average Monthly employment and each monthly employment of all industries for the year
emp_data1 <- emp_data[which(emp_data[,4]=="Total, All Industries"),]

write.csv(emp_data1, file="empdata2.csv",row.names=FALSE)

## Replace N/A's with "NA" to remove the slash.
emp_data2 <- emp_data1
emp_data2[,1:2]<-apply(emp_data2[,1:2],2, function(x) replace(x, x=="N/A", NA))
##No NA in Municipal

# grab only the columns that we want: 
emp_data3 <- emp_data2[,-4]


colnames(emp_data3)[c(3:7)] <- c("Year" , "Average_Monthly_Employment","Average_Weekly_Wage","Number_of_Employer_Establishments",
                                  "Total_Wages_Paid_to_All_Workers")

## save data
write.csv(emp_data3, file="employment/empdata1.csv",row.names=FALSE)

##calculate the inflation adjusted Wage
Adjusted_index<-data.frame(Year=2001:2012, Annual=c(177.1,179.9,184.0,188.9,195.3,201.6,207.342,215.303,214.537,218.056,224.939,229.594))
Adjusted_index$Inflation_rate<-229.594/Adjusted_index$Annual
emp_data3$Inflation_Adjusted_Average_Weekly_Wage<-rep(0,4180)
for (i in 2001:2012) {
  emp_data3$Inflation_Adjusted_Average_Weekly_Wage[which(emp_data3$Year==i)]<-emp_data3$Average_Weekly_Wage[which(emp_data3$Year==i)]*Adjusted_index$Inflation_rate[i-2000]
}
emp_data3$Inflation_Adjusted_Average_Weekly_Wage<-round(emp_data3$Inflation_Adjusted_Average_Weekly_Wage,0)
  
##calculate the employment percentage change since 2001
emp_data4<-emp_data3[order(emp_data3$Municipal),]
year_01<-emp_data4[which(emp_data4$Year==2001),]
emp_data4$Change<-emp_data4$Average_Monthly_Employment/year_01$Average_Monthly_Employment[match(emp_data4$Municipal,year_01$Municipal)]
##"Hancock" (2003-2012),"Leyden"(2006-2012), "Mount Washington"(2003-2012), "Peru"(2002-2012), so the Change_Pct for these town are NA
emp_data4$Change[which(emp_data4$Municipal=="Hancock")]<-emp_data4$Average_Monthly_Employment[which(emp_data4$Municipal=="Hancock")]/emp_data4$Average_Monthly_Employment[which(emp_data4$Municipal=="Hancock"&emp_data4$Year==2003)]
emp_data4$Change[which(emp_data4$Municipal=="Peru")]<-emp_data4$Average_Monthly_Employment[which(emp_data4$Municipal=="Peru")]/emp_data4$Average_Monthly_Employment[which(emp_data4$Municipal=="Peru"&emp_data4$Year==2002)]
emp_data4$Change[which(emp_data4$Municipal=="Leyden")]<-emp_data4$Average_Monthly_Employment[which(emp_data4$Municipal=="Leyden")]/emp_data4$Average_Monthly_Employment[which(emp_data4$Municipal=="Leyden"&emp_data4$Year==2006)]
emp_data4$Change[which(emp_data4$Municipal=="Mount Washington")]<-emp_data4$Average_Monthly_Employment[which(emp_data4$Municipal=="Mount Washington")]/emp_data4$Average_Monthly_Employment[which(emp_data4$Municipal=="Mount Washington"&emp_data4$Year==2003)]
emp_data4$Change_Pct<-emp_data4$Change*100
colnames(emp_data4)[10]<-"Employment_Change_Pct"

##calculate the Establishment percentage change since 2001
emp_data4$Establishment_Change<-emp_data4$Number_of_Employer_Establishments/year_01$Number_of_Employer_Establishments[match(emp_data4$Municipal,year_01$Municipal)]
##"Hancock" (2003-2012),"Leyden"(2006-2012), "Mount Washington"(2003-2012), "Peru"(2002-2012), so the Change_Pct for these town are NA
emp_data4$Establishment_Change[which(emp_data4$Municipal=="Hancock")]<-emp_data4$Number_of_Employer_Establishments[which(emp_data4$Municipal=="Hancock")]/emp_data4$Number_of_Employer_Establishments[which(emp_data4$Municipal=="Hancock"&emp_data4$Year==2003)]
emp_data4$Establishment_Change[which(emp_data4$Municipal=="Peru")]<-emp_data4$Number_of_Employer_Establishments[which(emp_data4$Municipal=="Peru")]/emp_data4$Number_of_Employer_Establishments[which(emp_data4$Municipal=="Peru"&emp_data4$Year==2002)]
emp_data4$Establishment_Change[which(emp_data4$Municipal=="Leyden")]<-emp_data4$Number_of_Employer_Establishments[which(emp_data4$Municipal=="Leyden")]/emp_data4$Number_of_Employer_Establishments[which(emp_data4$Municipal=="Leyden"&emp_data4$Year==2006)]
emp_data4$Establishment_Change[which(emp_data4$Municipal=="Mount Washington")]<-emp_data4$Number_of_Employer_Establishments[which(emp_data4$Municipal=="Mount Washington")]/emp_data4$Number_of_Employer_Establishments[which(emp_data4$Municipal=="Mount Washington"&emp_data4$Year==2003)]
emp_data4$Establishment_Change_Pct<-emp_data4$Establishment_Change*100

##calculate the Average Weely wages percentage change since 2001
emp_data4$Average_Weekly_Wage_Change<-emp_data4$Inflation_Adjusted_Average_Weekly_Wage/year_01$Inflation_Adjusted_Average_Weekly_Wage[match(emp_data4$Municipal,year_01$Municipal)]
##"Hancock" (2003-2012),"Leyden"(2006-2012), "Mount Washington"(2003-2012), "Peru"(2002-2012), so the Change_Pct for these town are NA
emp_data4$Average_Weekly_Wage_Change[which(emp_data4$Municipal=="Hancock")]<-emp_data4$Inflation_Adjusted_Average_Weekly_Wage[which(emp_data4$Municipal=="Hancock")]/emp_data4$Inflation_Adjusted_Average_Weekly_Wage[which(emp_data4$Municipal=="Hancock"&emp_data4$Year==2003)]
emp_data4$Average_Weekly_Wage_Change[which(emp_data4$Municipal=="Peru")]<-emp_data4$Inflation_Adjusted_Average_Weekly_Wage[which(emp_data4$Municipal=="Peru")]/emp_data4$Inflation_Adjusted_Average_Weekly_Wage[which(emp_data4$Municipal=="Peru"&emp_data4$Year==2002)]
emp_data4$Average_Weekly_Wage_Change[which(emp_data4$Municipal=="Leyden")]<-emp_data4$Inflation_Adjusted_Average_Weekly_Wage[which(emp_data4$Municipal=="Leyden")]/emp_data4$Inflation_Adjusted_Average_Weekly_Wage[which(emp_data4$Municipal=="Leyden"&emp_data4$Year==2006)]
emp_data4$Average_Weekly_Wage_Change[which(emp_data4$Municipal=="Mount Washington")]<-emp_data4$Inflation_Adjusted_Average_Weekly_Wage[which(emp_data4$Municipal=="Mount Washington")]/emp_data4$Inflation_Adjusted_Average_Weekly_Wage[which(emp_data4$Municipal=="Mount Washington"&emp_data4$Year==2003)]
emp_data4$Average_Weekly_Wage_Change_Pct<-emp_data4$Average_Weekly_Wage_Change*100


write.csv(emp_data4, file="employment/empdata2.csv",row.names=FALSE)

##Only keep data since 2003
emp_data5<-emp_data3%>%
  filter(Year>=2003)
##calculate the employment percentage change since 2003
emp_data6<-emp_data5[order(emp_data5$Municipal),]
year_03<-emp_data6[which(emp_data6$Year==2003),]
emp_data6$Change<-emp_data6$Average_Monthly_Employment/year_03$Average_Monthly_Employment[match(emp_data6$Municipal,year_03$Municipal)]
##"Tolland" (2001,2005-2012),"Leyden"(2006-2012), "Savoy"(2001,2007-2009,2012), so the Change_Pct for these town are NA
emp_data6$Change[which(emp_data6$Municipal=="Tolland")]<-emp_data6$Average_Monthly_Employment[which(emp_data6$Municipal=="Tolland")]/emp_data6$Average_Monthly_Employment[which(emp_data6$Municipal=="Tolland"&emp_data6$Year==2005)]
emp_data6$Change[which(emp_data6$Municipal=="Leyden")]<-emp_data6$Average_Monthly_Employment[which(emp_data6$Municipal=="Leyden")]/emp_data6$Average_Monthly_Employment[which(emp_data6$Municipal=="Leyden"&emp_data6$Year==2006)]
emp_data6$Change[which(emp_data6$Municipal=="Savoy")]<-emp_data6$Average_Monthly_Employment[which(emp_data6$Municipal=="Savoy")]/emp_data6$Average_Monthly_Employment[which(emp_data6$Municipal=="Savoy"&emp_data6$Year==2007)]
emp_data6$Change_Pct<-round(emp_data6$Change*100,1)
colnames(emp_data6)[10]<-"Employment_Change_Pct"

##calculate the Establishment percentage change since 2003
emp_data6$Establishment_Change<-emp_data6$Number_of_Employer_Establishments/year_03$Number_of_Employer_Establishments[match(emp_data6$Municipal,year_03$Municipal)]
##"Tolland" (2001,2005-2012),"Leyden"(2006-2012), "Savoy"(2001,2007-2009,2012), so the Change_Pct for these town are NA
emp_data6$Establishment_Change[which(emp_data6$Municipal=="Tolland")]<-emp_data6$Number_of_Employer_Establishments[which(emp_data6$Municipal=="Tolland")]/emp_data6$Number_of_Employer_Establishments[which(emp_data6$Municipal=="Tolland"&emp_data6$Year==2005)]
emp_data6$Establishment_Change[which(emp_data6$Municipal=="Leyden")]<-emp_data6$Number_of_Employer_Establishments[which(emp_data6$Municipal=="Leyden")]/emp_data6$Number_of_Employer_Establishments[which(emp_data6$Municipal=="Leyden"&emp_data6$Year==2006)]
emp_data6$Establishment_Change[which(emp_data6$Municipal=="Savoy")]<-emp_data6$Number_of_Employer_Establishments[which(emp_data6$Municipal=="Savoy")]/emp_data6$Number_of_Employer_Establishments[which(emp_data6$Municipal=="Savoy"&emp_data6$Year==2007)]
emp_data6$Establishment_Change_Pct<-round(emp_data6$Establishment_Change*100,1)

##calculate the Average Weely wages percentage change since 2003
emp_data6$Average_Weekly_Wage_Change<-emp_data6$Inflation_Adjusted_Average_Weekly_Wage/year_03$Inflation_Adjusted_Average_Weekly_Wage[match(emp_data6$Municipal,year_03$Municipal)]
##"Tolland" (2001,2005-2012),"Leyden"(2006-2012), "Savoy"(2001,2007-2009,2012), so the Change_Pct for these town are NA
emp_data6$Average_Weekly_Wage_Change[which(emp_data6$Municipal=="Tolland")]<-emp_data6$Inflation_Adjusted_Average_Weekly_Wage[which(emp_data6$Municipal=="Tolland")]/emp_data6$Inflation_Adjusted_Average_Weekly_Wage[which(emp_data6$Municipal=="Tolland"&emp_data6$Year==2005)]
emp_data6$Average_Weekly_Wage_Change[which(emp_data6$Municipal=="Leyden")]<-emp_data6$Inflation_Adjusted_Average_Weekly_Wage[which(emp_data6$Municipal=="Leyden")]/emp_data6$Inflation_Adjusted_Average_Weekly_Wage[which(emp_data6$Municipal=="Leyden"&emp_data6$Year==2006)]
emp_data6$Average_Weekly_Wage_Change[which(emp_data6$Municipal=="Savoy")]<-emp_data6$Inflation_Adjusted_Average_Weekly_Wage[which(emp_data6$Municipal=="Savoy")]/emp_data6$Inflation_Adjusted_Average_Weekly_Wage[which(emp_data6$Municipal=="Savoy"&emp_data6$Year==2007)]
emp_data6$Average_Weekly_Wage_Change_Pct<-round(emp_data6$Average_Weekly_Wage_Change*100,1)

##Calculate the difference since 2003
emp_data6$Employment_difference<-round(emp_data6$Employment_Change_Pct-100,1)
emp_data6$Establishment_difference<-round(emp_data6$Establishment_Change_Pct-100,1)
emp_data6$Average_Weekly_Wage_difference<-round(emp_data6$Average_Weekly_Wage_Change_Pct-100,1)

write.csv(emp_data6, file="employment/empdata3.csv",row.names=FALSE)
