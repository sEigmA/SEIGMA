###########################################
## Title: Property Value Data Cleaning   ##
## Author(s): Xuelian Li                 ## 
## Date Created:  07/09/2016             ##
## Date Modified: 07/12/2016 XL          ##
###########################################

require(haven)
require(dplyr)
require(tidyr)
require(reshape)
require(readr)
      

## load SAS data
pV <- read_csv("AR001_02.csv")
pValue <- read_sas("ar001_01.sas7bdat")

inds <- which(is.na(pV$Year))
pV2 <- pV[-inds,]
pV2$Municipal <- pV2$NAME

df <- pValue[,1:3]

df$Municipal <- as.character(df$Municipal)
df$Municipal[df$Municipal=="Manchester-by-the-Sea"] <- "Manchester By The Sea"


inds <- seq(from=1, to=nrow(df), by = 11)
y <- df[inds,]

z <- y[rep(seq_len(nrow(y)), each=15),]
Year <- rep(print(2003:2017),351)
zy <- cbind(z, Year)
inds <- which(is.na(pV$Year))
pV2 <- pV[-inds,-1]
names(pV2)[1]<- "Municipal"

pValue <- full_join(zy,pV2)



## give columns relevant titles
column_titles <- read.csv("AR001_01_contents.csv", skip=1)
colnames(pValue)[5:12] <- as.character(column_titles$Label[1:8])
# remove unnecessary characters from column names

colnames(pValue)[5:12] <- substring(colnames(pValue)[5:12], first=12)
colnames(pValue)[5:12]<-gsub("in dollars", "", colnames(pValue)[5:12])
colnames(pValue)[5:12]<-gsub("()", "", colnames(pValue)[5:12], fixed = TRUE)
colnames(pValue)[5:10]<-gsub("Value", "", colnames(pValue)[5:10])
colnames(pValue) <- trimws(colnames(pValue), which = "right")

colnames(pValue)[5:10]<-gsub(" ", ".", colnames(pValue)[5:10])
pValue[,1:2]<-apply(pValue[,1:2],2, function(x) replace(x, x=="N/A", NA))

write.csv(pValue, file="pValueData2.csv",row.names=FALSE)

##calculate the inflation adjusted Residential NEED $ for 2013-2016
Adjusted_index<-data.frame(Year=2003:2017, Annual=c(184.0,188.9,195.3,201.6,207.342,215.303,214.537,218.056,224.939,229.594, 232.957,
                                                    236.736,237.017,240.007,240.007))
Adjusted_index$Inflation_rate<-240.007/Adjusted_index$Annual
pValue$Inflation_Adjusted_Residential<-rep(0,5265)
for (i in 2003:2017) {
  pValue$Inflation_Adjusted_Residential[which(pValue$Year==i)]<-pValue$Residential[which(pValue$Year==i)]*Adjusted_index$Inflation_rate[i-2002]
}
pValue$Inflation_Adjusted_Residential<-round(pValue$Inflation_Adjusted_Residential,0)

##calculate the inflation adjusted Open.Space
pValue$Inflation_Adjusted_Open_Space<-rep(0,5265)
for (i in 2003:2017) {
  pValue$Inflation_Adjusted_Open_Space[which(pValue$Year==i)]<-pValue$Open.Space[which(pValue$Year==i)]*Adjusted_index$Inflation_rate[i-2002]
}
pValue$Inflation_Adjusted_Open_Space<-round(pValue$Inflation_Adjusted_Open_Space,0)

##calculate the inflation adjusted Commercial
pValue$Inflation_Adjusted_Commercial<-rep(0,5265)
for (i in 2003:2017) {
  pValue$Inflation_Adjusted_Commercial[which(pValue$Year==i)]<-pValue$Commercial [which(pValue$Year==i)]*Adjusted_index$Inflation_rate[i-2002]
}
pValue$Inflation_Adjusted_Commercial<-round(pValue$Inflation_Adjusted_Commercial,0)

##calculate the inflation adjusted Industrial
pValue$Inflation_Adjusted_Industrial<-rep(0,5265)
for (i in 2003:2017) {
  pValue$Inflation_Adjusted_Industrial[which(pValue$Year==i)]<-pValue$Industrial[which(pValue$Year==i)]*Adjusted_index$Inflation_rate[i-2002]
}
pValue$Inflation_Adjusted_Industrial<-round(pValue$Inflation_Adjusted_Industrial,0)

##calculate the inflation adjusted Personal.Property
pValue$Inflation_Adjusted_Personal_Property<-rep(0,5265)
for (i in 2003:2017) {
  pValue$Inflation_Adjusted_Personal_Property[which(pValue$Year==i)]<-pValue$Personal.Property[which(pValue$Year==i)]*Adjusted_index$Inflation_rate[i-2002]
}
pValue$Inflation_Adjusted_Personal_Property<-round(pValue$Inflation_Adjusted_Personal_Property,0)

##calculate the inflation adjusted Total.Assessed
pValue$Inflation_Adjusted_Total_Assessed<-rep(0,5265)
for (i in 2003:2017) {
  pValue$Inflation_Adjusted_Total_Assessed[which(pValue$Year==i)]<-pValue$Total.Assessed[which(pValue$Year==i)]*Adjusted_index$Inflation_rate[i-2002]
}
pValue$Inflation_Adjusted_Total_Assessed<-round(pValue$Inflation_Adjusted_Total_Assessed,0)
write.csv(pValue, file="pValuedata3.csv",row.names=FALSE)

##calculate the Percent of Assessed by Class
pValue$Percentage_of_Residential<-round(pValue$Residential/pValue$Total.Assessed*100, 2)
pValue$Percentage_of_Open_Space<-round(pValue$Open.Space/pValue$Total.Assessed*100, 2)
pValue$Percentage_of_Commercial<-round(pValue$Commercial/pValue$Total.Assessed*100, 2)
pValue$Percentage_of_Industrial<-round(pValue$Industrial/pValue$Total.Assessed*100, 2)
pValue$Percentage_of_Personal_Property<-round(pValue$Personal.Property/pValue$Total.Assessed*100, 2)
write.csv(pValue, file="PropertyValue/pValuedata3.csv",row.names=FALSE)
##calculate the Total Assessed percent change since 2003
year_2003<-pValue[which(pValue$Year==2003),]
pValue$Total_Assessed_Pct_Change<-round((pValue$Inflation_Adjusted_Total_Assessed/year_2003$Inflation_Adjusted_Total_Assessed[match(pValue$Municipal,year_2003$Municipal)]-1)*100,1)
write.csv(pValue, file="PropertyValue/pValuedata3.csv",row.names=FALSE)
##put the label as x million
pValue$Total_Assessed_Million<-round(pValue$Inflation_Adjusted_Total_Assessed/1000000, 2)
pValue$Residential_Million<-round(pValue$Inflation_Adjusted_Residential/1000000, 2)
pValue$Open_Space_Million<-round(pValue$Inflation_Adjusted_Open_Space/1000000, 2)
pValue$Commercial_Million<-round(pValue$Inflation_Adjusted_Commercial/1000000, 2)
pValue$Industrial_Million<-round(pValue$Inflation_Adjusted_Industrial/1000000, 2)
pValue$Personal_Property_Million<-round(pValue$Inflation_Adjusted_Personal_Property/1000000, 2)
write.csv(pValue, file="PropertyValue/pValuedata3.csv",row.names=FALSE)
