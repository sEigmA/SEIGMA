setwd("C:\\Users\\Justin\\Desktop\\Research\\SEIGMA\\Github SEIGMA\\SEIGMA\\rent")
require(sas7bdat)
rent<-read.sas7bdat("AR003_02_5yr.sas7bdat")
column_titles <- read.csv("AR003_02_5yr_contents.csv", skip=1)
colnames(rent)[5:10]<-as.character(column_titles$Label[1:6])
rent<-rent[,c(1,2,7:10)]

rent$Municipal<-as.character(rent$Municipal)
rent$County<-as.character(rent$County)
rent$Municipal[rent$`AR003_003: Geography`=="Massachusetts"]<-"MA"
rent$County[rent$`AR003_003: Geography`=="Massachusetts"]<-"MA"
rent$Municipal[rent$`AR003_003: Geography`=="United States"]<-"USA"
rent$County[rent$`AR003_003: Geography`=="United States"]<-"USA"
rent$Municipal[rent$Municipal=="N/A"]<-paste(rent$County[rent$Municipal=="N/A"], "County")

colnames(rent)[4:6]<-c("Five Year Range", "Median Rent", "Rent Margin of Error")
rent<-rent[,-3]

#only one var, NA in that var means unusable data
rent<-rent[is.na(rent$`Median Rent`)==F,]
rent<-rent[order(rent$County),]


write.csv(rent, "rent.csv", row.names=F)
