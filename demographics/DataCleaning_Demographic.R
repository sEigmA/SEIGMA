#######################################
## Title: Demographic Data Cleaning  ##
## Author(s): Xuelian Li,  Arvind    ##
##            Ramakrishnan, Jenna    ##
##            Kiridly, Emily Ramos,  ##
##                                   ##
## Date Created:  01/29/2015         ##
## Date Modified: 02/27/2015         ##
#######################################

require(sas7bdat)

## load SAS data
Demographic <- read.sas7bdat("ba003_01_5yr.sas7bdat")

## give columns relevant titles
column_titles <- read.csv("BA003_01_5yr_contents.csv", skip=1)
colnames(Demographic)[5:133] <- as.character(column_titles$Label[1:129])

## remove unnecessary characters from column names
colnames(Demographic)[5:133] <- substring(colnames(Demographic[5:133]), first=12)
colnames(Demographic)[5:133]<-gsub("; SEX AND AGE", "", colnames(Demographic[5:133]))

##Choose cloumn we need(total population(9-10),Percentage of Age(38-71) and percentage of Sex(14,15,18,19), Race(100-121), Ethnicity(124-129))
Dem_data<-Demographic[,c(1:5,9,10,14,15,18,19,22,23,26,27,30,31,34,35,38,39,42,43,46,47,50,51,54,55,58,59,62,63,66,67,70,71,
                         100,101,104,105,108,109,112,113,116,117,120,121,124,125,128,129)]

## Replace N/A's with "NA" to remove the slash.
Dem_data2<-Dem_data
Dem_data2[,1:4]<-apply(Dem_data2[,1:4],2, function(x) replace(x, x=="N/A", NA))

Dem_data2$County  <- ifelse(!is.na(Dem_data2$County), paste(Dem_data2$County, "County"), NA)
Dem_data2$Region  <- ifelse(!is.na(Dem_data2$Region), "United States", "MA")
Dem_data2$Region  <- ifelse(!is.na(Dem_data2$County), Dem_data2$County, Dem_data2$Region)
Dem_data2$Region  <- ifelse(!is.na(Dem_data2$Municipal),as.character(Dem_data2$Municipal),as.character(Dem_data2$Region))

##Rename the columns
Dem_data3<-Dem_data2
colnames(Dem_data3)[5:53] <- c("Five_Year_Range","Total_Population","Margin_Error_Total_Population","Male_Pct", "Margin_Error_Male","Female_Pct", "Margin_Error_Female",
                               "Age_under_5_Pct","Margin_Error_under_5_Pct","Age_5_9_Pct", "Margin_Error_5_9_Pct",
                               "Age_10_14_Pct","Margin_Error_10_14_Pct","Age_15_19_Pct", "Margin_Error_15_19_Pct",
                               "Age_20_24_Pct","Margin_Error_20_24_Pct","Age_25_34_Pct", "Margin_Error_25_34_Pct",
                               "Age_35_44_Pct", "Margin_Error_35_44_Pct","Age_45_54_Pct", "Margin_Error_45_54_Pct",
                               "Age_55_59_Pct", "Margin_Error_55_59_Pct","Age_60_64_Pct", "Margin_Error_60_64_Pct",
                               "Age_65_74_Pct", "Margin_Error_65_74_Pct","Age_75_84_Pct", "Margin_Error_75_84_Pct",
                               "Age_over_85_Pct", "Margin_Error_85+_Pct","White_Pct","Margin_Error_White_Pct",
                               "Black_Pct","Margin_Error_Black_Pct","American_Indian_and_Alaska_Native_Pct","Margin_Error_American_Indian_and_Alaska_Native_Pct",
                               "Asian_Pct","Margin_Error_Asian_Pct","Hawaiian_and_Other_Pacific_Islander_Pct","Margin_Error_Hawaiian_and_Other_Pacific_Islander_Pct",
                               "Others_Pct","Margin_Error_Others_Pct","Hispanic_Pct","Margin_Error_Hispanic_Pct",
                               "Not_Hispanic_Pct", "Margin_Error_Not_Hispanic_Pct")

## save and reload to make factors into numeric
write.csv(Dem_data3, file="demodata1.csv",row.names=FALSE)
Dem_data4 <- read.csv(file="demodata1.csv")

#Add 6 new variables for age "under_20_Pct, 20_34_Pct, 35_54_Pct, 55_64_Pct, 65_74_Pct, 75+_Pct"
Dem_data4$Age_under_20_Pct_plot<-Dem_data4$Age_under_5_Pct + Dem_data4$Age_5_9_Pct + Dem_data4$Age_10_14_Pct + Dem_data4$Age_15_19_Pct
Dem_data4$Age_20_34_Pct_plot<-Dem_data4$Age_20_24_Pct + Dem_data4$Age_25_34_Pct
Dem_data4$Age_35_54_Pct_plot<-Dem_data4$Age_35_44_Pct + Dem_data4$Age_45_54_Pct
Dem_data4$Age_55_64_Pct_plot<-Dem_data4$Age_55_59_Pct + Dem_data4$Age_60_64_Pct
Dem_data4$Age_65_74_Pct_plot<-Dem_data4$Age_65_74_Pct
Dem_data4$Age_over_75_Pct_plot<-Dem_data4$Age_75_84_Pct + Dem_data4$Age_over_85_Pct

write.csv(Dem_data4, file="demographics/demodata.csv",row.names=FALSE)

#Organizing Region
Dem_data5 <- Dem_data4[!is.na(Dem_data4$County),]
Dem_data5<-Dem_data5[order(Dem_data5$Region),]
idx_MA <- which(Dem_data4$Region == "MA")
idx_US <- which(Dem_data4$Region == "United States")
Dem_data6 <- rbind.data.frame(Dem_data4[idx_US,],Dem_data4[idx_MA,], Dem_data5)


write.csv(Dem_data6, file="demographics/demodata1.csv",row.names=FALSE)


