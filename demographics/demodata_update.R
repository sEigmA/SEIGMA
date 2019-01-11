############################################
## Title: Demographics Data Update        ##
## Author(s): Valerie Evans               ## 
## Date Created:  11/09/2017              ##
## Date Modified:                         ##
############################################


require(dplyr)
require(tidyr)
require(tm)
require(readr)

############################################################################################################################
## CLEAN NEW DATASETS ##
############################################################################################################################

# load new datasets
ACS_13_5YR_DP05 <- read_csv("ACS_13_5YR_DP05.csv")
ACS_14_5YR_DP05 <- read_csv("ACS_14_5YR_DP05.csv")
ACS_15_5YR_DP05 <- read_csv("ACS_15_5YR_DP05.csv")

# remove geo.id columns (1 and 2)
ACS_13_5YR_DP05$GEO.id <- NULL
ACS_13_5YR_DP05$GEO.id2 <- NULL
ACS_14_5YR_DP05$GEO.id <- NULL
ACS_14_5YR_DP05$GEO.id2 <- NULL
ACS_15_5YR_DP05$GEO.id <- NULL
ACS_15_5YR_DP05$GEO.id2 <- NULL

# add new column with five year ranges
ACS_13_5YR_DP05$dates <- rep(2009-2013,nrow(ACS_13_5YR_DP05))
ACS_14_5YR_DP05$dates <- rep(2010-2014,nrow(ACS_14_5YR_DP05))
ACS_15_5YR_DP05$dates <- rep(2011-2015,nrow(ACS_15_5YR_DP05))

# save new csv file
write.csv(file="BA003_02_13.csv", x=ACS_13_5YR_DP05)

# load new data to be merged with old data
BA003_02_13 <- read_csv("BA003_02_13.csv")
BA003_02_14 <- read_csv("BA003_02_14.csv")
BA003_02_15 <- read_csv("BA003_02_15.csv")

# merge datasets
demomerge <- rbind(BA003_02_13, BA003_02_14, BA003_02_15)

# save new merged dataset
write.csv(file="demomerge.csv", x=demomerge)

BA003_02_dataset <- read_csv("demomerge.csv")

# give columns relevant titles 
column_titles <- read.csv("BA003_02_contents.csv", skip=1)
colnames(BA003_02_dataset)[5:133] <- as.character(column_titles$Label[1:129])

# remove unnecessary characters from column names
colnames(BA003_02_dataset)[5:133] <- substring(colnames(BA003_02_dataset[5:133]), first=12)
colnames(BA003_02_dataset)[5:133] <- gsub("; SEX AND AGE", "", colnames(BA003_02_dataset[5:133]))

# choose columns we need (total population, percentage of age, percentage of sex, race, ethnicity)
BA003_02 <- BA003_02_dataset[,c(1:5,9,10,14,15,18,19,22,23,26,27,30,31,34,35,38,39,42,43,46,47,50,51,54,55,58,59,62,
                                63,66,67,70,71,100,101,104,105,108,109,112,113,116,117,120,121,124,125,128,129)]

# rename the columns
colnames(BA003_02)[5:53] <- c("Five_Year_Range", "Total_Population", "Margin_Error_Total_Population", "Male_Pct","Margin_Error_Male", 
                              "Female_Pct", "Margin_Error_Female", "Age_under_5_Pct", "Margin_Error_under_5_Pct", 
                              "Age_5_9_Pct", "Margin_Error_5_9_Pct", "Age_10_14_Pct", "Margin_Error_10_14_Pct", 
                              "Age_15_19_Pct", "Margin_Error_15_19_Pct", "Age_20_24_Pct", "Margin_Error_20_24_Pct", 
                              "Age_25_34_Pct", "Margin_Error_25_34_Pct", "Age_35_44_Pct", "Margin_Error_35_44_Pct", 
                              "Age_45_54_Pct", "Margin_Error_45_54_Pct", "Age_55_59_Pct", "Margin_Error_55_59_Pct", 
                              "Age_60_64_Pct", "Margin_Error_60_64_Pct", "Age_65_74_Pct", "Margin_Error_65_74_Pct", 
                              "Age_75_84_Pct", "Margin_Error_75_84_Pct", "Age_over_85_Pct", "Margin_Error_85._Pct", 
                              "White_Pct", "Margin_Error_White_Pct", "Black_Pct", "Margin_Error_Black_Pct", 
                              "American_Indian_and_Alaska_Native_Pct", "Margin_Error_American_Indian_and_Alaska_Native_Pct", 
                              "Asian_Pct", "Margin_Error_Asian_Pct",
                              "Hawaiian_and_Other_Pacific_Islander_Pct", "Margin_Error_Hawaiian_and_Other_Pacific_Islander_Pct",
                              "Others_Ptc", "Margin_Error_Others_Pct", "Hispanic_Pct", "Margin_Error_Hispanic_Pct", 
                              "Not_Hispanic_Pct", "Margin_Error_Not_Hispanic_Pct")

# add 6 new variables for age "under_20_Pct, 20_34_Pct, 35_54_Pct, 55_64_Pct, 65_74_Pct, 75+_Pct"
BA003_02$Age_under_20_Pct_plot <- BA003_02$Age_under_5_Pct + BA003_02$Age_5_9_Pct + BA003_02$Age_10_14_Pct + BA003_02$Age_15_19_Pct
BA003_02$Age_20_34_Pct_plot <- BA003_02$Age_20_24_Pct + BA003_02$Age_25_34_Pct
BA003_02$Age_35_54_Pct_plot <- BA003_02$Age_35_44_Pct + BA003_02$Age_45_54_Pct
BA003_02$Age_55_64_Pct_plot <- BA003_02$Age_55_59_Pct + BA003_02$Age_60_64_Pct
BA003_02$Age_65_74_Pct_plot <- BA003_02$Age_65_74_Pct
BA003_02$Age_over_75_Pct_plot <- BA003_02$Age_75_84_Pct + BA003_02$Age_over_85_Pct

write.csv(BA003_02, file="demographics2.csv", row.names=FALSE)

# save new file
write.csv(file="BA003_02.csv", x=BA003_02)

##################################################################################################################################
## MERGE DATASETS ##
##################################################################################################################################

# load old and new datasets
demodata_backup <- read_csv("demodata_backup.csv")
BA003_02_13_15 <- read_csv("BA003_02.csv")

# merge datasets
demomerge_all <- rbind(demodata_backup, BA003_02_13_15)

# fix error message "Error in match.names(clabs, names(xi)) : names do not match previous names" and rerun previous code
colnames(BA003_02_13_15) <- colnames(demodata_backup)

# save new merged dataset
write.csv(file="demomerge_all.csv", x=demomerge_all)