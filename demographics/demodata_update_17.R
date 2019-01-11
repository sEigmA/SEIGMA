############################################
## Title: Demographics Data Update        ##
## Author(s): Valerie Evans               ## 
## Date Created:  11/09/2017              ##
## Date Modified: 01/10/2019              ##
############################################


require(dplyr)
require(tidyr)
require(readr)
require(stringr)


###############################################################################################################
## CREATE NEW DATASETS ##
###############################################################################################################

# set working directory
setwd("J:/Projects/Gambling/SEIGMA/Data Management Center/SECONDARY DATA/ACTIVE/SHINY DATA/Demographics/")

# load new datasets
ACS_16_5YR_DP05 <- read_csv("2012-2016/ACS_16_5YR_DP05.csv")
ACS_17_5YR_DP05 <- read_csv("2013-2017/ACS_17_5YR_DP05.csv")

# remove geo.id columns (1 and 2)
ACS_16_5YR_DP05$GEO.id <- NULL
ACS_16_5YR_DP05$GEO.id2 <- NULL
ACS_17_5YR_DP05$GEO.id <- NULL
ACS_17_5YR_DP05$GEO.id2 <- NULL

# choose columns we need (total population, percentage of age, percentage of sex, race, ethnicity)
ACS_16_5YR_DP05_2 <- ACS_16_5YR_DP05[,c('GEO.display-label', 'HC01_VC03', 'HC02_VC03', 'HC03_VC04', 'HC04_VC04', 
                                        'HC03_VC05', 'HC04_VC05', 'HC03_VC08', 'HC04_VC08', 'HC03_VC09', 
                                        'HC04_VC09', 'HC03_VC10', 'HC04_VC10', 'HC03_VC11', 'HC04_VC11', 
                                        'HC03_VC12', 'HC04_VC12', 'HC03_VC13', 'HC04_VC13', 'HC03_VC14', 
                                        'HC04_VC14', 'HC03_VC15', 'HC04_VC15', 'HC03_VC16', 'HC04_VC16', 
                                        'HC03_VC17', 'HC04_VC17', 'HC03_VC18', 'HC04_VC18', 'HC03_VC19', 
                                        'HC04_VC19', 'HC03_VC20', 'HC04_VC20', 'HC03_VC49', 'HC04_VC49', 
                                        'HC03_VC50', 'HC04_VC50', 'HC03_VC51', 'HC04_VC51', 'HC03_VC56', 
                                        'HC04_VC56', 'HC03_VC64', 'HC04_VC64', 'HC03_VC69', 'HC04_VC69', 
                                        'HC03_VC88', 'HC04_VC88', 'HC03_VC93', 'HC04_VC93')]
ACS_17_5YR_DP05_2 <- ACS_17_5YR_DP05[,c('GEO.display-label', 'HC01_VC03', 'HC02_VC03', 'HC03_VC04', 'HC04_VC04', 
                                        'HC03_VC05', 'HC04_VC05', 'HC03_VC09', 'HC04_VC09', 'HC03_VC10', 
                                        'HC04_VC10', 'HC03_VC11', 'HC04_VC11', 'HC03_VC12', 'HC04_VC12', 
                                        'HC03_VC13', 'HC04_VC13', 'HC03_VC14', 'HC04_VC14', 'HC03_VC15', 
                                        'HC04_VC15', 'HC03_VC16', 'HC04_VC16', 'HC03_VC17', 'HC04_VC17', 
                                        'HC03_VC18', 'HC04_VC18', 'HC03_VC19', 'HC04_VC19', 'HC03_VC20', 
                                        'HC04_VC20', 'HC03_VC21', 'HC04_VC21', 'HC03_VC54', 'HC04_VC54', 
                                        'HC03_VC55', 'HC04_VC55', 'HC03_VC56', 'HC04_VC56', 'HC03_VC61', 
                                        'HC04_VC61', 'HC03_VC69', 'HC04_VC69', 'HC03_VC74', 'HC04_VC74', 
                                        'HC03_VC93', 'HC04_VC93', 'HC03_VC98', 'HC04_VC98')]

# get column names and "delete" the first row
colnames(ACS_16_5YR_DP05_2) <- as.character(ACS_16_5YR_DP05_2[1,])
ACS_16_5YR_DP05_2 <- ACS_16_5YR_DP05_2[-1, ]
colnames(ACS_17_5YR_DP05_2) <- as.character(ACS_17_5YR_DP05_2[1,])
ACS_17_5YR_DP05_2 <- ACS_17_5YR_DP05_2[-1, ]

# add new column with five year ranges
ACS_16_5YR_DP05_2 <- cbind(Range = "2012-2016", ACS_16_5YR_DP05_2)
ACS_17_5YR_DP05_2 <- cbind(Range = "2013-2017", ACS_17_5YR_DP05_2)

# save new csv files
write.csv(file="2012-2016/BA003_02_16.csv", x=ACS_16_5YR_DP05_2)
write.csv(file="2013-2017/BA003_02_17.csv", x=ACS_17_5YR_DP05_2)


###############################################################################################################
## CLEAN DATASET ##
###############################################################################################################

# merge new data into one file (130 columns)
ACS_16_17 <- bind_rows(ACS_16_5YR_DP05_2, ACS_17_5YR_DP05_2)

# clean dataset and add municipal, county, state, and region columns
ACS_16_17_2 <- ACS_16_17 
ACS_16_17_2$State <- ifelse(grepl("Massachusetts", ACS_16_17_2$Geography), "MA", NA)
ACS_16_17_2$Geography <- str_replace(ACS_16_17_2$Geography, ", Massachusetts", "")
ACS_16_17_2$Geography <- gsub("Massachusetts", "MA", ACS_16_17_2$Geography)
ACS_16_17_2$Region = ACS_16_17_2$Geography
ACS_16_17_2 <- ACS_16_17_2[!grepl("subdivisions", ACS_16_17_2$Geography),]
ACS_16_17_2 <- ACS_16_17_2 %>% separate(Region, c("Region", "County"), sep = ", ")
ACS_16_17_2$Region <- str_replace(ACS_16_17_2$Region, " town", "")
ACS_16_17_2$Region <- str_replace(ACS_16_17_2$Region, " Town", "")
ACS_16_17_2$Region <- str_replace(ACS_16_17_2$Region, " city", "")
ACS_16_17_2$Region <- str_trim(ACS_16_17_2$Region)
ACS_16_17_2$Municipal = ACS_16_17_2$Region
ACS_16_17_2$County[is.na(ACS_16_17_2$County)] <- ACS_16_17_2$Region[is.na(ACS_16_17_2$County)]
ACS_16_17_2$Municipal <- gsub("United States", NA, ACS_16_17_2$Municipal)
ACS_16_17_2$County <- gsub("United States", NA, ACS_16_17_2$County)
ACS_16_17_2$Municipal <- gsub("MA", NA, ACS_16_17_2$Municipal)
ACS_16_17_2$County <- gsub("MA", NA, ACS_16_17_2$County)
ACS_16_17_2$Municipal <- gsub("County", NA, ACS_16_17_2$Municipal)
ACS_16_17_2$Geography = NULL
colnames(ACS_16_17_2)[colnames(ACS_16_17_2) == 'Range'] <- 'Five_Year_Range'
ACS_16_17_2 <- ACS_16_17_2[c(53, 52, 50, 51, 1, 2:49)]

# save new file
write.csv(file="demodata_update_17.csv", x=ACS_16_17_2)

# rename the columns
ACS_16_17_3 <- ACS_16_17_2
colnames(ACS_16_17_3)[6:53] <- c("Total_Population", "Margin_Error_Total_Population", "Male_Pct","Margin_Error_Male", 
                              "Female_Pct", "Margin_Error_Female", "Age_under_5_Pct", "Margin_Error_under_5_Pct", 
                              "Age_5_9_Pct", "Margin_Error_5_9_Pct", "Age_10_14_Pct", "Margin_Error_10_14_Pct", 
                              "Age_15_19_Pct", "Margin_Error_15_19_Pct", "Age_20_24_Pct", "Margin_Error_20_24_Pct", 
                              "Age_25_34_Pct", "Margin_Error_25_34_Pct", "Age_35_44_Pct", "Margin_Error_35_44_Pct", 
                              "Age_45_54_Pct", "Margin_Error_45_54_Pct", "Age_55_59_Pct", "Margin_Error_55_59_Pct", 
                              "Age_60_64_Pct", "Margin_Error_60_64_Pct", "Age_65_74_Pct", "Margin_Error_65_74_Pct", 
                              "Age_75_84_Pct", "Margin_Error_75_84_Pct", "Age_over_85_Pct", "Margin_Error_85._Pct", 
                              "White_Pct", "Margin_Error_White_Pct", "Black_Pct", "Margin_Error_Black_Pct", 
                              "American_Indian_and_Alaska_Native_Pct", "Margin_Error_American_Indian_and_Alaska_Native_Pct", 
                              "Asian_Pct", "Margin_Error_Asian_Pct", "Hawaiian_and_Other_Pacific_Islander_Pct", 
                              "Margin_Error_Hawaiian_and_Other_Pacific_Islander_Pct","Others_Pct", 
                              "Margin_Error_Others_Pct", "Hispanic_Pct", "Margin_Error_Hispanic_Pct", "Not_Hispanic_Pct", 
                              "Margin_Error_Not_Hispanic_Pct")

# convert renamed columns from character to numeric 
ACS_16_17_3[, 6:ncol(ACS_16_17_3)] <- lapply(6:ncol(ACS_16_17_3), function(x) as.numeric(ACS_16_17_3[[x]]))

# add 6 new variables for age "under_20_Pct, 20_34_Pct, 35_54_Pct, 55_64_Pct, 65_74_Pct, 75+_Pct"
ACS_16_17_3$Age_under_20_Pct_plot <- ACS_16_17_3$Age_under_5_Pct + ACS_16_17_3$Age_5_9_Pct + ACS_16_17_3$Age_10_14_Pct + 
  ACS_16_17_3$Age_15_19_Pct
ACS_16_17_3$Age_20_34_Pct_plot <- ACS_16_17_3$Age_20_24_Pct + ACS_16_17_3$Age_25_34_Pct
ACS_16_17_3$Age_35_54_Pct_plot <- ACS_16_17_3$Age_35_44_Pct + ACS_16_17_3$Age_45_54_Pct
ACS_16_17_3$Age_55_64_Pct_plot <- ACS_16_17_3$Age_55_59_Pct + ACS_16_17_3$Age_60_64_Pct
ACS_16_17_3$Age_65_74_Pct_plot <- ACS_16_17_3$Age_65_74_Pct
ACS_16_17_3$Age_over_75_Pct_plot <- ACS_16_17_3$Age_75_84_Pct + ACS_16_17_3$Age_over_85_Pct

# save new file
write.csv(ACS_16_17_3, file="demodata_update_17_2.csv", row.names=FALSE)


###############################################################################################################
## MERGE DATASETS ##
###############################################################################################################

# load old and new datasets
demomerge_all <- read_csv("demomerge_all.csv")
#ACS_16_17_3 <- read_csv("ACS_16_17_3.csv")

# replace ***** with NA for consistency 
demomerge_all[, 6:59][demomerge_all[, 6:59] == "*****"] <- NA

# convert character columns to numeric 
demomerge_all[, 6:ncol(demomerge_all)] <- lapply(6:ncol(demomerge_all), function(x) as.numeric(demomerge_all[[x]]))

# merge datasets
demomerge_all_17 <- rbind(demomerge_all, ACS_16_17_3)

# rearrange rows
demomerge_all_17 <- demomerge_all_17 %>% arrange(!is.na(Municipal), !is.na(County), !is.na(State), Municipal)

# save new merged dataset
write.csv(file="demomerge_all_17.csv", x=demomerge_all_17, row.names = FALSE)

