############################################
## Title: Educational Attainment Update   ##
## Author(s): Valerie Evans               ## 
## Date Created:  10/19/2017              ##
## Date Modified:                         ##
############################################


require(dplyr)
require(tidyr)
require(tm)
require(readr)


# set working directory
setwd("ASC Table DP02")


############################################################################################################################
## CLEAN NEW DATASETS ##
############################################################################################################################

# load new datasets
ACS13 <- read.csv("ACS_13_5YR_DP02_with_ann.csv")
ACS14 <- read.csv("ACS_14_5YR_DP02_with_ann.csv")
ACS15 <- read.csv("ACS_15_5YR_DP02_with_ann.csv")

# select relevant columns
edu13 <- select(ACS13, "GEO.display.label", "HC01_VC85", "HC02_VC85", "HC03_VC95", "HC04_VC95", "HC03_VC96", "HC04_VC96", "HC03_VC92", "HC04_VC92")
edu14 <- select(ACS14, "GEO.display.label", "HC01_VC85", "HC02_VC85", "HC03_VC95", "HC04_VC95", "HC03_VC96", "HC04_VC96", "HC03_VC92", "HC04_VC92")
edu15 <- select(ACS15, "GEO.display.label", "HC01_VC85", "HC02_VC85", "HC03_VC95", "HC04_VC95", "HC03_VC96", "HC04_VC96", "HC03_VC92", "HC04_VC92")

# remove first row
edu13 <- edu13[-c(1),]
edu14 <- edu14[-c(1),]
edu15 <- edu15[-c(1),]

# remove rows with 'County subdivisions not defined'
edu13 <- dplyr::filter(edu13, !grepl("County subdivisions not defined", GEO.display.label))
edu14 <- dplyr::filter(edu14, !grepl("County subdivisions not defined", GEO.display.label))
edu15 <- dplyr::filter(edu15, !grepl("County subdivisions not defined", GEO.display.label))

# rename columns
colnames(edu13) <- c("Region", "Pop_25", "Margin_Error_Pop", "HS_Pct", "Margin_Error_HS", "Bachelors_Pct", "Margin_Error_Bach", "Grad_Pct", "Margin_Error_Grad")
colnames(edu14) <- c("Region", "Pop_25", "Margin_Error_Pop", "HS_Pct", "Margin_Error_HS", "Bachelors_Pct", "Margin_Error_Bach", "Grad_Pct", "Margin_Error_Grad")
colnames(edu15) <- c("Region", "Pop_25", "Margin_Error_Pop", "HS_Pct", "Margin_Error_HS", "Bachelors_Pct", "Margin_Error_Bach", "Grad_Pct", "Margin_Error_Grad")


##############################################################################################################################
# split geography column into 4: Municipal, County, State, Region
edu13$Region <- gsub("Massachusetts", "\\MA", edu13$Region)
edu13 <- separate(edu13, "Region", c("Municipal", "County", "State"), ",", remove=FALSE, extra="merge", fill="left")
edu13$Municipal <- removeWords(edu13$Municipal, "town")
edu13$Municipal <- removeWords(edu13$Municipal, "city")
edu13$Municipal <- removeWords(edu13$Municipal, "Town")
edu13$State[edu13$State == "United States"] <- NA
edu13$County[is.na(edu13$County)] <- "NA County"
edu13$Region <- gsub("(.*),.*", "\\1", edu13$Region) # run twice to remove all text after commas
edu13$Region <- removeWords(edu13$Region, "town")
edu13$Region <- removeWords(edu13$Region, "city")
edu13$Region <- removeWords(edu13$Region, "Town")

# add columns with year range
edu13$Five_Year_Range <- rep("2009-2013", nrow(edu13))

# order columns by indexing
edu13 <- edu13[c(2,3,4,1,13,5,6,7,8,9,10,11,12)]


################################################################################################################################
# repeat above column split for each dataset (edu14 and edu15)
edu14$Region <- gsub("Massachusetts", "\\MA", edu14$Region)
edu14 <- separate(edu14, "Region", c("Municipal", "County", "State"), ",", remove=FALSE, extra="merge", fill="left")
edu14$Municipal <- removeWords(edu14$Municipal, "town")
edu14$Municipal <- removeWords(edu14$Municipal, "city")
edu14$Municipal <- removeWords(edu14$Municipal, "Town")
edu14$State[edu14$State == "United States"] <- NA
edu14$County[is.na(edu14$County)] <- "NA County"
edu14$Region <- gsub("(.*),.*", "\\1", edu14$Region) # run twice to remove all text after commas
edu14$Region <- gsub("(.*),.*", "\\1", edu14$Region)
edu14$Region <- removeWords(edu14$Region, "town")
edu14$Region <- removeWords(edu14$Region, "city")
edu14$Region <- removeWords(edu14$Region, "Town")

# add columns with year range
edu14$Five_Year_Range <- rep("2010-2014", nrow(edu14))

# order columns by indexing
edu14 <- edu14[c(2,3,4,1,13,5,6,7,8,9,10,11,12)]


#################################################################################################################################
edu15$Region <- gsub("Massachusetts", "\\MA", edu15$Region)
edu15 <- separate(edu15, "Region", c("Municipal", "County", "State"), ",", remove=FALSE, extra="merge", fill="left")
edu15$Municipal <- removeWords(edu15$Municipal, "town")
edu15$Municipal <- removeWords(edu15$Municipal, "city")
edu15$Municipal <- removeWords(edu15$Municipal, "Town")
edu15$State[edu15$State == "United States"] <- NA
edu15$County[is.na(edu15$County)] <- "NA County"
edu15$Region <- gsub("(.*),.*", "\\1", edu13$Region) # run twice to remove all text after commas
edu15$Region <- gsub("(.*),.*", "\\1", edu13$Region) 
edu15$Region <- removeWords(edu15$Region, "town")
edu15$Region <- removeWords(edu15$Region, "city")
edu15$Region <- removeWords(edu15$Region, "Town")


# add columns with year range
edu15$Five_Year_Range <- rep("2011-2015", nrow(edu15))

# order columns by indexing
edu15 <- edu15[c(2,3,4,1,13,5,6,7,8,9,10,11,12)]


#################################################################################################################################
# save new datasets as csv files
write.csv(file="edu13.csv", x=edu13)
write.csv(file="edu14.csv", x=edu14)
write.csv(file="edu15.csv", x=edu15)


##################################################################################################################################
## MERGE DATASETS ##
##################################################################################################################################


# merge new datasets
edu_13_15 <- rbind(edu13, edu14, edu15)

# save new merged dataset
write.csv(file="edu_13_15.csv", x=edu_13_15) 

# import old and new datasets
edudata_backup <- read_csv("edudata_backup.csv")
edumerge <- read_csv("edu_13_15.csv")

# delete first columns [X1] 
edudata_backup[1] <- NULL 
edumerge[1] <- NULL

# merge datasets
edumerge_all <- rbind(edudata_backup, edumerge)

# save new merged dataset
write.csv(file="edumerge_all.csv", x=edumerge_all)

