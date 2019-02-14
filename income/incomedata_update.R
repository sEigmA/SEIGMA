############################################
## Title: Household Income Data Update    ##
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
ACS13 <- read.csv("ACS_13_5YR_B19013.csv")
ACS14 <- read.csv("ACS_14_5YR_B19013.csv")
ACS15 <- read.csv("ACS_15_5YR_B19013.csv")

# select relevant columns
income13 <- select(ACS13, "GEO.display.label", "HD01_VD01", "HD02_VD01")
income14 <- select(ACS14, "GEO.display.label", "HD01_VD01", "HD02_VD01")
income15 <- select(ACS15, "GEO.display.label", "HD01_VD01", "HD02_VD01")

# remove rows with 'County subdivisions not defined'
income13 <- dplyr::filter(income13, !grepl("County subdivisions not defined", GEO.display.label))
income14 <- dplyr::filter(income14, !grepl("County subdivisions not defined", GEO.display.label))
income15 <- dplyr::filter(income15, !grepl("County subdivisions not defined", GEO.display.label))

# rename columns
colnames(income13) <- c("Region", "Median_Annual_Household_Income", "Margin_Error_Median")
colnames(income14) <- c("Region", "Median_Annual_Household_Income", "Margin_Error_Median")
colnames(income15) <- c("Region", "Median_Annual_Household_Income", "Margin_Error_Median")

# remove first row
income13 <- income13[-c(1),]
income14 <- income14[-c(1),]
income15 <- income15[-c(1),]

##############################################################################################################################
# split geography column into 4: Municipal, County, State, Region
income13$Region <- gsub("Massachusetts", "\\MA", income13$Region)
income13 <- separate(income13, "Region", c("Municipal", "County", "State"), ",", remove=FALSE, extra="merge", fill="left")
income13$Municipal <- removeWords(income13$Municipal, "town")
income13$Municipal <- removeWords(income13$Municipal, "city")
income13$Municipal <- removeWords(income13$Municipal, "Town")
income13$State[income13$State == "United States"] <- NA
income13$County[is.na(income13$County)] <- "NA County"
income13$Region <- gsub("(.*),.*", "\\1", income13$Region) 
income13$Region <- gsub("(.*),.*", "\\1", income13$Region) # run twice to remove all text after commas
income13$Region <- removeWords(income13$Region, "town")
income13$Region <- removeWords(income13$Region, "city")
income13$Region <- removeWords(income13$Region, "Town")

# add columns with year range
income13$Five_Year_Range <- rep("2009-2013", nrow(income13))

# add columns with five year average
income13$Five_Year_Average <-rep("2011", nrow(income13))

# order columns by indexing
income13 <- income13[c(2,3,4,1,7,8,5,6)]

################################################################################################################################
# repeat above column split for each dataset (income14 and income15)
income14$Region <- gsub("Massachusetts", "\\MA", income14$Region)
income14 <- separate(income14, "Region", c("Municipal", "County", "State"), ",", remove=FALSE, extra="merge", fill="left")
income14$Municipal <- removeWords(income14$Municipal, "town")
income14$Municipal <- removeWords(income14$Municipal, "city")
income14$Municipal <- removeWords(income14$Municipal, "Town")
income14$State[income14$State == "United States"] <- NA
income14$County[is.na(income14$County)] <- "NA County"
income14$Region <- gsub("(.*),.*", "\\1", income14$Region) 
income14$Region <- gsub("(.*),.*", "\\1", income14$Region) # run twice to remove all text after commas
income14$Region <- removeWords(income14$Region, "town")
income14$Region <- removeWords(income14$Region, "city")
income14$Region <- removeWords(income14$Region, "Town")

# add columns with year range
income14$Five_Year_Range <- rep("2010-2014", nrow(income14))

# add columns with five year average
income14$Five_Year_Average <-rep("2012", nrow(income14))

# order columns by indexing
income14 <- income14[c(2,3,4,1,7,8,5,6)]

################################################################################################################################
# repeat above column split for each dataset (income14 and income15)
income15$Region <- gsub("Massachusetts", "\\MA", income15$Region)
income15 <- separate(income15, "Region", c("Municipal", "County", "State"), ",", remove=FALSE, extra="merge", fill="left")
income15$Municipal <- removeWords(income15$Municipal, "town")
income15$Municipal <- removeWords(income15$Municipal, "city")
income15$Municipal <- removeWords(income15$Municipal, "Town")
income15$State[income15$State == "United States"] <- NA
income15$County[is.na(income13$County)] <- "NA County"
income15$Region <- gsub("(.*),.*", "\\1", income15$Region) 
income15$Region <- gsub("(.*),.*", "\\1", income15$Region) # run twice to remove all text after commas
income15$Region <- removeWords(income15$Region, "town")
income15$Region <- removeWords(income15$Region, "city")
income15$Region <- removeWords(income15$Region, "Town")

# add columns with year range
income15$Five_Year_Range <- rep("2011-2015", nrow(income15))

# add columns with five year average
income15$Five_Year_Average <-rep("2013", nrow(income15))

# order columns by indexing
income15 <- income15[c(2,3,4,1,7,8,5,6)]

#################################################################################################################################
# save new datasets as csv files
write.csv(file="income13.csv", x=income13)
write.csv(file="income14.csv", x=income14)
write.csv(file="income15.csv", x=income15)


##################################################################################################################################
## MERGE DATASETS ##
##################################################################################################################################


# merge new datasets
income_13_15 <- rbind(income13, income14, income15)

# sort by municipality
income_13_15 <- income_13_15[order(income_13_15$Municipal),]

# save new merged dataset
write.csv(file="income_13_15.csv", x=income_13_15) 

# import old and new datasets
incomedata_backup <- read_csv("incomedata_backup.csv")
incomemerge <- read_csv("income_13_15.csv")

# delete first columns [X1] 
incomedata_backup[1] <- NULL
incomemerge[1] <- NULL

# merge datasets
incomemerge_all <- rbind(incomedata_backup, incomemerge)

# save new merged dataset
write.csv(file="incomemerge_all.csv", x=incomemerge_all)

