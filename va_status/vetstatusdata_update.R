############################################
## Title: Veterans Status Update          ##
## Author(s): Valerie Evans               ## 
## Date Created:  10/20/2017              ##
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
ACS13 <- read.csv("ACS_13_5YR_DP02_with_ann.csv")
ACS14 <- read.csv("ACS_14_5YR_DP02_with_ann.csv")
ACS15 <- read.csv("ACS_15_5YR_DP02_with_ann.csv")

# select relevant columns
vet13 <- select(ACS13, "GEO.display.label", "HC01_VC100", "HC01_VC101", "HC03_VC101", "HC04_VC101")
vet14 <- select(ACS14, "GEO.display.label", "HC01_VC100", "HC01_VC101", "HC03_VC101", "HC04_VC101")
vet15 <- select(ACS15, "GEO.display.label", "HC01_VC100", "HC01_VC101", "HC03_VC101", "HC04_VC101")

# remove rows with 'County subdivisions not defined'
vet13 <- dplyr::filter(vet13, !grepl("County subdivisions not defined", GEO.display.label))
vet14 <- dplyr::filter(vet14, !grepl("County subdivisions not defined", GEO.display.label))
vet15 <- dplyr::filter(vet15, !grepl("County subdivisions not defined", GEO.display.label))

# rename columns
colnames(vet13) <- c("Region", "Civilian_Pop", "Vet_Pop", "Percent_Vet", "Margin_Error_Percent")
colnames(vet14) <- c("Region", "Civilian_Pop", "Vet_Pop", "Percent_Vet", "Margin_Error_Percent")
colnames(vet15) <- c("Region", "Civilian_Pop", "Vet_Pop", "Percent_Vet", "Margin_Error_Percent")

# remove first row
vet13 <- vet13[-c(1),]
vet14 <- vet14[-c(1),]
vet15 <- vet15[-c(1),]

##############################################################################################################################
# split geography column into 4: Municipal, County, State, Region
vet13$Region <- gsub("Massachusetts", "\\MA", vet13$Region)
vet13 <- separate(vet13, "Region", c("Municipal", "County", "State"), ",", remove=FALSE, extra="merge", fill="left")
vet13$Municipal <- removeWords(vet13$Municipal, "town")
vet13$Municipal <- removeWords(vet13$Municipal, "city")
vet13$Municipal <- removeWords(vet13$Municipal, "Town")
vet13$State[vet13$State == "United States"] <- NA
vet13$County[is.na(vet13$County)] <- "NA County"
vet13$Region <- gsub("(.*),.*", "\\1", vet13$Region) 
vet13$Region <- gsub("(.*),.*", "\\1", vet13$Region) # run twice to remove all text after commas
vet13$Region <- removeWords(vet13$Region, "town")
vet13$Region <- removeWords(vet13$Region, "city")
vet13$Region <- removeWords(vet13$Region, "Town")

# add columns with year range
vet13$Five_Year_Range <- rep("2009-2013", nrow(vet13))

# order columns by indexing
vet13 <- vet13[c(2,3,4,1,9,5,6,7,8)]


################################################################################################################################
# repeat above column split for each dataset (vet14 and vet15)
vet14$Region <- gsub("Massachusetts", "\\MA", vet14$Region)
vet14 <- separate(vet14, "Region", c("Municipal", "County", "State"), ",", remove=FALSE, extra="merge", fill="left")
vet14$Municipal <- removeWords(vet14$Municipal, "town")
vet14$Municipal <- removeWords(vet14$Municipal, "city")
vet14$Municipal <- removeWords(vet14$Municipal, "Town")
vet14$State[vet14$State == "United States"] <- NA
vet14$County[is.na(vet14$County)] <- "NA County"
vet14$Region <- gsub("(.*),.*", "\\1", vet14$Region) 
vet14$Region <- gsub("(.*),.*", "\\1", vet14$Region) # run twice to remove all text after commas
vet14$Region <- removeWords(vet14$Region, "town")
vet14$Region <- removeWords(vet14$Region, "city")
vet14$Region <- removeWords(vet14$Region, "Town")

# add columns with year range
vet14$Five_Year_Range <- rep("2010-2014", nrow(vet14))

# order columns by indexing
vet14 <- vet14[c(2,3,4,1,9,5,6,7,8)]

################################################################################################################################
# repeat above column split for each dataset (vet14 and vet15)
vet15$Region <- gsub("Massachusetts", "\\MA", vet15$Region)
vet15 <- separate(vet15, "Region", c("Municipal", "County", "State"), ",", remove=FALSE, extra="merge", fill="left")
vet15$Municipal <- removeWords(vet15$Municipal, "town")
vet15$Municipal <- removeWords(vet15$Municipal, "city")
vet15$Municipal <- removeWords(vet15$Municipal, "Town")
vet15$State[vet15$State == "United States"] <- NA
vet15$County[is.na(vet15$County)] <- "NA County"
vet15$Region <- gsub("(.*),.*", "\\1", vet15$Region) 
vet15$Region <- gsub("(.*),.*", "\\1", vet15$Region) # run twice to remove all text after commas
vet15$Region <- removeWords(vet15$Region, "town")
vet15$Region <- removeWords(vet15$Region, "city")
vet15$Region <- removeWords(vet15$Region, "Town")

# add columns with year range
vet15$Five_Year_Range <- rep("2011-2015", nrow(vet15))

# order columns by indexing
vet15 <- vet15[c(2,3,4,1,9,5,6,7,8)]


#################################################################################################################################
# save new datasets as csv files
write.csv(file="vet13.csv", x=vet13)
write.csv(file="vet14.csv", x=vet14)
write.csv(file="vet15.csv", x=vet15)


##################################################################################################################################
## MERGE DATASETS ##
##################################################################################################################################


# merge new datasets
vet_13_15 <- rbind(vet13, vet14, vet15)

# save new merged dataset
write.csv(file="vet_13_15.csv", x=vet_13_15) 

# import old and new datasets
vetdata_backup <- read_csv("vetstatusdata_backup.csv")
vetmerge <- read_csv("vet_13_15.csv")

# delete first columns [X1] 
vetdata_backup[1] <- NULL 
vetmerge[1] <- NULL

# merge datasets
vetmerge_all <- rbind(vetdata_backup, vetmerge)

# save new merged dataset
write.csv(file="vetmerge_all.csv", x=vetmerge_all)



