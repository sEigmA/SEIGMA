######################################
##  Title: Poverty Rate Update      ##
##  Author(s): Valerie Evans        ##
##  Date Created: 10/13/2017        ##
##  Date Modified:                  ##
######################################



require(readr)

# set working directory
setwd("C:/Users/vevans/Documents/Secondary Data/Updated/Updated poverty data/")


############################################################################################################################
## CLEAN NEW DATASETS ##

# import dataset 
AT001_03_13 <- read_csv("ACS_13_5YR_S1701/AT001_03_13_5YR.csv")

# print first 10 rows of dataset
head(AT001_03_13, n=20)

# add columns with year range
AT001_03_13$Five_Year_Range <- rep("2009-2013", nrow(AT001_03_13))

# order columns by indexing
AT001_03_13 <- AT003_03_13[c(1,2,3,4,9,5,6,7,8)]

# rename columns
colnames(AT001_03_13) <- c("Municipal", "County", "State", "Region", "Five_Year_Range", "Total_Pop", "Pov_Pop", "Percent_Pov", "Margin_Error_Percent")

# save new file
write.csv(file="ACS_13_5YR_S1701/povrate_13.csv", x=AT001_03_13)

############################################################################################################################

# import dataset 
AT001_03_14 <- read_csv("ACS_14_5YR_S1701/AT001_03_14_5YR.csv")

# print first 10 rows of dataset
head(AT001_03_14, n=20)

# add columns with year range
AT001_03_14$Five_Year_Range <- rep("2010-2014", nrow(AT001_03_14))

# order columns by indexing
AT001_03_14 <- AT001_03_14[c(1,2,3,4,9,5,6,7,8)]

# rename columns
colnames(AT001_03_14) <- c("Municipal", "County", "State", "Region", "Five_Year_Range", "Total_Pop", "Pov_Pop", "Percent_Pov", "Margin_Error_Percent")

# save new file
write.csv(file="ACS_14_5YR_S1701/povrate_14.csv", x=AT001_03_14)

############################################################################################################################

# import dataset 
AT001_03_15 <- read_csv("ACS_15_5YR_S1701/AT001_03_15_5YR.csv")

# print first 10 rows of dataset
head(AT001_03_15, n=20)

# add columns with year range
AT001_03_15$Five_Year_Range <- rep("2011-2015", nrow(AT001_03_15))

# order columns by indexing
AT001_03_15 <- AT003_03_15[c(1,2,3,4,9,5,6,7,8)]

# rename columns
colnames(AT001_03_15) <- c("Municipal", "County", "State", "Region", "Five_Year_Range", "Total_Pop", "Pov_Pop", "Percent_Pov", "Margin_Error_Percent")

# save new file
write.csv(file="ACS_15_5YR_S1701/povrate_15.csv", x=AT001_03_15)

############################################################################################################################
## MERGE DATASETS ##

# import new datasets 
AT001_03_13 <- read_csv("ACS_13_5YR_S1701/povrate_13.csv")
AT001_03_14 <- read_csv("ACS_14_5YR_S1701/povrate_14.csv")
AT001_03_15 <- read_csv("ACS_15_5YR_S1701/povrate_15.csv")

# merge new datasets
povrate_13_15 <- rbind(AT001_03_13, AT001_03_14, AT001_03_15)

# save new merged dataset
write.csv(file="povrate_13_15.csv", x=povrate_13_15) 

# import old and new datasets
povratedata_backup <- read_csv("povratedata_backup.csv")
povratemerge <- read_csv("povrate_13_15.csv")

# delete first columns [X1] 
povratedata_backup[1] <- NULL 
povratemerge[1:2] <- list(NULL)

# merge datasets
povratemerge_all <- rbind(povratedata_backup, povratemerge)

# save new merged dataset
write.csv(file="povratemerge_all.csv", x=povratemerge_all)