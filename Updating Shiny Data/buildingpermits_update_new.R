###############################################
## Title: Building Permits Data Update       ##
## Author: Zhenning Kang                     ##
## Date Created:  04/06/2018                 ##
## Last Modified: 01/17/2020 VE              ##
###############################################
# Building Permit data can be downloaded from UMDI via this link: 
# http://www.donahue.umassp.edu/business-groups/economic-public-policy-research/massachusetts-state-data-center 
# Population data can be downloaded from: https://www2.census.gov/programs-surveys/popest/datasets 


####  SETTINGS  ####
library(plyr)
library(tidyverse)
library(readxl)


####  SUPPORTING DATA  ####
# Ensure all files used below are in the same directory or directory will need to be switched to access files
## Load column names, geography 
colnames <- read_excel("Shiny App Variables.xlsx", sheet = 2)
colnames_pre <- colnames[c(1:15),]
geography <- read.csv("geography.csv", stringsAsFactors = FALSE)
CPI <- read.csv("CPI_2000_2018.csv", stringsAsFactors = FALSE)
# # CPI can also be obtained directly from BLS API (https://www.bls.gov/developers/api_r.htm) with code below
# library(blscrapeR)
# # Modify startyear and endyear dates to change dataset (i.e. 2003-2017)
# df_cpi <- bls_api("CUUS0000SA0", startyear = 2000, endyear = 2018,
#                   registrationKey = "a621394b12a84fe0adc76df37ca675b5",
#                   annualaverage = TRUE)
# CPI_annual_avg <- df_cpi %>% filter(periodName == "Annual")
# CPI <- CPI_annual_avg[,c(1,4)]
CPI_2018 <- as.numeric(CPI[which(CPI$year == 2018),2]) #get CPI for current year
est_2010 <- read.csv("sub-est00int.csv", stringsAsFactors = FALSE) #population counts before 2010
est_2018 <- read.csv("sub-est2018_25.csv", stringsAsFactors = FALSE) #population counts after 2010


####  STEPS TO CREATE UPDATED DATASET  ####


## Step One: Read in datasets ##

## Load downloaded excel files (each sheets is a year in excel file)
## Obtain column names to use in function
# table_2018 <- read_excel("building_permits_2000-2018.xlsx", sheet = 2, col_names = FALSE)
# colnames(table_2018) <- unlist(c(table_2018[3,])) #extract column names
# bp_2018 <- table_2018[-c(1:4), -c(2:12)] #drop data not in use
# bp_2018 <- bp_2018[-367,]
# colnames(bp_2018)[29] <- "Total Evaluation" #2018 dataset uses "Total Valuation" where other years use "Total Evaluation"
# remove(table_2018)
# 
# col_need <- colnames(bp_2018) #keep the same columns onward
# for (sheet_ind in 3:20){
#   # load data per year from each sheet in xlsx file
#   pre_year <- read_excel("building_permits_2000-2018.xlsx", sheet = sheet_ind, col_names = F)
#   colnames(pre_year) <- unlist(c(pre_year[3,])) #extract column names
#   table_pre <- as.data.frame(pre_year[-c(1:4), col_need])
#   bp_2018 <- rbind(bp_2018, table_pre) #merge tables
# }
# buildingpermits <- bp_2018

## Above code should work for this any new dataset however under close scrutiny it seems many of the sheets in the most recent 
## dataset (2018) do not have a consistent structure and, therefore, cannot be imported in this way. The following code allows 
## for individual import of some sheets that aren't able to be imported through the general function.

## Load downloaded excel files (each sheets is a year in excel file)
read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  #if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}
allsheets <- read_excel_allsheets("building_permits_2000-2018.xlsx")

## Combine into one dataframe
buildingpermits <- bind_rows(allsheets, .id = 'id')
buildingpermits$id <- parse_number(buildingpermits$id)

## Remove survey years with similar structures 
## A = 2018-2015,2013-2012; B = 2014,2011; C = 2010-2007; D = 2006-2005,2003-2000; E = 2004
buildingpermits_A <- buildingpermits %>% filter(id %in% c("2018", "2017", "2016", "2015", "2013", "2012"))
buildingpermits_A <- buildingpermits_A[-c(1,3,370:373,740:743,1110:1113,1480:1482,1849:1850),c(1,15:28)] #remove unnecessary rows and keep specific columns
colnames(buildingpermits_A) <- buildingpermits_A[1,]
buildingpermits_A <- buildingpermits_A[-1,] 
colnames(buildingpermits_A)[1] = "Year"

buildingpermits_B <- buildingpermits %>% filter(id %in% c("2014", "2011"))
buildingpermits_B <- buildingpermits_B[-c(1,3,370:371),c(1,16:29)] #remove unnecessary rows and keep specific columns
colnames(buildingpermits_B) <- buildingpermits_B[1,]
buildingpermits_B <- buildingpermits_B[-1,] 
colnames(buildingpermits_B)[1] = "Year"

buildingpermits_C <- buildingpermits %>% filter(id %in% c("2010", "2009", "2008", "2007"))
buildingpermits_C <- buildingpermits_C[-c(1,369:370,737:743,1110:1113,1480:1482),c(1,14:27)] #remove unnecessary rows and keep specific columns
colnames(buildingpermits_C) <- buildingpermits_C[1,]
buildingpermits_C <- buildingpermits_C[-1,] 
colnames(buildingpermits_C)[1] = "Year"

# buildingpermits_D <- buildingpermits %>% filter(id %in% c("2006", "2005", "2003", "2002", "2001", "2000"))
# buildingpermits_D <- buildingpermits_D[,c(1,11:23)] #remove unnecessary rows and keep specific columns
# colnames(buildingpermits_D) <- buildingpermits_D[1,]
# buildingpermits_D <- buildingpermits_D[-1,] 
# colnames(buildingpermits_D)[1] = "Year"
# 
# buildingpermits_E <- buildingpermits %>% filter(id %in% "2004")
# buildingpermits_E <- buildingpermits_E[,c(1,14:27)] #remove unnecessary rows and keep specific columns
# colnames(buildingpermits_E) <- buildingpermits_E[1,]
# buildingpermits_E <- buildingpermits_E[-1,] 
# colnames(buildingpermits_E)[1] = "Year"

## Above code didn't work for 2006-2000 (files D and E) as the data did not import correctly so need to import manually and merge
MA2006 <- read_excel("building_permits_2000-2018.xlsx", sheet = 14, col_names = FALSE)
MA2006 <- MA2006[-c(1:2,370:372),c(8:21)]
colnames(MA2006) <- MA2006[1,]
MA2006 <- MA2006[-1, ] 
MA2006 <- cbind(Year = 2006, MA2006)
MA2005 <- read_excel("building_permits_2000-2018.xlsx", sheet = 15, col_names = FALSE)
MA2005 <- MA2005[-c(1:2,370:372),c(8:21)]
colnames(MA2005) <- MA2005[1,]
MA2005 <- MA2005[-1, ] 
MA2005 <- cbind(Year = 2005, MA2005)
MA2004 <- read_excel("building_permits_2000-2018.xlsx", sheet = 16, col_names = FALSE)
MA2004 <- MA2004[-c(1:2,370:373),c(9:22)]
colnames(MA2004) <- MA2004[1,]
MA2004 <- MA2004[-1, ] 
MA2004 <- cbind(Year = 2004, MA2004)
MA2003 <- read_excel("building_permits_2000-2018.xlsx", sheet = 17, col_names = FALSE)
MA2003 <- MA2003[-c(1:2,370:373),c(8:21)]
colnames(MA2003) <- MA2003[1,]
MA2003 <- MA2003[-1, ] 
MA2003 <- cbind(Year = 2003, MA2003)
MA2002 <- read_excel("building_permits_2000-2018.xlsx", sheet = 18, col_names = FALSE)
MA2002 <- MA2002[-c(1:2,370:373),c(8:21)]
colnames(MA2002) <- MA2002[1,]
MA2002 <- MA2002[-1, ] 
MA2002 <- cbind(Year = 2002, MA2002)
MA2001 <- read_excel("building_permits_2000-2018.xlsx", sheet = 19, col_names = FALSE)
MA2001 <- MA2001[-c(1:2,370:373),c(8:21)]
colnames(MA2001) <- MA2001[1,]
MA2001 <- MA2001[-1, ] 
MA2001 <- cbind(Year = 2001, MA2001)
MA2000 <- read_excel("building_permits_2000-2018.xlsx", sheet = 20, col_names = FALSE)
MA2000 <- MA2000[-c(1:2,370:373),c(8:21)]
colnames(MA2000) <- MA2000[1,]
MA2000 <- MA2000[-1, ] 
MA2000 <- cbind(Year = 2000, MA2000)
MA2006_2000 <- bind_rows(MA2006, MA2005, MA2004, MA2003, MA2002, MA2001, MA2000) 

## Merge all years and modify column order and names
buildingpermits <- bind_rows(buildingpermits_A, buildingpermits_B, buildingpermits_C, MA2006_2000)
buildingpermits <- buildingpermits[,c(3,1:2,4:15)] # reorder columns
colnames(buildingpermits) <- colnames_pre$`COLUMN NAME` #modify column names
buildingpermits[3:15] <- sapply(buildingpermits[3:15],as.numeric)

## Change Region names for consistency
for (i in grep("own", buildingpermits$Region)){
  buildingpermits[i,"Region"] <- gsub( " t.*$", "", buildingpermits[i,"Region"])
  buildingpermits[i,"Region"] <- gsub( " T.*$", "", buildingpermits[i,"Region"])
}

## Correct Region names (if necessary)
buildingpermits$Region <- ifelse(buildingpermits$Region == "Massachusetts", "MA", buildingpermits$Region) #previous dataset used "MA"
buildingpermits$Region[buildingpermits$Region == "Manchester By"] <- "Manchester-by-the-Sea"
buildingpermits$Region[buildingpermits$Region == "Aquinnah (Gay Head)"] <- "Aquinnah"

## Add total columns
buildingpermits$Total_Buildings <- buildingpermits$"Single_Family_Buildings" + buildingpermits$"I2_Family_Buildings" + 
  buildingpermits$"I3-4_Family_Buildings" + buildingpermits$"I5_Family_Buildings"
buildingpermits$Total_Units <- buildingpermits$"Single_Family_Units" + buildingpermits$"I2_Family_Units" + 
  buildingpermits$"I3-4_Family_Units" + buildingpermits$"I5_Family_Units"
buildingpermits$Total_Valuation <- buildingpermits$"Single_Family_Valuation" + buildingpermits$"I2_Family_Valuation" + 
  buildingpermits$"I3-4_Family_Valuation" + buildingpermits$"I5_Family_Valuation"
buildingpermits$Average_Valuation_PerUnits <- buildingpermits$Total_Valuation/buildingpermits$Total_Units

# Add empty columns for percent change in new datasets
pct_col <- c("Total_Pct_Change", "Change_from_previous", "Pct_Change_from_previous", "Percentage_of_1_Family", 
             "Percentage_of_2_Family", "Percentage_of_3_and_4_Family", "Percentage_of_5_Family")
for (col in pct_col){
  buildingpermits[,col] <- NA #replace NAs in Step Four
}


## Step Two: Update inflation adjusted values ##

## Create inflation adjusted columns for each category
inf_adj_col <- c("Inflation_Adjusted_1_Family_Valuation", "Inflation_Adjusted_2_Family_Valuation", "Inflation_Adjusted_3_4_Family_Valuation", "Inflation_Adjusted_5_Family_Valuation", "Inflation_Adjusted_Total_Valuation", "Inflation_Adjusted_Average_Valuation")
for (col in inf_adj_col){
  buildingpermits[,col] <- NA #replace NAs in next loop
}

## Create inflation adjusted values respectively
for (i in 1:nrow(buildingpermits)){
  cpi_pre <- CPI[which(CPI$year == buildingpermits$Year[i]),2] #CPI for each year
  buildingpermits$Inflation_Adjusted_1_Family_Valuation[i] <- (CPI_2018/cpi_pre) * buildingpermits$Single_Family_Valuation[i]
  buildingpermits$Inflation_Adjusted_2_Family_Valuation[i] <- (CPI_2018/cpi_pre) * buildingpermits$I2_Family_Valuation[i]
  buildingpermits$Inflation_Adjusted_3_4_Family_Valuation[i] <- (CPI_2018/cpi_pre) * buildingpermits$'I3-4_Family_Valuation'[i]
  buildingpermits$Inflation_Adjusted_5_Family_Valuation[i] <- (CPI_2018/cpi_pre) * buildingpermits$I5_Family_Valuation[i]
  buildingpermits$Inflation_Adjusted_Total_Valuation[i] <- (CPI_2018/cpi_pre) * buildingpermits$Total_Valuation[i]
  buildingpermits$Inflation_Adjusted_Average_Valuation[i] <- (CPI_2018/cpi_pre) * buildingpermits$Average_Valuation_PerUnits[i]
}

# # created new columns as list so need to unlist
# list <- sapply(buildingpermits, is.list)
# buildingpermits[list] <- lapply(buildingpermits[list], unlist)


## Step Three: Update population counts ##

## Create population counts file
est_2010 <- filter(est_2010, STNAME == "Massachusetts") #only MA data needed
est_2010 <- est_2010[!duplicated(est_2010$NAME), c(6,9:18)] #unique values
est_2010 <- est_2010[!grepl("Balance of", est_2010$NAME),]
est_2018 <- est_2018[!duplicated(est_2018$NAME), c(9,13:21)] #unique values
est_2018 <- est_2018[!grepl("Balance of", est_2018$NAME),]
#est_2018$NAME[!est_2018$NAME %in% est_2010$NAME] #to find mismatches
pop_est <- cbind(est_2010, est_2018) #merge full population data
colnames(pop_est)[1] <- "Region"

## Change Region names for consistancy
for (i in grep("own", pop_est$Region)){
  pop_est[i,"Region"] <- gsub( " t.*$", "", pop_est[i,"Region"])
  pop_est[i,"Region"] <- gsub( " T.*$", "", pop_est[i,"Region"])
}
pop_est$Region <- gsub(" city", "", pop_est$Region)
pop_est$Region[125] <- "West Tisbury"  
pop_est$Region[1] <- "MA" #use MA as earlier

## Drop rows for "Balace of XXX County" (not used in population count) and any extra columns
pop_est <- pop_est[!grepl("Balance", pop_est$Region),]
pop_est <- pop_est[-12] #extra NAME column

## Sort population counts by region ascending order
pop_est <- pop_est[order(pop_est$Region),] 

## Create new empty column for permits per 1000 population and update population counts per year
buildingpermits$Permits_Per_1000_Population <- NA #replace NAs in next loop
for (i in 1:length(2000:2018)){
  year <- 1999 + i
  buildingpermits[buildingpermits$Year == year,]$Permits_Per_1000_Population <- buildingpermits[buildingpermits$Year == year,]$Total_Buildings/pop_est[,i+1]*1000
}

buildingpermits <- buildingpermits[order(buildingpermits$Year),] #reorder by year
buildingpermits <- buildingpermits[order(buildingpermits$Region),] #reorder by region

## Step Four: update percentage columns ##

## Calculate percentage change per region
for (i in 1:length(unique(buildingpermits$Region))){
  region <- unique(buildingpermits$Region)[i]
  # Use the total units from 2000 as orig_units
  orig_units <- buildingpermits[buildingpermits$Region == region,]$Total_Units[1]
  for (r in 2:19){
    # Total_Pct_Change=(Total_Units[per year]-Orig_Units)/Orig_Units*100
    buildingpermits[buildingpermits$Region == region,]$Total_Pct_Change[r] <- (buildingpermits[buildingpermits$Region == region,]$Total_Units[r] - orig_units) / orig_units * 100
    # Use the total units from previous year as Prev_Units 
    prev_units <- buildingpermits[buildingpermits$Region == region,]$Total_Units[r-1]
    # Change_from_previous=(Total_Units[per year]-Prev_Units)/Prev_Units
    buildingpermits[buildingpermits$Region == region,]$Change_from_previous[r] <- (buildingpermits[buildingpermits$Region == region,]$Total_Units[r] - prev_units) / prev_units
    # Pct_Change_from_previous=Change_from_previous*100
    buildingpermits[buildingpermits$Region == region,]$Pct_Change_from_previous[r] <- buildingpermits[buildingpermits$Region == region,]$Change_from_previous[r]*100
    # Percentage_of_1_Family=Single_Family_Units/Total_Units*100
    buildingpermits[buildingpermits$Region == region,]$Percentage_of_1_Family[r] <- buildingpermits[buildingpermits$Region == region,]$Single_Family_Units[r]/buildingpermits[buildingpermits$Region == region,]$Total_Units[r]*100
    # Percentage_of_2_Family=I2_Family_Units/Total_Units*100
    buildingpermits[buildingpermits$Region == region,]$Percentage_of_2_Family[r] <- buildingpermits[buildingpermits$Region == region,]$I2_Family_Units[r]/buildingpermits[buildingpermits$Region == region,]$Total_Units[r]*100
    # Percentage_of_3_and_4_Family=I3-4_Family_Units/Total_Units*100
    buildingpermits[buildingpermits$Region == region,]$Percentage_of_3_and_4_Family[r] <- buildingpermits[buildingpermits$Region == region,]$'I3-4_Family_Units'[r]/buildingpermits[buildingpermits$Region == region,]$Total_Units[r]*100
    # Percentage_of_5_Family=I5_Family_Units/Total_Units*100
    buildingpermits[buildingpermits$Region == region,]$Percentage_of_5_Family[r] <- buildingpermits[buildingpermits$Region == region,]$I5_Family_Units[r]/buildingpermits[buildingpermits$Region == region,]$Total_Units[r]*100
    }
}

buildingpermits = as.data.frame(buildingpermits)
buildingpermits[buildingpermits == "Inf"] <- NA
buildingpermits[buildingpermits == "NaN"] <- NA


# save dataset to folder
# write.csv(buildingpermits, "buildingdata_updated_2018.csv")
