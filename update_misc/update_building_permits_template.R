######################################
## Update Building Permits Template ##
## Author: Zhenning Kang            ##
## Date Created:  04/06/2018        ##
## Last Modified: 04/14/2018        ##
######################################



### Settings ###

# load libraries
library(readxl)
library(dplyr)



### Step One: Read in datasets ###

# data from year 2016
Mass2016Annl <- read_excel("C:/Users/Zhenning Kang/Documents/UMass/SEIGMA/update_misc/building_permits_2000-2016.xlsx", sheet = 2, col_names = F)
colnames(Mass2016Annl) <- unlist(c(Mass2016Annl[3,])) # extract column names
table2016 <- Mass2016Annl[-c(1:4),-c(2:12)] # drop data not in use

# data from years 2012 to 2015
col_need <- colnames(table2016) # keep the same columns onward
for (sheet_ind in 3:6){
    # load data per year from each sheet in xlsx file
    pre_year <- read_excel("C:/Users/Zhenning Kang/Documents/UMass/SEIGMA/update_misc/building_permits_2000-2016.xlsx", sheet = sheet_ind, col_names = F)
    colnames(pre_year) <- unlist(c(pre_year[3,])) # extract column names
    # drop data not in use
    if (sheet_ind == 3 | sheet_ind == 4 ){
      table_pre <- as.data.frame(pre_year[-c(1:4), col_need])
    }else if (sheet_ind == 5 | sheet_ind == 6 ){
      table_pre <- as.data.frame(pre_year[-c(1:3), col_need])
    }
    # add the survey year if the value of year is missing
    table_pre$`Survey Date`[is.na(table_pre$`Survey Date`)] <- unique(table_pre$`Survey Date`)[2]
    # merge tables
    table2016 <- rbind(table2016, table_pre)
}

# load the dataset currently used in the individual app
buildingdata <- read.csv("C:/Users/Zhenning Kang/Documents/UMass/SEIGMA/update_misc/buildingdata.csv")

# same name for columns in use to keep consistancy 
colnames(buildingdata)[1:18] <- colnames(table2016)[c(3,1,2,4:15,28:30)] <- c("Region","Year","Number_of_Months_Reported" ,"Single_Family_Buildings","Single_Family_Units","Single_Family_Valuation","I2_Family_Buildings","I2_Family_Units","I2_Family_Valuation","I3-4_Family_Buildings","I3-4_Family_Units","I3-4_Family_Valuation","I5_Family_Buildings","I5_Family_Units","I5_Family_Valuation", "Total_Buildings", "Total_Units", "Total_Valuation") 

# select columns to be used in the app
building2016 <- table2016 %>% select(3,1,2,4:15,28:30)

# change region names for consistancy
for (i in grep("own", building2016$Region)){
  building2016[i,"Region"] <- gsub( " t.*$", "", building2016[i,"Region"])
  building2016[i,"Region"] <- gsub( " T.*$", "", building2016[i,"Region"])
}
building2016$Region <- ifelse(building2016$Region=="Massachusetts", "MA", building2016$Region) # old dataset used "MA"

# remove duplicate rows without year value
building2016 <- building2016[complete.cases(building2016[ , "Year"]),]

# calculate the average valuation per unit in new datasets
building2016$Average_Valuation_PerUnits <- as.numeric(building2016$Total_Valuation)/as.numeric(building2016$Total_Units)
colnames(buildingdata)[19] <- "Average_Valuation_PerUnits"

# add empty columns for percente change in new datasets
pct_col <- c("Total_Pct_Change", "Change_from_previous", "Pct_Change_from_previous", "Percentage_of_1_Family", "Percentage_of_2_Family", "Percentage_of_3_and_4_Family", "Percentage_of_5_Family")
for (col in pct_col){
  building2016[,col] <- NA # replace NAs in Step Four
}

# merge all datasets into one table
building2016 <- rbind(building2016, buildingdata[,c(1:19,26:32)]) 
for (c in 2:ncol(building2016)){
  building2016[,c] <- as.numeric(unlist(building2016[,c])) # convert to numeric
}

# order the dataset by region, year
building2016 <- building2016[order(building2016$Region,building2016$Year),]
building2016$Region[5679:5683] <- "West Tisbury" # correct this town name


### Step Two: Update inflation adjusted values ###

# get CPI values from documentation
cpi <- read_excel("C:/Users/Zhenning Kang/Documents/UMass/SEIGMA/update_misc/CPI-U_2000 to 2017.xlsx")
colnames(cpi) <- unlist(c(cpi[11,])) # add column names
cpi <- cpi[-c(1:11),] # drop columns not in use
cpi$Year <- gsub(".0","", cpi$Year, fixed = T) # use four digits year value

# get CPI for current year
cpi_2016 <- as.numeric(cpi[which(cpi$Year == 2016),2])

# create inflation adjusted columns for each category
inf_adj_col <- c("Inflation_Adjusted_1_Family_Valuation", "Inflation_Adjusted_2_Family_Valuation", "Inflation_Adjusted_3_4_Family_Valuation", "Inflation_Adjusted_5_Family_Valuation", "Inflation_Adjusted_Total_Valuation", "Inflation_Adjusted_Average_Valuation")
for (col in inf_adj_col){
  building2016[,col] <- NA # replace NAs in next loop
}

# create inflation adjusted values respectively
for (i in 1:nrow(building2016)){
  cpi_pre <- as.numeric(cpi[which(cpi$Year == building2016$Year[i]),2]) # CPI for each year
  building2016$Inflation_Adjusted_1_Family_Valuation[i] <- (cpi_2016/cpi_pre) * building2016$Single_Family_Valuation[i]
  building2016$Inflation_Adjusted_2_Family_Valuation[i] <- (cpi_2016/cpi_pre) * building2016$I2_Family_Valuation[i]
  building2016$Inflation_Adjusted_3_4_Family_Valuation[i] <- (cpi_2016/cpi_pre) * building2016$'I3-4_Family_Valuation'[i]
  building2016$Inflation_Adjusted_5_Family_Valuation[i] <- (cpi_2016/cpi_pre) * building2016$I5_Family_Valuation[i]
  building2016$Inflation_Adjusted_Total_Valuation[i] <- (cpi_2016/cpi_pre) * building2016$Total_Valuation[i]
  building2016$Inflation_Adjusted_Average_Valuation[i] <- (cpi_2016/cpi_pre) * building2016$Average_Valuation_PerUnits[i]
}



### Step Three: Update population counts ###

# load population counts before 2010
est_2010 <- read.csv("C:/Users/Zhenning Kang/Documents/UMass/SEIGMA/update_misc/sub-est00int.csv")
est_2010 <- filter(est_2010, STNAME == "Massachusetts") # only MA data needed
est_2010 <- est_2010[!duplicated(est_2010$NAME), c(6,9:18)] # unique values

# load population counts after 2010
est_2016 <- read.csv("C:/Users/Zhenning Kang/Documents/UMass/SEIGMA/update_misc/sub-est2016_25.csv")
est_2016 <- est_2016[!duplicated(est_2016$NAME), c(9,13:19)] # unique values

# merge full population data
pop_est <- cbind(est_2010, est_2016[,-1])

# rename id column as "Region" and as.character for future calculations
colnames(pop_est)[1] <- "Region"
pop_est$Region <- as.character(pop_est$Region)

# change region names for consistancy
for (i in grep("own", pop_est$Region)){
  pop_est[i,"Region"] <- gsub( " t.*$", "", pop_est[i,"Region"])
  pop_est[i,"Region"] <- gsub( " T.*$", "", pop_est[i,"Region"])
}
pop_est$Region <- gsub(" city", "", pop_est$Region)
pop_est$Region[128] <- "West Tisbury" # correct this town name 
pop_est$Region[1] <- "MA" # use MA as earlier

# drop rows for "Balace of XXX County" (not used in population count)
pop_est <- pop_est[!grepl("Balance", pop_est$Region),]

# sort population counts by region ascending order
pop_est <- pop_est[order(pop_est$Region),] 

# create new empty comlune for permits per 1000 population
building2016$Permits_Per_1000_Population <- NA # replace NAs in next loop

# update population counts per year
for (i in 1:length(2000:2016)){
  year <- 1999 + i
  building2016[building2016$Year == year,]$Permits_Per_1000_Population <- building2016[building2016$Year == year,]$Total_Buildings/pop_est[,i+1]*1000
}



### Step Four: update percentage columns ###

# calculate percentage change per region
for (i in 1:length(unique(building2016$Region))){
  region <- unique(building2016$Region)[i]
  # use the total units from 2000 as Orig_Units
  orig_units <- building2016[building2016$Region == region,]$Total_Units[1]
  for (r in 13:17){
    # Total_Pct_Change=(Total_Units[per year]-Orig_Units)/Orig_Units*100
    building2016[building2016$Region == region,]$Total_Pct_Change[r] <- (building2016[building2016$Region == region,]$Total_Units[r] - orig_units) / orig_units * 100
    # use the total units from previous year as Prev_Units 
    prev_units <- building2016[building2016$Region == region,]$Total_Units[r-1]
    # Change_from_previous=(Total_Units[per year]-Prev_Units)/Prev_Units
    building2016[building2016$Region == region,]$Change_from_previous[r] <- (building2016[building2016$Region == region,]$Total_Units[r] - prev_units) / prev_units
    # Pct_Change_from_previous=Change_from_previous*100
    building2016[building2016$Region == region,]$Pct_Change_from_previous[r] <- building2016[building2016$Region == region,]$Change_from_previous[r]*100
    # Percentage_of_1_Family=Single_Family_Units/Total_Units*100
    building2016[building2016$Region == region,]$Percentage_of_1_Family[r] <- building2016[building2016$Region == region,]$Single_Family_Units[r]/building2016[building2016$Region == region,]$Total_Units[r]*100
    # Percentage_of_2_Family=I2_Family_Units/Total_Units*100
    building2016[building2016$Region == region,]$Percentage_of_2_Family[r] <- building2016[building2016$Region == region,]$I2_Family_Units[r]/building2016[building2016$Region == region,]$Total_Units[r]*100
    # Percentage_of_3_and_4_Family=I3-4_Family_Units/Total_Units*100
    building2016[building2016$Region == region,]$Percentage_of_3_and_4_Family[r] <- building2016[building2016$Region == region,]$'I3-4_Family_Units'[r]/building2016[building2016$Region == region,]$Total_Units[r]*100
    # Percentage_of_5_Family=I5_Family_Units/Total_Units*100
    building2016[building2016$Region == region,]$Percentage_of_5_Family[r] <- building2016[building2016$Region == region,]$I5_Family_Units[r]/building2016[building2016$Region == region,]$Total_Units[r]*100
    }
}

# save dataset to folder
# write.csv(building2016, "C:/Users/Zhenning Kang/Documents/UMass/SEIGMA/update_misc/updated_buildingdata.csv")
