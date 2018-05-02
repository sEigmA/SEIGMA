################################
## Update Employment Template ##
## Author: Zhenning Kang      ##
## Date Created:  05/01/2018  ##
## Last Modified: 05/01/2018  ##
################################


### Settings ###

# load libraries
library(readxl)
library(dplyr)


### Step One: Read in datasets ###

# data from year 2013 and 2016
emp_new <- c()

for (sheet_ind in 14:17){
  emp_pre <- read_excel("C:/Users/Zhenning Kang/Documents/UMass/SEIGMA/update_misc/orig_data/OLWD_employment_MA towns_2001-2016.xlsx", sheet = sheet_ind, col_names = T) %>% 
    select(3,6,7,10,23)
  
  colnames(emp_pre) <- c("Municipal", "Average_Monthly_Employment", "Average_Weekly_Wage", "Number_of_Employer_Establishments", "Total_Wages_Paid_to_All_Workers")
  emp_pre$Municipal[emp_pre$Municipal=="Attleborough"] <- "Attleboro"
  n <- nrow(emp_pre)
  emp_pre$State <- rep("MA", n)
  year <- 1999 + sheet_ind
  emp_pre$Year <- rep(year, n)
  emp_pre <- emp_pre[,c("Municipal", "State", "Year", "Average_Monthly_Employment", "Average_Weekly_Wage", "Number_of_Employer_Establishments", "Total_Wages_Paid_to_All_Workers")]
  
  emp_new <- rbind(emp_new, emp_pre)
}

# dataset currently in use
emp_data <- read.csv(file="C:/Users/Zhenning Kang/Documents/UMass/SEIGMA/update_misc/old_data/empdata3_backup.csv")

# add empty columns in new dataset
for (col in colnames(emp_data)[8:17]){
  emp_new[,col] <- NA
}

# merge datasets
emp_all <- rbind(emp_data, emp_new)
emp_all <- emp_all[order(emp_all$Municipal, emp_all$Year),]


### Step Two: Update Inflation ###

# get CPI values from documentation
cpi <- read_excel("C:/Users/Zhenning Kang/Documents/UMass/SEIGMA/update_misc/orig_data/CPI-U_2000 to 2017.xlsx")
colnames(cpi) <- unlist(c(cpi[11,])) # add column names
cpi <- cpi[-c(1:11),] # drop columns not in use
cpi$Year <- gsub(".0","", cpi$Year, fixed = T) # use four digits year value

# get CPI for current year
cpi_2016 <- as.numeric(cpi[which(cpi$Year == 2016),2])

# calculate Inflation_Adjusted_Average_Weekly_Wage
for (i in 1:nrow(emp_all)){
  cpi_pre <- as.numeric(cpi[which(cpi$Year == emp_all$Year[i]),2]) # CPI for each year
  emp_all$Inflation_Adjusted_Average_Weekly_Wage[i] <- (cpi_2016/cpi_pre) * emp_all$Average_Weekly_Wage[i]
}


### Step Three: Calculate Changes ###

for (i in 1:length(unique(emp_all$Municipal))){
  muni <- unique(emp_all$Municipal)[i]
  # use the Average_Monthly_Employment from 2003 as orig_emp
  orig_emp <- emp_all[emp_all$Municipal == muni,]$Average_Monthly_Employment[1]
  # use the Number_of_Employer_Establishments from 2003 as orig_etb
  orig_etb <- emp_all[emp_all$Municipal == muni,]$Number_of_Employer_Establishments[1]
  # use the Inflation_Adjusted_Average_Weekly_Wage from 2003 as orig_inf
  orig_inf <- emp_all[emp_all$Municipal == muni,]$Inflation_Adjusted_Average_Weekly_Wage[1]
  
  for (r in 2:nrow(emp_all[emp_all$Municipal == muni,])){ 
    # Change[year]=Average_Monthly_Employment[year]/orig_emp
    emp_all[emp_all$Municipal == muni,]$Change[r] <- emp_all[emp_all$Municipal == muni,]$Average_Monthly_Employment[r]/orig_emp
    # Employment_Change_Pct[year]=Average_Monthly_Employment[year]/orig_emp*100
    emp_all[emp_all$Municipal == muni,]$Employment_Change_Pct[r] <- emp_all[emp_all$Municipal == muni,]$Average_Monthly_Employment[r]/orig_emp*100
    # Establishment_Change[year]=Number_of_Employer_Establishments[year]/orig_etb
    emp_all[emp_all$Municipal == muni,]$Establishment_Change[r] <- emp_all[emp_all$Municipal == muni,]$Number_of_Employer_Establishments[r]/orig_etb
    # Establishment_Change_Pct[year]=Number_of_Employer_Establishments[year]/orig_etb*100
    emp_all[emp_all$Municipal == muni,]$Establishment_Change_Pct[r] <- emp_all[emp_all$Municipal == muni,]$Number_of_Employer_Establishments[r]/orig_etb*100
    # Average_Weekly_Wage_Change[year]=Inflation_Adjusted_Average_Weekly_Wage[year]/orig_inf
    emp_all[emp_all$Municipal == muni,]$Average_Weekly_Wage_Change[r] <- emp_all[emp_all$Municipal == muni,]$Inflation_Adjusted_Average_Weekly_Wage[r]/orig_inf
    # Average_Weekly_Wage_Change_Pct[year]=Inflation_Adjusted_Average_Weekly_Wage[year]/orig_inf*100
    emp_all[emp_all$Municipal == muni,]$Average_Weekly_Wage_Change_Pct[r] <- emp_all[emp_all$Municipal == muni,]$Inflation_Adjusted_Average_Weekly_Wage[r]/orig_inf*100
    # Employment_difference[year]=Employment_Change_Pct[year]-100
    emp_all[emp_all$Municipal == muni,]$Employment_difference[r] <- emp_all[emp_all$Municipal == muni,]$Employment_Change_Pct[r]-100
    # Establishment_difference[year]=Establishment_Change_Pct[year]-100
    emp_all[emp_all$Municipal == muni,]$Establishment_difference[r] <- emp_all[emp_all$Municipal == muni,]$Establishment_Change_Pct[r]-100
    # Average_Weekly_Wage_difference[year]=Average_Weekly_Wage_Change_Pct[year]-100
    emp_all[emp_all$Municipal == muni,]$Average_Weekly_Wage_difference[r] <- emp_all[emp_all$Municipal == muni,]$Average_Weekly_Wage_Change_Pct[r]-100
  }
}


### Step Four: Save New Dataset ###

# save dataset to folder
#write.csv(emp_all, "C:/Users/Zhenning Kang/Documents/UMass/SEIGMA/update_misc/new_data/updated_empdata.csv")
