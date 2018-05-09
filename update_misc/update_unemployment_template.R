##################################
## Update Unemployment Template ##
## Author: Zhenning Kang        ##
## Date Created:  05/07/2018    ##
## Last Modified: 05/08/2018    ##
##################################


### Settings ###

# load libraries
library(readxl)
library(dplyr)


### Step One: Read in datasets ###

# load old dataset
backup <- read.csv("C:/Users/Zhenning Kang/Documents/UMass/SEIGMA/update_misc/old_data/unempdata2_backup.csv")

# load new dataset
orig_data <- read_excel("C:/Users/Zhenning Kang/Documents/UMass/SEIGMA/update_misc/lau_muni/Updated.LAU_MA2013to2016eBLS.xlsx", sheet = 2, col_names = T) 

# get data between year 2013 and year 2016
ump_data <- orig_data %>% 
  select(2,3,4,6,7) %>%
  filter(year >= 2013)

# rename columns
colnames(ump_data) <- c("Municipal", "Variable", "Year", "Month", "Value")

# calculate average numbers
ump_new <- matrix(NA, nrow = 398, ncol = 4)
for (i in 1:4){
  val <- unique(ump_data$Variable)[i]
  tmp <- ump_data %>% 
    filter(Variable == val) %>% 
    group_by(Municipal) %>% 
    summarise(avg = mean(Value))
  ump_new[,i] <-tmp$avg
}
ump_new <- data.frame(ump_new)
ump_new$Municipal <- tmp$Municipal
colnames(ump_new) <- c("Unemployment_Rate_Avg", "No_Unemployed_Avg", "No_Employed_Avg", "No_Labor_Avg", "Municipal")
ump_new$Region <- ump_new$Municipal
for (r in grep("own", ump_new$Region)){
  ump_new[r,"Region"] <- gsub( " t.*$", "", ump_new[r,"Region"])
  ump_new[r,"Region"] <- gsub( " T.*$", "", ump_new[r,"Region"])
}
ump_new$Municipal <- ifelse(ump_new$Region == "Massachusetts", NA, ump_new$Municipal)
ump_new$Year <- rep(2013, nrow(ump_new))
ump_new$State <- rep("MA", nrow(ump_new))
ump_new$County <- ifelse(ump_new$Region == backup$Municipal, backup$County, NA)
ump_new$Labor_Pct_Change <- rep(NA, nrow(ump_new))

# reorder the columns
ump_new <- ump_new[,colnames(backup)]

# save dataset to folder
#write.csv(ump_new, "C:/Users/Zhenning Kang/Documents/UMass/SEIGMA/update_misc/new_data/ump_2013.csv")

