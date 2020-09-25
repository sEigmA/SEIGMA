###############################################
## Title: Files for Building Permits Updater ##
## Author: Zhenning Kang                     ##
## Date Created:  04/06/2018                 ##
## Last Modified: 09/22/2020 ajcr.          ##
###############################################

# 1. CPI file
# # CPI can also be obtained directly from BLS API (https://www.bls.gov/developers/api_r.htm) with code below
library(blscrapeR)
# # Modify startyear and endyear dates to change dataset (i.e. 2003-2017)
df_cpi <- bls_api("CUUS0000SA0", startyear = 2000, endyear = 2019,
                  registrationKey = "a621394b12a84fe0adc76df37ca675b5",
                  annualaverage = TRUE)
CPI_annual_avg <- df_cpi %>% filter(periodName == "Annual")
CPI <- CPI_annual_avg[,c(1,4)]
write.csv(CPI, "CPI_2000_2019.csv", row.names=FALSE)

# 2. Obtain column names to use in function
table_2019 <- read_excel("building_permits_2000-2019.xlsx", sheet = 2, col_names = FALSE)
colnames(table_2019) <- unlist(c(table_2019[3,])) #extract column names
bp_2019 <- table_2019[-c(1:4), -c(2:12)] #drop data not in use
bp_2019 <- bp_2019[-367,]
colnames(bp_2019)[29] <- "Total Evaluation" #2019 dataset uses "Total Valuation" where other years use "Total Evaluation"
remove(table_2019)

col_need <- colnames(bp_2019) #keep the same columns onward
for (sheet_ind in 3:20){
# load data per year from each sheet in xlsx file
  pre_year <- read_excel("building_permits_2000-2019.xlsx", sheet = 2, col_names = F)
  colnames(pre_year) <- unlist(c(pre_year[3,])) #extract column names
  table_pre <- as.data.frame(pre_year[-c(1:4), col_need])
  bp_2019 <- rbind(bp_2019, table_pre) #merge tables
}
buildingpermits <- bp_2019

## Above code should work for this any new dataset however under close scrutiny it seems many of the sheets in the most recent 
## dataset (2018, 2019) do not have a consistent structure and, therefore, cannot be imported in this way. The following code allows 
## for individual import of some sheets that aren't able to be imported through the general function.