#########################################
## Title: Unemployment Updates         ##
## Author(s): Valerie Evans            ##
## Date Created:  12/19/2018           ##
## Date Modified: 12/23/2019           ##
#########################################
# Data from ftp server: https://download.bls.gov/pub/time.series/la/


####  SETTINGS  ####
library(tidyverse)
library(readxl)
library(blsAPI)
library(RJSONIO)


####  SUPPORTING DATA  ####
## Load column names, geography 
colnames <- read_excel("Shiny App Variables.xlsx", sheet = "Unemployment")
geography <- read.csv("geography.csv", stringsAsFactors = FALSE)
seriesid <- read.csv("unemployment_seriesid.csv", stringsAsFactors = FALSE)
seriesid$new_area_code <- paste(seriesid$new_area_code, seriesid$measure_code, sep = "")
seriesid <- seriesid[,-3]


####  DATA  ####
## 2018 data file does not contains state annual average values so will pull those separately with API (see below)

# Read in text file downloaded from ftp server
setwd("../Unemployment")
url <- "https://download.bls.gov/pub/time.series/la/la.data.28.Massachusetts"
download.file(url, "unemployment.txt" )
unemp <- read.delim("unemployment.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
unemp <- unemp %>% filter(year >= 2003 & year <= 2018) #keep only required years
unemp <- unemp[,-5]
unemp$series_id <- str_trim(unemp$series_id, side = "right")
unemployment <- merge(unemp, seriesid, by.x = "series_id", by.y = "new_series_id", all.x = TRUE)
unemployment <- drop_na(unemployment) #dataset contains metropolitian regions to remove

# Keep only M13 (annual average) values
unemployment <- unemployment %>% filter(period == "M13")
unemployment <- unemployment[,c(7,2,6,4)]

# Spread data to wide format
unemployment <- unemployment %>% spread(measure_text, value)
colnames(unemployment) <- c("region", "year", "employment", "labor.force", "unemployment", "unemployment.rate")


## Unemployoment MA -- need to add in series id to final dataframe for location 
# Pull data from API
payload <- list(
  'seriesid' = c("LAUST250000000000003", "LAUST250000000000004", "LAUST250000000000005", "LAUST250000000000006"), 
  'startyear' = 2003,
  'endyear' = 2018,
  'annualaverage' = TRUE,
  'registrationKey' = "a621394b12a84fe0adc76df37ca675b5")
response <- blsAPI(payload, 2)
json <- fromJSON(response)

apiDF_ma <- function(data){
  df <- data.frame(year=character(),
                   period=character(),
                   periodName=character(),
                   value=character(),
                   stringsAsFactors=FALSE)
  i <- 0
  for(d in data){
    i <- i + 1
    df[i,] <- unlist(d)
  }
  return(df)
}  

unemploymentrate.ma <- apiDF_ma(json$Results$series[[1]]$data)
unemployment.ma <- apiDF_ma(json$Results$series[[2]]$data)
employment.ma <- apiDF_ma(json$Results$series[[3]]$data)
laborforce.ma <- apiDF_ma(json$Results$series[[4]]$data)

# Change value type from character to numeric
unemploymentrate.ma[,4] <- as.numeric(unemploymentrate.ma[,4])
unemployment.ma[,4] <- as.numeric(unemployment.ma[,4])
employment.ma[,4] <- as.numeric(employment.ma[,4])
laborforce.ma[,4] <- as.numeric(laborforce.ma[,4])

# Rename value prior to merging
names(unemploymentrate.ma)[4] <- 'unemployment.rate'
names(unemployment.ma)[4] <- 'unemployment'
names(employment.ma)[4] <- 'employment'
names(laborforce.ma)[4] <- 'labor.force'

# Keep just annual values
unemploymentrate_ma <- unemploymentrate.ma %>% filter(periodName == "Annual")
unemployment_ma <- unemployment.ma %>% filter(periodName == "Annual")
employment_ma <- employment.ma %>% filter(periodName == "Annual")
laborforce_ma <- laborforce.ma %>% filter(periodName == "Annual")

# Merge MA data frames
unemployment_state <- unemploymentrate_ma %>% right_join(unemployment_ma) %>% right_join(employment_ma) %>% 
  right_join(laborforce_ma)
unemployment_state <- unemployment_state[,c(1,4:7)]
unemployment_state$region <- "MA"


## Bind rows from MA and Counties/Municipalities
unemployment_state$year <- as.integer(unemployment_state$year)
unemployment <- bind_rows(unemployment, unemployment_state)


## Add new columns
# Calculate the labor force percent change since 2003
year_2003 <- unemployment[which(unemployment$year == 2003),]
unemployment$Labor_Pct_Change <- round((unemployment$labor.force/year_2003$labor.force[match(unemployment$region, year_2003$region)]-1)*100, 1)

# Add geography columns
unemployment <- merge(unemployment, geography, by.x = "region", by.y = "Region", all.x = TRUE)

# Final column order
unemployment <- unemployment[,c(8:10,1:2,6,5,3,4,7)]

# Update column names
colnames(unemployment) <- colnames$`COLUMN NAME`

## Write new csv file to replace old in shiny app
setwd("..")
write_csv(unemployment, "unemployment_update.csv")


