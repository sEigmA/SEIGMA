############################################
## Title: Schools Profile Data Update     ##
## Author(s):  Valerie Evans              ##
## Date Created:   02/19/2019             ##
## Date Modified:  01/22/2019             ##
############################################
# Data can be downloaded through a search for data by topic and school year via this link: 
# http://profiles.doe.mass.edu/state_report/


####  SETTINGS  ####
library(tidyverse)
library(readxl)
library(data.table)
library(magrittr)


####  SUPPORTING DATA  ####
## Load column titles, schoolsnames, municipal-counties school data 
## **ORG.CODE data doesn't match up**
colnames <- read_excel("Shiny App Variables.xlsx", sheet = "School Profiles")
schoollocation <- read.csv("schools_2019.csv", stringsAsFactors = FALSE)


####  DATA IMPORT  ####
## Enrollment By Grade
setwd("School Profiles/Enrollment by Grade") #2003-04 school year columns organized differently and need to be imported separately
file.list <- list.files(pattern = '*.xlsx')
file.list <- setNames(file.list, file.list) #only when you need a column with the file names
grade.list <- lapply(file.list, read_excel, skip = 1)
grade.list <- Map(function(grade, name) {
  grade$Year <- name
  grade
}, grade.list, names(grade.list))
grade <- rbindlist(grade.list, fill = TRUE)
grade$Year <- str_replace(grade$Year, "enrollmentbygrade_", "")
grade$Year <- str_replace(grade$Year, ".xlsx", "")

## Enrollment By Race & Gender
# These racial categories were only introduced in 2005-2006 so first 2 years lacking these columns: 
#"Native Hawaiian, Pacific Islander"vand "Multi-Race, Non-Hispanic"
setwd("../..")
setwd("School Profiles/Enrollment by Race&Gender")
file.list <- list.files(pattern = '*.xlsx')
file.list <- setNames(file.list, file.list) #only when you need a column with the file names
racegender.list <- lapply(file.list, read_excel, skip = 1)
racegender.list <- Map(function(racegender, name) {
  racegender$Year <- name
  racegender
}, racegender.list, names(racegender.list))
racegender <- rbindlist(racegender.list, fill = TRUE)
racegender$Year <- str_replace(racegender$Year, "enrollmentbyracegender_", "")
racegender$Year <- str_replace(racegender$Year, ".xlsx", "")

## Enrollment By Selected Populations (Students with disabilities, English Learner, High Needs, Low Income, Economically Disadvantaged)
setwd("../..")
setwd("School Profiles/Enrollment by Selected Populations")
file.list <- list.files(pattern = '*.xlsx')
file.list <- setNames(file.list, file.list) #only when you need a column with the file names
selectedpop.list <- lapply(file.list, read_excel, skip = 1)
selectedpop.list <- Map(function(selectedpop, name) {
  selectedpop$Year <- name
  selectedpop
}, selectedpop.list, names(selectedpop.list))
selectedpop <- rbindlist(selectedpop.list, fill = TRUE)
selectedpop$Year <- str_replace(selectedpop$Year, "selectedpopulations_", "")
selectedpop$Year <- str_replace(selectedpop$Year, ".xlsx", "")

## Enrollment By Mobility Rate Report (Students with disabilities, English Learner, High Needs, Low Income, Economically Disadvantaged)
## Columns change over time (e.g. Low Income gets replaced by Economically Disadvantaged in 2015)
## Data only available from 2015 for Economically Disadvantaged, 2012 for High Needs, and until 2014 for Low Income
setwd("../..")
setwd("School Profiles/Mobility")
file.list <- list.files(pattern = '*.xlsx')
file.list <- setNames(file.list, file.list) #only when you need a column with the file names
mobility.list <- lapply(file.list, read_excel, skip = 1)
mobility.list <- Map(function(mobility, name) {
  mobility$Year <- name
  mobility
}, mobility.list, names(mobility.list))
mobility <- rbindlist(mobility.list, fill = TRUE)
mobility$Year <- str_replace(mobility$Year, "mobilityrates_", "")
mobility$Year <- str_replace(mobility$Year, ".xlsx", "")
mobility <- mobility %>% separate(Year, c("Mobility", "Year"), "_")
mobility <- mobility[,c(1:2,9,8,3:7)]
mobility <- mobility %>% gather(variable, value, -('School Name':'Mobility')) %>% 
  unite(temp, Mobility, variable) %>% 
  spread(temp, value)
mobility <- mobility[,c(1:3,7,4,5,8,6,12,9,10,13,11,17,14,15,18,16,22,19,20,23,21,27,24,25,28,26)]

## Join all datasets (grade, race/gender, selected populations, mobility)
schools1 <- grade %>% full_join(racegender, by = c("School Name", "School Code", "Year")) %>% 
  full_join(selectedpop, by = c("School Name", "School Code", "Year")) %>% 
  full_join(mobility, by = c("School Name", "School Code", "Year"))

## Merge by ORG.CODE
schools1$`School Code` <- as.numeric(schools1$`School Code`)
schools2 <- full_join(schools1, schoollocation, by = c("School Code" = "ORG.CODE"))

## Modify column order
schools2 <- schools2[,c(1:2,71:73,19,3:18,20:69,74:79)]

## Convert data columns to numeric
schools2[,7:72] <- apply(schools2[,7:72], 2, FUN = function(x){as.numeric(gsub(x, pattern = ",", replacement = ""))})

## Add Gender/Race Counts
schools3 = schools2
schools3$FemaleCounts <- ceiling(schools3$Total*schools3$Females/100)
schools3$MaleCounts <- ceiling(schools3$Total*schools3$Males/100)
schools3$African.AmericanCounts <- ceiling(schools3$Total*schools3$`African American`/100)
schools3$AsianCounts <- ceiling(schools3$Total*schools3$Asian/100)
schools3$HispanicCounts <- ceiling(schools3$Total*schools3$Hispanic/100)
schools3$WhiteCounts <- ceiling(schools3$Total*schools3$White/100)
schools3$Native.AmericanCounts <- ceiling(schools3$Total*schools3$`Native American`/100)
schools3$Native.Hawaiian.Pacific.IslanderCounts <- ceiling(schools3$Total*schools3$`Native Hawaiian, Pacific Islander`/100)
schools3$Multi.Race.Non.HispanicCounts <- ceiling(schools3$Total*schools3$`Multi-Race, Non-Hispanic`/100)

## Add Selected Populations Counts
schools3$NonFLL <- ceiling(schools3$Total - schools3$`First Language Not English #`)
schools3$NonELL <- ceiling(schools3$Total - schools3$`English Language Learner #`)
schools3$NonDISAB <- ceiling(schools3$Total - schools3$`Students With Disabilities #`)
schools3$NonLOW <- ceiling(schools3$Total - schools3$`Low Income #`)
schools3$NonHIGH <- ceiling(schools3$Total - schools3$`High Needs #...15`)
schools3$NonED <- ceiling(schools3$Total - schools3$`Economically Disadvantaged #`)

## Final column order and names
schools <- schools3[,c(1:27,30:31,28:29,32:52,58:62,63:67,53:57,68:93)]
colnames(schools) <- colnames$`COLUMN NAME`

## Write new csv file to replace old in shiny app
write_csv(schools, "schools_update.csv")


