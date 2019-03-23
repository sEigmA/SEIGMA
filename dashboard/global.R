###############################
## Title: global.R           ##
## App: SEIGMA dashboard     ##
## Author: Zhenning Kang     ##
## Date Created:  09/27/2017 ##
## Last Modified: 03/23/2019 ##
###############################

##### SETTINGS #####
library(shiny)
library(shinydashboard)
library(dplyr)
library(reshape2)
library(ggplot2)
library(shinyBS)

##### DATA #####
### DEMOGRAPHIC TAB
dem_data <- read.csv(file="data/demodata.csv")
dem_data$Year <- as.factor(as.numeric(substr(dem_data$Five_Year_Range, 1, 4))+2)
dem_data$Under35 <- dem_data$Age_under_20_Pct_plot+dem_data$Age_20_34_Pct_plot
dem_data$Under65 <- dem_data$Under35 + dem_data$Age_35_54_Pct_plot + dem_data$Age_55_64_Pct_plot

### SOCIAL TAB
# data for married status plot
mar_data <- read.csv(file="data/BA002_02_marriagedata.csv")
mar_data$Year <- as.factor(as.numeric(substr(mar_data$Five_Year_Range, 1, 4))+2)
mar_data$Gender <- relevel(mar_data$Gender, "Male")
# data for education plot
edu_data <- read.csv(file="data/edudata.csv")[,-1]
edu_data$Year <- as.factor(as.numeric(substr(edu_data$Five_Year_Range, 1, 4))+2)
edu_data$No_HS_Pct <- 100-edu_data$HS_Pct
# data for suicide plot
sui_data <- read.csv(file="data/SASuicidedata_Updated2017.csv")[,-1]
#If there is no age adjusted rate, get rid of the bounds and standard errors
sui_data$Age.Adjusted.Rate.Lower.Bound[is.na(sui_data$Age.Adjusted.Rate)] <- NA
sui_data$Age.Adjusted.Rate.Upper.Bound[is.na(sui_data$Age.Adjusted.Rate)] <- NA
sui_data$Age.Adjusted.Rate.Standard.Error[is.na(sui_data$Age.Adjusted.Rate)] <- NA
sui_data$County <- gsub("US", "United States", sui_data$County)
colnames(sui_data) <- gsub("County", "Region", colnames(sui_data))
# data for vetaran plot
vet_data <- read.csv(file="data/vetstatusdata.csv")[,-1]
vet_data$Year <- as.factor(as.numeric(substr(vet_data$Five_Year_Range, 1, 4))+2)
# data for school plot
sch_data <- read.csv(file="data/BF001_002.csv")
names(sch_data)[58] <- "Churn.Enrollment.for.High.Needs.Students" 
colnames(sch_data)[7:21]<-gsub(x=names(sch_data)[7:21],pattern=".", replacement=" ", fixed=T)
sch_data$school.year<-as.numeric(substr(sch_data$school.year, 1, 4))
names(sch_data)[c(75,74, 78)]<-c("Lng", "Lat", "loc")

### ECONOMICS TAB
## Load formatted Income status data
inc_data <- read.csv(file="data/incomedata.csv")[,-1]
inc_data$Year <- as.factor(as.numeric(substr(inc_data$Five_Year_Range, 1, 4))+2)
## Load formatted Rent data
ren_data <- read.csv(file="data/rent.csv")
ren_data$Year <- as.factor(as.numeric(substr(ren_data$Five.Year.Range, 1, 4))+2)
ren_data$Region <- ifelse(ren_data$Municipal=="Massachusetts", "MA", as.character(ren_data$Municipal))
## Load formatted Poverty data
pov_data <- read.csv(file="data/povratedata.csv")[,-1]
pov_data$Year <- as.factor(as.numeric(substr(pov_data$Five_Year_Range, 1, 4))+2)
## Load formatted unemp data
## -1 eliminates first column [rows,columns]
une_data <- read.csv(file="data/unempdata2.csv")
## Load formatted Bankruptcy data
ban_data <- read.csv(file="data/bankdata.csv")
## Load formatted Employment data
emp_data <- read.csv(file="data/empdata3.csv")
## Load formatted Building Permits data
bui_data <- read.csv(file="data/buildingdata.csv")[,-1]
#colnames(bui_data)[2:15]<-c("Year","Number_of_Months_Reported" ,"Single_Family_Buildings","Single_Family_Units","Single_Family_validation","I2_Family_Buildings","I2_Family_Units","I2_Family_validation","I3-4_Family_Buildings","I3-4_Family_Units","I3-4_Family_validation","I5_Family_Buildings","I5_Family_Units","I5_Family_validation")
## Load formatted pValue data
val_data <- read.csv(file="data/pValuedata3.csv")
colnames(val_data)[4:10]<-c("Year","Residential","Open_Space", "Commercial", "Industrial", "Personal_Property", "Total_Assessed")
val_data <- filter(val_data, Year != 2017)
## Load formatted Tax data
tax_data <- read.csv(file="data/taxdata2.csv")
colnames(tax_data)[2:3]<-c("Year", "Total_Budget")


### REGIONS
MA_municipals <- as.character(na.omit(unique(dem_data$Municipal)))
muni_county <- data.frame(unique(na.omit(subset(dem_data, select = c("Municipal", "County")))))

###### INFORMATION #####
### POP-UPS
age_pop <- "The number of people within each age group, for a region over a specified five year range. Age groups were specified in the dataset as <5, 5-9, 10-14, 15-19, 20-24, 25-34, 35-44, 45-54, 55-59, 60-64, 65-74, 75-84, and 85+. For this app, the number of categories for age has been collapsed to the following six groups: <20, 20-34, 35-54, 55-64, 65-74, and 75+. This is done in order to simplify the presentation of data. Source: American Community Survey (ACS)."

rac_pop <- "The number of people within each race for a region over a specified five year range. Data is available as ‘one race’ and ‘two or more races’. Only the ‘one race’ categories are presented here. One race categories are: White, Black or African American, Asian, American Indian or Alaska Native, Native Hawaiian or Other Pacific Islander, or Other (some other race). Source: American Community Survey (ACS)."

gen_pop <- "The number of people within each gender, for a region over a specified five year range. Source: American Community Survey (ACS)."

his_pop <- "The number of people within each ethnicity, for a region over a specified five year range. Ethnicities were listed as Hispanic or not Hispanic. Source: American Community Survey (ACS)"

edu_pop <- "The number of people within each level of educational attainment for a specific region over a specific five-year period of time. All people represented in this measure are at least 25 years of age. Respondents were classified according to highest level of school completed. When a municipality is missing data, this indicates that data cannot be displayed because the number of people is too small. Source: American Community Survey (ACS)."

mar_pop <- "The number of people within each marital status category for a region over a specified five year range. Differences in the number of married males and females occurs because reporting is based on usual residence but spouses may live in different areas. When the number of people in a particular marital status category is too small, data cannot be displayed. Source: American Community Survey (ACS)."

sui_pop <- "Age-adjusted suicide rates are expressed as the number of suicides, per 100,000 persons, reported each calendar year for the region you select. Rates are considered 'unreliable' when the death count is less than 20 and thus are not displayed. Age-adjusted Suicide Rate is calculated by: Age-adjusted Suicide Rate = Count / Population * 100,000. Source: Centers for Disease Control and Prevention (CDC) WONDER."

vet_pop <-  "The number of people with active duty military service, and/or service in the military Reserves or National Guard. All people are at least 18 years of age. Source: American Community Survey (ACS)."

eng_pop <- "The number of students whose first language is a language other than English, and who are unable to perform ordinary classroom work in English. Source: MA Department of Education."

dis_pop <- "The number of students who have received an Individualized Education Program (IEP), which provides free services in public schools to children who need special education. Source: MA Department of Education."

inc_pop <- "The income of the household which includes all individuals ages 15 and over. Median annual household income provides a clear trend to assess residents' household income over time. Data was collected at multiple levels to allow for comparative analysis at municipal, state, and US levels. Source: American Community Survey (ACS). "

ren_pop <- "Contract rent is the dollar amount of the rental obligation specified in the lease. Five-year median rent estimates were collected between 2002 and 2015 and adjusted for inflation to the 2015 dollar. Data were collected at multiple levels to allow for analysis at multiple geographic scales; municipality, state, and national levels. Source: American Community Survey (ACS)."

pov_pop <- "A person’s total family income in the last 12 months compared to the poverty threshold appropriate for that person's family size and composition. If the total income of that person's family is less than the threshold appropriate for that family, then the people is considered below the poverty level. Poverty is defined at the family level and not the household level; the poverty status of the household is determined by the poverty status of the householder. Source: American Community Survey (ACS)."

une_pop <- "The unemployment rate represents the number of unemployed people as a proportion of the total labor force. For example, if the civilian labor force equals 100 people and 7 people are unemployed, then the unemployment rate would be 7 percent. Source: Bureau of Labor Statistics (BLS)/Current Population Survey (CPS) Local Areas Unemployment Statistics Series."

emp_pop <- "To estimate monthly employment, all employees who were paid at any point in the past year are counted (this includes full-time, part-time, seasonal, salaried, and hourly employees). The total number of employees is then divided by the number of pay periods per calendar year at each business establishment. Source: MA Department of Unemployment Assistance."

# ban_pop <- "Bankruptcy is a legal procedure that allows individuals and businesses to resolve debts to their creditors."

bus_pop <- "Any bankruptcy filed by a business, corporation, or professional partnership. Chapter 7 provides for liquidation which includes the sale of a debtor's nonexempt property and the distribution of the funds to the creditors. Chapter 11 provides a reorganization for corporations or partnerships to propose a restructuring plan which keeps the business afloat by repaying debt over time. Chapter 12 provides with the debt of a family farmer or family fisherman and allows them to create a payment plan to pay back their debt over the course of three to five years, if they can prove regular income. Chapter 13 is an individual debt adjustment which allows an individual to create a payment plan to pay back their debt over the course of three to five years, if they can prove regular annual income. Source: US Courts."

per_pop <- "Any bankruptcy filed by an individual for a personal, family, or household purpose. Chapter 7 provides for liquidation which includes the sale of a debtor's nonexempt property and the distribution of the funds to the creditors. Chapter 11 provides a reorganization for an individual to propose a restructuring plan which keeps an estate afloat by repaying debt over time but is typically only filed by small business owners. Chapter 13 is an individual debt adjustment which allows an individual to create a payment plan to pay back their debt over the course of three to five years, if they can prove regular annual income. Source: US Courts."

bui_pop <- "Annual residential building permits data are collected from people permit offices, most of which are municipalities. The statistics are based on reports submitted by local building permit officials in response to a mail survey and imputed data. Source: U.S. Census Bureau Survey of Construction."

val_pop <- "Assessed property values in Massachusetts are based on 'full and fair cash value'. Massachusetts General Laws defines 'full and fair cash value' as the price an owner willing, but not under compulsion, to sell, ought to receive from one willing, but not under compulsion, to buy. Source: MA Department of Revenue Division of Local Services. "

tax_pop <- "A levy, or tax, on property that the owner is required to pay. The tax is given by the region in which the property is located. Source: MA Department of Revenue."
