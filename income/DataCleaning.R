require(sas7bdat)

## load SAS data
income <- read.sas7bdat("at003_03_5yr.sas7bdat")

## give columns relevant titles
column_titles <- read.csv("AT003_03_5yr_contents.csv", skip=1)
colnames(income)[5:32] <- as.character(column_titles$Label[1:28])

inc_data <- income

## remove all "Margin of Error" columns
inc_data <- inc_data[,-c(grep("Margin", colnames(inc_data)))]

## remove unnecessary characters from column names
colnames(inc_data)[5:20] <- substring(colnames(inc_data[5:20]), first=12)

inc_data2 <- inc_data

## Replace N/A's with "NA" to remove the slash.
inc_data2$Region <- replace(inc_data2$Region,inc_data2$Region=="N/A", NA)
inc_data2$County <- replace(inc_data2$County,inc_data2$County=="N/A", NA)
inc_data2$State <- replace(inc_data2$State,inc_data2$State=="N/A", NA)
inc_data2$Municipal <- replace(inc_data2$Municipal,inc_data2$Municipal=="N/A", NA)

inc_data2$Region  <- ifelse(!is.na(inc_data2$Region), "United States", "MA")
inc_data2$Region  <- ifelse(!is.na(inc_data2$County), paste(inc_data2$County, "County"), inc_data2$Region)
inc_data2$Region  <- ifelse(!is.na(inc_data2$Municipal),as.character(inc_data2$Municipal),as.character(inc_data2$Region))

inc_data2$County <- paste(inc_data2$County, "County")

colnames(inc_data2)[8] <- "Five_Year_Average"

inc_data2$Five_Year_Average <- as.numeric(substr(inc_data2$Five_Year_Average, 6, 9))

##Remove IDs
inc_data3 <- inc_data2[,-c(5,6)]

## save and reload to make factors into numeric (this is faster than the other methods)
write.csv(inc_data3, file="incomedata.csv")
inc_data3 <- read.csv("incomedata.csv")[,-c(1)]

colnames(inc_data3)[7:18] <- c("Median_Household_Income", "Upper_Limits-Lowest_Quintile", "Upper_Limits-Second_Quintile", "Upper_Limits-Third_Quintile", "Upper_Limits-Fourth_Quintile", "Lower_Limit_Top_Five_Pct", "Lowest_Quintile_Means", "Second_Quintile_Means", "Third_Quintile_Means", "Fourth_Quintile_Means", "Highest_Quintile_Means", "Lower_Limit_Top_Five_Pct_Means")

write.csv(inc_data3, file="income/incomedata.csv")
