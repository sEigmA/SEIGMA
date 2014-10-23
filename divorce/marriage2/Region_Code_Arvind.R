
getwd()
setwd("C:/Users/ramaka/Desktop/Courses F14/SEIGMA-Official-Github/SEIGMA_10_22_14/divorce/marriage2")

mdata <- read.csv("marriagedata.csv")

origmdata <- read.csv("marriagedata.csv")

l <- length(mdata$Municipal)

## Replace N/A's with "NA" to remove the slash.
mdata$Region <- replace(mdata$Region,mdata$Region=="N/A", NA)
mdata$County <- replace(mdata$County,mdata$County=="N/A", NA)
mdata$State <- replace(mdata$State,mdata$State=="N/A", NA)
mdata$Municipal <- replace(mdata$Municipal,mdata$Municipal=="N/A", NA)

  mdata$Region  <- ifelse(!is.na(mdata$Region),"United States","MA")
  mdata$Region  <- ifelse(!is.na(mdata$County),as.character(mdata$County),as.character(mdata$Region))
mdata$Region  <- ifelse(!is.na(mdata$Municipal),as.character(mdata$Municipal),as.character(mdata$Region))

