
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

    for (i in 1:l){
      
      if(is.na(mdata$Region[i]))
        mdata$Region[i] <- "United States"
      
      if(!is.na(mdata$State[i]))
        
        mdata$Region[i] <- mdata$State[i]
      
      
      if(!is.na(mdata$County[i]))
        
        mdata$Region[i] <- mdata$County[i]
      
        
      if(!is.na(mdata$Municipal[i]))
        mdata$Region[i] <- mdata$Municipal[i]
    
    }
     
View(mdata)
      




