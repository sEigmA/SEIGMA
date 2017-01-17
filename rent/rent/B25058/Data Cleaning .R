
###########################################
## Title: Rent Data Cleaning             ##
## Author(s): BF, JB                     ## 
## Date Created:  12/16/17               ##
## Date Modified: 1/12/17                ##
###########################################


require(dplyr)
require(tidyr)
<<<<<<< HEAD
require(readr)
require(devtools)
=======
#require(read)
>>>>>>> 21fc4e4c1614716c21e4e602648b7c687a6f187a
setwd( "~/Desktop/Fall 2016/Seigma/seigma/rent/rent/B25058")
path <- "~/Desktop/Fall 2016/Seigma/seigma/rent/rent/B25058" #change to local
files <- list.files(path="~/Desktop/Fall 2016/Seigma/seigma/rent/rent/B25058", pattern="*_ann.csv")
DF <- NULL
for (f in files) {
  dat <- read.csv(f, header=F, sep=",", na.strings="", colClasses="character")
  dat$file <- unlist(strsplit(f,split=".",fixed=T))[1]
  DF <- rbind(DF, dat)
}
DF <- (DF)[-1,]
colnames(DF) <- DF[1,]
DF <- DF[-1,]

DF <- separate(DF, "Geography", c("Municipality", "County", "State"), ",") #spread geo column into Muni, County, State
Rent <- DF[,-c(1, 2, 5)]
colnames(Rent) <- c("Municipal", "County", "Median Rent", "Rent Margin of Error", "Five Year Range")
Rent$`Five Year Range`<- parse_number(Rent$`Five Year Range`)
Rent$`Five Year Range` <- Rent$`Five Year Range`+1996 # create starting years
Rent$`Five Year Range` <- as.numeric(Rent$`Five Year Range`)
Rent$`Median Rent` <- as.numeric(Rent$`Median Rent`)

# Inflation 
Adjusted_index<-data.frame(Year=2005:2015, Annual=c(195.3,201.6,207.342,215.303,214.537,218.056,224.939,229.594, 232.957,
                                                    236.736,237))


Adjusted_index$Inflation_rate<-237/Adjusted_index$Annual
Adjusted_index <- data.frame(apply(Adjusted_index, 2, as.numeric))
Rent$IA_Med_Rent<-rep(0,nrow(Rent))

# Adjust median rent for inflation
for (i in 2005:2010) {
  Rent$IA_Med_Rent[which(Rent$`Five Year Range`==i)]<-Rent$`Median Rent`[which(Rent$`Five Year Range`==i)]*Adjusted_index$Inflation_rate[which(Adjusted_index$Year==i)]
}

Rent$IA_Med_Rent<-round(Rent$IA_Med_Rent,0)


# Adjust margin of error for inflation
Rent$IA_Rent_Error<-rep(0,nrow(Rent))
for (i in 2005:2010) {
  Rent$IA_Rent_Error[which(Rent$`Five Year Range`==i)]<-Rent$`Rent Margin of Error`[which(Rent$`Five Year Range`==i)]*Adjusted_index$Inflation_rate[which(Adjusted_index$Year==i)]
}

Rent$IA_Rent_Error<-round(Rent$IA_Rent_Error,0)


Rent$`Five Year Range` <- paste(Rent$`Five Year Range`, "-", Rent$`Five Year Range`+ 4, sep = "")
for(i in 1:length(Rent$Municipal)) {
  Rent$Municipal <- gsub(Rent$Municipal, pattern=c(" [Tt]own| [Cc]ity"), replacement = "")
}

Rent <- subset(Rent, Municipal != "County subdivisions not defined")
Rent$`Median Rent`<- ifelse(Rent$`Median Rent`!=as.numeric(Rent$`Median Rent`), "NA", as.numeric(Rent$`Median Rent`))
Rent$`Rent Margin of Error` <- ifelse(Rent$`Rent Margin of Error`!=as.numeric(Rent$`Rent Margin of Error`), "NA", as.numeric(Rent$`Rent Margin of Error`))


write.csv(Rent, "AR003_03_5yr")


