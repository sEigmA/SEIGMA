# get files from j drive, move to folder 

require(dplyr)
require(tidyr)
path <- "~/Desktop/Fall 2016/Seigma/seigma/rent/rent/B25058.11_16" #change from local
files <- list.files(path="~/Desktop/Fall 2016/Seigma/seigma/rent/rent/B25058", pattern="*_ann.csv") #change from local 
DF <- NULL
for (f in files) {
  dat <- read.csv(f, header=F, sep=",", na.strings="", colClasses="character")
  dat$file <- unlist(strsplit(f,split=".",fixed=T))[1]
  DF <- rbind(DF, dat)
}
DF <- (DF)[-1,]
colnames(DF) <- DF[1,]
DF <- DF[-1,]

DF <- separate(DF, "Geography", c("Municipality", "County", "State"), ",")

Rent <- DF[,-c(1, 2, 5)]
colnames(Rent) <- c("Municipal", "County", "Median Rent", "Rent Margin of Error", "Five Year Range")
Rent$`Five Year Range`<- parse_number(Rent$`Five Year Range`)
Rent$`Five Year Range` <- Rent$`Five Year Range`+1996 # create starting years

Rent$`Five Year Range` <- paste(Rent$`Five Year Range`, "-", Rent$`Five Year Range`+ 4, sep = "")

View(Rent)
for(i in 1:length(Rent$Municipal)) {
  Rent$Municipal <- gsub(Rent$Municipal, pattern=c(" [Tt]own| [Cc]ity"), replacement = "")
}



