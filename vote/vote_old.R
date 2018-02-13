require(readr)
require(rvest)

#### Download voting data ####
## Download muni data manually, scrape table for county data 

#### 2014 Q3
Q3.Muni <- read_csv("~/Desktop/Q3_2014.csv")
Q3.Muni <- Q3.Muni[,-c(2,3)]
write.csv(Q3.Muni, "Q3_2014.csv")

# url <- "http://electionstats.state.ma.us/ballot_questions/view/2467/"
# vote.table <- url %>%
#   read_html() %>%
#   html_nodes(xpath='//*[(@id = "precinct_data")]') %>%
#   html_table()

# Q3.County <- data.frame(vote.table)[,-c(1,2)]
# Q3.County[,1] <- gsub("\n\t\t\t More »", "",county.q3[,1]) 
# 
# colnames(Q3.County) <- colnames(Q3.Muni)


### 2016 Q1

Q1.Muni <- read_csv("~/Desktop/Q1_2016.csv")
Q1.Muni <- Q1.Muni[,-c(2,3)]
write.csv(Q1.Muni, "Q1_2016.csv")



