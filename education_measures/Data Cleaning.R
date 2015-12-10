#######################################
## Title: Education_Measures         ##
##                  Data Cleaning    ##
## Author(s): Emily Ramos, Arvind    ##
##            Ramakrishnan, Jenna    ##
##            Kiridly, Steve Lauer   ## 
## Date Created:  10/21/2015         ##
## Date Modified: 12/04/2014         ##
#######################################

setwd("education_measures/")
setwd("C://Users//jwbaldwin//Documents//shiny-draft//education_measures")
require(sas7bdat)
require(plyr)

## load SAS data
edum <- read.sas7bdat("bf001_01.sas7bdat")

#load column titles
column_titles <- read.csv("BF001_01_contents.csv", skip=1)
column_names<-as.character(column_titles$Label[1:60])

names(edum)[5:64]<-column_names

############################################################################

edum$`BF001_001: School Name`<-as.character(edum$`BF001_001: School Name`)
edum$`BF001_001: School Name`<-sapply(strsplit(edum[,5], ": ", fixed = T), function(x){x[2]})

query<-data.frame("school"=as.character(unique(paste(edum$`BF001_001: School Name`, "School", edum$Municipal, "MA", sep=" "))),
                  "org.code"=unique(as.character(edum$Org_Code)))
library(ggmap)
query$school<-as.character(query$school)
gc<-geocode(query$school)
query<-cbind(query, gc)
#write.csv(query,file = "query.csv")
query<-read.csv("query.csv")

#5 failed to find coordinates
View(query[is.na(query$lat==T),])


#hoosac 42.594262, -73.115702
query[18,4]<--73.115702
query[18,5]<-42.594262

#business lawrence 42.691189, -71.144918
query[790,4]<--71.144918
query[790,5]<-42.691189

#humanities leadership lawrence 42.691229, -71.144886
query[801,4]<--71.144886
query[801,5]<-42.691229

#lawrence math high 42.690156, -71.146023
query[808,4]<--71.146023
query[808,5]<-42.690156

#performing arts 42.691220, -71.145170
query[810,4]<--71.145170
query[810,5]<-42.691220

###rename query org.code to orgcode2
names(query)[3]<-c("orgcode2")
#combine lat,lon info with edum dataset
edum<-merge(edum, query[,3:5], by="orgcode2")
##########################################################################


####
# Construct new variables to show what type of school it is
# PK, K, Elementary, Middle, High School and Beyond HS
####
names(edum)[6]<-c("school.name")

library(plyr)
school.type.sum<-ddply(edum, "school.name", function(x){
                       data.frame(
                       "n.PREK"=sum(x[,7], na.rm=T),
                       "n.KIND"=sum(x[,8], na.rm=T),
                       "n.ELEM"=sum(x[,9:13], na.rm=T),
                       "n.MIDD"=sum(x[,14:16], na.rm=T),
                       "n.HIGH"=sum(x[,17:21], na.rm=T)
                       )}
                       )
school.type.sum$tot<-rowSums(school.type.sum[,2:6])

id.school.type<-function(x){
  p.prek<-x$n.PREK/x$tot
  p.kind<-x$n.KIND/x$tot
  p.elem<-x$n.ELEM/x$tot
  p.midd<-x$n.MIDD/x$tot
  p.high<-x$n.HIGH/x$tot
  
  #one school didn't report enrollment
  #View(edum[which(edum$school.name=="Taunton Alternative High School"),])
  
  if(p.prek>0.05){PREK=1}else(PREK=0)
  if(p.kind>0.05){KIND=1}else(KIND=0)
  if(p.elem>0.05){ELEM=1}else(ELEM=0)
  if(p.midd>0.05){MIDD=1}else(MIDD=0)
  if(p.high>0.05){HIGH=1}else (HIGH=0)
    
  data.frame("PREK"=PREK,"KIND"=KIND, "ELEM"=ELEM,"MIDD"=MIDD, "HIGH"=HIGH)
}
clean.school<-school.type.sum[-which(school.type.sum$school.name=="Taunton Alternative High School"),]
split.school<-split(clean.school, 
                    clean.school$school.name)
school.type<-data.frame(t(sapply(split.school,id.school.type)))
school.type$school.name<-rownames(school.type)
school.type<-rbind(school.type, c(0,0,1,"Taunton Alternative High School"))

###
#merge with edum
###
edum<-merge(edum, school.type, "school.name")

unique(edum$school.name[which(edum$PREK==1)])
unique(edum$school.name[which(edum$KIND==1)])
unique(edum$school.name[which(edum$ELEM==1)])
unique(edum$school.name[which(edum$MIDD==1)])
unique(edum$school.name[which(edum$HIGH==1)])

for (i in 1:dim(edum)[2]){
  edum[,i] <- unlist(edum[,i])}

###########################################################
#11/2/2015
write.csv(edum, file="edum_data.csv",row.names = F )

##########################################################################
#11/3/2015
edum<-read.csv("edum_data.csv", row.names = 1)
names(edum)[c(6, 66)]<-c("school.year", "org.code")
names(edum)<-sapply(strsplit(names(edum), "_"), 
       function(x){
         if(length(x)>1){substr(x[2], 6,nchar(x[2]))}
         else(x[1])}
       )


wam<-function(x){weighted.mean(x, w=Total.Students.Enrolled, na.rm = TRUE)}
info.cols<-names(edum)[c(1:6,66:73)]

#Make MA average for all school years, 
#weighting each school x school.year's data by number of students enrolled
ma.avg.fill<-data.frame(matrix(rep("NA",130), ncol=13, nrow=10))
colnames(ma.avg.fill)<-info.cols[-6]
MA_avg<-cbind(ma.avg.fill,
              ddply(edum, c("school.year"), function(x)
  numcolwise(weighted.mean, x$Total.Students.Enrolled, 
             na.rm=T)(x[!colnames(x) %in% 
                          info.cols]))
)
MA_avg$State<-"MA"


#same for county level
#county.avg.fill<-data.frame(matrix(rep("NA",1680), ncol=12, nrow=140))
#colnames(county.avg.fill)<-info.cols[-c(4,6)]

#county_avgs<-cbind(county.avg.fill,
#  ddply(edum, c("County", "school.year"), function(x)
#  numcolwise(weighted.mean, x$Total.Students.Enrolled, 
#             na.rm=T)(x[!colnames(x) %in% 
#                          info.cols]))
#)
#county_avgs$State<-"MA"
######################################################################
#12/8/2015

edum$school.year<-as.numeric(as.character(substr(edum$school.year, 1,4)))
edum$school.type_code<-paste(edum$PREK, edum$KIND, edum$ELEM, edum$MIDD, edum$HIGH)
