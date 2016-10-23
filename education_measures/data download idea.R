#10/21/2016
#incorporate data download into pipeline


#################################################
# automate data download as well. try without selenium
# something along the lines of 
# download.file("http://profiles.doe.mass.edu/state_report/enrollmentbygrade.aspx?mode=school&year=2004&Continue.x=7&Continue.y=6&export_excel=yes", "file.xls")
# use some package not openxlsx that can read .xls files. (works with excel)
#################################################
library(htmltab)

dir.create("raw")
setwd("raw")


#define years
years<-2003:2015

#start with enrollment by grade

if(dir.exists("Enrollment by Grade")==F){
  dir.create("Enrollment by Grade")}
  setwd("Enrollment by Grade")

  
  
for(i in years){
  url<-paste(c("http://profiles.doe.mass.edu/state_report/enrollmentbygrade.aspx?mode=school&year=", i, "&Continue.x=7&Continue.y=6&export_excel=yes"),
             collapse = "")
  filename<-paste(c("EnrollmentGrade",i,".xls"), collapse="")
  download.file(url, filename)
}  

files<-list.files()  


#for i files, read in like this, ignore warnings
file=htmltab(doc="EnrollmentGrade2015.xls", which = 1)
  