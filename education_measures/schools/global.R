###########################################
## Title: Schools                        ##
## Author(s): Xuelian Li, Emily Ramos,   ## 
##            Arvind Ramakrishnan,       ##
##            Jenna Kiridly, Steve Lauer,##
##            Justin Baldwin             ##
## Date Created:  12/10/2015             ##
## Date Modified: 12/10/2015             ##
##                05/08/2019 VE          ##
###########################################


##First file run - Environment Setup
## load necessary libraries
require(dplyr)
##require(sp)
require(maptools)
##require(rgeos)
require(Hmisc)
require(reshape2)
require(shiny)
require(googleCharts)
require(leaflet)
require(RJSONIO)
require(tidyr)
require(DT)

# ## load map data
# #MA_map_county <- fromJSON("County_2010Census_DP1.geojson")
# MA_map_muni <- fromJSON("Muni_2010Census_DP1.geojson")
edu_data <- read.csv(file="BF001_002.csv")
names(edu_data)[58] <- "Churn.Enrollment.for.High.Needs.Students" 
#edu_data<-edu_data[,-c(1:2)]
#put this in data cleaning
colnames(edu_data)[7:21]<-gsub(x=names(edu_data)[7:21],pattern=".", replacement=" ", fixed=T)
#colnames(edu_data)[16]<-"Eighth Grade"

edu_data$school.year<-as.numeric(substr(edu_data$school.year, 1, 4))
#reorder dataframe to match old dataframe and save work
# edu_data<-cbind(edu_data[,c(1:65, 2, 68, 67)] ,
#                      "PREK"=rep(NA, nrow(edu_data)),
#                      "KIND"=rep(NA, nrow(edu_data)),
#                      "ELEM"=rep(NA, nrow(edu_data)),
#                      "MIDD"=rep(NA, nrow(edu_data)),
#                      "HIGH"=rep(NA, nrow(edu_data)),
#               edu_data[,c(71:85,66:71)] 
# )
names(edu_data)[c(75,74, 78)]<-c("Lng", "Lat", "loc")

## Find order of municipals in geojson files
## Each municipal is a separate feature
# for(i in 1:length(MA_map_muni$features)){
#   MA_map_muni$features[[i]]$properties$NAMELSAD10 <- substr(MA_map_muni$features[[i]]$properties$NAMELSAD10, 1, nchar(MA_map_muni$features[[i]]$properties$NAMELSAD10)-5)
# }
# 
# MA_municipals_map <- c()
# for(i in 1:length(MA_map_muni$features)){
#   MA_municipals_map <- c(MA_municipals_map, MA_map_muni$features[[i]]$properties$NAMELSAD10)
# }
# 
# idx_leftovers <- which(!MA_municipals_map %in% emp_data$Municipal)
# leftover_munis <- MA_municipals_map[idx_leftovers]
# for(i in 1:length(leftover_munis)){
#   MA_map_muni$features[[idx_leftovers[i]]]$properties$NAMELSAD10 <-
#     substr(leftover_munis[i], 1, nchar(leftover_munis[i])-5)
# }
# 
# 
MA_county <- sort(as.character(unique(edu_data$County)))
 MA_municipals <- sort(as.character(unique(edu_data$Municipal)))
 all_schools<-as.character(unique(edu_data$school.name))
 
 #When a school has no mobility data that needs to not show up in options
 #on the side bar. Due to formatting issues there's this if else puzzle to do
 
 nodata<-unlist(sapply(split(edu_data, edu_data$school.name), FUN=
   function(x){ifelse(
     #if there's only one year
     nrow(x)==1,
    ifelse(
     #are all mobility data missing? if yes T
     all(apply(t(data.frame(apply(x[,48:72], 2, FUN=is.na))), 1, all))==T,
     #return school name
      return(as.character(x$school.name[1])),
     #else
     return(NULL)),
    
    #if there are multiple years of data for a school
    ifelse(
      #are all mobility data missing? if yes T
      all(apply(data.frame(apply(x[,48:72], 2, FUN=is.na)), 1, all))==T,
      #return school name
      return(as.character(x$school.name[1])),
      #else
      return(NULL))
   )
    
    }
 ))
 
 #subtract those with no mobility data
 
 all_schools<-all_schools[-which(all_schools %in% nodata)]
 all_school_table<-lapply(split(edu_data, edu_data$County), FUN=function(x){
   sk<-as.character(unique(x$school.name))
   okschools<-sk[!sk %in% nodata]
   l=c(" ",okschools)
   l[order(l)]})
 all_school_table[[" "]]<-c(" ",all_schools)[order(c(" ",all_schools))]
 
 #ut the interest groups that have non data in the mobility data in a list
 
#  school_availabilitytable<-lapply(split(edu_data[,46:65], edu_data$school.name),
#                                   FUN=function(x){
#                                     
#                                     names<-colnames(x)
#                                     colnas<-apply(x,2, function(y){sum(is.na(y))})
#                                     names[which(colnas!=nrow(x))]
#                                   })
#  school_availabilitytable<-lapply(school_availabilitytable,
#                                   function(x){
#                                     available<-c()
#                                     
#                                     n.disab<-sum(grepl(x, pattern="Disabilities"))
#                                     n.high<-sum(grepl(x, pattern="High"))
#                                     n.low<-sum(grepl(x, pattern="Low"))
#                                     n.english<-sum(grepl(x, pattern="English"))
#                                     
#                                     if(n.english!=0){available<-append(available, "English Language Learner Students")}
#                                     if(n.disab!=0){available<-append(available, "Students with Disabilities")}
#                                     if(n.low!=0){available<-append(available, "Low Income Students")}
#                                     if(n.high!=0){available<-append(available, "High Needs Students")}
#                                     
#                                     available
#                                   })
# school_availabilitytable[[" "]]<-c("English Language Learner Students", "Students with Disabilities" ,
#                                    "Low Income Students",  "High Needs Students")
# school_availabilitytable<-school_availabilitytable[-which(names(school_availabilitytable) %in% nodata)]
#   
#  > howmanyopts[howmanyopts==0]
#  Cuttyhunk Elementary   Ma Academy for Math and Science School                                 Monterey 
#  0                                        0                                        0 
#  Pathways Early College Innovation School                 Peter Fitzpatrick School                           South Egremont 
#  0                                        0                                        0 
#  > 
 
# for(i in 1:length(MA_map_muni$features)){
#   MA_municipals <- c(MA_municipals, MA_map_muni$features[[i]]$properties$NAMELSAD10)
# }
# 
# idx_leftovers2 <- which(!MA_municipals %in% emp_data$Municipal)
# leftover_munis_map <- MA_municipals[idx_leftovers2]
# MA_municipals <- sort(MA_municipals)
# 
# 
# 

## Set graph colors (special for colorblind people,turquoise)
 cbbPalette <- c("cyan","darkviolet","deeppink", "blue","green","yellow","darkorange","red",
                 "darksalmon","deepskyblue","lawngreen","magenta","brown") 
## Create maxs and mins for googleCharts/Plot tab
xlim <- list(
  min = min(edu_data$school.year)-1,
  max = max(edu_data$school.year)+1
)
ylim <- list(
  min = min(edu_data$Females, na.rm=T)-5,

  ##+5 = max Avg monthly employment plus a little extra
  max = max(edu_data$Females, na.rm=T)+5
)

# ## Colors for a Employment legend
paint.brush <- colorRampPalette(colors=c("white", "violetred"))
map_colors <- c(paint.brush(n=25), "black")
# 
#####
#create min and max values for colors of all variables in the map
range.table<-data.frame(apply(edu_data[,c(7:72)], 2, FUN=function(x){range(x, na.rm=T)}))
names(range.table)<-gsub(names(range.table), pattern=".", replacement=" ", fixed=T)
names(range.table)<-gsub(names(range.table), pattern="enrolled  1", replacement="Enrolled %")
names(range.table)[c(24,25)]<-c("Native Hawaiian/Pacific Islander","Multi-Race/Non-Hispanic")
names(range.table)<-gsub(names(range.table), pattern="   ", replacement=" ", fixed=T)
names(range.table)[c(33,39, 41)]<-c("Low Income Students Enrolled %","High Needs Students Enrolled %", "Economically Disadvantaged Students Enrolled %")


# 
# #############################
# ### Large Text Block Area ###
# #############################
# 
# ## Generate map button
gen_map_button <- HTML('<style type="text/css">
                       .action-button {
                       -moz-box-shadow:inset 0px 1px 0px 0px #54a3f7;
                       -webkit-box-shadow:inset 0px 1px 0px 0px #54a3f7;
                       box-shadow:inset 0px 1px 0px 0px #54a3f7;
                       background:-webkit-gradient(linear, left top, left bottom, color-stop(0.05, #007dc1), color-stop(1, #0061a7));
                       background:-moz-linear-gradient(top, #007dc1 5%, #0061a7 100%);
                       background:-webkit-linear-gradient(top, #007dc1 5%, #0061a7 100%);
                       background:-o-linear-gradient(top, #007dc1 5%, #0061a7 100%);
                       background:-ms-linear-gradient(top, #007dc1 5%, #0061a7 100%);
                       background:linear-gradient(to bottom, #007dc1 5%, #0061a7 100%);
                       filter:progid:DXImageTransform.Microsoft.gradient(startColorstr="#007dc1", endColorstr="#0061a7",GradientType=0);
                       background-color:#007dc1;
                       -moz-border-radius:3px;
                       -webkit-border-radius:3px;
                       border-radius:3px;
                       border:1px solid #124d77;
                       display:inline-block;
                       cursor:pointer;
                       color:#ffffff;
                       font-family:arial;
                       font-size:16px;
                       padding:12px 36px;
                       text-decoration:none;
                       text-shadow:0px 1px 0px #154682;
                       }
                       .action-button:hover {
                       background:-webkit-gradient(linear, left top, left bottom, color-stop(0.05, #0061a7), color-stop(1, #007dc1));
                       background:-moz-linear-gradient(top, #0061a7 5%, #007dc1 100%);
                       background:-webkit-linear-gradient(top, #0061a7 5%, #007dc1 100%);
                       background:-o-linear-gradient(top, #0061a7 5%, #007dc1 100%);
                       background:-ms-linear-gradient(top, #0061a7 5%, #007dc1 100%);
                       background:linear-gradient(to bottom, #0061a7 5%, #007dc1 100%);
                       filter:progid:DXImageTransform.Microsoft.gradient(startColorstr="#0061a7", endColorstr="#007dc1",GradientType=0);
                       background-color:#0061a7;
                       }
                       .action-button:active {
                       position:relative;
                       top:1px;
                       }

                       </style>')

summary_side_text <- conditionalPanel(
  condition="input.tabs == 'summary'",
  ## h4 created 4th largest header
  h4("How to use this app:"),
  ## Creates text

  helpText(p(strong('Please select the years for which you are interested in viewing Massachusetts school profiles and student mobility data.'))),
  tags$br(),
  tags$ul(
    tags$li('Select a county then select one or multiple municipalities.'),
    tags$br(),
    tags$li('To look at the enrollment profile of a school by race/ethnicity, gender or grade levels, select "Race/Ethnicity", "Gender" or "Grade Level" from the "Variables" list.'),
    tags$br(),
    tags$li('To look at the student mobility of English language learning students, students with disabilities, low income students or students with high needs, select "English Language Learners", "Students with Disabilities", "Low Income", "Economically Disadvantaged" or "High Needs" from the "Variables" list.'),
    tags$br(),
    tags$li('Sort columns in ascending or descending order by clicking on the column or variable title.')
  
  )
)

## Same concept
plot_side_text <- conditionalPanel(
  condition="input.tabs == 'plot'",
  h4("How to use this app:"),
  helpText(p(strong('Please choose the school you are interested in'))),
  tags$br(),
  tags$ul(
    tags$li("First select the type of data which you are interested in viewing"),
    tags$br(),
    tags$li("To view school enrollment data such as the percentage of students by race/ethnicity, gender, grade level, or focus group, select 'Enrollment Profile' from the 'Data' drop down menu"),
    tags$br(),
    tags$li("To view student mobility data such as churn, intake, and stability rates by specific interest group select 'Mobility Measure' from the 'Data' drop down menu"),
    tags$br(),
    tags$li("Select a school for which you are interested in viewing data"),
    tags$br(),
    tags$li("You can narrow down the list of schools to choose from by first selecting the county where the school is located"),
    tags$br(),
    tags$li("To view student mobility data from an interest group select either English Language Learners, Students with Disabilities, Low income, Economically Disadvantaged or High Needs"),
  tags$br(),
  tags$li("Then select either mobility rate or enrollment"),
  tags$br(),
  tags$li("If the plot is empty, the school did not report the data selected")
  ))

# 
map_side_text <- conditionalPanel(
  condition="input.tabs == 'lmap'",
  h4("How to use this app:"),
  helpText(p(strong("Please select a year to get started"))),
  tags$br(),
  tags$ul(
    tags$li("To zoom in the map press the '+' sign at the top left corner of the map."),
    tags$br(),
    tags$li("To view enrollment data such as the percentage of students by race/ethnicity, gender, or grade level, select 'Enrollment Profile' from the Data drop down menu."),
    tags$br(),
    tags$li("To view mobility data such as churn, intake, and stability rates by specific profile variable select 'Mobility Measure' from the 'Data' drop down menu."),
    tags$br(),
    tags$li("Then select the level of information by which you are interested in viewing the data from the 'Choose Level to Map' drop down menu."),
    tags$br(),
    tags$li("If you selected 'Mobility Profile' select either 'Rate' or 'Enrollment' to view the profile variable you are interested in.")
  ))

info_side_text <- conditionalPanel(
  condition="input.tabs == 'info'",
  h4("How to use this app:"),
  helpText(p(strong('This tab contains more detailed information regarding the variable of interest.'))))

about_main_text <- p(strong("The SEIGMA Schools App"), "displays enrollment and mobility data for Massachusetts schools and includes:",
                      tags$br(),
                      tags$ul(
                        tags$li("General information about students enrolled in Massachusetts schools including race/ethnicity, gender, and grade level."),
                        tags$br(),
                        tags$li("Enrollment information about English language learners, students whose first language is not English, students with disabilities, low income students, economically disadvantaged students and students classified as high needs."),
                        tags$br(),
                        tags$li("Information about student mobility, including intake (number of students entering schools), churn (number of new students entering into and transferring out of schools), and stability (the number of students that remian in a school over the course of a school year).")
                        
                      ))

plot_main_text <- p(strong("Variable Summary:"),
                    ## breaks between paragraphs
                    tags$br(),
                    strong("Annual Average Monthly Employment-"),
                    " Info about Avg. Monthly Employment.",
                    tags$br(),
                    strong("SEIGMA. Social and Economic Impacts of Gambling in Massachusetts, University of Massachusetts School of Public Health and Health Sciences. (2014). Report on the Social and Economic Impact of Gambling in Massachusetts SEIGMA Gambling study. Report to the Massachusetts Gaming Commission & the Massachusetts department of Public Health. Retrieved from:"), a("http://www.umass.edu/seigma/sites/default/files/March%202014%20SEIGMA%20Report_6-19_for%20website.pdf", align="center"))

 font_size <- 14
# 
 
 percentcolchart <- googleColumnChart("percentcolplot",width="100%", height = "500px")
 
 countcolchart <- googleColumnChart("countcolplot",width="100%", height = "500px")
 
 
 mobenrollment_plot_options<-googleColumnChart("mobenrollment_plot",width="100%", height = "500px")

 
 mobrate_plot_options<-googleColumnChart("mobrate_plot",width="100%", height = "500px")
 
 ##################################
 # Add map casino icons
 
 MAcasinos <- data.frame("Name"=c("Wynn Boston Harbor",
                                "Plainridge Park Casino",
                                "MGM Springfield"),
                       "Lat"=c(42.394964,42.0330381,42.1006063),
                       "Lon"=c(-71.066760,-71.3039442,-72.5870506))
 star <- makeIcon( iconUrl = "www/star.png",
                      iconWidth = 30, iconHeight = 30,
                      iconAnchorX = 15, iconAnchorY = 15)
 
 casinosOPEN <- data.frame("Name"=c("Mohegan Sun",
                                  "Foxwoods"),
                         "Lat"=c(41.491549,41.473775),
                         "Lon"=c(-72.091842,-71.960177))
 gc2 <- makeIcon( iconUrl = "www/greencircle2.gif",
                   iconWidth = 20, iconHeight = 20,
                   iconAnchorX = 10, iconAnchorY = 10)
 
 
 casinosCLOSED <- data.frame("Name"=c(
   "Tiverton",
   "River Casino"),
   "Lat"=c(41.660301,42.824163),
   "Lon"=c(-71.155845,-73.937884))
 gc1 <- makeIcon( iconUrl = "www/greencircle1.png",
                  iconWidth = 20, iconHeight = 20,
                  iconAnchorX = 10, iconAnchorY = 10)
 
 
 
 #Plainicon <- makeIcon( iconUrl = "www/plainridge_logo.png",
#                      iconWidth = 120, iconHeight = 50,
 #                     iconAnchorX = 60, iconAnchorY = 25)
 #Wynnicon <- makeIcon( iconUrl = "www/wynn_logo.png",
#                      iconWidth = 120, iconHeight = 50,
#                      iconAnchorX = 60, iconAnchorY = 25)