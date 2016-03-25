###########################################
## Title: Schools                        ##
## Author(s): Xuelian Li, Emily Ramos,   ## 
##            Arvind Ramakrishnan,       ##
##            Jenna Kiridly, Steve Lauer,##
##            Justin Baldwin             ##
## Date Created:  12/10/2015             ##
## Date Modified: 12/10/2015             ##
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

# ## load map data
# #MA_map_county <- fromJSON("County_2010Census_DP1.geojson")
# MA_map_muni <- fromJSON("Muni_2010Census_DP1.geojson")
edu_data <- read.csv(file="edum_data.csv")
#put this in data cleaning
colnames(edu_data)[7:21]<-gsub(x=names(edu_data)[7:21],pattern=".", replacement=" ", fixed=T)
colnames(edu_data)[16]<-"Eighth Grade"

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
 all_school_table<-lapply(split(edu_data, edu_data$County), FUN=function(x){unique(x$school.name)})
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
range.table<-data.frame(apply(edu_data[,c(7:65)], 2, FUN=function(x){range(x, na.rm=T)}))
names(range.table)<-gsub(names(range.table), pattern=".", replacement=" ", fixed=T)
names(range.table)<-gsub(names(range.table), pattern="enrolled  1", replacement="Enrolled %")
names(range.table)[c(24,25)]<-c("Native Hawaiian/Pacific Islander","Multi-Race/Non-Hispanic")
names(range.table)<-gsub(names(range.table), pattern="   ", replacement=" ", fixed=T)
names(range.table)[c(33,39)]<-c("Low Income Students Enrolled %","High Needs Students Enrolled %")


# 
# #############################
# ### Large Text Block Area ###
# #############################
# 
# ## Generate map button
# gen_map_button <- HTML('<style type="text/css">
#                        .action-button {
#                        -moz-box-shadow:inset 0px 1px 0px 0px #54a3f7;
#                        -webkit-box-shadow:inset 0px 1px 0px 0px #54a3f7;
#                        box-shadow:inset 0px 1px 0px 0px #54a3f7;
#                        background:-webkit-gradient(linear, left top, left bottom, color-stop(0.05, #007dc1), color-stop(1, #0061a7));
#                        background:-moz-linear-gradient(top, #007dc1 5%, #0061a7 100%);
#                        background:-webkit-linear-gradient(top, #007dc1 5%, #0061a7 100%);
#                        background:-o-linear-gradient(top, #007dc1 5%, #0061a7 100%);
#                        background:-ms-linear-gradient(top, #007dc1 5%, #0061a7 100%);
#                        background:linear-gradient(to bottom, #007dc1 5%, #0061a7 100%);
#                        filter:progid:DXImageTransform.Microsoft.gradient(startColorstr="#007dc1", endColorstr="#0061a7",GradientType=0);
#                        background-color:#007dc1;
#                        -moz-border-radius:3px;
#                        -webkit-border-radius:3px;
#                        border-radius:3px;
#                        border:1px solid #124d77;
#                        display:inline-block;
#                        cursor:pointer;
#                        color:#ffffff;
#                        font-family:arial;
#                        font-size:16px;
#                        padding:12px 36px;
#                        text-decoration:none;
#                        text-shadow:0px 1px 0px #154682;
#                        }
#                        .action-button:hover {
#                        background:-webkit-gradient(linear, left top, left bottom, color-stop(0.05, #0061a7), color-stop(1, #007dc1));
#                        background:-moz-linear-gradient(top, #0061a7 5%, #007dc1 100%);
#                        background:-webkit-linear-gradient(top, #0061a7 5%, #007dc1 100%);
#                        background:-o-linear-gradient(top, #0061a7 5%, #007dc1 100%);
#                        background:-ms-linear-gradient(top, #0061a7 5%, #007dc1 100%);
#                        background:linear-gradient(to bottom, #0061a7 5%, #007dc1 100%);
#                        filter:progid:DXImageTransform.Microsoft.gradient(startColorstr="#0061a7", endColorstr="#007dc1",GradientType=0);
#                        background-color:#0061a7;
#                        }
#                        .action-button:active {
#                        position:relative;
#                        top:1px;
#                        }
# 
#                        </style>')

summary_side_text <- conditionalPanel(
  condition="input.tabs == 'summary'",
  ## h4 created 4th largest header
  h4("How to use this app:"),
  ## Creates text

  helpText(p(strong('Please select the years for which you are interested in viewing Massachusetts school profiles and student mobility data.'))),
  tags$br(),
  tags$ul(
    tags$br(),
    tags$li('Select a county then select one or multiple municipalities.'),
    tags$br(),
    tags$li('To look at the enrollment profile of a school by race/ethnicity, gender or grade levels, select "Race/Ethnicity", "Gender" or "Grade Level" from the "Variables" list.'),
    tags$br(),
    tags$li('To look at the student mobility of English language learning students, students with disabilities, low income students or students with high needs, select "English Language Learners", "Students with Disabilities", "Low Income" or "High Needs" from the "Variables" list.'),
    tags$br(),
    tags$li('Sort columns in ascending or descending order by clicking on the column or variable title.')

  )
)

## Same concept
plot_side_text <- conditionalPanel(
  condition="input.tabs == 'plot'",
  h4("How to use this app:"),
  p(strong('Please select the county in which the school you are interested in is located.')),
  tags$br(),
  tags$ul(
    tags$li("Then select the type of data which you are interested in viewing."),
    tags$br(),
    tags$li("To view enrollment data such as the percentage of students by race/ethnicity, gender, grade level, or focus group, select 'Enrollment Profile' from the Data drop down menu."),
    tags$br(),
    tags$li("To view mobility data such as churn, intake, and stability rates by specific interest group select 'Mobility Measure' from the 'Data' drop down menu."),
    tags$br(),
  tags$li("Next, please select a school for which you are interested in viewing data"),
  tags$br(),
  tags$li("To view the percentage of students by race/ethnicity, gender, grade level, or focus group select 'Race/Ethnicity', 'Gender', 'Grade Level', or 'Focus Group' from the 'Profile Variable' list."),
  tags$br(),
  tags$li("To view churn, intake, and stability rates select either Enlgish Language Learners, Students with Disabilities, Low income, or High Needs."),
  tags$br(),
  tags$li("Then select either 'Rate' or 'Enrollment' from the options to view rates or number of students per interest group.")
  ))

# 
map_side_text <- conditionalPanel(
  condition="input.tabs == 'lmap'",
  h4("How to use this app:"),
  helpText(p(strong("Please select a year, and click on 'Generate Map' to get started"))),
  tags$br(),
  tags$ul(
    tags$li("To view the percentage of students by race/ethnicity, gender, grade level or focus group, select 'Race/Ethnicity', 'Gender', 'Grade Level', or 'Focus Group' from the 'Variables' list."),
    tags$br(),
    tags$li("To view churn, intake, and stability rates select either 'Enlgish Language Learners', 'Students with Disabilities', 'Low income', or 'High Needs' from the 'Variables' list."),
    tags$br(),
    tags$li("Then select the level of information by which you are interested in viewing the data from the 'Choose Level to Map' drop down menu."),
    tags$br(),
    tags$li("To view the actual number of students within each focus group select any of the 'Count' options from the'Choose Level to Map' drop down menu ."),
    tags$br(),
    tags$li("To view the percentage of students within each focus group select any of the 'Percentage' options from the'Choose Level to Map' drop down menu ."),
    tags$br(),
    tags$li("To view the churn, intake or Stability rate of the students within each focus group select any of the 'Churn', 'Intake', or 'Stability' options from the'Choose Level to Map' drop down menu .")
  ))

info_side_text <- conditionalPanel(
  condition="input.tabs == 'info'",
  h4("How to use this app:"),
  helpText(p(strong('This tab contains more detailed information regarding the variables of interest.'))))

 about_main_text <- p(strong("The SEIGMA Schools App"), "displays the percentage of students by race/ethnicity, gender or grade levels and student mobility data for Massachusetts schools.",
                      p(strong("Click on different tabs to view the data in different formats.")),
                      tags$br(),
                      tags$ul(
                        tags$li(p(strong("Summary"), "shows the source data in table format.")),
                        tags$li(p(strong("Plot"), "compares the percentage of students by race and ethnicity, gender, and grade levels, as well as student mobility profiles for each school.")),
                        tags$li(p(strong("Map"), "visually displays the percentage of students by race and ethnicity, gender, and grade level, as well as student mobility profiles for each school.")),
                        tags$li(p(strong("More Info"), "describes schools profile and student mobility data including formulas and calculations."))
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
 