###########################################
## Title: Building Permits global.R      ##
## Author(s): Xuelian Li, Zhenning Kang  ## 
## Date Created:  08/05/2016             ##
## Date Modified: 05/10/2019 VE          ##
###########################################

##First file run - Environment Setup
## load necessary libraries
require(dplyr)
require(maptools)
require(Hmisc)
require(shiny)
require(googleCharts)
require(leaflet)
require(RJSONIO)
require(tidyr)

## load map data
MA_map_muni <- fromJSON("Muni_2010Census_DP1.geojson")

## Load formatted pValue data
bPermit_data1 <- read.csv(file="buildingpermits.csv")[,-1]
bPermit_data <- bPermit_data1 %>%
  select(1:5,27,7,8,28,10,11,29,13,14,30,16,17,31,32,20:26,33)
colnames(bPermit_data) <- c("Region","Year","Number_of_Months_Reported","Single_Family_Buildings","Single_Family_Units","Inflation_Adjusted_1_Family_Valuation","I2_Family_Buildings","I2_Family_Units","Inflation_Adjusted_2_Family_Valuation","I3-4_Family_Buildings","I3-4_Family_Units","Inflation_Adjusted_3_4_Family_Valuation","I5_Family_Buildings","I5_Family_Units","Inflation_Adjusted_5_Family_Valuation","Total_Buildings_Reported_Imputed","Total_Units_Reported_Imputed","Inflation_Adjusted_Total_Valuation","Inflation_Adjusted_Average_Valuation","Total_Pct_Change","Change_from_previous","Pct_Change_from_previous","Percentage_of_1_Family","Percentage_of_2_Family","Percentage_of_3_and_4_Family","Percentage_of_5_Family","Permits_Per_1000_Population")
# colnames(bPermit_data1)[2:15]<-c("Year","Number_of_Months_Reported" ,"Single_Family_Buildings","Single_Family_Units","Single_Family_validation","I2_Family_Buildings","I2_Family_Units","I2_Family_validation","I3-4_Family_Buildings","I3-4_Family_Units","I3-4_Family_validation","I5_Family_Buildings","I5_Family_Units","I5_Family_validation")
# bPermit_data<-bPermit_data1%>%
#   select(1:5,20,7:8,21,10:11,22,13:14,23,16:17,24:33)


## Find order of municipals in geojson files
## Each municipal is a separate feature
for(i in 1:length(MA_map_muni$features)){
  MA_map_muni$features[[i]]$properties$NAMELSAD10 <- substr(MA_map_muni$features[[i]]$properties$NAMELSAD10, 1, nchar(MA_map_muni$features[[i]]$properties$NAMELSAD10)-5)
}

MA_municipals_map <- c()
for(i in 1:length(MA_map_muni$features)){
  MA_municipals_map <- c(MA_municipals_map, MA_map_muni$features[[i]]$properties$NAMELSAD10)
}

idx_leftovers <- which(!MA_municipals_map %in% bPermit_data$Region)
leftover_munis <- MA_municipals_map[idx_leftovers]
for(i in 1:length(leftover_munis)){
  MA_map_muni$features[[idx_leftovers[i]]]$properties$NAMELSAD10 <-
    substr(leftover_munis[i], 1, nchar(leftover_munis[i])-5)
}

MA_municipals <- c()
for(i in 1:length(MA_map_muni$features)){
  MA_municipals <- c(MA_municipals, MA_map_muni$features[[i]]$properties$NAMELSAD10)
}
idx_leftovers2 <- which(!MA_municipals %in% bPermit_data$Region)
leftover_munis_map <- MA_municipals[idx_leftovers2]
MA_municipals <- sort(MA_municipals[-idx_leftovers2])

## Set graph colors (special for colorblind people)
## In order: black, orange, light blue, green, yellow, dark blue, red, pink
cbbPalette <- c("black", "red", "orange","green",
                "blue", "maroon", "deeppink", "yellow")

## Create maxs and mins for pValue total levy plot in googleCharts/Plot tab
xlim <- list(
  min = min(bPermit_data$Year)-1,
  max = max(bPermit_data$Year)+1
)
ylim <- list(
  min = 0,
  ##+5 = max rate plus a little extra
  max = max(bPermit_data$Total_Units_Reported_Imputed, na.rm=T)+5
)

##create ylim for the Housing Units by structure 
ylim_str<-list(
 min = 0,
  
 max = max(bPermit_data$Single_Family_Units, na.rm=T)+0.5
)

## create ylim for the change of total units since 2001
# ylim_cha<-list(
#   min = min(bPermit_data$Total_Pct_Change, na.rm=T)-0.5,
#   
#   max = max(bPermit_data$Total_Pct_Change, na.rm=T)+0.5
# )
## create ylim for the change of total units from previous year
ylim_pre<-list(
  min = min(bPermit_data$Pct_Change_from_previous, na.rm=T)-0.5,
  
  max = max(bPermit_data$Pct_Change_from_previous, na.rm=T)+0.5
)

#################################################################
## Colors for change since 2001 legend
paint.brush1 <- colorRampPalette(colors=c("darkgreen", "white", "maroon"))
map_colors1 <- c(paint.brush1(n=9), "#999999")

paint.brush <- colorRampPalette(colors=c("white", "violetred"))
map_colors <- c(paint.brush(n=20), "black")

##Cuts for Total Units reported and imputed 
TotUnitsmax.val <- max(bPermit_data$Total_Units_Reported_Imputed, na.rm=TRUE)
TotUnitsmin.val <- min(bPermit_data$Total_Units_Reported_Imputed, na.rm=TRUE)
## Puts each county year in between the cuts (n colors, n+1 cuts)
## length.out will make that many cuts
##TotUnitscuts <- quantile(bPermit_data$Total_Units_Reported_Imputed, probs = seq(0, 1, length.out = length(map_colors)), na.rm=TRUE)
TotUnitsLogcuts<-seq(0,log10(TotUnitsmax.val), length.out=length(map_colors))
TotUnitscuts1<-10^(TotUnitsLogcuts)
TotUnitscuts <- unique(c(0, TotUnitscuts1))
##Cuts for Permits Per 1,000 Population
UniPerPopmax.val <- max(bPermit_data$Permits_Per_1000_Population, na.rm=TRUE)
UniPerPopmin.val <- min(bPermit_data$Permits_Per_1000_Population, na.rm=TRUE)
UniPerPopLogcuts<-seq(0,log10(UniPerPopmax.val), length.out=length(map_colors))
UniPerPopcuts1<-10^(UniPerPopLogcuts)
UniPerPopcuts <- unique(c(0, UniPerPopcuts1))

##Cuts for change in Total_Units since 2000
# TotUniChamax.val <- max(bPermit_data$Total_Pct_Change, na.rm=TRUE)
# TotUniChamin.val <- min(bPermit_data$Total_Pct_Change, na.rm=TRUE)
# TotUniChacuts <- seq(TotUniChamin.val, TotUniChamax.val, length.out = length(map_colors1))

##Cuts for the change from the previous year
PreChamax.val<-max(bPermit_data$Pct_Change_from_previous, na.rm=TRUE)
PreChamin.val<-min(bPermit_data$Pct_Change_from_previous, na.rm=TRUE)
PreChamax1.val<-log(PreChamax.val)
#PreChamin1.val<-PreChamin.val/100
PreChacuts1 <- seq(PreChamin.val, 0, length.out = 5)
PreChaLogcuts <- seq(0, PreChamax1.val, length.out = 1)
PreChacuts2 <-exp(PreChaLogcuts)
PreChacuts<-unique(c(PreChacuts1, PreChacuts2))
##PreChacuts <- seq(PreChamin.val, PreChamax.val, length.out = length(map_colors1))

##Cuts for the Percent of Units by structure

pctmax.val<- max(bPermit_data$Percentage_of_1_Family, na.rm=TRUE)
pctmin.val<- 0
pctcuts <- seq(pctmin.val, pctmax.val, length.out = length(map_colors))



#############################
### Large Text Block Area ###
#############################

## Generate map button
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
  
  helpText(p(strong('Please select the years for which you are interested in viewing the annual total number of new housing units authorized by building permits and number of new housing units by structure size.'))),
  tags$ul(
    tags$br(),
    tags$li('Select one or multiple municipalities.'),
    tags$br(),
    tags$li('To look at the number of residential building permits, number of new housing units authorized by building permits and number of new housing units by structure size for a single year, select Single Year from the drop down menu.'),
    tags$br(),
    tags$li('To look at the data over a specific time period select Multiple Years from the drop down menu. Then use the sliding bar to select a range.'),
    tags$br(),
    tags$li('Sort the number of residential building permits, number of new housing units authorized by building permits and number of new housing units by structure size in ascending and descending order by clicking on the column or variable title.'),
    tags$br(),
    tags$li("To compare data to the Massachusetts data, select the corresponding box."),
    tags$br(),
    tags$li("Data is based on reported data plus data imputed for non-reporters and partial reporters.")
  )
)

## Same concept
plot_side_text <- conditionalPanel(
  condition="input.tabs == 'plot'",
  h4("How to use this app:"),
  helpText(p(strong('Please select the municipality for which you are interested in viewing the number of new housing units authorized by building permits and number of new housing units by structure size. Please do not select more than ten municipalities at a time.'))),
  tags$br(),
  tags$ul(
    tags$li('Once you have selected the municipalities which you are interested in viewing the annual total number of new housing units and number of new housing units by structure size, select a Variable of Interest.'),
    tags$br(),
    tags$li("To view the annual total number of new housing units authorized by building permits, select Total Number of New Housing Units."),
    tags$br(),
    tags$li("To view the number of new housing units by structure size, select Number of New Housing Units by Structure Size."),
    tags$br(),
    tags$li("Select Actual Values from the Display Options to view the number of annual total number of new housing units authorized by building permits for the years of interest."),
    tags$br(),
    tags$li("To view the annual total number of new housing units authorized by building permits change between the year you selected and the previous year select Change from the Previous Year."),
    tags$br(),
    tags$li("To compare data to the Massachusetts data, select the corresponding box.")
  ))


map_side_text <- conditionalPanel(
  condition="input.tabs == 'map'",
  h4("How to use this app:"),
  helpText(p(strong('Please select a year and click on "Generate Map" to get started.'))),
  tags$br(),
  tags$ul(
    
    tags$li('To view the annual total number of new housing units authorized by building permits select Total Number of New Housing Units, then click on a municipality for which you are interested in viewing the annual total number of new housing units.'),
    tags$br(),
    tags$li("To view the number of new housing units by structure size select Number of New Housing Units by Structure Size, then click on a municipality for which you are interested in viewing the number of new housing units by structure size."),
    tags$br(),
    tags$li("To view the annual total number of new housing units authorized by building permits change between the year you selected and the previous year select Change from the Previous Year."),
    tags$br(),
    tags$li("To view the number of new housing units by structure size select a specific structure from the number of new housing units by structure size list.")
    
  ))

info_side_text <- conditionalPanel(
  condition="input.tabs == 'info'",
  h4("How to use this app:"),
  helpText(p(strong('This tab contains more detailed information regarding the variables of interest.'))))

about_main_text <- p(strong("The SEIGMA Residential Building Permits App"), "displays the total number of residential building permits, total number of new housing units authorized by building permits and number of housing units by structure size in Massachusetts' municipalities between 2000 and 2017.",
                     p(strong("Click on different tabs to see the data in different forms.")),
                     tags$br(),
                     tags$ul(
                       tags$li(p(strong("Summary"), "shows the data in table format.")),
                       tags$li(p(strong("Plot"), "displays measures for each municipality over time. Choose between viewing the annual total number of new housing units authorized by building permits or the number of new housing units, grouped by structure size.")),
                       tags$li(p(strong("Map"), "displays the geographic pattern in total number of new housing units authorized by building permits, total number of new housing units per 1000 inhabitants, and number of new housing units by structure size for each municipality for the years 2000-2017.")),
                       tags$li(p(strong("More Info"), "describes the annual total number of new housing units authorized by building permits, total number of new housing units per 1000 inhabitants, and number of new housing units by structure size, including formulas and calculations."))
                     ))

plot_main_text <- p(strong("Variable Summary:"),
                    ## breaks between paragraphs
                    tags$br(),
                    strong("Annual Housing Permits-"),
                    " Data are obtained from the U.S. Census Bureau's Survey of Construction. Building permits data are collected from individual permit offices, most of which are municipalities.  The statistics are based on reports submitted by local building permit officials in response to a mail survey and imputed data.",
                    tags$br(),
                    strong("SEIGMA. Social and Economic Impacts of Gambling in Massachusetts, University of Massachusetts School of Public Health and Health Sciences. (2014). Report on the Social and Economic Impact of Gambling in Massachusetts SEIGMA Gambling study. Report to the Massachusetts Gaming Commission & the Massachusetts department of Public Health. Retrieved from:"), a("http://www.umass.edu/seigma/sites/default/files/March%202014%20SEIGMA%20Report_6-19_for%20website.pdf", align="center"))

font_size <- 14

## column chart for housing units by structure
Pct_plot_options <- googleColumnChart("pct_plot1", width="100%", height="475px") 

## line chart
TotUni_plot_options <- googleLineChart("TotUni_plot1", width="100%", height="475px", options = list(
  
  ## set fonts
  fontName = "Source Sans Pro",
  fontSize = 14,
  
  ## set axis titles, ticks, fonts, and ranges
  hAxis = list(
    title = "Year",
    format = "####",
    ticks = seq(2000, 2017, 2),
    viewWindow = xlim,
    textStyle = list(
      fontSize = 14),
    titleTextStyle = list(
      fontSize = 16,
      bold = TRUE,
      italic = FALSE)
  ),
  vAxis = list(
    title = "Total New Housing Units",
    viewWindow = ylim,
    textStyle = list(
      fontSize = 14),
    titleTextStyle = list(
      fontSize = 16,
      bold = TRUE,
      italic = FALSE)
  ),
  
  ## set legend fonts
  legend = list(
    textStyle = list(
      fontSize=14)),
  
  ## set chart area padding
  chartArea = list(
    top = 50, left = 75,
    height = "80%", width = "70%"
  ),
  
  ## set colors
  colors = cbbPalette,
  
  ## set point size
  pointSize = 3,
  
  ## set tooltip font size
  ## Hover text font stuff
  tooltip = list(
    textStyle = list(
      fontSize = 14
    )
  )
))



##Change from the previous year
PreUniCha_plot_options<- googleLineChart("PreUniCha_plot", width="100%", height="475px",options = list(
  
  ## set fonts
  fontName = "Source Sans Pro",
  fontSize = 14,
  
  ## set axis titles, ticks, fonts, and ranges
  hAxis = list(
    title = "Year",
    format = "####",
    ticks = seq(2001, 2017, 2),
    viewWindow = xlim,
    textStyle = list(
      fontSize = 14),
    titleTextStyle = list(
      fontSize = 16,
      bold = TRUE,
      italic = FALSE)
  ),
  vAxis = list(
    title = "Percent Change in Total New Housing Units from the Previous Year (%)",
    # viewWindow = ylim_pre_max,
    textStyle = list(
      fontSize = 14),
    titleTextStyle = list(
      fontSize = 16,
      bold = TRUE,
      italic = FALSE)
  ),
  
  ## set legend fonts
  legend = list(
    textStyle = list(
      fontSize=14)),
  
  ## set chart area padding
  chartArea = list(
    top = 50, left = 75,
    height = "75%", width = "70%"
  ),
  
  ## set colors
  colors = cbbPalette,
  
  ## set point size
  pointSize = 3,
  
  ## set tooltip font size
  ## Hover text font stuff
  tooltip = list(
    textStyle = list(
      fontSize = 14
    )
  )
))

