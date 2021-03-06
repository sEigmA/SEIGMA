#######################################
## Title: Property Value global.R    ##
## Author(s): Xuelian Li             ## 
## Date Created:  07/11/2016         ##
## Date Modified: 05/10/2019 VE      ##
#######################################

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
pValue_data <- read.csv(file="propertyvalue.csv")
colnames(pValue_data)[4:10]<-c("Year","Residential","Open_Space", "Commercial", "Industrial", "Personal_Property", "Total_Assessed")

## Find order of municipals in geojson files
## Each municipal is a separate feature
for(i in 1:length(MA_map_muni$features)){
  MA_map_muni$features[[i]]$properties$NAMELSAD10 <- substr(MA_map_muni$features[[i]]$properties$NAMELSAD10, 1, nchar(MA_map_muni$features[[i]]$properties$NAMELSAD10)-5)
}

MA_municipals_map <- c()
for(i in 1:length(MA_map_muni$features)){
  MA_municipals_map <- c(MA_municipals_map, MA_map_muni$features[[i]]$properties$NAMELSAD10)
}

idx_leftovers <- which(!MA_municipals_map %in% pValue_data$Municipal)
leftover_munis <- MA_municipals_map[idx_leftovers]
for(i in 1:length(leftover_munis)){
  MA_map_muni$features[[idx_leftovers[i]]]$properties$NAMELSAD10 <-
    substr(leftover_munis[i], 1, nchar(leftover_munis[i])-5)
}

MA_municipals <- c()
for(i in 1:length(MA_map_muni$features)){
  MA_municipals <- c(MA_municipals, MA_map_muni$features[[i]]$properties$NAMELSAD10)
}
idx_leftovers2 <- which(!MA_municipals %in% pValue_data$Municipal)
leftover_munis_map <- MA_municipals[idx_leftovers2]
MA_municipals <- sort(MA_municipals[-idx_leftovers2])

## Set graph colors (special for colorblind people)
## In order: black, orange, light blue, green, yellow, dark blue, red, pink
cbbPalette <- c("black", "red", "orange","green",
                "blue", "maroon", "deeppink", "yellow")

## Create maxs and mins for pValue total levy plot in googleCharts/Plot tab
xlim <- list(
  min = min(pValue_data$Year)-1,
  max = max(pValue_data$Year)+1
)
ylim <- list(
  min = 0,
  ##+5 = max rate plus a little extra
  max = max(pValue_data$Total_Assessed_Million, na.rm=T)+5
)

## create ylim for the Percent of Levy by Class 
##ylim_pct<-list(
##  min = 0,
  
##  max = max(pValue_data$Percentage_of_Residential, na.rm=T)+0.5
##)

## create ylim for the change of pValue total levy since 2003
ylim_cha<-list(
  min = min(pValue_data$Total_Assessed_Pct_Change, na.rm=T)-0.5,
  
  max = max(pValue_data$Total_Assessed_Pct_Change, na.rm=T)+0.5
)

#################################################################
## Colors for legend
paint.brush1 <- colorRampPalette(colors=c("darkgreen", "white", "maroon"))
map_colors1 <- c(paint.brush1(n=30), "#999999")

paint.brush <- colorRampPalette(colors=c("white", "violetred"))
map_colors <- c(paint.brush(n=30), "black")

##Cuts for Inflation_Adjusted_Total_Levy 
TotpValuemax.val <- max(pValue_data$Inflation_Adjusted_Total_Assessed, na.rm=TRUE)
TotpValuemin.val <- min(pValue_data$Inflation_Adjusted_Total_Assessed, na.rm=TRUE)
## Puts each county year in between the cuts (n colors, n+1 cuts)
## length.out will make that many cuts
TotpValuecuts <- quantile(pValue_data$Inflation_Adjusted_Total_Assessed, probs = seq(0, 1, length.out = length(map_colors)), na.rm=TRUE)

##Cuts for change in Total_Levy since 2003
pValueChamax.val <- max(pValue_data$Total_Assessed_Pct_Change, na.rm=TRUE)
pValueChamin.val <- min(pValue_data$Total_Assessed_Pct_Change, na.rm=TRUE)
pValueChacuts <- seq(pValueChamin.val, pValueChamax.val, length.out = length(map_colors1))

##Cuts for the Percent of Levy by Class
pctmax.val<-max(pValue_data$Percentage_of_Residential, na.rm=TRUE)
pctmin.val<--0
##pctmin.val<-min(unemp_data1$Labor_Pct_Change, na.rm=FALSE)
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
  
  helpText(p(strong('Please select the years for which you are interested in viewing the annual total assessed property values and percent assessed property values by class.'))),
  tags$br(),
  tags$ul(
    tags$li('Select one or multiple municipalities.'),
    tags$br(),
    tags$li('To look at the annual total assessed property values and percent assessed property values by class for a single year, select single year from the drop down menu.'),
    tags$br(),
    tags$li('To look at the annual total assessed property values and percent assessed property values by class over a specific time period select multiple years from the drop down menu. Then use the sliding bar to select a range.'),
    tags$br(),
    tags$li('Sort the annual total assessed property values and percent assessed property values by class in ascending and descending order by clicking on the column or variable title.')
    
  )
)

## Same concept
plot_side_text <- conditionalPanel(
  condition="input.tabs == 'plot'",
  h4("How to use this app:"),
  helpText(p(strong('Please select the municipality for which you are interested in viewing the annual total assessed property values and percent of the total assessed property values by class. Please do not select more than ten municipalities at a time.'))),
  tags$br(),
  tags$ul(
    tags$li('Once you have selected the municipalities which you are interested in viewing the annual total assessed property values and percent of the total assessed property values by class, select a Variable of Interest.'),
    tags$br(),
    tags$li("To view the annual total assessed property values, select Total Assessed Property Values."),
    tags$br(),
    tags$li("To view the percent assessed property values by class, select Percent of Assessed Property Values by Class."),
    tags$br(),
    tags$li("Select Actual Values from the Display Options to view the annual total assessed property values for the years 2003-2018."),
    tags$br(),
    tags$li("Select Change Since 2003 from the Display Options to view the percent change in the the annual total assessed property values each year for the years 2003-2018.")
  ))


map_side_text <- conditionalPanel(
  condition="input.tabs == 'map'",
  h4("How to use this app:"),
  helpText(p(strong('Please select a year and click on "Generate Map" to get started.'))),
  tags$br(),
  tags$ul(
    
    tags$li('To view the annual total assessed property values select Total assessed property values, then click on a municipality for which you are interested in viewing the annual total assessed property values.'),
    tags$br(),
    tags$li("To view the percent assessed property values by class select Percent of assessed property values by Class, then click on a municipality for which you are interested in viewing the percent assessed property values by class."),
    tags$br(),
    tags$li("To view the annual total assessed property values change between the year you selected and year 2003 select Change Since 2003."),
    tags$br(),
    tags$li("To view the percent assessed property values by a specific class select a specific class from the Percent of assessed property values by Class list.")
  ))

info_side_text <- conditionalPanel(
  condition="input.tabs == 'info'",
  h4("How to use this app:"),
  helpText(p(strong('This tab contains more detailed information regarding the variables of interest.'))))

about_main_text <- p(strong("The SEIGMA Assessed Property Values App"), "displays the total assessed property values and percent assessed property values by class in Massachusetts' municipalities annually.",
                     p(strong("Click on different tabs to see the data in different forms.")),
                     tags$br(),
                     tags$ul(
                       tags$li(p(strong("Summary"), "shows the data in table format.")),
                       tags$li(p(strong("Plot"), "compares the annual total assessed property values and percent assessed property values by class for each municipality over time.")),
                       tags$li(p(strong("Map"), "visually displays the annual total assessed property values and percent assessed property values by class for each municipality")),
                       tags$li(p(strong("More Info"), "describes the annual total assessed property values and percent assessed property values by class, including formulas and calculations."))
                     ))

plot_main_text <- p(strong("Variable Summary:"),
                    ## breaks between paragraphs
                    tags$br(),
                    strong("Annual Average Unemployment Rate-"),
                    " Average annual unemployment rates account for workers who have lost their jobs and are looking for new ones. This excludes people who are not looking for work. The unemployment rate is produced by the Bureau of Labor Statistics, which uses state and national level information from the Current Population Survey. Municipality unemployment rates were gathered from a section of thr BLS and CPS called the Local Areas Unemployment Statistics Series.",
                    tags$br(),
                    strong("SEIGMA. Social and Economic Impacts of Gambling in Massachusetts, University of Massachusetts School of Public Health and Health Sciences. (2014). Report on the Social and Economic Impact of Gambling in Massachusetts SEIGMA Gambling study. Report to the Massachusetts Gaming Commission & the Massachusetts department of Public Health. Retrieved from:"), a("http://www.umass.edu/seigma/sites/default/files/March%202014%20SEIGMA%20Report_6-19_for%20website.pdf", align="center"))

font_size <- 14

## stacked column chart
Pct_plot_options <- googleColumnChart("pct_plot1", width="100%", height="475px") 

pValue_plot_options <- googleLineChart("pValue_plot1", width="100%", height="475px", options = list(
  
  ## set fonts
  fontName = "Source Sans Pro",
  fontSize = 14,
  
  ## set axis titles, ticks, fonts, and ranges
  hAxis = list(
    title = "Year",
    format = "####",
    ticks = seq(2003, 2018, 2),
    viewWindow = xlim,
    textStyle = list(
      fontSize = 14),
    titleTextStyle = list(
      fontSize = 16,
      bold = TRUE,
      italic = FALSE)
  ),
  vAxis = list(
    title = "Total Assessed Property Values (2018 dollars, Million)",
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

pValueCha_plot_options<- googleLineChart("pValue_plot2", width="100%", height="475px", options = list(
  
  ## set fonts
  fontName = "Source Sans Pro",
  fontSize = 14,
  
  ## set axis titles, ticks, fonts, and ranges
  hAxis = list(
    title = "Year",
    format = "####",
    ticks = seq(2003, 2018, 2),
    viewWindow = xlim,
    textStyle = list(
      fontSize = 14),
    titleTextStyle = list(
      fontSize = 16,
      bold = TRUE,
      italic = FALSE)
  ),
  vAxis = list(
    title = "Chang in Total Assessed Property Values since 2003 (%)",
    viewWindow = ylim_cha,
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

# Pct_plot_options<- googleLineChart("pct_plot1", width="100%", height="475px", options = list(
#   
#   ## set fonts
#   fontName = "Source Sans Pro",
#   fontSize = 14,
#   
#   ## set axis titles, ticks, fonts, and ranges
#   hAxis = list(
#     title = "Year",
#     format = "####",
#     ticks = seq(2003, 2013, 2),
#     viewWindow = xlim,
#     textStyle = list(
#       fontSize = 14),
#     titleTextStyle = list(
#       fontSize = 16,
#       bold = TRUE,
#       italic = FALSE)
#   ),
#   vAxis = list(
#     title = "Percent of pValue Levy by Class (%)",
#     viewWindow = ylim_pct,
#     textStyle = list(
#       fontSize = 14),
#     titleTextStyle = list(
#       fontSize = 16,
#       bold = TRUE,
#       italic = FALSE)
#   ),
#   
#   ## set legend fonts
#   legend = list(
#     textStyle = list(
#       fontSize=14)),
#   
#   ## set chart area padding
#   chartArea = list(
#     top = 50, left = 75,
#     height = "75%", width = "70%"
#   ),
#   
#   ## set colors
#   colors = cbbPalette,
#   
#   ## set point size
#   pointSize = 3,
#   
#   ## set tooltip font size
#   ## Hover text font stuff
#   tooltip = list(
#     textStyle = list(
#       fontSize = 14
#     )
#   )
# ))

