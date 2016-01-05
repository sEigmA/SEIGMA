#######################################
## Title: Bankruptices global.R      ##
## Author(s): Xuelian Li, Jenna      ##
##            Kiridly                ## 
## Date Created:  08/11/15           ##
## Date Modified: 08/15/15 XL        ##
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

## Load formatted tax data
tax_data <- read.csv(file="taxdata.csv")
colnames(tax_data)[2]<-"Year"

## Find order of municipals in geojson files
## Each municipal is a separate feature
for(i in 1:length(MA_map_muni$features)){
  MA_map_muni$features[[i]]$properties$NAMELSAD10 <- substr(MA_map_muni$features[[i]]$properties$NAMELSAD10, 1, nchar(MA_map_muni$features[[i]]$properties$NAMELSAD10)-5)
}

MA_municipals_map <- c()
for(i in 1:length(MA_map_muni$features)){
  MA_municipals_map <- c(MA_municipals_map, MA_map_muni$features[[i]]$properties$NAMELSAD10)
}

idx_leftovers <- which(!MA_municipals_map %in% tax_data$Municipal)
leftover_munis <- MA_municipals_map[idx_leftovers]
for(i in 1:length(leftover_munis)){
  MA_map_muni$features[[idx_leftovers[i]]]$properties$NAMELSAD10 <-
    substr(leftover_munis[i], 1, nchar(leftover_munis[i])-5)
}

MA_municipals <- c()
for(i in 1:length(MA_map_muni$features)){
  MA_municipals <- c(MA_municipals, MA_map_muni$features[[i]]$properties$NAMELSAD10)
}
idx_leftovers2 <- which(!MA_municipals %in% tax_data$Municipal)
leftover_munis_map <- MA_municipals[idx_leftovers2]
MA_municipals <- sort(MA_municipals[-idx_leftovers2])

## Set graph colors (special for colorblind people)
## In order: black, orange, light blue, green, yellow, dark blue, red, pink
cbbPalette <- c("black", "red", "orange","green",
                "blue", "maroon", "deeppink", "yellow")

## Create maxs and mins for tax total levy plot in googleCharts/Plot tab
xlim <- list(
  min = min(tax_data$Year)-1,
  max = max(tax_data$Year)+1
)
ylim <- list(
  min = 0,
  ##+5 = max rate plus a little extra
  max = max(tax_data$Inflation_Adjusted_Total_Levy, na.rm=T)+5
)

## create ylim for the Percent of Levy by Class 
ylim_pct<-list(
  min = 0,
  
  max = 100
)

#################################################################
## Colors for unemployment rate legend
paint.brush1 <- colorRampPalette(colors=c("darkgreen", "white", "maroon"))
map_colors1 <- c(paint.brush1(n=25), "#999999")

paint.brush <- colorRampPalette(colors=c("white", "violetred"))
map_colors <- c(paint.brush(n=25), "black")

##Cuts for Inflation_Adjusted_Total_Levy 
TotTaxmax.val <- max(tax_data$Inflation_Adjusted_Total_Levy, na.rm=TRUE)
TotTaxmin.val <- min(tax_data$Inflation_Adjusted_Total_Levy, na.rm=TRUE)

## Puts each county year in between the cuts (n colors, n+1 cuts)
## length.out will make that many cuts
unecuts <- quantile(tax_data$Inflation_Adjusted_Total_Levy, probs = seq(0, 1, length.out = length(map_colors)), na.rm=TRUE)

labmax.val <- max(unemp_data1$No_Labor_Avg, na.rm=TRUE)
labmin.val <- min(unemp_data1$No_Labor_Avg, na.rm=TRUE)
labcuts <- quantile(unemp_data1$No_Labor_Avg, probs = seq(0, 1, length.out = length(map_colors)), na.rm=TRUE)

pctmax.val<-max(unemp_data1$Labor_Pct_Change, na.rm=FALSE)
pctmin.val<--pctmax.val
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
  
  helpText(p(strong('Please select the years for which you are interested in viewing average annual unemployment rates and the average annual number of people within the labor force.'))),
  tags$br(),
  tags$ul(
    tags$br(),
    tags$li('Select one or multiple municipalities.'),
    tags$br(),
    tags$li('To look at average annual unemployment rates and the number of people within the labor force for a single year, select single year from the drop down menu.'),
    tags$br(),
    tags$li('To look at average annual unemployment rates and the number of people within the labor force over a specific time period select multiple years from the drop down menu.  Then use the sliding bar to select a range.'),
    tags$br(),
    tags$li('Sort average annual unemployment rates and the number of people within the labor force in ascending and descending order by clicking on the column or variable title.')
    
  )
)

## Same concept
plot_side_text <- conditionalPanel(
  condition="input.tabs == 'plot'",
  h4("How to use this app:"),
  p(strong('Please select the municipality for which you are interested in viewing the average annual unemployment rate and average annual number of people within the labor force. Please do not select more than ten municipalities at a time.')),
  tags$br(),
  tags$ul(
    tags$li('Once you have selected the municipalities which you are interested in viewing, select a Variable of Interest.'),
    tags$br(),
    tags$li("To view average annual unemployment rates, select Unemployment Rate."),
    tags$br(),
    tags$li("To view the average annual number of people within the labor force, select Labor force."),
    tags$br(),
    tags$li("Select Actual Values from the Display Options to view average annual unemployment rates and the average annual number of people in the labor force for 2003-2012."),
    tags$br(),
    tags$li("Select Change Since 2003 from the Display Options to view the percent change in the average annual number of people in the labor force each year for the years 2003-2012.")
  ))


map_side_text <- conditionalPanel(
  condition="input.tabs == 'map'",
  h4("How to use this app:"),
  helpText(p(strong('Please select a yearly range and click on "Generate Map" to get started.'))),
  tags$br(),
  tags$ul(
    
    tags$li('To view average annual unemployment rate select Unemployment Rate, then click on a municipality for which you are interested in viewing average annual unemployment rate.'),
    tags$br(),
    tags$li("To view the number of people within the labor force select Labor Force, then click on a municipality for which you are interested in viewing the number of people witin the labor force."),
    tags$br(),
    tags$li("To view the change in the annual average number of people within the labor force between the year you selected and 2003 select Change Since 2003.")
  ))

info_side_text <- conditionalPanel(
  condition="input.tabs == 'info'",
  h4("How to use this app:"),
  helpText(p(strong('This tab contains more detailed information regarding the variables of interest.'))))

about_main_text <- p(strong("The SEIGMA Unemployment App"), "displays the average unemployment rate and the average number of people within the labor force in Massachusetts' municipalities annually.",
                     p(strong("Click on different tabs to see the data in different forms.")),
                     tags$br(),
                     tags$ul(
                       tags$li(p(strong("Summary"), "shows the data in table format.")),
                       tags$li(p(strong("Plot"), "compares the annual unemployment rate and the annual average number of people within the labor force for each municipality to state rates.")),
                       tags$li(p(strong("Map"), "visually displays the annual unemployment rate and the annual average number of people within the labor force by municipality")),
                       tags$li(p(strong("More Info"), "describes the annual unemployment rate and the annual average number of people within the labor force, including formulas and calculations."))
                     ))

plot_main_text <- p(strong("Variable Summary:"),
                    ## breaks between paragraphs
                    tags$br(),
                    strong("Annual Average Unemployment Rate-"),
                    " Average annual unemployment rates account for workers who have lost their jobs and are looking for new ones.  This excludes people who are not looking for work.  The unemployment rate is produced by the Bureau of Labor Statistics, which uses state and national level information from the Current Population Survey.  Municipality unemployment rates were gathered form a secition of thr BLS and CPS called the Local Areas Unemployment Statistics Series.",
                    tags$br(),
                    strong("SEIGMA. Social and Economic Impacts of Gambling in Massachusetts, University of Massachusetts School of Public Health and Health Sciences. (2014). Report on the Social and Economic Impact of Gambling in Massachusetts SEIGMA Gambling study. Report to the Massachusetts Gaming Commission & the Massachusetts department of Public Health. Retrieved from:"), a("http://www.umass.edu/seigma/sites/default/files/March%202014%20SEIGMA%20Report_6-19_for%20website.pdf", align="center"))

font_size <- 14

une_plot_options <- googleLineChart("une_plot1", width="100%", height="475px", options = list(
  
  ## set fonts
  fontName = "Source Sans Pro",
  fontSize = 14,
  
  ## set axis titles, ticks, fonts, and ranges
  hAxis = list(
    title = "Year",
    format = "####",
    ticks = seq(2003, 2012, 2),
    viewWindow = xlim,
    textStyle = list(
      fontSize = 14),
    titleTextStyle = list(
      fontSize = 16,
      bold = TRUE,
      italic = FALSE)
  ),
  vAxis = list(
    title = "Average Annual Unemployment Rate",
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

