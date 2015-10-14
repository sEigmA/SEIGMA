#######################################
## Title: Bankruptices global.R      ##
## Author(s): Xuelian Li, Jenna      ##
##            Kiridly                ## 
## Date Created:  08/11/15           ##
## Date Modified: 08/15/15 XL        ##
#######################################

##First file run - Environment Setup

require(RJSONIO)
require(dplyr)
require(leaflet)
require(shiny)
require(googleCharts)
require(reshape2)
require(tidyr)
require(maptools)
require(Hmisc)
require(ggplot2)

## load map data
MAmap <- fromJSON("County_2010Census_DP1.geojson")


## Find order of counties in geojson files
## Each county is a separate feature
MAcounties <- c()
for(i in 1:length(MAmap$features)){
  MAcounties <- c(MAcounties, MAmap$features[[i]]$properties$County)
}

## Load formatted suicide data
## -1 eliminates first column [rows,columns]
bankdata <- read.csv(file="bankdata1.csv")
MAcounties <- sort(MAcounties)

bank_data1<-bankdata[which(bankdata$Region!="MA"&bankdata$Region!="United States"),]

## Set graph colors (special for colorblind people,turquoise)
cbbPalette <- c("#000000", "red", "yellow", "green", "blue",
                "turquoise", "lightblue", "deeppink")

## Create maxs and mins for googleCharts/Plot tab
xlim <- list(
  min = min(bank_data1$Year)-1,
  max = max(bank_data1$Year)+1
)
##Creat ylim for Business plot
ylim_bus <- list(
  min = min(bank_data1$Business_Filings_Total, na.rm=T)-5,
  
  ##+5 = max Avg monthly employment plus a little extra
  max = max(bank_data1$Business_Filings_Total, na.rm=T)+5
)

##Creat ylim for Personal plot
ylim_Non_bus <- list(
  min = min(bank_data1$Personal_Filings_Total, na.rm=T)-5,
  
  ##+5 = max Avg monthly employment plus a little extra
  max = max(bank_data1$Personal_Filings_Total, na.rm=T)+5
)

##Creat ylim for proportion of chapter plot
ylim_pro <- list(
  min = 0,
  
  max = 103
)


## Colors for business total legend
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                "#0072B2", "#D55E00", "#CC79A7", "#cccccc")
##paint_brush <- colorRampPalette(colors=c("#053061", "#CCCCCC", "#FF2C19"))
##map_colors <- c(paint_brush(n=20), "#999999")
paint_brush<-colorRampPalette(colors=c("white","red3"))
map_colors <- c(paint_brush(n=25), "#999999")
## (split into quintiles).  Cuts are quintiles of the total data
## Cuts based on entire dataset - not year specific - This keeps colors consistent for maps year-to-year

busmax.val <- round(max(bank_data1$Business_Filings_Total, na.rm=TRUE),0)
busmin.val <- round(min(bank_data1$Business_Filings_Total, na.rm=TRUE),0)

## Puts each county year in between the cuts (n colors, n+1 cuts)
## length.out will make that many cuts
# scuts <- seq(smin.val, smax.val, length.out = length(smap_colors))
buscuts <- quantile(bank_data1$Business_Filings_Total, probs = seq(0, 1, length.out = length(map_colors)), na.rm=TRUE)

## Construct break ranges for displaying in the legend
## Creates a data frame
## head = scuts takes everything except for the last one,
## tails = same thing opposite

buscolorRanges <- data.frame(
  from = head(buscuts, length(buscuts)-1),
  to = tail(buscuts, length(buscuts)-1)
)

## Colors for Personal total legend
nonbusmax.val <- round(max(bank_data1$Personal_Filings_Total, na.rm=TRUE),0)
nonbusmin.val <- round(min(bank_data1$Personal_Filings_Total, na.rm=TRUE),0)
nonbuscuts <- quantile(bank_data1$Personal_Filings_Total, probs = seq(0, 1, length.out = length(map_colors)), na.rm=TRUE)

## Construct break ranges for displaying in the legend
## Creates a data frame
## head = scuts takes everything except for the last one,
## tails = same thing opposite

nonbuscolorRanges <- data.frame(
  from = head(nonbuscuts, length(nonbuscuts)-1),
  to = tail(nonbuscuts, length(nonbuscuts)-1)
)

## Colors for proportion of chapter legend
procuts <- seq(0, 100, length.out = length(map_colors))

procolorRanges <- data.frame(
  from = head(procuts, length(procuts)-1),
  to = tail(procuts, length(procuts)-1)
)

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
  
  helpText(p(strong('Please select the years for which you are interested in viewing the number of business and personal bankruptcies. By selecting a year, you may also view the number of business and personal bankruptcies by chapter.'))),
  tags$br(),
  tags$ul(
    tags$br(),
    tags$li('Select one or multiple counties.'),
    tags$br(),
    tags$li('To look at the number of business and personal bankruptcies for a single year select a single year from the drop down menu.'),
    tags$br(),
    tags$li('Select the variable you are interested in seeing; total filings presents both buisness and personal bankruptcies.'),
    tags$br(),
    tags$li('To compare the number of business and personal bankruptcies to Massachusetts or US number please select "Compare to MA" or "Compare to US".'),
    tags$br(),
    tags$li('Sort the number of business and personal bankruptcies in ascending and descending order by clicking on the column or variable title.')
    
  )
)

## Same concept
plot_side_text <- conditionalPanel(
  condition="input.tabs == 'plot'",
  h4("How to use this app:"),
  p(strong('Please select the county for which you are interested in viewing the number of business and personal bankruptcies.')),
  tags$br(),
  tags$ul(
    tags$li("Once you have selected the counties for which you are interested in viewing the number of business and personal bankruptcies, select a Variable of Interest."),
    tags$br(),
    tags$li("Select your preferred display option- total bankruptcies or look at bankruptcies by specific chapter."),
    tags$br(),
    tags$li("To compare total business and personal bankruptcies within a specific chapter to Massachusetts or US totals please select 'Compare to MA' or 'Compare to US'."),
p(strong("Please note this can only be done when looking at a specific chapter of business or personal bankruptcies."))
    
  ))


map_side_text <- conditionalPanel(
  condition="input.tabs == 'map'",
  h4("How to use this app:"),
  helpText(p(strong("Please select a year, and click on 'Generate Map' to get started"))),
  tags$br(),
  tags$ul(
    tags$li('Select your preferred display option- total bankruptcies or look at bankruptcies by specific chapter.'),
    tags$br(),
    tags$li("To compare total business and personal bankruptcies within a specific chapter to Massachusetts or US totals please select 'Compare to MA' or 'Compare to US'."),
  p(strong("Please note this can only be done when looking at a specific chapter of business or personal bankruptcies."))
  ))

info_side_text <- conditionalPanel(
  condition="input.tabs == 'info'",
  h4("How to use this app:"),
  helpText(p(strong('This tab contains more detailed information regarding the variables of interest.'))))

about_main_text <- p(strong("The SEIGMA Bankruptcy App"), "displays business and personal bankruptcies by total number and percentage of bankruptcy by chapter for Massachusetts counties.",
                     p(strong("Click on different tabs to view the data in different formats.")),
                     tags$br(),
                     tags$ul(
                       tags$li(p(strong("Summary"), "shows the source data in table format.")),
                       tags$li(p(strong("Plot"), "displays business and personal bankruptcies by total number and total bankruptcies by chapter for each county.")),
                       tags$li(p(strong("Map"), "visually displays business and personal bankrupticies by total number and the total bankruptcies by chapter for each county.")),
                       tags$li(p(strong("More Info"), "defines business and personal bankruptcies, chapters of bankruptcies, and describes the percentage of bankruptcies including formulas and calculations."))
                     ))

plot_main_text <- p(strong("Variable Summary:"),
                    ## breaks between paragraphs
                    tags$br(),
                    strong("Annual Average Monthly Employment-"),
                    " Info about Avg. Monthly Employment.",
                    tags$br(),
                    strong("SEIGMA. Social and Economic Impacts of Gambling in Massachusetts, University of Massachusetts School of Public Health and Health Sciences. (2014). Report on the Social and Economic Impact of Gambling in Massachusetts SEIGMA Gambling study. Report to the Massachusetts Gaming Commission & the Massachusetts department of Public Health. Retrieved from:"), a("http://www.umass.edu/seigma/sites/default/files/March%202014%20SEIGMA%20Report_6-19_for%20website.pdf", align="center"))

font_size <- 14

Bus_plot_options <- googleLineChart("Bus_plot", width="100%", height="475px", options = list(
  
  ## set fonts
  fontName = "Source Sans Pro",
  fontSize = 14,
  
  ## set axis titles, ticks, fonts, and ranges
  hAxis = list(
    title = "Year",
    format = "####",
    ticks = seq(2012, 2017, 1),
    viewWindow = xlim,
    textStyle = list(
      fontSize = 14),
    titleTextStyle = list(
      fontSize = 16,
      bold = TRUE,
      italic = FALSE)
  ),
  vAxis = list(
    title = "Business Filings Total",
    viewWindow = ylim_bus,
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

NonBus_plot_options <- googleLineChart("NonBus_plot", width="100%", height="475px", options = list(
  ## set fonts
  fontName = "Source Sans Pro",
  fontSize = 14,
  
  ## set axis titles, ticks, fonts, and ranges
  hAxis = list(
    title = "Year",
    format = "####",
    ticks = seq(2012, 2017, 1),
    viewWindow = xlim,
    textStyle = list(
      fontSize = 14),
    titleTextStyle = list(
      fontSize = 16,
      bold = TRUE,
      italic = FALSE)
  ),
  vAxis = list(
    title = "Personal Filings Total",
    viewWindow = ylim_Non_bus,
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

Pro_Bus_plot_options <- googleLineChart("Pro_Bus_plot", width="100%", height="475px", options = list(
  
  ## set fonts
  fontName = "Source Sans Pro",
  fontSize = 14,
  
  ## set axis titles, ticks, fonts, and ranges
  hAxis = list(
    title = "Year",
    format = "####",
    ticks = seq(2012, 2017, 1),
    viewWindow = xlim,
    textStyle = list(
      fontSize = 14),
    titleTextStyle = list(
      fontSize = 16,
      bold = TRUE,
      italic = FALSE)
  ),
  vAxis = list(
    title = "Percentage of Chapter in Business Filings",
    viewWindow = ylim_pro,
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
Pro_NonBus_plot_options <- googleLineChart("Pro_NonBus_plot", width="100%", height="475px", options = list(
  
  ## set fonts
  fontName = "Source Sans Pro",
  fontSize = 14,
  
  ## set axis titles, ticks, fonts, and ranges
  hAxis = list(
    title = "Year",
    format = "####",
    ticks = seq(2012, 2017, 1),
    viewWindow = xlim,
    textStyle = list(
      fontSize = 14),
    titleTextStyle = list(
      fontSize = 16,
      bold = TRUE,
      italic = FALSE)
  ),
  vAxis = list(
    title = "Percentage of Chapter in Personal Filings",
    viewWindow = ylim_pro,
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