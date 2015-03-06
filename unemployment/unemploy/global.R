#######################################
## Title: Unemploy global.R          ##
## Author(s): Emily Ramos, Arvind    ##
##            Ramakrishnan, Jenna    ##
##            Kiridly, Steve Lauer   ##
## Date Created:  01/07/2015         ##
## Date Modified: 03/05/2015  ER     ##
#######################################

## First file run - Environment Setup
## load necessary libraries
require(dplyr)
require(sp)
require(maptools)
require(rgeos)
require(Hmisc)
# require(reshape2)
require(shiny)
require(googleCharts)
require(leaflet)
require(RJSONIO)
require(tidyr)

## load map data
#MA_map_county <- fromJSON("County_2010Census_DP1.geojson")
MA_map_muni <- fromJSON("Muni_2010Census_DP1.geojson")

## Load formatted unemp data
## -1 eliminates first column [rows,columns]
unemp_data <- read.csv(file="unempdata.csv")[,-1]

## Find order of counties in geojson files
## Each county is a separate feature
# MA_counties <- c()
# for(i in 1:length(MA_map_county$features)){
#   MA_counties <- c(MA_counties, MA_map_county$features[[i]]$properties$County)
# }

## Find order of municipals in geojson files
## Each municipal is a separate feature
for(i in 1:length(MA_map_muni$features)){
  MA_map_muni$features[[i]]$properties$NAMELSAD10 <- substr(MA_map_muni$features[[i]]$properties$NAMELSAD10, 1, nchar(MA_map_muni$features[[i]]$properties$NAMELSAD10)-5)
}

MA_municipals_map <- c()
for(i in 1:length(MA_map_muni$features)){
  MA_municipals_map <- c(MA_municipals_map, MA_map_muni$features[[i]]$properties$NAMELSAD10)
}

idx_leftovers <- which(!MA_municipals_map %in% unemp_data$Region)
leftover_munis <- MA_municipals_map[idx_leftovers]
for(i in 1:length(leftover_munis)){
  MA_map_muni$features[[idx_leftovers[i]]]$properties$NAMELSAD10 <-
    substr(leftover_munis[i], 1, nchar(leftover_munis[i])-5)
}

MA_municipals <- c()
for(i in 1:length(MA_map_muni$features)){
  MA_municipals <- c(MA_municipals, MA_map_muni$features[[i]]$properties$NAMELSAD10)
}
idx_leftovers2 <- which(!MA_municipals %in% unemp_data$Region)
leftover_munis_map <- MA_municipals[idx_leftovers2]
MA_municipals <- sort(MA_municipals[-idx_leftovers2])

## Set graph colors (special for colorblind people)
## In order: black, orange, light blue, green, yellow, dark blue, red, pink
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
                "#0072B2", "#D55E00", "#CC79A7")

## Create maxs and mins for googleCharts/Plot tab
xlim <- list(
  min = min(unemp_data$Year)-1,
  max = max(unemp_data$Year)+1
)
ylim <- list(
  min = 0,
  ##+5 = max rate plus a little extra
  max = max(unemp_data$Unemployment.Rate.Avg, na.rm=T)+5
)

#################################################################

## Colors for a single-year legend
spaint.brush <- colorRampPalette(colors=c("white", "red3"))
smap.colors <- c(spaint.brush(n=5), "#999999")

## For a single year data, we have a series of rates (split into quintiles).  Cuts are quintiles of the total data
## Cuts based on entire dataset - not year specific - This keeps colors consistent for maps year-to-year

smax.val <- max(unemp_data$Unemployment.Rate.Avg, na.rm=TRUE)
smin.val <- min(unemp_data$Unemployment.Rate.Avg, na.rm=TRUE)

## Puts each county year in between the cuts (n colors, n+1 cuts)
## length.out will make that many cuts
# scuts <- seq(smin.val, smax.val, length.out = length(smap.colors))
scuts <- quantile(unemp_data$Unemployment.Rate.Avg, probs = seq(0, 1, length.out = length(smap.colors)), na.rm=TRUE)

## Construct break ranges for displaying in the legend
## Creates a data frame
## head = scuts takes everything except for the last one,
## tails = same thing opposite

scolorRanges <- data.frame(
  from = head(scuts, length(scuts)-1),
  to = tail(scuts, length(scuts)-1)
)

## colors fade from one color to white to another color, with gray for NAs
## m-prefix = multiple years
mpaint.brush <- colorRampPalette(colors=c(cbbPalette[6], "white", cbbPalette[7]))
mmap.colors <- c(mpaint.brush(n=6), "#999999")

## find max and min (crude rates) values for each region
bound <- unemp_data %>%
  group_by(Region) %>%

  ##n.rm=FALSE = needed
  summarise(max.val = max(Unemployment.Rate.Avg, na.rm=FALSE),
            min.val = min(Unemployment.Rate.Avg, na.rm=FALSE))

## find the difference between each region's max and min
bound$diff <- abs(bound$max.val - bound$min.val)

## set the max and min value (for the legend) at 95% of the largest difference
mmax.val <- max(bound$diff)

#mmax.val <- quantile(bound$diff, .95, na.rm=TRUE)

mmin.val <- -1*mmax.val
mcuts <- seq(mmin.val, mmax.val, length.out = length(mmap.colors))

# Construct break ranges for displaying in the legend

mcolorRanges <- data.frame(
  from = head(mcuts, length(mcuts)-1),
  to = tail(mcuts, length(mcuts)-1)
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

  helpText(p(strong('Please select the years for which you are interested in viewing the annual average unemployment rate.'))),
  tags$br(),
  tags$ul(
    tags$br(),
    tags$li('Select one or multiple municipalities.'),
    tags$br(),
    tags$li('To compare the annual average unemployment rate to the Massachusetts or national rates, select the corresponding box.'),
    tags$br(),
    tags$li('Sort annual average umemployment rates in ascending and descending order by clicking the column or variable title.')

  )
)

## Same concept
plot_side_text <- conditionalPanel(
  condition="input.tabs == 'plot'",
  h4("How to use this app:"),
  p(strong('Please select a municipality to analyze annual average unemployment rates, do not slecet more than ten municipalities at a time.')),
  tags$br(),
  tags$ul(
    tags$li('For a given timespan, you can compare average annual unemployment rates to the national, state, and county rates.')
  ))


map_side_text <- conditionalPanel(
  condition="input.tabs == 'map'",
  h4("How to use this app:"),
  helpText(p(strong('Please select a yearly range and click on "Generate Map" to get started.'))),
  tags$br(),
  tags$ul(

    tags$li('Clicking on a municipality will display average annual unemployment status rates for the time period you selected.')
  ))

info_side_text <- conditionalPanel(
  condition="input.tabs == 'info'",
  h4("How to use this app:"),
  helpText(p(strong('This tab contains more detailed information regarding the variables of interest.'))))

about_main_text <- p(strong("The SEIGMA Annual Average Unemployment Rate App"), "displays the average annual unemployment rate for Massachusetts by municipality.",
                     p(strong("Click on different tabs to see the data in different forms.")),
                     tags$br(),
                     tags$ul(
                       tags$li(p(strong("Summary"), "shows the data in table format.")),
                       tags$li(p(strong("Plot"), "compares average annual unemployment rate for each municipality to county, state, and national rates.")),
                       tags$li(p(strong("Map"), "visually displays annual average unemployment rates by municipality")),
                       tags$li(p(strong("More Info"), "describes annual average unemployment rates, including formulas and calculations."))
                     ))

plot_main_text <- p(strong("Variable Summary:"),
                    ## breaks between paragraphs
                    tags$br(),
                    strong("Annual Avergage Unemployment Rate-"),
                    " Average annual unemployment rates account for workers who have lost their jobs and are looking for new ones.  This excludes people who are not looking for work.  The unemployment rate is produced by the Bureau of Labor Statistics, which uses state and national level information from the Current Population Survey.  Municipality unemployment rates were gathered form a secition of thr BLS and CPS called the Local Areas Unemployment Statistics Series.",
                    tags$br(),
                    strong("SEIGMA. Social and Economic Impacts of Gambling in Massachusetts, University of Massachusetts School of Public Health and Health Sciences. (2014). Report on the Social and Economic Impact of Gambling in Massachusetts SEIGMA Gambling study. Report to the Massachusetts Gaming Commission & the Massachusetts department of Public Health. Retrieved from:"), a("http://www.umass.edu/seigma/sites/default/files/March%202014%20SEIGMA%20Report_6-19_for%20website.pdf", align="center"))

font_size <- 14

plot_options <- googleColumnChart("plot", width="100%", height="475px",
                                  options = list(
                                    ## set fonts
                                    fontName = "Source Sans Pro",
                                    fontSize = font_size,
                                    title = "",
                                    ## set axis titles, ticks, fonts, and ranges
                                    hAxis = list(
                                      title = "",
                                      textStyle = list(
                                        fontSize = font_size),
                                      titleTextStyle = list(
                                        fontSize = font_size+2,
                                        bold = TRUE,
                                        italic = FALSE)
                                    ),
                                    vAxis = list(
                                      title = "% of Population",
                                      viewWindow = ylim,
                                      textStyle = list(
                                        fontSize = font_size),
                                      titleTextStyle = list(
                                        fontSize = font_size+2,
                                        bold = TRUE,
                                        italic = FALSE)
                                    ),

                                    ## set legend fonts
                                    legend = list(
                                      textStyle = list(
                                        fontSize=font_size),
                                      position = "right"),

                                    ## set chart area padding
                                    chartArea = list(
                                      top = 50, left = 100,
                                      height = "75%", width = "65%"
                                    ),

                                    ## set colors
                                    colors = cbbPalette[c(2:8)],

                                    ## set point size
                                    pointSize = 3,

                                    ## set tooltip font size
                                    ## Hover text font stuff
                                    tooltip = list(
                                      textStyle = list(
                                        fontSize = font_size
                                      )
                                    )
                                  ))
