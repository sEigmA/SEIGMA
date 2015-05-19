#######################################
## Title: Education global.R         ##
## Author(s): Emily Ramos, Arvind    ##
##            Ramakrishnan, Jenna    ##
##            Kiridly, Steve Lauer   ## 
## Date Created:  12/04/14           ##
## Date Modified: 02/24/15  ER       ##
#######################################

##First file run - Environment Setup
## load necessary libraries
require(dplyr)
require(sp)
require(maptools)
require(rgeos)
require(Hmisc)
require(reshape2)
require(shiny)
require(googleCharts)
require(leaflet)
require(RJSONIO)
# require(tidyr)

## load map data
#MA_map_county <- fromJSON("County_2010Census_DP1.geojson")
MA_map_muni <- fromJSON("Muni_2010Census_DP1.geojson")

## Load formatted marital status data
## -1 eliminates first column [rows,columns]
edu_data <- read.csv(file="edudata.csv")[,-1]

## Find order of counties in geojson files
## Each county is a separate feature
#MA_counties <- c()
#for(i in 1:length(MA_map_county$features)){
#  MA_counties <- c(MA_counties, MA_map_county$features[[i]]$properties$County)
#}

## Find order of municipals in geojson files
## Each municipal is a separate feature
for(i in 1:length(MA_map_muni$features)){
  MA_map_muni$features[[i]]$properties$NAMELSAD10 <- substr(MA_map_muni$features[[i]]$properties$NAMELSAD10, 1, nchar(MA_map_muni$features[[i]]$properties$NAMELSAD10)-5)
}

MA_municipals_map <- c()
for(i in 1:length(MA_map_muni$features)){
  MA_municipals_map <- c(MA_municipals_map, MA_map_muni$features[[i]]$properties$NAMELSAD10)
}

idx_leftovers <- which(!MA_municipals_map %in% edu_data$Region)
leftover_munis <- MA_municipals_map[idx_leftovers]
for(i in 1:length(leftover_munis)){
  MA_map_muni$features[[idx_leftovers[i]]]$properties$NAMELSAD10 <- 
    substr(leftover_munis[i], 1, nchar(leftover_munis[i])-5)
}

MA_municipals <- c()
for(i in 1:length(MA_map_muni$features)){
  MA_municipals <- c(MA_municipals, MA_map_muni$features[[i]]$properties$NAMELSAD10)
}
idx_leftovers2 <- which(!MA_municipals %in% edu_data$Region)
leftover_munis_map <- MA_municipals[idx_leftovers2]
MA_municipals <- sort(MA_municipals[-idx_leftovers2])

## Set graph colors (special for colorblind people)
## In order: black, orange, light blue, green, yellow, dark blue, red, pink
cbbPalette <- c("black", "red", "orange", "yellow", "darkgreen", 
                "blue", "purple", "deeppink")

## Create maxs and mins for googleCharts/Plot tab
ylim <- list(
  min = 0,
  max = 105
)

## Colors for a single-year legend
paint_brush <- colorRampPalette(colors=c("white", "darkblue"))
map_colors <- c(paint_brush(n=4), "#999999")

## For a single year data, we have a series of percentages (split into quintiles).  Cuts are quintiles of the total data percentages
## Cuts based on entire dataset - not year specific - This keeps colors consistent for maps year-to-year

max_val <- 100
min_val <- 0

## Puts each county year in between the cuts (n colors, n+1 cuts)
## length.out will make that many cuts
cuts <- seq(min_val, max_val, length.out = length(map_colors))

## Construct break ranges for displaying in the legend
## Creates a data frame
## head = scuts takes everything except for the last one, 
## tails = same thing opposite

colorRanges <- data.frame(
  from = head(cuts, length(cuts)-1),
  to = tail(cuts, length(cuts)-1)
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

  helpText(p(strong('Please select the five-year range for which you are interested in viewing the educational attainment estimate.'))),
  tags$br(),
  tags$ul(
    tags$br(),
    tags$li('Select one or multiple municipalities.'),
    tags$br(),
    tags$li('To compare educational attainment estimate to the Massachusetts or United States estimate, select the corresponding check box.'),
    tags$br(),
    tags$li('Sort the educational attainment estimate in ascending and descending order by clicking the column or variable title.'),
    tags$br(),
    tags$li('Please note that all statistics are five-year estimates.')

  )
)

## Same concept
plot_side_text <- conditionalPanel(
  condition="input.tabs == 'plot'",
  h4("How to use this app:"),
  p(strong('Please select the five- year range and municipality for which you are interested in viewing educational attainment.')),
  tags$br(),
  tags$ul(
    tags$li("For a given five-year period, you can compare a municipality's estimate of educational attainment to the county, state, and national estimate.")
  ))


map_side_text <- conditionalPanel(
  condition="input.tabs == 'map'",
  h4("How to use this app:"),
  helpText(p(strong("Please select a variable of interest and five- year range, and click on 'Generate Map' to get started."))),
  tags$br(),
  tags$ul(
    
    tags$li('Clicking on a municipality will display the educational attainment estimate for the five-year range that you selected.')
  ))

info_side_text <- conditionalPanel(
  condition="input.tabs == 'info'",
  h4("How to use this app:"),
  helpText(p(strong('This tab contains more detailed information regarding the variable of interest.'))))

about_main_text <- p(strong("The SEIGMA Educational Status App"), "displays the five-year estimate of educational attainment for municipalities in Massachusetts.",
                     p(strong("Click on different tabs to see the data in different formats.")),
                     tags$br(),
                     tags$ul(
                       tags$li(p(strong("Summary"), "shows the data in table format.")),
                       tags$li(p(strong("Plot"), "compares the educational attainment estimate for each municipality to the county, state, and national estimate.")),
                       tags$li(p(strong("Map"), "visually displays educational attainment estimate by municipality.")),
                       tags$li(p(strong("More Info"), "describes educational attainment, including formulas and calculations."))
                     ))

plot_main_text <- p(strong("Variable Summary:"),
                    ## breaks between paragraphs
                    tags$br(),
                    strong("Placeholder-Var Summary"),
                    " Placeholder Text", 
                    tags$br(),
                    strong("Place Holder"), 
                    " - Place-holder Text :",
                    tags$br(),
                    strong("Educational Status Equation Placeholder", align="center"))

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
