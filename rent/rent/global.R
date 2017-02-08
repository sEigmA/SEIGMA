#######################################
## Title: RENT   global.R            ##
## Author(s): JWB, BF                ## 
## Date Created:  12/01/2016         ##
#######################################

##First file run - Environment Setup
## load necessary libraries
#require(devtools)
#install_github("rstudio/shinyapps")
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
##require(rCharts)
require(tidyr)
require(plotly)

## load map data
#MA_map_county <- fromJSON("County_2010Census_DP1.geojson")
MA_map_muni <- fromJSON("Muni_2010Census_DP1.geojson")

## Load formatted Rent data
## -1 eliminates first column [rows,columns]
rent <- read.csv(file="rent.csv")

## Find order of counties in geojson files
## Each county is a separate feature
# MA_counties <- c()
# for(i in 1:length(MA_map_county$features)){
#   MA_counties <- c(MA_counties, MA_map_county$features[[i]]$properties$County)
# }

## Find order of municipals in geojson files
## Each municipal is a separate feature
for(i in 1:length(MA_map_muni$features)){
  MA_map_muni$features[[i]]$properties$NAMELSAD10 <- gsub(MA_map_muni$features[[i]]$properties$NAMELSAD10, pattern=c(" [Tt]own| [Cc]ity"), replacement = "")
}

MA_municipals_map <- c()
for(i in 1:length(MA_map_muni$features)){
  MA_municipals_map <- c(MA_municipals_map, MA_map_muni$features[[i]]$properties$NAMELSAD10)
}

idx_leftovers <- which(!MA_municipals_map %in% rent$Municipal)
leftover_munis <- MA_municipals_map[idx_leftovers]

#eliminate county subdivision lines (coasts, no land, etc)

# save_list <- list("features"=list())
# for(i in 1:length(MA_map_muni$features)){
#   if(
#   MA_map_muni$features[[i]]$properties$NAMELSAD10=="!County subdivisions not defined"
#   ){save_list$features[[i]]  <- MA_map_muni$features[[i]]}
# }

MA_municipals <- c()
for(i in 1:length(MA_map_muni$features)){
  MA_municipals <- c(MA_municipals, MA_map_muni$features[[i]]$properties$NAMELSAD10)
}
idx_leftovers2 <- which(!MA_municipals %in% rent$Municipal)
leftover_munis_map <- MA_municipals[idx_leftovers2]
leftover_munis_map <- leftover_munis_map[leftover_munis_map=="County subdivisions not defined"]
MA_municipals <- sort(MA_municipals[-which(MA_municipals=="County subdivisions not defined")])

MA_municipals<-unique(rent$Municipal[-c(grep(rent$Municipal, pattern = "County"),which(rent$Municipal %in% c("MA", "USA")))])

## Set graph colors (special for colorblind people)
## In order: black, orange, light blue, green, yellow, dark blue, red, pink
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                "#0072B2", "#D55E00", "#CC79A7", "#CC79A7", "#CC79A7")

## Create maxs and mins for googleCharts/Plot tab
ylim <- list(
  min = 0,
  max = max(rent$Median.Rent)
)

## Colors for a single-year legend
paint_brush <- colorRampPalette(colors=c("white", "#009E73"))
map_colors <- c(paint_brush(n=25), "#999999")

##Cuts are quintiles of the total data
## Cuts based on entire dataset - not year specific - This keeps colors consistent for maps year-to-year

max_val <- max(rent$Median.Rent, na.rm = TRUE)
min_val <- 0

## Puts each county year in between the cuts (n colors, n+1 cuts)
## length.out will make that many cuts
cuts <- seq(min_val, max_val, length.out = length(map_colors))

## Construct break ranges for displaying in the legend
## Creates a data frame
## head = scuts takes everything except for the last one, 
## tails = same thing opposite

#colorRanges <- data.frame(
 # from = head(cuts, length(cuts)-1),
#  to = tail(cuts, length(cuts)-1)
#)


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
  helpText(p(strong('Please select the five-year range for which you are interested in viewing median rent data.'))),
  tags$br(),
  tags$ul(
    #       tags$li('View rates by: male or female (or both by leaving this selection blank)'),
    #       tags$br(),
    tags$li('Select one or multiple municipalities.'),
    tags$br(),
    tags$li('To compare median data to the Massachusetts or US median, select the corresponding box.'),
    tags$br(),
    tags$li(p(strong('Please note that all statistics are 5-year estimates.'))),
    tags$br(),
    tags$li("For more information about how 5- year estimates are calculated, click on the 'More Info' tab.")
    
  )
)


## Creates horizontal line
##tags$hr()


## Same concept
plot_side_text <- conditionalPanel(
  condition="input.tabs == 'plot'",
  h4("How to use this app:"),
  p(strong('Please select the five- year range and municipality for which you are interested in viewing median contract rent')),
  tags$br(),
  tags$ul(
    tags$li("For a five-year period, you can compare a municipalitiy's median contract rent to the country, state, and national median.")
  ))

map_side_text <- conditionalPanel(
  condition="input.tabs == 'map'",
  h4("How to use this app:"),
  helpText(p(strong("Please select a five- year range, and click on 'Generate Map' to get started. "))),
  tags$br(),
  tags$ul(
    tags$li('Clicking on a municipality will display the median contract rent for the five-year range that you selected.')
  ))

info_side_text <- conditionalPanel(
  condition="input.tabs == 'info'",
  h4("How to use this app:"),
  helpText(p(strong('This tab contains more detailed information regarding the variables of interest.'))))



about_main_text <- p(strong("The SEIGMA Median Contract Rent App"), "displays median contract rent for municipalities in Massachusetts.",
                     tags$br(),
                     p(strong("Click on different tabs to view the data in different formats.")),
                     tags$ul(
                       tags$li(p(strong("Summary"), "shows the data in table format.")),
                       tags$li(p(strong("Plot"), "compares municipality's  median contract rent to county, state, and national medians.")),
                       tags$li(p(strong("Map"), "visually displays  median contract rent by municipality.")),
                       tags$li(p(strong("More Info"), "describes  median contract rent."))
                     )
)



plot_main_text <- p(strong("Variable Summary:"),
                    ## breaks between paragraphs
                    tags$br(),
                    p(strong("Median Contract Rent"),
                      " - Average median contract rent over a five year period for each municipality."))

font_size <- 14




##############################################################################



lplot<-googleLineChart("plot", width="100%", height="475px", options = list(
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
    title = "Median Rent (inflation-adjusted $)",
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
    position = "right"),
  
  ## set chart area padding
  chartArea = list(
    top = 50, left = 75,
    height = "75%", width = "70%"
  ),
  
  domain = list(
    role = c("domain", "data", "style")),
  
  ## set colors
  colors = cbbPalette[4:8],
  
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