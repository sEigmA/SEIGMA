#######################################
## Title: Marital global.R           ##
## Author(s): Emily Ramos, Arvind    ##
##            Ramakrishnan, Jenna    ##
##            Kiridly, Steve Lauer,` ##
##            Justin Baldwin         ## 
## Date Created:  10/22/2014         ##
## Date Modified: 01/31/2017 JB      ##
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

library(maps)
library(lubridate)
library(gbm)
library(markdown)



## load map data
#MA_map_county <- fromJSON("County_2010Census_DP1.geojson")
MA_map_muni <- fromJSON("Muni_2010Census_DP1.geojson")

## Load formatted marital status data
## -1 eliminates first column [rows,columns]
mar_data <- read.csv(file="BA002_02_marriagedata.csv")

names(mar_data)[10:12] <- gsub("Now_", "", names(mar_data)[10:12])

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

idx_leftovers <- which(!MA_municipals_map %in% mar_data$Region)
leftover_munis <- MA_municipals_map[idx_leftovers]
for(i in 1:length(leftover_munis)){
 MA_map_muni$features[[idx_leftovers[i]]]$properties$NAMELSAD10 <- 
  substr(leftover_munis[i], 1, nchar(leftover_munis[i])-5)
}

MA_municipals <- c()
for(i in 1:length(MA_map_muni$features)){
 MA_municipals <- c(MA_municipals, MA_map_muni$features[[i]]$properties$NAMELSAD10)
}
idx_leftovers2 <- which(!MA_municipals %in% mar_data$Region)
leftover_munis_map <- MA_municipals[idx_leftovers2]
MA_municipals <- sort(MA_municipals[-idx_leftovers2])

## Set graph colors (special for colorblind people)
## In order: black, orange, light blue, green, yellow, dark blue, red, pink
cbbPalette <- c("black", "orange", "blue", "green", "yellow", 
                "darkblue", "red", "pink")

## Create maxs and mins for googleCharts/Plot tab
ylim <- list(
  min = 0,
  max = 100
)

## Colors for a single-year legend
paint_brush <- colorRampPalette(colors=c("white", "deeppink"))
map_colors <- c(paint_brush(n=25), "#999999")

## For a single year data, we have a series of percentages (split into quintiles).  Cuts are quintiles of the total data percentages
## Cuts based on entire dataset - not year specific - This keeps colors consistent for maps year-to-year

max_val <- 100
min_val <- 0

marmax.val <- tapply(mar_data$Married_pct, mar_data$Gender, FUN=function(x){max(x, na.rm=T)})
marmin.val <- tapply(mar_data$Married_pct, mar_data$Gender, FUN=function(x){min(x, na.rm=T)})

marcuts <- quantile(mar_data$Married_pct, probs = seq(0, 1, length.out = length(map_colors)), na.rm=TRUE)

nevmax.val <- tapply(mar_data$Never_Married_pct, mar_data$Gender, FUN=function(x){max(x, na.rm=T)})
nevmin.val <- tapply(mar_data$Never_Married_pct, mar_data$Gender, FUN=function(x){min(x, na.rm=T)})

nevcuts <- quantile(mar_data$Never_Married_pct, probs = seq(0, 1, length.out = length(map_colors)), na.rm=TRUE)

sepmax.val <- tapply(mar_data$Separated_pct, mar_data$Gender, FUN=function(x){max(x, na.rm=T)})
sepmin.val <- tapply(mar_data$Separated_pct, mar_data$Gender, FUN=function(x){min(x, na.rm=T)})
##sepmax.val <-20

sepcuts <- quantile(mar_data$Separated_pct, probs = seq(0, 1, length.out = length(map_colors)), na.rm=TRUE)

widmax.val <- tapply(mar_data$Widowed_pct, mar_data$Gender, FUN=function(x){max(x, na.rm=T)})
widmin.val <- tapply(mar_data$Widowed_pct, mar_data$Gender, FUN=function(x){min(x, na.rm=T)})

widcuts <- quantile(mar_data$Widowed_pct, probs = seq(0, 1, length.out = length(map_colors)), na.rm=TRUE)

divmax.val <- tapply(mar_data$Divorced_pct, mar_data$Gender, FUN=function(x){max(x, na.rm=T)})
divmin.val <- tapply(mar_data$Divorced_pct, mar_data$Gender, FUN=function(x){max(x, na.rm=T)})

divcuts <- quantile(mar_data$Divorced_pct, probs = seq(0, 1, length.out = length(map_colors)), na.rm=TRUE)

## Puts each county year in between the cuts (n colors, n+1 cuts)
## length.out will make that many cuts
cuts <- seq(min_val, max_val, length.out = length(map_colors))

## Construct break ranges for displaying in the legend
## Creates a data frame
## head = scuts takes everything except for the last one, 
## tails = same thing opposite

marcolorRanges <- data.frame(
  from = head(marcuts, length(marcuts)-1),
  to = tail(marcuts, length(marcuts)-1)
)

nevcolorRanges <- data.frame(
  from = head(nevcuts, length(nevcuts)-1),
  to = tail(nevcuts, length(nevcuts)-1)
)

sepcolorRanges <- data.frame(
  from = head(sepcuts, length(sepcuts)-1),
  to = tail(sepcuts, length(sepcuts)-1)
)

widcolorRanges <- data.frame(
  from = head(widcuts, length(widcuts)-1),
  to = tail(widcuts, length(widcuts)-1)
)

divcolorRanges <- data.frame(
  from = head(divcuts, length(divcuts)-1),
  to = tail(divcuts, length(divcuts)-1)
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
  helpText(p(strong('Please select the five-year range for which you are interested in seeing marital status estimates.'))),
  tags$br(),
  tags$ul(
      tags$li('View rates by selecting male or female. To veiw both leave this selection blank.'),
      tags$br(),
      tags$li('Select one or multiple municipalities.'),
      tags$br(),
      tags$li('To compare the data to the Massachusetts average or US average select the corresponding check box.'),
      tags$br(),
      tags$li(p(strong('Please note that all statistics are five-year estimates.')))
            
  )
  
  
  ## Creates horizontal line
  ##tags$hr()
)

## Same concept
plot_side_text <- conditionalPanel(
  condition="input.tabs == 'plot'",
  h4("How to use this app:"),
p(strong('Please select the five- year range and municipality for which you are interested in viewing marital status.')),
           tags$br(),
  tags$ul(
    tags$li('For a given five-year period, you can compare the municipality of your choice to the national, state, and county averages for females and males.')
    ))
          
  tags$hr()


map_side_text <- conditionalPanel(
  condition="input.tabs == 'map'",
  h4("How to use this app:"),
  helpText(p(strong("Please select a variable of interest, a five-year range, a gender, and click on 'Generate Map' to get started."))),
  tags$br(),
  tags$ul(
    tags$li('Clicking on a municipality will display the variable of interest for the five-year range and gender that you selected.')
    ))

  tags$hr()

info_side_text <- conditionalPanel(
  condition="input.tabs == 'info'",
  h4("How to use this app:"),
  helpText(p(strong('This tab contains more detailed information regarding the variables of interest.'))),
         tags$br(),
 tags$ul(
   tags$li('Formulae.'),
  tags$li('Calculations to derive the five-year averages.')
      ))
           
  tags$hr()


about_main_text <- p(strong("The SEIGMA Marital Status App"), "Displays the five-year estimates of marital status for Massachusetts by municipality.",
  p(strong("Click on different tabs to see the data in different formats.")),
    tags$br(),
    tags$ul(
      tags$li(p(strong("Summary"), "shows the data in table format.")),
      tags$li(p(strong("Plot"), "compares a municipality's marital status estimate to county, state, and national estimates.")),
      tags$li(p(strong("Map"), "visually displays marital status estimates by municipality.")),
      tags$li(p(strong("More Info"), "describes marital status including formulas and calculations."))
)
)


plot_main_text <- p(strong("Variable Summary:"),
                    ## breaks between paragraphs
                    tags$br(),
                    strong("Suicides"),
                    " - Number of suicides for a specified region in a specific year. Due to confidentiality constraints, sub-national death counts and rates are suppressed when the number of deaths is less than 10.", 
                    tags$br(),
                    strong("Crude Rate"), 
                    " - Crude rates are expressed as the number of suicides, per 100,000 persons, reported each calendar year for the region you select. Rates are considered 'unreliable' when the death count is less than 20 and thus are not displayed. This is calculated by:",
                    tags$br(),
                    strong("Crude Rate = Count / Population * 100,000", align="center"))

font_size <- 14

US_plot_options <- googleColumnChart("plot_US", width="100%", height="475px", options = list(
 ## set fonts
 fontName = "Source Sans Pro",
 fontSize = font_size,
 title = "Marital Status Statistics for the United States",
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
  position = "in"),
 
 ## set chart area padding
 chartArea = list(
   top = 50, left = 75,
  height = "75%", width = "70%"
 ),
 
 ## set colors
 colors = cbbPalette[c(8,3)],
 
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

MA_plot_options <- googleColumnChart("plot_MA", width="100%", height="475px", options = list(
 ## set fonts
 fontName = "Source Sans Pro",
 fontSize = font_size,
 title = "Marital Status Statistics for Massachusetts",
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
  position = "in"),
 
 ## set chart area padding
 chartArea = list(
  top = 50, left = 100,
  height = "75%", width = "70%"
 ),
 
 ## set colors
 colors = cbbPalette[c(8,3)],
 
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

county_plot_options <- googleColumnChart("plot_county", width="100%", height="475px", options = list(
  ## set fonts
  fontName = "Source Sans Pro",
  fontSize = font_size,
  
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
    position = "in"),
  
  ## set chart area padding
  chartArea = list(
    top = 50, left = 75,
    height = "75%", width = "70%"
  ),
  
  ## set colors
  colors = cbbPalette[c(8,3)],
  
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

muni_plot_options <- googleColumnChart("plot_muni", width="100%", height="475px", options = list(
  ## set fonts
  fontName = "Source Sans Pro",
  fontSize = font_size,
  
  ## set axis titles, ticks, fonts, and ranges
  hAxis = list(
    title = "",
    textStyle = list(
      fontSize = font_size),
    titleTextStyle = list(
      fontSize = font_size,
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
    position = "in"),
  
  ## set chart area padding
  chartArea = list(
    top = 50, left = 100,
    height = "75%", width = "70%"
  ),
  
  ## set colors
  colors = cbbPalette[c(8,3)],
  
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



########################
#variable plot titles

#plot.titles <- data.frame("plotvar"=c("Never_Married_pct", "Married_pct", "Separated_pct", "Widowed_pct", "Divorced_pct"))