#######################################
## Title: Demographics global.R      ##
## Author(s): Xuelian Li, Arvind     ##
##            Ramakrishnan, Jenna    ##
##            Kiridly, Emily Ramos   ## 
## Date Created:  02/28/2015         ##
## Date Modified: 03/12/2015 XL      ##
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

## load map data
MA_map_county <- fromJSON("County_2010Census_DP1.geojson")
MA_map_muni <- fromJSON("Muni_2010Census_DP1.geojson")

## Load formatted marital status data
## -1 eliminates first column [rows,columns]
Dem_data <- read.csv(file="demodata.csv")
colnames(Dem_data)[11:28] <- c("Age_20-24_Pct","Margin_Error_20-24_Pct","Age_25-34_Pct", "Margin_Error_25-34_Pct",
                                "Age_35-44_Pct", "Margin_Error_35-44_Pct","Age_45-54_Pct", "Margin_Error_45-54_Pct",
                                "Age_55-59_Pct", "Margin_Error_55-59_Pct","Age_60-64_Pct", "Margin_Error_60-64_Pct",
                                "Age_65-74_Pct", "Margin_Error_65-74_Pct","Age_75-84_Pct", "Margin_Error_75-84_Pct",
                                "Age_85+Pct", "Margin_Error_85+_Pct")
## Find order of counties in geojson files
## Each county is a separate feature
MA_counties <- c()
for(i in 1:length(MA_map_county$features)){
   MA_counties <- c(MA_counties, MA_map_county$features[[i]]$properties$County)
 }

## Find order of municipals in geojson files
## Each municipal is a separate feature
for(i in 1:length(MA_map_muni$features)){
  MA_map_muni$features[[i]]$properties$NAMELSAD10 <- substr(MA_map_muni$features[[i]]$properties$NAMELSAD10, 1, nchar(MA_map_muni$features[[i]]$properties$NAMELSAD10)-5)
}

MA_municipals_map <- c()
for(i in 1:length(MA_map_muni$features)){
  MA_municipals_map <- c(MA_municipals_map, MA_map_muni$features[[i]]$properties$NAMELSAD10)
}

idx_leftovers <- which(!MA_municipals_map %in% Dem_data$Region)
leftover_munis <- MA_municipals_map[idx_leftovers]
for(i in 1:length(leftover_munis)){
 MA_map_muni$features[[idx_leftovers[i]]]$properties$NAMELSAD10 <- 
  substr(leftover_munis[i], 1, nchar(leftover_munis[i])-5)
}

MA_municipals <- c()
for(i in 1:length(MA_map_muni$features)){
 MA_municipals <- c(MA_municipals, MA_map_muni$features[[i]]$properties$NAMELSAD10)
}
idx_leftovers2 <- which(!MA_municipals %in% Dem_data$Region)
leftover_munis_map <- MA_municipals[idx_leftovers2]
MA_municipals <- sort(MA_municipals[-idx_leftovers2])

## Set graph colors (special for colorblind people)
## In order: black, orange, light blue, green, yellow, dark blue, red, pink
cbbPalette <- c("cyan","darkviolet","deeppink", "blue","green","yellow","darkorange","red",
                "darksalmon") 

## Create maxs and mins for googleCharts/Plot tab
ylim <- list(
  min = 0,
  max = 110
)

## Colors for a five-year legend "#00FF00" "white","#0072B2"#FF0000
paint_brush <- colorRampPalette(colors=c("lightgreen","yellow", "red"))
map_colors <- c(paint_brush(n=6), "#999999")


## Cuts based on Age range 
agemax.val <- 40
agemin.val <- 0
agecuts <- seq(agemin.val, agemax.val, length.out = length(map_colors))

## Puts each county year in between the cuts (n colors, n+1 cuts)
## length.out will make that many cuts
## Cuts based on Gender range
genmax.val <- max(c(max(Dem_data$Female_Pct, na.rm=TRUE),max(Dem_data$Male_Pct, na.rm=TRUE)))
genmin.val <- min(c(min(Dem_data$Female_Pct, na.rm=TRUE),min(Dem_data$Male_Pct, na.rm=TRUE)))
gencuts <- seq(genmin.val, genmax.val, length.out = length(map_colors))

racemax.val <- 100
racemin.val <- 0
racecuts <- seq(racemin.val, racemax.val, length.out = length(map_colors))

## Construct break ranges for displaying in the legend
## Creates a data frame
## head = scuts takes everything except for the last one, 
## tails = same thing opposite
agecolorRanges <- data.frame(
  from = head(agecuts, length(agecuts)-1),
  to = tail(agecuts, length(agecuts)-1)
)

gencolorRanges <- data.frame(
  from = head(gencuts, length(gencuts)-1),
  to = tail(gencuts, length(gencuts)-1)
)

racecolorRanges <- data.frame(
  from = head(racecuts, length(racecuts)-1),
  to = tail(racecuts, length(racecuts)-1)
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
  helpText(p(strong('Please select the five-year range for which you are interested in viewing the estimate of age, gender, race, or ethinicity.'))),
  tags$br(),
  tags$ul(
      tags$li('View the estimate for age, gender, race, or ethnicity by selecting the appropriate box below.'),
      tags$br(),
      tags$li('Select one or multiple municipalities.'),
      tags$br(),
      tags$li('For the five year ranges below, you can compare the demographic estimate in a municipality to national, state, and county estimate.'),
      tags$br(),
      tags$li(p(strong('The estimate of age, gender, race, or ethinicity can be sorted in ascending and descending order by clicking the column or variable.'))),
      tags$br(),
      tags$li('Please note that all statistics are five-year estimates.')
            
  )
  
  
  ## Creates horizontal line
  ##tags$hr()
)

## Same concept
plot_side_text <- conditionalPanel(
  condition="input.tabs == 'plot'",
  h4("How to use this app:"),
p(strong('Please select the municipality and five year range for which you are interested in viewing the estimate of age, gender, race, or ethinicity.')),
           tags$br(),
  tags$ul(
    tags$li('View the estimate for age, gender, race, or ethnicity by selecting the appropriate box below.'),
    tags$br(),
    tags$li('For a given five-year period, you can compare the municipality of your choice to the national, state, and county estimate for age, gender, race, and ethnicity.')
    ))
          
  tags$hr()


map_side_text <- conditionalPanel(
  condition="input.tabs == 'map'",
  h4("How to use this app:"),
  helpText(p(strong("Please select a five- year range and click on Generate Map to get started."))),
  tags$br(),
  tags$ul(
    tags$li('Select the variable of interest for age, gender, race, or ethnicity by selecting either an age range, gender, race, or ethnicity from the drop down list below.'),
    tags$br(),
    tags$li('Clicking on a municipality will display the variable of interest for the five-year range and gender that you selected.')
    ))

  tags$hr()

info_side_text <- conditionalPanel(
  condition="input.tabs == 'info'",
  h4("How to use this app:"),
  helpText(p(strong('This tab contains more detailed information regarding the variables of interest.')))
        # tags$br(),
 #tags$ul(
   #tags$li('Formulae.'),
  #tags$li('Calculations to derive the five-year estimates.')
      )#)
           
  #tags$hr()


about_main_text <- p(strong("The SEIGMA Demographics App"), "displays the demogrpahic composition in Massachusetts' municipalities over a five-year period.",
  p(strong("Click on different tabs to see the data in different formats.")),
    tags$br(),
    tags$ul(
      tags$li(p(strong("Summary"), "shows the data in table format.")),
      tags$li(p(strong("Plot"), "compares a municipality's demogrpahic estimate to county, state, and national estimate.")),
      tags$li(p(strong("Map"), "visually displays demographic estimates by municipality.")),
      tags$li(p(strong("More Info"), "describes demographic estimates including formulas and calculations."))
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

plot_options <- googleColumnChart("plot", width="100%", height="475px", options = list(
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
  position = "in"),
 
 ## set chart area padding
 chartArea = list(
   top = 50, left = 75,
  height = "75%", width = "70%"
 ),
 
 ## set colors
 colors = cbbPalette[c(1:9)],
 
 ## set point size
 pointSize = 3,
 
 ## set tooltip font size
 ## Hover text font stuff
 tooltip = list(
  textStyle = list(
   fontSize = font_size
  )
 ),
 
 ## stacked column
 isStacked= TRUE
))


 