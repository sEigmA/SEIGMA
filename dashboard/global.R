################################
## Title: Dashboard global    ##
## Author: Zhenning Kang      ##
## Date Created:  09/27/2017  ##
## Date Modified: 10/01/2017  ##
################################

##### load in libraries #####
library(shiny)
library(shinydashboard)
library(dplyr)
library(maptools)
library(Hmisc)
library(reshape2)
library(shiny)
library(googleCharts)
library(leaflet)
library(RJSONIO)

### load employment app data
emp_data <- read.csv(file="empdata3.csv")

### load rent app data
rent <- read.csv(file="rent.csv")

### load edu app data
edu_data <- read.csv(file="edudata.csv")[,-1]

### load formatted marital status data
mar_data <- read.csv(file="BA002_02_marriagedata.csv")
names(mar_data)[10:12] <- gsub("Now_", "", names(mar_data)[10:12])

### load demo app data
Dem_data <- read.csv(file="demodata.csv")
colnames(Dem_data)[12:37] <- c("Age_under_5_Pct","Margin_Error_under_5_Pct","Age_5-9_Pct", "Margin_Error_5-9_Pct",
                               "Age_10-14_Pct","Margin_Error_10-14_Pct","Age_15-19_Pct", "Margin_Error_15-19_Pct",
                               "Age_20-24_Pct","Margin_Error_20-24_Pct","Age_25-34_Pct", "Margin_Error_25-34_Pct",
                               "Age_35-44_Pct", "Margin_Error_35-44_Pct","Age_45-54_Pct", "Margin_Error_45-54_Pct",
                               "Age_55-59_Pct", "Margin_Error_55-59_Pct","Age_60-64_Pct", "Margin_Error_60-64_Pct",
                               "Age_65-74_Pct", "Margin_Error_65-74_Pct","Age_75-84_Pct", "Margin_Error_75-84_Pct",
                               "Age_85+Pct", "Margin_Error_85+_Pct")
colnames(Dem_data)[54:59] <- c("Age_under_20_Pct_plot","Age_20-34_Pct_plot","Age_35-54_Pct_plot", "Age_55-64_Pct_plot",
                               "Age_65-74_Pct_plot", "Age_75+Pct_plot")

### load map data
MA_map_county <- fromJSON("County_2010Census_DP1.geojson")
MA_map_muni <- fromJSON("Muni_2010Census_DP1.geojson")

## Find order of counties in geojson files
## Each county is a separate feature
MA_counties <- c()
for(i in 1:length(MA_map_county$features)){
    MA_counties <- c(MA_counties, MA_map_county$features[[i]]$properties$County)
}

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
    max = max(rent$Median.Rent.2015.Dollar)
)

## Colors for a single-year legend
paint_brush <- colorRampPalette(colors=c("white", "#009E73"))
map_colors <- c(paint_brush(n=25), "#999999")

##Cuts are quintiles of the total data
## Cuts based on entire dataset - not year specific - This keeps colors consistent for maps year-to-year

max_val <- max(rent$Median.Rent.2015.Dollar, na.rm = TRUE)
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
    p(strong('Please select the five- year range and municipality for which you are interested in viewing inflation-adjusted (2015 $) median contract rent')),
    tags$br(),
    tags$ul(
        tags$li("For a five-year period, you can compare a municipalitiy's inflation-adjusted (2015 $) median contract rent to the country, state, and national median.")
    ))

map_side_text <- conditionalPanel(
    condition="input.tabs == 'map'",
    h4("How to use this app:"),
    helpText(p(strong("Please select a five- year range, and click on 'Generate Map' to get started. "))),
    tags$br(),
    tags$ul(
        tags$li('Clicking on a municipality will display the inflation-adjusted (2015 $) median contract rent for the five-year range that you selected.')
    ))

info_side_text <- conditionalPanel(
    condition="input.tabs == 'info'",
    h4("How to use this app:"),
    helpText(p(strong('This tab contains more detailed information regarding the variables of interest.'))))



about_main_text <- p(strong("The SEIGMA Median Contract Rent App"), "displays inflation-adjusted (2015 $) median contract rent for municipalities in Massachusetts.",
                     tags$br(),
                     p(strong("Click on different tabs to view the data in different formats.")),
                     tags$ul(
                         tags$li(p(strong("Summary"), "shows the data in table format.")),
                         tags$li(p(strong("Plot"), "compares municipality's  inflation-adjusted (2015 $) median contract rent to county, state, and national medians.")),
                         tags$li(p(strong("Map"), "visually displays  inflation-adjusted median contract rent by municipality.")),
                         tags$li(p(strong("More Info"), "describes  inflation-adjusted median contract rent."))
                     )
)



plot_main_text <- p(strong("Variable Summary:"),
                    ## breaks between paragraphs
                    tags$br(),
                    p(strong("Median Contract Rent 2015$"),
                      " - Average inflation-adjusted (2015 $) median contract rent over a five year period for each municipality."))

font_size <- 14




##############################################################################



lplot<-googleLineChart("plot", width="100%", height="475px", options = list(
    ## set fonts
    fontName = "Source Sans Pro",
    fontSize = font_size,
    title = "",
    ## set axis titles, ticks, fonts, and ranges
    hAxis = list(
        title = "Median Rent (2015 $)",
        textStyle = list(
            fontSize = font_size),
        titleTextStyle = list(
            fontSize = font_size+2,
            bold = TRUE,
            italic = FALSE)
    ),
    vAxis = list(
        title = "Median Rent (2015 $)",
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
