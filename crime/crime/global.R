#######################################
## Title: Crime global.R             ##
## Author(s): Heather Weaver,        ##
##            Valerie Evans          ##
## Date Created:  06/28/2019         ##
## Date Modified: 09/19/2019 VE      ##
#######################################


##First file run - Environment Setup
##load necessary libraries
require(dplyr)
require(tidyr)
require(maptools)
require(Hmisc)
require(reshape2)
require(shiny)
require(googleCharts)
require(leaflet)
require(RJSONIO)
require(DT)

## load map data
MA_map_muni <- fromJSON("Muni_2010Census_DP1.geojson")

## Load crime data
crime_data <- read.csv("crime_data.csv")

## Find order of municipals in geojson files
## Each municipal is a separate feature
for(i in 1:length(MA_map_muni$features)){
  MA_map_muni$features[[i]]$properties$NAMELSAD10 <- substr(MA_map_muni$features[[i]]$properties$NAMELSAD10, 1, nchar(MA_map_muni$features[[i]]$properties$NAMELSAD10)-5)
}

MA_municipals_map <- c()
for(i in 1:length(MA_map_muni$features)){
  MA_municipals_map <- c(MA_municipals_map, MA_map_muni$features[[i]]$properties$NAMELSAD10)
}

idx_leftovers <- which(!MA_municipals_map %in% crime_data$Region)
leftover_munis <- MA_municipals_map[idx_leftovers]
for(i in 1:length(leftover_munis)){
  MA_map_muni$features[[idx_leftovers[i]]]$properties$NAMELSAD10 <- 
    substr(leftover_munis[i], 1, nchar(leftover_munis[i])-5)
}

MA_municipals <- c()
for(i in 1:length(MA_map_muni$features)){
  MA_municipals <- c(MA_municipals, MA_map_muni$features[[i]]$properties$NAMELSAD10)
}
idx_leftovers2 <- which(!MA_municipals %in% crime_data$Region)
leftover_munis_map <- MA_municipals[idx_leftovers2]
MA_municipals <- sort(MA_municipals[-idx_leftovers2])

## Set graph colors (special for colorblind people)
## In order: black, orange, light blue, green, yellow, dark blue, red, pink
cbbPalette <- c("black", "red", "orange", "yellow", "darkgreen", "blue", "purple", "deeppink")

## Create maxs and mins for googleCharts/Plot tab
xlim <- list(
  min = min(crime_data$Year)-1,
  max = max(crime_data$Year)+1
)

# ylim <- list(
#   min = 0,
#   max = max(crime_data$Property_crime_Rate, na.rm = T) + 50
# )

ylim_vcr <- list(
  min = 0,
  max = max(crime_data$Violent_crime_Rate, na.rm = T) + 50
)

ylim_mnm <- list(
  min = 0,
  max = max(crime_data$Murder_and_nonnegligent_manslaughter_Rate, na.rm = T)
)

ylim_rpr <- list(
  min = 0,
  max = max(crime_data$Rape_Rate, na.rm = T)
)

ylim_rbr <- list(
  min = 0,
  max = max(crime_data$Robbery_Rate, na.rm = T)
)

ylim_aar <- list(
  min = 0,
  max = max(crime_data$Aggravated_assault_Rate, na.rm = T)
)

ylim_pcr <- list(
  min = 0,
  max = max(crime_data$Property_crime_Rate, na.rm = T)
)

ylim_bgr <- list(
  min = 0,
  max = max(crime_data$Burglary_Rate, na.rm = T)
)

ylim_ltr <- list(
  min = 0,
  max = max(crime_data$Larceny_theft_Rate, na.rm=T)
)

ylim_mvr <- list(
  min = 0,
  max = max(crime_data$Motor_vehicle_theft_Rate, na.rm=T)
)

ylim_ars <- list(
  min = 0, 
  max = max(crime_data$Arson_Rate, na.rm = TRUE)
)

## Colors for a single-year legend
paint_brush <- colorRampPalette(colors=c("white", "darkblue"))
map_colors <- c(paint_brush(n=25), "#999999")

## For a single year data, we have a series of percentages (split into quintiles).  Cuts are quintiles of the total data percentages
## Cuts based on entire dataset - not year specific - This keeps colors consistent for maps year-to-year

# max_val <- max(crime_data$Property_crime_Rate, na.rm = TRUE) + 50
# min_val <- 0

## Puts each county year in between the cuts (n colors, n+1 cuts)
## length.out will make that many cuts

##Colors for violent crime rates map
violentmax.val <- max(crime_data$Violent_crime_Rate, na.rm=TRUE)
violentmin.val <- min(crime_data$Violent_crime_Rate, na.rm=TRUE)
violentcuts <- seq(0, violentmax.val, length.out = length(map_colors))

##Colors for murder and nonnegligent manslaughter crime rates map
murdermax.val <- max(crime_data$Murder_and_nonnegligent_manslaughter_Rate, na.rm=TRUE)
murdermin.val <- min(crime_data$Murder_and_nonnegligent_manslaughter_Rate, na.rm=TRUE)
murdercuts <- seq(0, murdermax.val, length.out = length(map_colors))

##Colors for rape crime rates map
rapemax.val <- max(crime_data$Rape_Rate, na.rm=TRUE)
rapemin.val <- min(crime_data$Rape_Rate, na.rm=TRUE)
rapecuts <- seq(0, rapemax.val, length.out = length(map_colors))

##Colors for robbery crime rates map
robberymax.val <- max(crime_data$Robbery_Rate, na.rm=TRUE)
robberymin.val <- min(crime_data$Robbery_Rate, na.rm=TRUE)
robberycuts <- seq(0, rapemax.val, length.out = length(map_colors))

##Colors for aggreavated assault crime rates map
assaultmax.val <- max(crime_data$Aggravated_assault_Rate, na.rm=TRUE)
assaultmin.val <- min(crime_data$Aggravated_assault_Rate, na.rm=TRUE)
assaultcuts <- seq(0, assaultmax.val, length.out = length(map_colors))

##Colors for property crime rates map
propertymax.val <- max(crime_data$Property_crime_Rate, na.rm=TRUE)
propertymin.val <- min(crime_data$Property_crime_Rate, na.rm=TRUE)
propertycuts <- seq(0, propertymax.val, length.out = length(map_colors))

##Colors for burglary rates map
burglarymax.val <- max(crime_data$Burglary_Rate, na.rm=TRUE)
burglarymin.val <- min(crime_data$Burglary_Rate, na.rm=TRUE)
burglarycuts <- seq(0, burglarymax.val, length.out = length(map_colors))

##Colors for larceny-theft crime rates map
larcenymax.val <- max(crime_data$Larceny_theft_Rate, na.rm=TRUE)
larcenymin.val <- min(crime_data$Larceny_theft_Rate, na.rm=TRUE)
larcenycuts <- seq(0, larcenymax.val, length.out = length(map_colors))

##Colors for motor vehicle theft crime rates map
motormax.val <- max(crime_data$Motor_vehicle_theft_Rate, na.rm=TRUE)
motormin.val <- min(crime_data$Motor_vehicle_theft_Rate, na.rm=TRUE)
motorcuts <- seq(0, motormax.val, length.out = length(map_colors))

##Colors for arson crime rates map
arsonmax.val <- max(crime_data$Arson_Rate, na.rm=TRUE)
arsonmin.val <- min(crime_data$Arson_Rate, na.rm=TRUE)
arsoncuts <- seq(0, arsonmax.val, length.out = length(map_colors))


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
  helpText(p(strong('Please select the year for which you are interested in viewing the crime rate estimates.'))),
  tags$ul(
    tags$li('Select one or multiple municipalities.'),
    tags$br(),
    tags$li('To compare crime rate estimates to the Massachusetts or United States estimate, select the corresponding check box.'),
    tags$br(),
    tags$li('Sort the crime estimates in ascending and descending order by clicking the column or variable title.')
  )
)

## Same concept
plot_side_text <- conditionalPanel(
  condition="input.tabs == 'plot'",
  h4("How to use this app:"),
  helpText(p(strong('Please select the municipality for which you are interested in viewing crime rates.'))),
  tags$ul(
    tags$li('Once you have selected the municipalities which you are interested in viewing, select a Crime Rate of Interest.')
  ))

map_side_text <- conditionalPanel(
  condition="input.tabs == 'map'",
  h4("How to use this app:"),
  helpText(p(strong("Please select a crime of interest and year, and click on 'Generate Map' to get started."))),
  tags$ul(
    tags$li('Clicking on a municipality will display the violent crime rate estimates for the year that you selected.')
  ))

info_side_text <- conditionalPanel(
  condition="input.tabs == 'info'",
  h4("How to use this app:"),
  helpText(p(strong('This tab contains more detailed information regarding the variable of interest.'))))

about_main_text <- p(strong("The SEIGMA Crime App"), "displays the yearly estimates of crime for municipalities in Massachusetts.",
                     p(strong("Click on different tabs to see the data in different formats.")),
                     tags$br(),
                     tags$ul(
                       tags$li(p(strong("Summary"), "shows the data in table format.")),
                       tags$li(p(strong("Plot"), "compares the crime rates for each municipality to the state, and national estimate.")),
                       tags$li(p(strong("Map"), "visually displays crime data by municipality.")),
                       tags$li(p(strong("More Info"), "describes crime data and crime rates."))
                     ))

plot_main_text <- p(strong("Variable Summary:"),
                    tags$br(),
                    strong("Annual Crime Rate"),
                    " - Annual crime rate per 100,000 population for reported crimes.",
                    tags$br(),
                    strong("Crime Rate = Count / Population * 100,000", align = "center"))

violent_plot_options <- googleLineChart("plot_violent", width = "100%", height = "475px", options = list(
  ## set fonts
  fontName = "Source Sans Pro",
  fontSize = 14,
  ## set axis titles, ticks, fonts, and ranges
  hAxis = list(
    title = "Year",
    format = "####",
    ticks = seq(2010, 2017, 2),
    viewWindow = xlim,
    textStyle = list(
      fontSize = 14),
    titleTextStyle = list(
      fontSize = 16,
      bold = TRUE,
      italic = FALSE)
  ),
  vAxis = list(
    title = "Crime Rate Per 100,000 Population",
    viewWindow = ylim_vcr,
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
      fontSize = 14)),
  ## set chart area padding
  chartArea = list(
    top = 50, left = 75,
    height = "75%", width = "60%"
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

murder_plot_options <- googleLineChart("plot_murder", width = "100%", height = "475px", options = list(
  ## set fonts
  fontName = "Source Sans Pro",
  fontSize = 14, 
  ## set axis titles, ticks, fonts, and ranges
  hAxis = list(
    title = "Year",
    format = "####",
    ticks = seq(2010, 2017, 2),
    viewWindow = xlim,
    textStyle = list(
      fontSize = 14),
    titleTextStyle = list(
      fontSize = 16,
      bold = TRUE,
      italic = FALSE)
  ),
  vAxis = list(
    title = "Crime Rate Per 100,000 Population",
    viewWindow = ylim_mnm,
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
      fontSize = 14)),
  ## set chart area padding
  chartArea = list(
    top = 50, left = 75,
    height = "75%", width = "60%"
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

rape_plot_options <- googleLineChart("plot_rape", width = "100%", height = "475px", options = list(
  ## set fonts
  fontName = "Source Sans Pro",
  fontSize = 14, 
  ## set axis titles, ticks, fonts, and ranges
  hAxis = list(
    title = "Year",
    format = "####",
    ticks = seq(2010, 2017, 2),
    viewWindow = xlim,
    textStyle = list(
      fontSize = 14),
    titleTextStyle = list(
      fontSize = 16,
      bold = TRUE,
      italic = FALSE)
  ),
  vAxis = list(
    title = "Crime Rate Per 100,000 Population",
    viewWindow = ylim_rpr,
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
      fontSize = 14)),
  ## set chart area padding
  chartArea = list(
    top = 50, left = 75,
    height = "75%", width = "60%"
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

robbery_plot_options <- googleLineChart("plot_robbery", width = "100%", height = "475px", options = list(
  ## set fonts
  fontName = "Source Sans Pro",
  fontSize = 14, 
  ## set axis titles, ticks, fonts, and ranges
  hAxis = list(
    title = "Year",
    format = "####",
    ticks = seq(2010, 2017, 2),
    viewWindow = xlim,
    textStyle = list(
      fontSize = 14),
    titleTextStyle = list(
      fontSize = 16,
      bold = TRUE,
      italic = FALSE)
  ),
  vAxis = list(
    title = "Crime Rate Per 100,000 Population",
    viewWindow = ylim_rbr,
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
      fontSize = 14)),
  ## set chart area padding
  chartArea = list(
    top = 50, left = 75,
    height = "75%", width = "60%"
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

assault_plot_options <- googleLineChart("plot_assault", width = "100%", height = "475px", options = list(
  ## set fonts
  fontName = "Source Sans Pro",
  fontSize = 14, 
  ## set axis titles, ticks, fonts, and ranges
  hAxis = list(
    title = "Year",
    format = "####",
    ticks = seq(2010, 2017, 2),
    viewWindow = xlim,
    textStyle = list(
      fontSize = 14),
    titleTextStyle = list(
      fontSize = 16,
      bold = TRUE,
      italic = FALSE)
  ),
  vAxis = list(
    title = "Crime Rate Per 100,000 Population",
    viewWindow = ylim_aar,
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
      fontSize = 14)),
  ## set chart area padding
  chartArea = list(
    top = 50, left = 75,
    height = "75%", width = "60%"
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

property_plot_options <- googleLineChart("plot_property", width = "100%", height = "475px", options = list(
  ## set fonts
  fontName = "Source Sans Pro",
  fontSize = 14, 
  ## set axis titles, ticks, fonts, and ranges
  hAxis = list(
    title = "Year",
    format = "####",
    ticks = seq(2010, 2017, 2),
    viewWindow = xlim,
    textStyle = list(
      fontSize = 14),
    titleTextStyle = list(
      fontSize = 16,
      bold = TRUE,
      italic = FALSE)
  ),
  vAxis = list(
    title = "Crime Rate Per 100,000 Population",
    viewWindow = ylim_pcr,
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
      fontSize = 14)),
  ## set chart area padding
  chartArea = list(
    top = 50, left = 75,
    height = "75%", width = "60%"
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

burglary_plot_options <- googleLineChart("plot_burglary", width = "100%", height = "475px", options = list(
  ## set fonts
  fontName = "Source Sans Pro",
  fontSize = 14, 
  ## set axis titles, ticks, fonts, and ranges
  hAxis = list(
    title = "Year",
    format = "####",
    ticks = seq(2010, 2017, 2),
    viewWindow = xlim,
    textStyle = list(
      fontSize = 14),
    titleTextStyle = list(
      fontSize = 16,
      bold = TRUE,
      italic = FALSE)
  ),
  vAxis = list(
    title = "Crime Rate Per 100,000 Population",
    viewWindow = ylim_bgr,
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
      fontSize = 14)),
  ## set chart area padding
  chartArea = list(
    top = 50, left = 75,
    height = "75%", width = "60%"
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

larceny_plot_options <- googleLineChart("plot_larceny", width = "100%", height = "475px", options = list(
  ## set fonts
  fontName = "Source Sans Pro",
  fontSize = 14, 
  ## set axis titles, ticks, fonts, and ranges
  hAxis = list(
    title = "Year",
    format = "####",
    ticks = seq(2010, 2017, 2),
    viewWindow = xlim,
    textStyle = list(
      fontSize = 14),
    titleTextStyle = list(
      fontSize = 16,
      bold = TRUE,
      italic = FALSE)
  ),
  vAxis = list(
    title = "Crime Rate Per 100,000 Population",
    viewWindow = ylim_ltr,
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
      fontSize = 14)),
  ## set chart area padding
  chartArea = list(
    top = 50, left = 75,
    height = "75%", width = "60%"
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

motor_plot_options <- googleLineChart("plot_motor", width = "100%", height = "475px", options = list(
  ## set fonts
  fontName = "Source Sans Pro",
  fontSize = 14, 
  ## set axis titles, ticks, fonts, and ranges
  hAxis = list(
    title = "Year",
    format = "####",
    ticks = seq(2010, 2017, 2),
    viewWindow = xlim,
    textStyle = list(
      fontSize = 14),
    titleTextStyle = list(
      fontSize = 16,
      bold = TRUE,
      italic = FALSE)
  ),
  vAxis = list(
    title = "Crime Rate Per 100,000 Population",
    viewWindow = ylim_mvr,
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
      fontSize = 14)),
  ## set chart area padding
  chartArea = list(
    top = 50, left = 75,
    height = "75%", width = "60%"
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

arson_plot_options <- googleLineChart("plot_arson", width = "100%", height = "475px", options = list(
  ## set fonts
  fontName = "Source Sans Pro",
  fontSize = 14, 
  ## set axis titles, ticks, fonts, and ranges
  hAxis = list(
    title = "Year",
    format = "####",
    ticks = seq(2010, 2017, 2),
    viewWindow = xlim,
    textStyle = list(
      fontSize = 14),
    titleTextStyle = list(
      fontSize = 16,
      bold = TRUE,
      italic = FALSE)
  ),
  vAxis = list(
    title = "Crime Rate Per 100,000 Population",
    viewWindow = ylim_ars,
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
      fontSize = 14)),
  ## set chart area padding
  chartArea = list(
    top = 50, left = 75,
    height = "75%", width = "60%"
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

