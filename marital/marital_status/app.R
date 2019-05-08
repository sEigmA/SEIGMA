#######################################
## Title: Marital App.R              ##
## Author(s): Valerie Evans          ##
## Date Created:  11/02/2017         ##
## Date Modified: 05/08/2019 VE      ##
## Map Updated:   02/16/2018 ZK      ##
#######################################

##### GLOBAL #####
## Load necessary libraries
require(dplyr)
require(geojsonio)
require(googleCharts)
require(Hmisc)
require(leaflet)
require(maptools)
require(reshape2)
require(rgeos)
require(RJSONIO)
require(shiny)
require(sp)

## Load map data
MA_map_muni <- geojson_read("Muni_2010Census_DP1.geojson")

## Load formatted marital status data
mar_data <- read.csv(file="maritalstatus.csv")
names(mar_data)[10:12] <- gsub("Now_", "", names(mar_data)[10:12])

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
MA_municipals <- unique(mar_data$Region[-c(grep(mar_data$Region, pattern = "County"),which(mar_data$Region %in% c("MA", "USA")))])

## Set graph colors (special for colorblind people); in order: black, orange, light blue, green, yellow, dark blue, red, pink
cbbPalette <- c("black", "red", "orange", "yellow", "green", "blue", "red", "pink")
pal <- colorNumeric("viridis", NULL)

## Create maxs and mins for googleCharts/Plot tab
ylim <- list(
  min = 0,
  max = 100
)

## Colors for a single-year legend
paint_brush <- colorRampPalette(colors=c("white", "deeppink"))
map_colors <- c(paint_brush(n=25), "#999999")

## For a single year data, we have a series of percentages (split into quintiles); cuts are quintiles of the total data percentages
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

#asking for 26 cuts in the range of separated (0-13%) returns a ton of small values that get rounded to 0.0
#non-unique values for cuts are problematic, so we shrink the scale to 10 colors.
sepcuts <- quantile(mar_data$Separated_pct, probs = seq(0, 1, length.out = 10), na.rm=TRUE)

widmax.val <- tapply(mar_data$Widowed_pct, mar_data$Gender, FUN=function(x){max(x, na.rm=T)})
widmin.val <- tapply(mar_data$Widowed_pct, mar_data$Gender, FUN=function(x){min(x, na.rm=T)})

widcuts <- quantile(mar_data$Widowed_pct, probs = seq(0, 1, length.out = length(map_colors)), na.rm=TRUE)

divmax.val <- tapply(mar_data$Divorced_pct, mar_data$Gender, FUN=function(x){max(x, na.rm=T)})
divmin.val <- tapply(mar_data$Divorced_pct, mar_data$Gender, FUN=function(x){max(x, na.rm=T)})

divcuts <- quantile(mar_data$Divorced_pct, probs = seq(0, 1, length.out = length(map_colors)), na.rm=TRUE)

## Puts each county year in between the cuts (n colors, n+1 cuts); length.out will make that many cuts
cuts <- seq(min_val, max_val, length.out = length(map_colors))

## Construct break ranges for displaying in the legend; head = scuts takes everything except for the last one, tails = same thing opposite
## Creates a data frame
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
  ## h3 created 3rd largest header, h4 created 4th largest header
  h3("SEIGMA: Marital Status Shiny App"),
  h4("How to use this app:"),
  ## Creates text
  helpText(p(strong('Please select the five-year range for which you are interested in seeing marital status estimates.'))),
  tags$br(),
  tags$ul(
    tags$li('View rates by selecting male or female. To view both leave this selection blank.'),
    tags$br(),
    tags$li('Select one or multiple municipalities.'),
    tags$br(),
    tags$li('To compare the data to the Massachusetts average or US average select the corresponding check box.'),
    p('Please note that all statistics are five-year estimates.')
  )
)

## Same concept
plot_side_text <- conditionalPanel(
  condition="input.tabs == 'plot'",
  h3("SEIGMA: Marital Status Shiny App"),
  h4("How to use this app:"),
  helpText(p(strong('Please select the variable and municipality for which you are interested in viewing marital status.'))),
  tags$br(),
  tags$ul(
    tags$li('For a given five-year period, you can compare the municipality of your choice to the state and national averages for females and males.')
  ))

tags$hr()

map_side_text <- conditionalPanel(
  condition="input.tabs == 'map'",
  h3("SEIGMA: Marital Status Shiny App"),
  h4("How to use this app:"),
  helpText(p(strong("Please select a variable of interest, a five-year range, and a gender. Click on 'Generate Map' in the main page."))),
  tags$br(),
  tags$ul(
    tags$li('Clicking on a municipality will display the variable of interest for the five-year range and gender that you selected.')
  ))

tags$hr()

info_side_text <- conditionalPanel(
  condition="input.tabs == 'info'",
  h3("SEIGMA: Marital Status Shiny App"),
  h4("How to use this app:"),
  helpText(p(strong('This tab contains more detailed information regarding the variables of interest.'))),
  tags$br(),
  tags$ul(
    tags$li('Formulae.'),
    tags$li('Calculations to derive the five-year estimates')
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

font_size <- 14

US_plot_options <- googleColumnChart("plot_US", width="100%", height="475px", options = list(
  ## Set fonts
  fontName = "Source Sans Pro",
  fontSize = font_size,
  title = "Marital Status Statistics for the United States",
  ## Set axis titles, ticks, fonts, and ranges
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
  
  ## Set legend fonts
  legend = list(
    position = "in"),
  
  ## Set chart area padding
  chartArea = list(
    top = 50, left = 75,
    height = "75%", width = "70%"
  ),
  
  ## Set colors
  colors = cbbPalette[c(8,3)],
  
  ## Set point size
  pointSize = 3,
  
  ## Set tooltip font size
  ## Hover text font stuff
  tooltip = list(
    textStyle = list(
      fontSize = font_size
    )
  )
))

MA_plot_options <- googleColumnChart("plot_MA", width="100%", height="475px", options = list(
  ## Set fonts
  fontName = "Source Sans Pro",
  fontSize = font_size,
  title = "Marital Status Statistics for Massachusetts",
  ## Set axis titles, ticks, fonts, and ranges
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
  
  ## Set legend fonts
  legend = list(
    textStyle = list(
      fontSize=font_size),
    position = "in"),
  
  ## Set chart area padding
  chartArea = list(
    top = 50, left = 100,
    height = "75%", width = "70%"
  ),
  
  ## Set colors
  colors = cbbPalette[c(8,3)],
  
  ## Set point size
  pointSize = 3,
  
  ## Set tooltip font size
  ## Hover text font stuff
  tooltip = list(
    textStyle = list(
      fontSize = font_size
    )
  )
))

county_plot_options <- googleColumnChart("plot_county", width="100%", height="475px", options = list(
  ## Set fonts
  fontName = "Source Sans Pro",
  fontSize = font_size,
  
  ## Set axis titles, ticks, fonts, and ranges
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
  
  ## Set legend fonts
  legend = list(
    textStyle = list(
      fontSize=font_size),
    position = "in"),
  
  ## Set chart area padding
  chartArea = list(
    top = 50, left = 75,
    height = "75%", width = "70%"
  ),
  
  ## Set colors
  colors = cbbPalette[c(8,3)],
  
  ## Set point size
  pointSize = 3,
  
  ## Set tooltip font size
  ## Hover text font stuff
  tooltip = list(
    textStyle = list(
      fontSize = font_size
    )
  )
))

muni_plot_options <- googleColumnChart("plot_muni", width="100%", height="475px", options = list(
  ## Set fonts
  fontName = "Source Sans Pro",
  fontSize = font_size,
  
  ## Set axis titles, ticks, fonts, and ranges
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
  
  ## Set legend fonts
  legend = list(
    textStyle = list(
      fontSize=font_size),
    position = "in"),
  
  ## Set chart area padding
  chartArea = list(
    top = 50, left = 100,
    height = "75%", width = "70%"
  ),
  
  ## Set colors
  colors = cbbPalette[c(8,3)],
  
  ## Set point size
  pointSize = 3,
  
  ## Set tooltip font size
  ## Hover text font stuff
  tooltip = list(
    textStyle = list(
      fontSize = font_size
    )
  )
))

## Add map casino icons
#MAcasinos <- data.frame("Name"=c("Wynn Boston Harbor", "Plainridge Park Casino", "MGM Springfield"),
#                        "Lat"=c(42.394964,42.0330381,42.1006063),
#                        "Lon"=c(-71.066760,-71.3039442,-72.5870506))
#star <- makeIcon(iconUrl = "www/star.png",
#                  iconWidth = 30, iconHeight = 30,
#                  iconAnchorX = 15, iconAnchorY = 15)
#casinosOPEN <- data.frame("Name"=c("Mohegan Sun", "Foxwoods"),
#                          "Lat"=c(41.491549,41.473775),
#                          "Lon"=c(-72.091842,-71.960177))
#gc2 <- makeIcon(iconUrl = "www/greencircle2.gif",
#                 iconWidth = 20, iconHeight = 20,
#                 iconAnchorX = 10, iconAnchorY = 10)
#casinosCLOSED <- data.frame("Name"=c("Tiverton", "River Casino"),
#                            "Lat"=c(41.660301,42.824163),
#                            "Lon"=c(-71.155845,-73.937884))
#gc1 <- makeIcon(iconUrl = "www/greencircle1.png",
#                 iconWidth = 20, iconHeight = 20,
#                 iconAnchorX = 10, iconAnchorY = 10)


##### UI #####
ui <- shinyUI(fluidPage(
  ## embed the google analytics script in the app
  tags$head(includeScript("google-analytics.js")),
  ## HTML to create generate map button
  gen_map_button, 
  ## this starts the googleCharts engine
  googleChartsInit(),
  ## blank title, but put in a special title for window tab
  titlePanel("", windowTitle = "SEIGMA: Marital Status Shiny App"),
  
  ## Create sidebar
  sidebarLayout(
    sidebarPanel(width=4,
                 ## Conditional panel means if the condition is met show all text below otherwise Don't!
                 summary_side_text, 
                 
                 plot_side_text, 
                 
                 map_side_text, 
                 
                 info_side_text,
                 
                 ## In summary, allow for year, gender, and municipality selection
                 conditionalPanel(
                   condition="input.tabs == 'summary'",
                   selectInput("sum_year", "Select Five-Year Range",
                               choices = list("2006-2010" = "2006-2010", "2007-2011" = "2007-2011", "2008-2012" = "2008-2012",
                                              "2009-2013" = "2009-2013", "2010-2014" = "2010-2014", "2011-2015" = "2011-2015", 
                                              "2012-2016" = "2012-2016", "2013-2017" = "2013-2017"), 
                               multiple = TRUE),
                   selectInput("sum_gender", "Select Gender",
                               choices = list("Female" = "Female", "Male" = "Male"), multiple=TRUE),
                   selectInput("sum_muni", "Select Municipality", 
                               choices = sort(MA_municipals), multiple=TRUE),
                   ## False at the end means it starts off unchecked
                   checkboxInput("MA_mean", "Compare to MA Average", FALSE),
                   checkboxInput("US_mean", "Compare to US Average", FALSE)
                 ),
                 
                 ## In plot, allow for variable and municipality selection
                 conditionalPanel(
                   condition="input.tabs == 'plot'",
                   selectInput("plotvar", "Select Variable of Interest",
                               choices = list("Never Married" = "Never_Married_pct", 
                                              "Married" = "Married_pct",
                                              "Separated" = "Separated_pct",
                                              "Widowed" = "Widowed_pct",
                                              "Divorced" = "Divorced_pct")),
                   selectInput("plot_muni", "Select Municipality", 
                               choices = sort(MA_municipals), multiple = TRUE, selected = "MA"),
                   checkboxInput("plotMA_mean", "Compare to MA Average", TRUE),
                   checkboxInput("plotUS_mean", "Compare to US Average", FALSE),
                   selectInput("plotcombine", "Show Plots by Gender", choices = list("Separated" = "Separate", 
                                                                                     "Together" = "Together"))
                 ),
                 
                 ## In map, allow for year, gender, and variable selection
                 conditionalPanel(
                   condition="input.tabs == 'map'",
                   selectInput("map_year", "Select Five-Year Range",
                               choices = list("2006-2010" = "2006-2010", "2007-2011" = "2007-2011", "2008-2012" = "2008-2012",
                                              "2009-2013" = "2009-2013", "2010-2014" = "2010-2014", "2011-2015" = "2011-2015", 
                                              "2012-2016" = "2012-2016", "2013-2017" = "2013-2017")),
                   selectInput("map_gender", "Select Gender",
                               choices = list("Female", "Male")),
                   selectInput("var", "Select Variable of Interest",
                               choices = list("Never Married" = "Never_Married_pct",
                                              "Married" = "Married_pct",
                                              "Separated" = "Separated_pct",
                                              "Widowed" = "Widowed_pct",
                                              "Divorced" = "Divorced_pct"))
                   # checkboxInput("lmap_cas", "Display Casinos", value=FALSE),
                   # actionButton("action2", "REDRAW MAP")
                 ),

                 tags$hr(),
                 
                 ## Author line
                 helpText("Created by Emily R. Ramos, Arvind Ramakrishnan, Jenna F. Kiridly, Xuelian Li, Justin Baldwin and Stephen A. Lauer"),
                 helpText("Recreated and Updated by Valerie Evans"),
                 
                 ## Email feedback link
                 ## To develop a link in HTML
                 helpText(a("Send us your comments or feedback!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank")),
                 
                 ## Data source citation
                 helpText(a("Data Source: American Community Survey - Table S1201", href="http://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_14_5YR_S1201&prodType=table",
                            target="_blank")),
                 
                 ## GitHub link
                 helpText(a("View our data and code on GitHub", 
                            href="https://github.com/sEigmA/SEIGMA/tree/gh-pages/marital/marital_status", target="_blank")),
                 
                 helpText("If using Internet Explorer, application only visible in version 10.")
    ),
    
    ######### End of Sidebar  #########
    
    ######### Start of Main Panel #####
    
    bootstrapPage(mainPanel(
      ## Put in logo for title
      a(img(src = "logo.jpg", height=105, width=920), href="http://www.umass.edu/seigma/"),
      
      ## Create tabs
      tabsetPanel(
        tabPanel("About", 
                 ## strong=bold, p=paragraph, em=emboss/italicised or bold italicized, 
                 about_main_text, value="about"),
        
        ## Summary tab
        tabPanel("Summary", 
                 
                 dataTableOutput("summary"),
                 tags$br(),
                 HTML("Population includes individuals aged 15 years and older."),
                 tags$br(),
                 value="summary", 
                 tags$style(type="text/css", '#summary tfoot {display:none;}')),
        
        ## Plot tab with google chart options
        tabPanel("Plot",
                 ## Make chart title here (otherwise not centered)
                 conditionalPanel(
                   condition="input.plotcombine=='Separate'",
                                  h4("Marital Status as a Percentage of the Population by Gender", align="center"),                 
                   ## Make a row to put two charts in
                   plotOutput("fplot"), 
                   plotOutput("mplot")),
                 conditionalPanel(
                   condition = "input.plotcombine=='Together'",
                                  h4("Marital Status as a Percentage of the Population by Gender", align="center"),                 
                   plotOutput("fmplot")),
                 HTML("Horizontal bars indicate the span of five-year estimates, vertical bars with hinges indicate the standard errors. Population includes individuals aged 15 years and older."),
                 value="plot"),
        
        ## plot map
        tabPanel("Map",
                 ## Add a little CSS to make the map background pure white
                 tags$head(tags$style("{display: none;}.floater {background-color: white; padding: 8px; opacity: 1; border-radius: 6px; box-shadow: 0 0 15px rgba(0,0,0,0.2);}
                                      ")),
                 leafletMap("map", width="100%", height=500, 
                            options=list(center = c(42.15, -71.65), zoom=8, 
                                         ##Bounds for the map for when zoomed in on mass
                                         maxBounds = list(list(41, -73.5), list(43, -70)))),
                 
                 ## Info Box 
                 conditionalPanel(
                   condition="input.action != 0",
                   absolutePanel(left=100, top=450, width=300, class="floater", htmlOutput("details"))),
                 conditionalPanel(
                   condition="input.tabs == 'map' && input.action == 0",
                   ## within the map area, you can create an action button.  similar to initializing the legend but just putting a button instead.
                   absolutePanel(right=400, top=300, class="floater", actionButton("action", "Generate Map")
                   )),
                 
                 
                 ## Legend                 
                 # Never Married
                 conditionalPanel(
                   condition="input.tabs == 'map' && input.var == 'Never_Married_pct' && input.action != 0",
                   absolutePanel(
                     right = 10, top = 100, draggable=FALSE, style = "", 
                     class = "floater",
                     strong("Never Married"),
                     plotOutput("legend1"),
                     tags$table(
                       tags$tr(
                         tags$td(tags$div(
                           style = sprintf("width: 16px; height: 16px; background-color: %s;", "#999999")
                         )),
                         tags$td("Data not", br(), "available", align = "right")
                       )
                     )
                   )),
                 # Married
                 conditionalPanel(
                   condition="input.tabs == 'map' && input.var == 'Married_pct' && input.action != 0",
                   absolutePanel(
                     right = 10, top = 100, draggable=FALSE, style = "", 
                     class = "floater",
                     strong("Married"),
                     plotOutput("legend2"),
                     tags$table(
                       tags$tr(
                         tags$td(tags$div(
                           style = sprintf("width: 16px; height: 16px; background-color: %s;", "#999999")
                         )),
                         tags$td("Data not", br(), "available", align = "right")
                       )
                     )
                   )),
                 # Separated
                 conditionalPanel(
                   condition="input.tabs == 'map' && input.var == 'Separated_pct' && input.action != 0",
                   absolutePanel(
                     right = 10, top = 100, draggable=FALSE, style = "", 
                     class = "floater",
                     strong("Separated"),
                     plotOutput("legend3"),
                     tags$table(
                       tags$tr(
                         tags$td(tags$div(
                           style = sprintf("width: 16px; height: 16px; background-color: %s;", "#999999")
                         )),
                         tags$td("Data not", br(), "available", align = "right")
                       )
                     )
                   )),
                 # Widowed
                 conditionalPanel(
                   condition="input.tabs == 'map' && input.var == 'Widowed_pct' && input.action != 0",
                   absolutePanel(
                     right = 10, top = 100, draggable=FALSE, style = "", 
                     class = "floater",
                     strong("Widowed"),
                     plotOutput("legend4"),
                     tags$table(
                       tags$tr(
                         tags$td(tags$div(
                           style = sprintf("width: 16px; height: 16px; background-color: %s;", "#999999")
                         )),
                         tags$td("Data not", br(), "available", align = "right")
                       )
                     )
                   )),
                 # Divorced
                 conditionalPanel(
                   condition="input.tabs == 'map' && input.var == 'Divorced_pct' && input.action != 0",
                   absolutePanel(
                     right = 10, top = 100, draggable=FALSE, style = "", 
                     class = "floater",
                     strong("Divorced"),
                     plotOutput("legend5"),
                     tags$table(
                       tags$tr(
                         tags$td(tags$div(
                           style = sprintf("width: 16px; height: 16px; background-color: %s;", "#999999")
                         )),
                         tags$td("Data not", br(), "available", align = "right")
                       )
                     )
                   )),
                 value="map"),
        
        tabPanel("More Info", 
                 p(strong("Variable Summary:")),
                 p(strong("Marital Status Rates"),
                   " - The number of people within each marital status category for a region over a specified five-year range. When the number of people in a particular marital status category is too small, data cannot be displayed."), 
                 tags$br(),
                 p(strong("Five-Year Estimate"),
                   " - Survey information is collected everyday of the year and then aggregated over a specific time period, five years. Multi-year estimates are available for regions with populations less than 65,000. However, more precise estimates are possible with larger geographic regions. To analyze change over time, users are discouraged from utilizing overlapping multi-year estimates (e.g. 2005-2009, 2006-2010) due to the inability to isolate change with precision."),
                 
                 ## Email feedback link
                 h3(a("Please fill out our survey to help improve the site!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank")), value="info"),
        id="tabs"
                 )
        ))
)
))


##### SERVER #####
server <- shinyServer(function(input, output, session) {
  ## mar_df is a reactive dataframe. Necessary for when summary/plot/map have common input (Multiple Variables). Not in this project
  mar_df <- reactive({
   
     ## Make year a vector based on input variable 
    years <- input$sum_year
    if(is.null(years)==TRUE){years <- paste(c(2006:2013), c(2006:2013)+4, sep="-")}
    
    ## Filter the data by the chosen Five-Year Range 
    mar_df <- mar_data %>%
      filter(Five_Year_Range %in% years) %>%
      select(1:4, Gender, Five_Year_Range, Population, Never_Married_pct, Never_Married_pct_error, 
             Married_pct, Married_pct_error, Separated_pct, Separated_pct_error, Widowed_pct, Widowed_pct_error, 
             Divorced_pct, Divorced_pct_error) %>%
      arrange(Region, Gender)
    ## Output reactive dataframe
    mar_df    
  })
    
  ## Create summary table
  output$summary <- renderDataTable({
    ## Make reactive dataframe into regular dataframe
    mar_df <- mar_df()
    
    ## Make gender a vector based on input variable
    if(!is.null(input$sum_gender))
      genders <- input$sum_gender
    ## If none selected, put all genders in vector
    if(is.null(input$sum_gender))
      genders <- c("Female", "Male")
    
    ## Make municipals a vector based on input variable
    if(!is.null(input$sum_muni))
      munis <- input$sum_muni
    ## If none selected, put all municipals in vector
    if(is.null(input$sum_muni))
      munis <- MA_municipals
    
    ## If the user checks the meanUS box or the meanMA box, add those to counties vector
    if(input$US_mean){
      if(input$MA_mean){
        munis <- c("United States", "MA", munis) ## US and MA  
      } else{
        munis <- c("United States", munis) ## US only
      }
    } else{
      if(input$MA_mean){
        munis <- c("MA", munis) ## US only ## MA only
      }
    }
    
    ## Create a dataframe consisting only of counties in vector
    mar_df <- mar_df %>%
      filter(Gender %in% genders, Region %in% munis) %>%
      select(4:length(colnames(mar_df)))
    colnames(mar_df) <- gsub("_", " ", colnames(mar_df))
    colnames(mar_df) <- gsub("pct error", "error %", colnames(mar_df))
    colnames(mar_df) <- gsub("pct", "%", colnames(mar_df))
    return(mar_df)
  }, 
  
  ## There are a bunch of options to edit the appearance of datatables, this removes one of the ugly features
  options=list(searching = FALSE, orderClasses = TRUE))   
  
  ## Create plots
  munis_p <- reactive({
    munis_p2 <- input$plot_muni
    ## MA
    if(input$plotMA_mean==T && any(grepl(x=munis_p2, pattern = "MA"))==F){
      return(c("MA", munis_p2[!(munis_p2 =="MA")])) ## US only ## MA only
    }else if(input$plotMA_mean==T && any(grepl(x=munis_p2, pattern = "MA"))==T){
      return(c("MA", munis_p2[!(munis_p2 =="MA")])) ## US only ## MA only
    }
    else if(input$plotMA_mean==F && any(grepl(x=munis_p2, pattern = "MA"))==T){
      return(munis_p2[!(munis_p2 =="MA")]) ## remove MA
    } else if(input$plotMA_mean==F && any(grepl(x=munis_p2, pattern = "MA"))==F){
      return(munis_p2[!(munis_p2 =="MA")]) ## remove MA
    }
  })
  
  munis_pfinal <- reactive({
    munis_p3 <- munis_p()
    ## AMERICA FWURST
    if(input$plotUS_mean==T && any(grepl(x=munis_p3, pattern = "United States"))==F){
      return(c("United States", munis_p3[!(munis_p3 =="United States")])) ##  United States
    }else if(input$plotUS_mean==T && any(grepl(x=munis_p3, pattern = "United States"))==T){
      return(c("United States", munis_p3[!(munis_p3 =="United States")])) ##  United States
    }
    else if(input$plotUS_mean==F && any(grepl(x=munis_p3, pattern = "United States"))==T){
      return(munis_p3[!(munis_p3 =="United States")]) ## remove United States
    } else if(input$plotUS_mean==F && any(grepl(x=munis_p3, pattern = "United States"))==F){
      return(munis_p3[!(munis_p3 =="United States")]) ## remove United States
    }
  })
  
  plot_mar_df <- reactive({
    ## Filter the data by the chosen Five-Year Range
    pvars <- c(input$plotvar, paste(input$plotvar, "error", sep="_"))
    vars <- which(names(mar_data) %in% pvars)
    selmun <- munis_pfinal()
    plot_mar_df <- mar_data %>%
      filter(Region %in% selmun) %>%
      select(c(22,4,5,vars))
    plot_mar_df$Year <- as.integer(sapply(strsplit(as.character(plot_mar_df$Five_Year_Range), split="-"), FUN=function(x){x[1]}))+2
    names(plot_mar_df)[c(4,5)] <- c("Var", "Error")
    
    ## Output reactive dataframe, sorted like selected munis
    return(plot_mar_df[order(match(plot_mar_df$Region, selmun)),])
  })
  
  output$fmplot <- renderPlot({
    ## Make one for males and one for females
    pdf <- plot_mar_df()
    row.names(pdf) <- 1:nrow(pdf)
    pdf$Region <- factor(pdf$Region, levels = unique(pdf$Region,ordered = TRUE))
    ## Mess with the levels statement
    ap=0.5
    sz=1
    
    p=ggplot(pdf, aes(x=Year, y=Var, colour=Region))+
      geom_errorbarh(aes(xmax = Year + 2, xmin = Year - 2, height = 0,colour=Region),alpha=ap/2, size=sz/2)+
      geom_errorbar(aes(ymin = Var-Error, ymax = Var+Error,colour=Region),alpha=ap,size=sz, width=0.125)+
      ylab(paste("Percent of",gsub("_", " ", gsub("_pct", "", input$plotvar)),"Population (%)", sep=" "))+
      scale_color_manual(values=cbbPalette, guide="legend")+
      scale_x_continuous(breaks=c(2006, 2008, 2010, 2012, 2014))+
      geom_point(aes(colour=Region),size=4,alpha=1)+
      geom_line(aes(colour=Region, linetype=Gender),size=2,alpha=1)+
      theme_bw() + 
      theme(plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank() )+
      theme(panel.border= element_blank())+
      theme(axis.line.x = element_line(color="black", size = 0.5),
            axis.line.y = element_line(color="black", size = 0.5))+
      theme(axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14))+
      theme(legend.title=element_text(size=16),
            legend.text=element_text(size=14))+
      ggtitle(paste("Percent of",gsub("_", " ", gsub("_pct", "", input$plotvar)),"Population (%)", sep=" "))
    p
  })
  
  output$fplot <- renderPlot({
    ## Make one for males and one for females
    pdf <- plot_mar_df()
    pdff <- subset(pdf, pdf$Gender=="Female")
    pdff$Region <- factor(pdff$Region, levels = unique(pdff$Region, ordered = TRUE))
    ap=0.5
    sz=1
    
    p=ggplot(pdff, aes(x=Year, y=Var, colour=Region))+
      geom_errorbarh(aes(xmax = Year + 2, xmin = Year - 2, height = 0,colour=Region),alpha=ap/2, size=sz/2)+
      geom_errorbar(aes(ymin = Var-Error, ymax = Var+Error,colour=Region),alpha=ap,size=sz, width=0.125)+
      ylab(paste("Percent of",gsub("_", " ", gsub("_pct", "", input$plotvar)),"Females (%)", sep=" "))+
      scale_color_manual(values=cbbPalette, guide="legend")+
      scale_x_continuous(breaks=c(2006, 2008, 2010, 2012, 2014, 2016))+
      geom_point(aes(colour=Region),size=4,alpha=1)+
      geom_line(aes(colour=Region),size=2,alpha=1)+
      theme_bw() + 
      theme(plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank() )+
      theme(panel.border= element_blank())+
      theme(axis.line.x = element_line(color="black", size = 0.5),
            axis.line.y = element_line(color="black", size = 0.5))+
      theme(axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14))+
      theme(legend.title=element_text(size=16),
            legend.text=element_text(size=14))+
      ggtitle(paste("Percent of",gsub("_", " ", gsub("_pct", "", input$plotvar)),"Females (%)", sep=" "))
    p
  })
  
  output$mplot <- renderPlot({
    ## Make one for males and one for females
    pdf <- plot_mar_df()
    pdfm <- subset(pdf, pdf$Gender=="Male")
    pdfm$Region <- factor(pdfm$Region, levels = unique(pdfm$Region,ordered = TRUE))
    ap=0.5
    sz=1
    
    p=ggplot(pdfm, aes(x=Year, y=Var, colour=Region))+
      geom_errorbarh(aes(xmax = Year + 2, xmin = Year - 2, height = 0,colour=Region),alpha=ap/2, size=sz/2)+
      geom_errorbar(aes(ymin = Var-Error, ymax = Var+Error,colour=Region),alpha=ap,size=sz, width=0.125)+
      ylab(paste("Percent of",gsub("_", " ", gsub("_pct", "", input$plotvar)),"Males (%)", sep=" "))+
      scale_color_manual(values=cbbPalette, guide="legend")+
      scale_x_continuous(breaks=c(2006, 2008, 2010, 2012, 2014, 2016))+
      geom_point(aes(colour=Region),size=4,alpha=1)+
      geom_line(aes(colour=Region),size=2,alpha=1)+
      theme_bw() + 
      theme(plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank() )+
      theme(panel.border= element_blank())+
      theme(axis.line.x = element_line(color="black", size = 0.5),
            axis.line.y = element_line(color="black", size = 0.5))+
      theme(axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14))+
      theme(legend.title=element_text(size=16),
            legend.text=element_text(size=14))+
      ggtitle(paste("Percent of",gsub("_", " ", gsub("_pct", "", input$plotvar)),"Males (%)", sep=" "))
    p
  })

  ## Create map

  ## Set map colors
  map_dat <- reactive({
    op <- 0.8
    #browser()
    ## Browser command - Stops the app right when it's about to break
    
    ## Make reactive dataframe into regular dataframe
    mar_map_df <- mar_df()

    ## Take US, MA, and counties out of map_dat
    map_dat <- mar_map_df %>%
      #filter(Five_Year_Range == input$map_year) %>%
      filter(!is.na(Region), Gender == input$map_gender)
    
    ## assign colors to each entry in the data frame
    color <- as.integer(cut2(map_dat[,input$var],cuts=cuts))
    map_dat <- cbind.data.frame(map_dat, color)
    map_dat$color <- ifelse(is.na(map_dat$color), length(map_colors), 
                            map_dat$color)
    map_dat$opacity <- op
    ## find missing counties in data subset and assign NAs to all values
    missing_munis <- setdiff(leftover_munis_map, map_dat$Region)
    missing_df <- data.frame(Municipal = NA, County = NA, State = NA, Region = missing_munis,
                             Gender = NA, Five_Year_Range = NA, Population = NA, 
                             Never_Married_pct = NA, Never_Married_pct_error = NA, Married_pct = NA,
                             Married_pct_error = NA, Separated_pct = NA, Separated_pct_error = NA,
                             Widowed_pct = NA, Widowed_pct_error = NA, Divorced_pct = NA, 
                             Divorced_pct_error = NA,
                             color=length(map_colors), opacity = 0)
    # combine data subset with missing counties data
    map_dat <- rbind.data.frame(map_dat, missing_df)
    map_dat$color <- map_colors[map_dat$color]
    return(map_dat)
  })

  values <- reactiveValues(selectedFeature=NULL, highlight=c())
  
  ## Draw leaflet map
  map <- createLeafletMap(session, "map")
  ## Does nothing until called (done with action button)
  observe({
    input$action
    
    ## load in relevant map data
    map_dat <- map_dat()
    
    ## All functions which are isolated, will not run until the above observe function is activated
    isolate({
      ## Duplicate MAmap to x
      x <- MA_map_muni
      
      ## for each county in the map, attach the Crude Rate and colors associated
      for(i in 1:length(x$features)){
        ## Each feature is a county
        x$features[[i]]$properties[input$var] <- 
          map_dat[match(x$features[[i]]$properties$NAMELSAD10, map_dat$Region), input$var]
        ## Style properties
        x$features[[i]]$properties$style <- list(
          fill=TRUE, 
          ## Fill color has to be equal to the map_dat color and is matched by county
          fillColor = map_dat$color[match(x$features[[i]]$properties$NAMELSAD10, map_dat$Region)], 
          ## "#000000" = Black, "#999999"=Grey, 
          weight=1, stroke=TRUE, 
          opacity=map_dat$opacity[match(x$features[[i]]$properties$NAMELSAD10, map_dat$Region)], 
          color="#000000", 
          fillOpacity=map_dat$opacity[match(x$features[[i]]$properties$NAMELSAD10, map_dat$Region)])
      }
      
      map$addGeoJSON(x) # draw map
    })
  })

      observe({
        ## EVT = Mouse Click
        evt <- input$map_click
        if(is.null(evt))
          return()
        isolate({
          values$selectedFeature <- NULL
        })
      })
      
      observe({
        evt <- input$map_geojson_click
        if(is.null(evt))
          return()
        input$map_year
        map_dat <- map_dat()
        isolate({
          values$selectedFeature <- evt$properties
          region <- evt$properties$NAMELSAD10
          values$selectedFeature[input$var] <- map_dat[match(region, map_dat$Region), input$var]
        })
      })

     ## This function creates the info box 
      output$details <- renderText({
        
        ## Before a county is clicked, display a message
        if(is.null(values$selectedFeature)){
          return(as.character(tags$div(
            tags$div(
              h4("Click on a town or city"))
          )))
        }
        
        muni_name <- values$selectedFeature$NAMELSAD10
        muni_value <- values$selectedFeature[input$var]
        var_select <- gsub("_", " ", input$var)
        var_select <- gsub("pct", "", var_select)
        

        ## If clicked county has no crude rate, display a message
        if(is.null(values$selectedFeature[input$var])){
          return(as.character(tags$div(
            tags$h5(var_select, "% in ", muni_name, "is not available for this timespan"))))
        }
        ## For a single year when county is clicked, display a message
        as.character(tags$div(
          tags$h4(var_select, "% in ", muni_name, " for ", input$map_year),
          tags$h5(muni_value, "%")
        ))
      })
      
      output$legend1 <- renderPlot({  
        paint.brush = colorRampPalette(colors=c("white", "deeppink"))
        cols <- paint.brush(101)
        leg_dat <- data_frame(y = seq(0, 100), x = 1, col = cols)
        
        b <- ggplot(data = leg_dat) +
          geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
          scale_fill_manual(values = leg_dat$col) + theme_bw() +
          theme(axis.text.x = element_blank(),
                axis.text.y = element_text(size = 12),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.ticks.x = element_blank(),
                panel.border = element_blank(),
                panel.grid.minor = element_blank(),
                panel.grid.major = element_blank())
        return(b)
        
      })
      
      output$legend2 <- renderPlot({  
        paint.brush = colorRampPalette(colors=c("white", "deeppink"))
        cols <- paint.brush(101)
        leg_dat <- data_frame(y = seq(0, 100), x = 1, col = cols)
        
        b <- ggplot(data = leg_dat) +
          geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
          scale_fill_manual(values = leg_dat$col) + theme_bw() +
          theme(axis.text.x = element_blank(),
                axis.text.y = element_text(size = 12),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.ticks.x = element_blank(),
                panel.border = element_blank(),
                panel.grid.minor = element_blank(),
                panel.grid.major = element_blank())
        return(b)
        
      })
      
      output$legend3 <- renderPlot({  
        paint.brush = colorRampPalette(colors=c("white", "deeppink"))
        cols <- paint.brush(101)
        leg_dat <- data_frame(y = seq(0, 100), x = 1, col = cols)
        
        b <- ggplot(data = leg_dat) +
          geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
          scale_fill_manual(values = leg_dat$col) + theme_bw() +
          theme(axis.text.x = element_blank(),
                axis.text.y = element_text(size = 12),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.ticks.x = element_blank(),
                panel.border = element_blank(),
                panel.grid.minor = element_blank(),
                panel.grid.major = element_blank())
        return(b)
        
      })
      
      output$legend4 <- renderPlot({  
        paint.brush = colorRampPalette(colors=c("white", "deeppink"))
        cols <- paint.brush(101)
        leg_dat <- data_frame(y = seq(0, 100), x = 1, col = cols)
        
        b <- ggplot(data = leg_dat) +
          geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
          scale_fill_manual(values = leg_dat$col) + theme_bw() +
          theme(axis.text.x = element_blank(),
                axis.text.y = element_text(size = 12),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.ticks.x = element_blank(),
                panel.border = element_blank(),
                panel.grid.minor = element_blank(),
                panel.grid.major = element_blank())
        return(b)
        
      })
      
      output$legend5 <- renderPlot({  
        paint.brush = colorRampPalette(colors=c("white", "deeppink"))
        cols <- paint.brush(101)
        leg_dat <- data_frame(y = seq(0, 100), x = 1, col = cols)
        
        b <- ggplot(data = leg_dat) +
          geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
          scale_fill_manual(values = leg_dat$col) + theme_bw() +
          theme(axis.text.x = element_blank(),
                axis.text.y = element_text(size = 12),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.ticks.x = element_blank(),
                panel.border = element_blank(),
                panel.grid.minor = element_blank(),
                panel.grid.major = element_blank())
        return(b)
        
      })

})


##### RUN APP #####
shinyApp(ui=ui, server=server)
