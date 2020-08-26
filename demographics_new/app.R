#################################################
##  Demographics App with Plotly               ##
##  Using ACS demographic data (Table S2502)   ##
##  Created:  06/25/2020 VE                    ##
##  Modified: 08/05/2020                       ##
#################################################

library(RJSONIO)
library(shiny)
library(plotly)
library(dplyr)
library(DT)
library(Hmisc)
library(leaflet)
library(reshape2)

####  Global ####
## Load map data
MA_map_county <- fromJSON("County_2010Census_DP1.geojson")
MA_map_muni <- fromJSON("Muni_2010Census_DP1.geojson")

## Load formatted demographics data
Dem_data <- read.csv(file = "demographics.csv")
colnames(Dem_data)[12:37] <- c("Age_under_5_Pct","Margin_Error_under_5_Pct","Age_5-9_Pct", "Margin_Error_5-9_Pct",
                               "Age_10-14_Pct","Margin_Error_10-14_Pct","Age_15-19_Pct", "Margin_Error_15-19_Pct",
                               "Age_20-24_Pct","Margin_Error_20-24_Pct","Age_25-34_Pct", "Margin_Error_25-34_Pct",
                               "Age_35-44_Pct", "Margin_Error_35-44_Pct","Age_45-54_Pct", "Margin_Error_45-54_Pct",
                               "Age_55-59_Pct", "Margin_Error_55-59_Pct","Age_60-64_Pct", "Margin_Error_60-64_Pct",
                               "Age_65-74_Pct", "Margin_Error_65-74_Pct","Age_75-84_Pct", "Margin_Error_75-84_Pct",
                               "Age_85+Pct", "Margin_Error_85+_Pct")
colnames(Dem_data)[54:59] <- c("Age_under_20_Pct_plot","Age_20-34_Pct_plot","Age_35-54_Pct_plot", "Age_55-64_Pct_plot",
                               "Age_65-74_Pct_plot", "Age_75+_Pct_plot")

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

## Colors for a five-year legend "#00FF00" "white","#0072B2", "'"#FF0000"
paint_brush <- colorRampPalette(colors = c("lightgreen","yellow", "red"))
map_colors <- c(paint_brush(n = 25), "#999999")

## Puts each county year in between the cuts (n colors, n+1 cuts), length.out will make that many cuts
## Cuts based on Age range 
agemax.val <- 46
agemin.val <- 0
agecuts <- seq(agemin.val, agemax.val, length.out = length(map_colors))

## Cuts based on Gender range
genmax.val <- max(c(max(Dem_data$Female_Pct, na.rm = TRUE), max(Dem_data$Male_Pct, na.rm = TRUE)))
genmin.val <- min(c(min(Dem_data$Female_Pct, na.rm = TRUE), min(Dem_data$Male_Pct, na.rm = TRUE)))
gencuts <- seq(genmin.val, genmax.val, length.out = length(map_colors))

## Cuts based on Rage/Ethnicity range
racemax.val <- 100
racemin.val <- 0
racecuts <- seq(racemin.val, racemax.val, length.out = length(map_colors))

## Construct a data frame with break ranges for displaying in the legend
## head = scuts takes everything except for the last one, tails = same thing opposite
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

about_side_text <- conditionalPanel(
  condition = "input.tabs == 'about'",
  h5(strong("SEIGMA Demographics App"))
  )

tags$hr()

summary_side_text <- conditionalPanel(
  condition = "input.tabs == 'summary'",
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
    tags$li('For the five-year ranges below, you can compare the demographic estimate in a municipality to national, state, and county estimates.'),
    tags$br(),
    tags$li('The estimate for age, gender, race, or ethinicity can be sorted in ascending and descending order by clicking the column or variable.'),
    tags$br(),
    tags$li('Please note that all statistics are five-year estimates.')
  )
)

plot_side_text <- conditionalPanel(
  condition = "input.tabs == 'plot'",
  h4("How to use this app:"), 
  helpText(p(strong('Please select the municipality and five-year range for which you are interested in viewing the estimate of age, gender, race, or ethinicity.'))),
  tags$br(),
  tags$ul(
    tags$li('View the estimate for age, gender, race, or ethnicity by selecting the appropriate box below.'),
    tags$br(),
    tags$li('For a given five-year period, you can compare the municipality of your choice to the national, state, and county estimate for age, gender, race, and ethnicity.')
  ))
tags$hr()

map_side_text <- conditionalPanel(
  condition = "input.tabs == 'map'",
  h4("How to use this app:"),
  helpText(p(strong("Please select a five-year range and click on Generate Map to get started."))),
  tags$br(),
  tags$ul(
    tags$li('Select the variable of interest for age, gender, race, or ethnicity by selecting either an age range, gender, race, or ethnicity from the drop down list below.'),
    tags$br(),
    tags$li('Clicking on a municipality will display the variable of interest for the five-year range and gender that you selected.')
  ))

tags$hr()

info_side_text <- conditionalPanel(
  condition = "input.tabs == 'info'",
  h4("How to use this app:"),
  helpText(p(strong('This tab contains more detailed information regarding the variables of interest.')))
)

about_main_text <- p(strong("The SEIGMA Demographics App"), "displays the demographic composition of Massachusetts' municipalities over a five-year period.",
                     p(strong("Click on different tabs to see the data in different formats.")),
                     tags$br(),
                     tags$ul(
                       tags$li(p(strong("Summary"), "shows the data in table format.")),
                       tags$li(p(strong("Plot"), "compares a municipality's demographic estimate to county, state, and national estimates.")),
                       tags$li(p(strong("Map"), "visually displays demographic estimates by municipality.")),
                       tags$li(p(strong("More Info"), "describes demographic estimates including formulas and calculations."))
                     )
)

font_size <- 14


####  UI  ####
ui <- shinyUI(fluidPage(
  ## embed the google analytics script in the app
  tags$head(includeScript("google-analytics.js")),
  ## HTML to create generate map button
  gen_map_button,
  ## blank title, but put in a special title for window tab
  titlePanel("", windowTitle = "SEIGMA: Demographics App"),
  
  ## Create sidebar
  sidebarLayout(
    sidebarPanel(width = 4,
                 ## Conditional panel means if the condition is met show all text below otherwise Don't!
                 about_side_text,
                 summary_side_text,
                 plot_side_text,
                 map_side_text,
                 info_side_text,
                 
                 ##in summary, allow for year, categorical variables,  municipal selection selection
                 conditionalPanel(
                   condition = "input.tabs == 'summary'",
                   selectInput("sum_year", "Select Five Year Range",
                               choices = list("2006-2010" = "2006-2010", "2007-2011" = "2007-2011", "2008-2012" = "2008-2012", 
                                              "2009-2013" = "2009-2013", "2010-2014" = "2010-2014", "2011-2015" = "2011-2015", 
                                              "2012-2016" = "2012-2016", "2013-2017" = "2013-2017", "2014-2018" = "2014-2018")
                   ),
                   radioButtons("sum_radio", "Categorical variables",
                                c("Age" = "Age", "Gender" = "Gender",
                                  "Race" = "Race","Ethnicity" ="Ethnicity"),
                                selected = "Age"),
                   selectInput("sum_muni", "Select Municipality",
                               choices = MA_municipals,
                               ## Multiple allows for multi-county selection
                               multiple = TRUE),
                   ##show boxes that will compare to MA or US average,False at the end means it starts off unchecked
                   checkboxInput("MA_mean", "Compare to MA Average", FALSE),
                   checkboxInput("US_mean", "Compare to US Average", FALSE)
                 ),
                 
                 ## in plot, allow for catigorical variables,  municipal selection selection
                 conditionalPanel(
                   condition = "input.tabs == 'plot'",
                   ## Select input = List
                   selectInput("plot_year", "Select Five Year Range",
                               choices = list("2006-2010" = "2006-2010", "2007-2011" = "2007-2011", "2008-2012" = "2008-2012", 
                                              "2009-2013" = "2009-2013", "2010-2014" = "2010-2014", "2011-2015" = "2011-2015", 
                                              "2012-2016" = "2012-2016", "2013-2017" = "2013-2017", "2014-2018" = "2014-2018")
                   ),
                   radioButtons("plot_radio", "Categorical variables",
                                c("Age" = "Age", "Gender" = "Gender",
                                  "Race" = "Race", "Ethnicity" = "Ethnicity"),
                                selected = "Age"),
                   selectInput("plot_muni", "Select Municipality",
                               choices = MA_municipals, 
                               selected = "Springfield")
                 ),
                 
                 ## In map, 
                 conditionalPanel(
                   condition = "input.tabs == 'map'",
                   selectInput("map_year", "Select Five Year Range",
                               choices = list("2006-2010" = "2006-2010", "2007-2011" = "2007-2011", "2008-2012" = "2008-2012", 
                                              "2009-2013" = "2009-2013", "2010-2014" = "2010-2014", "2011-2015" = "2011-2015", 
                                              "2012-2016" = "2012-2016", "2013-2017" = "2013-2017", "2014-2018" = "2014-2018")
                   ),
                   radioButtons("map_radio", "Categorical variables",
                                c("Age" = "Age", "Gender" = "Gender",
                                  "Race" = "Race", "Ethnicity" ="Ethnicity"),
                                selected = "Age")
                 ),
                 
                 ## in map, allow for variable selection
                 conditionalPanel(
                   condition = "input.tabs == 'map'&& input.map_radio =='Age'",
                   selectInput("var_age", "Select Variable of Interest",
                               choices = list("under 20" = "Age_under_20_Pct_plot",
                                              "20-34"="Age_20-34_Pct_plot",
                                              "35-54"="Age_35-54_Pct_plot",
                                              "55-64"="Age_55-64_Pct_plot",
                                              "65-74"="Age_65-74_Pct_plot",
                                              "over 75"="Age_75+Pct_plot"),
                               selected = "Age_under_20_Pct_plot")
                 ),
                 
                 conditionalPanel(
                   condition = "input.tabs == 'map'&& input.map_radio =='Gender'",
                   selectInput("var_gen", "Select Variable of Interest",
                               choices = list("Male" = "Male_Pct", "Female" = "Female_Pct"),
                               selected = "Female_Pct")
                 ),
                 
                 conditionalPanel(
                   condition = "input.tabs == 'map'&& input.map_radio =='Race'",
                   selectInput("var_rac", "Select Variable of Interest",
                               choices = list("Percent White" = "White_Pct", "Percent Black" = "Black_Pct", "Percent Asian" = "Asian_Pct")
                   )
                 ),
                 
                 conditionalPanel(
                   condition="input.tabs == 'map'&& input.map_radio =='Ethnicity'",
                   selectInput("var_eth", "Select Variable of Interest",
                               choices = list("Hispanic or Latino" = "Hispanic_Pct", "not Hispanic or Latino" = "Not_Hispanic_Pct"),
                               selected = "Not_Hispanic_Pct")
                 ),
                 tags$hr(),
                 
                 ## author line
                 helpText("Created by Xuelian Li, Emily R. Ramos, Arvind Ramakrishnan, and Jenna F. Kiridly"),
                 helpText("Recreated by Valerie Evans"),
                 
                 ## email feedback link
                 ## To develop a link in HTML
                 helpText(a("Send us your comments or feedback!", href = "http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target = "_blank", onclick = "ga('send', 'event', 'click', 'link', 'feedback', 1)")),
                 
                 ## data source citation
                 helpText(a("Data Source: American Community Survey - Table S2502", href = "https://data.census.gov/cedsci/table?q=Table%20S2502&tid=ACSST5Y2018.S2502",
                            target = "_blank", onclick = "ga('send', 'event', 'click', 'link', 'dataAge', 1)")),
                 
                 ## GitHub link
                 helpText(a("View our data and code on GitHub",
                            href = "https://github.com/sEigmA/SEIGMA/tree/gh-pages/demographics_new", 
                            target = "_blank", onclick = "ga('send', 'event', 'click', 'link', 'code', 1)")),
                 helpText("If using Internet Explorer, application only visible in version 10.")
    ),
    ######### End of Sidebar  #########
    
    ######### Start of Main Panel #####
    bootstrapPage(
      mainPanel(
        ## put in logo for title
        a(img(src = "SEIGMA-Logo-Hex.jpg", height = 120, width = 600), href = "http://www.umass.edu/seigma/"),
        
        ## create tabs
        tabsetPanel(
          tabPanel("About",
                   ## strong=bold, p=paragraph, em=emboss/italicised or bold italicized,
                   about_main_text, value = "about"),
          
          ## summary tab
          tabPanel("Summary",
                   dataTableOutput("summary"), value = "summary",
                   tags$style(type = "text/css", '#summary tfoot {display:none;}')),
          
          ## plot tab with plotly
          tabPanel("Plot",
                   ## make chart title here (otherwise not centered)
                   conditionalPanel(
                     condition = "input.plot_radio =='Age'",
                     plotlyOutput("Plot_age"),
                     p(strong("Age"),
                       " - The number of categories for age has been collapsed to the following six groups: <20, 20-34, 35-54, 55-64, 65-74,75+.  This is done in order to simplify the presentation of data.  To see all age groups please go to the summary tab.")),
                   conditionalPanel(
                     condition = "input.plot_radio == 'Gender'",
                     plotlyOutput("Plot_gender")
                   ), 
                   conditionalPanel(
                     condition = "input.plot_radio == 'Race'",
                     plotlyOutput("Plot_race")
                   ),
                   conditionalPanel(
                     condition = "input.plot_radio == 'Ethnicity'",
                     plotlyOutput("Plot_ethnicity")
                   ),
                   value = "plot"),
          
          ## map tab
          tabPanel("Map",
                   ## Add a little CSS to make the map background pure white
                   tags$head(tags$style(
                     "#showcase-code-position-toggle, #showcase-sxs-code { display: none; }
.floater { background-color: white; padding: 8px; opacity: 1; border-radius: 6px; box-shadow: 0 0 15px rgba(0,0,0,0.2); }"
                   )),
                   ## Map Creation
                   leafletMap("map", width = "100%", height = 500,
                              options=list(center = c(42.15, -71.65), zoom = 8,
                                           ##Bounds for the map for when zoomed in on mass
                                           maxBounds = list(list(41, -73.5), list(43, -70)))),
                   ## Info Box
                   conditionalPanel(
                     condition="input.action != 0",
                     absolutePanel(left = 100, top = 450, width = 300, class = "floater", htmlOutput("details"))),
                   
                   conditionalPanel(
                     condition="input.tabs == 'map' && input.action == 0",
                     ## within the map area, you can create an action button.  similar to initializing the legend but just putting a button instead.
                     absolutePanel(right = 400, top = 300, class = "floater",
                                   actionButton("action", "Generate Map"))
                   ),
                   
                   ## Age Legend
                   conditionalPanel(
                     condition = "input.action != 0",
                     absolutePanel(
                       right = 10, top = 100, draggable = FALSE, style = "",
                       class = "floater",
                       strong(textOutput("text1")),
                       strong("Percentage"),
                       plotOutput("legend1"),
                       tags$table(
                         tags$tr(
                           tags$td(tags$div(
                             style = sprintf("width: 16px; height: 16px; background-color: %s;", "#999999")
                           )),
                           tags$td("Data not", br(),"available", align = "right")
                         )
                       )
                     )),
                   ## Gender Legend
                   conditionalPanel(
                     condition = "input.map_radio =='Age' && input.action != 0",
                     p(strong("Age"),
                       " - The number of categories for age has been collapsed to the following six groups: <20, 20-34, 35-54, 55-64, 65-74,75+. This is done in order to simplify the presentation of data. To see all age groups please go to the summary tab.") 
                   ),
                   ## Race Legend
                   conditionalPanel(
                     condition = "input.map_radio =='Race' && input.action != 0",
                     p(strong("Race"),
                       " - Race categories are listed here as White, Black, and Asian. Although the data for other races is available, the percentage is too small to depict in map format accurately. To view the percentage of other race categories please refer to the Plot or Summary tabs.") 
                   ),
                   value = "map"
          ),
          
          tabPanel("More Info",
                   p(strong("Variable Summary:")),
                   p(strong("Race"),
                     " - The number of people within each race for a region over a specified five-year range. Races are listed as White, Black or African American, Asian, American Indian or Alaska Native, Native Hawaiian or Other Pacific Islander, or some other race (Other). Within the Map tab, race categories are listed as White, Black, and Asian. Although the data for other races is available, the percentage is too small to depict in map format accurately. To view the percentage of other race categories please refer to the Plot or Summary tabs."),
                   tags$br(),
                   p(strong("Ethnicity"),
                     " - The number of people within each ethnicity for a region over a specified five-year range. Ethnicities are listed as Hispanic or not Hispanic."),
                   tags$br(),
                   p(strong("Gender"),
                     " - The number of people within each gender for a region over a specified five-year range."),
                   tags$br(),
                   p(strong("Age"),
                     " - The number of people within each age group for a region over a specified five-year range. Age groups are specified as <5, 5-9, 10-14, 15-19, 20-24, 25-34, 35-44, 45-54, 55-59, 60-54, 65-74, 75-84, and 85+. Within the Plot and Map tabs the number of categories for age has been collapsed to the following six groups: <20, 20-34, 35-54, 55-64, 65-74, 75+. This is done in order to simplify the presentation of data. To see all age groups please go to the Summary tab."),
                   tags$br(),
                   p(strong("Five-Year Estimate"), 
                     " - Survey information is collected everyday of the year and then aggregated over a specific time period, in this case, five years. Multi-year estimates are available for regions with populations less than 65,000. However, more precise estimates are possible with larger geographic regions. To analyze change over time, users are discouraged from utilizing overlapping multi-year estimates (e.g. 2006-2010, 2007-2011) due to the inability to isolate change with precision."),
                   
                   ## email feedback link
                   h3(a("Please fill out our survey to help improve the site!", href = "http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target = "_blank")),
                   value = "info"
          ),
          id = "tabs"
        ) 
      )
    )
  )
)
)

####  SERVER  ####
server <- shinyServer(function(input, output, session) {
  ## Dem_df is a reactive dataframe
  
  Dem_df <- reactive({
    Dem_df <- Dem_data       
    ## Output reactive dataframe
    Dem_df
  })
  
  
  ## Create summary table
  output$summary <- renderDataTable({
    ## Make reactive dataframe into regular dataframe
    Dem_df <- Dem_df()
    
    ## make municipals a vector based on input variable
    if(!is.null(input$sum_muni)){
      munis <- input$sum_muni
    }
    ## if none selected, put all municipals in vector
    else {
      munis <- MA_municipals
    }
    ## if the user checks the meanUS box or the meanMA box, add those to counties vector
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
    ##select columns according input$radio
    sel_col_num <- c()
    if (input$sum_radio == "Age") {
      sel_col_num <- c(12:37)
    } else if (input$sum_radio == "Gender") {
      sel_col_num <- c(8:11)
    } else if (input$sum_radio == "Race") {
      sel_col_num <- c(38:49)
    } else {sel_col_num <- c(50:53)}
    
    ## create a dataframe consisting only of counties in vector
    Dem_df <- Dem_df %>%
      filter(Region %in% munis, Five_Year_Range == input$sum_year) %>%
      select(4:7, sel_col_num)
    
    colnames(Dem_df) <- gsub("_", " ", colnames(Dem_df))
    colnames(Dem_df) <- gsub("Pct", "Percentage", colnames(Dem_df))
    
    return(Dem_df)
  }, options=list(searching = FALSE, orderClasses = TRUE)) # there are a bunch of options to edit the appearance of datatables, this removes one of the ugly features

  ## create the plot of the data
  munis_df <- reactive({
    ## make reactive dataframe into regular dataframe
    Dem_df <- Dem_df()%>%
      filter(Five_Year_Range == input$plot_year) 
    ## find the county of the municipal
    county <- as.character(Dem_df$County[match(input$plot_muni, Dem_df$Municipal)])
    
    ## make counties a vector based on input variable
    munis <- input$plot_muni
    
    muni_index <- c()
    for(i in 1:length(munis)){
      muni_index[i] <- match(munis[i], Dem_df$Region)
    }
    
    Dem_df$Region <- factor(Dem_df$Region, levels = c(munis, as.character(Dem_df$Region)[-muni_index]))
    
    sel_col_num1 <- c()
    if (input$plot_radio == "Age") {
      sel_col_num1 <- c(54, 55, 56, 57, 58, 59)
    } else if (input$plot_radio == "Gender") {
      sel_col_num1 <- c(8,10)
    } else if (input$plot_radio == "Race") {
      sel_col_num1 <- c(38,40,42,44,46,48)
    } else {sel_col_num1 <- c(50,52)}
    
    ## create a dataframe consisting only of counties in vector
    muni_df <- Dem_df %>%
      filter(Region %in% munis) %>%
      select(4, sel_col_num1)
    muni_df <- melt(muni_df)
    muni_df$variable <- gsub("_plot", "", muni_df$variable)
    muni_df$variable <- gsub("_Pct", "", muni_df$variable)
    muni_df$variable <- gsub("under ", "<", muni_df$variable)
    muni_df$variable <- gsub("_", " ", muni_df$variable)
    muni_df
  })
    
    output$Plot_age <- renderPlotly({
      plot_ly(data = munis_df(), x = ~variable, y = ~value, type = 'bar') %>%
        layout(
          title = paste("Percentage of each Age Group in", input$plot_muni, "for", input$plot_year),
          xaxis = list(title = "Age Groups", categoryorder = "array", 
                       categoryarray = c("Age under 20", "Age 20-34", "Age 35-54", "Age 55-64", "Age 65-74", "Age 75+")), 
          yaxis = list(title = "Percentage"), 
          margin = list(t = 50, b = 5)
        )
    })
    
    output$Plot_gender <- renderPlotly({
      plot_ly(data = munis_df(), x = ~variable, y = ~value, type = 'bar', width = 400) %>%
        layout(
          title = paste("Percentage by Gender \nin", input$plot_muni, "for", input$plot_year),
          xaxis = list(title = "Gender"), 
          yaxis = list(title = "Percentage"), 
          margin = list(t = 50, b = 5)
        )
    })
    
    output$Plot_race <- renderPlotly({
      plot_ly(data = munis_df(), x = ~variable, y = ~value, type = 'bar') %>%
        layout(
          title = paste("Percentage by Race in", input$plot_muni, "for", input$plot_year),
          xaxis = list(title = "Race", categoryorder = "array", 
                       categoryarray = c("White", "Black", "Asian", "Hawaiian and Other Pacific Islander", "American Indian and Alaska Native", "Other")), 
          yaxis = list(title = "Percentage"), 
          margin = list(t = 50, b = 5)
        )
    })
    
    output$Plot_ethnicity <- renderPlotly({
      plot_ly(data = munis_df(), x = ~variable, y = ~value, type = 'bar', width = 400) %>%
        layout(
          title = paste("Percentage Hispanic \nin", input$plot_muni, "for", input$plot_year),
          xaxis = list(title = "Ethnicity"), 
          yaxis = list(title = "Percentage"), 
          margin = list(t = 50, b = 5)
        )
    })
    
  ## set map colors
  map_dat <- reactive({
    ## make reactive dataframe into regular dataframe
    Dem_dat <- Dem_df()%>%
      filter(Five_Year_Range == input$map_year)%>%
      ## take US, MA, and counties out of map_dat
      filter(!is.na(Municipal))
    
    ## get column name and cuts based on input
    if (input$map_radio == "Age") {
      col <- input$var_age
      cuts <- agecuts
    }
    else if (input$map_radio == "Gender"){
      col <- input$var_gen
      cuts <- gencuts
    }
    else if (input$map_radio == "Race"){
      col <- input$var_rac
      cuts <- racecuts
    }
    else {
      col <- input$var_eth
      cuts <- racecuts 
    }
    
    ## assign colors to each entry in the data frame
    col_sel_num1 <- which(colnames(Dem_dat) == col )
    map_dat <- select(Dem_dat, c(1:6, col_sel_num1))
    color <- as.integer(cut2(map_dat[,col], cuts = cuts))
    map_dat <- cbind.data.frame(map_dat, color)
    map_dat$color <- ifelse(is.na(map_dat$color), length(map_colors), map_dat$color)
    map_dat$opacity <- 0.7
    
    ## find missing counties in data subset and assign NAs to all values
    missing_munis <- setdiff(leftover_munis_map, map_dat$Region)
    missing_df <- data.frame(Municipal = NA, County = NA, State = "MA",
                             Region = missing_munis, Five_Year_Range = input$map_year,
                             Total_Population = NA,
                             Map_var = NA, color = length(map_colors), opacity = 0)
    colnames(missing_df)[7] <- col
    # combine data subset with missing counties data
    map_dat <- rbind.data.frame(map_dat, missing_df)
    map_dat$color <- map_colors[map_dat$color]
    return(map_dat)
  })
  
  values <- reactiveValues(selectedFeature = NULL, highlight = c())
  ## draw leaflet map
  map <- createLeafletMap(session, "map")
  ## the functions within observe are called when any of the inputs are called
  
  ## Does nothing until called (done with action button)
  observe({
    input$action
    
    ## load in relevant map data
    map_dat <- map_dat()
    col_name <- colnames(map_dat)[7]
    ## All functions which are isolated, will not run until the above observe function is activated
    isolate({
      ## Duplicate MAmap to x
      x <- MA_map_muni
      ## for each county in the map, attach the Crude Rate and colors associated
      for(i in 1:length(x$features)){
        ## Each feature is a county
        x$features[[i]]$properties[col_name] <-
          map_dat[match(x$features[[i]]$properties$NAMELSAD10, map_dat$Region), col_name]
        ## Style properties
        x$features[[i]]$properties$style <- list(
          fill = TRUE,
          ## Fill color has to be equal to the map_dat color and is matched by county
          fillColor = map_dat$color[match(x$features[[i]]$properties$NAMELSAD10, map_dat$Region)],
          ## "#000000" = Black, "#999999" = Grey,
          weight = 1, stroke = TRUE,
          opacity = map_dat$opacity[match(x$features[[i]]$properties$NAMELSAD10, map_dat$Region)],
          color = "#000000",
          fillOpacity = map_dat$opacity[match(x$features[[i]]$properties$NAMELSAD10, map_dat$Region)])
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
    map_dat <- map_dat()
    col_name <- colnames(map_dat)[7]
    isolate({
      values$selectedFeature <- evt$properties
      region <- evt$properties$NAMELSAD10
      values$selectedFeature[col_name] <- map_dat[match(region, map_dat$Region), col_name]
    })
  })
  ##  This function is what creates info box
  output$details <- renderText({
    if (input$map_radio == "Age") {
      col <- input$var_age
    }
    else if (input$map_radio == "Gender"){
      col <- input$var_gen
    }
    else if (input$map_radio == "Race"){
      col <- input$var_rac
    }
    else{
      col <- input$var_eth 
    }
    ## Before a county is clicked, display a message
    if(is.null(values$selectedFeature)){
      return(as.character(tags$div(
        tags$div(
          h4("Click on a town or city"))
      )))
    }
    
    muni_name <- values$selectedFeature$NAMELSAD10
    muni_value <- values$selectedFeature[col]
    var_select <- gsub("Pct", "", col)
    var_select <- gsub("_", " ", var_select)
    var_select <- gsub("plot", "", var_select)
    
    ## If clicked county has no crude rate, display a message
    if(is.null(values$selectedFeature[col])){
      return(as.character(tags$div(
        tags$h5("Percentage of", var_select, "in ", muni_name, "is not available for this timespan"))))
    }
    ## For a single year when county is clicked, display a message
    as.character(tags$div(
      tags$h4("Percentage of", var_select, "in ", muni_name, " for ", input$map_year),
      tags$h5(muni_value, "%")
    ))
  })

##########################################################

    output$legend1 <- renderPlot({  
    paint.brush = colorRampPalette(colors = c("lightgreen","yellow", "red"))
    cols <- paint.brush(length(map_colors)-1)
    if(input$map_radio =='Age'){
      leg_dat <- data_frame(y = seq(agemin.val, agemax.val,length.out=(length(map_colors)-1)), x = 1, col = cols)
      
      q <- ggplot(data = leg_dat) +
        geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
        scale_y_continuous(limits = c(agemin.val, agemax.val), breaks = round(seq(agemin.val, agemax.val, length.out = 5),0)) +
        scale_fill_manual(values = leg_dat$col) + theme_bw() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_text(size = 12),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks.x = element_blank(),
              panel.border = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank())
    }
    else if(input$map_radio == 'Gender'){
      leg_dat <- data_frame(y = seq(genmin.val, genmax.val, length.out = (length(map_colors)-1)), x = 1, col = cols)
      
      q <- ggplot(data = leg_dat) +
        geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
        scale_y_continuous(limits = c(genmin.val, genmax.val), breaks = round(seq(genmin.val, genmax.val, length.out = 5),1)) +
        scale_fill_manual(values = leg_dat$col) + theme_bw() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_text(size = 12),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks.x = element_blank(),
              panel.border = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank())
    }
    else {
      leg_dat <- data_frame(y = seq(racemin.val, racemax.val, length.out = (length(map_colors)-1)), x = 1, col = cols)
      
      q <- ggplot(data = leg_dat) +
        geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
        scale_y_continuous(limits = c(racemin.val, racemax.val), breaks = round(seq(racemin.val, racemax.val, length.out = 5),1)) +
        scale_fill_manual(values = leg_dat$col) + theme_bw() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_text(size = 12),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks.x = element_blank(),
              panel.border = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank())
    }
    return(q)
    
  })
  output$text1 <- renderText({
    return(as.character(
      input$map_radio
    ))
  })
})

# Run the application 
shinyApp(ui = ui, server = server)
