## load necessary libraries
require(shiny)
require(googleCharts)

## load in the data
suidata <- read.csv(file="SASuicidedata.csv")[,-1]

## create maxs and mins for googleCharts
xlim <- list(
  min = min(suidata$Year)-1,
  max = max(suidata$Year)+1
)
ylim <- list(
  min = 0,
  max = max(suidata$Crude.Rate, na.rm=T)+5
)

## set graph colors (special for colorblind people)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                "#0072B2", "#D55E00", "#CC79A7")

shinyUI(fluidPage(
  ## this starts the googleCharts engine
  googleChartsInit(),
  
  ## blank title, but put in a special title for window tab
  titlePanel("", windowTitle = "SEIGMA Shiny App: Suicide"),
  
  ## create sidebar
  sidebarLayout(
    sidebarPanel(
      helpText("If using Internet Explorer, application only visible in version 10."),
      tags$hr(),
      
      ## in summary and map, allow for timespan selection
      conditionalPanel(
        condition="input.tabs == 'summary' || input.tabs == 'map'",
        selectInput("timespan", "Select Timespan",
                    list("Single Year" = "sing.yr",
                         "Multiple Years" = "mult.yrs"))
      ),
      
      # if single year is selected, select year. if multiple years are selected, choose range
      conditionalPanel(
        condition="input.tabs == 'summary' || input.tabs == 'map'",
        conditionalPanel(
          condition="input.timespan == 'sing.yr'",
          sliderInput("year", "Select Year",
                      min=1999, max=2011, value=2011,
                      format="####")),
        conditionalPanel(
          condition="input.timespan == 'mult.yrs'",
          sliderInput("range", "Select Years",
                      min=1999, max=2011, value=c(2010,2011),
                      format="####")
        )
      ),
      
      ## in summary or plot, allow for county selection
      conditionalPanel(
        condition="input.tabs == 'summary' || input.tabs == 'plot'",
        selectInput("county", "Select County", 
                    names(table(suidata[,1]))[c(1:7, 9:12,14)], 
                    multiple=TRUE)),
      
      ## if a county is selected, show boxes that will compare to MA or US average
      conditionalPanel(
        condition="input.tabs == 'summary' || input.tabs == 'plot' && input.county != null",
        checkboxInput("meanMA", "Compare to MA Average", FALSE),
        checkboxInput("meanUS", "Compare to US Average", FALSE)),
      
      tags$hr(),
      
      ## author line
      helpText("Created by Sophie E O'Brien and Stephen A Lauer"),
      
      ## email feedback link
      helpText(a("Send us your comments or feedback!", href="mailto:seigmateam@gmail.com", 
                 target="_blank")),
      
      ## data source citation
      helpText(a("Data Source: CDC Wonder", href="http://wonder.cdc.gov/wonder/help/cmf.html",
                 target="_blank")),
      
      ## GitHub link
      helpText(a("View our data and code on GitHub", 
                 href="https://github.com/sEigmA/SEIGMA/tree/gh-pages/suicide", target="_blank"))
    ),
    
    ## create main panel
    mainPanel(
      ## put in logo for title
      a(img(src = "logo.jpg", height=105, width=920), href="http://www.umass.edu/seigma/"),
      
      ## create tabs
      tabsetPanel(
        ## summary tab
        tabPanel("Summary", 
                 dataTableOutput("summary"), value="summary", 
                 tags$style(type="text/css", '#summary tfoot {display:none;}')),
        ## plot tab with google chart options
        tabPanel("Plot",
                 ## make chart title here (otherwise not centered)
                 h4("Crude Suicide Rate Over Time (per 100,000 population)", align="center"),
                 ## make line chart
                 googleLineChart("plot", width="100%", height="475px", options = list(
                   
                   ## set fonts
                   fontName = "Source Sans Pro",
                   fontSize = 14,
                   
                   ## set axis titles, ticks, fonts, and ranges
                   hAxis = list(
                     title = "Year",
                     format = "####",
                     ticks = seq(1999, 2011, 2),
                     viewWindow = xlim,
                     textStyle = list(
                       fontSize = 14),
                     titleTextStyle = list(
                       fontSize = 16,
                       bold = TRUE,
                       italic = FALSE)
                   ),
                   vAxis = list(
                     title = "Crude Suicide Rate (per 100,000 population)",
                     viewWindow = ylim,
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
                   
                   # set tooltip font size
                   tooltip = list(
                     textStyle = list(
                       fontSize = 14
                     )
                   )
                 )),
                 
                 ## add text about the variables
                 p(strong("Variable Summary:")),
                 tags$br(),
                 p(strong("Suicides"),
                   " - Number of suicides for a specified region in a specific year. Due to confidentiality constraints, sub-national death counts and rates are suppressed when the number of deaths is less than 10."), 
                 tags$br(),
                 p(strong("Crude Rate"), 
                   " - Crude rates are expressed as the number of suicides, per 100,000 persons, reported each calendar year for the region you select. Rates are considered 'unreliable' when the death count is less than 20 and thus are not displayed. This is calculated by:"),
                 tags$br(),
                 p(strong("Crude Rate = Count / Population * 100,000"), align="center"),
        value="plot"),
      
      ## plot map
      tabPanel("Map", plotOutput("map"),
               
               ## add text about the variables
               p(strong("Variable Summary:")),
               tags$br(),
               p(strong("Suicides"),
                 " - Number of suicides for a specified region in a specific year. Due to confidentiality constraints, sub-national death counts and rates are suppressed when the number of deaths is less than 10."), 
               tags$br(),
               p(strong("Crude Rate"), 
                 " - Crude rates are expressed as the number of suicides, per 100,000 persons, reported each calendar year for the region you select. Rates are considered 'unreliable' when the death count is less than 20 and thus are not displayed. This is calculated by:"),
               tags$br(),
               p(strong("Crude Rate = Count / Population * 100,000"), align="center"),
      value="map"),
    
    tabPanel("More Info", 
             p(strong("Variable Summary:")),
             tags$br(),
             p(strong("Suicides"),
               " - Number of suicides for a specified region in a specific year. Due to confidentiality constraints, sub-national death counts and rates are suppressed when the number of deaths is less than 10."), 
             tags$br(),
             p(strong("Crude Rate"), 
               " - Crude rates are expressed as the number of suicides, per 100,000 persons, reported each calendar year for the region you select. Rates are considered 'unreliable' when the death count is less than 20 and thus are not displayed. This is calculated by:"),
             tags$br(),
             p(strong("Crude Rate = Count / Population * 100,000"), align="center"),
             tags$br(),
             p(strong("Crude Rate Lower Bound"),
               " - 95% confidence interval lower bound based upon the Crude Rate Standard Error (see below)."),
             tags$br(),
             p(strong("Crude Rate Upper Bound"),
               " - 95% confidence interval upper bound based upon the Crude Rate Standard Error (see below)."),
             tags$br(),
             p(strong("Crude Rate Standard Error"),
               " - The relative standard error for Crude Rate. Even though Suicides represents the complete counts for each region, and thus are not subject to sampling error, they are subject to non-sampling errors in the registration process. This is calculated by:"),
             tags$br(),
             p(strong("Crude Rate Standard Error = 100 / sqrt(Suicides)."), align="center")),
    id="tabs"
  )
)
)
))