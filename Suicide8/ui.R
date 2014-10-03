require(shiny)
require(googleCharts)
require(leaflet)

suidata <- read.csv(file="SASuicidedata.csv")[,-1]
xlim <- list(
  min = min(suidata$Year)-1,
  max = max(suidata$Year)+1
)
ylim <- list(
  min = 0,
  max = max(suidata$Crude.Rate, na.rm=T)+5
)

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                "#0072B2", "#D55E00", "#CC79A7")

shinyUI(fluidPage(
  #googleChartsInit(),
  
  titlePanel(""),
  #headerPanel(a(img(src = "logo.jpg", height=105, width=920), href="http://www.umass.edu/seigma/")),
  sidebarLayout(
    sidebarPanel(
      
      
      conditionalPanel(
        condition="input.tabs == 'summary' || input.tabs == 'map'",
        selectInput("timespan", "Select Timespan",
                    list("Single Year" = "sing.yr",
                         "Multiple Years" = "mult.yrs"))
      ),
      
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
      
      conditionalPanel(
       condition="input.tabs == 'map'",
       actionButton("gen", "Generate Map")
      ),
      
      conditionalPanel(
        condition="input.tabs == 'summary' || input.tabs == 'plot'",
        selectInput("county", "Select County", 
                    names(table(suidata[,1]))[c(1:7, 9:12,14)], 
                    multiple=TRUE)),
      
      conditionalPanel(
        condition="input.tabs == 'summary' || input.tabs == 'plot' && input.county != null",
        checkboxInput("meanMA", "Compare to MA Average", FALSE),
        checkboxInput("meanUS", "Compare to US Average", FALSE)),
      
      tags$hr(),
      
      helpText("Created by Emily R. Ramos, Arvind Ramakrishnan, Jenna F Kiridly, Sophie E. O'Brien and Stephen A. Lauer"),
      
      helpText(a("Send us your comments or feedback!", href="mailto:slauer@schoolph.umass.edu", target="_blank")),
      
      helpText(a("Data Source: CDC Wonder", href="http://wonder.cdc.gov/wonder/help/cmf.html", target="_blank")),
      
      helpText("Note: If using Internet Explorer, application only visible in version 10.")
      
    ),
    
    mainPanel(
      a(img(src = "logo.jpg", height=105, width=920), href="http://www.umass.edu/seigma/"),
      tabsetPanel(
       tabPanel("Map",
                leafletMap("map", width="100%", height=500, 
                           options=list(center = c(42.15, -71.65), zoom=8)),
                htmlOutput("details"), 
                value="map"),
        tabPanel("Summary", 
                 dataTableOutput("summary"), value="summary", 
                 tags$style(type="text/css", '#summary tfoot {display:none;}')),
        tabPanel("Plot", 
                 h4("Crude Suicide Rate Over Time (per 100,000 population)", align="center"),
                 googleLineChart("plot", width="100%", height="475px", options = list(
                   fontName = "Source Sans Pro",
                   fontSize = 14,
                   # Set axis labels and ranges
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
                   # Set legend fonts
                   legend = list(
                     textStyle = list(
                       fontSize=14)),
                   # Set chart area padding
                   chartArea = list(
                     top = 50, left = 75,
                     height = "75%", width = "75%"
                   ),
                   # Set colors
                   colors = cbbPalette,
                   ## Set title fonts
                   #title = "Crude Suicide Rate Over Time (per 100,000 population)",
                   #titleTextStyle = list(
                  #   fontSize = 18,
                  #   bold = TRUE
                  # ),
                   pointSize = 3,
                   tooltip = list(
                     textStyle = list(
                       fontSize = 14
                     )
                   )
                 )),
                 p(strong("Variable Summary:"),
                   tags$br(),
                   strong("Suicides"),
                   " - Number of suicides for a specified region in a specific year. Due to confidentiality constraints, sub-national death counts and rates are suppressed when the number of deaths is less than 10.", 
                   tags$br(),
                   strong("Crude Rate"), 
                   " - Crude rates are expressed as the number of suicides, per 100,000 persons, reported each calendar year for the region you select. Rates are considered 'unreliable' when the death count is less than 20 and thus are not displayed. This is calculated by:",
                   tags$br(),
                   p("Crude Rate = Count / Population * 100,000", align="center")),
                   value="plot"),
        
        tabPanel("More Info", 
                 p(strong("Variable Summary:"),
                   tags$br(),
                   strong("Suicides"),
                   " - Number of suicides for a specified region in a specific year. Due to confidentiality constraints, sub-national death counts and rates are suppressed when the number of deaths is less than 10.", 
                   tags$br(),
                   strong("Crude Rate"), 
                   " - Crude rates are expressed as the number of suicides, per 100,000 persons, reported each calendar year for the region you select. Rates are considered 'unreliable' when the death count is less than 20 and thus are not displayed. This is calculated by:",
                   tags$br(),
                   "Crude Rate = Count / Population * 100,000",
                   tags$br(),
                   strong("Crude Rate Lower Bound"),
                   " - 95% confidence interval lower bound based upon the Crude Rate Standard Error (see below).",
                   tags$br(),
                   strong("Crude Rate Upper Bound"),
                   " - 95% confidence interval upper bound based upon the Crude Rate Standard Error (see below).",
                   tags$br(),
                   strong("Crude Rate Standard Error"),
                   " - The relative standard error for Crude Rate. Even though Suicides represents the complete counts for each region, and thus are not subject to sampling error, they are subject to non-sampling errors in the registration process. This is calculated by:",
                   tags$br(),
                   "Crude Rate Standard Error = 100 / sqrt(Suicides).")),
        id="tabs"
      )
    )
  )
))