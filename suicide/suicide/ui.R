###############################
## Preamble
###############################

## load necessary libraries
require(shiny)
require(googleCharts)

shinyUI(fluidPage(
  ## this starts the googleCharts engine
  googleChartsInit(),
  
  ## blank title, but put in a special title for window tab
  titlePanel("", windowTitle = "SEIGMA Shiny App: Suicide"),
  
  ## create sidebar
  sidebarLayout(
    sidebarPanel(
      
      conditionalPanel(
        condition="input.tabs == 'summary'",
        h4("How to use this app:"),
        helpText('Please select a timespan for which you are interested in seeing suicide data organized by county.  If you are interested in comparing multiple years, select "Multiple Years" and adjust the slider and select a range accordingly.'),
        helpText('Next, if you are interested in a specific county or multiple counties select them; alternatively for data on all Massachusetts counties leave this selection blank.  To compare the data to the Massachusetts average or US average select the corresponding check box.  Please note that only consecutive year ranges can be selected.')
      ),
      
      conditionalPanel(
        condition="input.tabs == 'plot'",
        h4("How to use this app:"),
        helpText('Please select a county to analyze.  Multiple counties can be selected to compare the plots of crude suicide rate over time.  Select the Massachusetts and/or US average check boxes to compare county rates with the national and state averages.')
      ),
      
      conditionalPanel(
        condition="input.tabs == 'map'",
        h4("How to use this app:"),
        helpText('Please click on "Generate Map" to get started. When "Single Year" is selected, clicking on a county displays the crude suicide rate for that year. When "Multiple Years" is selected, clicking on a county displays the increase in crude suicide rate over that timespan.')
      ),
      
      conditionalPanel(
        condition="input.tabs == 'info'",
        h4("How to use this app:"),
        helpText('This tab contains more detailed information regarding the variables of interest, including formulae and calculations which were used to derive the crude suicide rate.')
      ),
      
      tags$hr(),
      
      conditionalPanel(
        condition="input.tabs == 'map' && input.action == 0",
        actionButton("action", "Generate Map"),
        tags$hr()
      ),
      
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
      helpText("Created by Emily R. Ramos, Arvind Ramakrishnan, Jenna F Kiridly, Sophie E. O'Brien and Stephen A. Lauer"),
      
      ## email feedback link
      helpText(a("Send us your comments or feedback!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank")),
      
      ## data source citation
      helpText(a("Data Source: CDC Wonder", href="http://wonder.cdc.gov/wonder/help/cmf.html",
                 target="_blank")),
      
      ## GitHub link
      helpText(a("View our data and code on GitHub", 
                 href="https://github.com/sEigmA/SEIGMA/tree/gh-pages/suicide", target="_blank")),
      
      helpText("If using Internet Explorer, application only visible in version 10.")
    ),
    
    ## create main panel
    mainPanel(
      ## put in logo for title
      a(img(src = "logo.jpg", height=105, width=920), href="http://www.umass.edu/seigma/"),
      
      ## create tabs
      tabsetPanel(
        tabPanel("About", 
                 p(strong("The SEIGMA Suicide App"), "displays the crude suicide rate for Massachusetts by counties for a given year or multiple years from 1999 to 2011. Toggle between tabs to visualize the data differently. ", em("Summary"), "shows the source data in a table format. ", em("Plot"), "shows crude suicide rate over time per 100,000 in each county population, Massachusetts average and U.S. average. ", em("Map"), "visually displays crude suicide rate comparatively by county. ", em("More Info"), "lists descriptions for the variables of interest, including formulas and calculations."), value="about"),
        
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
        tabPanel("Map", leafletMap("map", width="100%", height=500, 
                                   options=list(center = c(42.15, -71.65), zoom=8,
                                                maxBounds = list(list(41, -73.5), 
                                                                 list(43, -70)))),
                 htmlOutput("details"),
                 #                  absolutePanel(
                 #                    right = 20, top = 200, width = 150, class = "floater",
                 #                    
                 #                    #h4("Crude Suicide Rate"),
                 #                    uiOutput("details")
                 #                  ),
                 
                 
                 #                  absolutePanel(
                 #                    right = 30, top = 280, style = "", class = "floater",
                 #                    tags$table(
                 #                      mapply(function(from, to, color) {
                 #                        tags$tr(
                 #                          tags$td(tags$div(
                 #                            style = sprintf("width: 16px; height: 16px; background-color: %s;", color)
                 #                          )),
                 #                          tags$td(from, "-", to)
                 #                        )
                 #                      }, unlist(uiOutput("from")), unlist(uiOutput("to")), unlist(uiOutput("color")), SIMPLIFY=FALSE)
                 #                    )
                 #                  ),
                 
                 ## add text about the variables
                 tags$br(),
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
                 p(strong("Crude Rate Standard Error = 100 / sqrt(Suicides)."), align="center"),
                 ## email feedback link
                 h3(a("Please fill out our survey to help improve the site!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank")), value="info"),
        id="tabs"
      )
    )
  )
))