#######################################
## Title: Suicide ui.R               ##
## Author(s): Emily Ramos, Arvind    ##
##            Ramakrishnan, Jenna    ##
##            Kiridly, Steve Lauer   ## 
## Date Created:                     ##
## Date Modified: 10/22/2014         ##
#######################################

## load necessary libraries
require(shiny)
require(googleCharts)


shinyUI(fluidPage(
  ## HTML to create generate map button
  HTML('<style type="text/css">
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
       
       </style>'),
  ## this starts the googleCharts engine
  googleChartsInit(),
  
  ## blank title, but put in a special title for window tab
  titlePanel("", windowTitle = "SEIGMA: Suicide Shiny App"),
  
  ## Create sidebar
  sidebarLayout(
    sidebarPanel(
      ## Conditional panel means if the condition is met show all text below otherwise Don't!
      conditionalPanel(
        condition="input.tabs == 'summary'",
        ## h4 created 4th largest header
        h4("How to use this app:"),
        ## Creates text
        p(strong(helpText('Please select a timespan for which you are interested in seeing suicide data organized by county.'))),
        ##tags$ul and tags$li are to create bullet points using HTML  
        tags$ul(
          tags$li("If you are interested in comparing multiple years, select Multiple Years, adjust the slider, and select a range accordingly."),
          tags$br(),
          tags$li("If you are interested in a specific county or multiple counties select them by clicking on the box or typing them in."), 
          p(strong('For data on all Massachusetts counties, leave this selection blank.')),
          tags$li("To compare the data to the Massachusetts average or US average, select the corresponding check box."),
          p(strong('Please note that only consecutive year ranges can be selected'))
        )),
      
      ## Same concept
      conditionalPanel(
        condition="input.tabs == 'plot'",
        h4("How to use this app:"),
        p(strong(helpText('Please select a county to analyze.'))),
        tags$ul(
          tags$li("Multiple counties can be selected to compare the plots of crude suicide rate over time."),
          tags$br(),
          tags$li("Select the Massachusetts and/or US average check boxes to compare county rates with the national and state averages.")
        )
      ),
      
      conditionalPanel(
        condition="input.tabs == 'map'",
        h4("How to use this app:"),
        p(strong(helpText('Please click on "Generate Map" to get started.'))),
        tags$br(),
        tags$ul(
          tags$li('When "Single Year" is selected, clicking on a county displays the crude suicide rate for that year.'),
        tags$li('When "Multiple Years" is selected, clicking on a county displays the increase in crude suicide rate over that timespan.')
        )
      ),
      
      
      conditionalPanel(
        condition="input.tabs == 'info'",
        h4("How to use this app:"),
        helpText(p(strong('This tab contains more detailed information regarding the variables of interest, including:'))),
  tags$br(),
  tags$ul(
  tags$li('formulae'),
  tags$li('calculations to derive the crude suicide rate.')
  )
  ),
  
      
      ## in summary and map, allow for timespan selection
      conditionalPanel(
        condition="input.tabs == 'summary' || input.tabs == 'map'",
        ## Select input = Drop down list of timespan (variable name on server side) 
        selectInput("timespan", "Select Timespan",
                    list("Single Year" = "sing.yr",
                         "Multiple Years" = "mult.yrs"))
      ),
      
      ## if single year is selected, select year. if                  multiple years are selected, choose range.
      conditionalPanel(
        condition="input.tabs == 'summary' || input.tabs == 'map'",
        conditionalPanel(
          condition="input.timespan == 'sing.yr'",
          
          ## Initializing a single slider
          sliderInput("year", "Select Year",
                      min=1999, max=2011, value=2011,
                      format="####")),
        conditionalPanel(
          ## Initializes a multi-year slider (range)
          condition="input.timespan == 'mult.yrs'",
          ## Slider starts from 2010-2011
          sliderInput("range", "Select Years",
                      min=1999, max=2011, value=c(2010,2011),
                      format="####")
        )
      ),
      
      ## in summary or plot, allow for county selection
      conditionalPanel(
        condition="input.tabs == 'summary' || input.tabs == 'plot'",
        ## Select input = List
        selectInput("county", "Select County", 
                    names(table(suidata[,1]))[c(1:7, 9:12,14)], 
                    ## Multiple allows for multi-county selection
                    multiple=TRUE)),
      
      ## If a county is selected, show boxes that will compare to MA or US average
      conditionalPanel(
        condition="input.tabs == 'summary' || input.tabs == 'plot' && input.county != null",
        ## False at the end means it starts off unchecked
        checkboxInput("meanMA", "Compare to MA Average", FALSE),
        checkboxInput("meanUS", "Compare to US Average", FALSE)),
      
      tags$hr(),
      
      ## author line
      helpText("Created by Emily R. Ramos, Arvind Ramakrishnan, Jenna F Kiridly, Sophie E. O'Brien and Stephen A. Lauer"),
      
      ## email feedback link
      ## To develop a link in HTML
      helpText(a("Send us your comments or feedback!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank")),
      
      ## data source citation
      helpText(a("Data Source: CDC Wonder", href="http://wonder.cdc.gov/wonder/help/cmf.html",
                 target="_blank")),
      
      ## GitHub link
      helpText(a("View our data and code on GitHub", 
                 href="https://github.com/sEigmA/SEIGMA/tree/gh-pages/suicide", target="_blank")),
      
      helpText("If using Internet Explorer, application only visible in version 10.")
    ),
    
    ######### End of Sidebar  #########
    
    ######### Start of Main Panel #####
    
    mainPanel(
      ## put in logo for title
      a(img(src = "logo.jpg", height=105, width=920), href="http://www.umass.edu/seigma/"),
      
      ## create tabs
      tabsetPanel(
        tabPanel("About", 
                 ## strong=bold, p=paragraph, em=emboss/italicised or bold italicized, 
                 p(strong("The SEIGMA Suicide App"), "displays the crude suicide rate for Massachusetts by counties for a given year or multiple years from 1999 to 2011. Toggle between tabs to visualize the data differently. ",
                   tags$br(),
                   ##tags$ul and tags$li are to create bullet points using HTML
                   tags$ul(
                     tags$li(p(strong("Summary"), "shows the source data in a table format.")),
                     tags$li(p(strong("Plot"), "shows crude suicide rate over time per 100,000 in each county population, Massachusetts average and U.S. average.")),
                     tags$li(p(strong("Map"), "visually displays crude suicide rate comparatively by county.")),
                     tags$li(p(strong("More Info"), "lists descriptions for the variables of interest, including formulas and calculations.")
                   )))),
        
        ## summary tab
        tabPanel("Summary", 
                 dataTableOutput("summary"),
                 tags$br(),
                 tags$ul(
                   tags$li(p(strong(em('NA'),"-Rates are considered 'unreliable' when the death count is less than 20 and thus are not displayed. This is calculated by:")))
                 ),
                 tags$br(),
                 p(strong("Crude Rate = Count / Population * 100,000"), align="center"),
                 value="summary", 
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
                   
                   ## set tooltip font size
                   ## Hover text font stuff
                   tooltip = list(
                     textStyle = list(
                       fontSize = 14
                     )
                   )
                 )),
                 
                 ## add text about the variables
                 p(strong("Variable Summary:")),
                 ## breaks between paragraphs
                 tags$br(),
                 p(strong("Suicides"),
                   " - Number of suicides for a specified region in a specific year.Due to confidentiality constraints, sub-national death counts and rates are suppressed when the number of deaths is less than 10."), 
                 tags$br(),
                 p(strong("Crude Rate"), 
                   " - Crude rates are expressed as the number of suicides, per 100,000 persons, reported each calendar year for the region you select. Rates are considered 'unreliable' when the death count is less than 20 and thus are not displayed. This is calculated by:"),
                 tags$br(),
                 p(strong("Crude Rate = Count / Population * 100,000"), align="center"),
                 value="plot"),
        
        ## plot map
        tabPanel("Map",
                 
                 ## Add a little CSS to make the map background pure white
                 tags$head(tags$style("
                                      #showcase-code-position-toggle, #showcase-sxs-code { display: none; }
                                      .floater { background-color: white; padding: 8px; opacity: 1; border-radius: 6px; box-shadow: 0 0 15px rgba(0,0,0,0.2); }
                                      ")),
                 ## Map Creation
                 leafletMap("map", width="100%", height=500, 
                            options=list(center = c(42.15, -71.65), zoom=8, 
                                         ##Bounds for the map for when zoomed in on mass
                                         maxBounds = list(list(41, -73.5), 
                                                          list(43, -70)))),
                 ## Info Box 
                 conditionalPanel(
                   condition="input.action != 0",
                   absolutePanel(left=450, top=450, width=300, class="floater",
                                 htmlOutput("details"))),
                 
                 conditionalPanel(
                   condition="input.tabs == 'map' && input.action == 0",
                   ## within the map area, you can create an action button.  similar to initializing the legend but just putting a button instead.
                   absolutePanel(right = 400, top = 300, class = "floater",
                                 actionButton("action", "Generate Map")
                   )),
                 
                 ## Single Year Legend
                 conditionalPanel(
                   condition="input.timespan == 'sing.yr' && input.action != 0",
                   absolutePanel(
                     right = 30, top = 215, draggable=FALSE, style = "", 
                     class = "floater",
                     strong("Single Year"),
                     tags$br(),
                     strong("Crude Suicide Rate"),
                     tags$table(
                       mapply(function(from, to, color) {
                         tags$tr(
                           tags$td(tags$div(
                             style = sprintf("width: 16px; height: 16px; background-color: %s;", color)
                           )),
                           tags$td(round(from, 2), "to", round(to, 2))
                         )
                       }, 
                       scolorRanges$from, scolorRanges$to, smap.colors[-length(smap.colors)],
                       SIMPLIFY=FALSE)
                     )
                   )),
                 
                 ## Multi Year Legend
                 conditionalPanel(
                   condition="input.timespan == 'mult.yrs' && input.action != 0",
                   absolutePanel(
                     right = 30, top = 215, draggable=FALSE, style = "", 
                     class = "floater",
                     strong("Multiple Year"),
                     tags$br(),
                     strong("Increase in CSR"),
                     tags$table(
                       mapply(function(from, to, color) {
                         tags$tr(
                           tags$td(tags$div(
                             style = sprintf("width: 16px; height: 16px; background-color: %s;", color)
                           )),
                           tags$td(round(from, 2), "to", round(to, 2))
                         )
                       }, 
                       mcolorRanges$from, mcolorRanges$to, mmap.colors[-length(mmap.colors)],
                       SIMPLIFY=FALSE)
                     )
                   )),
                 
                 ## Data not available box
                 conditionalPanel(
                   condition="input.action != 0",
                   absolutePanel(
                     right = 350, top = 600, draggable=FALSE, style = "", 
                     class = "floater",
                     tags$table(
                       tags$tr(
                         tags$td(tags$div(
                           style = sprintf("width: 16px; height: 16px; background-color: %s;", "#999999")
                         )),
                         tags$td("Data not available")
                       )
                     )
                   )),
                 
                 ## Add text about the variables
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