#######################################
## Title: Income ui.R                ##
## Author(s): Emily Ramos, Arvind    ##
##            Ramakrishnan, Jenna    ##
##            Kiridly, Steve Lauer   ## 
## Date Created:  11/5/2014          ##
## Date Modified: 11/7/2014          ##
#######################################

shinyUI(fluidPage(
  ## HTML to create generate map button
  gen_map_button,
  ## this starts the googleCharts engine
  googleChartsInit(),
  
  ## blank title, but put in a special title for window tab
  titlePanel("", windowTitle = "SEIGMA: Household Income Shiny App"),
  
  ## Create sidebar
  sidebarLayout(
    sidebarPanel(width=4,
      ## Conditional panel means if the condition is met show all text below otherwise Don't!
      summary_side_text,
      
      plot_side_text,
      
      map_side_text,
      
      info_side_text,
      
      ## in map, allow for variable selection
      
      ## Choose range for year.
      ## Initializing a single slider
      conditionalPanel(
        condition="input.tabs == 'summary' || input.tabs == 'plot' || input.tabs == 'map'",
      selectInput("year", "Select Five Year Range",
                  choices = list("2005-2009" = "2005-2009", "2006-2010" = "2006-2010",
                                 "2007-2011" = "2007-2011"))
      ),
      

      ## in summary, allow for municipal selection
      conditionalPanel(
        condition="input.tabs == 'summary'",
        ## Select input = List
        selectInput("sum_muni", "Select Municipality", 
                    choices = MA_municipals,
                    ## Multiple allows for multi-county selection
                    multiple=TRUE)),
      
      ## in plot, allow for municipal selection
      conditionalPanel(
        condition="input.tabs == 'plot'",
        ## Select input = List
        selectInput("plot_muni", "Select Municipality", 
                    choices = MA_municipals)),
      
      ## In summary, show boxes that will compare to MA or US average
      conditionalPanel(
        condition="input.tabs == 'summary'",
        ## False at the end means it starts off unchecked
        checkboxInput("MA_mean", "Compare to MA Average", FALSE),
        checkboxInput("US_mean", "Compare to US Average", FALSE)
      ),
      
      tags$hr(),
      
      ## author line
      helpText("Created by Emily R. Ramos, Arvind Ramakrishnan, Jenna F. Kiridly, Xuelian Li, Sophie E. O'Brien and Stephen A. Lauer"),
      
      ## email feedback link
      ## To develop a link in HTML
      helpText(a("Send us your comments or feedback!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank")),
      
      ## data source citation
      helpText(a("Data Source: American Community Survey", href="http://www.census.gov/acs/www/",
                 target="_blank")),
      
      ## GitHub link
      helpText(a("View our data and code on GitHub", 
                 href="https://github.com/sEigmA/SEIGMA/tree/gh-pages/income", target="_blank")),
      
      helpText("If using Internet Explorer, application only visible in version 10.")
    ),

######### End of Sidebar  #########
    
######### Start of Main Panel #####
    
bootstrapPage(mainPanel(
      ## put in logo for title
      a(img(src = "logo.jpg", height=105, width=920), href="http://www.umass.edu/seigma/"),
      
      ## create tabs
      tabsetPanel(
        tabPanel("About", 
                 ## strong=bold, p=paragraph, em=emboss/italicised or bold italicized, 
                 about_main_text, value="about"),
        
        ## summary tab
        tabPanel("Summary", 
                 dataTableOutput("summary"), value="summary", 
                 tags$style(type="text/css", '#summary tfoot {display:none;}')),
        
        ## plot tab with google chart options
        tabPanel("Plot",
                 ## make chart title here (otherwise not centered)
                 h4("Median Household Income (Inflation-Adjusted Dollars) by Region", align="center"),
                 ## make a row to put two charts in
                 
                 googleColumnChart("plot", width="100%", height="475px", options = list(
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
                     title = "Median Household Income",
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
                     position = "none"),
                   
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
                 )),
                 ## add text about the variables
#                  plot_main_text,
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
                 
                 ## Legend
                 conditionalPanel(
                   condition="input.action != 0",
                   absolutePanel(
                     right = 30, top = 215, draggable=FALSE, style = "", 
                     class = "floater",
#                      strong("Crude Suicide Rate"),
                     tags$table(
                       mapply(function(from, to, color) {
                         tags$tr(
                           tags$td(tags$div(
                             style = sprintf("width: 16px; height: 16px; background-color: %s;", color)
                           )),
                           tags$td("$", prettyNum(round(from), big.mark = ","), "to", "$", 
                                   prettyNum(round(to), big.mark = ","), align = "right")
                         )
                       }, 
                       colorRanges$from, colorRanges$to, map_colors[-length(map_colors)],
                       SIMPLIFY=FALSE),
                       tags$td(tags$div(
                         style = sprintf("width: 16px; height: 16px; background-color: %s;", "#999999")
                       )),
                       tags$td("Data not available", align = "right")
                     )
                   )),
                 
#                plot_main_text,
                 value="map"),
        
        tabPanel("More Info", 
                 p(strong("Variable Summary:")),
                 tags$br(),
                 tags$ul(
                   tags$li(p(strong("Average Annual Household Income"), "This includes the income of the householder and all other individuals ages 15 and over. Average annual household income provides a clear trend to assess resident's household income overtime. Annual data for median household income was collected for a ten-year time series, from 2002- 2012, the latest data available. Data was collected at multiple levels to allow for analysis at multiple levels; municipality, state, and US level comparatively.")),
                   tags$br(),
                   tags$li(p(strong("Median"),"This represents the middle value or the average of the two middle values (if 'n' is  even) in an ordered list of 'n' data values.  The median divides the distribution of average annual household income into two equal parts; half that fall below the median for average household income and half that fall above the average annual household income.")),
                   tags$br(),
                   tags$li(p(strong("Five- Year Percentages"),"Survey information collected everyday of the year and then aggregated over a specific time period, five years.  Multiyear estimates are available to regions with populations less than 65,000.  However, large geographic regions also benefit from increased sample, as this results in more precise estimates.")
                   )
                   ),
                 #tags$br(),
                # p("SEIGMA. Social and Economic Impacts of Gambling in Massachusetts, University of Massachusetts School of Public Health and Health Sciences. (2014). Report on the Social and Economic Impact of Gambling in Massachusetts SEIGMA Gambling study. Report to the Massachusetts Gaming Commission & the Massachusetts department of Public Health. Retrieved from:"), a("http://www.umass.edu/seigma/sites/default/files/March%202014%20SEIGMA%20Report_6-19_for%20website.pdf"),
                 
                 
                 
              
                  
                 
                 ## email feedback link
                 h3(a("Please fill out our survey to help improve the site!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank")), value="info"),
        id="tabs"   
    ))
  ))
))