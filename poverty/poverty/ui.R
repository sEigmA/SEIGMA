########################################
## Title: Poverty ui.R                ##
## Author(s): Emily Ramos, Xuelian Li ##
##            Arvind Ramakrishnan,    ##
##            Jenna Kiridly           ## 
## Date Created:  02/25/2015          ##
## Date Modified: 03/05/2015 ER       ##
########################################

shinyUI(fluidPage(
  ## embed the google analytics script in the app
  tags$head(includeScript("google-analytics.js")),
  ## HTML to create generate map button
  gen_map_button,
  ## this starts the googleCharts engine
  googleChartsInit(),
  
  ## blank title, but put in a special title for window tab
  titlePanel("", windowTitle = "SEIGMA: Poverty Rate App"),
  
  ## Create sidebar
  sidebarLayout(
    sidebarPanel(width=4,
                 ## Conditional panel means if the condition is met show all text below otherwise Don't!
                 summary_side_text,
                 
                 plot_side_text,
                 
                 map_side_text,
                 
                 info_side_text,
                              
                 
                 ## in summary, allow for year, municipal selection
                 conditionalPanel(
                   condition="input.tabs == 'summary'",
                   selectInput("sum_year", "Select Five Year Range",
                               choices = list("2005-2009" = "2005-2009", "2006-2010" = "2006-2010", "2007-2011" = "2007-2011", "2008-2012" = "2008-2012")),
                   selectInput("sum_muni", "Select Municipality", 
                               choices = MA_municipals,
                               ## Multiple allows for multi-county selection
                               multiple=TRUE),
                   ##show boxes that will compare to MA or US average
                   ## False at the end means it starts off unchecked
                   checkboxInput("MA_mean", "Compare to MA Average", FALSE),
                   checkboxInput("US_mean", "Compare to US Average", FALSE)
                 ),
                 
                 ## in plot, allow for municipal selection
                 conditionalPanel(
                   condition="input.tabs == 'plot'",
                   selectInput("plot_year", "Select Five Year Range",
                               choices = list("2005-2009" = "2005-2009", "2006-2010" = "2006-2010", "2007-2011" = "2007-2011", "2008-2012" = "2008-2012")),
                   ## Select input = List
                   selectInput("plot_muni", "Select Municipality", 
                               choices = MA_municipals)),
                 
                 ## in map, allow for year selection
                 conditionalPanel(
                   condition="input.tabs == 'map'",
                   selectInput("map_year", "Select Five Year Range",
                               choices = list("2005-2009" = "2005-2009", "2006-2010" = "2006-2010","2007-2011" = "2007-2011", "2008-2012" = "2008-2012"))
                   ),
                 
                
                 tags$hr(),
                 
                 ## author line
                 helpText("Created by Emily R. Ramos, Arvind Ramakrishnan, Jenna F. Kiridly and Xeulian Li "),
      
      ## email feedback link
      ## To develop a link in HTML
     helpText(a("Send us your comments or feedback!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank",onclick="ga('send', 'event', 'click', 'link', 'feedback', 1)")),
      
      ## data source citation
      helpText(a("Data Source: American Community Survey", href="http://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_13_1YR_S1701&prodType=table",
                 target="_blank", onclick="ga('send', 'event', 'click', 'link', 'dataSource', 1)")),
      
      ## GitHub link
      helpText(a("View our data and code on GitHub", 
                 href="https://github.com/sEigmA/SEIGMA/tree/gh-pages/poverty", target="_blank",onclick="ga('send', 'event', 'click', 'link', 'code', 1)")),
      
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
                 h4("Poverty Rate by Region Over Selected Five Year Period", align="center"),
                 ## make a row to put two charts in
                 
                 googleColumnChart("plot", width="100%", height="475px", options = list(
                   ## set fonts
                   fontName = "Source Sans Pro",
                   fontSize = 14,
                   title = "",
                   ## set axis titles, ticks, fonts, and ranges
                   hAxis = list(
                     title = "",
                     textStyle = list(
                       fontSize = 14),
                     titleTextStyle = list(
                       fontSize = 14+2,
                       bold = TRUE,
                       italic = FALSE)
                   ),
                   vAxis = list(
                     title = "Poverty Rate",
                     viewWindow = ylim,
                     textStyle = list(
                       fontSize = 14),
                     titleTextStyle = list(
                       fontSize = 14+2,
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
                   colors = c("purple", "black"),
                          
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
             absolutePanel(left=100, top=450, width=300, class="floater",
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
               right = 10, top = 150, draggable=FALSE, style = "", 
               class = "floater",
               strong("Poverty Rate"),
               plotOutput("legend1"),
               tags$table(
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
             tags$li(p(strong("Poverty Status"), "-To determine a person's poverty status, one compares the person’s total family income in the last 12 months with the poverty threshold appropriate for that person's family size and composition. If the total income of that person's family is less than the threshold appropriate for that family, then the person is considered below the poverty level. Poverty is defined at the family level and not the household level, the poverty status of
the household is determined by the poverty status of the householder.")),
             tags$br(),
             tags$li(p(strong("Poverty Threshold"), "-Poverty thresholds are determined by multiplying base year thresholds (set as yearly income in 1982) by the monthly inflation factor for the 12- months before the period of interest.  For example, if we want to determine if a family with three children with a total income of $14,000 (July 2012- June 2013) was at or below the poverty threshold we would do the following. First we would see what the base year threshold was in 1982 for a family of this size, $7,765. We then multiply this by the inflation rate for the 12 month period before June 2012- July 2013, 2.39719.  This would give us a threshold income of $18,614.  When we compare this to the income of the family above, $14,000 is well below the threshold we calculated, meaning this family woudl be considered to be in poverty.  In this app we calculate the poverty threshold as described in the example above as being $23,268." )),
             tags$br(),
             tags$li(p(strong("Five-Year Estimates"), "-Survey information is collected everyday of the year and then aggregated over a specific time period, five years.  Multiyear estimates are available to regions with populations less than 65,000.  However, more precise estimates, are possible for larger municipalities. To analyze change over time, users are dicouraged from utilizing overlapping multi-year estimates (e.g. 2005-2009, 2006-2010) due to the inability to isolate change with precision." ))),
           
           #tags$br(),
           # tags$li(p(strong("Median Household Income  (MHI)"),
           #           " : Average annual median household income in inflation-adjusted dollars over a five-year period for each municipality")),
           #  tags$br(),
           # tags$li("When analyzing data sets per municipality five- year sets are used because estimates for smaller regions require a larger sample size than can be provided by single year data.")
           
           #tags$br(),
           # p("SEIGMA. Social and Economic Impacts of Gambling in Massachusetts, University of Massachusetts School of Public Health and Health Sciences. (2014). Report on the Social and Economic Impact of Gambling in Massachusetts SEIGMA Gambling study. Report to the Massachusetts Gaming Commission & the Massachusetts department of Public Health. Retrieved from:"), a("http://www.umass.edu/seigma/sites/default/files/March%202014%20SEIGMA%20Report_6-19_for%20website.pdf"),       
           
           ## email feedback link
           h3(a("Please fill out our survey to help improve the site!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank")), value="info"),
  id="tabs"
)
))
)
))