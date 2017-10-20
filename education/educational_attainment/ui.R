#######################################
## Title: Education ui.R             ##
## Author(s): Emily Ramos, Arvind    ##
##            Ramakrishnan, Jenna    ##
##            Kiridly, Xuelian Li    ## 
##            Steve Lauer            ##
## Date Created:  12/04/14           ##
## Date Modified: 04/04/15  XL       ##
## Data Updated: 10/20/2017 VE       ##
#######################################


shinyUI(fluidPage(
  ## embed the google analytics script in the app
  tags$head(includeScript("google-analytics.js")),
  ## HTML to create generate map button
  gen_map_button,
  ## this starts the googleCharts engine
  googleChartsInit(),
  
  ## blank title, but put in a special title for window tab
  titlePanel("", windowTitle = "SEIGMA: Educational Status Shiny App"),
  
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
                  choices = list("2006-2010" = "2006-2010", "2007-2011" = "2007-2011",
                                 "2008-2012" = "2008-2012", "2009-2013" = "2009-2013", 
                                 "2010-2014" = "2010-2014", "2011-2015" = "2011-2015")),
      selectInput("sum_muni", "Select Municipality", 
                  choices = MA_municipals,
                  ## Multiple allows for multi-county selection
                  multiple=TRUE),
      ## In summary, show boxes that will compare to MA or US average
      checkboxInput("MA_mean", "Compare to MA Average", FALSE),
      checkboxInput("US_mean", "Compare to US Average", FALSE)
      ),
      
          
      ## in plot, allow for year, municipal selection
      conditionalPanel(
        condition="input.tabs == 'plot'",
        selectInput("plot_year", "Select Five Year Range",
                    choices = list("2006-2010" = "2006-2010", "2007-2011" = "2007-2011",
                                   "2008-2012" = "2008-2012", "2009-2013" = "2009-2013",
                                   "2010-2014" = "2010-2014", "2011-2015" = "2011-2015")),
        ## Select input = List
        selectInput("plot_muni", "Select Municipality", 
                    choices = MA_municipals)),
      
      ## in map, allow for year, municipal selection
      conditionalPanel(
        condition="input.tabs == 'map'",
        selectInput("map_year", "Select Five Year Range",
                    choices = list("2006-2010" = "2006-2010", "2007-2011" = "2007-2011",
                                   "2008-2012" = "2008-2012", "2009-2013" = "2009-2013",
                                   "2010-2014" = "2010-2014", "2011-2015" = "2011-2015")),
        ## in map, allow for variable selection
        selectInput("var", "Select Variable of Interest",
                    choices = list("High School Diploma or Higher" = "HS_Pct", 
                                   "Bachelors Degree or Higher" = "Bachelors_Pct",
                                   "Graduate or Professional Degree" = "Grad_Pct"))
        
      ),
      
      tags$hr(),
      
      ## author line
      helpText("Created by Emily R. Ramos, Arvind Ramakrishnan, Jenna F. Kiridly, and Stephen A. Lauer", "Updated by Valerie Evans"),
      
      ## email feedback link
      ## To develop a link in HTML
      helpText(a("Send us your comments or feedback!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank",onclick="ga('send', 'event', 'click', 'link', 'feedback', 1)")),
      
      ## data source citation
      helpText(a("Data Source: American Community Survey- Table DP02", href="http://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_13_1YR_S1501&prodType=table",
                 target="_blank",onclick="ga('send', 'event', 'click', 'link', 'dataSource', 1)")),
      
      ## GitHub link
      helpText(a("View our data and code on GitHub", 
                 href="https://github.com/sEigmA/SEIGMA/tree/gh-pages/education", target="_blank",onclick="ga('send', 'event', 'click', 'link', 'code', 1)")),
      
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
                 h4("Educational Attainment by Region Over Selected Five Year Period", align="center"),
                 plot_options,
                 ## add text about the variables
#                  plot_main_text,
                 value="plot"),
# conditionalPanel(
#   condition="input.tabs == 'plot' && input.action == 0",
#   ## within the map area, you can create an action button.  similar to initializing the legend but just putting a button instead.
#   absolutePanel(right = 400, top = 300, class = "floater",
#                 actionButton("action", "Generate Plot")
#   ))),
        
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
                 #High School
                 conditionalPanel(
                   condition="input.var == 'HS_Pct' && input.action != 0",
                   absolutePanel(
                     right = 10, top = 100, draggable=FALSE, style = "", 
                     class = "floater",
                    strong("High School"),
                    br(),
                    strong("Attainment",br(), "or Greater"),
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
                 #Colledge
                 conditionalPanel(
                   condition="input.var == 'Bachelors_Pct' && input.action != 0",
                   absolutePanel(
                     right = 10, top = 100, draggable=FALSE, style = "", 
                     class = "floater",
                     strong("Bachelor's",br(), "Attainment"),
                     br(),
                     strong("or Greater"),
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
                 #Graduate School
                 conditionalPanel(
                   condition="input.var == 'Grad_Pct' && input.action != 0",
                   absolutePanel(
                     right = 10, top = 100, draggable=FALSE, style = "", 
                     class = "floater",
                     strong("Graduate Degree"),
                     br(),
                     strong("Attainment",br(), "or Greater"),
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
                 
                 
                 
                 
#                  plot_main_text,
                 value="map"),
        
        tabPanel("More Info", 
                 p(strong("Variable Summary:")),
                 tags$br(),
                 tags$ul(
                  tags$li(p(strong("Educational Attainment Rates"),
                 " - The number of people with each level of educational attainment for a specific region over a specific five-year period of time.  All inidviduals represented in this measure were at least 25 years of age. Respondents were classified according to highest level of school completed. When a municipaility is missing data, this indicates that data cannot be displayed because the number of people is too small.")), 
                 tags$br(),
                 tags$li(p(strong("Five-Year Estimates"), "-Survey information is collected everyday of the year and then aggregated over a specific time period,in this case, five years.  Multiyear estimates are available to regions with populations less than 65,000.  However, more precise estimates are possible for larger municipalities.To analyze change over time, users are dicouraged from utilizing overlapping multi-year estimates (e.g. 2005-2009, 2006-2010) due to the inability to isolate change with precision." ))),
                 
                
                 
                 ## email feedback link
                 h3(a("Please fill out our survey to help improve the site!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank")), value="info"),
        id="tabs"
      )
    ))
  )
))