#######################################
## Title: Crime ui.R                 ##
## Author(s): Heather Weaver         ##
## Date Created:  6/26/2019          ##
## Date Modified:                    ##
## Data Updated:                     ##
#######################################


shinyUI(fluidPage(
  ## embed the google analytics script in the app
  tags$head(includeScript("google-analytics.js")),
  ## HTML to create generate map button
  gen_map_button,
  ## this starts the googleCharts engine
  #gvisChartsInit(),
  
  ## blank title, but put in a special title for window tab
  titlePanel("", windowTitle = "SEIGMA: Crime Shiny App"),
  
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
      selectInput("sum_year", "Select Year",
                  choices = list("2010" = "2010", "2011" = "2011",
                                 "2012" = "2012", "2013" = "2013", 
                                 "2014" = "2014", "2015" = "2015", 
                                 "2016" = "2016", "2017" = "2017")),
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
        selectInput("plot_year", "Select Year",
                    choices = list("2010" = "2010", "2011" = "2011",
                                   "2012" = "2012", "2013" = "2013", 
                                   "2014" = "2014", "2015" = "2015", 
                                   "2016" = "2016", "2017" = "2017")),
        ## Select input = List
        selectInput("plot_muni", "Select Municipality", 
                    choices = MA_municipals)),
      
      ## in map, allow for year, municipal selection
      conditionalPanel(
        condition="input.tabs == 'map'",
        selectInput("map_year", "Select Year",
                    choices = list("2010" = "2010", "2011" = "2011",
                                   "2012" = "2012", "2013" = "2013", 
                                   "2014" = "2014", "2015" = "2015", 
                                   "2016" = "2016", "2017" = "2017")),
        ## in map, allow for variable selection
        selectInput("var", "Select Crime(s) of Interest",
                    choices = list("Violent Crime" = "Violent crime Rate", 
                                   "Rape" = "Rape Rate",
                                   "Robbery" = "Robbery Rate",
                                   "Aggravated Assault" = "Aggravated assault Rate",
                                   "Property Crime" = "Property crime Rate",
                                   "Burglary" = "Burglary Rate",
                                   "Larceny-theft" = "Larceny-theft Rate",
                                   "Motor vehicle Theft" = "Motor vehicle theft Rate",
                                   "Arson" = "Arson Rate"))
        ),
      
      tags$hr(),
      
      ## author line
      helpText("Created by Heather Weaver and Valerie Evans"),
      ## email feedback link
      ## To develop a link in HTML
      helpText(a("Send us your comments or feedback!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank",onclick="ga('send', 'event', 'click', 'link', 'feedback', 1)")),
      
      ## data source citation
      helpText(a("Data Source: FBI - Estimated Crime Data", href="https://crime-data-explorer.fr.cloud.gov/",
                 target="_blank",onclick="ga('send', 'event', 'click', 'link', 'dataSource', 1)")),
      
      ## GitHub link
      helpText(a("View our data and code on GitHub", 
                 href="https://github.com/sEigmA/SEIGMA/tree/gh-pages/crime", target="_blank",onclick="ga('send', 'event', 'click', 'link', 'code', 1)")),
      
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
                 p(strong("The SEIGMA Crime App"), "displays the yearly estimates of crime
                     for municipalities in Massachusetts.",
                   p(strong("Click on different tabs to see the data in different formats.")),
                   tags$br(),
                   tags$ul(
                     tags$li(p(strong("Summary"), "shows the data in table format.")),
                     tags$li(p(strong("Plot"), "compares the crime rates for each municipality to the state, and national estimate.")),
                     tags$li(p(strong("Map"), "visually displays crime data by municipality.")),
                     tags$li(p(strong("More Info"), "describes crime data and crime rates."))
                   ))
                 ),
        
        ## summary tab
        tabPanel("Summary", 
                 dataTableOutput("summary"), value="summary", 
                 tags$style(type="text/css", '#summary tfoot {display:none;}')),
        
        ## plot tab with google chart options
        tabPanel("Plot",
                 ## make chart title here (otherwise not centered)
                 h4("Crime by Region", align="center"),
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
                 #Violent crime
                 conditionalPanel(
                   condition="input.var == 'Violent.crime.Rate' && input.action != 0",
                   absolutePanel(
                     right = 10, top = 100, draggable=FALSE, style = "", 
                     class = "floater",
                    strong("Violent Crime Rate"),
                    br(),
                
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
                 #Rape
                 conditionalPanel(
                   condition="input.var == 'Rape.Rate' && input.action != 0",
                   absolutePanel(
                     right = 10, top = 100, draggable=FALSE, style = "", 
                     class = "floater",
                     strong("Rape"),
                     br(),
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
                 #Robbery
                 conditionalPanel(
                   condition="input.var == 'Robbery.Rate' && input.action != 0",
                   absolutePanel(
                     right = 10, top = 100, draggable=FALSE, style = "", 
                     class = "floater",
                     strong("Robbery Rate"),
                     br(),
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
                 #Aggravated Assault Rate
                 conditionalPanel(
                   condition="input.var == 'Aggravated.assault.Rate' && input.action != 0",
                   absolutePanel(
                     right = 10, top = 100, draggable=FALSE, style = "", 
                     class = "floater",
                     strong("Aggravated Assault Rate"),
                     br(),
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
                 #Property crime Rate
                 conditionalPanel(
                   condition="input.var == 'Property.crime.Rate' && input.action != 0",
                   absolutePanel(
                     right = 10, top = 100, draggable=FALSE, style = "", 
                     class = "floater",
                     strong("Property Crime Rate"),
                     br(),
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
                 #Burglary Rate
                 conditionalPanel(
                   condition="input.var == 'Burglary.Rate' && input.action != 0",
                   absolutePanel(
                     right = 10, top = 100, draggable=FALSE, style = "", 
                     class = "floater",
                     strong("Burglary Rate"),
                     br(),
                     plotOutput("legend6"),
                     tags$table(
                       
                       tags$tr(
                         tags$td(tags$div(
                           style = sprintf("width: 16px; height: 16px; background-color: %s;", "#999999")
                         )),
                         tags$td("Data not", br(), "available", align = "right")
                       )
                     )
                   )),
                 #Larceny-theft Rate
                 conditionalPanel(
                   condition="input.var == 'Larceny.theft.Rate' && input.action != 0",
                   absolutePanel(
                     right = 10, top = 100, draggable=FALSE, style = "", 
                     class = "floater",
                     strong("Larceny-theft Rate"),
                     br(),
                     plotOutput("legend7"),
                     tags$table(
                       
                       tags$tr(
                         tags$td(tags$div(
                           style = sprintf("width: 16px; height: 16px; background-color: %s;", "#999999")
                         )),
                         tags$td("Data not", br(), "available", align = "right")
                       )
                     )
                   )),
                 #Motor Vehicle theft Rate
                 conditionalPanel(
                   condition="input.var == 'Motor.vehicle.theft.Rate' && input.action != 0",
                   absolutePanel(
                     right = 10, top = 100, draggable=FALSE, style = "", 
                     class = "floater",
                     strong("Motor Vehicle Theft Rate"),
                     br(),
                     plotOutput("legend8"),
                     tags$table(
                       
                       tags$tr(
                         tags$td(tags$div(
                           style = sprintf("width: 16px; height: 16px; background-color: %s;", "#999999")
                         )),
                         tags$td("Data not", br(), "available", align = "right")
                       )
                     )
                   )),
                 #Arson
                 conditionalPanel(
                   condition="input.var == 'Arson.Rate' && input.action != 0",
                   absolutePanel(
                     right = 10, top = 100, draggable=FALSE, style = "", 
                     class = "floater",
                     strong("Arson"),
                     br(),
                     plotOutput("legend9"),
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
                  tags$li(p(strong("Crime Rates"),
                 " - The number of a specific crime per 100,000 people.")),
                 tags$br(),
                 tags$li(p("If a blank is presented in the arson column for city level data for MA, it indicates that the FBI did not receive 12 complete months of arson data for that agency.")),
                 tags$br(),
                 tags$li(p("The violent crime figures include the offenses of murder, rape (revised definition), robbery, and aggravated assault.")),
                 tags$br(),
                 tags$li(p("No arson data is included for MA and the United States as there is not sufficient data available to estimate totals.")),
                 tags$br(),
                 tags$li(p("Because of changes in the state/local agency's reporting practices, values from 2012 are not comparable to previous years' data.")),
                 tags$br(),
                 tags$li(p("The 2013 figures for the offense of rape were reported using the revised Uniform Crime Reporting (UCR) definition of rape for all MAtowns/cities except for Avon, Becket, Boston, Dighton, Egremont, Essex, Granville, Holbrook, Lawrence and Westhampton, which still used the legacy definition. See the Data Declaration on the FBI's website for further explanation.")),
                 tags$br(),
                 tags$li(p("The 2014 figures for the offense of rape were reported using the revised Uniform Crime Reporting (UCR) definition of rape for all MAtowns/cities except for Avon, Becket, Boston, Dighton, Egremont, Essex, and Lawrence, which still used the legacy definition. See the Data Declaration on the FBI's website for further explanation.")),
                 tags$br(),
                 tags$li(p("The 2015 through 2017 values for the offense of rape were reported using the revised Uniform Crime Reporting (UCR) definition of rape for all MAtowns/cities. See the Data Declaration on the FBI's website for further explanation."))),
              
                 
                ## email feedback link
                h3(a("Please fill out our survey to help improve the site!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank")), value="info"),
        id="tabs"
      )
    ))
  )
))

