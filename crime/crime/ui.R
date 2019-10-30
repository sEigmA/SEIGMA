#######################################
## Title: Crime ui.R                 ##
## Author(s): Heather Weaver,        ##
##            Valerie Evans          ##
## Date Created:  06/26/2019         ##
## Date Modified: 10/30/2019 VE      ##
#######################################


shinyUI(fluidPage(
  ## embed the google analytics script in the app
  tags$head(includeScript("google-analytics.js")),
  ## HTML to create generate map button
  gen_map_button,
  ## this starts the googleCharts engine
  googleChartsInit(),
  ## blank title, but put in a special title for window tab
  titlePanel("", windowTitle = "SEIGMA: Crime App"),
  
  ## Create sidebar
  sidebarLayout(
    sidebarPanel(width = 4, 
                 summary_side_text, 
                 plot_side_text, 
                 map_side_text, 
                 info_side_text,
                 
                 ## in summary, allow for year, municipal selection 
                 conditionalPanel(
                   condition = "input.tabs == 'summary'", 
                   selectInput("sum_year", "Select Year", 
                               choices = list("2010" = "2010", "2011" = "2011", 
                                              "2012" = "2012", "2013" = "2013", 
                                              "2014" = "2014", "2015" = "2015", 
                                              "2016" = "2016", "2017" = "2017")), 
                   selectInput("sum_muni", "Select Municipality",
                               choices = MA_municipals,
                               ## Multiple allows for multi-county selection
                               multiple = TRUE),
                   ##show boxes that will compare to MA or US average,False at the end means it starts off unchecked
                   checkboxInput("MA_mean", "Compare to MA", FALSE),
                   checkboxInput("US_mean", "Compare to US", FALSE)
                 ),
                 
                 
                 ## in plot, allow for year, municipal selection
                 conditionalPanel(
                   condition = "input.tabs == 'plot'", 
                   ## Select input = List       
                   selectInput("plot_muni", "Select Municipality", 
                               choices = MA_municipals, selected = "Springfield", multiple = TRUE),
                   selectInput("plot_var", "Select Crime Rate of Interest",
                               choices = list("Violent Crime" = "Violent_crime_Rate", 
                                              "Murder and Nonnegligent Manslaughter" = "Murder_and_nonnegligent_manslaughter_Rate",
                                              "Rape" = "Rape_Rate",
                                              "Robbery" = "Robbery_Rate",
                                              "Aggravated Assault" = "Aggravated_assault_Rate",
                                              "Property Crime" = "Property_crime_Rate",
                                              "Burglary" = "Burglary_Rate",
                                              "Larceny/Theft" = "Larceny_theft_Rate",
                                              "Motor Vehicle Theft" = "Motor_vehicle_theft_Rate",
                                              "Arson" = "Arson_Rate")),
                   conditionalPanel(
                     condition = "input.plot_var == 'Violent_crime_Rate'",
                     checkboxInput("MA_violent", "Compare to MA", FALSE),
                     checkboxInput("US_violent", "Compare to US", FALSE)),
                   conditionalPanel(
                     condition = "input.plot_var == 'Murder_and_nonnegligent_manslaughter_Rate'",
                     checkboxInput("MA_murder", "Compare to MA", FALSE),
                     checkboxInput("US_murder", "Compare to US", FALSE)),
                   conditionalPanel(
                     condition = "input.plot_var == 'Rape_Rate'",
                     checkboxInput("MA_rape", "Compare to MA", FALSE),
                     checkboxInput("US_rape", "Compare to US", FALSE)),
                   conditionalPanel(
                     condition = "input.plot_var == 'Robbery_Rate'",
                     checkboxInput("MA_robbery", "Compare to MA", FALSE),
                     checkboxInput("US_robbery", "Compare to US", FALSE)),
                   conditionalPanel(
                     condition = "input.plot_var == 'Aggravated_assault_Rate'",
                     checkboxInput("MA_assault", "Compare to MA", FALSE),
                     checkboxInput("US_assault", "Compare to US", FALSE)),
                   conditionalPanel(
                     condition = "input.plot_var == 'Property_crime_Rate'",
                     checkboxInput("MA_property", "Compare to MA", FALSE),
                     checkboxInput("US_property", "Compare to US", FALSE)),
                   conditionalPanel(
                     condition = "input.plot_var == 'Burglary_Rate'",
                     checkboxInput("MA_burglary", "Compare to MA", FALSE),
                     checkboxInput("US_burglary", "Compare to US", FALSE)),
                   conditionalPanel(
                     condition = "input.plot_var == 'Larceny_theft_Rate'",
                     checkboxInput("MA_larceny", "Compare to MA", FALSE),
                     checkboxInput("US_larceny", "Compare to US", FALSE)),
                   conditionalPanel(
                     condition = "input.plot_var == 'Motor_vehicle_theft_Rate'",
                     checkboxInput("MA_motor", "Compare to MA", FALSE),
                     checkboxInput("US_motor", "Compare to US", FALSE)),
                   conditionalPanel(
                     condition = "input.plot_var == 'Arson_Rate'",
                     checkboxInput("MA_arson", "Compare to MA", FALSE),
                     checkboxInput("US_arson", "Compare to US", FALSE), 
                     textOutput('text'))
                 ),
                 
                 ## in map, allow for year, municipal selection
                 conditionalPanel(
                   condition = "input.tabs == 'map'",
                   selectInput("map_year", "Select Year",
                               choices = list("2010" = "2010", "2011" = "2011",
                                              "2012" = "2012", "2013" = "2013", 
                                              "2014" = "2014", "2015" = "2015", 
                                              "2016" = "2016", "2017" = "2017")),
                   ## in map, allow for variable selection
                   selectInput("var", "Select Crime Rate of Interest",
                               choices = list("Violent Crime" = "Violent_crime_Rate", 
                                              "Murder and Nonnegligent Manslaughter" = "Murder_and_nonnegligent_manslaughter_Rate",
                                              "Rape" = "Rape_Rate",
                                              "Robbery" = "Robbery_Rate",
                                              "Aggravated Assault" = "Aggravated_assault_Rate",
                                              "Property Crime" = "Property_crime_Rate",
                                              "Burglary" = "Burglary_Rate",
                                              "Larceny/Theft" = "Larceny_theft_Rate",
                                              "Motor Vehicle Theft" = "Motor_vehicle_theft_Rate",
                                              "Arson" = "Arson_Rate"))
                 ),
                 
                 tags$hr(),
                 
                 ## author line
                 helpText("Created by Heather Weaver and Valerie Evans"), 
                 
                 ## email feedback link
                 ## To develop a link in HTML
                 helpText(a("Send us your comments or feedback!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank",onclick="ga('send', 'event', 'click', 'link', 'feedback', 1)")),
                 
                 ## data source citation
                 helpText(a("Data Source: FBI - Estimated Crime Data", href="https://crime-data-explorer.fr.cloud.gov/downloads-and-docs",
                            target="_blank",onclick="ga('send', 'event', 'click', 'link', 'dataSource', 1)")),
                 
                 ## GitHub link
                 helpText(a("View our data and code on GitHub", 
                            href="https://github.com/sEigmA/SEIGMA/tree/gh-pages/Crime", target="_blank",onclick="ga('send', 'event', 'click', 'link', 'code', 1)")),
                 
                 helpText("If using Internet Explorer, application only visible in version 10.")
    ),
    
    ######### End of Sidebar  #########
    
    ######### Start of Main Panel #####
    
    bootstrapPage(mainPanel(
      ## put in logo for title
      a(img(src = "logo.jpg", height = 105, width = 920), href = "http://www.umass.edu/seigma/"),
      
      ## create tabs
      tabsetPanel(
        tabPanel("About", 
                 about_main_text, value = "about"),
        
        ## summary tab
        tabPanel("Summary", 
                 dataTableOutput("summary"), value = "summary", 
                 tags$style(type="text/css", '#summary tfoot {display:none;}')),
        
        ## plot tab with google chart options
        tabPanel("Plot",
                 ## make chart title here (otherwise not centered)
                 # h4("Crime Rate by Municipality", align = "center"),
                 ## add text about the variables
                 conditionalPanel(
                   condition = "input.plot_var == 'Violent_crime_Rate'",
                   h4("Annual Violent Crime Rate by Municipality Over Time", align = "center"),
                   violent_plot_options),
                 conditionalPanel(
                   condition = "input.plot_var == 'Murder_and_nonnegligent_manslaughter_Rate'",
                   h4("Annual Murder and Nonnegligent Manslaughter Crime Rate by Municipality Over Time", align = "center"),
                   murder_plot_options),
                 conditionalPanel(
                   condition = "input.plot_var == 'Rape_Rate'",
                   h4("Annual Rape Crime Rate by Municipality Over Time", align = "center"),
                   rape_plot_options),
                 conditionalPanel(
                   condition = "input.plot_var == 'Robbery_Rate'",
                   h4("Annual Robbery Crime Rate by Municipality Over Time", align = "center"),
                   robbery_plot_options),
                 conditionalPanel(
                   condition = "input.plot_var == 'Aggravated_assault_Rate'",
                   h4("Annual Aggravated Assault Crime Rate by Municipality Over Time", align = "center"),
                   assault_plot_options),
                 conditionalPanel(
                   condition = "input.plot_var == 'Property_crime_Rate'",
                   h4("Annual Property Crime Rate by Municipality Over Time", align = "center"),
                   property_plot_options),
                 conditionalPanel(
                   condition = "input.plot_var == 'Burglary_Rate'",
                   h4("Annual Burglary Crime Rate by Municipality Over Time", align = "center"),
                   burglary_plot_options),
                 conditionalPanel(
                   condition = "input.plot_var == 'Larceny_theft_Rate'",
                   h4("Annual Larceny-Theft Crime Rate by Municipality Over Time", align = "center"),
                   larceny_plot_options),
                 conditionalPanel(
                   condition = "input.plot_var == 'Motor_vehicle_theft_Rate'",
                   h4("Annual Motor Vehicle Theft Crime Rate by Municipality Over Time", align = "center"),
                   motor_plot_options),
                 conditionalPanel(
                   condition = "input.plot_var == 'Arson_Rate'",
                   h4("Annual Arson Crime Rate by Municipality Over Time", align = "center"),
                   arson_plot_options),
                 plot_main_text, 
                 value = "plot"),
        
        ## plot map
        tabPanel("Map",
                 ## Add a little CSS to make the map background pure white
                 tags$head(tags$style("
                 #showcase-code-position-toggle, #showcase-sxs-code { display: none; }
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
                   absolutePanel(left = 100, top = 450, width = 300, class = "floater",
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
                   condition = "input.var == 'Violent_crime_Rate' && input.action != 0",
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
                 #Murder_and_nonnegligent_manslaughter_Rate
                 conditionalPanel(
                   condition="input.var == 'Murder_and_nonnegligent_manslaughter_Rate' && input.action != 0",
                   absolutePanel(
                     right = 10, top = 100, draggable=FALSE, style = "", 
                     class = "floater",
                     strong("Murder and"),
                     br(),
                     strong("Nonnegligent"),
                     br(),
                     strong("Manslaughter Rate"),
                     br(),
                     plotOutput("legend2"),
                     tags$table(
                       tags$tr(
                         tags$td(tags$div(
                           style = sprintf("width: 16px; height: 16px; background-color: %s;", "#999999")
                         )),
                         tags$td("Data not available", align = "right")
                       )
                     )
                   )),
                 #Rape
                 conditionalPanel(
                   condition="input.var == 'Rape_Rate' && input.action != 0",
                   absolutePanel(
                     right = 10, top = 100, draggable=FALSE, style = "", 
                     class = "floater",
                     strong("Rape Rate"),
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
                 #Robbery
                 conditionalPanel(
                   condition="input.var == 'Robbery_Rate' && input.action != 0",
                   absolutePanel(
                     right = 10, top = 100, draggable=FALSE, style = "", 
                     class = "floater",
                     strong("Robbery Rate"),
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
                 #Aggravated Assault Rate
                 conditionalPanel(
                   condition="input.var == 'Aggravated_assault_Rate' && input.action != 0",
                   absolutePanel(
                     right = 10, top = 100, draggable=FALSE, style = "", 
                     class = "floater",
                     strong("Aggravated Assault Rate"),
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
                 #Property crime Rate
                 conditionalPanel(
                   condition="input.var == 'Property_crime_Rate' && input.action != 0",
                   absolutePanel(
                     right = 10, top = 100, draggable=FALSE, style = "", 
                     class = "floater",
                     strong("Property Crime Rate"),
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
                 #Burglary Rate
                 conditionalPanel(
                   condition="input.var == 'Burglary_Rate' && input.action != 0",
                   absolutePanel(
                     right = 10, top = 100, draggable=FALSE, style = "", 
                     class = "floater",
                     strong("Burglary Rate"),
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
                 #Larceny-theft Rate
                 conditionalPanel(
                   condition="input.var == 'Larceny_theft_Rate' && input.action != 0",
                   absolutePanel(
                     right = 10, top = 100, draggable=FALSE, style = "", 
                     class = "floater",
                     strong("Larceny-theft Rate"),
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
                 #Motor Vehicle theft Rate
                 conditionalPanel(
                   condition="input.var == 'Motor_vehicle_theft_Rate' && input.action != 0",
                   absolutePanel(
                     right = 10, top = 100, draggable=FALSE, style = "", 
                     class = "floater",
                     strong("Motor Vehicle Theft Rate"),
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
                 #Arson
                 conditionalPanel(
                   condition="input.var == 'Arson_Rate' && input.action != 0",
                   absolutePanel(
                     right = 10, top = 100, draggable=FALSE, style = "", 
                     class = "floater",
                     strong("Arson Rate"),
                     br(),
                     plotOutput("legend10"),
                     tags$table(
                       tags$tr(
                         tags$td(tags$div(
                           style = sprintf("width: 16px; height: 16px; background-color: %s;", "#999999")
                         )),
                         tags$td("Data not", br(), "available", align = "right")
                       )
                     )
                   )), 
                 #plot_main_text,
                 value = "map"),
        
        tabPanel("More Info", 
                 p(strong("Variable Summary:")),
                 tags$br(),
                 tags$ul(
                   tags$li(p(strong("Crime Rates"), " - Calculated by dividing the number of reported crimes by the population and multiplying the result by 100,000. The result is presented as the rate of a particular crime per 100,000 population. Extrapolation was used to calculate crime rates for those populations below 100,000.")), 
                   tags$br(), 
                   tags$ul(p(strong("Crime Rate = "), "Number of Reported Crimes / Total Population * 100,000")), 
                   tags$br(), 
                   tags$li(p(strong("Arson")," - If data is not available for arson at the municipality level, it may indicate that the FBI did not receive 12 complete months of arson data from that local agency. No arson data is included for MA and the United States as there is not sufficient data available to estimate totals.")), 
                   tags$br(), 
                   tags$li(p(strong("Violent Crimes")," - The values include the offenses of murder, rape (revised definition), robbery, and aggravated assault.")), 
                   # tags$br(), 
                   tags$div(
                     tags$p("Because of changes in the state/local agency's reporting practices, values for the following regions are not comparable to previous years:")), 
                   tags$ul(
                     tags$li(p(strong("2010:"), "Billerica, Hanson, North Brookfield, Plymouth, Shrewsbury, and Yarmouth")), 
                     tags$li(p(strong("2012:"), "Framingham, Milton, and Salem"))), 
                   # tags$br(), 
                   tags$div(
                     tags$p("The FBI deemed that the data was overreported from the following agency's for the years specified below, so the data is not included.")),
                   tags$ul(
                     tags$li(p(strong("2014:"), "Cohasset")),
                     tags$li(p(strong("2016:"), "Brookline, Gardner, and Yarmouth")))),
                 tags$br(), 
                 tags$ul(
                   tags$li(p(strong("Rape - Revised Definition")),
                           tags$ul(
                             tags$li(p("The 2013 values for the offense of rape were reported using the revised Uniform Crime Reporting (UCR) definition of rape for all MA municipalities except for Avon, Becket, Boston, Dighton, Egremont, Essex, Granville, Holbrook, Lawrence, and Westhampton, which still used the legacy definition. See the Data Declaration on the FBI's website for further explanation.")),
                             # tags$br(),
                             tags$li(p("The 2014 values for the offense of rape were reported using the revised Uniform Crime Reporting (UCR) definition of rape for all MA municipalities except for Avon, Becket, Boston, Dighton, Egremont, Essex, and, Lawrence, which still used the legacy definition. See the Data Declaration on the FBI's website for further explanation.")),
                             # tags$br(),
                             tags$li(p("The 2015 through 2017 values for the offense of rape were reported using the revised Uniform Crime Reporting (UCR) definition of rape for all MA municipalities. See the Data Declaration on the FBI's website for further explanation.")))
                   )),
                 
                 ## email feedback link
                 h3(a("Please fill out our survey to help improve the site!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank")), value="info"),
        id="tabs"
      )
    ))
  )
))

