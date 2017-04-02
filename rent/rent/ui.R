#######################################
## Title: RENT   ui.R                ##
## Author(s): JWB, BF                ## 
## Date Created:  12/01/2016         ##
#######################################

shinyUI(fluidPage(
  ## embed the google analytics script in the app
  tags$head(includeScript("google-analytics.js")),
  ## HTML to create generate map button
  gen_map_button,
  ## this starts the googleCharts engine
  googleChartsInit(),
  
  ## blank title, but put in a special title for window tab
  titlePanel("", windowTitle = "SEIGMA: RENT Shiny App"),
  
  ## Create sidebar
  sidebarLayout(
    sidebarPanel(width=4,
                 ## Conditional panel means if the condition is met show all text below otherwise Don't!
                 summary_side_text,
                 
                 plot_side_text,
                 
                 map_side_text,
                 
                 info_side_text,
                 
                 ## in map, allow for variable selection
                 
                 # Choose range for year.
                 # Initializing a single slider
                 conditionalPanel(
                   condition="input.tabs == 'map'",
                   selectInput("map_year", "Select Five Year Range",
                               choices = list("2005-2009" = "2005-2009", "2006-2010" = "2006-2010",
                                              "2007-2011" = "2007-2011", "2008-2012" = "2008-2012", "2009-2013" = "2009-2013",
                                              "2010-2014" = "2010-2014","2011-2015" = "2011-2015"))
                 ),


                 ## in summary, allow for municipal selection
                 conditionalPanel(
                   condition="input.tabs == 'summary'",
                   ## Select input = List
                   selectInput("sum_year", "Select Five Year Range",
                               choices = list("2005-2009" = "2005-2009", "2006-2010" = "2006-2010",
                                              "2007-2011" = "2007-2011", "2008-2012" = "2008-2012", "2009-2013" = "2009-2013",
                                              "2010-2014" = "2010-2014","2011-2015" = "2011-2015"),
                               multiple=TRUE),
                   selectInput("sum_muni", "Select Municipality", 
                               choices = MA_municipals,
                               ## Multiple allows for multi-county selection
                               multiple=TRUE),
                   
                     ## In summary, show boxes that will compare to MA or US median
                   checkboxInput("MA_mean", "Compare to MA Median", FALSE),
                   checkboxInput("US_mean", "Compare to US Median", FALSE)
                   
                 ),
                 
                  ## in plot, allow for municipal selection
                  conditionalPanel(
                    condition="input.tabs == 'plot'",
                    ## Select input = List
                    
                    selectInput("plot_muni", "Select Municipality",
                                choices = MA_municipals, multiple = T),
                    checkboxInput("MA_mean_p", "Compare to MA Median", TRUE),
                    checkboxInput("US_mean_p", "Compare to US Median", FALSE)
                 
                 
                  ),
                 

                 tags$hr(),
                 
                 ## author line
                 helpText("Created by Justin Baldwin and Brook Frye"),
                 
                 ## email feedback link
                 ## To develop a link in HTML
                 helpText(a("Send us your comments or feedback!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank")),
                 
                 ## data source citation
                 helpText(a("Data Source: American Community Survey- Table B25058", href="http://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_14_5YR_B25058&prodType=table",
                            target="_blank")),
                 
                 ## GitHub link
                 helpText(a("View our data and code on GitHub", 
                            href="https://github.com/sEigmA/SEIGMA/tree/gh-pages/rent", target="_blank")),
                 
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
                 
                 dataTableOutput("summary")
                 
                 
                 
                 , value="summary", 
                 tags$style(type="text/css", '#summary tfoot {display:none;}')),
        
#         ## plot tab with google chart options
        tabPanel("Plot",
                 ## make chart title here (otherwise not centered)
                 h4("Inflation-Adjusted (2015 $) Median Annual Rent Over Selected Five Year Period", align="center"),
                 ## make a row to put two charts in
                 
                 plotOutput("plot"),
                 HTML("Horizontal grey bars indicate the span of five-year estimates, vertical grey bars with hinges indicate the standard errors"),
                 #verbatimTextOutput("event"),
# 
#lplot,
                  #textOutput("plot_df")

                 ## add text about the variables
                 #                  plot_main_text,
                 value="plot"),
#         
#         ## plot map
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
                     right = 5, top = 100, draggable=FALSE, style = "",
                     class = "floater",
                     strong("Median"),
                     tags$br(),
                     strong("Contract", br(), "Rent 2015$"),
                     plotOutput("legend1"),
                     tags$table(

                       tags$td(tags$div(
                         style = sprintf("width: 16px; height: 16px; background-color: %s;", "#999999")
                       )),
                       tags$td("Data not", br(), "available", align = "right")
                     )
                   )),

                 #                plot_main_text,
                 value="map"),
#         
        tabPanel("More Info", 
                 p(strong("Variable Summary:")),
                 tags$br(),
                 tags$ul(
                   tags$li(p(strong("Inflation-Adjusted (2015 $) Median Contract Rent"), "-Contract rent is the dollar amount of the rental obligation specified in the lease. Five-year estimates were collected between 2002 and 2015 and adjusted for inlation to the 2015 dollar. Data were collected at multiple levels to allow for analysis at multiple geographic scales; municipality, state, and national level.")),
            
                   tags$br(),
                   tags$li(p(strong("Median"),"-The median divides the distribution of inflation-adjusted median contract rent into two equal parts; half that fall below the median for contract rent and half that fall above.")),
                   tags$br(),
                   tags$li(p(strong("Five-Year Estimates"),"-Survey information is collected every year and then aggregated over a specific time period, in this case, five years at the municipality level.  Yearly estimates of inflation-adjusted median contract rent are available only at the county level to acheive a representative sample. ")
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