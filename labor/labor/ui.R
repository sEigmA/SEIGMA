#######################################
## Title: Labor ui.R                 ##
## Author(s): Emily Ramos, Arvind    ##
##            Ramakrishnan, Jenna    ##
##            Kiridly, Xuelian Li    ## 
## Date Created:  02/27/2015         ##
## Date Modified: 03/14/2015         ##
#######################################

shinyUI(fluidPage(
  ## HTML to create generate map button
  gen_map_button,
  ## this starts the googleCharts engine
  googleChartsInit(),
  
  ## blank title, but put in a special title for window tab
  titlePanel("", windowTitle = "SEIGMA: Labor Force App"),
  
  ## Create sidebar
  sidebarLayout(
    sidebarPanel(width=4,
                 ## Conditional panel means if the condition is met show all text below otherwise Don't!
                 summary_side_text,
                 
                 plot_side_text,
                 
                 map_side_text,
                 
                 info_side_text,
                 
                 ## in map, allow for variable selection
                 ## in summary and map, allow for timespan selection
                 conditionalPanel(
                   condition="input.tabs == 'summary' || input.tabs == 'map'",
                   ## Select input = Drop down list of timespan (variable name on server side) 
                   selectInput("timespan", "Select Timespan",
                               list("Single Year" = "sing.yr",
                                    "Multiple Years" = "mult.yrs"))
                 ),
                 
                 ## if single year is selected, select year. if multiple years are selected, choose range.
                 conditionalPanel(
                   condition="input.tabs == 'summary' || input.tabs == 'map'",
                   conditionalPanel(
                     condition="input.timespan == 'sing.yr'",
                     
                     ## Initializing a single slider
                     sliderInput("year", "Select Year",
                                 min=1990, max=2012, value=2012,
                                 sep="")
                   ),
                   conditionalPanel(
                     ## Initializes a multi-year slider (range)
                     condition="input.timespan == 'mult.yrs'",
                     ## Slider starts from 2010-2012
                     sliderInput("range", "Select Years",
                                 min=1990, max=2012, value=c(2010,2012),
                                 sep="")
                   )
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
                               choices = MA_municipals, multiple=TRUE)),
                 
                 ## In summary and plot, show boxes that will compare to MA or US average
                 #       conditionalPanel(
                 #         condition="input.tabs == 'summary'|| input.tabs == 'plot'",
                 #         ## False at the end means it starts off unchecked
                 #         checkboxInput("MA_mean", "Compare to MA Average", FALSE),
                 #         checkboxInput("US_mean", "Compare to US Average", FALSE)
                 #       ),
                 
                 tags$hr(),
                 
                 ## author line
                 helpText("Created by Arvind Ramakrishnan, Jenna F. Kiridly, Xeulian Li and Emily R. Ramos"),
                 
                 ## email feedback link
                 ## To develop a link in HTML
                 helpText(a("Send us your comments or feedback!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank")),
                 
                 ## data source citation
                 helpText(a("Data Source: Bureau of Labor Statistics", href="http://www.bls.gov/lau/data.htm",
                            target="_blank")),
                 
                 ## GitHub link
                 helpText(a("View the data and code on GitHub", 
                            href="https://github.com/sEigmA/SEIGMA/tree/gh-pages/labor", target="_blank")),
                 
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
                 h4("Annual Average Labor by Region", align="center"),
                 ## make a row to put two charts in
                 
                 googleLineChart("plot", width="100%", height="475px", options = list(
                   
                   ## set fonts
                   fontName = "Source Sans Pro",
                   fontSize = 14,
                   
                   ## set axis titles, ticks, fonts, and ranges
                   hAxis = list(
                     title = "Year",
                     format = "####",
                     ticks = seq(1990, 2012, 2),
                     viewWindow = xlim,
                     textStyle = list(
                       fontSize = 14),
                     titleTextStyle = list(
                       fontSize = 16,
                       bold = TRUE,
                       italic = FALSE)
                   ),
                   vAxis = list(
                     title = "Annual Average Labor Force Participation",
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
                 
                 ## Single Year Legend
                 conditionalPanel(
                   condition="input.timespan == 'sing.yr' && input.action != 0",
                   absolutePanel(
                     right = 30, top = 215, draggable=FALSE, style = "", 
                     class = "floater",
                     strong("Single Year"),
                     tags$br(),
                     strong("Annual Average Labor"),
                     tags$table(
                       mapply(function(from, to, color) {
                         tags$tr(
                           tags$td(tags$div(
                             style = sprintf("width: 16px; height: 16px; background-color: %s;", color)
                           )),
                           
                           tags$td(prettyNum(round(from), big.mark = ","), "to", 
                                   prettyNum(round(to), big.mark = ","), align = "right")
                           
                           #  tags$td(round(from, 2), "to", round(to, 2), align = "right")
                         )
                       }, 
                       scolorRanges$from, scolorRanges$to, smap.colors[-length(smap.colors)],
                       SIMPLIFY=FALSE),
                       tags$tr(
                         tags$td(tags$div(
                           style = sprintf("width: 16px; height: 16px; background-color: %s;", "#999999")
                         )),
                         tags$td("Data not available", align = "right")))
                   )),
                 
                 ## Multi Year Legend
                 conditionalPanel(
                   condition="input.timespan == 'mult.yrs' && input.action != 0",
                   absolutePanel(
                     right = 30, top = 215, draggable=FALSE, style = "", 
                     class = "floater",
                     strong("Multiple Year"),
                     tags$br(),
                     strong("Change in Labor"),
                     tags$table(
                       mapply(function(from, to, color) {
                         tags$tr(
                           tags$td(
                             tags$div(
                               style = sprintf("width: 16px; height: 16px; background-color: %s;", color)
                             )),
                           tags$td(prettyNum(round(from), big.mark = ","), "to", 
                                   prettyNum(round(to), big.mark = ","), align = "right")
                         )
                       }, 
                       mcolorRanges$from, mcolorRanges$to, mmap.colors[-length(mmap.colors)],
                       SIMPLIFY=FALSE),
                       tags$tr(
                         tags$td(tags$div(
                           style = sprintf("width: 16px; height: 16px; background-color: %s;", "#999999")
                         )),
                         tags$td("Data not available", align = "right"))
                     )
                   )),
                 
                 #                plot_main_text,
                 value="map"),
        
        tabPanel("More Info", 
                 p(strong("Variable Summary:")),
                 tags$br(),
                 tags$ul(
                   tags$li(p(strong("Labor Force"),
                             "-All people classified in the civilian labor force plus members of the U.S.Armed Forces who are on active duty. People excluded from this are mainly students, homemakers, retired workers,seasonal workers not looking for work,institutionalized people, and people doing only incidental unpaid family work  for less than 15 hours.")),
                   tags$br(),
                   tags$li((p(strong('Bureau of Labor Statistics'),
                              "-This data was collected from the Bureau of Labor Statistics (BLS). It's helpful to note that the BLS is subject to annual revision revised to reflect new population controls from the Census Bureau, updated input data, and re-estimation.  Links to the data source will be updated when new data is available.")))
                 ),
                 
                 #tags$br(),
                 # p("SEIGMA. Social and Economic Impacts of Gambling in Massachusetts, University of Massachusetts School of Public Health and Health Sciences. (2014). Report on the Social and Economic Impact of Gambling in Massachusetts SEIGMA Gambling study. Report to the Massachusetts Gaming Commission & the Massachusetts department of Public Health. Retrieved from:"), a("http://www.umass.edu/seigma/sites/default/files/March%202014%20SEIGMA%20Report_6-19_for%20website.pdf"),                  
                 
                 ## email feedback link
                 h3(a("Please fill out our survey to help improve the site!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank")), value="info"),
        id="tabs"
                 ) #ends tableset panel
        ))#end bootstrap page
    )
  ))