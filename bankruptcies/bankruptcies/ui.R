#######################################
## Title: Bankruptices ui.R          ##
## Author(s): Xuelian Li, Jenna      ##
##            Kiridly                ## 
## Date Created:  08/11/15           ##
## Date Modified: 08/15/15 XL        ##
#######################################


shinyUI(fluidPage(
  ## embed the google analytics script in the app
  ##tags$head(includeScript("google-analytics.js")),
  ## HTML to create generate map button
  gen_map_button,
  ## this starts the googleCharts engine
  googleChartsInit(),
  
  ## Create sidebar
  sidebarLayout(
    sidebarPanel(width=4,
                 ## Conditional panel means if the condition is met show all text below otherwise Don't!
                 summary_side_text,
                 
                 plot_side_text,
                 
                 map_side_text,
                 
                 info_side_text,
                 
                 
                 ## Choose range for year.
                 ## Initializing a single slider
                 conditionalPanel(
                   condition="input.tabs == 'summary'",
                   ## Select input = Drop down list of timespan (variable name on server side) 
                   
                   selectInput("sum_year", "Select Year",
                               choices = list("2013" = "2013", "2014" = "2014")
                   ),
                   
                   selectInput("sum_county", "Select County", 
                               choices = MAcounties,
                               ## Multiple allows for multi-county selection
                               multiple=TRUE),
                   radioButtons("sum_radio", "Select Variable of Interest",
                                c("Total Filings" = "Total Filings",
                                  "Business Filings" = "Business Filings", 
                                  "NonBusiness Filings" = "NonBusiness Filings"),
                                selected="Total Filings"),
                   checkboxInput("sum_MA", "Compare to MA", FALSE),
                   checkboxInput("sum_US", "Compare to US", FALSE)
                   
                 ),
                 
                 ## in plot, allow for County selection
                 conditionalPanel(
                   condition="input.tabs == 'plot'",
                   ## Select input = List
                   selectInput("plot_county", "Select County", 
                               choices = MAcounties, selected="Barnstable County", multiple=TRUE),
                   radioButtons("plot_radio", "Select Variable of Interest",
                                c("Business Filings" = "Business Filings", 
                                  "NonBusiness Filings" = "NonBusiness Filings"),
                                selected="Business Filings"),
                   conditionalPanel(
                     condition="input.plot_radio == 'Business Filings'",
                     selectInput("plot_bus_display", "Display Options",
                                 choices=list("Total" = "Business_Filings_Total", "Proportion of Chapter 7" = "Proportion_Business_Filings_Chapter_7",
                                              "Proportion of Chapter 11" = "Proportion_Business_Filings_Chapter_11","Proportion of Chapter 12" = "Proportion_Business_Filings_Chapter_12",
                                              "Proportion of Chapter 13" = "Proportion_Business_Filings_Chapter_13"),
                                 selected = "Business_Filings_Total"
                       ),
                     conditionalPanel(
                       condition="input.plot_bus_display != 'Business_Filings_Total'",
                       checkboxInput("plot_MA", "Compare to MA", FALSE),
                       checkboxInput("plot_US", "Compare to US", FALSE)
                       )
                     ),
                   conditionalPanel(
                     condition="input.plot_radio == 'NonBusiness Filings'",
                     selectInput("plot_nonbus_display", "Display Options",
                                 choices=list("Total" = "NonBusiness_Filings_Total", "Proportion of Chapter 7" = "Proportion_NonBusiness_Filings_Chapter_7",
                                              "Proportion of Chapter 11" = "Proportion_NonBusiness_Filings_Chapter_11",
                                              "Proportion of Chapter 13" = "Proportion_NonBusiness_Filings_Chapter_13"),
                                 selected = "NonBusiness_Filings_Total"
                     ),
                     conditionalPanel(
                       condition="input.plot_nonbus_display != 'NonBusiness_Filings_Total'",
                       checkboxInput("plot_MA", "Compare to MA", FALSE),
                       checkboxInput("plot_US", "Compare to US", FALSE)
                     )
                   )
                 ),
                 
                 
                 ## in map, allow for variable , option and year selection
                 conditionalPanel(
                   condition="input.tabs == 'map'",
                   selectInput("map_year", "Select Year",
                               choices = list("2013" = "2013", "2014" = "2014")
                   ),
                   radioButtons("map_radio", "Select Variable of Interest",
                                c("Business Filings" = "Business Filings", 
                                  "NonBusiness Filings" = "NonBusiness Filings"),
                                selected="Business Filings"),
                   conditionalPanel(
                     condition="input.map_radio == 'Business Filings'",
                     selectInput("map_bus_display", "Display Options",
                                 choices=list("Total" = "Business_Filings_Total", "Proportion of Chapter 7" = "Proportion_Business_Filings_Chapter_7",
                                              "Proportion of Chapter 11" = "Proportion_Business_Filings_Chapter_11","Proportion of Chapter 12" = "Proportion_Business_Filings_Chapter_12",
                                              "Proportion of Chapter 13" = "Proportion_Business_Filings_Chapter_13"),
                                 selected = "Business_Filings_Total"
                     )
                    ),
                   conditionalPanel(
                     condition="input.map_radio == 'NonBusiness Filings'",
                     selectInput("map_nonbus_display", "Display Options",
                                 choices=list("Total" = "NonBusiness_Filings_Total", "Proportion of Chapter 7" = "Proportion_NonBusiness_Filings_Chapter_7",
                                              "Proportion of Chapter 11" = "Proportion_NonBusiness_Filings_Chapter_11",
                                              "Proportion of Chapter 13" = "Proportion_NonBusiness_Filings_Chapter_13"),
                                 selected = "NonBusiness_Filings_Total"
                     )
                   )
                ),
              
                 tags$hr(),
              
                 ## author line
                 
                 helpText("Created by Xuelian Li and Jenna F. Kiridly"),
                 
                 ## email feedback link
                 ## To develop a link in HTML
                 helpText(a("Send us your comments or feedback!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank")),
                 
                 ## data source citation
                 helpText(a("United States Courts, Caseload Statistics Data Tables", href="http://www.uscourts.gov/statistics-reports/caseload-statistics-data-tables",
                            target="_blank")),
                 
                 ## GitHub link
                 helpText(a("View the data and code on GitHub", 
                            href="https://github.com/sEigmA/SEIGMA/tree/gh-pages/bankruptcies", target="_blank")),
                 
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
                 about_main_text, value="about"),
        
        ## summary tab
        tabPanel("Summary",
                 dataTableOutput("summary"), value="summary",
                 tags$style(type="text/css", '#summary tfoot {display:none;}')),
       
        ## plot tab with google chart options
        tabPanel("Plot",
                 ##plot upon the selected variable and display option           
                 conditionalPanel(
                   condition="input.plot_radio =='Business Filings'",
                 conditionalPanel(
                   condition="input.plot_bus_display == 'Business_Filings_Total'",
                    ## make chart title here (otherwise not centered)
                     h4("Total Business Filings Bankruptcies Over Time", align="center"),
                   Bus_plot_options),
                   conditionalPanel(
                     condition="input.plot_bus_display != 'Business_Filings_Total'",
                     ## make chart title here (otherwise not centered)
                     h4("Proportiong by Chapter in Business Filings Bankruptcies Over Time ", align="center"),
                     Pro_Bus_plot_options
                     )
                 ),
                 conditionalPanel(
                   condition="input.plot_radio =='NonBusiness Filings'",
                   conditionalPanel(
                     condition="input.plot_nonbus_display == 'NonBusiness_Filings_Total'",
                     ## make chart title here (otherwise not centered)
                     h4("Total NonBusiness Filings Bankruptcies Over Time", align="center"),
                     NonBus_plot_options),
                   conditionalPanel(
                     condition="input.plot_nonbus_display != 'NonBusiness_Filings_Total'",
                     ## make chart title here (otherwise not centered)
                     h4("Proportion by Chapter in NonBusiness Filings Bankruptcies Over Time ", align="center"),
                     Pro_NonBus_plot_options
                   )
                 ),
                 
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
                 
                 ## Business Filings Legend
                 conditionalPanel(
                   condition="input.map_radio == 'Business Filings' && input.map_bus_display == 'Business_Filings_Total' && input.action != 0",
                   absolutePanel(
                     right = 10, top = 150, draggable=FALSE, style = "", 
                     class = "floater",
                     strong("Business Filings of Bankruptcies"),
                     tags$table(
                       mapply(function(from, to, color) {
                         tags$tr(
                           tags$td(tags$div(
                             style = sprintf("width: 16px; height: 16px; background-color: %s;border:1px solid black;", color)
                           )),
                           tags$td(prettyNum(round(from), big.mark = ","), "to", 
                                   prettyNum(round(to), big.mark = ","), align = "right")
                         )
                       }, 
                       buscolorRanges$from, buscolorRanges$to, map_colors[-length(map_colors)],
                       SIMPLIFY=FALSE),
                       tags$tr(
                         tags$td(tags$div(
                           style = sprintf("width: 16px; height: 16px; background-color: %s;", "black")
                         )),
                         tags$td("Data not available", align = "right")))
                   )),
                 
                 ## NonBusiness Legend
                 conditionalPanel(
                   condition="input.map_radio =='NonBusiness Filings' && input.map_nonbus_display == 'NonBusiness_Filings_Total' && input.action != 0",
                   absolutePanel(
                     right = 30, top = 150, draggable=FALSE, style = "",
                     class = "floater",
                     strong("NonBusiness Filings of Bankruptcies"),
                     tags$table(
                       mapply(function(from, to, color) {
                         tags$tr(
                           tags$td(tags$div(
                             style = sprintf("width: 16px; height: 16px; background-color: %s;border:1px solid black;", color)
                           )),
                           tags$td(prettyNum(round(from), big.mark = ","), "to",
                                   prettyNum(round(to), big.mark = ","),  align = "right")
                         )
                       },
                       nonbuscolorRanges$from, nonbuscolorRanges$to, map_colors[-length(map_colors)],
                       SIMPLIFY=FALSE),
                       tags$tr(
                         tags$td(tags$div(
                           style = sprintf("width: 16px; height: 16px; background-color: %s;", "black")
                         )),
                         tags$td("Data not available", align = "right")
                       )
                     )
                   )),
                 ## Proportion by Chapter in Business Filings Bankruptcies Legend
                 conditionalPanel(
                   condition="input.map_radio =='Business Filings' && input.map_bus_display != 'Business_Filings_Total' && input.action != 0",
                   absolutePanel(
                     right = 30, top = 150, draggable=FALSE, style = "",
                     class = "floater",
                     strong("Proportion by Chapter in"),
                     br(),
                     strong("Business Filings Bankruptcies"),
                     tags$table(
                       mapply(function(from, to, color) {
                         tags$tr(
                           tags$td(tags$div(
                             style = sprintf("width: 16px; height: 16px; background-color: %s; border:1px solid black;", color)
                           )),
                           tags$td(prettyNum(round(from), big.mark = ","), "to",
                                   prettyNum(round(to), big.mark = ","),align = "right")
                         )
                       },
                       procolorRanges$from, procolorRanges$to, map_colors[-length(map_colors)],
                       SIMPLIFY=FALSE),
                       tags$tr(
                         tags$td(tags$div(
                           style = sprintf("width: 16px; height: 16px; background-color: %s;", "black")
                         )),
                         tags$td("Data not available", align = "right")
                       )
                     )
                   )
                 ),
                 ## Proportion by Chapter in NonBusiness Filings Bankruptcies
                 conditionalPanel(
                   condition="input.map_radio =='NonBusiness Filings' && input.map_nonbus_display != 'NonBusiness_Filings_Total' && input.action != 0",
                   absolutePanel(
                     right = 10, top = 130, draggable=FALSE, style = "",
                     class = "floater",
                     strong("Proportion by Chapter in"),
                     br(),
                     strong("NonBusiness Filings Bankruptcies"),
                     tags$table(
                       mapply(function(from, to, color) {
                         tags$tr(
                           tags$td(tags$div(
                             style = sprintf("width: 16px; height: 16px; background-color: %s;border:1px solid black;", color)
                           )),
                           tags$td(prettyNum(round(from, 2)), "% to",
                                   prettyNum(round(to, 2)), "%", align = "right")
                         )
                       },
                       procolorRanges$from, procolorRanges$to, map_colors[-length(map_colors)],
                       SIMPLIFY=FALSE),
                       tags$tr(
                         tags$td(tags$div(
                           style = sprintf("width: 16px; height: 16px; background-color: %s;", "black")
                         )),
                         tags$td("Data not available", align = "right")
                       )
                     )
                   )
                 ),
                 
                value="map"),
        
        tabPanel("More Info", 
                 p(strong("Variable Summary:")),
                 tags$br(),
                 p(strong("Business Filings of Bankruptcies"),
                   " - Number of suicides for a region during a specified year.Due to confidentiality constraints, sub-national death counts and rates are suppressed when the number of deaths is less than 10."), 
                 tags$br(),
                 p(strong("Age-adjusted Suicide Rate"), 
                   " - Age-adjusted suicide rates are expressed as the number of suicides, per 100,000 persons, reported each calendar year for the region you select. Rates are considered 'unreliable' when the death count is less than 20 and thus are not displayed. This is calculated by:"),
                 tags$br(),
                 p(strong("Age-adjusted Suicide Rate = Count / Population * 100,000"), align="center"),
                 tags$br(),
                 p(strong("Age-adjusted Suicide Rate Lower Bound"),
                   " - 95% confidence interval lower bound based upon the Age-Adjusted Rate Standard Error (see below)."),
                 tags$br(),
                 p(strong("Age-adjusted Suicide Rate Upper Bound"),
                   " - 95% confidence interval upper bound based upon the Age-adjusted Rate Standard Error (see below)."),
                 tags$br(),
                 p(strong("Age-adjusted Rate Standard Error"),
                   " - The relative standard error for Age-adjusted Rate. The number of suicides represents the complete counts for each region and are not suject to sampling error.  However, they are subject to non-sampling errors. This is calculated by:"),
                 tags$br(),
                 p(strong("Age-adjusted Suicide Rate Standard Error = 100 / sqrt(Suicides)."), align="center"),
                 
                 ## email feedback link
                 h3(a("Please fill out our survey to help improve the site!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank")), value="info"),
        id="tabs"
      )
    )
  )
))
