#######################################
## Title: Building Permits ui.R      ##
## Author(s): Xuelian Li, Zhenning   ##
##            Kang                   ## 
## Date Created:  08/10/16           ##
## Date Modified: 05/01/18 ZK        ##
#######################################

shinyUI(fluidPage(
  ## HTML to create generate map button
  gen_map_button,
  ## this starts the googleCharts engine
  googleChartsInit(),
  
  ## blank title, but put in a special title for window tab
  titlePanel("", windowTitle = "SEIGMA: Residential Building Permits App"),
  
  ## Create sidebar
  sidebarLayout(
    sidebarPanel(width=4,
                 ## Conditional panel means if the condition is met show all text below otherwise Don't!
                 summary_side_text,
                 
                 plot_side_text,
                 
                 map_side_text,
                 
                 info_side_text,
                 
                 ## in map, allow for variable selection
                 ## in summary, allow for timespan selection
                 ## if single year is selected, select year. if multiple years are selected, choose range.
                 conditionalPanel(
                   condition="input.tabs == 'summary'",
                   ## Select input = Drop down list of timespan (variable name on server side) 
                   selectInput("sum_timespan", "Select Timespan",
                               list("Single Year" = "sing.yr",
                                    "Multiple Years" = "mult.yrs"),selected = "sing.yr"),
                   conditionalPanel(
                     condition="input.sum_timespan == 'sing.yr'",
                     ## Initializing a single slider
                     sliderInput("sum_year", "Select Year",
                                 min=2000, max=2016, value=2016,
                                 sep="")
                   ),
                   conditionalPanel(
                     ## Initializes a multi-year slider (range)
                     condition="input.sum_timespan == 'mult.yrs'",
                     ## Slider starts from 2010-2016
                     sliderInput("sum_range", "Select Years",
                                 min=2000, max=2016, value=c(2010,2016),
                                 sep="")
                   ),
                   ## in summary, allow for municipal selection
                   selectInput("sum_muni", "Select Municipality", 
                               choices = MA_municipals,
                               ## Multiple allows for multi-county selection
                               multiple=TRUE),
                   checkboxInput("sum_MA_mean", "Compare to MA", FALSE)
                   ),
                 
                 ## in plot, allow for municipal selection
                 conditionalPanel(
                   condition="input.tabs == 'plot'",
                   
                   radioButtons("plot_radio", "Select Variable of Interest",
                                c("Total Number of New Housing Units" = "Total_Permits",
                                  "Number of New Housing Units by Structure Size" = "Percent_Permits"),
                                selected="Total_Permits"),
                   conditionalPanel(
                     condition="input.plot_radio == 'Total_Permits'",
                     
                     radioButtons("plot_display_radio", "Display Options",
                                  c("Actual Values"="Total_Units_Reported_Imputed",  "Change From the Previous Year"="Pct_Change_from_previous"),
                                  selected="Total_Units_Reported_Imputed"),
                     ## Select input = List
                     selectInput("plot_muni", "Select Municipality", 
                                 choices = MA_municipals, selected="Everett",multiple=TRUE),
                      conditionalPanel(
                        condition="input.plot_display_radio == 'Pct_Change_from_previous'",
                     ## In plot, show boxes that will compare to MA average
                     checkboxInput("plot_MA", "Compare to MA", FALSE))
                     ),
                   
                   conditionalPanel(
                     condition="input.plot_radio == 'Percent_Permits'",
                     ## Select input = List
                     selectInput("plot_muni2", "Select Municipality", 
                                 choices = MA_municipals, selected="Everett"))
                 ),
                 
                 ## in map, allow for timespan selection
                 conditionalPanel(
                   condition="input.tabs == 'map'",
                   ## Initializing a single slider
                   sliderInput("map_year", "Select Year",
                               min=2000, max=2016, value=2016,
                               sep=""),
                   radioButtons("map_radio", "Select Variable of Interest",
                                c("Total Number of New Housing Units" = "Total_Permits",
                                  "New Housing Units Per 1000 Population"="Permits_Per_1000_Population",
                                  "Percentage of New Housing Units by Structure Size" = "Percent_Permits"),
                                selected="Total_Permits"),
                   conditionalPanel(
                     condition="input.map_radio == 'Total_Permits'",
                     
                     radioButtons("map_display_radio", "Display Options",
                                  c("Actual Values"="Total_Units_Reported_Imputed",  "Change From the Previous Year"="Pct_Change_from_previous"),
                                  selected="Total_Units_Reported_Imputed")),
                   conditionalPanel(
                     condition="input.map_radio == 'Percent_Permits'",
                     radioButtons("map_class_radio", "Percentage of New Housing Units by Structure Size",
                                  c("1 Unit"="Percentage_of_1_Family", "2 Units"="Percentage_of_2_Family",
                                    "3-4 Units"="Percentage_of_3_and_4_Family", "5+ Units"="Percentage_of_5_Family"),
                                  selected="Percentage_of_1_Family"))
                 ),
                 
                 
                 tags$hr(),
                 
                 ## author line
                 
                 helpText("Created by Xuelian Li"),
                 
                 
                 ## email feedback link
                 ## To develop a link in HTML
                 helpText(a("Send us your comments or feedback!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank")),
                 
                 ## data source citation
                 helpText(a("Data Source: MA State Data Center/U.S. Census Building Permit Survey", href="http://www.massbenchmarks.org/statedata/data.htm",
                            target="_blank")),
                 
                 ## GitHub link
                 helpText(a("View the data and code on GitHub", 
                            href="https://github.com/sEigmA/SEIGMA/tree/gh-pages/Residential%20building%20permits", target="_blank")),
                 
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
                 conditionalPanel(
                   condition="input.plot_radio =='Total_Permits'",
                   conditionalPanel(
                     condition="input.plot_display_radio=='Total_Units_Reported_Imputed'", 
                   ## make chart title here (otherwise not centered)
                   h4("Annual Total New Housing Units Authorized by Building Permits by Region over Time", align="center"),
                   TotUni_plot_options),
#                    conditionalPanel(
#                      condition="input.plot_display_radio=='Total_Pct_Change'", 
#                      ## make chart title here (otherwise not centered)
#                      h4("Change in total number of new housing units authorized by building permits since 2000", align="center"),
#                      TotUniCha_plot_options,
#                      p(strong("Change Since 2000"), "- This is calculated by the total annual Housing Permits to the year 2000.  We selected 2000 in order to provide a ten year baseline period.  The baseline year of 2000 is considered '0' for these calculations. A positive number indicates an increase from 2000 and a negative number indicates a decrease from 2000.")
#                    ),
                   conditionalPanel(
                     condition="input.plot_display_radio=='Pct_Change_from_previous'", 
                     ## make chart title here (otherwise not centered)
                     h4("Change in Total New Housing Units from the Previous Year by Region over Time", align="center"),
                     PreUniCha_plot_options)
                 ),
                 
                 conditionalPanel(
                   condition="input.plot_radio =='Percent_Permits'",
                   Pct_plot_options
                     ),
                  value="plot",
                p(strong("Data is based on reported data plus data imputed for non-reporters and partial reporters."))
),
        
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
                   absolutePanel(left=200, top=450, width=300, class="floater",
                                 htmlOutput("details"))),
                 
                 conditionalPanel(
                   condition="input.tabs == 'map' && input.action == 0",
                   ## within the map area, you can create an action button.  similar to initializing the legend but just putting a button instead.
                   absolutePanel(right = 400, top = 300, class = "floater",
                                 actionButton("action", "Generate Map")
                   )),
                 
                 ## total number of new housing units authorized by building permits Legend
                 conditionalPanel(
                   condition="input.map_radio == 'Total_Permits' && input.map_display_radio == 'Total_Units_Reported_Imputed' && input.action != 0",
                   absolutePanel(
                     right = 5, top = 130, draggable=FALSE, style = "", 
                     class = "floater",
                     strong("Total Number"),
                     br(),
                     strong("New Housing Units"),
                     plotOutput("legend1"),
                     tags$table(
                       tags$tr(
                         tags$td(tags$div(
                           style = sprintf("width: 16px; height: 16px; background-color: %s;", "black")
                         )),
                         tags$td("Data not",br(), "available", align = "right")
                       )
                     )
                   )),
                 
                 ## total number of new housing units authorized by building permits Change since 2000 Legend
#                  conditionalPanel(
#                    condition="input.map_radio == 'Total_Permits' && input.map_display_radio == 'Total_Pct_Change' && input.action != 0",
#                    absolutePanel(
#                      right = 5, top = 130, draggable=FALSE, style = "", 
#                      class = "floater",
#                      strong("Change in"),
#                      br(),
#                      strong("total number of new housing units authorized by building permits"),
#                      br(),
#                      strong("Since 2000"),
#                      plotOutput("legend3")
#                    )), 
                 
                 ## total number of new housing units authorized by building permits Change from the Previous Year Legend
                 conditionalPanel(
                   condition="input.map_radio == 'Total_Permits' && input.map_display_radio == 'Pct_Change_from_previous' && input.action != 0",
                   absolutePanel(
                     right = 5, top = 130, draggable=FALSE, style = "", 
                     class = "floater",
                     strong("Change in"),
                     br(),
                     strong("Total Number of"),
                     br(),
                    strong("New Housing Units"),
                     br(),
                     strong("from the"),
                     br(),
                     strong("Previous Year"),
                     tags$table(
                       tags$tr(
                         tags$td(tags$div(
                           style = sprintf("width: 16px; height: 16px; background-color: %s;border:1px solid black;", "maroon")
                         )),
                         tags$td("Increase", align = "right")
                       ),
                       tags$tr(
                         tags$td(tags$div(
                           style = sprintf("width: 16px; height: 16px; background-color: %s;border:1px solid black;", "white")
                         )),
                         tags$td("No Change", align = "right")
                       ),
                       tags$tr(
                         tags$td(tags$div(
                           style = sprintf("width: 16px; height: 16px; background-color: %s;border:1px solid black;", "green")
                         )),
                         tags$td("Decrease", align = "right")
                       ),
                       tags$tr(
                         tags$td(tags$div(
                           style = sprintf("width: 16px; height: 16px; background-color: %s;", "black")
                         )),
                         tags$td("Data not available", align = "right")
                       )
                     )
                   ),
                   p(strong("This is calculated by comparing the annual total number of new housing units authorized by building permits for a specific year to the previous year. A positive number indicates an increase from the previous year and a negative number indicates a decrease from the previous year."))
        ),
                 
                 ## Permits Per 1000 Population Legend
                 conditionalPanel(
                   condition="input.map_radio == 'Permits_Per_1000_Population' && input.action != 0",
                   absolutePanel(
                     right = 5, top = 130, draggable=FALSE, style = "", 
                     class = "floater",
                     strong("Housing Permits"),
                     br(),
                     strong("Per 1000"),
                     br(),
                     strong("Population"),
                     plotOutput("legend5"),
                     tags$table(
                       tags$tr(
                         tags$td(tags$div(
                           style = sprintf("width: 16px; height: 16px; background-color: %s;", "black")
                         )),
                         tags$td("Data not",br(), "available", align = "right")
                       )
                     )
                   ),
                   p(strong("This is calculated by dividing the annual total number of new housing units for a specific year by the population for that year. For years 2000-2009, the estimated population by 2000 census was used. For years 2010-2016, the estimated population by 2010 census was used."))
        ),
                 
                 ## Percent by structure Legend
                 conditionalPanel(
                   condition="input.map_radio == 'Percent_Permits' && input.action != 0",
                   absolutePanel(
                     right = 5, top = 130, draggable=FALSE, style = "", 
                     class = "floater",
                     strong("Percent of"),
                     br(),
                     strong("New Housing Units"),
                     br(),
                     strong("by Structure Size"),
                     plotOutput("legend2"),
                     tags$table(
                       tags$tr(
                         tags$td(tags$div(
                           style = sprintf("width: 16px; height: 16px; background-color: %s;", "black")
                         )),
                         tags$td("Data not",br(), "available", align = "right")
                       )
                     )
                   )),
                 
                  #plot_main_text,
                 value="map",
                p(strong("Data is based on reported data plus data imputed for non-reporters and partial reporters."))
        ),
        
        tabPanel("More Info", 
                 p(strong("Variable Summary:")),
                 tags$br(),
                
                   
                 strong("Annual residential building permits-"),
                 " Data are obtained from the U.S. Census Bureau's Survey of Construction. Building permits data are collected from individual permit offices, most of which are municipalities. The statistics are based on reports submitted by local building permit officials in response to a mail survey and imputed data.",
                 
                 tags$br(),
                 tags$br(),
                 p(strong('Class- Single family unit'), 
                   "-means that the building is usually occupied by just one household or family, and consists of just one dwelling unit or suite."),
                 
                 tags$br(),
                   p(strong('Class - Multi-family units'),
                             "-is a classification of housing where multiple separate housing units for residents are contained within one building or several buildings within one complex. "),
                 tags$br(),
                 p(strong('2016 Dollars'), 
                          "-Due to inflation, the purchasing power of the dollar changes over time. In order to compare monetary values from one year to another, we convert  them from current dollar values to constant dollar values. For this app we used the dollar values from 2016 to be our constant."),
                 tags$br(),
                 
                 p(strong("Total number of new housing units per 1000 inhabitants-"),
                 " is calculated by dividing the annual total number of new housing units for a specific year by the population for that year. For the years 2000-2009, we used the estimated population size from the 2000 census. For years 2010-2016, the estimated population size from the  2010 census was used."),
                 
                 ## email feedback link
                 h3(a("Please fill out our survey to help improve the site!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank")), value="info"),
        id="tabs")) #ends tableset panel
      ))#end bootstrap page
  ))