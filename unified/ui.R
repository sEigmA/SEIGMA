#unified UI


shinyUI(
  fluidPage(
    ## embed the google analytics script in the app
    tags$head(includeScript("google-analytics.js")),
    
        ## blank title, but put in a special title for window tab
    titlePanel("", windowTitle = "SEIGMA: Data App"),
    
    ## Create sidebar
    sidebarLayout(
      sidebarPanel(width=4,
                   ## Conditional panel means if the condition is met show all text below otherwise Don't!
                   
                   HTML("<center><h1>What's going on in my community?</h1></center>"),
                   
                   radioButtons("how_onemuniselect", "Choose a municpality from the list or from a map",
                                c("plot" = "plot", "Map" = "Map"),
                                selected="plot"),
                   
                   conditionalPanel(
                     condition="input.how_onemuniselect == 'plot'",
                     ## Select input = List
                     
                     selectInput("one_muni", "Select Municipality",
                                 choices = MA_municipals, multiple = T),
                     checkboxInput("MA_mean", "Compare to MA Median", TRUE),
                     checkboxInput("US_mean", "Compare to US Median", FALSE)
                     
                   
                   ),
                   
                   conditionalPanel("input.how_onemuniselect=='Map'",
                                    HTML("Click on a municipality"),
                                    
                                    leafletOutput("map")
                                    
                                    
                   ),
                   tags$hr(),
                   HTML("<center>OR</center>"),
                   
                   tags$hr(),
                   
                   # selectInput("multi_muni", "Select multiple municipalities",
                   #             choices = MA_municipals, multiple=TRUE),
                   
                   
                   
                   tags$hr(),
                   
                   ## author line
                   helpText("Created by Justin Baldwin"),
                   
                   ## email feedback link
                   ## To develop a link in HTML
                   helpText(a("Send us your comments or feedback!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank",onclick="ga('send', 'event', 'click', 'link', 'feedback', 1)")),
                   
                   ## data source citation
                   helpText(a("Data Source: American Community Survey: table DP05", href="http://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_14_5YR_S2502&prodType=table",
                              target="_blank",onclick="ga('send', 'event', 'click', 'link', 'dataAge', 1)")),
                   
                   ## GitHub link
                   helpText(a("View our data and code on GitHub",
                              href="https://github.com/sEigmA/SEIGMA/tree/gh-pages/unified", target="_blank",onclick="ga('send', 'event', 'click', 'link', 'code', 1)")),
                   
                   
                   
                   helpText("If using Internet Explorer, application only visible in version 10.")
      ),
      ######### End of Sidebar  #########
      
      ######### Start of Main Panel #####
      
      bootstrapPage(
        mainPanel(
          ## put in logo for title
          a(img(src = "logo.jpg", height=105, width=920), href="http://www.umass.edu/seigma/"),
          
          ## create tabs
          tabsetPanel(
            
            
            
            tabPanel("Plot",
                     ## make chart title here (otherwise not centered)

                     plotOutput("colplot", click="plot_click"),
                     
                     plotOutput("tsplot"),
                     
                     #htmlOutput("counties"),
 
                     value="plot"),
            
            ## summary tab
            tabPanel("Data Table",
                     
                     HTML("Some column selector"),
                     HTML("can this be interactive ? https://shiny.rstudio.com/gallery/word-cloud.html"),
                     
                     
                     
                     tags$style(type="text/css", '#summary tfoot {display:none;}'),
                     value = "summary"),
            
            ## plot tab with google chart options
            
            # ## plot map
            # tabPanel("Map",
            #          ## Add a little CSS to make the map background pure white
            #          tags$head(tags$style(
            #            "#showcase-code-position-toggle, #showcase-sxs-code { display: none; }
            #            .floater { background-color: white; padding: 8px; opacity: 1; border-radius: 6px; box-shadow: 0 0 15px rgba(0,0,0,0.2); }"
            #          )),
            #          ## Map Creation
            #          leafletMap("map", width="100%", height=500,
            #                     options=list(center = c(42.15, -71.65), zoom=8,
            #                                  ##Bounds for the map for when zoomed in on mass
            #                                  maxBounds = list(list(41, -73.5), list(43, -70)))),
            #          ## Info Box
            #          conditionalPanel(
            #            condition="input.action != 0",
            #            absolutePanel(left=100, top=450, width=300, class="floater", htmlOutput("details"))),
            #          
            #          conditionalPanel(
            #            condition="input.tabs == 'map' && input.action == 0",
            #            ## within the map area, you can create an action button.  similar to initializing the legend but just putting a button instead.
            #            absolutePanel(right = 400, top = 300, class = "floater",
            #                          actionButton("action", "Generate Map"))
            #          ),
            #          
            #          ## Age Legend
            #          conditionalPanel(
            #            condition="input.action != 0",
            #            absolutePanel(
            #              right = 10, top = 100, draggable=FALSE, style = "",
            #              class = "floater",
            #              strong(textOutput("text1")),
            #              strong("Percentage"),
            #              plotOutput("legend1"),
            #              tags$table(
            #                tags$tr(
            #                  tags$td(tags$div(
            #                    style = sprintf("width: 16px; height: 16px; background-color: %s;", "#999999")
            #                  )),
            #                  tags$td("Data not", br(),"available", align = "right")
            #                )
            #              )
            #            )),
            #          ## Gender Legend
            #          conditionalPanel(
            #            condition="input.map_radio =='Age' && input.action != 0",
            #            p(strong("Age"),
            #              " - The number of categories for age has been collapsed to the following six groups; <20, 20-34, 35-54, 55-64, 65-74,75+.  This is done in order to simplify the presentation of data.  To see all age groups please go to the summary tab.") 
            #          ),
            #          ## Race Legend
            #          conditionalPanel(
            #            condition="input.map_radio =='Race' && input.action != 0",
            #            #absolutePanel(
            #            #  right = 30, top = 150, draggable=FALSE, style = "",
            #            #  class = "floater",
            #            #  strong("Race Percentage"),
            #            #  tags$table(
            #            #
            #            #    tags$tr(
            #            #     tags$td(tags$div(
            #            #       style = sprintf("width: 16px; height: 16px; background-color: %s;", "#999999")
            #            # )),
            #            # tags$td("Data not available", align = "right")
            #            # )
            #            # )
            #            # ),
            #            p(strong("Race"),
            #              " - Race categories are listed here as White, Black, and Asian.  Although the data for other races is available, the percentage is too small to depict in map format accurately.  To view the percentage of other race categories please refer to the Plot or Summary tabs.") 
            #          ),
            #          ## Ethnicity Legend
            #          #                  conditionalPanel(
            #          #                    condition="input.map_radio =='Ethnicity' && input.action != 0",
            #          #                    absolutePanel(
            #          #                      right = 30, top = 150, draggable=FALSE, style = "",
            #          #                      class = "floater",
            #          #                      strong("Ethnicity Percentage"),
            #          #                      tags$table(
            #          #                        mapply(function(from, to, color) {
            #          #                          tags$tr(
            #          #                            tags$td(tags$div(
            #          #                              style = sprintf("width: 16px; height: 16px; background-color: %s;", color)
            #          #                            )),
            #          #                            tags$td(prettyNum(round(from, 2)), "% to",
            #          #                                    prettyNum(round(to, 2)), "%", align = "right")
            #          #                          )
            #          #                        },
            #          #                        racecolorRanges$from, racecolorRanges$to, map_colors[-length(map_colors)],
            #          #                        SIMPLIFY=FALSE),
            #          #                        tags$tr(
            #          #                          tags$td(tags$div(
            #          #                            style = sprintf("width: 16px; height: 16px; background-color: %s;", "#999999")
            #          #                          )),
            #          #                          tags$td("Data not available", align = "right")
            #          #                        )
            #          #                      )
            #          #                    )),
            #          
            #          value="map"
            #          ),
            
            
            # tabPanel("More Info",
            #          p(strong("Variable Summary:")),
            #          
            #          p(strong("Race"),
            #            " - The number of people within each race, for a region over a specified five year range.  Races were listed as White, Black or African American, Asian, American Indian or Alaska Native, Native Hawaiian or Other Pacific Islander, or some other race. Within the Map tab race categories are listed here as White, Black, and Asian.  Although the data for other races is available, the percentage is too small to depict in map format accurately.  To view the percentage of other race categories please refer to the Plot or Summary tabs."),
            #          tags$br(),
            #          p(strong("Ethnicity"),
            #            " - The number of people within each ethnicity, for a region over a specified five year range.  Ethnicities were listed as hispanic or not hispanic."),
            #          tags$br(),
            #          p(strong("Gender"),
            #            " - The number of people within each gender, for a region over a specified five year range."),
            #          tags$br(),
            #          p(strong("Age"),
            #            " - The number of people within each age group, for a region over a specified five year range. Age groups were specified as <5, 5-9, 10-14, 15-19, 20-24, 25-34, 35-44, 45-54, 55-59, 60-54, 65-74, 75-84, and 85+. Within the Plot and Map tab the number of categories for age has been collapsed to the following six groups; <20, 20-34, 35-54, 55-64, 65-74,75+.  This is done in order to simplify the presentation of data.  To see all age groups please go to the summary tab."),
            #          tags$br(),
            #          p(strong("Five-Year Estimate"),
            #            "-Survey information is collected everyday of the year and then aggregated over a specific time period, five years.  Multiyear estimates are available to regions with populations less than 65,000.  However, more precise estimates are possible with larger geographic regions. To analyze change over time, users are discouraged from utilizing overlapping multi-year estimates (e.g. 2005-2009, 2006-2010) due to the inability to isolate change with precision."),
            #          
            #          
            #          ## email feedback link
            #          h3(a("Please fill out our survey to help improve the site!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank")),
            #          value="info"
            # ),
            id="tabs"
          ) #tabsetPanel
      ) #mainpanel
    ) #bootstrapPage
    )
      )
)