#######################################
## Title: Marital ui.R               ##
## Author(s): Emily Ramos, Arvind    ##
##            Ramakrishnan, Jenna    ##
##            Kiridly, Steve Lauer,  ##
##            Xuelian Li, Justin     ##
##            Baldwin                ##
## Date Created:  10/22/2014         ##
## Date Modified: 02/14/2019  VE     ##
#######################################

shinyUI(fluidPage(
  ## embed the google analytics script in the app
  tags$head(includeScript("google-analytics.js")),
  ## embed the googleCharts Init script to use version 43 (frozen version) to fixed the bug (2 charts in one page)
  tags$head(tags$script(src="https://www.google.com/jsapi")),
  tags$head(tags$script(src="https://www.gstatic.com/charts/loader.js")),
  tags$head(includeScript("googleChartInit.js")),
  tags$head(tags$script(src="bindings.js")),
  ## HTML to create generate map button
  gen_map_button,
  ## this starts the googleCharts engine
  ##googleChartsInit(),
  
  ## blank title, but put in a special title for window tab
  titlePanel("", windowTitle = "SEIGMA: Marital Status Shiny App"),
  
  ## Create sidebar
  sidebarLayout(
    sidebarPanel(width=4,

      ## Conditional panel means if the condition is met show all text below otherwise Don't!
      summary_side_text,
      
      plot_side_text,
      
      map_side_text,
      
      info_side_text,
      
      ## in map, allow for variable selection
      conditionalPanel(
       condition="input.tabs == 'map'"
       ,

       selectInput("map_year", "Select Five Year Range",
                   choices = list("2006-2010" = "2006-2010",
                                  "2007-2011" = "2007-2011",
                                  "2008-2012" = "2008-2012",
                                  "2009-2013" = "2009-2013",
                                  "2010-2014" = "2010-2014",
                                  "2011-2015" = "2011-2015", 
                                  "2012-2016" = "2012-2016", 
                                  "2013-2017" = "2013-2017")),
       selectInput("map_gender", "Select Gender",
                   choices = list("Female", "Male")),
       selectInput("var", "Select Variable of Interest",
                   choices = list("Never Married" = "Never_Married_pct",
                                  "Married" = "Married_pct",
                                  "Separated" = "Separated_pct",
                                  "Widowed" = "Widowed_pct",
                                  "Divorced" = "Divorced_pct")),
      checkboxInput("lmap_cas", "Display Casinos", value=TRUE)
      ,actionButton("action2", "PUSH THE BUTTON REDRAW THE MAP")

      
              
      ),
      
      ## if single year is selected, select year. if multiple years are selected, choose range.
      ## Initializing a single slider
      conditionalPanel(
        condition="input.tabs == 'plot'",
      selectInput("plotvar", "Select Variable of Interest",
                    choices = list("Never Married" = "Never_Married_pct", 
                                   "Married" = "Married_pct",
                                   "Separated" = "Separated_pct",
                                   "Widowed" = "Widowed_pct",
                                   "Divorced" = "Divorced_pct")),
        
      selectInput("plot_muni", "Select Municipality", 
                  choices = c(MA_municipals), multiple = TRUE, selected = "MA"),
      checkboxInput("plotMA_mean", "Compare to MA Average", TRUE),
      checkboxInput("plotUS_mean", "Compare to US Average", FALSE),
      selectInput("plotcombine", "Show Plots by Gender", choices = list("Separated" = "Separate", 
                                                                         "Together" = "Together"))
      
      
      ),
      
      ## in summary, allow for gender selection
      conditionalPanel(
       condition="input.tabs == 'summary'",

       selectInput("sum_year", "Select Five Year Range",
                   choices = list("2006-2010" = "2006-2010", 
                                  "2007-2011" = "2007-2011",
                                  "2008-2012" = "2008-2012",
                                  "2009-2013" = "2009-2013",
                                  "2010-2014" = "2010-2014",
                                  "2011-2015" = "2011-2015", 
                                  "2012-2016" = "2012-2016", 
                                  "2013-2017" = "2013-2017"),
                   multiple = TRUE),
       selectInput("sum_gender", "Select Gender",
                   choices = list("Female" = "Female", "Male" = "Male"), multiple=TRUE),
       selectInput("sum_muni", "Select Municipality", 
                   choices = MA_municipals,
                   ## Multiple allows for multi-county selection
                   multiple=TRUE),
      
       ## False at the end means it starts off unchecked
         checkboxInput("MA_mean", "Compare to MA Average", FALSE),
         checkboxInput("US_mean", "Compare to US Average", FALSE)
       
      ),
      
        
      ## in plot, allow for municipal selection
            
      ## In summary, show boxes that will compare to MA or US average
            
      tags$hr(),
      
      ## author line
      helpText("Created by Emily R. Ramos, Arvind Ramakrishnan, Jenna F. Kiridly, Xuelian Li, Justin Baldwin and Stephen A. Lauer"),
      
      ## email feedback link
      ## To develop a link in HTML
      helpText(a("Send us your comments or feedback!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank")),
      
      ## data source citation
      helpText(a("Data Source: American Community Survey - Table DP02", href="http://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_14_5YR_S1201&prodType=table",
                 target="_blank")),
      
      ## GitHub link
      helpText(a("View our data and code on GitHub", 
                 href="https://github.com/sEigmA/SEIGMA/tree/gh-pages/marital/marital", target="_blank")),
      
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
                 dataTableOutput("summary"),
                 tags$br(),
                 HTML("Population includes individuals aged 15 years and older."),
                 tags$br(),
                 value="summary", 
                 tags$style(type="text/css", '#summary tfoot {display:none;}')),
        
        ## plot tab with google chart options
        tabPanel("Plot",
                 ## make chart title here (otherwise not centered)
                 conditionalPanel(
                   condition="input.plotcombine=='Separate'",
                                  h4("Marital Status as a Percentage of the Population by  Gender", align="center"),                 
                                  
                 ## make a row to put two charts in
                 
                                  plotOutput("fplot"), 
                                  plotOutput("mplot")
                 )
                 ,
                 conditionalPanel(condition = "input.plotcombine=='Together'",
                                  h4("Marital Status as a Percentage of the Population by  Gender", align="center"),                 
                   plotOutput("fmplot")
                                  ),
                 # textOutput("ordermunis2"),
                 # textOutput("ordermunis")
                 # 
                 # ,
                 HTML("Horizontal grey bars indicate the span of five-year estimates, vertical grey bars with hinges indicate the standard errors. Population includes individuals aged 15 years and older."),
                 
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
                 
                  leafletOutput("map"),
                 
                 tableOutput("map_tab")
                 
                 # Map Creation
                 # verbatimTextOutput("mapdata1"),
                 # verbatimTextOutput("mapdata2"),
                 # verbatimTextOutput("mapdata3"),

                 ## Info Box 
                 # conditionalPanel(
                 #   condition="input.action != 0",
                 #   absolutePanel(left=100, top=450, width=300, class="floater",
                 #                 htmlOutput("details"),
                 #                 leafletOutput("map", width = "100%", height = "100%")
                 #                 )),
                 # 
                 # conditionalPanel(
                 #   condition="input.tabs == 'map' && input.action == 0",
                 #   ## within the map area, you can create an action button.  similar to initializing the legend but just putting a button instead.
                 #   absolutePanel(right = 400, top = 300, class = "floater",
                 #                 actionButton("action", "Generate Map")
                 #   )),
                 
                 ## Map Legend
                 # conditionalPanel(
                 #   condition="input.action != 0",
                 #   absolutePanel(
                 #     right = 10, top = 100, draggable=FALSE, style = "", 
                 #     class = "floater",
                 #     strong("Percent"),
                 #     strong(textOutput("text1")),
                 #     plotOutput("legend1"),
                 #     tags$table(
                 #      tags$tr(
                 #         tags$td(tags$div(
                 #           style = sprintf("width: 16px; height: 16px; background-color: %s;", "#999999")
                 #         )),
                 #         tags$td("Data not", br(), "available", align = "right")))
                 #   )),
                 # 
                 # ## Never Married Legend
                 # conditionalPanel(
                 #   condition="input.var == 'Never_Married_pct' && input.action != 0",
                 #   absolutePanel(
                 #     right = 30, top = 215, draggable=FALSE, style = "",
                 #     class = "floater",
                 #     strong("Percent Never Married"),
                 #     tags$br(),
                 #     #strong("Age Adjusted Suicide Rate"),
                 #     tags$table(
                 #       mapply(function(from, to, color) {
                 #         tags$tr(
                 #           tags$td(tags$div(
                 #             style = sprintf("width: 16px; height: 16px; background-color: %s;", color)
                 #           )),
                 #           tags$td(round(from, 2),"%", "to", round(to, 2),"%", align = "right")
                 #         )
                 #       },
                 #       nevcolorRanges$from, nevcolorRanges$to, map_colors[-length(map_colors)],
                 #       SIMPLIFY=FALSE),
                 #       tags$tr(
                 #         tags$td(tags$div(
                 #           style = sprintf("width: 16px; height: 16px; background-color: %s;", "#999999")
                 #         )),
                 #         tags$td("Data not available", align = "right")))
                 #   )),
                 # 
                 # ## Separated Legend
                 # conditionalPanel(
                 #   condition="input.var == 'Separated_pct' && input.action != 0",
                 #   absolutePanel(
                 #     right = 30, top = 215, draggable=FALSE, style = "",
                 #     class = "floater",
                 #     strong("Percent Separated"),
                 #     tags$br(),
                 #     #strong("Age Adjusted Suicide Rate"),
                 #     tags$table(
                 #       mapply(function(from, to, color) {
                 #         tags$tr(
                 #           tags$td(tags$div(
                 #             style = sprintf("width: 16px; height: 16px; background-color: %s;", color)
                 #           )),
                 #           tags$td(round(from, 2),"%", "to", round(to, 2),"%", align = "right")
                 #         )
                 #       },
                 #       sepcolorRanges$from, sepcolorRanges$to, map_colors[-length(map_colors)],
                 #       SIMPLIFY=FALSE),
                 #       tags$tr(
                 #         tags$td(tags$div(
                 #           style = sprintf("width: 16px; height: 16px; background-color: %s;", "#999999")
                 #         )),
                 #         tags$td("Data not available", align = "right")))
                 #   )),
                 # 
                 # 
                 # ## Widowed Legend
                 # conditionalPanel(
                 #   condition="input.var == 'Widowed_pct' && input.action != 0",
                 #   absolutePanel(
                 #     right = 30, top = 215, draggable=FALSE, style = "",
                 #     class = "floater",
                 #     strong("Percent Widowed"),
                 #     tags$br(),
                 #     #strong("Age Adjusted Suicide Rate"),
                 #     tags$table(
                 #       mapply(function(from, to, color) {
                 #         tags$tr(
                 #           tags$td(tags$div(
                 #             style = sprintf("width: 16px; height: 16px; background-color: %s;", color)
                 #           )),
                 #           tags$td(round(from, 2),"%", "to", round(to, 2),"%", align = "right")
                 #         )
                 #       },
                 #       widcolorRanges$from, widcolorRanges$to, map_colors[-length(map_colors)],
                 #       SIMPLIFY=FALSE),
                 #       tags$tr(
                 #         tags$td(tags$div(
                 #           style = sprintf("width: 16px; height: 16px; background-color: %s;", "#999999")
                 #         )),
                 #         tags$td("Data not available", align = "right")))
                 #   )),
                 # 
                 # ## Divorced Legend
                 # conditionalPanel(
                 #   condition="input.var == 'Divorced_pct' && input.action != 0",
                 #   absolutePanel(
                 #     right = 30, top = 215, draggable=FALSE, style = "",
                 #     class = "floater",
                 #     strong("Percent Divorced"),
                 #     tags$br(),
                 #     #strong("Age Adjusted Suicide Rate"),
                 #     tags$table(
                 #       mapply(function(from, to, color) {
                 #         tags$tr(
                 #           tags$td(tags$div(
                 #             style = sprintf("width: 16px; height: 16px; background-color: %s;", color)
                 #           )),
                 #           tags$td(round(from, 2),"%", "to", round(to, 2),"%", align = "right")
                 #         )
                 #       },
                 #       divcolorRanges$from, divcolorRanges$to, map_colors[-length(map_colors)],
                 #       SIMPLIFY=FALSE),
                 #       tags$tr(
                 #         tags$td(tags$div(
                 #           style = sprintf("width: 16px; height: 16px; background-color: %s;", "#999999")
                 #         )),
                 #         tags$td("Data not available", align = "right")))
                 #   )),

#tableOutput("maptab"),
                  ,
                 value="map"),
        
        tabPanel("More Info", 
                 p(strong("Variable Summary:")),

                 p(strong("Marital Status Rates"),
                 " - The number of people within each marital status category for a region over a specified five year range. When the number of people in a particular marital status category is too small, data cannot be displayed."), 
                  tags$br(),

                 p(strong("Five-Year Estimate"),
                 " - Survey information is collected everyday of the year and then aggregated over a specific time period, five years.  Multiyear estimates are available for regions with populations less than 65,000.  However, more precise estimates are possible with larger geographic regions.  To analyze change over time, users are dicouraged from utilizing overlapping multi-year estimates (e.g. 2005-2009, 2006-2010) due to the inability to isolate change with precision."),
                
                 
                 ## email feedback link
                 h3(a("Please fill out our survey to help improve the site!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank")), value="info"),
        id="tabs"
      )
    ))
  )
))