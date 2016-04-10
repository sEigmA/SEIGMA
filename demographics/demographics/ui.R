#######################################
## Title: Demographics ui.R          ##
## Author(s): Xuelian Li, Arvind     ##
##            Ramakrishnan, Jenna    ##
##            Kiridly, Emily Ramos   ##
## Date Created:  02/28/2015         ##
## Date Modified: 02/28/2015         ##
#######################################

shinyUI(
  fluidPage(
    ## embed the google analytics script in the app
    tags$head(includeScript("google-analytics.js")),
    ## HTML to create generate map button
    gen_map_button,
    ## this starts the googleCharts engine
    googleChartsInit(),
    
    ## blank title, but put in a special title for window tab
    titlePanel("", windowTitle = "SEIGMA: Demographics App"),
    
    ## Create sidebar
    sidebarLayout(
      sidebarPanel(width=4,
                   ## Conditional panel means if the condition is met show all text below otherwise Don't!
                   summary_side_text,
                   plot_side_text,
                   map_side_text,
                   info_side_text,
                   
                   ##in summary, allow for year, catigorical variables,  municipal selection selection
                   conditionalPanel(
                     condition="input.tabs == 'summary'",
                     selectInput("sum_year", "Select Five Year Range",
                                 choices = list("2005-2009" = "2005-2009", "2006-2010" = "2006-2010", "2007-2011" = "2007-2011",
                                                "2008-2012" = "2008-2012")
                     ),
                     radioButtons("sum_radio", "Catigorical variables",
                                  c("Age" = "Age", "Gender" = "Gender",
                                    "Race" = "Race","Ethnicity" ="Ethnicity"),
                                  selected="Age"),
                     selectInput("sum_muni", "Select Municipality",
                                 choices = MA_municipals,
                                 ## Multiple allows for multi-county selection
                                 multiple=TRUE),
                     ##show boxes that will compare to MA or US average,False at the end means it starts off unchecked
                     checkboxInput("MA_mean", "Compare to MA Average", FALSE),
                     checkboxInput("US_mean", "Compare to US Average", FALSE)
                   ),
                   
                   
                   ## in plot, allow for catigorical variables,  municipal selection selection
                   conditionalPanel(
                     condition="input.tabs == 'plot'",
                     ## Select input = List
                     selectInput("plot_year", "Select Five Year Range",
                                 choices = list("2005-2009" = "2005-2009", "2006-2010" = "2006-2010", "2007-2011" = "2007-2011",
                                                "2008-2012" = "2008-2012")
                     ),
                     radioButtons("plot_radio", "Catigorical variables",
                                  c("Age" = "Age", "Gender" = "Gender",
                                    "Race" = "Race","Ethnicity" ="Ethnicity"),
                                  selected="Age"),
                     selectInput("plot_muni", "Select Municipality",
                                 choices = MA_municipals)
                   ),
                   
                   ## In map, 
                   conditionalPanel(
                     condition="input.tabs == 'map'",
                     selectInput("map_year", "Select Five Year Range",
                                 choices = list("2005-2009" = "2005-2009", "2006-2010" = "2006-2010", "2007-2011" = "2007-2011",
                                                "2008-2012" = "2008-2012")
                     ),
                     radioButtons("map_radio", "Catigorical variables",
                                  c("Age" = "Age", "Gender" = "Gender",
                                    "Race" = "Race","Ethnicity" ="Ethnicity"),
                                  selected="Age")
                   ),
                   
                   ## in map, allow for variable selection
                   conditionalPanel(
                     condition="input.tabs == 'map'&& input.map_radio =='Age'",
                     selectInput("var_age", "Select Variable of Interest",
                                 choices = list("under 20"="Age_under_20_Pct_plot",
                                                "20-34"="Age_20-34_Pct_plot",
                                                "35-54"="Age_35-54_Pct_plot",
                                                "55-64"="Age_55-64_Pct_plot",
                                                "65-74"="Age_65-74_Pct_plot",
                                                "over 75"="Age_75+Pct_plot"),
                                 selected = "Age_under_20_Pct_plot")
                   ),
                   
                   conditionalPanel(
                     condition="input.tabs == 'map'&& input.map_radio =='Gender'",
                     selectInput("var_gen", "Select Variable of Interest",
                                 choices = list("Male"="Male_Pct", "Female"="Female_Pct"),
                                 selected = "Female_Pct")
                   ),
                   
                   conditionalPanel(
                     condition="input.tabs == 'map'&& input.map_radio =='Race'",
                     selectInput("var_rac", "Select Variable of Interest",
                                 choices = list("Percent White"="White_Pct", "Percent Black"="Black_Pct", "Percent Asian"="Asian_Pct")
                     )
                   ),
                   
                   conditionalPanel(
                     condition="input.tabs == 'map'&& input.map_radio =='Ethnicity'",
                     selectInput("var_eth", "Select Variable of Interest",
                                 choices = list("Hispanic or Latino"="Hispanic_Pct", "not Hispanic or Latino"="Not_Hispanic_Pct"),
                                 selected = "Not_Hispanic_Pct")
                   ),
                   
                   tags$hr(),
                   
                   ## author line
                   helpText("Created by Xuelian Li, Emily R. Ramos, Arvind Ramakrishnan, and Jenna F. Kiridly"),
                   
                   ## email feedback link
                   ## To develop a link in HTML
                   helpText(a("Send us your comments or feedback!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank",onclick="ga('send', 'event', 'click', 'link', 'feedback', 1)")),
                   
                   ## data source citation
                   helpText(a("Data Source: American Community Survey: table DP05", href="http://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_14_5YR_S2502&prodType=table",
                              target="_blank",onclick="ga('send', 'event', 'click', 'link', 'dataAge', 1)")),
                   
                   ## GitHub link
                   helpText(a("View our data and code on GitHub",
                            href="https://github.com/sEigmA/SEIGMA/tree/gh-pages/demographics/demographics", target="_blank",onclick="ga('send', 'event', 'click', 'link', 'code', 1)")),
                   
                       
    
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
                   condition="input.plot_radio =='Age'",
                   plot_options1,
                   p(strong("Age"),
                     " - The number of categories for age has been collapsed to the following six groups; <20, 20-34, 35-54, 55-64, 65-74,75+.  This is done in order to simplify the presentation of data.  To see all age groups please go to the summary tab.")),
                 conditionalPanel(
                   condition="input.plot_radio =='Gender'||input.plot_radio =='Race'||input.plot_radio =='Ethnicity'",
                   plot_options2),                                
                 ## add text about the variables
                 #                  plot_main_text,
                  
                 
                 
                 value="plot"),
        
        ## plot map
        tabPanel("Map",
                 ## Add a little CSS to make the map background pure white
                 tags$head(tags$style(
                   "#showcase-code-position-toggle, #showcase-sxs-code { display: none; }
.floater { background-color: white; padding: 8px; opacity: 1; border-radius: 6px; box-shadow: 0 0 15px rgba(0,0,0,0.2); }"
                 )),
                 ## Map Creation
                 leafletMap("map", width="100%", height=500,
                            options=list(center = c(42.15, -71.65), zoom=8,
                                         ##Bounds for the map for when zoomed in on mass
                                         maxBounds = list(list(41, -73.5), list(43, -70)))),
                 ## Info Box
                 conditionalPanel(
                   condition="input.action != 0",
                   absolutePanel(left=100, top=450, width=300, class="floater", htmlOutput("details"))),
                 
                 conditionalPanel(
                   condition="input.tabs == 'map' && input.action == 0",
                   ## within the map area, you can create an action button.  similar to initializing the legend but just putting a button instead.
                   absolutePanel(right = 400, top = 300, class = "floater",
                                 actionButton("action", "Generate Map"))
                 ),
                 
                 ## Age Legend
                 conditionalPanel(
                   condition="input.map_radio =='Age' && input.action != 0",
                   absolutePanel(
                     right = 30, top = 150, draggable=FALSE, style = "",
                     class = "floater",
                     strong("Age Percentage"),
                     tags$table(
                       mapply(function(from, to, color) {
                         tags$tr(
                           tags$td(tags$div(
                             style = sprintf("width: 16px; height: 16px; background-color: %s;", color)
                           )),
                           tags$td(prettyNum(round(from, 2)), "% to",
                                   prettyNum(round(to, 2)), "%", align = "right")
                         )
                       },
                       agecolorRanges$from, agecolorRanges$to, map_colors[-length(map_colors)],
                       SIMPLIFY=FALSE),
                       tags$tr(
                         tags$td(tags$div(
                           style = sprintf("width: 16px; height: 16px; background-color: %s;", "#999999")
                         )),
                         tags$td("Data not available", align = "right")
                       )
                     )
                   ),
                  p(strong("Age"),
                     " - The number of categories for age has been collapsed to the following six groups; <20, 20-34, 35-54, 55-64, 65-74,75+.  This is done in order to simplify the presentation of data.  To see all age groups please go to the summary tab.") 
                 ),
                 ## Gender Legend
                 conditionalPanel(
                   condition="input.map_radio =='Gender' && input.action != 0",
                   absolutePanel(
                     right = 30, top = 150, draggable=FALSE, style = "",
                     class = "floater",
                     strong("Sex Percentage"),
                     tags$table(
                       mapply(function(from, to, color) {
                         tags$tr(
                           tags$td(tags$div(
                             style = sprintf("width: 16px; height: 16px; background-color: %s;", color)
                           )),
                           tags$td(prettyNum(round(from, 2)), "% to",
                                   prettyNum(round(to, 2)), "%", align = "right")
                         )
                       },
                       gencolorRanges$from, gencolorRanges$to, map_colors[-length(map_colors)],
                       SIMPLIFY=FALSE),
                       tags$tr(
                         tags$td(tags$div(
                           style = sprintf("width: 16px; height: 16px; background-color: %s;", "#999999")
                         )),
                         tags$td("Data not available", align = "right")
                       )
                     )
                   )),
                 ## Race Legend
                 conditionalPanel(
                   condition="input.map_radio =='Race' && input.action != 0",
                   absolutePanel(
                     right = 30, top = 150, draggable=FALSE, style = "",
                     class = "floater",
                     strong("Race Percentage"),
                     tags$table(
                       mapply(function(from, to, color) {
                         tags$tr(
                           tags$td(tags$div(
                             style = sprintf("width: 16px; height: 16px; background-color: %s;", color)
                           )),
                           tags$td(prettyNum(round(from, 2)), "% to",
                                   prettyNum(round(to, 2)), "%", align = "right")
                         )
                       },
                       racecolorRanges$from, racecolorRanges$to, map_colors[-length(map_colors)],
                       SIMPLIFY=FALSE),
                       tags$tr(
                         tags$td(tags$div(
                           style = sprintf("width: 16px; height: 16px; background-color: %s;", "#999999")
                         )),
                         tags$td("Data not available", align = "right")
                       )
                     )
                   ),
                   p(strong("Race"),
                     " - Race categories are listed here as White, Black, and Asian.  Although the data for other races is available, the percentage is too small to depict in map format accurately.  To view the percentage of other race categories please refer to the Plot or Summary tabs.") 
                 ),
                 ## Ethnicity Legend
                 conditionalPanel(
                   condition="input.map_radio =='Ethnicity' && input.action != 0",
                   absolutePanel(
                     right = 30, top = 150, draggable=FALSE, style = "",
                     class = "floater",
                     strong("Ethnicity Percentage"),
                     tags$table(
                       mapply(function(from, to, color) {
                         tags$tr(
                           tags$td(tags$div(
                             style = sprintf("width: 16px; height: 16px; background-color: %s;", color)
                           )),
                           tags$td(prettyNum(round(from, 2)), "% to",
                                   prettyNum(round(to, 2)), "%", align = "right")
                         )
                       },
                       racecolorRanges$from, racecolorRanges$to, map_colors[-length(map_colors)],
                       SIMPLIFY=FALSE),
                       tags$tr(
                         tags$td(tags$div(
                           style = sprintf("width: 16px; height: 16px; background-color: %s;", "#999999")
                         )),
                         tags$td("Data not available", align = "right")
                       )
                     )
                   )),
                 
                 value="map"
        ),
        
        
        tabPanel("More Info",
                 p(strong("Variable Summary:")),
                 
                 p(strong("Race"),
                   " - The number of people within each race, for a region over a specified five year range.  Races were listed as White, Black or African American, Asian, American Indian or Alaska Native, Native Hawaiian or Other Pacific Islander, or some other race. Within the Map tab race categories are listed here as White, Black, and Asian.  Although the data for other races is available, the percentage is too small to depict in map format accurately.  To view the percentage of other race categories please refer to the Plot or Summary tabs."),
                 tags$br(),
                 p(strong("Ethnicity"),
                   " - The number of people within each ethnicity, for a region over a specified five year range.  Ethnicities were listed as hispanic or not hispanic."),
                 tags$br(),
                 p(strong("Gender"),
                   " - The number of people within each gender, for a region over a specified five year range."),
                 tags$br(),
                 p(strong("Age"),
                   " - The number of people within each age group, for a region over a specified five year range. Age groups were specified as <5, 5-9, 10-14, 15-19, 20-24, 25-34, 35-44, 45-54, 55-59, 60-54, 65-74, 75-84, and 85+. Within the Plot and Map tab the number of categories for age has been collapsed to the following six groups; <20, 20-34, 35-54, 55-64, 65-74,75+.  This is done in order to simplify the presentation of data.  To see all age groups please go to the summary tab."),
                 tags$br(),
                 p(strong("Five-Year Estimate"),
                   "-Survey information is collected everyday of the year and then aggregated over a specific time period, five years.  Multiyear estimates are available to regions with populations less than 65,000.  However, more precise estimates are possible with larger geographic regions. To analyze change over time, users are discouraged from utilizing overlapping multi-year estimates (e.g. 2005-2009, 2006-2010) due to the inability to isolate change with precision."),
                 
                 
                 ## email feedback link
                 h3(a("Please fill out our survey to help improve the site!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank")),
                 value="info"
        ),
        id="tabs"
      ) #tabsetPanel
    ) #mainpanel
  ) #bootstrapPage
)
)
)