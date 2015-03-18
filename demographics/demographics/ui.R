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
    ## HTML to create generate map button
    gen_map_button,
    ## this starts the googleCharts engine
    googleChartsInit(),
    
    ## blank title, but put in a special title for window tab
    titlePanel("", windowTitle = "SEIGMA: Demographics Status Shiny App"),
    
    ## Create sidebar
    sidebarLayout(
      sidebarPanel(width=4,
                   ## Conditional panel means if the condition is met show all text below otherwise Don't!
                   summary_side_text,
                   plot_side_text,
                   map_side_text,
                   info_side_text,
                   
                   ##if multiple years are selected, choose range.
                   ##allow for catigorical variables selection
                   conditionalPanel(
                     condition="input.tabs == 'summary' || input.tabs == 'plot' || input.tabs == 'map'",
                     selectInput("year", "Select Five Year Range",
                                 choices = list("2005-2009" = "2005-2009", "2006-2010" = "2006-2010", "2007-2011" = "2007-2011",
                                                "2008-2012" = "2008-2012")
                     ),
                     radioButtons("radio", "Catigorical variables",
                                  c("Age" = "Age", "Gender" = "Gender",
                                    "Race" = "Race","Ethnicity" ="Ethnicity"),
                                  selected="Age")
                   ),
                   
                   ## in summary, allow for municipal selection
                   conditionalPanel(
                     condition="input.tabs == 'summary'",
                     ## Select input = List
                     selectInput("sum_muni", "Select Municipality",
                                 choices = MA_municipals,
                                 ## Multiple allows for multi-county selection
                                 multiple=TRUE)
                   ),
                   
                   ## in plot, allow for municipal selection
                   conditionalPanel(
                     condition="input.tabs == 'plot'",
                     ## Select input = List
                     selectInput("plot_muni", "Select Municipality",
                                 choices = MA_municipals)
                   ),
                   
                   ## In summary, show boxes that will compare to MA or US average
                   conditionalPanel(
                     condition="input.tabs == 'summary'",
                     ## False at the end means it starts off unchecked
                     checkboxInput("MA_mean", "Compare to MA Average", FALSE),
                     checkboxInput("US_mean", "Compare to US Average", FALSE)
                   ),
                   
                   ## in map, allow for variable selection
                   conditionalPanel(
                     condition="input.tabs == 'map'&& input.radio =='Age'",
                     selectInput("var_age", "Select Variable of Interest",
                                 choices = list("20-24"="20-24_Pct",
                                                "25-34"="25-34_Pct",
                                                "35-44"="35-44_Pct",
                                                "45-54"="45-54_Pct",
                                                "55-59"="55-59_Pct",
                                                "60-64"="60-64_Pct",
                                                "65-74"="65-74_Pct",
                                                "75-84"="75-84_Pct", "over 85"="85+Pct"),
                                 selected = "20-24_Pct")
                   ),
                   
                   conditionalPanel(
                     condition="input.tabs == 'map'&& input.radio =='Gender'",
                     selectInput("var_gen", "Select Variable of Interest",
                                 choices = list("Male"="Male_Pct", "Female"="Female_Pct"),
                                 selected = "Female_Pct")
                   ),
                   
                   conditionalPanel(
                     condition="input.tabs == 'map'&& input.radio =='Race'",
                     selectInput("var_rac", "Select Variable of Interest",
                                 choices = list("Percent White"="White_Pct", "Percent Black"="Black_Pct", "Percent Asian"="Asian_Pct")
                     )
                   ),
                   
                   conditionalPanel(
                     condition="input.tabs == 'map'&& input.radio =='Ethnicity'",
                     selectInput("var_eth", "Select Variable of Interest",
                                 choices = list("Hispanic or Latino"="Hispanic_Pct", "not Hispanic or Latino"="Not_Hispanic_Pct"),
                                 selected = "Not_Hispanic_Pct")
                   ),
                   
                   tags$hr(),
                   
                   ## author line
                   helpText("Created by Xuelian Li, Emily R. Ramos, Arvind Ramakrishnan, and Jenna F. Kiridly"),
                   
                   ## email feedback link
                   ## To develop a link in HTML
                   helpText(a("Send us your comments or feedback!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank")),
                   
                   ## data source citation
                   helpText(a("Data Source: American Community Survey", href="http://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_13_1YR_S1501&prodType=table",
                              target="_blank")),
                   
                   ## GitHub link
                   helpText(a("View our data and code on GitHub",
                              href="https://github.com/sEigmA/SEIGMA/tree/gh-pages/education", target="_blank")),
                   
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
                     plot_options,
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
                       absolutePanel(left=450, top=450, width=300, class="floater", htmlOutput("details"))),
                     
                     conditionalPanel(
                       condition="input.tabs == 'map' && input.action == 0",
                       ## within the map area, you can create an action button.  similar to initializing the legend but just putting a button instead.
                       absolutePanel(right = 400, top = 300, class = "floater",
                                     actionButton("action", "Generate Map"))
                     ),
                     
                     ## Age Legend
                     conditionalPanel(
                       condition="input.radio =='Age' && input.action != 0",
                       absolutePanel(
                         right = 30, top = 215, draggable=FALSE, style = "",
                         class = "floater",
                         strong("Age Rate"),
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
                       )),
                     ## Gender Legend
                     conditionalPanel(
                       condition="input.radio =='Gender' && input.action != 0",
                       absolutePanel(
                         right = 30, top = 215, draggable=FALSE, style = "",
                         class = "floater",
                         strong("Sex Rate"),
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
                       condition="input.radio =='Race' && input.action != 0",
                       absolutePanel(
                         right = 30, top = 215, draggable=FALSE, style = "",
                         class = "floater",
                         strong("Race Rate"),
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
                     ## Ethnicity Legend
                     conditionalPanel(
                       condition="input.radio =='Ethnicity' && input.action != 0",
                       absolutePanel(
                         right = 30, top = 215, draggable=FALSE, style = "",
                         class = "floater",
                         strong("Ethnicity Rate"),
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
                     
                     p(strong("Marital Status Rates"),
                       " - The number of people within each marital status category for a region over a specified five year range. When the number of people in a particular marital status category is too small, data cannot be displayed."),
                     tags$br(),
                     
                     p(strong("Five-Year Estimate"),
                       "-Survey information is collected everyday of the year and then aggregated over a specific time period, five years.  Multiyear estimates are available to regions with populations less than 65,000.  However, more precise estimates are possible with larger geographic regions."),
                     
                     
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