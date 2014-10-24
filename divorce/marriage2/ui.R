#######################################
## Title: Marriage ui.R              ##
## Author(s): Emily Ramos, Arvind    ##
##            Ramakrishnan, Jenna    ##
##            Kiridly, Steve Lauer   ## 
## Date Created:  10/22/2014         ##
## Date Modified: 10/22/2014         ##
#######################################

shinyUI(fluidPage(
  ## HTML to create generate map button
  gen_map_button,
  ## this starts the googleCharts engine
  googleChartsInit(),
  
  ## blank title, but put in a special title for window tab
  titlePanel("", windowTitle = "SEIGMA: Marital Status Shiny App"),
  
  ## Create sidebar
  sidebarLayout(
    sidebarPanel(
      ## Conditional panel means if the condition is met show all text below otherwise Don't!
      summary_side_text,
      
      ## Same concept
      conditionalPanel(
        condition="input.tabs == 'plot'",
        h4("How to use this app:"),
        helpText('Please select a county to analyze.  Multiple counties can be selected to compare the plots of crude suicide rate over time.  Select the Massachusetts and/or US average check boxes to compare county rates with the national and state averages.'),
        tags$hr()
      ),
      
      conditionalPanel(
        condition="input.tabs == 'map'",
        h4("How to use this app:"),
        helpText('Please click on "Generate Map" to get started. When "Single Year" is selected, clicking on a county displays the crude suicide rate for that year. When "Multiple Years" is selected, clicking on a county displays the increase in crude suicide rate over that timespan.'),
        tags$hr()
      ),
      
      conditionalPanel(
        condition="input.tabs == 'info'",
        h4("How to use this app:"),
        helpText('This tab contains more detailed information regarding the variables of interest, including formulae and calculations which were used to derive the crude suicide rate.'),
        tags$hr()
      ),
      
      ## in map, allow for variable selection
      conditionalPanel(
       condition="input.tabs == 'map'",
       selectInput("var", "Select Variable of Interest",
                   choices = list("Never Married" = "Never_Married_Pct", 
                                  "Now Married Except Separated" = "Now_Married_Pct",
                                  "Separated" = "Separated_Pct",
                                  "Widowed" = "Widowed_Pct",
                                  "Divorced" = "Divorced_Pct"))
      ),
      
      ## if single year is selected, select year. if multiple years are selected, choose range.
      ## Initializing a single slider
      selectInput("year", "Select Five Year Range",
                  choices = list("2006-2010" = 2010, "2007-2011" = 2011,
                                 "2008-2012" = 2012)),
      
      ## in summary, allow for gender selection
      conditionalPanel(
       condition="input.tabs == 'summary'",
       selectInput("sum_gender", "Select Gender",
                   choices = list("Female" = "Female", "Male" = "Male"), multiple=TRUE)
      ),
      
      ## in map, allow for gender selection
      conditionalPanel(
       condition="input.tabs == 'map'",
       selectInput("map_gender", "Select Gender",
                   choices = list("Female", "Male"))
      ),
      
      ## in summary or plot, allow for municipal selection
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
                    choices = MA_municipals,
                    ## Multiple allows for multi-county selection
                    multiple=TRUE)),
      
      ## In summary, show boxes that will compare to MA or US average
      conditionalPanel(
        condition="input.tabs == 'summary'",
        ## False at the end means it starts off unchecked
        checkboxInput("MA_mean", "Compare to MA Average", FALSE),
        checkboxInput("US_mean", "Compare to US Average", FALSE)
      ),
      
      tags$hr(),
      
      ## author line
      helpText("Created by Emily R. Ramos, Arvind Ramakrishnan, Jenna F. Kiridly, Sophie E. O'Brien and Stephen A. Lauer"),
      
      ## email feedback link
      ## To develop a link in HTML
      helpText(a("Send us your comments or feedback!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank")),
      
      ## data source citation
      helpText(a("Data Source: CDC Wonder", href="http://wonder.cdc.gov/wonder/help/cmf.html",
                 target="_blank")),
      
      ## GitHub link
      helpText(a("View our data and code on GitHub", 
                 href="https://github.com/sEigmA/SEIGMA/tree/gh-pages/suicide", target="_blank")),
      
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
                 p(strong("The SEIGMA Suicide App"), "displays the crude suicide rate for Massachusetts by counties for a given year or multiple years from 1999 to 2011. Toggle between tabs to visualize the data differently. ", em("Summary"), "shows the source data in a table format. ", em("Plot"), "shows crude suicide rate over time per 100,000 in each county population, Massachusetts average and U.S. average. ", em("Map"), "visually displays crude suicide rate comparatively by county. ", em("More Info"), "lists descriptions for the variables of interest, including formulas and calculations."), value="about"),
        
        ## summary tab
        tabPanel("Summary", 
                 dataTableOutput("summary"), value="summary", 
                 tags$style(type="text/css", '#summary tfoot {display:none;}')),
        
        ## plot tab with google chart options
        tabPanel("Plot",
                 ## make chart title here (otherwise not centered)
                 h4("Crude Suicide Rate Over Time (per 100,000 population)", align="center"),
                 ## make line chart
                 googleLineChart("plot", width="100%", height="475px", options = list(
                   
                   ## set fonts
                   fontName = "Source Sans Pro",
                   fontSize = 14,
                   
                   ## set axis titles, ticks, fonts, and ranges
                   hAxis = list(
                     title = "Year",
                     format = "####",
                     ticks = seq(1999, 2011, 2),
                     textStyle = list(
                       fontSize = 14),
                     titleTextStyle = list(
                       fontSize = 16,
                       bold = TRUE,
                       italic = FALSE)
                   ),
                   vAxis = list(
                     title = "Crude Suicide Rate (per 100,000 population)",
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
                 p(strong("Variable Summary:")),
                 ## breaks between paragraphs
                 tags$br(),
                 p(strong("Suicides"),
                   " - Number of suicides for a specified region in a specific year. Due to confidentiality constraints, sub-national death counts and rates are suppressed when the number of deaths is less than 10."), 
                 tags$br(),
                 p(strong("Crude Rate"), 
                   " - Crude rates are expressed as the number of suicides, per 100,000 persons, reported each calendar year for the region you select. Rates are considered 'unreliable' when the death count is less than 20 and thus are not displayed. This is calculated by:"),
                 tags$br(),
                 p(strong("Crude Rate = Count / Population * 100,000"), align="center"),
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
                     strong("Crude Suicide Rate"),
                     tags$table(
                       mapply(function(from, to, color) {
                         tags$tr(
                           tags$td(tags$div(
                             style = sprintf("width: 16px; height: 16px; background-color: %s;", color)
                           )),
                           tags$td(round(from, 2), "to", round(to, 2))
                         )
                       }, 
                       colorRanges$from, colorRanges$to, map_colors[-length(map_colors)],
                       SIMPLIFY=FALSE)
                     )
                   )),
                 
                 ## Data not available box
                 conditionalPanel(
                   condition="input.action != 0",
                 absolutePanel(
                   right = 350, top = 600, draggable=FALSE, style = "", 
                   class = "floater",
                   tags$table(
                     tags$tr(
                         tags$td(tags$div(
                           style = sprintf("width: 16px; height: 16px; background-color: %s;", "#999999")
                         )),
                         tags$td("Data not available")
                       )
                    )
                 )),
   
                 ## Add text about the variables
                 tags$br(),
                 p(strong("Variable Summary:")),
                 tags$br(),
                 p(strong("Suicides"),
                   " - Number of suicides for a specified region in a specific year. Due to confidentiality constraints, sub-national death counts and rates are suppressed when the number of deaths is less than 10."), 
                 tags$br(),
                 p(strong("Crude Rate"), 
                   " - Crude rates are expressed as the number of suicides, per 100,000 persons, reported each calendar year for the region you select. Rates are considered 'unreliable' when the death count is less than 20 and thus are not displayed. This is calculated by:"),
                 tags$br(),
                 p(strong("Crude Rate = Count / Population * 100,000"), align="center"),
                 value="map"),
        
        tabPanel("More Info", 
                 p(strong("Variable Summary:")),
                 tags$br(),
                 p(strong("Suicides"),
                   " - Number of suicides for a specified region in a specific year. Due to confidentiality constraints, sub-national death counts and rates are suppressed when the number of deaths is less than 10."), 
                 tags$br(),
                 p(strong("Crude Rate"), 
                   " - Crude rates are expressed as the number of suicides, per 100,000 persons, reported each calendar year for the region you select. Rates are considered 'unreliable' when the death count is less than 20 and thus are not displayed. This is calculated by:"),
                 tags$br(),
                 p(strong("Crude Rate = Count / Population * 100,000"), align="center"),
                 tags$br(),
                 p(strong("Crude Rate Lower Bound"),
                   " - 95% confidence interval lower bound based upon the Crude Rate Standard Error (see below)."),
                 tags$br(),
                 p(strong("Crude Rate Upper Bound"),
                   " - 95% confidence interval upper bound based upon the Crude Rate Standard Error (see below)."),
                 tags$br(),
                 p(strong("Crude Rate Standard Error"),
                   " - The relative standard error for Crude Rate. Even though Suicides represents the complete counts for each region, and thus are not subject to sampling error, they are subject to non-sampling errors in the registration process. This is calculated by:"),
                 tags$br(),
                 p(strong("Crude Rate Standard Error = 100 / sqrt(Suicides)."), align="center"),
                 
                 ## email feedback link
                 h3(a("Please fill out our survey to help improve the site!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank")), value="info"),
        id="tabs"
      )
    )
  )
))