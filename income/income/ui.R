#######################################
## Title: Income ui.R                ##
## Author(s): Emily Ramos, Arvind    ##
##            Ramakrishnan, Jenna    ##
##            Kiridly, Steve Lauer   ## 
## Date Created:  11/5/2014          ##
## Date Modified: 11/6/2014          ##
#######################################

shinyUI(fluidPage(
  ## HTML to create generate map button
  gen_map_button,
  ## this starts the googleCharts engine
  googleChartsInit(),
  
  ## blank title, but put in a special title for window tab
  titlePanel("", windowTitle = "SEIGMA: Household Income Shiny App"),
  
  ## Create sidebar
  sidebarLayout(
    sidebarPanel(width=4,
      ## Conditional panel means if the condition is met show all text below otherwise Don't!
      summary_side_text,
      
      plot_side_text,
      
      map_side_text,
      
      info_side_text,
      
      ## in map, allow for variable selection
      
      ## Choose range for year.
      ## Initializing a single slider
      conditionalPanel(
        condition="input.tabs == 'summary' || input.tabs == 'plot' || input.tabs == 'map'",
      selectInput("year", "Select Five Year Range",
                  choices = list("2005-2009" = "2005-2009", "2006-2010" = "2006-2010",
                                 "2007-2011" = "2007-2011"))
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
                    choices = MA_municipals)),
      
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
                 href="https://github.com/sEigmA/SEIGMA/tree/gh-pages/income", target="_blank")),
      
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
                 h4("Average Annual Median Household Income (inflation-adjusted dollars) of Population by Region Over Five Year Period", align="center"),
                 ## make a row to put two charts in
                 div(class = "row",
                     div(muni_plot_options, class = "span6"),
                     div(county_plot_options, class = "span6")
                     ),
                 div(class = "row",
                     div(MA_plot_options, class = "span6"),
                     div(US_plot_options, class = "span6")
                 ),
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
                   condition="input.action != 0",
                   absolutePanel(
                     right = 30, top = 215, draggable=FALSE, style = "", 
                     class = "floater",
#                      strong("Crude Suicide Rate"),
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
   
#                  plot_main_text,
                 value="map"),
        
        tabPanel("More Info", 
                 p(strong("Variable Summary:")),
                 tags$br(),
                  p(strong("Median Household Income (inflation-adjusted dollars"),
                 " - text here"), 
                  tags$br(),

                p(strong("Median Household Income = ******"),align="center"), 
                 
                 ## email feedback link
                 h3(a("Please fill out our survey to help improve the site!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank")), value="info"),
        id="tabs"
      )
    ))
  )
))