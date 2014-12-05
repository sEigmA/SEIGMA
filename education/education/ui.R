#######################################
## Title: Education ui.R              ##
## Author(s): Emily Ramos, Arvind    ##
##            Ramakrishnan, Jenna    ##
##            Kiridly, Steve Lauer   ## 
## Date Created:  12/4/14            ##
## Date Modified: 12/4/14            ##
#######################################

shinyUI(fluidPage(
  ## HTML to create generate map button
  gen_map_button,
  ## this starts the googleCharts engine
  googleChartsInit(),
  
  ## blank title, but put in a special title for window tab
  titlePanel("", windowTitle = "SEIGMA: Educational Status Shiny App"),
  
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
       condition="input.tabs == 'map'",
       selectInput("var", "Select Variable of Interest",
                   choices = list("High School Diploma or Higher" = "HS_Pct", 
                                  "Bachelors Degree or Higher" = "Bachelors_Pct",
                                  "Graduate or Professional Degree" = "Grad_Pct"))
      ),
      
      ## if single year is selected, select year. if multiple years are selected, choose range.
      ## Initializing a single slider
      conditionalPanel(
        condition="input.tabs == 'summary' || input.tabs == 'plot' || input.tabs == 'map'",
      selectInput("year", "Select Five Year Range",
                  choices = list("2006-2010" = "2006-2010", "2007-2011" = "2007-2011",
                                 "2008-2012" = "2008-2012"))
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
      helpText("Created by Emily R. Ramos, Arvind Ramakrishnan, Jenna F. Kiridly, and Stephen A. Lauer"),
      
      ## email feedback link
      ## To develop a link in HTML
      helpText(a("Send us your comments or feedback!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank")),
      
      ## data source citation
      helpText(a("Data Source: American Community Survey", href="http://www.census.gov/acs/www/",
                 target="_blank")),
      
      ## GitHub link
      helpText(a("View our data and code on GitHub", 
                 href="https://github.com/sEigmA/SEIGMA/tree/gh-pages/education", target="_blank")),
      
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
                 h4("Edcuational Status as a Percentage of the Population by Region and Gender", align="center"),
                 plot_options,
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
                 
                 ## Legend
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
                           tags$td(prettyNum(round(from, 2)), "% to", 
                                   prettyNum(round(to, 2)), "%", align = "right")
                         )
                       }, 
                       colorRanges$from, colorRanges$to, map_colors[-length(map_colors)],
                       SIMPLIFY=FALSE),
                       tags$tr(
                         tags$td(tags$div(
                           style = sprintf("width: 16px; height: 16px; background-color: %s;", "#999999")
                         )),
                         tags$td("Data not available", align = "right")
                       )
                     )
                   )),
   
#                  plot_main_text,
                 value="map"),
        
        tabPanel("More Info", 
                 p(strong("Variable Summary:")),
                 tags$br(),
                  p(strong("Edcuation Rates"),
                 " - Number of people within each educational status category for a specific region over a specified five year range. No data for any municipality indicates that data cannot be displayed because the number of cases is too small. This calculated by:"), 
                  tags$br(),

                p(strong("Education Rate= Total in education group for five year range/ Total Population * 100,000"),align="center"), 
                 
                 ## email feedback link
                 h3(a("Please fill out our survey to help improve the site!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank")), value="info"),
        id="tabs"
      )
    ))
  )
))