###########################################
## Title: employment Data Cleaning       ##
## Author(s): Xuelian Li, Emily Ramos,   ## 
##            Arvind Ramakrishnan,       ##
##            Jenna Kiridly              ## 
## Date Created:  02/04/2015             ##
## Date Modified: 07/13/2015             ##
###########################################


shinyUI(fluidPage(
  ## embed the google analytics script in the app
  tags$head(includeScript("google-analytics.js")),
  ## HTML to create generate map button
  gen_map_button,
  ## this starts the googleCharts engine
  googleChartsInit(),
  
  ## blank title, but put in a special title for window tab
  titlePanel("", windowTitle = "SEIGMA: Employment Monthly Shiny App"),
  
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

                   selectInput("sum_timespan", "Select Timespan",
                               list("Single Year" = "sing.yr",
                                    "Multiple Years" = "mult.yrs"), selected="sing.yr"),
                   ## if single year is selected, select year. if multiple years are selected, choose range.
                   conditionalPanel(
                     condition="input.sum_timespan == 'sing.yr'",
                      ## Initializing a single slider
                     sliderInput("sum_year", "Select Year",
                                 min=2003, max=2012, value=2012,
                                 sep="")
                   ),
                   conditionalPanel(
                     ## Initializes a multi-year slider (range)
                     condition="input.sum_timespan == 'mult.yrs'",
                     ## Slider starts from 2010-2012
                     sliderInput("sum_range", "Select Years",
                                 min=2003, max=2012, value=c(2010,2012),
                                 sep="")
                   ),
                   
                   selectInput("sum_muni", "Select Municipality", 
                                 choices = MA_municipals,
                                 ## Multiple allows for multi-county selection
                                 multiple=TRUE)
                          
                   ),
                 
                 
                 ## in map, allow for variable , option and year selection
                  conditionalPanel(
                   condition="input.tabs == 'map'",
                   radioButtons("map_radio", "Select Variable of Interest",
                                c("Employment"="Employment", "Establishments" = "Establishments", 
                                  "Wages" = "Wages"),
                                selected="Employment"),
                   radioButtons("map_display_radio", "Display Options",
                                c("Actual Values"="Actual Values", "Difference Compared to Year 2003"="Change_Pct"),
                                selected="Actual Values"),
                   sliderInput("map_year", "Select Year",
                                 min=2003, max=2012, value=2012,
                                 sep="")
                   
                 ),
                 
                 
                 
                 ## in plot, allow for municipal selection
                 conditionalPanel(
                   condition="input.tabs == 'plot'",
                   ## Select input = List
                   selectInput("plot_muni", "Select Municipality", 
                               choices = MA_municipals, multiple=TRUE),
                   radioButtons("plot_radio", "Select Variable of Interest",
                                c("Employment and Establishments" = "Employment and Establishments", 
                                  "Wages" = "Wages"),
                                selected="Employment and Establishments"),
                   radioButtons("plot_display_radio", "Display Options",
                                c("Actual Values"="Actual Values", "Change Since 2003"="Change_Pct"),
                                selected="Actual Values")
                   ),
                 
                 tags$hr(),
                   
              
               
                 ## author line

                 helpText("Created by Xuelian Li, Arvind Ramakrishnan, Jenna F. Kiridly and Emily R. Ramos"),
                 
                 ## email feedback link
                 ## To develop a link in HTML
                 helpText(a("Send us your comments or feedback!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank")),
                 
                 ## data source citation
                 helpText(a("Massachusetts Office of Labor and Workforce Development, ES-202", href="http://lmi2.detma.org/lmi/lmi_es_a.asp",
                            target="_blank")),
                 
                 ## GitHub link
                 helpText(a("View the data and code on GitHub", 
                            href="https://github.com/sEigmA/SEIGMA/tree/gh-pages/employment", target="_blank")),
                 
                 helpText("If using Internet Explorer, application only visible in version 10.")
    ),
    
    ######### End of Sidebar  #########
    


    ######### Start of Main Panel #####
    
    bootstrapPage(mainPanel(
      ## put in logo for title
      a(img(src = "logo.jpg", height=82, width=750), href="http://www.umass.edu/seigma/"),
      
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
                   condition="input.plot_radio =='Employment and Establishments'",
                     conditionalPanel(
                       condition="input.plot_display_radio=='Actual Values'",
                       ## make chart title here (otherwise not centered)
                       h4("Average Monthly Employment and Establishments Over Time", align="center"),
                       Emp_plot_options1,
                       Est_plot_options1),
                     conditionalPanel(
                       condition="input.plot_display_radio=='Change_Pct'",
                       ## make chart title here (otherwise not centered)
                       h4("Change in Average Monthly Employment and Establishments Over Time Since 2003", align="center"),
                       Emp_pct_plot_options,
                       Est_pct_plot_options,
                   p(strong("Change Since 2003"), "- This is calculated by comparing the  monthly employment esimate or weekly wage estimate for a specific year to the baseline year, 2003.  The baseline year, 2003, is considered 100% for these calculations. ")
                 )),
                   conditionalPanel(
                   condition="input.plot_radio =='Wages'",
                   conditionalPanel(
                     condition="input.plot_display_radio=='Actual Values'",
                     ## make chart title here (otherwise not centered)
                     h4("Average Weekly Wage Over Time", align="center"),
                     Wage_plot_options1),
                   conditionalPanel(
                     condition="input.plot_display_radio=='Change_Pct'",
                     ## make chart title here (otherwise not centered)
                     h4("Change in Average Weekly Wage Since 2003", align="center"),
                     Wage_pct_plot_options,
                     p(strong("Change Since 2003"), "- This is calculated by comparing the  monthly employment esimate or weekly wage estimate for a specific year to the baseline year, 2003.  The baseline year, 2003, is considered 100% for these calculations. ")
                     )
                   ),
                 tags$br(),
                 p(strong("Broken Lines"),
                   " - For some municipalities, data may not appear only for certain years, resulting in shortened or broken line.  This occurs when the data is not available for that particular time period."),
                 
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
                 
                 ## Employment Legend
                 conditionalPanel(
                   condition="input.map_radio == 'Employment' && input.map_display_radio=='Actual Values' && input.action != 0",
                   absolutePanel(
                     right = 10, top = 150, draggable=FALSE, style = "", 
                     class = "floater",
                     strong("Average Monthly Employment"),
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
                       empcolorRanges$from, empcolorRanges$to, map_colors[-length(map_colors)],
                       SIMPLIFY=FALSE),
                       tags$tr(
                         tags$td(tags$div(
                           style = sprintf("width: 16px; height: 16px; background-color: %s;", "black")
                         )),
                         tags$td("Data not available", align = "right")))
                   )),
                 
                 ## Establishments Legend
                 conditionalPanel(
                   condition="input.map_radio =='Establishments' && input.map_display_radio=='Actual Values' && input.action != 0",
                   absolutePanel(
                     right = 30, top = 150, draggable=FALSE, style = "",
                     class = "floater",
                     strong("Number of Establishments"),
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
                       estcolorRanges$from, estcolorRanges$to, map_colors[-length(map_colors)],
                       SIMPLIFY=FALSE),
                       tags$tr(
                         tags$td(tags$div(
                           style = sprintf("width: 16px; height: 16px; background-color: %s;", "black")
                         )),
                         tags$td("Data not available", align = "right")
                       )
                     )
                   )),
                 ## Wages Legend
                 conditionalPanel(
                   condition="input.map_radio =='Wages' && input.map_display_radio=='Actual Values' && input.action != 0",
                   absolutePanel(
                     right = 30, top = 150, draggable=FALSE, style = "",
                     class = "floater",
                     strong("Average Weekly Wage"),
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
                       wagecolorRanges$from, wagecolorRanges$to, map_colors[-length(map_colors)],
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
                 ## Difference compared to Year 2003 Legend
                 conditionalPanel(
                   condition="input.map_display_radio=='Change_Pct' && input.action != 0",
                   absolutePanel(
                     right = 10, top = 130, draggable=FALSE, style = "",
                     class = "floater",
                     strong("Difference Compared to 2003"),
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
                       pctcolorRanges$from, pctcolorRanges$to, pctmap_colors[-length(pctmap_colors)],
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
                 
                 #                plot_main_text,
                 value="map"),
        
        tabPanel("More Info", 
                 p(strong("Variable Summary:")),
                 tags$br(),
                 tags$ul(
                  tags$li(p(strong("Employed"),
                           " - A monthly survey was conducted each year for more than 8,000 employers.  This survey provided estimates of employment by industry for the state. All civilians 16 years old and over who worked during the reference week as paid employees, worked in their own business, worked on a farm or family business, worked for 15 hours or more as unpaid workers on a family farm or business, or those who had a job but were absent due to illness, bad weather, vacation, or personal reasons.  Exluded from this category are people whose only activity consisted of work around the house or unpaid volunteer work for religious, charitable, or similar organizations.")),
                  tags$br(),
                 tags$li((p(strong("Monthly employment estimate"),
                            "- The annual estimate of employees is calculated by first calculating the total number of employees paid for all periods. You must count all employees that you paid at any time during the year and include full-time, part-time, temporary, seasonal, salaried, and hourly workers. Note that pay periods could be monthly, weekly, bi-weekly, etc. Next, divide the total number of employees by the number of pay periods your establishment had in the calender year."))),
                 tags$br(),
                 tags$li((p(strong("Weekly Wage Estimate"), "- Average weekly wage is calculated by dividing quarterly
total wages by the average employment level over three months.  This number is then divided by 13, which represents the 13 weeks wihtin the payment quarter.  Wage data can be effected by the ratio of full time to part time employees, the number of higher paid employees within an establishment, and by the number of pay periods within the quarter."))),
                 tags$br(),
                 tags$li(p(strong("2012 Dollars"), "-An adjusted value of currency used to compare dollar values from one period to another. Due to inflation, the purchasing power of the dollar changes over time, so in order to compare monitary values from one year to another, they must be converted from current dollar values to constant dollar values.")),
                 tags$br(),
                 tags$li((p(strong("Establishments"), "- An establishment is defined as an economic unit which produces goods or services at a single location and engages in predominantly one activity."))),
                 tags$br(),
                 tags$li(p(strong("Change Since 2003"), "- This is calculated by comparing the  monthly employment esimate or weekly wage estimate for a specific year to the baseline year, 2003.  The baseline year, 2003, is considered 100% for these calculations. "))
                 ),
                 
                 #tags$br(),
                 # p("SEIGMA. Social and Economic Impacts of Gambling in Massachusetts, University of Massachusetts School of Public Health and Health Sciences. (2014). Report on the Social and Economic Impact of Gambling in Massachusetts SEIGMA Gambling study. Report to the Massachusetts Gaming Commission & the Massachusetts department of Public Health. Retrieved from:"), a("http://www.umass.edu/seigma/sites/default/files/March%202014%20SEIGMA%20Report_6-19_for%20website.pdf"),
                 
                 
                 
                 
                 
                 
                 ## email feedback link
                 h3(a("Please fill out our survey to help improve the site!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank")), value="info"),
        id="tabs"
      )       
        ))
    )
  ))