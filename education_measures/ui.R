###########################################
## Title: Education Measures             ##
## Author(s): Xuelian Li, Emily Ramos,   ## 
##            Arvind Ramakrishnan,       ##
##            Jenna Kiridly, Steve Lauer,##
##            Justin Baldwin             ##
## Date Created:  12/10/2015             ##
## Date Modified: 12/10/2015             ##
###########################################


shinyUI(fluidPage(
  ## embed the google analytics script in the app
#   tags$head(includeScript("google-analytics.js")),
#   ## embed the googleCharts Init script to use version 43 (frozen version) to fixed the bug (2 charts in one page)
#   tags$head(tags$script(src="https://www.google.com/jsapi")),
#   tags$head(tags$script(src="https://www.gstatic.com/charts/loader.js")),
#   tags$head(includeScript("googleChartInit.js")),
#   tags$head(tags$script(src="bindings.js")),
  ## HTML to create generate map button
  # gen_map_button,
  ## this starts the googleCharts engine
  googleChartsInit(),
  
  ## blank title, but put in a special title for window tab
  titlePanel("", windowTitle = "SEIGMA: Education Measures Shiny App"),
  
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
                                    "Multiple Years" = "mult.yrs"), selected="mult.yrs"),
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
                                 min=2003, max=2012, value=c(2003,2012),
                                 sep="")
                   )
                 ,
               
                 selectInput("sum_county", "Select County", 
                             choices = c(MA_county, " "),
                             ## Multiple allows for multi-county selection
                             multiple=FALSE, selected = " ")
                 
                 ,
                 uiOutput("sum_muniui"),
                 

                 radioButtons("sum_radio", "Variables",
                            c("Race/Ethnicity"="Race/Ethnicity", 
                              "Gender"="Gender", "Grade Level"="Grade Level",
                              "English Language Learners"="English Language Learners",
                              "Students with Disabilities"="Students with Disabilities",
                              "Low Income"="Low Income", "High Needs"="High Needs"),
             selected="Race/Ethnicity")
                 )
             ,
             #
             #map sidebar options
             #
             
#               
#              conditionalPanel(
#                condition="input.tabs == 'map'",
#                ## Select year with multiyear slider
#                
#                sliderInput("map_year", "Select Year",
#                              min=2003, max=2012, value=2012,
#                              sep="")
#                ,
#                
#                radioButtons("map_schooltype", "School Type",
#                             c("Pre-K" = "prek", "Kindergarten" = "kindergarten",
#                               "Elementary" = "elementary","Middle School" ="middle","High School" ="high"),
#                             selected="High School"
#                ),
#                
#                radioButtons("map_radio", "Variables",
#                             c("Race/Ethnicity"="Race/Ethnicity", 
#                               "Gender"="Gender", "Grade Level"="Grade Level",
#                               "English Language Learners"="English Language Learners",
#                               "Students with Disabilities"="Students with Disabilities",
#                               "Low Income"="Low Income", "High Needs"="High Needs"),
#                             selected="Grade Level"
#                )
#                 ,
#                 uiOutput("mapvar_levels")
#                
#              )
#         ,
         conditionalPanel(
           condition="input.tabs == 'lmap'",
           ## Select year with multiyear slider
           
           sliderInput("lmap_year", "Select Year",
                       min=2003, max=2012, value=2012,
                       sep="")
           ,
           
           
           radioButtons("lmap_radio", "Variables",
                        c("Race/Ethnicity"="Race/Ethnicity", 
                          "Gender"="Gender", "Grade Level"="Grade Level",
                          "English Language Learners"="English Language Learners",
                          "Students with Disabilities"="Students with Disabilities",
                          "Low Income"="Low Income", "High Needs"="High Needs"),
                        selected="Race/Ethnicity"
           )
           ,
           uiOutput("lmapvar_levels")
           
         )
         ,
                 
                  ## in plot, allow for county and school selection
        conditionalPanel(
                    condition="input.tabs == 'plot'",
                    
                    ##Select enrollment profile or mobility
                    selectInput("plot_profile", "Select Data", 
                                choices=c("Enrollment Profile"="enrolled",
                                         "Mobility Measures"="mobility"),
                                selected="enrolled"),
                    
                    ## Select county
                    selectInput("plot_county", "Select County", 
                                choices = c(MA_county, " "),
                                ## Multiple allows for multi-county selection
                                multiple=FALSE, selected=" "),
                    
                    ##if county is not chosen, select school from list of all schools
                    uiOutput("plot_schoolui")
        ), conditionalPanel(
                    condition="input.tabs== 'plot' && input.plot_profile=='enrolled'",
                    #select variable
                     radioButtons("plot_enrolled", "Select Profile Variable",
                                 c("Race/Ethnicity"="race", 
                                   "Gender"="gender", 
                                   "Grade Level"="grade",
                                   "English Language Learners"="ell",
                                   "First Language Not English"="flnl",
                                   "Students with Disabilities"="disab",
                                   "Low Income"="low"
                                   #high needs only available data in 2012 - no time series warranted
                                   , 
                                   "High Needs"="high"
                                   ),
                                 selected='race')
        ), conditionalPanel(
           condition="input.tabs== 'plot' && input.plot_profile=='mobility'",
           #select variable
           radioButtons("plot_mobility", "Select Interest Group",
                       c("English Language Learners"="ell",
                         "Students with Disabilities"="disab",
                         "Low Income"="low", 
                         "High Needs"="high"),
                       selected='ell'),
           radioButtons("plot_mobility_var", "Select Rate or Enrollment",
                        c("Rate"="mobrate",
                          "Enrollment"="mobenrollment"),
                        selected='mobrate')
        )
        ,
        tags$hr(),
    
                  
               
                 ## author line

                 helpText("Created by Justin Baldwin, Jenna F. Kiridly and Xuelian Li "),
                 
                 ## email feedback link
                 ## To develop a link in HTML
                 helpText(a("Send us your comments or feedback!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank")),
                 
                 ## data source citation
                 helpText(a("Massachusetts Department of Education", href="http://www.doe.mass.edu/",
                            target="_blank")),
                 
                 ## GitHub link
                 helpText(a("View the data and code on GitHub", 
                            href="https://github.com/sEigmA/SEIGMA/tree/gh-pages/education_measures", target="_blank")),
                 
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
                #percent charts
                
                  conditionalPanel(
                   condition="input.plot_profile=='enrolled",
                   percentcolchart)

                
#                 #count charts
#                 #ell count
#                  conditionalPanel(
#                    condition="input.plot_profile=='mobility'",
#                    #ell
#                    conditionalPanel(
#                      condition="input.plot_mobility=='ell' && input.plot_mobility_var=='mobenrollment'",
#                      ell_mobenrollment_plot_options),
#                    conditionalPanel(
#                      condition="input.plot_mobility=='ell' && input.plot_mobility_var=='mobrate'",
#                      ell_mobrate_plot_options),
#                    #disab
#                    conditionalPanel(
#                      condition="input.plot_mobility=='disab' && input.plot_mobility_var=='mobenrollment'",
#                      disab_mobenrollment_plot_options),
#                    conditionalPanel(
#                      condition="input.plot_mobility=='disab' && input.plot_mobility_var=='mobrate'",
#                      disab_mobrate_plot_options),
#                    #low
#                    conditionalPanel(
#                      condition="input.plot_mobility=='low' && input.plot_mobility_var=='mobenrollment'",
#                      low_mobenrollment_plot_options),
#                    conditionalPanel(
#                      condition="input.plot_mobility=='low' && input.plot_mobility_var=='mobrate'",
#                      low_mobrate_plot_options),
#                    #high
#                    conditionalPanel(
#                      condition="input.plot_mobility=='high' && input.plot_mobility_var=='mobenrollment'",
#                      high_mobenrollment_plot_options),
#                    conditionalPanel(
#                      condition="input.plot_mobility=='high' && input.plot_mobility_var=='mobrate'",
#                      high_mobrate_plot_options)
#                    )
               ,
                value="plot")
        ,
        
           ##plot leaflet map
tabPanel("Map",
         
         ## Add a little CSS to make the map background pure white
         tags$head(tags$style("
                              #showcase-code-position-toggle, #showcase-sxs-code { display: none; }
                              .floater { background-color: white; padding: 8px; opacity: 1; border-radius: 6px; box-shadow: 0 0 15px rgba(0,0,0,0.2); }
                              ")),
         tags$br(),
         textOutput("lmap_title"),
         tags$br(),
         
         leafletOutput("leafmap"),
         
         
         #Legend- African American
         conditionalPanel(
           condition="input.lmap_level == 'African.American'",
           absolutePanel(
             right = 10, top = 150, draggable=FALSE, style = "", 
             class = "floater",
             strong("% African American Students"),
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
               AA_racecolorRanges$from, AA_racecolorRanges$to, map_colors[-length(map_colors)],
               SIMPLIFY=FALSE),
               tags$tr(
                 tags$td(tags$div(
                   style = sprintf("width: 16px; height: 16px; background-color: %s;", "black")
                 )),
                 tags$td("Data not available", align = "right")))
           )),
         
         value="lmap"
         )
,
        tabPanel("More Info", 
                 p(strong("Variable Summary:")),
                 tags$br(),
                 tags$ul(
                  tags$li(p(strong("Employment"),
                           " - More than 8,000 employers participated in an annual survey administered by the Executive Office of Labor and Workforce Development, which assessed employment by industry in Massachusetts. All civilians aged 16+ who were working during this survey period were counted.  This includes individuals who worked as paid employees, within their own business, within a farm or family business, and those who had a job but were absent due to illness, bad weather, vacation, or personal reasons. Individuals were excluded if they solely worked around the house or completed unpaid volunteer work for religious, charitable, or similar organizations.")),
                  tags$br(),
                 tags$li((p(strong("Average Monthly Employment"),
                            "- To estimate monthly employment, all employees who were paid at any point in the past year are counted (this includes full-time, part-time, seasonal, salaried, and hourly employees).  The total number of employees is then divided by the number of pay periods per calander year at each business establishment."))),
                 tags$br(),
                 tags$li((p(strong("Average Weekly Wage"), "- Average weekly wage is calculated by dividing quarterly
total wages by the average employment level over three months.  This number is then divided by 13, which represents the 13 weeks wihtin the payment quarter.  Wage data can be affected by the ratio of full time to part time employees, the number of higher paid employees within an establishment, and by the number of pay periods within the quarter."))),
                 tags$br(),
                 tags$li(p(strong("2012 Dollars"), "-Due to inflation, the purchasing power of the dollar changes over time. In order to compare monitary values from one year to another, they must be converted from current dollar values to constant dollar values. For this app we used the dollar values from 2012 to be our constant.")),
                 tags$br(),
                 tags$li((p(strong("Business Establishments"), "- A business establishment is defined as an economic unit which produces goods or services at a single location and engages in predominantly one activity."))),
                 tags$br(),
                 tags$li(p(strong("Change Since 2003"), "- This is calculated by comparing average monthly employment and weekly wages for a specific year to the year 2003.  We selected 2003 in order to provide a ten year baseline period.  The baseline year of 2003 is considered '0' for these calculations. A positive number indicates an increase from 2003 and a negative number indicates a decrease from 2003."))
                 ),
                 
                 tags$br(),
                 p("SEIGMA. Social and Economic Impacts of Gambling in Massachusetts, University of Massachusetts School of Public Health and Health Sciences. (2014). Report on the Social and Economic Impact of Gambling in Massachusetts SEIGMA Gambling study. Report to the Massachusetts Gaming Commission & the Massachusetts department of Public Health. Retrieved from:"), a("http://www.umass.edu/seigma/sites/default/files/March%202014%20SEIGMA%20Report_6-19_for%20website.pdf"),
                 
                 
                 
                 
                 
                 
                 ## email feedback link
                 h3(a("Please fill out our survey to help improve the site!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank")), 
                 value="info"),
        id="tabs"
             
        )
      ))
    )
  ))