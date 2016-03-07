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
             
         conditionalPanel(
           condition="input.tabs == 'lmap'",
           ## Select year with multiyear slider
           
           sliderInput("lmap_year", "Select Year",
                       min=2003, max=2012, value=2012,
                       sep="")
           ,
           selectInput("map_profile", "Select Data", 
                       choices=c("Enrollment Profile"="enrolled",
                                 "Mobility Measures"="mobility"),
                       selected="enrolled")
         ),
           conditionalPanel(
             condition="input.tabs == 'lmap' && input.map_profile=='enrolled'",
             #select variable
             radioButtons("lmap_radio", "Select Profile Variable",
                          c("Race/Ethnicity"="Race/Ethnicity", 
                            "Gender"="Gender", 
                            "Grade Level"="Grade Levels",
                            "Interest Groups"="Interest Groups"),
                          selected="Race/Ethnicity")
           ), 
           conditionalPanel(
             condition="input.tabs == 'lmap' && input.map_profile=='mobility'",
             #select variable
             radioButtons("lmap_mobility_var", "Select Rate or Enrollment",
                          c("Rate"="mobrate",
                            "Enrollment"="mobenrollment"),
                          selected="mobrate"),
             
             conditionalPanel(condition="input.lmap_mobility_var=='mobrate'",
                              radioButtons("lmap_radio", "Select Interest Group",
                                           c("English Language Learners"="English Language Learner Students Mobility Rate",
                                             "Students with Disabilities"="Students with Disabilities Mobility Rate",
                                             "Low Income"="Low Income Students Mobility Rate", 
                                             "High Needs"="High Needs Students Mobility Rate"),
                                           selected="English Language Learner Students Mobility Rate")
             ),
             conditionalPanel(condition="input.lmap_mobility_var=='mobenrollment'",
                              radioButtons("lmap_radio", "Select Interest Group",
                                           c("English Language Learners"="English Language Learner Students Mobility Enrollment",
                                             "Students with Disabilities"="Students with Disabilities Mobility Enrollment",
                                             "Low Income"="Low Income Students Mobility Enrollment", 
                                             "High Needs"="High Needs Students Mobility Enrollment"),
                                           selected="English Language Learner Students Mobility Enrollment")
             )
             
           ),
         conditionalPanel(condition="input.tabs == 'lmap'",
           uiOutput("lmapvar_levels"))
           
         
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
                                 c("Race/Ethnicity"="Race/Ethnicity", 
                                   "Gender"="Gender", 
                                   "Grade Level"="Grade Levels",
                                   "Interest Groups"="Interest Groups"
                                   ),
                                 selected='Race/Ethnicity')
        ), conditionalPanel(
           condition="input.tabs== 'plot' && input.plot_profile=='mobility'",
           #select variable
           radioButtons("plot_mobility", "Select Interest Group",
                       c("English Language Learners"="English Language Learner Students",
                         "Students with Disabilities"="Students with Disabilities",
                         "Low Income"="Low Income Students", 
                         "High Needs"="High Needs Students"),
                       selected='English Language Learner Students'),
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
                   condition="input.plot_profile=='enrolled' && input.plot_enrolled!='Interest Groups'",
                   percentcolchart),
                conditionalPanel(
                  condition="input.plot_profile=='enrolled' && input.plot_enrolled=='Interest Groups'",
                  countcolchart),

                #count charts 
                #of mobility enrollment
                conditionalPanel(
                  condition="input.plot_profile=='mobility' && input.plot_mobility_var=='mobenrollment'",
                  mobenrollment_plot_options),
                #of mobility rate
                conditionalPanel(
                  condition="input.plot_profile=='mobility' && input.plot_mobility_var=='mobrate'",
                  mobrate_plot_options)
                
              
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
           condition="input.lmap_level == 'African American'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("% African",br(),"American",br(),"Students"),
             plotOutput("AAlegend")
           ))
           ,
         conditionalPanel(
           condition="input.lmap_level == 'Asian'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("% Asian",br(),"Students"),
                    plotOutput("ASlegend")
             ))
         ,
         conditionalPanel(
           condition="input.lmap_level == 'Hispanic'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("% Hispanic",br(),"Students"),
                    plotOutput("HISPlegend")
             ))
         ,
         conditionalPanel(
           condition="input.lmap_level == 'White'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("% White",br(),"Students"),
                    plotOutput("WHlegend")
             ))
         ,
         conditionalPanel(
           condition="input.lmap_level == 'Native American'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("% Native",br(),"American",br(),"Students"),
                    plotOutput("NAlegend")
             ))
         ,
         conditionalPanel(
           condition="input.lmap_level == 'Native Hawaiian/Pacific Islander'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("% Native",br(),"Hawaiian", br(), "Pacific", br(), "Islander",br(),"Students"),
                    plotOutput("NHlegend")
             ))
         ,
         conditionalPanel(
           condition="input.lmap_level == 'Multi-Race/Non-Hispanic'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("% Multi",br(),"Race", br(), "Non-Hispanic", br(),"Students"),
                    plotOutput("MRlegend")
             ))
         ,
         conditionalPanel(
           condition="input.lmap_level == 'Females'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("% Female",br(),"Students"),
             plotOutput("FEMlegend")
           ))
         ,
         conditionalPanel(
           condition="input.lmap_level == 'Males'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("% Male",br(),"Students"),
             plotOutput("MALlegend")
           ))
         ,
         conditionalPanel(
           condition="input.lmap_level == 'Pre Kindergarden'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("Pre-",br(),"Kindergarteners"),
             plotOutput("PREKlegend")
           ))
         ,
         
         conditionalPanel(
           condition="input.lmap_level == 'Kindergarden'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("Kindergarteners"),
             plotOutput("KINDlegend")
           ))
         ,conditionalPanel(
           condition="input.lmap_level == 'First Grade'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("First",br(),"Graders"),
             plotOutput("FIRSTlegend")
           ))
         ,conditionalPanel(
           condition="input.lmap_level == 'Second Grade'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("Second",br(),"Graders"),
             plotOutput("SECONDlegend")
           ))
         ,conditionalPanel(
           condition="input.lmap_level == 'Third Grade'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("Third",br(),"Graders"),
             plotOutput("THIRDlegend")
           ))
         ,conditionalPanel(
           condition="input.lmap_level == 'Fourth Grade'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("Fourth",br(),"Graders"),
             plotOutput("FOURTHlegend")
           ))
         ,conditionalPanel(
           condition="input.lmap_level == 'Fifth Grade'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("Fifth",br(),"Graders"),
             plotOutput("FIFTHlegend")
           ))
         ,conditionalPanel(
           condition="input.lmap_level == 'Sixth Grade'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("Sixth",br(),"Graders"),
             plotOutput("SIXTHlegend")
           ))
         ,conditionalPanel(
           condition="input.lmap_level == 'Seventh Grade'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("Seventh",br(),"Graders"),
             plotOutput("SEVENTHlegend")
           ))
         ,conditionalPanel(
           condition="input.lmap_level == 'Eighth Grade'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("Eighth",br(),"Graders"),
             plotOutput("EIGHTHlegend")
           ))
         ,conditionalPanel(
           condition="input.lmap_level == 'Ninth Grade'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("Ninth",br(),"Graders"),
             plotOutput("NINTHlegend")
           ))
         ,conditionalPanel(
           condition="input.lmap_level == 'Tenth Grade'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("Tenth",br(),"Students"),
             plotOutput("TENTHlegend")
           ))
         ,conditionalPanel(
           condition="input.lmap_level == 'Eleventh Grade'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("Eleventh",br(),"Graders"),
             plotOutput("ELEVENTHlegend")
           ))
         ,conditionalPanel(
           condition="input.lmap_level == 'Twelfth Grade'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("Twelfth",br(),"Graders"),
             plotOutput("TWELFTHlegend")
           ))
         ,conditionalPanel(
           condition="input.lmap_level == 'Special Ed Beyond 12th Grade'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("Special",br(),"Education", br(), "beyond", br(), "Twelfth", br(), "Graders"),
             plotOutput("SPEClegend")
           ))
         ,conditionalPanel(
           condition="input.lmap_level == 'English Language Learner Enrolled %'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("% English",br(),"Language", br(), "Learners"),
             plotOutput("P_ELL_legend")
           ))
         ,conditionalPanel(
           condition="input.lmap_level == 'First Language Not English Enrolled %'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("% First",br(),"Language", br(), "Not", br(), "English"),
             plotOutput("P_FLNE_legend")
           ))
         ,conditionalPanel(
           condition="input.lmap_level == 'Students With Disabilities Enrolled %'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("% Students",br(),"with", br(), "Disabilities"),
             plotOutput("P_DISAB_legend")
           ))
         ,conditionalPanel(
           condition="input.lmap_level == 'Low Income Students Enrolled %'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("% Low",br(),"Income", br(), "Students"),
             plotOutput("P_LOW_legend")
           ))
         ,conditionalPanel(
           condition="input.lmap_level == 'High Needs Students Enrolled %'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("% High",br(),"Needs", br(), "Students"),
             plotOutput("P_HIGH_legend")
           ))
         ,
         
         
         value="lmap"
         )
,
        tabPanel("More Info", 
                 p(strong("Variable Summary:")),
                 tags$br(),
                 tags$ul(
                  tags$li(p(strong("Intake Rate"),
                           " - Measures the number of students that enroll in the state, a district, or school after the beginning of the school year.  Intake is calculated by dividing the number of students who enroll in school after the start of the school year by all students reported as enrolled in any School Infomartional Management System within that school.")),
                  tags$br(),
                 tags$li((p(strong("Churn Rate"),
                            "-  measures the number students transferring into or out of a public school or district throughout the course of a school year. Churn represents the sum of all students who were transferring in or out, divided by all students reported as enrolled at any point in time during the school year."))),
                 tags$br(),
                 tags$li((p(strong("Stability Rate"), "- measures how many students remain in a district or school throughout the school year."))),
                 tags$br(),
                 tags$li(p(strong("English Language Learners"), "- This focus group is defined as, a student whose first language is a language other than English who is unable to perform ordinary classroom work in English.")),
                 tags$br(),
                 tags$li((p(strong("First Language Not English"), "- This focus group is defined as those students whose first language is a language other than English."))),
                 tags$br(),
                 tags$li(p(strong("Students With Disabilities"), "- This focus group is defined as all students who have an Individualized Education Program (IEP).")),
                 tags$br(),
                 tags$li(p(strong("High Needs"), "- A student is high needs if he or she is either low income (prior to School Year 2015), economically disadvantaged (starting in School Year 2015), an enlgish language learner, or a student with disabilities.")),
                 tags$br(),
                 tags$li(p(strong("Low Income"), "- A student of low income status if they meet any of thr following criteria; they are eligible for free or reduced price lunch, they receive Transitional Aid to Families benefits, or the student is eligible for food stamps,"))
                 ),
                 
                
                 
                 
                 
                 
                 
                 
                 ## email feedback link
                 h3(a("Please fill out our survey to help improve the site!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank")), 
                 value="info"),
        id="tabs"
             
        )
      ))
    )
  ))