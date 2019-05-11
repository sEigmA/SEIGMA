###########################################
## Title: Schools                        ##
## Author(s): Xuelian Li, Emily Ramos,   ## 
##            Arvind Ramakrishnan,       ##
##            Jenna Kiridly, Steve Lauer,##
##            Justin Baldwin             ##
## Date Created:  12/10/2015             ##
## Date Modified: 05/10/2019 VE          ##
###########################################


shinyUI(fluidPage(
  ## embed the google analytics script in the app
  tags$head(includeScript("google-analytics.js")),
#   ## embed the googleCharts Init script to use version 43 (frozen version) to fixed the bug (2 charts in one page)
#   tags$head(tags$script(src="https://www.google.com/jsapi")),
#   tags$head(tags$script(src="https://www.gstatic.com/charts/loader.js")),
#   tags$head(includeScript("googleChartInit.js")),
#   tags$head(tags$script(src="bindings.js")),
  ## HTML to create generate map button
  gen_map_button,
  ## this starts the googleCharts engine
  googleChartsInit(),
  
  ## blank title, but put in a special title for window tab
  titlePanel("", windowTitle = "SEIGMA: Schools Shiny App"),
  
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
                     sliderInput("sum_year", "Select School Year beginning in",
                                 min=2003, max=2018, value=2017,
                                 sep="")
                   ),
                   conditionalPanel(
                     ## Initializes a multi-year slider (range)
                     condition="input.sum_timespan == 'mult.yrs'",
                     ## Slider starts from 2010-2012
                     sliderInput("sum_range", "Select School Years beginning in",
                                 min=2003, max=2018, value=c(2003,2017),
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
                              "Low Income"="Low Income", "Economically Disadvantaged"="Economically Disadvantaged",
                              "High Needs"="High Needs"),
             selected="Race/Ethnicity")
                 )
             ,
             
         conditionalPanel(
           condition="input.tabs == 'lmap'",
           ## Select year with multiyear slider
           

           sliderInput("lmap_year", "Select School Year beginning in",
                       min=2003, max=2018, value=2017,
                       sep="")
           ,
           selectInput("map_profile", "Select Data", 
                       choices=c("Enrollment Profile"="enrolled",
                                 "Mobility Measures"="mobility"),
                       selected="enrolled")
         ),
         
         #enrollment options
         
           conditionalPanel(
             condition="input.tabs == 'lmap' && input.map_profile=='enrolled'",
             #select variable
             radioButtons("lmap_radio", "Select Profile Variable",
                          c("Race/Ethnicity"="Race/Ethnicity", 
                            "Gender"="Gender", 
                            "Grade Level"="Grade Levels",
                            "Interest Groups"="Interest Groups"),
                          selected="Race/Ethnicity"),
             
             
             conditionalPanel("input.lmap_radio=='Race/Ethnicity'",
                              selectInput("lmap_level1","Choose Race/Ethnicity to map",
                                          choices = 
                                            c(" ", "African American" = "African American",
                                              "Asian" = "Asian",
                                              "Hispanic" = "Hispanic",
                                              "White" = "White",
                                              "Native American" = "Native American",
                                              "Native Hawaiian/Pacific Islander" = "Native Hawaiian/Pacific Islander",
                                              "Multi-Race Non-Hispanic" = "Multi-Race/Non-Hispanic"),
                                          selected = " "),
                              checkboxInput("lmap_cas1", "Display Casinos", value=TRUE)),
             
             conditionalPanel("input.lmap_radio=='Gender'",
                              selectInput("lmap_level2","Choose Gender to map",
                                   choices = c(" ", "Females" = "Females",
                                               "Males" = "Males"),
                                   selected = " "),
                              checkboxInput("lmap_cas2", "Display Casinos", value=TRUE)),
             
             conditionalPanel("input.lmap_radio=='Grade Levels'",
                              selectInput("lmap_level3","Choose Grade Level to map",
                                         choices = 
                                           c(" ","Pre-Kindergarten" = "Pre Kindergarten",
                                             "Kindergarten" = "Kindergarten",
                                             "First Grade" = "First Grade",
                                             "Second Grade" = "Second Grade",
                                             "Third Grade" = "Third Grade",
                                             "Fourth Grade" = "Fourth Grade",
                                             "Fifth Grade" = "Fifth Grade",
                                             "Sixth Grade" = "Sixth Grade", 
                                             "Seventh Grade" = "Seventh Grade",
                                             "Eighth Grade" = "Eighth Grade",
                                             "Ninth Grade" = "Ninth Grade",
                                             "Tenth Grade" = "Tenth Grade",
                                             "Eleventh Grade" = "Eleventh Grade",
                                             "Twelfth Grade" = "Twelfth Grade",
                                             "Special Education Beyond 12th Grade" = "Special Ed Beyond 12th Grade"),
                                         selected = " "),
                              checkboxInput("lmap_cas3", "Display Casinos", value=TRUE)),
             conditionalPanel("input.lmap_radio=='Interest Groups'",
                              selectInput("lmap_level4","Choose Level to map",
                                            choices = 
                                              c(" ", "English Language Learners" = "English Language Learner Enrolled %",
                                                "First Language Not English" = "First Language Not English Enrolled %",
                                                "Students with Disabilities" = "Students With Disabilities Enrolled %",
                                                "Low Income Students" = "Low Income Students Enrolled %",
                                                "Economically Disadvantaged Students" = "Economically Disadvantaged Students Enrolled %",
                                                "High Needs" = "High Needs Students Enrolled %"), 
                                            selected=" "),
                              checkboxInput("lmap_cas4", "Display Casinos", value=TRUE))
             
           )
         
         #mobility options
         
          , 
           conditionalPanel(
             condition="input.tabs == 'lmap' && input.map_profile=='mobility'",
             #select variable
             radioButtons("lmap_mobility_var", "Select Rate or Enrollment",
                          c("Rate"="mobrate",
                            "Enrollment"="mobenrollment"),
                          selected="mobrate")
           ),
             
             conditionalPanel(condition="input.tabs == 'lmap' && input.map_profile=='mobility' && input.lmap_mobility_var=='mobrate'",
                              radioButtons("lmap_radio2", "Select Interest Group",
                                           c("English Language Learners"="English Language Learner Students Mobility Rate",
                                             "Students with Disabilities"="Students with Disabilities Mobility Rate",
                                             "Low Income"="Low Income Students Mobility Rate", 
                                             "Economically Disadvantaged"="Economically Disadvantaged Students Mobility Rate", 
                                             "High Needs"="High Needs Students Mobility Rate"),
                                           selected="English Language Learner Students Mobility Rate"),
                              
                             conditionalPanel("input.lmap_radio2=='English Language Learner Students Mobility Rate'",
                                              selectInput("lmap_level5","Choose Level to map", choices=
                                                            c(" ", "Churn Rate: English Language Learners"="Churn Rate for English Language Learning Students",
                                                              "Stability Rate: English Language Learners"="Stability Rate for English Language Learning Students",
                                                              "Intake Rate: English Language Learners"="Intake Rate for English Language Learning Students"),
                                                          selected=" "),
                                              checkboxInput("lmap_cas5", "Display Casinos", value=TRUE)),
                             conditionalPanel("input.lmap_radio2=='Students with Disabilities Mobility Rate'",
                                              selectInput("lmap_level6","Choose Level to map", choices=
                                                            c(" ", "Churn Rate: Students with Disabilities"="Churn Rate for Students with Disabilities",
                                                              "Stability Rate: Students with Disabilities"="Stability Rate for Students with Disabilities",
                                                              "Intake Rate: Students with Disabilities"="Intake Rate for Students with Disabilities"),
                                                          selected=" "),
                                              checkboxInput("lmap_cas6", "Display Casinos", value=TRUE)),
                             conditionalPanel("input.lmap_radio2=='Low Income Students Mobility Rate'",
                                              selectInput("lmap_level7","Choose Level to map", choices=
                                                            c(" ","Churn Rate: Low Income Students"="Churn Rate for Low Income Students",
                                                              "Stability Rate: Low Income Students"="Stability Rate for Low Income Students",
                                                              "Intake Rate: Low Income Students"="Intake Rate for Low Income Students"),
                                                          selected=" "),
                                              checkboxInput("lmap_cas7", "Display Casinos", value=TRUE)),
                             conditionalPanel("input.lmap_radio2=='Economically Disadvantaged Students Mobility Rate'",
                                              selectInput("lmap_level13","Choose Level to map", choices=
                                                            c(" ","Churn Rate: Economically Disadvantaged Students"="Churn Rate for Economically Disadvantaged Students",
                                                              "Stability Rate: Economically Disadvantaged Students"="Stability Rate for Economically Disadvantaged Students",
                                                              "Intake Rate: Economically Disadvantaged Students"="Intake Rate for Economically Disadvantaged Students"),
                                                          selected=" "),
                                              checkboxInput("lmap_cas13", "Display Casinos", value=TRUE)),
                             conditionalPanel("input.lmap_radio2=='High Needs Students Mobility Rate'",
                                              selectInput("lmap_level8","Choose Level to map", choices=
                                                            c(" ","Churn Rate: High Needs Students"="Churn Rate for High Needs Students",
                                                              "Stability Rate: High Needs Students"="Stability Rate for High Needs Students",
                                                              "Intake Rate: High Needs Students"="Intake Rate for High Needs Students"),
                                                          selected=" "),
                                              checkboxInput("lmap_cas8", "Display Casinos", value=TRUE))
             ),
             conditionalPanel(condition="input.tabs == 'lmap' && input.map_profile=='mobility' && input.lmap_mobility_var=='mobenrollment'",
                              radioButtons("lmap_radio3", "Select Interest Group",
                                           c("English Language Learners"="English Language Learner Students Mobility Enrollment",
                                             "Students with Disabilities"="Students with Disabilities Mobility Enrollment",
                                             "Low Income"="Low Income Students Mobility Enrollment", 
                                             "Economically Disadvantaged"="Economically Disadvantaged Students Mobility Enrollment", 
                                             "High Needs"="High Needs Students Mobility Enrollment"),
                                           selected="English Language Learner Students Mobility Enrollment"),
                              conditionalPanel("input.lmap_radio3=='English Language Learner Students Mobility Enrollment'",
                                               selectInput("lmap_level9","Choose Level to map", choices=
                                                             c(" ","Churn Enrollment: English Language Learners"="Churn Enrollment for English Language Learning Students",
                                                               "Stability Enrollment: English Language Learners"="Stability Enrollment for English Language Learning Students"),
                                                           selected=" "),
                                               checkboxInput("lmap_cas9", "Display Casinos", value=TRUE)),
                              conditionalPanel("input.lmap_radio3=='Students with Disabilities Mobility Enrollment'",
                                               selectInput("lmap_level10","Choose Level to map", choices=
                                                             c(" ","Churn Enrollment: Students with Disabilities"="Churn Enrollment for Students with Disabilities",
                                                               "Stability Enrollment: Students with Disabilities"="Stability Enrollment for Students with Disabilities"),
                                                           selected=" "),
                                               checkboxInput("lmap_cas10", "Display Casinos", value=TRUE)),
                              conditionalPanel("input.lmap_radio3=='Low Income Students Mobility Enrollment'",
                                               selectInput("lmap_level11","Choose Level to map", choices=
                                                             c(" ","Churn Enrollment: Low Income Students"="Churn Enrollment for Low Income Students",
                                                               "Stability Enrollment: Low Income Students"="Stability Enrollment for Low Income Students"),
                                                           selected=" "),
                                               checkboxInput("lmap_cas11", "Display Casinos", value=TRUE)),
                              conditionalPanel("input.lmap_radio3=='Economically Disadvantaged Students Mobility Enrollment'",
                                               selectInput("lmap_level14","Choose Level to map", choices=
                                                             c(" ","Churn Enrollment: Economically Disadvantaged Students"="Churn Enrollment for Economically Disadvantaged Students",
                                                               "Stability Enrollment: Economically Disadvantaged Students"="Stability Enrollment for Economically Disadvantaged Students"),
                                                           selected=" "),
                                               checkboxInput("lmap_cas14", "Display Casinos", value=TRUE)),
                              conditionalPanel("input.lmap_radio3=='High Needs Students Mobility Enrollment'",
                                               selectInput("lmap_level12","Choose Level to map", choices=
                                                             c(" ","Churn Enrollment: High Needs Students"="Churn Enrollment for High Needs Students",
                                                               "Stability Enrollment: High Needs Students"="Stability Enrollment for High Needs Students"),
                                                           selected=" "),
                                               checkboxInput("lmap_cas12", "Display Casinos", value=TRUE))
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
                    selectizeInput("plot_school", "Choose School",selected=" ",
                                      choices="", options = list(maxOptions = 2000)
                    )
                    #uiOutput("plot_schoolui")
                    
                    
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

#            
           radioButtons("plot_mobility", "Select Interest Group",
                       c("English Language Learners"="English Language Learner Students",
                         "Students with Disabilities"="Students with Disabilities",
                         "Low Income"="Low Income Students", 
                         "Economically Disadvantaged"="Economically Disadvantaged Students", 
                         "High Needs"="High Needs Students"),
                       selected='English Language Learner Students'),
           radioButtons("plot_mobility_var", "Select Rate or Enrollment",
                        c("Rate"="mobrate",
                          "Enrollment"="mobenrollment"),
                        selected='mobrate'))
        ,
        tags$hr(),
    
                  
               
                 ## author line

                 helpText("Created by Justin Baldwin, Jenna F. Kiridly and Xuelian Li"),
                 helpText("Updated by Valerie Evans"),
                 
                 ## email feedback link
                 ## To develop a link in HTML
                 helpText(a("Send us your comments or feedback!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank")),
                 
                 ## data source citation
                 helpText(a("Massachusetts Department of Education", href="http://www.doe.mass.edu/",
                            target="_blank")),
                 
                 ## GitHub link
                 helpText(a("View the data and code on GitHub", 
                            href="https://github.com/sEigmA/SEIGMA/tree/gh-pages/education_measures/schools", target="_blank")),
                 
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
                 DT::dataTableOutput("summary"), value="summary", 
                 tags$style(type="text/css", '#summary tfoot {display:none;}'),
                 
                 HTML("Starting in the school year 2014-2015, the definition of 'low income' students was expanded and renamed 'economically disadvantaged' (See 'More Info' tab).")
                 ),
        
        
        ## plot tab with google chart options
        tabPanel("Plot",
                 ##plot upon the selected variable and display option 
                #percent charts
                
                  conditionalPanel(
                   condition="input.plot_profile=='enrolled' && input.plot_enrolled!='Interest Groups' && input.plot_school!=' '",
                   percentcolchart,
                   conditionalPanel(condition="input.plot_enrolled=='Interest Groups'",
                                  HTML("Starting in the school year 2014-2015, the definition of 'low income' students was expanded and renamed 'economically disadvantaged' (See 'More Info' tab).")),
                   HTML("Data are only shown from years in which schools were in operation and reported data"))
                ,conditionalPanel(
                  condition="input.plot_profile=='enrolled' && input.plot_enrolled!='Interest Groups' && input.plot_school==' '",
                  HTML("Please select a school")),
                conditionalPanel(
                  condition="input.plot_profile=='enrolled' && input.plot_enrolled=='Interest Groups' && input.plot_school!=' '",
                  countcolchart,
                  conditionalPanel(condition="input.plot_enrolled=='Interest Groups'",
                                   HTML("Starting in the school year 2014-2015, the definition of 'low income' students was expanded and renamed 'economically disadvantaged' (See 'More Info' tab).")),
                  HTML("Data are only shown from years in which schools were in operation and reported data"))
                ,conditionalPanel(
                  condition="input.plot_profile=='enrolled' && input.plot_enrolled=='Interest Groups' && input.plot_school==' '",
                  HTML("Please select a school")),

                #count charts 
                #of mobility enrollment
                conditionalPanel(
                  condition="input.plot_profile=='mobility' && input.plot_mobility_var=='mobenrollment'  && input.plot_school!=' '",
                  mobenrollment_plot_options,
                  HTML("Starting in the school year 2014-2015, the definition of 'low income' students was expanded and renamed 'economically disadvantaged' (See 'More Info' tab)."),
                  HTML("Data are only shown from years in which schools were in operation and reported data")),
                conditionalPanel(
                  condition="input.plot_profile=='mobility' && input.plot_mobility_var=='mobenrollment'  && input.plot_school==' '",
                  HTML("Please select a school")),
                
                #of mobility rate
                conditionalPanel(
                  condition="input.plot_profile=='mobility' && input.plot_mobility_var=='mobrate'  && input.plot_school!=' '",
                  mobrate_plot_options,
                  HTML("Starting in the school year 2014-2015, the definition of 'low income' students was expanded and renamed 'economically disadvantaged' (See 'More Info' tab)."),
                  HTML("Data are only shown from years in which schools were in operation and reported data")),
                conditionalPanel(
                  condition="input.plot_profile=='mobility' && input.plot_mobility_var=='mobrate'  && input.plot_school==' '",
                  HTML("Please select a school"))
                
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
         conditionalPanel("input.map_profile=='enrolled'",

         conditionalPanel("input.lmap_radio=='Race/Ethnicity'",
                          tags$br(),
                          conditionalPanel("input.lmap_level1==' ' ",
                                           HTML("<b>Please choose a variable to map</b>")),
                          conditionalPanel("input.lmap_level1!=' ' ",
                          htmlOutput("lmap_title1")),
                          tags$br(),
                   
          leafletOutput("leafmap1", width=750, height=500),
          HTML("The relative size of the circle markers indicates the total number of students enrolled"),
                         
         
         #Legend- African American
         conditionalPanel(
           condition="input.lmap_level1 == 'African American'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("% African",br(),"American",br(),"Students"),
             plotOutput("AAlegend")
           ))
           ,
         conditionalPanel(
           condition="input.lmap_level1 == 'Asian'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("% Asian",br(),"Students"),
                    plotOutput("ASlegend")
             ))
         ,
         conditionalPanel(
           condition="input.lmap_level1 == 'Hispanic'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("% Hispanic",br(),"Students"),
                    plotOutput("HISPlegend")
             ))
         ,
         conditionalPanel(
           condition="input.lmap_level1 == 'White'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("% White",br(),"Students"),
                    plotOutput("WHlegend")
             ))
         ,
         conditionalPanel(
           condition="input.lmap_level1 == 'Native American'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("% Native",br(),"American",br(),"Students"),
                    plotOutput("NAlegend")
             ))
         ,
         conditionalPanel(
           condition="input.lmap_level1 == 'Native Hawaiian/Pacific Islander'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("% Native",br(),"Hawaiian", br(), "Pacific", br(), "Islander",br(),"Students"),
                    plotOutput("NHlegend")
             ))
         ,
         conditionalPanel(
           condition="input.lmap_level1 == 'Multi-Race/Non-Hispanic'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("% Multi",br(),"Race", br(), "Non-Hispanic", br(),"Students"),
                    plotOutput("MRlegend")
             ))
         ),
         
         conditionalPanel("input.lmap_radio=='Gender'",
                          tags$br(),
                          conditionalPanel("input.lmap_level2==' ' ",
                                           HTML("<b>Please choose a variable to map</b>")),
                          conditionalPanel("input.lmap_level2!=' ' ",
                                           htmlOutput("lmap_title2")),
                          tags$br(),
                          
                          leafletOutput("leafmap2", width=750, height=500),
                          HTML("The relative size of the circle markers indicates the total number of students enrolled"),
                          
         conditionalPanel(
           condition="input.lmap_level2 == 'Females'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("% Female",br(),"Students"),
             plotOutput("FEMlegend")
           ))
         ,
         conditionalPanel(
           condition="input.lmap_level2 == 'Males'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("% Male",br(),"Students"),
             plotOutput("MALlegend")
           ))
         ),
         
         
         conditionalPanel("input.lmap_radio=='Grade Levels'",
                          tags$br(),
                          conditionalPanel("input.lmap_level3==' ' ",
                                           HTML("<b>Please choose a variable to map</b>")),
                          conditionalPanel("input.lmap_level3!=' ' ",
                                           htmlOutput("lmap_title3")),
                          tags$br(),
                          
                          leafletOutput("leafmap3", width=750, height=500),
                          HTML("The relative size of the circle markers indicates the total number of students enrolled"),
                          
         conditionalPanel(
           condition="input.lmap_level3 == 'Pre Kindergarten'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("Pre-",br(),"Kindergarteners"),
             plotOutput("PREKlegend")
           ))
         ,
         conditionalPanel(
           condition="input.lmap_level3 == 'Kindergarten'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("Kindergarteners"),
             plotOutput("KINDlegend")
           ))
         ,conditionalPanel(
           condition="input.lmap_level3 == 'First Grade'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("First",br(),"Graders"),
             plotOutput("FIRSTlegend")
           ))
         ,conditionalPanel(
           condition="input.lmap_level3 == 'Second Grade'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("Second",br(),"Graders"),
             plotOutput("SECONDlegend")
           ))
         ,conditionalPanel(
           condition="input.lmap_level3 == 'Third Grade'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("Third",br(),"Graders"),
             plotOutput("THIRDlegend")
           ))
         ,conditionalPanel(
           condition="input.lmap_level3 == 'Fourth Grade'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("Fourth",br(),"Graders"),
             plotOutput("FOURTHlegend")
           ))
         ,conditionalPanel(
           condition="input.lmap_level3 == 'Fifth Grade'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("Fifth",br(),"Graders"),
             plotOutput("FIFTHlegend")
           ))
         ,conditionalPanel(
           condition="input.lmap_level3 == 'Sixth Grade'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("Sixth",br(),"Graders"),
             plotOutput("SIXTHlegend")
           ))
         ,conditionalPanel(
           condition="input.lmap_level3 == 'Seventh Grade'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("Seventh",br(),"Graders"),
             plotOutput("SEVENTHlegend")
           ))
         ,conditionalPanel(
           condition="input.lmap_level3 == 'Eighth Grade'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("Eighth",br(),"Graders"),
             plotOutput("EIGHTHlegend")
           ))
         ,conditionalPanel(
           condition="input.lmap_level3 == 'Ninth Grade'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("Ninth",br(),"Graders"),
             plotOutput("NINTHlegend")
           ))
         ,conditionalPanel(
           condition="input.lmap_level3 == 'Tenth Grade'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("Tenth",br(),"Students"),
             plotOutput("TENTHlegend")
           ))
         ,conditionalPanel(
           condition="input.lmap_level3 == 'Eleventh Grade'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("Eleventh",br(),"Graders"),
             plotOutput("ELEVENTHlegend")
           ))
         ,conditionalPanel(
           condition="input.lmap_level3 == 'Twelfth Grade'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("Twelfth",br(),"Graders"),
             plotOutput("TWELFTHlegend")
           ))
         ,conditionalPanel(
           condition="input.lmap_level3 == 'Special Ed Beyond 12th Grade'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("Special",br(),"Education", br(), "beyond", br(), "Twelfth", br(), "Graders"),
             plotOutput("SPEClegend")
           ))
         ),
         conditionalPanel("input.lmap_radio=='Interest Groups'",
                          tags$br(),
                          conditionalPanel("input.lmap_level4==' ' ",
                                           HTML("<b>Please choose a variable to map</b>")),
                          conditionalPanel("input.lmap_level4!=' ' ",
                                           htmlOutput("lmap_title4")),
                          tags$br(),
                          
                          leafletOutput("leafmap4", width=750, height=500),
                          HTML("The relative size of the circle markers indicates the total number of students enrolled.  Data on Economically Disadvantaged students replaced data on Low Income students in the 2014-2015 school year."),
         conditionalPanel(
           condition="input.lmap_level4 == 'English Language Learner Enrolled %'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("% English",br(),"Language", br(), "Learners"),
             plotOutput("P_ELL_legend")
           ))
         ,conditionalPanel(
           condition="input.lmap_level4 == 'First Language Not English Enrolled %'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("% First",br(),"Language", br(), "Not", br(), "English"),
             plotOutput("P_FLNE_legend")
           ))
         ,conditionalPanel(
           condition="input.lmap_level4 == 'Students With Disabilities Enrolled %'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("% Students",br(),"with", br(), "Disabilities"),
             plotOutput("P_DISAB_legend")
           ))
         ,conditionalPanel(
           condition="input.lmap_level4 == 'Low Income Students Enrolled %'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("% Low",br(),"Income", br(), "Students"),
             plotOutput("P_LOW_legend")
           ))
         ,conditionalPanel(
           condition="input.lmap_level4 == 'Economically Disadvantaged Students Enrolled %'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("% Economically",br(),"Disadvantaged", br(), "Students"),
             plotOutput("P_ECODIS_legend")
           ))
         ,conditionalPanel(
           condition="input.lmap_level4 == 'High Needs Students Enrolled %'",
           absolutePanel(
             right = 5, top = 130, draggable=FALSE, style = "", 
             class = "floater",
             strong("% High",br(),"Needs", br(), "Students"),
             plotOutput("P_HIGH_legend")
           ))
         )
         ),
         conditionalPanel("input.map_profile=='mobility'  &&  input.lmap_mobility_var=='mobrate'",
         conditionalPanel("input.lmap_radio2=='English Language Learner Students Mobility Rate'",
                          tags$br(),
                          conditionalPanel("input.lmap_level5==' ' ",
                                           HTML("<b>Please choose a variable to map</b>")),
                          conditionalPanel("input.lmap_level5!=' ' ",
                                           htmlOutput("lmap_title5")),
                          tags$br(),
                          
                          leafletOutput("leafmap5", width=750, height=500),
                          HTML("The relative size of the circle markers indicates the total number of students enrolled"),
                          conditionalPanel(
                            condition="input.lmap_level5 == 'Churn Rate for English Language Learning Students'",
                            absolutePanel(
                              right = 5, top = 130, draggable=FALSE, style = "", 
                              class = "floater",
                              strong("Churn Rate", br(), "English",br(),"Language", br(), "Learners"),
                              plotOutput("CR_ELL_legend")
                            )),
                          conditionalPanel(
                            condition="input.lmap_level5 == 'Stability Rate for English Language Learning Students'",
                            absolutePanel(
                              right = 5, top = 130, draggable=FALSE, style = "", 
                              class = "floater",
                              strong("Stability Rate", br(), "English",br(),"Language", br(), "Learners"),
                              plotOutput("SR_ELL_legend")
                            )),
                          conditionalPanel(
                            condition="input.lmap_level5 == 'Intake Rate for English Language Learning Students'",
                            absolutePanel(
                              right = 5, top = 130, draggable=FALSE, style = "", 
                              class = "floater",
                              strong("Intake Rate", br(), "English",br(),"Language", br(), "Learners"),
                              plotOutput("IR_ELL_legend")
                            ))
         ),
         conditionalPanel("input.lmap_radio2=='Students with Disabilities Mobility Rate'",
                          tags$br(),
                          conditionalPanel("input.lmap_level6==' ' ",
                                           HTML("<b>Please choose a variable to map</b>")),
                          conditionalPanel("input.lmap_level6!=' ' ",
                                           htmlOutput("lmap_title6")),
                          tags$br(),
                          
                          leafletOutput("leafmap6", width=750, height=500),
                          HTML("The relative size of the circle markers indicates the total number of students enrolled"),
                          conditionalPanel(
                            condition="input.lmap_level6 == 'Churn Rate for Students with Disabilities'",
                            absolutePanel(
                              right = 5, top = 130, draggable=FALSE, style = "", 
                              class = "floater",
                              strong("Churn Rate", br(), "Students",br(),"with", br(), "Disabilities"),
                              plotOutput("CR_DISAB_legend")
                            )),
                          conditionalPanel(
                            condition="input.lmap_level6 == 'Stability Rate for Students with Disabilities'",
                            absolutePanel(
                              right = 5, top = 130, draggable=FALSE, style = "", 
                              class = "floater",
                              strong("Stability Rate", br(), "Students",br(),"with", br(), "Disabilities"),
                              plotOutput("SR_DISAB_legend")
                            )),
                          conditionalPanel(
                            condition="input.lmap_level6 == 'Intake Rate for Students with Disabilities'",
                            absolutePanel(
                              right = 5, top = 130, draggable=FALSE, style = "", 
                              class = "floater",
                              strong("Intake Rate", br(), "Students",br(),"with", br(), "Disabilities"),
                              plotOutput("IR_DISAB_legend")
                            ))
         ),
         conditionalPanel("input.lmap_radio2=='Low Income Students Mobility Rate'",
                          tags$br(),
                          conditionalPanel("input.lmap_level7==' ' ",
                                           HTML("<b>Please choose a variable to map</b>")),
                          conditionalPanel("input.lmap_level7!=' ' ",
                                           htmlOutput("lmap_title7")),
                          tags$br(),
                          
                          leafletOutput("leafmap7", width=750, height=500),
                          HTML("The relative size of the circle markers indicates the total number of students enrolled. Data on Economically Disadvantaged students replaced data on Low Income students in the 2014-2015 school year."),
                          conditionalPanel(
                            condition="input.lmap_level7 == 'Churn Rate for Low Income Students'",
                            absolutePanel(
                              right = 5, top = 130, draggable=FALSE, style = "", 
                              class = "floater",
                              strong("Churn Rate", br(), "Low",br(),"Income", br(), "Students"),
                              plotOutput("CR_LOW_legend")
                            )),
                          conditionalPanel(
                            condition="input.lmap_level7 == 'Stability Rate for Low Income Students'",
                            absolutePanel(
                              right = 5, top = 130, draggable=FALSE, style = "", 
                              class = "floater",
                              strong("Stability Rate", br(), "Low",br(),"Income", br(), "Students"),
                              plotOutput("SR_LOW_legend")
                            )),
                          conditionalPanel(
                            condition="input.lmap_level7 == 'Intake Rate for Low Income Students'",
                            absolutePanel(
                              right = 5, top = 130, draggable=FALSE, style = "", 
                              class = "floater",
                              strong("Intake Rate", br(), "Low",br(),"Income", br(), "Students"),
                              plotOutput("IR_LOW_legend")
                            ))
         ),
         conditionalPanel("input.lmap_radio2=='Economically Disadvantaged Students Mobility Rate'",
                          tags$br(),
                          conditionalPanel("input.lmap_level13==' ' ",
                                           HTML("<b>Please choose a variable to map</b>")),
                          conditionalPanel("input.lmap_level13!=' ' ",
                                           htmlOutput("lmap_title13")),
                          tags$br(),
                          
                          leafletOutput("leafmap13", width=750, height=500),
                          HTML("The relative size of the circle markers indicates the total number of students enrolled. Data on Economically Disadvantaged students replaced data on Low Income students in the 2014-2015 school year."),
                          conditionalPanel(
                            condition="input.lmap_level13 == 'Churn Rate for Economically Disadvantaged Students'",
                            absolutePanel(
                              right = 5, top = 130, draggable=FALSE, style = "", 
                              class = "floater",
                              strong("Churn Rate", br(), "Economically",br(),"Disadvantaged", br(), "Students"),
                              plotOutput("CR_ECODIS_legend")
                            )),
                          conditionalPanel(
                            condition="input.lmap_level13 == 'Stability Rate for Economically Disadvantaged Students'",
                            absolutePanel(
                              right = 5, top = 130, draggable=FALSE, style = "", 
                              class = "floater",
                              strong("Stability Rate", br(), "Economically",br(),"Disadvantaged", br(), "Students"),
                              plotOutput("SR_ECODIS_legend")
                            )),
                          conditionalPanel(
                            condition="input.lmap_level13 == 'Intake Rate for Economically Disadvantaged Students'",
                            absolutePanel(
                              right = 5, top = 130, draggable=FALSE, style = "", 
                              class = "floater",
                              strong("Intake Rate", br(), "Economically",br(),"Disadvantaged", br(), "Students"),
                              plotOutput("IR_ECODIS_legend")
                            ))
         ),
         
         conditionalPanel("input.lmap_radio2=='High Needs Students Mobility Rate'",
                          tags$br(),
                          conditionalPanel("input.lmap_level8==' ' ",
                                           HTML("<b>Please choose a variable to map</b>")),
                          conditionalPanel("input.lmap_level8!=' ' ",
                                           htmlOutput("lmap_title8")),
                          tags$br(),
                          
                          leafletOutput("leafmap8", width=750, height=500),
                          HTML("The relative size of the circle markers indicates the total number of students enrolled"),
                          conditionalPanel(
                            condition="input.lmap_level8 == 'Churn Rate for High Needs Students Students'",
                            absolutePanel(
                              right = 5, top = 130, draggable=FALSE, style = "", 
                              class = "floater",
                              strong("Churn Rate", br(), "High",br(),"Needs", br(), "Students"),
                              plotOutput("CR_HIGH_legend")
                            )),
                          conditionalPanel(
                            condition="input.lmap_level8 == 'Stability Rate for High Needs Students Students'",
                            absolutePanel(
                              right = 5, top = 130, draggable=FALSE, style = "", 
                              class = "floater",
                              strong("Stability Rate", br(), "High",br(),"Needs", br(), "Students"),
                              plotOutput("SR_HIGH_legend")
                            )),
                          conditionalPanel(
                            condition="input.lmap_level8 == 'Intake Rate for High Needs Students Students'",
                            absolutePanel(
                              right = 5, top = 130, draggable=FALSE, style = "", 
                              class = "floater",
                              strong("Intake Rate", br(), "High",br(),"Needs", br(), "Students"),
                              plotOutput("IR_HIGH_legend")
                            ))
         )
         ),
         conditionalPanel("input.map_profile=='mobility'  &&  input.lmap_mobility_var=='mobenrollment'",
         conditionalPanel("input.lmap_radio3=='English Language Learner Students Mobility Enrollment'",
                          tags$br(),
                          conditionalPanel("input.lmap_level9==' ' ",
                                           HTML("<b>Please choose a variable to map</b>")),
                          conditionalPanel("input.lmap_level9!=' ' ",
                                           htmlOutput("lmap_title9")),
                          tags$br(),
                          
                          leafletOutput("leafmap9", width=750, height=500),
                          HTML("The relative size of the circle markers indicates the total number of students enrolled"),
                          conditionalPanel(
                            condition="input.lmap_level9 == 'Churn Enrollment for English Language Learning Students'",
                            absolutePanel(
                              right = 5, top = 130, draggable=FALSE, style = "", 
                              class = "floater",
                              strong("Churn", br(), "Enrollment", br(), "English",br(),"Language", br(), "Learners"),
                              plotOutput("CE_ELL_legend")
                            )),
                          conditionalPanel(
                            condition="input.lmap_level9 == 'Stability Enrollment for English Language Learning Students'",
                            absolutePanel(
                              right = 5, top = 130, draggable=FALSE, style = "", 
                              class = "floater",
                              strong("Stability", br(), "Enrollment", br(), "English",br(),"Language", br(), "Learners"),
                              plotOutput("SE_ELL_legend")
                            ))
         ),
         conditionalPanel("input.lmap_radio3=='Students with Disabilities Mobility Enrollment'",
                          tags$br(),
                          conditionalPanel("input.lmap_level10==' ' ",
                                           HTML("<b>Please choose a variable to map</b>")),
                          conditionalPanel("input.lmap_level10!=' ' ",
                                           htmlOutput("lmap_title10")),
                          tags$br(),
                          
                          leafletOutput("leafmap10", width=750, height=500),
                          HTML("The relative size of the circle markers indicates the total number of students enrolled"),
                          conditionalPanel(
                            condition="input.lmap_level10 == 'Churn Enrollment for Students with Disabilities'",
                            absolutePanel(
                              right = 5, top = 130, draggable=FALSE, style = "", 
                              class = "floater",
                              strong("Churn", br(),"Enrollment", br(), "Students",br(),"with", br(), "Disabilities"),
                              plotOutput("CE_DISAB_legend")
                            )),
                          conditionalPanel(
                            condition="input.lmap_level10 == 'Stability Enrollment for Students with Disabilities'",
                            absolutePanel(
                              right = 5, top = 130, draggable=FALSE, style = "", 
                              class = "floater",
                              strong("Stability", br(), "Enrollment", br(), "Students",br(),"with", br(), "Disabilities"),
                              plotOutput("SE_DISAB_legend")
                            ))
                         ),
         conditionalPanel("input.lmap_radio3=='Low Income Students Mobility Enrollment'",
                          tags$br(),
                          conditionalPanel("input.lmap_level11==' ' ",
                                           HTML("<b>Please choose a variable to map</b>")),
                          conditionalPanel("input.lmap_level11!=' ' ",
                                           htmlOutput("lmap_title11")),
                          tags$br(),
                          
                          leafletOutput("leafmap11", width=750, height=500),
                          HTML("The relative size of the circle markers indicates the total number of students enrolled.  Data on Economically Disadvantaged students replaced data on Low Income students in the 2014-2015 school year."),
                          conditionalPanel(
                            condition="input.lmap_level11 == 'Churn Enrollment for Low Income Students'",
                            absolutePanel(
                              right = 5, top = 130, draggable=FALSE, style = "", 
                              class = "floater",
                              strong("Churn Enrollment", br(), "Low",br(),"Income", br(), "Students"),
                              plotOutput("CE_LOW_legend")
                            )),
                          conditionalPanel(
                            condition="input.lmap_level11 == 'Stability Enrollment for Low Income Students'",
                            absolutePanel(
                              right = 5, top = 130, draggable=FALSE, style = "", 
                              class = "floater",
                              strong("Stability Enrollment", br(), "Low",br(),"Income", br(), "Students"),
                              plotOutput("SE_LOW_legend")
                            ))
         ),
         conditionalPanel("input.lmap_radio3=='Economically Disadvantaged Students Mobility Enrollment'",
                          tags$br(),
                          conditionalPanel("input.lmap_level14==' ' ",
                                           HTML("<b>Please choose a variable to map</b>")),
                          conditionalPanel("input.lmap_level14!=' ' ",
                                           htmlOutput("lmap_title14")),
                          tags$br(),
                          
                          leafletOutput("leafmap14", width=750, height=500),
                          HTML("The relative size of the circle markers indicates the total number of students enrolled. Data on Economically Disadvantaged students replaced data on Low Income students in the 2014-2015 school year."),
                          conditionalPanel(
                            condition="input.lmap_level14 == 'Churn Enrollment for Economically Disadvantaged Students'",
                            absolutePanel(
                              right = 5, top = 130, draggable=FALSE, style = "", 
                              class = "floater",
                              strong("Churn Enrollment", br(), "Economically",br(),"Disadvantaged", br(), "Students"),
                              plotOutput("CE_ECODIS_legend")
                            )),
                          conditionalPanel(
                            condition="input.lmap_level14 == 'Stability Enrollment for Economically Disadvantaged Students'",
                            absolutePanel(
                              right = 5, top = 130, draggable=FALSE, style = "", 
                              class = "floater",
                              strong("Stability Enrollment", br(), "Economically",br(),"Disadvantaged", br(), "Students"),
                              plotOutput("SE_ECODIS_legend")
                            ))
         ),
         conditionalPanel("input.lmap_radio3=='High Needs Students Mobility Enrollment'",
                          tags$br(),
                          conditionalPanel("input.lmap_level12==' ' ",
                                           HTML("<b>Please choose a variable to map</b>")),
                          conditionalPanel("input.lmap_level12!=' ' ",
                                           htmlOutput("lmap_title12")),
                          tags$br(),
                          
                          leafletOutput("leafmap12", width=750, height=500),
                          HTML("The relative size of the circle markers indicates the total number of students enrolled"),
                          conditionalPanel(
                            condition="input.lmap_level12 == 'Churn Enrollment for High Needs Students'",
                            absolutePanel(
                              right = 5, top = 130, draggable=FALSE, style = "", 
                              class = "floater",
                              strong("Churn Enrollment", br(), "High",br(),"Needs", br(), "Students"),
                              plotOutput("CE_HIGH_legend")
                            )),
                          conditionalPanel(
                            condition="input.lmap_level12 == 'Stability Enrollment for High Needs Students'",
                            absolutePanel(
                              right = 5, top = 130, draggable=FALSE, style = "", 
                              class = "floater",
                              strong("Stability Enrollment", br(), "High",br(),"Needs", br(), "Students"),
                              plotOutput("SE_HIGH_legend")
                            ))
                          )
         )
         
         ,
         
         
         value="lmap"
         )
,
        tabPanel("More Info", 
                 p(strong("Variable Summary:")),
                 tags$ul(
                 tags$li((p(strong("Enrollment"), "- The number of students enrolled in a school."))),
                 tags$br(),
                 tags$li(p(strong("English Language Learners"), "- Students whose first language is a language other than English, and who are unable to perform ordinary classroom work in English.")),
                 tags$br(),
                 tags$li((p(strong("First Language Not English"), "- Students whose first language is a language other than English, and who are able to perform ordinary classroom work in English."))),
                 tags$br(),
                 tags$li(p(strong("Students With Disabilities"), "- Students who have received Individualized Education Program (IEP).")),
                 tags$br(),
                 tags$li(p(strong("High Needs"), "- A student is considered high needs if he or she is either low income (prior to School Year 2014-2015), economically disadvantaged (starting in School Year 2014-2015), an English language learner, or a student with disabilities. Data on High Needs students were not collected until 2011-2012.")),
                 tags$br(),
                 tags$li(p(strong("Low Income"), "- A student is considered low income if he or she meets any of the following criteria: they are eligible for free or reduced price lunch, they receive Transitional Aid to Families (TANF) benefits, or are eligible to receive food stamps.")),
                 tags$br(),
                 tags$li(p(strong("Economically Disadvantaged"), "- A student is considered economically disadvantaged if he or she participates in any of the following state programs: Supplemental Nutrition Assistance Program (SNAP), the Transitional Assistance for Families with Dependent Children (TAFDC), the Department of Children and Families' (DCF) foster care program and MassHealth (Medicaid).")),
                 tags$br(),
                 tags$li(p(strong("Mobility Data"), "- Mobility data were first reported for the 2007-2008 school year.")),
                 tags$br(),
                 tags$li(p(strong("Intake Rate"),
                           "- Measures the number of students that enroll in a school after the official start of the school year. Intake rate is calculated by dividing the number of students who enroll in a school after the official start of the school year by the total number of students that enrolled in that school.  Intake rates are collected from each school's School Information Management System.")),
                 tags$br(),
                 tags$li((p(strong("Churn Rate"),
                            "- Measures the number of students who transfer in or out of a school during the course of a single school year. Churn rate is calculated by dividing by the total number of students enrolled in that school."))),
                 tags$br(),
                 tags$li((p(strong("Stability Rate"), "- Measures how many students remain in a district or school throughout the school year.")))
                 ),
                
                 
                 
                 
                 
                 
                 
                 ## email feedback link
                 h3(a("Please fill out our survey to help improve the site!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank")), 
                 value="info"),
        id="tabs"
             
        )
      ))
    )
  ))