#######################################
## Title: Unemployment uil.R          ##
## Author(s): Xuelian Li,Jenna Kiridly##
##            Emily Ramos, Arvind     ##
##            Ramakrishnan,           ##
## Date Created:  01/07/2015          ##
## Date Modified: 11/13/2015  XL      ##
#######################################

shinyUI(fluidPage(
  ## HTML to create generate map button
  gen_map_button,
  ## this starts the googleCharts engine
  googleChartsInit(),
  
  ## blank title, but put in a special title for window tab
  titlePanel("", windowTitle = "SEIGMA: Unemployment App"),
  
  ## Create sidebar
  sidebarLayout(
    sidebarPanel(width=4,
      ## Conditional panel means if the condition is met show all text below otherwise Don't!
      summary_side_text,
      
      plot_side_text,
      
      map_side_text,
      
      info_side_text,
      
      ## in map, allow for variable selection
      ## in summary, allow for timespan selection
      ## if single year is selected, select year. if multiple years are selected, choose range.
      conditionalPanel(
        condition="input.tabs == 'summary'",
        ## Select input = Drop down list of timespan (variable name on server side) 
        selectInput("sum_timespan", "Select Timespan",
                    list("Single Year" = "sing.yr",
                         "Multiple Years" = "mult.yrs"),selected = "sing.yr"),
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
        ## in summary, allow for municipal selection
        selectInput("sum_muni", "Select Municipality", 
                    choices = MA_municipals,
                    ## Multiple allows for multi-county selection
                    multiple=TRUE),
        ## In summary and plot, show boxes that will compare to MA or US average
        ## False at the end means it starts off unchecked
        checkboxInput("sum_MA_mean", "Compare to MA Average", FALSE)
              ),
      
      ## in plot, allow for municipal selection
      conditionalPanel(
        condition="input.tabs == 'plot'",
        ## Select input = List
        selectInput("plot_muni", "Select Municipality", 
                    choices = MA_municipals, selected="Amherst", multiple=TRUE),
        radioButtons("plot_radio", "Select Variable of Interest",
                     c("Unemployment Rate" = "Unemployment_Rate_Avg",
                       "Labor Force" = "Labor Force"),
                     selected="Unemployment_Rate_Avg"),
        conditionalPanel(
          condition="input.plot_radio == 'Unemployment_Rate_Avg'",
          ## In plot, show boxes that will compare to MA average
          checkboxInput("plot_MA", "Compare to MA", FALSE)),
        
        conditionalPanel(
          condition="input.plot_radio == 'Labor Force'",
          radioButtons("plot_display_radio", "Display Options",
                     c("Actual Values"="No_Labor_Avg", "Change Since 2003"="Labor_Pct_Change"),
                     selected="No_Labor_Avg"),
          conditionalPanel(
            condition="input.plot_display_radio=='Labor_Pct_Change'",
            ## In plot, show boxes that will compare to MA average
            checkboxInput("plot2_MA", "Compare to MA", FALSE)
          )
        )
      ),
      
      ## in map, allow for timespan selection
     conditionalPanel(
        condition="input.tabs == 'map'",
        ## Initializing a single slider
          sliderInput("map_year", "Select Year",
                      min=2003, max=2012, value=2012,
        sep=""),
        radioButtons("map_radio", "Select Variable of Interest",
                     c("Unemployment Rate" = "Unemployment_Rate_Avg",
                       "Labor Force" = "Labor Force"),
                     selected="Unemployment_Rate_Avg"),
        conditionalPanel(
          condition="input.map_radio == 'Labor Force'",
        radioButtons("map_display_radio", "Display Options",
                     c("Actual Values"="No_Labor_Avg", "Change Since 2003"="Labor_Pct_Change"),
                     selected="No_Labor_Avg"))
       ),
        
           
      tags$hr(),
      
      ## author line

      helpText("Created by Xuelian Li, Jenna F. Kiridly, Emily R. Ramos and Arvind Ramakrishnan"),

      
      ## email feedback link
      ## To develop a link in HTML
      helpText(a("Send us your comments or feedback!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank")),
      
      ## data source citation
      helpText(a("Data Source: Bureau of Labor Statistics", href="http://www.bls.gov/lau/data.htm",
                 target="_blank")),
      
      ## GitHub link
      helpText(a("View the data and code on GitHub", 
                 href="https://github.com/sEigmA/SEIGMA/tree/gh-pages/unemployment", target="_blank")),
      
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
                conditionalPanel(
                   condition="input.plot_radio =='Unemployment_Rate_Avg'",
                  ## make chart title here (otherwise not centered)
                     h4("Annual Average Unemployment Rate by Region Over Time", align="center"),
                     une_plot_options),
                   
                 conditionalPanel(
                   condition="input.plot_radio =='Labor Force'",
                   conditionalPanel(
                     condition="input.plot_display_radio=='No_Labor_Avg'",
                     ## make chart title here (otherwise not centered)
                     h4("Annual Average # in Labor Force Over Time", align="center"),
                     lab_plot_options),
                   conditionalPanel(
                     condition="input.plot_display_radio=='Labor_Pct_Change'",
                     ## make chart title here (otherwise not centered)
                     h4("Change in Labor Force Since 2003", align="center"),
                     lab_pct_plot_options,
                     p(strong("Change Since 2003"), "- This is calculated by comparing average labor force for a specific year to the year 2003.  We selected 2003 in order to provide a ten year baseline period.  The baseline year of 2003 is considered '0' for these calculations. A positive number indicates an increase from 2003 and a negative number indicates a decrease from 2003.")
                     )
                 ),
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
                   absolutePanel(left=200, top=450, width=300, class="floater",
                                 htmlOutput("details"))),
                 
                 conditionalPanel(
                   condition="input.tabs == 'map' && input.action == 0",
                   ## within the map area, you can create an action button.  similar to initializing the legend but just putting a button instead.
                   absolutePanel(right = 400, top = 300, class = "floater",
                                 actionButton("action", "Generate Map")
                 )),
                 
                 ## Unemployment Rate Legend
                 conditionalPanel(
                   condition="input.map_radio == 'Unemployment_Rate_Avg' && input.action != 0",
                   absolutePanel(
                     right = 5, top = 130, draggable=FALSE, style = "", 
                     class = "floater",
                     strong("Annual Average"),
                     br(),
                     strong("Unemployment"),
                    br(),
                     strong("Rate"),
                     plotOutput("legend1")
                    )),
                 
                ## Labor Force Legend
                 conditionalPanel(
                   condition="input.map_radio == 'Labor Force' && input.map_display_radio == 'No_Labor_Avg' && input.action != 0",
                   absolutePanel(
                     right = 5, top = 130, draggable=FALSE, style = "", 
                     class = "floater",
                     strong("Annual Average #"),
                     br(),
                     strong("in Labor Force"),
                     plotOutput("legend2")
                     )),
                 
                 ## Labor Force Change Legend
                 conditionalPanel(
                   condition="input.map_radio == 'Labor Force' && input.map_display_radio == 'Labor_Pct_Change' && input.action != 0",
                   absolutePanel(
                     right = 5, top = 130, draggable=FALSE, style = "", 
                     class = "floater",
                     strong("Change in"),
                     br(),
                     strong("Labor Force"),
                     br(),
                     strong("Since 2003"),
                     plotOutput("legend3")
                     )),     
                                   
#                plot_main_text,
                 value="map"),
        
        tabPanel("More Info", 
                 p(strong("Variable Summary:")),
                 tags$br(),
                
    
                 p(strong("Umemployed Persons"), 
                            "-All people 16 years old and over are classified as unemployed if they were not at work during the survey period, but were actively seeking work during the last 4 weeks, and were available to start a job. Also included as unemployed are those who did not work at all during the reference week, those were waiting to be called back to a job from which they had been laid off, and those who were available for work except for temporary illness."),
                  tags$br(),
                  p(strong('Unemployment Rate'),
                              "-The unemployment rate is produced by the Bureau of Labor Statistics (BLS), which uses state and national level information from the Current Population Survey (CPS).  Municipality unemployment rates were gathered form a secition of the BLS and CPS called the Local Areas Unemployment Statistics Series. The unemployment rate represents the number of unemployed people as a proportion of the total labor force. For example, if the civilian labor force equals 100 people and 7 people are unemployed, then the unemployment rate would be 7 percent."),
                  tags$br(),
  
                p(strong('Average Number in Labor Force'), 
                           "-The labor force includes all persons classified as employed or unemployed."),
                tags$br(),
              p(strong('Bureau of Labor Statistics'), 
                           "-These data were collected from the Bureau of Labor Statistics (BLS). It's helpful to note that the BLS is subject to annual revision revised to reflect new population controls from the Census Bureau, updated input data, and re-estimation.  Links to the data source will be updated when new data are available."),
                   
                 #tags$br(),
                # p("SEIGMA. Social and Economic Impacts of Gambling in Massachusetts, University of Massachusetts School of Public Health and Health Sciences. (2014). Report on the Social and Economic Impact of Gambling in Massachusetts SEIGMA Gambling study. Report to the Massachusetts Gaming Commission & the Massachusetts department of Public Health. Retrieved from:"), a("http://www.umass.edu/seigma/sites/default/files/March%202014%20SEIGMA%20Report_6-19_for%20website.pdf"),                  
                 
                 ## email feedback link
                 h3(a("Please fill out our survey to help improve the site!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank")), value="info"),
        id="tabs"
      )) #ends tableset panel
    ))#end bootstrap page
))