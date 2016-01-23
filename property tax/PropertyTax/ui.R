#######################################
## Title: Property Tax ui.R          ##
## Author(s): Xuelian Li, Jenna      ##
##            Kiridly                ## 
## Date Created:  01/07/16           ##
## Date Modified: 01/07/16 XL        ##
#######################################
shinyUI(fluidPage(
  ## HTML to create generate map button
  gen_map_button,
  ## this starts the googleCharts engine
  googleChartsInit(),
  
  ## blank title, but put in a special title for window tab
  titlePanel("", windowTitle = "SEIGMA: Property Tax App"),
  
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
                                 min=2003, max=2013, value=2013,
                                 sep="")
                   ),
                   conditionalPanel(
                     ## Initializes a multi-year slider (range)
                     condition="input.sum_timespan == 'mult.yrs'",
                     ## Slider starts from 2010-2012
                     sliderInput("sum_range", "Select Years",
                                 min=2003, max=2013, value=c(2010,2013),
                                 sep="")
                   ),
                   ## in summary, allow for municipal selection
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
                               choices = MA_municipals, selected="Amherst", multiple=TRUE),
                   radioButtons("plot_radio", "Select Variable of Interest",
                                c("Total Tax Levy" = "Total_Levy",
                                  "Percent of Levy by Class" = "Percent_Levy"),
                                selected="Total_Levy"),
                   conditionalPanel(
                     condition="input.plot_radio == 'Total_Levy'",
                     ## In plot, show boxes that will compare to MA average
                     radioButtons("plot_display_radio", "Display Options",
                                  c("Actual Values"="Inflation_Adjusted_Total_Levy", "Change Since 2003"="Total_Levy_Pct_Change"),
                                  selected="Inflation_Adjusted_Total_Levy")),
                   
                   conditionalPanel(
                     condition="input.plot_radio == 'Percent_Levy'",
                     radioButtons("plot_class_radio", "Percent of Levy by Class",
                                  c("Residential"="Percentage_of_Residential", "Commercial"="Percentage_of_Commercial",
                                    "Industrial"="Percentage_of_Industrial", "Personal Property"="Percentage_of_Personal_Property"),
                                  selected="Percentage_of_Residential")
                   )
                 ),
                 
                 ## in map, allow for timespan selection
                 conditionalPanel(
                   condition="input.tabs == 'map'",
                   ## Initializing a single slider
                   sliderInput("map_year", "Select Year",
                               min=2003, max=2013, value=2013,
                               sep=""),
                   radioButtons("map_radio", "Select Variable of Interest",
                                c("Total Tax Levy" = "Total_Levy",
                                  "Percent of Levy by Class" = "Percent_Levy"),
                                selected="Total_Levy"),
                   conditionalPanel(
                     condition="input.map_radio == 'Total_Levy'",
                     ## In plot, show boxes that will compare to MA average
                     radioButtons("map_display_radio", "Display Options",
                                  c("Actual Values"="Inflation_Adjusted_Total_Levy", "Change Since 2003"="Total_Levy_Pct_Change"),
                                  selected="Inflation_Adjusted_Total_Levy")),
                   conditionalPanel(
                     condition="input.map_radio == 'Percent_Levy'",
                     radioButtons("map_class_radio", "Percent of Levy by Class",
                                  c("Residential"="Percentage_of_Residential", "Commercial"="Percentage_of_Commercial",
                                    "Industrial"="Percentage_of_Industrial", "Personal Property"="Percentage_of_Personal_Property"),
                                  selected="Percentage_of_Residential"))
                 ),
                 
                 
                 tags$hr(),
                 
                 ## author line
                 
                 helpText("Created by Xuelian Li, Jenna F. Kiridly"),
                 
                 
                 ## email feedback link
                 ## To develop a link in HTML
                 helpText(a("Send us your comments or feedback!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank")),
                 
                 ## data source citation
                 helpText(a("Data Source: Massachusetts Department of Revenue", href="https://dlsgateway.dor.state.ma.us/reports/rdPage.aspx?rdReport=PropertyTaxInformation.taxratesbyclass.taxratesbyclass_main",
                            target="_blank")),
                 
                 ## GitHub link
                 helpText(a("View the data and code on GitHub", 
                            href="https://github.com/sEigmA/SEIGMA/tree/gh-pages/property%20tax", target="_blank")),
                 
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
                   condition="input.plot_radio =='Total_Levy'",
                   conditionalPanel(
                     condition="input.plot_display_radio=='Inflation_Adjusted_Total_Levy'", 
                   ## make chart title here (otherwise not centered)
                   h4("Annual Total Tax Levy by Region Over Time", align="center"),
                   TotTax_plot_options),
                   conditionalPanel(
                     condition="input.plot_display_radio=='Total_Levy_Pct_Change'", 
                     ## make chart title here (otherwise not centered)
                     h4("Change in Total Tax Levy since 2003", align="center"),
                     TaxCha_plot_options)
                 ),
                 
                 conditionalPanel(
                   condition="input.plot_radio =='Percent_Levy'",
                   Pct_plot_options,
                     p(strong("Change Since 2003"), "- This is calculated by the total annual tax to the year 2003.  We selected 2003 in order to provide a ten year baseline period.  The baseline year of 2003 is considered '0' for these calculations. A positive number indicates an increase from 2003 and a negative number indicates a decrease from 2003.")
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
                 
                 ## Total_Levy Legend
                 conditionalPanel(
                   condition="input.map_radio == 'Total_Levy' && input.map_display_radio == 'Inflation_Adjusted_Total_Levy' && input.action != 0",
                   absolutePanel(
                     right = 5, top = 130, draggable=FALSE, style = "", 
                     class = "floater",
                     strong("Annual Total"),
                     br(),
                     strong("Tax Levy"),
                     plotOutput("legend1")
                   )),
                 
                 ## Total_Levy Change Legend
                 conditionalPanel(
                   condition="input.map_radio == 'Total_Levy' && input.map_display_radio == 'Total_Levy_Pct_Change' && input.action != 0",
                   absolutePanel(
                     right = 5, top = 130, draggable=FALSE, style = "", 
                     class = "floater",
                     strong("Change in"),
                     br(),
                     strong("Total Levy"),
                     br(),
                     strong("Since 2003"),
                     plotOutput("legend3")
                   )), 
                 
                 ## Percent_Levy Legend
                 conditionalPanel(
                   condition="input.map_radio == 'Percent_Levy' && input.action != 0",
                   absolutePanel(
                     right = 5, top = 130, draggable=FALSE, style = "", 
                     class = "floater",
                     strong("Percent of"),
                     br(),
                     strong("Levy by Class"),
                     plotOutput("legend2")
                   )),
                 
                  #plot_main_text,
                 value="map"),
        
        tabPanel("More Info", 
                 p(strong("Variable Summary:")),
                 tags$br(),
                
                   
                   p(strong("Total Tax Levy"), 
                             "-a levy, or tax, on property that the owner is required to pay. The tax is given by the region in which the property is located."),
                   tags$br(),
                   p(strong('Class - Residential'),
                             "-All property including one or more homes.  It also includes accessory buildings and land such as pools, tennis, courts, sheds, etc.  "),
                 tags$br(),
                 p(strong('Class- Commercial'), 
                            "-Property for the purpose of conducting a business.  This includes office buildings, retail stores, etc. Personal Property is also considered in this category of Commercial Class."),
                 tags$br(),
                 p(strong('Class- Industrial'), 
                   "-Property involved in manufacturing or processing. This includes porperty used for storage, transmission, and the regulated generation of utilities."),
                 tags$
                 
                 #tags$br(),
                 # p("SEIGMA. Social and Economic Impacts of Gambling in Massachusetts, University of Massachusetts School of Public Health and Health Sciences. (2014). Report on the Social and Economic Impact of Gambling in Massachusetts SEIGMA Gambling study. Report to the Massachusetts Gaming Commission & the Massachusetts department of Public Health. Retrieved from:"), a("http://www.umass.edu/seigma/sites/default/files/March%202014%20SEIGMA%20Report_6-19_for%20website.pdf"),                  
                 
                 ## email feedback link
                 h3(a("Please fill out our survey to help improve the site!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank")), value="info"),
        id="tabs")) #ends tableset panel
      ))#end bootstrap page
  ))