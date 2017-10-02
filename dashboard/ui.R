################################
## Title: Dashboard ui        ##
## Author: Zhenning Kang      ##
## Date Created:  09/27/2017  ##
## Date Modified: 10/01/2017  ##
################################

library(shiny)
library(shinydashboard)
library(DT)

header <- dashboardHeader(title = "SEIGMA Dashboard")

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Municipality", icon = icon("address-book"),
                 selectInput("muni", "Select Your Municipality",
                             choices = MA_municipals,
                             multiple = TRUE)
        ),
        
        menuItem("Year Range", icon = icon("calendar"),
                 selectInput("year", "Select Five Year Range",
                             choices = list("2005-2009" = "2005-2009",
                                            "2006-2010" = "2006-2010",
                                            "2007-2011" = "2007-2011",
                                            "2008-2012" = "2008-2012",
                                            "2009-2013" = "2009-2013",
                                            "2010-2014" = "2010-2014",
                                            "2011-2015" = "2011-2015"),
                             selected = "2006-2010",
                             multiple = FALSE)
        ),

        menuItem("Comparison", icon = icon("check-square-o"),
                 checkboxInput("MA_mean", "Compare to MA Average", TRUE),
                 checkboxInput("US_mean", "Compare to US Average", TRUE)
        ),
        
        menuItem("Demographics", icon = icon("area-chart"),
                 radioButtons("demo", "Select a Variable of Interest",
                              c("Age" = "Age", "Gender" = "Gender","Race" = "Race","Ethnicity" ="Ethnicity"),
                              selected="Age",
                              width = '98%')
        ),

        menuItem("Marital Status", icon = icon("heart"),
                 selectInput("gender", "Select a Gender",
                             choices = c("Female","Male"),
                             multiple = FALSE),
                 selectInput("plotvar", "Select Variable of Interest",
                             choices = list("Never Married" = "Never_Married_pct",
                                            "Married" = "Married_pct",
                                            "Separated" = "Separated_pct",
                                            "Widowed" = "Widowed_pct",
                                            "Divorced" = "Divorced_pct"),
                             width = '98%'),
                 radioButtons("plotcombine", "Show Plots by Gender",
                              choices = list("Separated" = "Separate",
                                             "Together" = "Together"),
                              width = '98%')
        ),
        
        # menuItem("Employment", icon = icon("bank"),
        #          radioButtons("plot_empl", "Select Variable of Interest",
        #                       c("Employment and Business Establishments" = "Employment and Establishments","Wages" = "Wages"),
        #                       selected="Employment and Establishments",
        #                       width = '98%'),
        #          radioButtons("plot_display_empl", "Display Options",
        #                       c("Actual Values"="Actual Values", "Change Since 2003"="Change_Pct"),
        #                       selected="Actual Values",
        #                       width = '98%')
        # ),
        # 
        menuItem("Distance", icon = icon("automobile"),
                 sliderInput("slider", "Choose a Distance", min=0, max=10, value=0))
            
        )
    )

body <- dashboardBody(
    fluidRow(
        tabBox(
            title = "Education",
            tabPanel("Summary", 
                     dataTableOutput("summary_edu"), value="summary", 
                     tags$style(type="text/css", '#summary tfoot {display:none;}')),
            
            ## plot tab with google chart options
            tabPanel("Plot",
                     ## make chart title here (otherwise not centered)
                     h4("Educational Attainment by Region Over Selected Five Year Period", align="center"),
                     plotOutput("plot_edu"),
                     value="plot")
        ),
        
        tabBox(
            title = "Rent",
            id = "tabset1",
            
            ## summary tab
            tabPanel("Summary", dataTableOutput("summary_rent"), value="summary", 
                     tags$style(type="text/css", '#summary tfoot {display:none;}')),
            
            ## plot tab with google chart options
            tabPanel("Plot",
                     ## make chart title here (otherwise not centered)
                     h4("Inflation-Adjusted (2015 $) Median Annual Rent Over Selected Five Year Period", align="center"),
                     ## make a row to put two charts in
                     plotOutput("plot"),
                     HTML("Horizontal grey bars indicate the span of five-year estimates, vertical grey bars with hinges indicate the standard errors"),
                     value="plot")
        )
    ),
    fluidRow(
        tabBox(
            title = "Demographic",
            id = "tabset2",
            tabPanel("Summary",
                     dataTableOutput("summary_demo"), value="summary",
                     tags$style(type="text/css", '#summary tfoot {display:none;}')),
            
            ## plot tab with google chart options
            tabPanel("Plot",
                     plotOutput("plot_demo"))
        ),
        
        tabBox(
            title = "Marital Status",
            tabPanel("Summary", 
                     dataTableOutput("summary_mar"),
                     tags$br(),
                     HTML("Population includes individuals aged 15 years and older."),
                     tags$br(),
                     value="summary", 
                     tags$style(type="text/css", '#summary tfoot {display:none;}')),
            
            ## plot tab with google chart options
            tabPanel("Plot",
                     ## make chart title here (otherwise not centered)
                     conditionalPanel(
                         condition="input.plotcombine=='Separate'",
                         h4("Marital Status as a Percentage of the Population by  Gender", align="center"),                 
                         
                         ## make a row to put two charts in
                         
                         plotOutput("fplot"), 
                         plotOutput("mplot")
                     )
                     ,
                     conditionalPanel(condition = "input.plotcombine=='Together'",
                                      h4("Marital Status as a Percentage of the Population by  Gender", align="center"),                 
                                      plotOutput("fmplot")
                     ),

                     HTML("Horizontal grey bars indicate the span of five-year estimates, vertical grey bars with hinges indicate the standard errors. Population includes individuals aged 15 years and older."),
                     value="plot")
        )
    )
)

dashboardPage(header, sidebar, body, skin = "red")
