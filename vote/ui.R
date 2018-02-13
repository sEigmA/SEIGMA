###############################
## Title: ui.R               ##
## App: SEIGMA VOTE          ##
## Author: Zhenning Kang     ##
## Date Created:  01/12/2018 ##
## Last Modified: 02/13/2018 ##
###############################

library(leaflet)

# Choices for drop-downs
vars <- c(
  "Did this town voted Yes ?" = "vote",
  "Municipal Population" = "population",
  "Percent of People w/ Bachelor Degree" = "bachelor",
  "Median Income" = "income",
  "Unemployment Rate" = "unemployment"
)

navbarPage("SEIGMA VOTE APP", id="nav",
           #header = a(img(src = "logo.jpg", height=105, width=920), href="http://www.umass.edu/seigma/"),
           tabPanel("Interactive map",
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("map", width="100%", height="100%"),
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      h2("Select Your Interest"),
                                      
                                      selectInput("color", "Color", vars),
                                      selectInput("size", "Size", vars, selected = "income"),
                                      plotOutput("histCentile", height = 200),
                                      plotOutput("scatterCollegeIncome", height = 250)
                        )
                    )
           ),
           
           tabPanel("Data explorer",
                    fluidRow(
                      column(6,
                             selectInput("muni", "Select Municipality", 
                                         ma_muni, multiple=TRUE, selected = "Plainville")
                    )
                    ),
                    # fluidRow(
                    #   column(3,
                    #          numericInput("minRate", "Min Yes Rate", min=0, max=100, value=0)
                    #   ),
                    #   column(3,
                    #          numericInput("maxRate", "Max Yes Rate", min=0, max=100, value=100)
                    #   )
                    # ),
                    hr(),
                    DT::dataTableOutput("votetable")
           ),
           
           conditionalPanel("false", icon("crosshair"))
)
