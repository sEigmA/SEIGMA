## load libraries and datasets
library(shiny)
munidata <- read.csv(file="munidata.csv")
countydata <- read.csv(file="countydata.csv")
madata <- read.csv(file="madata.csv")
usdata <- read.csv(file="usdata.csv")
mardata <- read.csv(file="marriagedata.csv")[,-1]

## create Shiny UI
shinyUI(pageWithSidebar(
  headerPanel(a(img(src = "logo.jpg", height=105, width=920), href="http://www.umass.edu/seigma/")),
  sidebarPanel(
    helpText("If using Internet Explorer, application only visible in version 10."),
    tags$hr(),
    
    ## choose variable to observe
    selectInput("var", "Select Variable", 
                list("Never Married" = "nm",
                     "Now Married (except separated)" = "nmes",
                     "Separated" = "sep",
                     "Widowed" = "wid",
                     "Divorced" = "div")
    ),
    
    ## choose data scope
    selectInput("type", "Select Region Type",
                list("Municipal" = "muni",
                     "County" = "county")),
    
    ## in map or summary choose timespan
    conditionalPanel(
      condition="input.tabs == 'map' || input.tabs == 'summary'",
      selectInput("timespan", "Select Timespan",
                  list("Single Year" = "sing.yr",
                       "Multiple Years" = "mult.yrs"))
    ),
    
    conditionalPanel(
      condition="input.tabs == 'map' || input.tabs == 'summary'",
      conditionalPanel(
        condition="input.timespan == 'sing.yr'",
        sliderInput("year", "Select Year (five year moving average)",
                    min=2010, max=2012, value=2012,
                    format="####")),
      conditionalPanel(
        condition="input.timespan == 'mult.yrs'",
        sliderInput("range", "Select Years (five year moving average)",
                    min=2010, max=2012, value=c(2011,2012),
                    format="####")
      )
    ),
    
    ## in plot or summary choose municipality
    conditionalPanel(
      condition="input.tabs == 'plot' || input.tabs == 'summary'",
      conditionalPanel(
        condition="input.type == 'muni'",
        selectInput("muni", "Select Municipality (max of 7)", 
                    names(table(munidata$Municipal)), 
                    multiple=TRUE),
        checkboxInput("meanMA", "Compare to MA Average", FALSE),
        checkboxInput("meanUS", "Compare to US Average", FALSE))),
    
    ## in plot or summary choose county
    conditionalPanel(
      condition="input.tabs == 'plot' || input.tabs == 'summary'",
      conditionalPanel(
        condition="input.type == 'county'",
        selectInput("county", "Select County (max of 7)", 
                    names(table(countydata$County)), 
                    multiple=TRUE),
        checkboxInput("meanMA", "Compare to MA Average", FALSE),
        checkboxInput("meanUS", "Compare to US Average", FALSE))),
    
    tags$hr(),
    
    helpText("Note: Year was chosen for the last year of a five-year moving average."),
    
    tags$hr(),
    
    
    ## author line
    helpText("Created by Sophie E O'Brien and Stephen A Lauer"),
    
    ## email feedback link
    helpText(a("Send us your comments or feedback!", href="mailto:seigmateam@gmail.com", target="_blank")),
    
    ## data source citation
    helpText(a("U.S. Census Bureau, 2006-2012 American Community Survey", href="http://factfinder2.census.gov/faces/nav/jsf/pages/index.xhtml", target="_blank")),
    
    ## GitHub link
    helpText(a("View our data and code on GitHub", 
               href="https://github.com/sEigmA/SEIGMA/tree/gh-pages/suicide", target="_blank"))
    
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Summary", 
               helpText("Note: search is case sensitive.", align="right"),
               dataTableOutput("summary"), value="summary", 
               tags$style(type="text/css", '#summary tfoot {display:none;}')),
      tabPanel("Plot", plotOutput("plot"), value="plot"),
      tabPanel("Map", plotOutput("map"), value="map"),
      id="tabs"
    )  
  )
))