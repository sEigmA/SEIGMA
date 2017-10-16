#############################
## Title: Unified Map APP  ##
## Author: Zhenning Kang   ##
## Date Created: 09/25/17  ##
## Last Modified: 10/15/17 ##
#############################

### SETTINGS ###
setwd("C:/Users/Zhenning Kang/Documents/UMass/SEIGMA/unified/unified_map")
library(shiny)
library(dplyr)
library(sp)
library(maptools)
library(rgeos)
library(Hmisc)
library(reshape2)
library(leaflet)
library(RJSONIO)
library(tidyr)
library(ggplot2)
library(ggmap)

# Load formatted data, -1 eliminates first column [rows,columns]
edu_data <- read.csv(file="edudata.csv")[,-1]
edu_data$Year <- as.factor(as.numeric(substr(edu_data$Five_Year_Range, 1, 4))+2)

inc_data <- read.csv(file="incomedata.csv")[,-1]
rent <- read.csv(file="rent.csv")
unemp_data <- read.csv(file="unempdata2.csv")
labor <- read.csv(file="povratedata.csv")[,-1]

# load mumicipal data from 2010 Cencus
MA_map_muni <- fromJSON("Muni_2010Census_DP1.geojson")

# Extract municipal names and stored in MA_municipals_map
for(i in 1:length(MA_map_muni$features)){
    MA_map_muni$features[[i]]$properties$NAMELSAD10 <- substr(MA_map_muni$features[[i]]$properties$NAMELSAD10, 1, nchar(MA_map_muni$features[[i]]$properties$NAMELSAD10)-5)
}
MA_municipals_map <- c()
for(i in 1:length(MA_map_muni$features)){
    MA_municipals_map <- c(MA_municipals_map, MA_map_muni$features[[i]]$properties$NAMELSAD10)
}

# Find order of municipals in geojson files
idx_leftovers <- which(!MA_municipals_map %in% inc_data$Region)
leftover_munis <- MA_municipals_map[idx_leftovers]
for(i in 1:length(leftover_munis)){
    MA_map_muni$features[[idx_leftovers[i]]]$properties$NAMELSAD10 <- 
        substr(leftover_munis[i], 1, nchar(leftover_munis[i])-5)
}
MA_municipals <- c()
for(i in 1:length(MA_map_muni$features)){
    MA_municipals <- c(MA_municipals, MA_map_muni$features[[i]]$properties$NAMELSAD10)
}
idx_leftovers2 <- which(!MA_municipals %in% inc_data$Region)
leftover_munis_map <- MA_municipals[idx_leftovers2]
MA_municipals <- sort(MA_municipals[-idx_leftovers2])

### Only Run Once, data saved in csv
# # get city name lat/lng from google map
# MA_muni_name <- paste(MA_municipals_map, "ma usa", sep=" ")
# muni_gg <- geocode(MA_muni_name)
# coor_data=data.frame(x = muni_gg$lon, y=muni_gg$lat, id=MA_municipals_map)
# coor_data <- subset(coor_data, coor_data$id != "County subdivisions not de")
# coor_data[201,1:2] <- geocode("sunderland ma usa")
# sum(is.na(coor_data))
# save(coor_data, file = "muni_coor.csv")

load("muni_coor.csv")

### USER INTERFACE ###
ui <- fluidPage(
    
    # blank title, but put in a special title for window tab
    titlePanel("", windowTitle = "SEIGMA Unified Shiny App"),
    
    # Create sidebar
    sidebarLayout(
        sidebarPanel(
            helpText("Please click a Municipal of Interest:"),
            leafletOutput("map", height="600px"),
            ## email feedback link
            ## To develop a link in HTML
            helpText(a("Send us your comments or feedback!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank",onclick="ga('send', 'event', 'click', 'link', 'feedback', 1)")),
            ## data source citation
            helpText(a("Data Source: American Community Survey: table DP05", href="http://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_14_5YR_S2502&prodType=table",
                       target="_blank",onclick="ga('send', 'event', 'click', 'link', 'dataAge', 1)")),
            ## GitHub link
            helpText(a("View our data and code on GitHub",
                       href="https://github.com/sEigmA/SEIGMA/tree/gh-pages/unified", target="_blank",onclick="ga('send', 'event', 'click', 'link', 'code', 1)")),
            ## author line
            helpText("Created by Zhenning Kang")
        ),
        # mainpanel
        mainPanel(
          ## put in logo for title
          a(img(src = "logo.jpg", height=105, width=920), href="http://www.umass.edu/seigma/"),
          
          ## create tabs
          tabsetPanel(
            tabPanel("Demographics",
                     fluidRow(
                       column(width = 6,
                               plotOutput("plot_inc", height="300px")
                              ),
                       column(width = 6,
                               plotOutput("plot_rent", height="300px")
                              )),
                     fluidRow(
                       column(width = 6,
                               plotOutput("plot_une", height="300px")
                              ),
                       column(width = 6,
                               plotOutput("plot_pov", height="300px")
                              ))),
            tabPanel("Social",
                     fluidRow(
                       column(width = 6,
                              plotOutput("plot_inc", height="300px")
                              ),
                       column(width = 6,
                              plotOutput("plot_rent", height="300px")
                              )),
                     fluidRow(
                       column(width = 6,
                              plotOutput("plot_une", height="300px")
                              ),
                       column(width = 6,
                              plotOutput("plot_pov", height="300px")
                              )))
            )
          )
        )
    )


### SERVER ###
server <- function(input, output) {
    # create a reactive value that will store the click position
    data_of_click <- reactiveValues(clickedMarker=NULL)
    
    # Leaflet map with markers
    output$map <- renderLeaflet({
        leaflet(data=coor_data) %>% 
            setView(lng = -71.4, lat = 42.4,  zoom = 7) %>%
            addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
            addCircleMarkers(~x , ~y, layerId=~id, popup=, radius=1 , color="white",  fillColor="white", stroke = TRUE, fillOpacity = 0) %>% 
            addMarkers(~x , ~y, popup = ~id, label = ~id)
    })
    
    # store the click
    observeEvent(input$map_marker_click,{
        data_of_click$clickedMarker <- input$map_marker_click
    })
    
    # set default plot with MA and USA
    my_place <- data_of_click$clickedMarker$id
    if(is.null(my_place)){my_place <- c("MA", "United States")}
    
    # education plot
    muni_df <- subset(edu_data, edu_data$Region %in% my_place)

    
    # income plot
    output$plot_inc=renderPlot({
      
      muni_df <- subset(inc_data, inc_data$Region==my_place)
      theme_set(theme_classic())
      ggplot(muni_df, aes(x=(Five_Year_Average-2), y = Median_Annual_Household_Income)) + 
            geom_line() +
            ggtitle(paste0("Median Annual Household Income in ", my_place)) + 
            xlab("Average Data Point of Five Year Period") + 
            ylab("Dollar") + 
            theme(plot.title = element_text(size=14, face="bold"))
    })
    
    # rent plot
    output$plot_rent=renderPlot({
        my_place=data_of_click$clickedMarker$id
        if(is.null(my_place)){my_place="MA"}
        
        muni_df <- subset(rent, labor$Region==my_place)
        muni_df$Year <- as.numeric(substr(muni_df$Five.Year.Range, start = 1, stop = 4))+2
        
        ggplot(muni_df, aes(x=(Year), y = Median.Rent.2015.Dollar)) + 
            geom_line() +
            ggtitle(paste0("Inflation-Adjusted (2015) Annual Median Rent in ",my_place)) + 
            xlab("Average Data Point of Five Year Period") + 
            ylab("Dollar") + 
            theme(plot.title = element_text(size=14, face="bold"))
    })
    
    # unemployment plot
    output$plot_une=renderPlot({
        my_place=data_of_click$clickedMarker$id
        if(is.null(my_place)){my_place="MA"}
        
        muni_df <- subset(unemp_data, unemp_data$Region==my_place)
        
        ggplot(muni_df, aes(x=Year, y = Unemployment_Rate_Avg)) + 
            geom_line() +
            ggtitle(paste0("Annual Average Unemployment Rate in ",my_place)) + 
            xlab("Average Data Point of Five Year Period") + 
            ylab("Percentile") + 
            theme(plot.title = element_text(size=14, face="bold"))
    })
    
    # poverty plot
    output$plot_pov=renderPlot({
        my_place=data_of_click$clickedMarker$id
        if(is.null(my_place)){my_place="MA"}
        
        muni_df <- subset(labor, labor$Region==my_place)
        muni_df$Year <- as.numeric(substr(muni_df$Five_Year_Range, start = 1, stop = 4))+2
        
        ggplot(muni_df, aes(x=(Year), y = Percent_Pov)) + 
            geom_line() +
            ggtitle(paste0("Annual Poverty Rates in ",my_place)) + 
            xlab("Average Data Point of Five Year Period") + 
            ylab("Percentile") + 
            theme(plot.title = element_text(size=14, face="bold"))
    })
}

shinyApp(ui = ui, server = server)

    