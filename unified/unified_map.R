# unified_map APP 

### SETTINGS ###
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

# Load formatted data, -1 eliminates first column [rows,columns]
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

# store municipal coordinates into one data frame
muni_num <- length(MA_map_muni$features)
MA_muni_lat <- MA_muni_lng <- rep(NA, muni_num)
for(i in 1:muni_num){
    lat <- lng <- c()
    for(c in 1:length(MA_map_muni$features[[i]]$geometry$coordinates[[1]][[1]])){
        lat <- c(lat, MA_map_muni$features[[i]]$geometry$coordinates[[1]][[1]][[c]][1])
        lng <- c(lng, MA_map_muni$features[[i]]$geometry$coordinates[[1]][[1]][[c]][2])
        MA_muni_lat[i] <- median(lat)
        MA_muni_lng[i] <- median(lng)
    }
}
coor_data=data.frame(x=MA_muni_lat, y=MA_muni_lng, id=MA_municipals_map)
coor_data <- subset(coor_data, coor_data$id != "County subdivisions not de")

### USER INTERFACE ###
ui <- fluidPage(
    
    # blank title, but put in a special title for window tab
    titlePanel("", windowTitle = "SEIGMA: Unified Shiny App"),
    
    # Create sidebar
    sidebarLayout(
        sidebarPanel(
            helpText("Click a Municipal of Interest:"),
            leafletOutput("map", height="600px")
            ),
        # mainpanel
        mainPanel(
            fluidRow(
                ## put in logo for title
                a(img(src = "logo.jpg", height=105, width=920), href="http://www.umass.edu/seigma/")
                ),
            fluidRow(
                column(width = 6,
                       plotOutput("plot_inc", height="300px")
                ),
                column(width = 6,
                       plotOutput("plot_rent", height="300px")
                )
            ),
            fluidRow(
                column(width = 6,
                       plotOutput("plot_une", height="300px")
                ),
                column(width = 6,
                       plotOutput("plot_pov", height="300px")
                )
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
        leaflet() %>% 
            setView(lng = -71.4, lat = 42.4,  zoom = 8) %>%
            addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
            addCircleMarkers(data=coor_data, ~x , ~y, layerId=~id, popup=~id, radius=10 , color="red",  fillColor="orange", stroke = TRUE, fillOpacity = 0.8)
    })
    
    # store the click
    observeEvent(input$map_marker_click,{
        data_of_click$clickedMarker <- input$map_marker_click
    })
    
    # income plot
    output$plot_inc=renderPlot({
        my_place=data_of_click$clickedMarker$id
        if(is.null(my_place)){my_place="MA"}
        
        muni_df <- subset(inc_data, inc_data$Region==my_place)
        
        ggplot(muni_df, aes(x=(Five_Year_Average-2), y = Median_Annual_Household_Income)) + 
            geom_line() +
            ggtitle(paste0("Median Annual Household Income in ",my_place)) + 
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
            ggtitle(paste0("Inflation-Adjusted (2015) Median Annual Rent in ",my_place)) + 
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
            ggtitle(paste0("Poverty Rates in ",my_place)) + 
            xlab("Average Data Point of Five Year Period") + 
            ylab("Percentile") + 
            theme(plot.title = element_text(size=14, face="bold"))
    })
}

shinyApp(ui = ui, server = server)

    