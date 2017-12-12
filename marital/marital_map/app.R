################
# MAP TEMPLATE #
################

##### GLOBAL #####
## load necessary libraries
require(dplyr)
require(sp)
require(maptools)
require(rgeos)
require(Hmisc)
require(reshape2)
require(shiny)
require(googleCharts)
require(leaflet)
require(RJSONIO)
require(tidyr)
require(plotly)

## Load formatted marital status data
map_data <- read.csv(file="BA002_02_marriagedata.csv")
names(map_data)[10:12] <- gsub("Now_", "", names(map_data)[10:12])

## load map data
MA_map_muni <- fromJSON("Muni_2010Census_DP1.geojson")
## Find order of municipals in geojson files
## Each municipal is a separate feature
for(i in 1:length(MA_map_muni$features)){
  MA_map_muni$features[[i]]$properties$NAMELSAD10 <- gsub(MA_map_muni$features[[i]]$properties$NAMELSAD10, pattern=c(" [Tt]own| [Cc]ity"), replacement = "")
}

MA_municipals_map <- c()
for(i in 1:length(MA_map_muni$features)){
  MA_municipals_map <- c(MA_municipals_map, MA_map_muni$features[[i]]$properties$NAMELSAD10)
}

idx_leftovers <- which(!MA_municipals_map %in% map_data$Municipal)
leftover_munis <- MA_municipals_map[idx_leftovers]

MA_municipals <- c()
for(i in 1:length(MA_map_muni$features)){
  MA_municipals <- c(MA_municipals, MA_map_muni$features[[i]]$properties$NAMELSAD10)
}
idx_leftovers2 <- which(!MA_municipals %in% map_data$Municipal)
leftover_munis_map <- MA_municipals[idx_leftovers2]
leftover_munis_map <- leftover_munis_map[leftover_munis_map=="County subdivisions not defined"]
MA_municipals <- sort(MA_municipals[-which(MA_municipals=="County subdivisions not defined")])

MA_municipals<-unique(map_data$Municipal[-c(grep(map_data$Municipal, pattern = "County"),which(map_data$Municipal %in% c("MA", "USA")))])

## Set graph colors (special for colorblind people)
## In order: black, orange, light blue, green, yellow, dark blue, red, pink
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                "#0072B2", "#D55E00", "#CC79A7", "#CC79A7", "#CC79A7")

## Colors for a single-year legend
paint_brush <- colorRampPalette(colors=c("white", "#009E73"))
map_colors <- c(paint_brush(n=25), "#999999")

## For a single year data, we have a series of percentages (split into quintiles).  Cuts are quintiles of the total data percentages
## Cuts based on entire dataset - not year specific - This keeps colors consistent for maps year-to-year

max_val <- 100
min_val <- 0

## Puts each county year in between the cuts (n colors, n+1 cuts)
## length.out will make that many cuts
cuts <- seq(min_val, max_val, length.out = length(map_colors))

## Puts each county year in between the cuts (n colors, n+1 cuts)
## length.out will make that many cuts
cuts <- seq(min_val, max_val, length.out = length(map_colors))

## Generate map button
gen_map_button <- HTML('<style type="text/css">
                       .action-button {
                       -moz-box-shadow:inset 0px 1px 0px 0px #54a3f7;
                       -webkit-box-shadow:inset 0px 1px 0px 0px #54a3f7;
                       box-shadow:inset 0px 1px 0px 0px #54a3f7;
                       background:-webkit-gradient(linear, left top, left bottom, color-stop(0.05, #007dc1), color-stop(1, #0061a7));
                       background:-moz-linear-gradient(top, #007dc1 5%, #0061a7 100%);
                       background:-webkit-linear-gradient(top, #007dc1 5%, #0061a7 100%);
                       background:-o-linear-gradient(top, #007dc1 5%, #0061a7 100%);
                       background:-ms-linear-gradient(top, #007dc1 5%, #0061a7 100%);
                       background:linear-gradient(to bottom, #007dc1 5%, #0061a7 100%);
                       filter:progid:DXImageTransform.Microsoft.gradient(startColorstr="#007dc1", endColorstr="#0061a7",GradientType=0);
                       background-color:#007dc1;
                       -moz-border-radius:3px;
                       -webkit-border-radius:3px;
                       border-radius:3px;
                       border:1px solid #124d77;
                       display:inline-block;
                       cursor:pointer;
                       color:#ffffff;
                       font-family:arial;
                       font-size:16px;
                       padding:12px 36px;
                       text-decoration:none;
                       text-shadow:0px 1px 0px #154682;
                       }
                       .action-button:hover {
                       background:-webkit-gradient(linear, left top, left bottom, color-stop(0.05, #0061a7), color-stop(1, #007dc1));
                       background:-moz-linear-gradient(top, #0061a7 5%, #007dc1 100%);
                       background:-webkit-linear-gradient(top, #0061a7 5%, #007dc1 100%);
                       background:-o-linear-gradient(top, #0061a7 5%, #007dc1 100%);
                       background:-ms-linear-gradient(top, #0061a7 5%, #007dc1 100%);
                       background:linear-gradient(to bottom, #0061a7 5%, #007dc1 100%);
                       filter:progid:DXImageTransform.Microsoft.gradient(startColorstr="#0061a7", endColorstr="#007dc1",GradientType=0);
                       background-color:#0061a7;
                       }
                       .action-button:active {
                       position:relative;
                       top:1px;
                       }
                       
                       </style>')

map_side_text <- conditionalPanel(
  condition="input.tabs == 'map'",
  h4("How to use this app:"),
  helpText(p(strong("Please select a variable of interest and five- year range, and click on 'Generate Map' to get started."))),
  tags$br(),
  tags$ul(
    
    tags$li('Clicking on a municipality will display the marriage status for the five-year range that you selected.')
  ))

## Add map casino icons
MAcasinos <- data.frame("Name"=c("Wynn Boston Harbor", "Plainridge Park Casino", "MGM Springfield"),
                        "Lat"=c(42.394964,42.0330381,42.1006063),
                        "Lon"=c(-71.066760,-71.3039442,-72.5870506))

##############
##### UI #####
##############

ui <- shinyUI(fluidPage(
  ## embed the google analytics script in the app
  tags$head(includeScript("google-analytics.js")),
  ## HTML to create generate map button
  gen_map_button,
  
  ## blank title, but put in a special title for window tab
  titlePanel("", windowTitle = "SEIGMA MAP TAMPLATE"),
  
  ## Create sidebar
  sidebarLayout(
    sidebarPanel(width=4,
                 map_side_text,
                 selectInput("map_year", "Select Five Year Range",
                             choices = list("2006-2010" = "2006-2010", 
                                            "2007-2011" = "2007-2011", 
                                            "2008-2012" = "2008-2012",
                                            "2009-2013" = "2009-2013", 
                                            "2010-2014" = "2010-2014", 
                                            "2011-2015" = "2011-2015")),
                 selectInput("map_gender", "Select Gender",
                             choices = list("Female", "Male")),
                 selectInput("var", "Select Variable of Interest",
                             choices = list("Never Married" = "Never_Married_pct",
                                            "Married" = "Married_pct",
                                            "Separated" = "Separated_pct",
                                            "Widowed" = "Widowed_pct",
                                            "Divorced" = "Divorced_pct")),
                 # checkboxInput("map_cas", "Display Casinos", value=FALSE),
                 # actionButton("action2", "REDRAW MAP"),
                 tags$hr(),
                 
                 ## author line
                 helpText("Created by Zhenning Kang"),
                 
                 ## email feedback link
                 ## To develop a link in HTML
                 helpText(a("Send us your comments or feedback!", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank")),
                 ## Data source citation
                 helpText(a("Data Source: American Community Survey - Table DP02", href="http://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_14_5YR_S1201&prodType=table",
                            target="_blank")),
                 
                 ## GitHub link
                 helpText(a("View our data and code on GitHub", 
                            href="https://github.com/sEigmA/SEIGMA/tree/gh-pages/marital/marital", target="_blank")),
                 
                 helpText("If using Internet Explorer, application only visible in version 10.")
    ),
    ######### End of Sidebar  #########
    
    ######### Start of Main Panel #####
    mainPanel(
      ## put in logo for title
      a(img(src = "logo.jpg", height=105, width=920), href="http://www.umass.edu/seigma/"),                 tags$head(tags$style("
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
        absolutePanel(left=100, top=450, width=300, class="floater",
                      htmlOutput("details"))),
      
      conditionalPanel(
        condition="input.action == 0",
        ## within the map area, you can create an action button.  similar to initializing the legend but just putting a button instead.
        absolutePanel(right = 400, top = 300, class = "floater",
                      actionButton("action", "Generate Map")
        )),
      
      ## Legend                 
      # Never Married
      conditionalPanel(
        condition="input.var == 'Never_Married_pct' && input.action != 0",
        absolutePanel(
          right = 10, top = 100, draggable=FALSE, style = "", 
          class = "floater",
          strong("Never Married"),
          plotOutput("legend1"),
          tags$table(
            tags$tr(
              tags$td(tags$div(
                style = sprintf("width: 16px; height: 16px; background-color: %s;", "#999999")
              )),
              tags$td("Data not", br(), "available", align = "right")
            )
          )
        )),
      # Married
      conditionalPanel(
        condition="input.var == 'Married_pct' && input.action != 0",
        absolutePanel(
          right = 10, top = 100, draggable=FALSE, style = "", 
          class = "floater",
          strong("Married"),
          plotOutput("legend2"),
          tags$table(
            tags$tr(
              tags$td(tags$div(
                style = sprintf("width: 16px; height: 16px; background-color: %s;", "#999999")
              )),
              tags$td("Data not", br(), "available", align = "right")
            )
          )
        )),
      # Separated
      conditionalPanel(
        condition="input.var == 'Separated_pct' && input.action != 0",
        absolutePanel(
          right = 10, top = 100, draggable=FALSE, style = "", 
          class = "floater",
          strong("Separated"),
          plotOutput("legend3"),
          tags$table(
            tags$tr(
              tags$td(tags$div(
                style = sprintf("width: 16px; height: 16px; background-color: %s;", "#999999")
              )),
              tags$td("Data not", br(), "available", align = "right")
            )
          )
        )),
      # Widowed
      conditionalPanel(
        condition="input.var == 'Widowed_pct' && input.action != 0",
        absolutePanel(
          right = 10, top = 100, draggable=FALSE, style = "", 
          class = "floater",
          strong("Widowed"),
          plotOutput("legend4"),
          tags$table(
            tags$tr(
              tags$td(tags$div(
                style = sprintf("width: 16px; height: 16px; background-color: %s;", "#999999")
              )),
              tags$td("Data not", br(), "available", align = "right")
            )
          )
        )),
      # Divorced
      conditionalPanel(
        condition="input.var == 'Divorced_pct' && input.action != 0",
        absolutePanel(
          right = 10, top = 100, draggable=FALSE, style = "", 
          class = "floater",
          strong("Divorced"),
          plotOutput("legend5"),
          tags$table(
            tags$tr(
              tags$td(tags$div(
                style = sprintf("width: 16px; height: 16px; background-color: %s;", "#999999")
              )),
              tags$td("Data not", br(), "available", align = "right")
            )
          )
        ))
                 ))
      ))

##################
##### SERVER #####
##################
server <- shinyServer(function(input, output, session) {
  map_df <- reactive({
    ## Filter the data by the chosen Five Year Range 
    map_df <- map_data %>%
      arrange(Region)
    ## get column name and cuts based on input
    if (input$map_gender == "Female") {
      map_df <- filter(map_df, Gender == "Female")
    } else {
      map_df <- filter(map_df, Gender == "Male")
    }
    ## Output reactive dataframe
    map_df    
  })
  
  map_dat <- reactive({
    ## make reactive dataframe into regular dataframe
    map_df <- map_df()
    
    ## take US, MA, and counties out of map_dat
    map_dat <- map_df %>%
      filter(Five_Year_Range == input$map_year)%>%
      filter(!is.na(Region))
    
    ## assign colors to each entry in the data frame
    color <- as.integer(cut2(map_dat[,input$var],cuts=cuts))
    map_dat <- cbind.data.frame(map_dat, color)
    map_dat$color <- ifelse(is.na(map_dat$color), length(map_colors), 
                            map_dat$color)
    map_dat$opacity <- 0.7
    ## find missing counties in data subset and assign NAs to all values
    missing_munis <- setdiff(leftover_munis_map, map_dat$Region)
    missing_df <- data.frame(Municipal = NA, County = NA, State = NA, Region = missing_munis,
                             Five_Year_Range = input$map_year, Population = NA, Never_Married = NA,
                             Never_Married_pct = NA, Never_Married_pct_error = NA, Married = NA, 
                             Married_pct = NA, Married_pct_error = NA, Separated = NA,
                             Separated_pct = NA, Separated_pct_error = NA, Widowed = NA, 
                             Widowed_pct = NA, Widowed_pct_error = NA, Divorced = NA, 
                             Divorced_pct = NA, Divorced_pct_error = NA, Gender = NA, 
                             color=length(map_colors), 
                             opacity = 0)
    
    # combine data subset with missing counties data
    map_dat <- rbind.data.frame(map_dat, missing_df)
    map_dat$color <- map_colors[map_dat$color]
    return(map_dat)
  })
  
  values <- reactiveValues(selectedFeature=NULL, highlight=c())
  
  
  ##
  cas_dat <- reactive({
    cas_dat <- MAcasinos
    cas_dat
    
  })

    ## draw leaflet map
  map <- createLeafletMap(session, "map")
  
  ## the functions within observe are called when any of the inputs are called
  
  ## Does nothing until called (done with action button)
  observe({
    input$action
    
    ## load in relevant map data
    map_dat <- map_dat()
    
    ## All functions which are isolated, will not run until the above observe function is activated
    isolate({
      ## Duplicate MAmap to x
      x <- MA_map_muni
      
      ## for each county in the map, attach the Crude Rate and colors associated
      for(i in 1:length(x$features)){
        ## Each feature is a county
        x$features[[i]]$properties[input$var] <- 
          map_dat[match(x$features[[i]]$properties$NAMELSAD10, map_dat$Region), input$var]
        ## Style properties
        x$features[[i]]$properties$style <- list(
          fill=TRUE, 
          ## Fill color has to be equal to the map_dat color and is matched by county
          fillColor = map_dat$color[match(x$features[[i]]$properties$NAMELSAD10, map_dat$Region)], 
          ## "#000000" = Black, "#999999"=Grey, 
          weight=1, stroke=TRUE, 
          opacity=map_dat$opacity[match(x$features[[i]]$properties$NAMELSAD10, map_dat$Region)], 
          color="#000000", 
          fillOpacity=map_dat$opacity[match(x$features[[i]]$properties$NAMELSAD10, map_dat$Region)])
      }
      
      map$addGeoJSON(x) # draw map
      
    })
  })
  
  observe({
    ## EVT = Mouse Click
    evt <- input$map_click
    if(is.null(evt))
      return()
    
    isolate({
      values$selectedFeature <- NULL
    })
  })
  
  observe({
    evt <- input$map_geojson_click
    if(is.null(evt))
      return()
    input$map_year
    map_dat <- map_dat()
    isolate({
      values$selectedFeature <- evt$properties
      region <- evt$properties$NAMELSAD10
      values$selectedFeature[input$var] <- map_dat[match(region, map_dat$Region), input$var]
    })
  })
  ##  This function is what creates info box
  output$details <- renderText({
    
    ## Before a county is clicked, display a message
    if(is.null(values$selectedFeature)){
      return(as.character(tags$div(
        tags$div(
          h4("Click on a town or city"))
      )))
    }
    
    muni_name <- values$selectedFeature$NAMELSAD10
    muni_value <- values$selectedFeature[input$var]
    var_select <- gsub("_", " ", input$var)
    var_select <- gsub("pct", "", var_select)
    
    ## If clicked county has no crude rate, display a message
    if(is.null(values$selectedFeature[input$var])){
      return(as.character(tags$div(
        tags$h5(var_select, " % in ", muni_name, "is not available for this timespan"))))
    }
    ## For a single year when county is clicked, display a message
    as.character(tags$div(
      tags$h4(var_select, "% in ", muni_name, " for ", input$map_year),
      tags$h5(muni_value, "%")
    ))
  })
  
  ## Legend
  output$legend1 <- renderPlot({  
    paint.brush = colorRampPalette(colors=c("white", "darkblue"))
    cols <- paint.brush(101)
    leg_dat <- data_frame(y = seq(0, 100), x = 1, col = cols)
    
    b <- ggplot(data = leg_dat) +
      geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
      scale_fill_manual(values = leg_dat$col) + theme_bw() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.x = element_blank(),
            panel.border = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank())
    return(b)
    
  })
  
  output$legend2 <- renderPlot({  
    paint.brush = colorRampPalette(colors=c("white", "darkblue"))
    cols <- paint.brush(101)
    leg_dat <- data_frame(y = seq(0, 100), x = 1, col = cols)
    
    b <- ggplot(data = leg_dat) +
      geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
      scale_fill_manual(values = leg_dat$col) + theme_bw() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.x = element_blank(),
            panel.border = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank())
    return(b)
    
  })
  
  output$legend3 <- renderPlot({  
    paint.brush = colorRampPalette(colors=c("white", "darkblue"))
    cols <- paint.brush(101)
    leg_dat <- data_frame(y = seq(0, 100), x = 1, col = cols)
    
    b <- ggplot(data = leg_dat) +
      geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
      scale_fill_manual(values = leg_dat$col) + theme_bw() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.x = element_blank(),
            panel.border = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank())
    return(b)
    
  })
  
  output$legend4 <- renderPlot({  
    paint.brush = colorRampPalette(colors=c("white", "darkblue"))
    cols <- paint.brush(101)
    leg_dat <- data_frame(y = seq(0, 100), x = 1, col = cols)
    
    b <- ggplot(data = leg_dat) +
      geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
      scale_fill_manual(values = leg_dat$col) + theme_bw() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.x = element_blank(),
            panel.border = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank())
    return(b)
    
  })
  
  output$legend5 <- renderPlot({  
    paint.brush = colorRampPalette(colors=c("white", "darkblue"))
    cols <- paint.brush(101)
    leg_dat <- data_frame(y = seq(0, 100), x = 1, col = cols)
    
    b <- ggplot(data = leg_dat) +
      geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
      scale_fill_manual(values = leg_dat$col) + theme_bw() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.x = element_blank(),
            panel.border = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank())
    return(b)
    
  })

})

shinyApp(ui=ui, server=server)
