#############################
## Unified Municipal App   ##
## Author: Zhenning Kang   ##
## Date Created: 10/19/17  ##
## Last Modified: 10/29/17 ##
#############################

### SETTINGS ###
library(shiny)
library(dplyr)
library(reshape2)
library(ggplot2)
library(plotly)

### DATA ###
### DEMOGRAPHIC TAB
dem_data <- read.csv(file="data/demodata.csv")
dem_data$Year <- as.factor(as.numeric(substr(dem_data$Five_Year_Range, 1, 4))+2)

### SOCIAL TAB
# data for education plot
edu_data <- read.csv(file="data/edudata.csv")[,-1]
edu_data$Year <- as.factor(as.numeric(substr(edu_data$Five_Year_Range, 1, 4))+2)

# data for vetaran plot
vet_data <- read.csv(file="data/vetstatusdata.csv")[,-1]
vet_data$Year <- as.factor(as.numeric(substr(vet_data$Five_Year_Range, 1, 4))+2)

# data for married status plot
mar_data <- read.csv(file="data/BA002_02_marriagedata.csv")
mar_data$Year <- as.factor(as.numeric(substr(mar_data$Five_Year_Range, 1, 4))+2)

# data for suicide plot
sui_data <- read.csv(file="data/SASuicidedata_Updated2017.csv")[,-1]
#If there is no age adjusted rate, get rid of the bounds and standard errors
sui_data$Age.Adjusted.Rate.Lower.Bound[is.na(sui_data$Age.Adjusted.Rate)] <- NA
sui_data$Age.Adjusted.Rate.Upper.Bound[is.na(sui_data$Age.Adjusted.Rate)] <- NA
sui_data$Age.Adjusted.Rate.Standard.Error[is.na(sui_data$Age.Adjusted.Rate)] <- NA
sui_data$County <- gsub("US", "United States", sui_data$County)

### REGIONS
MA_municipals <- as.character(na.omit(unique(dem_data$Municipal)))
muni_county <- data.frame(unique(na.omit(subset(dem_data, select = c("Municipal", "County")))))
# MA_map_muni <- geojson_read("Muni_2010Census_DP1.geojson", what = "sp")
# MA_municipals <- as.character(unique(MA_map_muni$NAMELSAD10))
# MA_municipals <- gsub(MA_municipals, pattern = " [Tt]own| city", replacement = "")
# MA_municipals <- sort(MA_municipals[-grep(MA_municipals, pattern = "County subdivisions not defined")])
# 
# MA_map_county <- fromJSON("County_2010Census_DP1.geojson")
# MA_counties <- c()
# for(i in 1:length(MA_map_county$features)){
#   MA_counties <- c(MA_counties, MA_map_county$features[[i]]$properties$County)
# }

##### USER INTERFACE #####
ui <- fluidPage(
  # blank title, but put in a special title for window tab
  titlePanel("", windowTitle = "SEIGMA Unified Shiny App"),
  fluidRow(
    column(3,
           h4("Please Select Municipalities"),
           selectInput("muni", "",
                       choices = MA_municipals, 
                       selected = "Abington",
                       multiple = T),
           checkboxInput("CT_mean", "Compare to County Average", FALSE),
           fluidRow(column(6,
                           checkboxInput("MA_mean", "MA Average", TRUE)
           ),
           column(6,           checkboxInput("US_mean", "US Average", TRUE)
))
           ),
    column(6,
           ## put logo on the top
           div(a(img(src = "SEIGMA-Logo-crop.jpg", height=101, width=600), href="http://www.umass.edu/seigma/"), style="text-align: center;")),
    column(3,
           helpText(a("Comments or Feedback", href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation", target="_blank",onclick="ga('send', 'event', 'click', 'link', 'feedback', 1)")),
           ## data source citation
           helpText(a("Data Source", href="https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml",
                      target="_blank",onclick="ga('send', 'event', 'click', 'link', 'dataAge', 1)")),
           ## GitHub link
           helpText(a("Codes on GitHub",
                      href="https://github.com/sEigmA/SEIGMA/tree/gh-pages/unified/unified_map", target="_blank",onclick="ga('send', 'event', 'click', 'link', 'code', 1)")),
           ## author line
           helpText("Created by Zhenning Kang")
           )),
  fluidRow(
      ## create tabs
      tabsetPanel(
        tabPanel("Demographics",
                 # app link
                 fluidRow(
                   column(4),
                   column(4,
                          h3(helpText(a("More information about Demographics.", href="https://seigma.shinyapps.io/demographics/", target="_blank",onclick="ga('send', 'event', 'click', 'link', 'dem_app', 1)")))
                          )
                   ),
                 fluidRow(
                            column(9,
                                   plotlyOutput("plot_age")
                            ),
                            column(3,
                                   h4("Age of Interest"),
                                   checkboxInput("under20", "Age Under 20 ", TRUE),                             checkboxInput("under34", "Age 20 to 34 ", FALSE),
                                   checkboxInput("under54", "Age 35 to 54 ", FALSE),
                                   checkboxInput("under64", "Age 55 to 64 ", FALSE),
                                   checkboxInput("under74", "Age 65 to 74 ", FALSE),
                                   checkboxInput("over75", "Age over 75 ", FALSE),
                                   br(),
                                   actionButton("age_info", "Information"),
                                   downloadButton(outputId = "age_down", label = "Download the plot")
                                   )),
                 br(),
                 fluidRow(
                   column(9,
                          plotlyOutput("plot_rac")
                   ),
                   column(3,
                          h4("Race of Interest"),
                          checkboxInput("white", "White", TRUE),
                          checkboxInput("black", "Black", FALSE),
                          checkboxInput("native", "American Indian and Alaska Native", FALSE),
                          checkboxInput("asian", "Asian", FALSE),
                          checkboxInput("hawaiian", "Hawaiian and Other Pacific Islander", FALSE),
                          checkboxInput("others", "Others", FALSE),
                          br(),
                          actionButton("rac_info", "Information"),
                          downloadButton(outputId = "rac_down", label = "Download the plot")
                   )),
                 br(),
                 fluidRow(
                   column(6,
                          plotlyOutput("plot_gen"),
                          actionButton("gen_info", "Information"),
                          downloadButton(outputId = "gen_down", label = "Download the plot")
                          ),
                   column(6,
                          plotlyOutput("plot_his"),
                          actionButton("his_info", "Information"),
                          downloadButton(outputId = "his_down", label = "Download the plot")
                          )
                   )
                 ),
        tabPanel("Social",
                 br(),
                 fluidRow(
                   column(6,
                          fluidRow(
                            column(9,
                                   plotlyOutput("plot_edu")
                                   ),
                            column(3,
                                   radioButtons("education", "Choose a Level:",
                                                c("High School" = "hs",
                                                  "Bachelor" = "bac",
                                                  "Graduate" = "grad"),
                                                inline=T),
                                   br(),
                                   actionButton("edu_info", "Information"),
                                   downloadButton(outputId = "edu_down", label = "Download the plot")
                                   )),
                          # app link
                          h4(helpText(a("More information about  Educational Attainment.", href="https://seigma.shinyapps.io/educational_attainment/", target="_blank",onclick="ga('send', 'event', 'click', 'link', 'edu_app', 1)")))
                          ),
                   column(6,
                          fluidRow(
                            column(9,
                                   plotlyOutput("plot_mar")
                                   ),
                            column(3,
                                   radioButtons("status", "Choose a Status:",
                                                c("Married" = "married",
                                                  "Separated" = "separated",
                                                  "Divorced" = "divorced",
                                                  "Widowed" = "widowed",
                                                  "Never" = "never"),
                                                inline=T),
                                   br(),
                                   actionButton("mar_info", "Information"),
                                   downloadButton(outputId = "mar_down", label = "Download the plot")
                                   )),
                          # app link
                          h4(helpText(a("More information about Marital Status.",
                                     href="https://seigma.shinyapps.io/marital/", target="_blank",onclick="ga('send', 'event', 'click', 'link', 'mar_app', 1)")))
                   )),
                 br(),
                 fluidRow(
                   column(6,
                          plotlyOutput("plot_sui"),
                          # app link                                   
                          actionButton("sui_info", "Information"),
                          downloadButton(outputId = "sui_down", label = "Download the plot"),
                          br(),
                          h4(helpText(a("More information about Suicide Rate.", href="https://seigma.shinyapps.io/suicide/", target="_blank",onclick="ga('send', 'event', 'click', 'link', 'sui_app', 1)")))
                   ),
                   column(6,
                          plotlyOutput("plot_vet"),
                          # app link
                          actionButton("vet_info", "Information"),
                          downloadButton(outputId = "vet_down", label = "Download the plot"),
                          br(),
                          h4(helpText(a("More information about Veteran’s Status.", href="https://seigma.shinyapps.io/va_status/", target="_blank",onclick="ga('send', 'event', 'click', 'link', 'vet_app', 1)")))
                   ))
                 )
      )
    )
  )


##### SERVER #####
server <- function(input, output){
  
  observeEvent(input$age_info, {
    showNotification("AGE",
                     "The number of people within each age group, for a region over a specified five year range. Age groups were specified as <5, 5-9, 10-14, 15-19, 20-24, 25-34, 35-44, 45-54, 55-59, 60-64, 65-74, 75-84, and 85+. Within the Plot, the number of categories for age has been collapsed to the following six groups; <20, 20-34, 35-54, 55-64, 65-74,75+. This is done in order to simplify the presentation of data."
    )
  })
  
  observeEvent(input$rac_info, {
    showNotification("RACE",
                     "The number of people within each race, for a region over a specified five year range. Races were listed as White, Black or African American, Asian, American Indian or Alaska Native, Native Hawaiian or Other Pacific Islander, or some other race. "
    )
  })
  
  observeEvent(input$gen_info, {
    showNotification("GENDER",
                     "The number of people within each gender, for a region over a specified five year range.")
  })
  
  observeEvent(input$his_info, {
    showNotification("ETHNICITY",
                     "The number of people within each ethnicity, for a region over a specified five year range. Ethnicities were listed as hispanic or not hispanic.")
  })
  
  observeEvent(input$edu_info, {
    showNotification("Educational Attainment Rates ",
                     "The number of people with each level of educational attainment for a specific region over a specific five-year period of time. All inidviduals represented in this measure were at least 25 years of age. Respondents were classified according to highest level of school completed. When a municipaility is missing data, this indicates that data cannot be displayed because the number of people is too small.")
  })
  
  observeEvent(input$mar_info, {
    showNotification("Marital Status Rates",
                     "The number of people within each marital status category for a region over a specified five year range. When the number of people in a particular marital status category is too small, data cannot be displayed.")
  })
  
  observeEvent(input$sui_info, {
    showNotification("Age-adjusted Suicide Rate",
                     "Age-adjusted suicide rates are expressed as the number of suicides, per 100,000 persons, reported each calendar year for the region you select. Rates are considered 'unreliable' when the death count is less than 20 and thus are not displayed. This is calculated by: Age-adjusted Suicide Rate = Count / Population * 100,000")
  })
  
  observeEvent(input$vet_info, {
    showNotification("Veteran Status",
                     "People with active duty military service and or service in the military Reserves or National Guard. All individuals were at least 18 years of age.")
  })
  
  age_df <- reactive({
    
    county <- as.character(muni_county[muni_county$Municipal == input$muni,]$County)
    
    if(is.null(input$muni))
      my_place <- c("MA", "United States")
    if(!is.null(input$muni)){
      if(input$US_mean){
        if(input$MA_mean){
          if(input$CT_mean){
            my_place <- c("United States", "MA", input$muni, county) 
          } else{
            my_place <- c("United States", "MA", input$muni)
          }
        } else{
          if(input$CT_mean){
            my_place <- c("United States", input$muni, county)
          }
          my_place <- c("United States", input$muni)
        }
      } else{
        if(input$MA_mean){
          if(input$CT_mean){
            my_place <- c("MA", input$muni, county)
          } else{
            my_place <- c("MA", input$muni)
          }
        } else{
          if(input$CT_mean){
            my_place <- c(input$muni, county)
          } else{
            my_place <- c(input$muni)
          }
        }
      }
    }
    
    muni_df <- filter(dem_data, Region %in% my_place) %>% select(Region, Age_under_20_Pct_plot, Age_20_34_Pct_plot, Age_35_54_Pct_plot, Age_55_64_Pct_plot, Age_65_74_Pct_plot, Age_over_75_Pct_plot, Year)
    muni_df <- melt(muni_df)
    muni_df$variable <- gsub("0_3", "0 to 3", muni_df$variable)
    muni_df$variable[1:70] <- gsub("5_", "5 to ", muni_df$variable[1:70])
    muni_df$variable <- gsub("_", " ", muni_df$variable)
    muni_df$variable <- gsub("Pct plot", "", muni_df$variable)
    muni_df$Year <- gsub("20", "'", muni_df$Year)
    muni_df
  })
  
  output$plot_age <- renderPlotly({
    dat <- age_df()
    
    age_var <- unique(dat$variable)
    age <- c()
    if(input$under20)
      age <- append(age, age_var[1])
    if(input$under34)
      age <- append(age, age_var[2])
    if(input$under54)
      age <- append(age, age_var[3])
    if(input$under64)
      age <- append(age, age_var[4])
    if(input$under74)
      age <- append(age, age_var[5])
    if(input$over75)
      age <- append(age, age_var[6])
    
    # age <- switch(input$age,
    #               under20 = "Age under 20 ",
    #               under34 = "Age 20 to 34 ",
    #               under54 = "Age 35 to 54 ",
    #               under64 = "Age 55 to 64 ",
    #               under74 = "Age 65 to 74 ",
    #               over75 = "Age over 75 ",
    #               "Age under 20 ")

    dat <- filter(dat, variable %in% age)
    
    theme_set(theme_classic())
    p<- ggplot(dat, aes(x = Year, y = value, group = interaction(Region, variable), colour = interaction(Region, variable))) +
      geom_line() + 
      geom_point() + 
      labs(title = "Age Distribution", 
           x = "Mid-Year of Five Year Range",
           y = "% Population") + 
      theme(plot.title = element_text(face="bold", size=14, hjust=0)) +
      theme(axis.title = element_text(size=12)) + 
      theme(axis.text=element_text(size=10)) + 
      theme(plot.background = element_rect(fill = "light grey")) +
      theme(legend.text = element_text(size = 10)) +
      theme(legend.title = element_blank()) +
      theme(legend.position = "top")
    ggplotly(p, tooltip = c("x", "y", "colour")) %>% config(displayModeBar = F)
    
  })
  
  output$age_down <- downloadHandler(
    filename = function() {
      "plot_age.png"
    },
    content = function(file) {
      png(file)
      
      dat <- age_df()
      
      age_var <- unique(dat$variable)
      age <- c()
      if(input$under20)
        age <- append(age, age_var[1])
      if(input$under34)
        age <- append(age, age_var[2])
      if(input$under54)
        age <- append(age, age_var[3])
      if(input$under64)
        age <- append(age, age_var[4])
      if(input$under74)
        age <- append(age, age_var[5])
      if(input$over75)
        age <- append(age, age_var[6])
      
      dat <- filter(dat, variable %in% age)
      
      theme_set(theme_classic())
      p <- ggplot(dat, aes(x = Year, y = value, group = interaction(Region, variable), colour = interaction(Region, variable))) +
        geom_line() + 
        geom_point() + 
        labs(title = "Age Distribution", 
             x = "Mid-Year of Five Year Range",
             y = "% Population") + 
        theme(plot.title = element_text(face="bold", size=14, hjust=0)) +
        theme(axis.title = element_text(size=12)) + 
        theme(axis.text=element_text(size=10)) + 
        theme(plot.background = element_rect(fill = "light grey")) +
        theme(legend.text = element_text(size = 10)) +
        theme(legend.title = element_blank()) +
        theme(legend.position = "top")
      print(p)
      dev.off()
    }
  )

  rac_df <- reactive({
    
    county <- as.character(muni_county[muni_county$Municipal == input$muni,]$County)
    
    if(is.null(input$muni))
      my_place <- c("MA", "United States")
    if(!is.null(input$muni)){
      if(input$US_mean){
        if(input$MA_mean){
          if(input$CT_mean){
            my_place <- c("United States", "MA", input$muni, county) 
          } else{
            my_place <- c("United States", "MA", input$muni)
          }
        } else{
          if(input$CT_mean){
            my_place <- c("United States", input$muni, county)
          }
          my_place <- c("United States", input$muni)
        }
      } else{
        if(input$MA_mean){
          if(input$CT_mean){
            my_place <- c("MA", input$muni, county)
          } else{
            my_place <- c("MA", input$muni)
          }
        } else{
          if(input$CT_mean){
            my_place <- c(input$muni, county)
          } else{
            my_place <- c(input$muni)
          }
        }
      }
    }
    
    muni_df <- filter(dem_data, Region %in% my_place) %>% select(Region, White_Pct, Black_Pct, American_Indian_and_Alaska_Native_Pct, Asian_Pct, Hawaiian_and_Other_Pacific_Islander_Pct, Others_Pct, Year)
    muni_df <- melt(muni_df)
    muni_df$variable <- gsub("_Pct", "", muni_df$variable)
    muni_df$variable <- gsub("_", " ", muni_df$variable)
    muni_df$Year <- gsub("20", "'", muni_df$Year)
    muni_df
  })
  
  output$plot_rac <- renderPlotly({
    dat <- rac_df()
    
    race_var <- unique(dat$variable)
    race <- c()
    if(input$white)
      race <- append(race, race_var[1])
    if(input$black)
      race <- append(race, race_var[2])
    if(input$native)
      race <- append(race, race_var[3])
    if(input$asian)
      race <- append(race, race_var[4])
    if(input$hawaiian)
      race <- append(race, race_var[5])
    if(input$others)
      race <- append(race, race_var[6])
    
    # race <- switch(input$race,
    #               white = "White",
    #               black = "Black" ,
    #               native = "American Indian and Alaska Native",
    #               hawaiian = "Hawaiian and Other Pacific Islander",
    #               asian = "Asian",
    #               others = "Others",
    #               "White")
    
    dat <- filter(dat, variable %in% race)
    
    theme_set(theme_classic())
    p<- ggplot(dat, aes(x = Year, y = value, group = interaction(Region, variable), colour = interaction(Region, variable))) +
      geom_line() + 
      geom_point() + 
      labs(title = "Race Distribution", 
           x = "Mid-Year of Five Year Range",
           y = "% Population") + 
      theme(plot.title = element_text(face="bold", size=14, hjust=0)) +
      theme(axis.title = element_text(size=12)) + 
      theme(axis.text=element_text(size=10)) + 
      theme(plot.background = element_rect(fill = "light grey")) + 
      theme(legend.text = element_text(size = 10)) +
      theme(legend.title = element_blank()) + 
      theme(axis.title.x = element_text(margin = margin(r=10)))
    ggplotly(p, tooltip = c("x", "y", "colour")) %>% config(displayModeBar = F)
  })
  
  output$rac_down <- downloadHandler(
    filename = function() {
      "plot_race.png"
    },
    content = function(file) {
      png(file)
      
      dat <- rac_df()
      
      race_var <- unique(dat$variable)
      race <- c()
      if(input$white)
        race <- append(race, race_var[1])
      if(input$black)
        race <- append(race, race_var[2])
      if(input$native)
        race <- append(race, race_var[3])
      if(input$asian)
        race <- append(race, race_var[4])
      if(input$hawaiian)
        race <- append(race, race_var[5])
      if(input$others)
        race <- append(race, race_var[6])
      
      dat <- filter(dat, variable %in% race)
      
      theme_set(theme_classic())
      p<- ggplot(dat, aes(x = Year, y = value, group = interaction(Region, variable), colour = interaction(Region, variable))) +
        geom_line() + 
        geom_point() + 
        labs(title = "Race Distribution", 
             x = "Mid-Year of Five Year Range",
             y = "% Population") + 
        theme(plot.title = element_text(face="bold", size=14, hjust=0)) +
        theme(axis.title = element_text(size=12)) + 
        theme(axis.text=element_text(size=10)) + 
        theme(plot.background = element_rect(fill = "light grey")) + 
        theme(legend.text = element_text(size = 10)) +
        theme(legend.title = element_blank()) + 
        theme(axis.title.x = element_text(margin = margin(r=10)))
      print(p)
      dev.off()
    }
  )
  
  gen_df <- reactive({
    
    county <- as.character(muni_county[muni_county$Municipal == input$muni,]$County)
    
    if(is.null(input$muni))
      my_place <- c("MA", "United States")
    if(!is.null(input$muni)){
      if(input$US_mean){
        if(input$MA_mean){
          if(input$CT_mean){
            my_place <- c("United States", "MA", input$muni, county) 
          } else{
            my_place <- c("United States", "MA", input$muni)
          }
        } else{
          if(input$CT_mean){
            my_place <- c("United States", input$muni, county)
          }
          my_place <- c("United States", input$muni)
        }
      } else{
        if(input$MA_mean){
          if(input$CT_mean){
            my_place <- c("MA", input$muni, county)
          } else{
            my_place <- c("MA", input$muni)
          }
        } else{
          if(input$CT_mean){
            my_place <- c(input$muni, county)
          } else{
            my_place <- c(input$muni)
          }
        }
      }
    }
    
    muni_df <- filter(dem_data, Region %in% my_place) %>% select(Region, Male_Pct, Female_Pct, Year)
    colnames(muni_df) <- gsub("_Pct", "", colnames(muni_df))
    muni_df <- melt(muni_df)
    muni_df$Year <- gsub("20", "'", muni_df$Year)
    muni_df
  })
  
  output$plot_gen <- renderPlotly({
    dat <- gen_df() 
    theme_set(theme_classic())
    p<- ggplot(dat, aes(x=Year, y=value, group = interaction(Region,variable), colour = Region)) +
      geom_line() + 
      geom_point() + 
      facet_grid(. ~ variable) + 
      labs(title = "Gender Distribution", 
           x = "\n Mid-Year of Five Year Range",
           y = "% Population") + 
      theme(plot.title = element_text(face="bold", size=14, hjust=0)) +
      theme(axis.title = element_text(size=12)) + 
      theme(axis.text=element_text(size=10)) + 
      theme(plot.background = element_rect(fill = "light grey")) + 
      theme(legend.text = element_text(size = 10)) +
      theme(legend.title=element_blank())
    mytext=paste("Mid Year = ", dat$Year, "\n", "Value = ", dat$value, "%" ,"\n", "Region: ", dat$Region, sep="")  
    pp=plotly_build(p)   
    style(pp, text=mytext, hoverinfo = "text" ) %>% config(displayModeBar = F) 
  })
  
  output$gen_down <- downloadHandler(
    filename = function() {
      "plot_gender.png"
    },
    content = function(file) {
      png(file)
      dat <- gen_df() 
      theme_set(theme_classic())
      p<- ggplot(dat, aes(x=Year, y=value, group = interaction(Region,variable), colour = Region)) +
        geom_line() + 
        geom_point() + 
        facet_grid(. ~ variable) + 
        labs(title = "Gender Distribution", 
             x = "\n Mid-Year of Five Year Range",
             y = "% Population") + 
        theme(plot.title = element_text(face="bold", size=14, hjust=0)) +
        theme(axis.title = element_text(size=12)) + 
        theme(axis.text=element_text(size=10)) + 
        theme(plot.background = element_rect(fill = "light grey")) + 
        theme(legend.text = element_text(size = 10)) +
        theme(legend.title=element_blank())
      print(p)
      dev.off()
    }
  )
  
  his_df <- reactive({
    
    county <- as.character(muni_county[muni_county$Municipal == input$muni,]$County)
    
    if(is.null(input$muni))
      my_place <- c("MA", "United States")
    if(!is.null(input$muni)){
      if(input$US_mean){
        if(input$MA_mean){
          if(input$CT_mean){
            my_place <- c("United States", "MA", input$muni, county) 
          } else{
            my_place <- c("United States", "MA", input$muni)
          }
        } else{
          if(input$CT_mean){
            my_place <- c("United States", input$muni, county)
          }
          my_place <- c("United States", input$muni)
        }
      } else{
        if(input$MA_mean){
          if(input$CT_mean){
            my_place <- c("MA", input$muni, county)
          } else{
            my_place <- c("MA", input$muni)
          }
        } else{
          if(input$CT_mean){
            my_place <- c(input$muni, county)
          } else{
            my_place <- c(input$muni)
          }
        }
      }
    }
    
    muni_df <- filter(dem_data, Region %in% my_place) %>% select(Region, Hispanic_Pct, Not_Hispanic_Pct, Year)
    colnames(muni_df) <- gsub("_Pct", "", colnames(muni_df))
    colnames(muni_df) <- gsub("_", " ", colnames(muni_df))
    muni_df <- melt(muni_df)
    muni_df$Year <- gsub("20", "'", muni_df$Year)
    muni_df
  })
  
  output$plot_his <- renderPlotly({
    dat <- his_df() 
    theme_set(theme_classic())
    p<- ggplot(dat, aes(x=Year, y=value, group = interaction(Region,variable), colour = Region)) +
      geom_line() + 
      geom_point() + 
      facet_grid(. ~ variable) + 
      labs(title = "Ethnicity Distribution", 
           x = "Mid-Year of Five Year Range",
           y = "% Population") + 
      theme(plot.title = element_text(face="bold", size=14, hjust=0)) +
      theme(axis.title = element_text(size=12)) + 
      theme(axis.text=element_text(size=10)) + 
      theme(plot.background = element_rect(fill = "light grey")) + 
      theme(legend.text = element_text(size = 10)) +
      theme(legend.title=element_blank())
    mytext=paste("Mid Year = ", dat$Year, "\n", "Value = ", dat$value, "%" , "\n", "Region: ", dat$Region, sep="")    
    pp=plotly_build(p)   
    style(pp, text=mytext, hoverinfo = "text" ) %>% config(displayModeBar = F)
  })
  
  output$his_down <- downloadHandler(
    filename = function() {
      "plot_ethnicity.png"
    },
    content = function(file) {
      png(file)
      dat <- his_df() 
      theme_set(theme_classic())
      p<- ggplot(dat, aes(x=Year, y=value, group = interaction(Region,variable), colour = Region)) +
        geom_line() + 
        geom_point() + 
        facet_grid(. ~ variable) + 
        labs(title = "Ethnicity Distribution", 
             x = "Mid-Year of Five Year Range",
             y = "% Population") + 
        theme(plot.title = element_text(face="bold", size=14, hjust=0)) +
        theme(axis.title = element_text(size=12)) + 
        theme(axis.text=element_text(size=10)) + 
        theme(plot.background = element_rect(fill = "light grey")) + 
        theme(legend.text = element_text(size = 10)) +
        theme(legend.title=element_blank())
      print(p)
      dev.off()
    }
  )
  
  edu_df <- reactive({
    
    county <- as.character(muni_county[muni_county$Municipal == input$muni,]$County)
    
    if(is.null(input$muni))
      my_place <- c("MA", "United States")
    if(!is.null(input$muni)){
      if(input$US_mean){
        if(input$MA_mean){
          if(input$CT_mean){
            my_place <- c("United States", "MA", input$muni, county) 
          } else{
            my_place <- c("United States", "MA", input$muni)
          }
        } else{
          if(input$CT_mean){
            my_place <- c("United States", input$muni, county)
          }
          my_place <- c("United States", input$muni)
        }
      } else{
        if(input$MA_mean){
          if(input$CT_mean){
            my_place <- c("MA", input$muni, county)
          } else{
            my_place <- c("MA", input$muni)
          }
        } else{
          if(input$CT_mean){
            my_place <- c(input$muni, county)
          } else{
            my_place <- c(input$muni)
          }
        }
      }
    }
    
    muni_df <- filter(edu_data, Region %in% my_place) %>% select(Region, HS_Pct, Bachelors_Pct, Grad_Pct, Year)
    muni_df <- melt(muni_df)
    muni_df$variable <- gsub("_Pct", " %", muni_df$variable)
    muni_df$variable <- gsub("HS", "High School", muni_df$variable)
    muni_df$Year <- gsub("20", "'", muni_df$Year)
    muni_df
  })
  
  output$plot_edu <- renderPlotly({
    dat <- edu_df() 
    
    education <- switch(input$education,
                     hc = "High School %",
                     bac = "Bachelors %",
                     grad = "Grad %",
                     "High School %")
    dat <- filter(dat, variable == education)
    
    theme_set(theme_classic())
    p<- ggplot(dat, aes(x=Year, y=value, group = Region, colour = Region)) +
      geom_line() + 
      geom_point() + 
      labs(title = "Educational Attainment", 
           x = "Mid-Year of Five Year Range",
           y = "% Population") + 
      theme(plot.title = element_text(face="bold", size=14, hjust=0)) +
      theme(axis.title = element_text(size=12)) + 
      theme(axis.text=element_text(size=10)) + 
      theme(plot.background = element_rect(fill = "light grey")) + 
      theme(legend.text = element_text(size = 10)) +
      theme(legend.title=element_blank())
    mytext=paste("Mid Year = ", dat$Year, "\n", "Value = ", dat$value, "%" ,"\n", "Group: ", input$education, "\n", "Region: ", dat$Region, sep="")    
    pp=plotly_build(p)   
    style(pp, text=mytext, hoverinfo = "text" ) %>% config(displayModeBar = F)
    })
  
  output$edu_down <- downloadHandler(
    filename = function() {
      "plot_education.png"
    },
    content = function(file) {
      png(file)
      dat <- edu_df() 
      
      education <- switch(input$education,
                          hc = "High School %",
                          bac = "Bachelors %",
                          grad = "Grad %",
                          "High School %")
      dat <- filter(dat, variable == education)
      
      theme_set(theme_classic())
      p<- ggplot(dat, aes(x=Year, y=value, group = Region, colour = Region)) +
        geom_line() + 
        geom_point() + 
        labs(title = "Educational Attainment", 
             x = "Mid-Year of Five Year Range",
             y = "% Population") + 
        theme(plot.title = element_text(face="bold", size=14, hjust=0)) +
        theme(axis.title = element_text(size=12)) + 
        theme(axis.text=element_text(size=10)) + 
        theme(plot.background = element_rect(fill = "light grey")) + 
        theme(legend.text = element_text(size = 10)) +
        theme(legend.title=element_blank())
      print(p)
      dev.off()
    }
  )
  
  mar_df <- reactive({
    
    county <- as.character(muni_county[muni_county$Municipal == input$muni,]$County)
    
    if(is.null(input$muni))
      my_place <- c("MA", "United States")
    if(!is.null(input$muni)){
      if(input$US_mean){
        if(input$MA_mean){
          if(input$CT_mean){
            my_place <- c("United States", "MA", input$muni, county) 
          } else{
            my_place <- c("United States", "MA", input$muni)
          }
        } else{
          if(input$CT_mean){
            my_place <- c("United States", input$muni, county)
          }
          my_place <- c("United States", input$muni)
        }
      } else{
        if(input$MA_mean){
          if(input$CT_mean){
            my_place <- c("MA", input$muni, county)
          } else{
            my_place <- c("MA", input$muni)
          }
        } else{
          if(input$CT_mean){
            my_place <- c(input$muni, county)
          } else{
            my_place <- c(input$muni)
          }
        }
      }
    }
    
    muni_df <- filter(mar_data, Region %in% my_place) %>% select(Region, Never_Married_pct, Now_Married_pct, Separated_pct, Widowed_pct, Divorced_pct, Gender, Year)
    names(muni_df) <- gsub("_", " ", names(muni_df))
    names(muni_df) <- gsub("pct", "%", names(muni_df))
    muni_df$Year <- gsub("20", "'", muni_df$Year)
    muni_df
  })
  
  output$plot_mar <- renderPlotly({
    
    dat <- mar_df()
    
    status <- switch(input$status,
                     married = "Now Married %",
                     separated = "Separated %",
                     divorced = "Divorced %",
                     widowed = "Widowed %",
                     never = "Never Married %",
                     "Now Married %")
    
    dat <- melt(dat)
    dat <- subset(dat, variable == status)

    # theme_set(theme_classic())
    # p<- ggplot(dat, aes(x=Year, y=value, fill=variable)) +
    #   geom_bar(stat='density', position='dodge') +
    #   facet_grid(. ~ Region) + 
    #   labs(title = "Educational Attainment", 
    #        x = "Mid-Year of Five Year Range",
    #        y = "% Population")
    # ggplotly(p)  

    theme_set(theme_classic())
    p<- ggplot(dat, aes(x=Year, y=value, group = Region, colour = Region)) +
      geom_line() + 
      geom_point() + 
      facet_grid(. ~ Gender) + 
      labs(title = paste("Marital Status (",status,")"), 
           x = "Mid-Year of Five Year Range",
           y = "% Population") + 
      theme(plot.title = element_text(face="bold", size=14, hjust=0)) +
      theme(axis.title = element_text(size=12)) + 
      theme(axis.text=element_text(size=10)) + 
      theme(plot.background = element_rect(fill = "light grey")) + 
      theme(legend.text = element_text(size = 10)) +
      theme(legend.title=element_blank())
    mytext=paste("Mid Year = ", dat$Year, "\n", "Value = ", dat$value, "%" ,"\n", "Group: ", input$status, "\n", "Region: ", dat$Region, sep="")    
    pp=plotly_build(p)   
    style(pp, text=mytext, hoverinfo = "text" )  %>% config(displayModeBar = F)
    })
  
  output$mar_down <- downloadHandler(
    filename = function() {
      "plot_marriage.png"
    },
    content = function(file) {
      png(file)
      dat <- mar_df()
      
      status <- switch(input$status,
                       married = "Now Married %",
                       separated = "Separated %",
                       divorced = "Divorced %",
                       widowed = "Widowed %",
                       never = "Never Married %",
                       "Now Married %")
      
      dat <- melt(dat)
      dat <- subset(dat, variable == status)
      theme_set(theme_classic())
      p<- ggplot(dat, aes(x=Year, y=value, group = Region, colour = Region)) +
        geom_line() + 
        geom_point() + 
        facet_grid(. ~ Gender) + 
        labs(title = paste("Marital Status (",status,")"), 
             x = "Mid-Year of Five Year Range",
             y = "% Population") + 
        theme(plot.title = element_text(face="bold", size=14, hjust=0)) +
        theme(axis.title = element_text(size=12)) + 
        theme(axis.text=element_text(size=10)) + 
        theme(plot.background = element_rect(fill = "light grey")) + 
        theme(legend.text = element_text(size = 10)) +
        theme(legend.title=element_blank())
      print(p)
      dev.off()
    }
  )
  
  sui_df <- reactive({
    
    if(is.null(input$muni))
      my_place <- c("MA", "United States")

    if(!is.null(input$muni))
      county <- c()
      for (m in 1:length(input$muni)){
        county[m] <- as.character(muni_county$County[muni_county$Municipal==input$muni[m]])
      }
    county <- gsub(" County", "", county)
      my_place <- c(county, "MA", "United States")
    
    muni_df <- filter(sui_data, County %in% my_place) %>% select(County, Age.Adjusted.Rate, Year)
    muni_df$Year <- gsub("20", "'", muni_df$Year)
    muni_df <- muni_df[muni_df$Year!="1999",]
    muni_df
  })
  
  output$plot_sui <- renderPlotly({
    dat <- sui_df() 
    theme_set(theme_classic())
    p <- ggplot(dat, aes(x=Year, y=Age.Adjusted.Rate, group = County, colour=County)) + 
      geom_line() + 
      geom_point() + 
      labs(title = "Suicide Rate",
           x = "One Year Estimates",
           y = "% Population") + 
      theme(plot.title = element_text(face="bold", size=14, hjust=0)) +
      theme(axis.title = element_text(size=12)) + 
      theme(axis.text=element_text(size=10)) + 
      theme(plot.background = element_rect(fill = "light grey")) + 
      theme(legend.text = element_text(size = 10)) +
      theme(legend.title=element_blank())
    mytext=paste("Year = ", dat$Year, "\n", "Value = ", dat$Age.Adjusted.Rate, "%" , "\n", "Region: ", dat$County, sep="")    
    pp=plotly_build(p)   
    style(pp, text=mytext, hoverinfo = "text" ) %>% config(displayModeBar = F)
  })
  
  output$sui_down <- downloadHandler(
    filename = function() {
      "plot_suicide.png"
    },
    content = function(file) {
      png(file)
      dat <- sui_df() 
      theme_set(theme_classic())
      p <- ggplot(dat, aes(x=Year, y=Age.Adjusted.Rate, group = County, colour=County)) + 
        geom_line() + 
        geom_point() + 
        labs(title = "Suicide Rate",
             x = "One Year Estimates",
             y = "% Population") + 
        theme(plot.title = element_text(face="bold", size=14, hjust=0)) +
        theme(axis.title = element_text(size=12)) + 
        theme(axis.text=element_text(size=10)) + 
        theme(plot.background = element_rect(fill = "light grey")) + 
        theme(legend.text = element_text(size = 10)) +
        theme(legend.title=element_blank())
      print(p)
      dev.off()
    }
  )
  
  vet_df <- reactive({
    
    county <- as.character(muni_county[muni_county$Municipal == input$muni,]$County)
    
    if(is.null(input$muni))
      my_place <- c("MA", "United States")
    if(!is.null(input$muni)){
      if(input$US_mean){
        if(input$MA_mean){
          if(input$CT_mean){
            my_place <- c("United States", "MA", input$muni, county) 
          } else{
            my_place <- c("United States", "MA", input$muni)
          }
        } else{
          if(input$CT_mean){
            my_place <- c("United States", input$muni, county)
          }
          my_place <- c("United States", input$muni)
        }
      } else{
        if(input$MA_mean){
          if(input$CT_mean){
            my_place <- c("MA", input$muni, county)
          } else{
            my_place <- c("MA", input$muni)
          }
        } else{
          if(input$CT_mean){
            my_place <- c(input$muni, county)
          } else{
            my_place <- c(input$muni)
          }
        }
      }
    }
    
    muni_df <- filter(vet_data, Region %in% my_place) %>% select(Region, Percent_Vet, Year)
    muni_df <- melt(muni_df)
    muni_df$Year <- gsub("20", "'", muni_df$Year)
    muni_df
  })
  
  output$plot_vet <- renderPlotly({
    dat <- vet_df() 
    theme_set(theme_classic())
    p <- ggplot(dat, aes(x=Year, y=value, group = Region, colour=Region)) + 
      geom_line() + 
      geom_point() + 
      labs(title = "Civilian Veteran's Status", 
           x = "Mid-Year of Five Year Range",
           y = "% Population") + 
      theme(plot.title = element_text(face="bold", size=14, hjust=0)) +
      theme(axis.title = element_text(size=12)) + 
      theme(axis.text=element_text(size=10)) + 
      theme(plot.background = element_rect(fill = "light grey")) + 
      theme(legend.text = element_text(size = 10)) +
      theme(legend.title=element_blank())
    mytext=paste("Mid Year = ", dat$Year, "\n", "Value = ", dat$value, "%" ,"\n", "Region: ", dat$Region, sep="")    
    pp=plotly_build(p)   
    style(pp, text=mytext, hoverinfo = "text" ) %>% config(displayModeBar = F)
    })
  
  output$vet_down <- downloadHandler(
    filename = function() {
      "plot_veteran.png"
    },
    content = function(file) {
      png(file)
      dat <- vet_df() 
      theme_set(theme_classic())
      p <- ggplot(dat, aes(x=Year, y=value, group = Region, colour=Region)) + 
        geom_line() + 
        geom_point() + 
        labs(title = "Civilian Veteran's Status", 
             x = "Mid-Year of Five Year Range",
             y = "% Population") + 
        theme(plot.title = element_text(face="bold", size=14, hjust=0)) +
        theme(axis.title = element_text(size=12)) + 
        theme(axis.text=element_text(size=10)) + 
        theme(plot.background = element_rect(fill = "light grey")) + 
        theme(legend.text = element_text(size = 10)) +
        theme(legend.title=element_blank())
      print(p)
      dev.off()
    }
  )
  
}

shinyApp(ui = ui, server = server)

