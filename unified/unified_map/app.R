#############################
## Unified Municipal App   ##
## Author: Zhenning Kang   ##
## Date Created: 10/19/17  ##
## Last Modified: 10/20/17 ##
#############################

### SETTINGS ###
library(shiny)
library(dplyr)
library(reshape2)
library(ggplot2)

### DATA ###
### DEMOGRAPHIC TAB
dem_data <- read.csv(file="demodata.csv")
dem_data$Year <- as.factor(as.numeric(substr(dem_data$Five_Year_Range, 1, 4))+2)

### SOCIAL TAB
# data for education plot
edu_data <- read.csv(file="edudata.csv")[,-1]
edu_data$Year <- as.factor(as.numeric(substr(edu_data$Five_Year_Range, 1, 4))+2)

# data for vetaran plot
vet_data <- read.csv(file="vetstatusdata.csv")[,-1]
vet_data$Year <- as.factor(as.numeric(substr(vet_data$Five_Year_Range, 1, 4))+2)

# data for married status plot
mar_data <- read.csv(file="BA002_02_marriagedata.csv")
mar_data$Year <- as.factor(as.numeric(substr(mar_data$Five_Year_Range, 1, 4))+2)

# data for suicide plot
sui_data <- read.csv(file="SASuicidedata_Updated2017.csv")[,-1]
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

### USER INTERFACE ###
ui <- fluidPage(
  # blank title, but put in a special title for window tab
  titlePanel("", windowTitle = "SEIGMA Unified Shiny App"),
  fluidRow(
    column(2,
           selectInput("muni", "Select Municipality",
                       choices = MA_municipals, 
                       selected = "Abington",
                       multiple = T)
           ),
    column(8,
           ## put in logo for title
           a(img(src = "logo.jpg", height=105, width=920), href="http://www.umass.edu/seigma/")
           ),
    column(2,
           helpText(a("Comments or Feedback", href="http://www.surveygizmo.com/s3/1832020/ShinyApp-Evaluation", target="_blank",onclick="ga('send', 'event', 'click', 'link', 'feedback', 1)")),
           ## data source citation
           helpText(a("Data Source", href="http://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_14_5YR_S2502&prodType=table",
                      target="_blank",onclick="ga('send', 'event', 'click', 'link', 'dataAge', 1)")),
           ## GitHub link
           helpText(a("Codes on GitHub",
                      href="https://github.com/sEigmA/SEIGMA/tree/gh-pages/unified", target="_blank",onclick="ga('send', 'event', 'click', 'link', 'code', 1)")),
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
                          helpText(a("More information about Demographics.", href="https://seigma.shinyapps.io/demographics/", target="_blank",onclick="ga('send', 'event', 'click', 'link', 'dem_app', 1)"))),
                   column(4)
                 ),
                 fluidRow(
                   column(6,
                          plotOutput("plot_gen")
                          ),
                   column(6,
                          fluidRow(
                            column(9,
                                   plotOutput("plot_age")
                            ),
                            column(3,
                                   radioButtons("age", "Select an Age:",
                                                c("Under 20" = "under20",
                                                  "20 to 34" = "under34",
                                                  "35 to 54" = "under54",
                                                  "55 to 64" = "under64",
                                                  "65 to 74" = "under74",
                                                  "Over 75" = "over75"),
                                                inline=F)
                                   ))
                   )),
                 br(),
                 fluidRow(
                   column(6,
                          fluidRow(
                            column(9,
                                   plotOutput("plot_rac")
                            ),
                            column(3,
                                   radioButtons("race", "Select Race:",
                                                c("White" = "white",
                                                  "Black" = "black",
                                                  "American Indian and Alaska Native" = "native",
                                                  "Hawaiian and Other Pacific Islander" = "hawaiian",
                                                  "Asian" = "asian",
                                                  "Others" = "other"),
                                                inline=F)
                            ))),
                   column(6,
                          plotOutput("plot_his"))
                   )                 ),
        tabPanel("Social",
                 br(),
                 fluidRow(
                   column(6,
                          fluidRow(
                            column(9,
                                   plotOutput("plot_edu")
                                   ),
                            column(3,
                                   radioButtons("education", "Choose a Level:",
                                                c("High School" = "hs",
                                                  "Bachelor" = "bac",
                                                  "Graduate" = "grad"),
                                                inline=T)
                                   )),
                          # app link
                          helpText(a("More information about Education.", href="https://seigma.shinyapps.io/educational_attainment/", target="_blank",onclick="ga('send', 'event', 'click', 'link', 'edu_app', 1)"))
                          ),
                   column(6,
                          fluidRow(
                            column(9,
                                   plotOutput("plot_mar")
                                   ),
                            column(3,
                                   radioButtons("status", "Choose a Status:",
                                                c("Married" = "married",
                                                  "Separated" = "separated",
                                                  "Divorced" = "divorced",
                                                  "Widowed" = "widowed",
                                                  "Never" = "never"),
                                                inline=T)
                                   )),
                          # app link
                          helpText(a("More information about Marital.",
                                     href="https://seigma.shinyapps.io/marital/", target="_blank",onclick="ga('send', 'event', 'click', 'link', 'mar_app', 1)"))
                   )),
                 br(),
                 fluidRow(
                   column(6,
                          plotOutput("plot_sui"),
                          # app link
                          helpText(a("More information about Suicide.", href="https://seigma.shinyapps.io/suicide/", target="_blank",onclick="ga('send', 'event', 'click', 'link', 'sui_app', 1)"))
                   ),
                   column(6,
                          plotOutput("plot_vet"),
                          # app link
                          helpText(a("More information about Veteran.", href="https://seigma.shinyapps.io/va_status/", target="_blank",onclick="ga('send', 'event', 'click', 'link', 'vet_app', 1)"))
                   ))
                 )
      )
    )
  )


### SERVER
server <- function(input, output){

  gen_df <- reactive({
    
    if(!is.null(input$muni))
      my_place <- c(input$muni, "MA", "United States") 
    if(is.null(input$muni))
      my_place <- c("MA", "United States") 
    
    muni_df <- filter(dem_data, Region %in% my_place) %>% select(Region, Male_Pct, Female_Pct, Year)
    colnames(muni_df) <- gsub("_Pct", "", colnames(muni_df))
    muni_df <- melt(muni_df)
    muni_df
  })
  
  output$plot_gen <- renderPlot({
    dat <- gen_df() 
    theme_set(theme_classic())
    p<- ggplot(dat, aes(x=Year, y=value, group = interaction(Region,variable), colour = Region)) +
      geom_line() + 
      geom_point() + 
      facet_grid(. ~ variable) + 
      labs(title = "Gender Distribution", 
           x = "Mid-Year of Five Year Range",
           y = "% Population") + 
      theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
      theme(axis.title = element_text(face="bold", size=18)) 
    print(p) 
  })
  
  age_df <- reactive({
    
    if(!is.null(input$muni))
      my_place <- c(input$muni, "MA", "United States") 
    if(is.null(input$muni))
      my_place <- c("MA", "United States")
    
    muni_df <- filter(dem_data, Region %in% my_place) %>% select(Region, Age_under_20_Pct_plot, Age_20_34_Pct_plot, Age_35_54_Pct_plot, Age_55_64_Pct_plot, Age_65_74_Pct_plot, Age_over_75_Pct_plot, Year)
    muni_df <- melt(muni_df)
    muni_df$variable <- gsub("0_3", "0 to 3", muni_df$variable)
    muni_df$variable[1:70] <- gsub("5_", "5 to ", muni_df$variable[1:70])
    muni_df$variable <- gsub("_", " ", muni_df$variable)
    muni_df$variable <- gsub("Pct plot", "", muni_df$variable)
    muni_df
  })
  
  output$plot_age <- renderPlot({
    dat <- age_df()
    
    age <- switch(input$age,
                  under20 = "Age under 20 ",
                  under34 = "Age 20 to 34 ",
                  under54 = "Age 35 to 54 ",
                  under64 = "Age 55 to 64 ",
                  under74 = "Age 65 to 74 ",
                  over75 = "Age over 75 ",
                  "Age under 20 ")

    dat <- filter(dat, variable == age)
    
    theme_set(theme_classic())
    p<- ggplot(dat, aes(x = Year, y = value, group = Region, colour = Region)) +
      geom_line() + 
      geom_point() + 
      labs(title = paste(age,"Distribution"), 
           x = "Mid-Year of Five Year Range",
           y = "% Population") + 
      theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
      theme(axis.title = element_text(face="bold", size=18)) 
    print(p) 
  })

  rac_df <- reactive({
    
    if(!is.null(input$muni))
      my_place <- c(input$muni, "MA", "United States") 
    if(is.null(input$muni))
      my_place <- c("MA", "United States")
    
    muni_df <- filter(dem_data, Region %in% my_place) %>% select(Region, White_Pct, Black_Pct, American_Indian_and_Alaska_Native_Pct, Asian_Pct, Hawaiian_and_Other_Pacific_Islander_Pct, Others_Pct, Year)
    muni_df <- melt(muni_df)
    muni_df$variable <- gsub("_Pct", "", muni_df$variable)
    muni_df$variable <- gsub("_", " ", muni_df$variable)
    muni_df
  })
  
  output$plot_rac <- renderPlot({
    dat <- rac_df()
    
    race <- switch(input$race,
                  white = "White",
                  black = "Black" ,
                  native = "American Indian and Alaska Native",
                  hawaiian = "Hawaiian and Other Pacific Islander",
                  asian = "Asian",
                  others = "Others",
                  "White")
    
    dat <- filter(dat, variable == race)
    
    theme_set(theme_classic())
    p<- ggplot(dat, aes(x = Year, y = value, group = Region, colour = Region)) +
      geom_line() + 
      geom_point() + 
      labs(title = paste(race,"Distribution"), 
           x = "Mid-Year of Five Year Range",
           y = "% Population") + 
      theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
      theme(axis.title = element_text(face="bold", size=18)) 
    print(p) 
  })
    
  his_df <- reactive({
    
    if(!is.null(input$muni))
      my_place <- c(input$muni, "MA", "United States") 
    if(is.null(input$muni))
      my_place <- c("MA", "United States")
    
    muni_df <- filter(dem_data, Region %in% my_place) %>% select(Region, Hispanic_Pct, Not_Hispanic_Pct, Year)
    colnames(muni_df) <- gsub("_Pct", "", colnames(muni_df))
    colnames(muni_df) <- gsub("_", " ", colnames(muni_df))
    muni_df <- melt(muni_df)
    muni_df
  })
  
  output$plot_his <- renderPlot({
    dat <- his_df() 
    theme_set(theme_classic())
    p<- ggplot(dat, aes(x=Year, y=value, group = interaction(Region,variable), colour = Region)) +
      geom_line() + 
      geom_point() + 
      facet_grid(. ~ variable) + 
      labs(title = "Ethnicity Distribution", 
           x = "Mid-Year of Five Year Range",
           y = "% Population") + 
      theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
      theme(axis.title = element_text(face="bold", size=18)) 
    print(p) 
  })
  
  edu_df <- reactive({
    
    if(!is.null(input$muni))
      my_place <- c(input$muni, "MA", "United States") 
    if(is.null(input$muni))
      my_place <- c("MA", "United States")
    
    muni_df <- filter(edu_data, Region %in% my_place) %>% select(Region, HS_Pct, Bachelors_Pct, Grad_Pct, Year)
    muni_df <- melt(muni_df)
    muni_df$variable <- gsub("_Pct", " %", muni_df$variable)
    muni_df$variable <- gsub("HS", "High School", muni_df$variable)
    muni_df
  })
  
  output$plot_edu <- renderPlot({
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
      theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
      theme(axis.title = element_text(face="bold", size=18)) 
    print(p) 
    })
  
  mar_df <- reactive({
    
    if(!is.null(input$muni))
      my_place <- c(input$muni, "MA", "United States") 
    if(is.null(input$muni))
      my_place <- c("MA", "United States")
    
    mar_df <- filter(mar_data, Region %in% my_place) %>% select(Region, Never_Married_pct, Now_Married_pct, Separated_pct, Widowed_pct, Divorced_pct, Gender, Year)
    names(mar_df) <- gsub("_", " ", names(mar_df))
    names(mar_df) <- gsub("pct", "%", names(mar_df))
    mar_df
  })
  
  output$plot_mar <- renderPlot({
    
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
    # print(p)  

    theme_set(theme_classic())
    p<- ggplot(dat, aes(x=Year, y=value, group = Region, colour = Region)) +
      geom_line() + 
      geom_point() + 
      facet_grid(. ~ Gender) + 
      labs(title = paste("Marital Status (",status,")"), 
           x = "Mid-Year of Five Year Range",
           y = "% Population") + 
      theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
      theme(axis.title = element_text(face="bold", size=18)) 
    print(p) 
  })
  
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
    muni_df
  })
  
  output$plot_sui <- renderPlot({
    dat <- sui_df() 
    theme_set(theme_classic())
    p <- ggplot(dat, aes(x=Year, y=Age.Adjusted.Rate, group = County, colour=County)) + 
      geom_line() + 
      geom_point() + 
      labs(title = "Age-adjusted Suicide Rate (per 100,000 population)", 
           x = "Mid-Year of Five Year Range",
           y = "% Population") + 
      theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
      theme(axis.title = element_text(face="bold", size=18)) 
    print(p)  
  })
  
  vet_df <- reactive({
    
    if(!is.null(input$muni))
      my_place <- c(input$muni, "MA", "United States") 
    if(is.null(input$muni))
      my_place <- c("MA", "United States")
    
    muni_df <- filter(vet_data, Region %in% my_place) %>% select(Region, Percent_Vet, Year)
    muni_df <- melt(muni_df)
    muni_df
  })
  
  output$plot_vet <- renderPlot({
    dat <- vet_df() 
    theme_set(theme_classic())
    p <- ggplot(dat, aes(x=Year, y=value, group = Region, colour=Region)) + 
      geom_line() + 
      geom_point() + 
      labs(title = "Civilian Veteran's Status", 
           x = "Mid-Year of Five Year Range",
           y = "% Population") + 
      theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
      theme(axis.title = element_text(face="bold", size=18)) 
    print(p)  
  })
  
}

shinyApp(ui = ui, server = server)

