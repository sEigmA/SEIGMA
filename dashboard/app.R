################################
## Title: SEIGMA dashboard    ##
## Author: Zhenning Kang      ##
## Date Created:  09/27/2017  ##
## Date Modified: 10/20/2017  ##
################################

### SETTINGS ###
library(shiny)
library(shinydashboard)
library(dplyr)
library(reshape2)
library(ggplot2)

##### DATA #####
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

### REGIONS
MA_municipals <- as.character(na.omit(unique(dem_data$Municipal)))

##### UI #####

header <- dashboardHeader(title = "SEIGMA Dashboard", disable = TRUE)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Municipality", icon = icon("address-book"),
             selectInput("muni", "Select Municipalities",
                         choices = MA_municipals,
                         selected = "Abington",
                         multiple = TRUE)
    ),
    menuItem("Comparison", icon = icon("check-square-o"),
             checkboxInput("MA_mean", "Compare to MA Average", TRUE),
             checkboxInput("US_mean", "Compare to US Average", TRUE)
    ),
    menuItem("Demographics", tabName = "demo", icon = icon("dashboard")
             ),
    menuItem("Social", tabName = "soci", icon = icon("th")
             ),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    menuItem("Comments or Feedback", icon = icon("envelope-o"),
             href="http://www.surveygizmo.com/s3/1832020/ShinyApp-Evaluation"),
    menuItem("Data Source", icon = icon("file-code-o"), 
             href = "http://www.surveygizmo.com/s3/1832020/ShinyApp-Evaluation"),
    menuItem("Codes on Github", icon = icon("code-fork"), 
             href = "http://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_14_5YR_S2502&prodType=table"),
    menuItem(
      helpText("Created by Zhenning Kang")
    )
    )
  
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "demo",
            fluidRow(
              box(width = 12,
                  a(img(src = "logo.jpg", height=105, width=920), href="http://www.umass.edu/seigma/")
                  )
              ),
            fluidRow(
              box(width = 12,
                  helpText(a("More information about Demographics.", href="https://seigma.shinyapps.io/demographics/", target="_blank",onclick="ga('send', 'event', 'click', 'link', 'dem_app', 1)"))
              )),
            fluidRow(
              box(width = 6,
                fluidRow(
                  box(width = 11,
                      selectInput("age", "Select an Age:",
                                  c("Under 20" = "under20",
                                    "20 to 34" = "under34",
                                    "35 to 54" = "under54",
                                    "55 to 64" = "under64",
                                    "65 to 74" = "under74",
                                    "Over 75" = "over75"),
                                  selected = "under20",
                                  multiple = FALSE)
                      )
                ),
                fluidRow(
                  plotOutput("plot_age")
                )
              ),
              box(width = 6,
                fluidRow(
                  box(width = 11,
                      selectInput("race", "Select a Race:",
                                  c("White" = "white",
                                    "Black" = "black",
                                    "American Indian and Alaska Native" = "native",
                                    "Hawaiian and Other Pacific Islander" = "hawaiian",
                                    "Asian" = "asian",
                                    "Others" = "other"),
                                  selected = "white",
                                  multiple = FALSE)
                  )
                ),
                fluidRow(
                  plotOutput("plot_rac")
                )
              )
            ),
            fluidRow(
              box(width = 6,
                plotOutput("plot_gen")
              ),
              box(width = 6,
                plotOutput("plot_his")
              )
            )
    ),

    tabItem(tabName = "soci",
            fluidRow(
              box(width = 12,
                  a(img(src = "logo.jpg", height=105, width=920), href="http://www.umass.edu/seigma/")
              )
            ),
            fluidRow(
              box(width = 12,
                  fluidRow(
                    box(width = 11,
                        selectInput("status", "Choose a Status of Interest:",
                                    c("Married" = "married",
                                      "Separated" = "separated",
                                      "Divorced" = "divorced",
                                      "Widowed" = "widowed",
                                      "Never" = "never"),
                                    selected = "married",
                                    multiple = FALSE),
                        helpText(a("More information about Marital.",
                                   href="https://seigma.shinyapps.io/marital/"))
                    )
                  ),
                  fluidRow(
                    plotOutput("plot_mar")
                    )
                  )
              ),
              fluidRow(
                box(width = 6,
                    fluidRow(
                      box(width = 11,
                          selectInput(
                            "education", "Choose a Level:",
                            c("High School" = "hs",
                              "Bachelor" = "bac",
                              "Graduate" = "grad"),
                            selected = "hs",
                            multiple = FALSE),
                          helpText(a("More information about Education.", href="https://seigma.shinyapps.io/educational_attainment/"))
                          )
                    ),
                    fluidRow(
                      plotOutput("plot_edu")
                      )
                    ),
                box(width = 6,
                    plotOutput("plot_vet"),
                    fluidRow(
                      box(width = 11,
                          helpText(a("More information about Veteran.", href="https://seigma.shinyapps.io/va_status/"))
                      )
                    )
              )
    )
  )
)
)

##### SERVER #####

server <- function(input, output, session){
  gen_df <- reactive({
    if(is.null(input$muni))
      my_place <- c("MA", "United States")
    if(!is.null(input$muni)){
      if(input$US_mean){
        if(input$MA_mean){
          my_place <- c("United States", "MA", input$muni) ## US and MA  
        } else{
          my_place <- c("United States", input$muni) ## US only
        }
      } else{
        if(input$MA_mean){
          my_place <- c("MA", input$muni) ## US only ## MA only
        } else{
          my_place <- c(input$muni)
        }
      }
    }
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
    if(is.null(input$muni))
      my_place <- c("MA", "United States")
    if(!is.null(input$muni)){
      if(input$US_mean){
        if(input$MA_mean){
          my_place <- c("United States", "MA", input$muni) ## US and MA  
        } else{
          my_place <- c("United States", input$muni) ## US only
        }
      } else{
        if(input$MA_mean){
          my_place <- c("MA", input$muni) ## US only ## MA only
        } else{
          my_place <- c(input$muni)
        }
      }
    }
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
    if(is.null(input$muni))
      my_place <- c("MA", "United States")
    if(!is.null(input$muni)){
      if(input$US_mean){
        if(input$MA_mean){
          my_place <- c("United States", "MA", input$muni) ## US and MA  
        } else{
          my_place <- c("United States", input$muni) ## US only
        }
      } else{
        if(input$MA_mean){
          my_place <- c("MA", input$muni) ## US only ## MA only
        } else{
          my_place <- c(input$muni)
        }
      }
    }
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
    if(is.null(input$muni))
      my_place <- c("MA", "United States")
    if(!is.null(input$muni)){
      if(input$US_mean){
        if(input$MA_mean){
          my_place <- c("United States", "MA", input$muni) ## US and MA  
        } else{
          my_place <- c("United States", input$muni) ## US only
        }
      } else{
        if(input$MA_mean){
          my_place <- c("MA", input$muni) ## US only ## MA only
        } else{
          my_place <- c(input$muni)
        }
      }
    }
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
    if(is.null(input$muni))
      my_place <- c("MA", "United States")
    if(!is.null(input$muni)){
      if(input$US_mean){
        if(input$MA_mean){
          my_place <- c("United States", "MA", input$muni) ## US and MA  
        } else{
          my_place <- c("United States", input$muni) ## US only
        }
      } else{
        if(input$MA_mean){
          my_place <- c("MA", input$muni) ## US only ## MA only
        } else{
          my_place <- c(input$muni)
        }
      }
    }
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
    if(is.null(input$muni))
      my_place <- c("MA", "United States")
    if(!is.null(input$muni)){
      if(input$US_mean){
        if(input$MA_mean){
          my_place <- c("United States", "MA", input$muni) ## US and MA  
        } else{
          my_place <- c("United States", input$muni) ## US only
        }
      } else{
        if(input$MA_mean){
          my_place <- c("MA", input$muni) ## US only ## MA only
        } else{
          my_place <- c(input$muni)
        }
      }
    }
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
  
  vet_df <- reactive({
    if(is.null(input$muni))
      my_place <- c("MA", "United States")
    if(!is.null(input$muni)){
      if(input$US_mean){
        if(input$MA_mean){
          my_place <- c("United States", "MA", input$muni) ## US and MA  
        } else{
          my_place <- c("United States", input$muni) ## US only
        }
      } else{
        if(input$MA_mean){
          my_place <- c("MA", input$muni) ## US only ## MA only
        } else{
          my_place <- c(input$muni)
        }
      }
    }
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

##### RUN APP #####
shinyApp(
  ui = dashboardPage(header, sidebar, body, skin="red"),
  server = server 
)