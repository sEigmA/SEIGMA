################################
## Title: SEIGMA dashboard    ##
## Author: Zhenning Kang      ##
## Date Created:  09/27/2017  ##
## Last Modified: 11/07/2017  ##
################################

##### SETTINGS #####
source("global.R")

##### UI #####
header <- dashboardHeader(title = "SEIGMA Dashboard", disable = TRUE)

sidebar <- dashboardSidebar(
  sidebarMenu(
    h4("Select Your Interest"),
    menuItem("Municipality", icon = icon("address-book"),
             selectInput("muni", "Select Municipalities",
                         choices = MA_municipals,
                         selected = "Abington",
                         multiple = TRUE)
    ),
    menuItem("County/MA/US Average", icon = icon("check-square-o"),
             checkboxInput("CT_mean", "Compare to County Average", FALSE),
             checkboxInput("MA_mean", "Compare to MA Average", TRUE),
             checkboxInput("US_mean", "Compare to US Average", TRUE)
             ),
    br(),
    br(),
    h4("Select Data to Visualize"),
    menuItem("Demographics", tabName = "demo", icon = icon("street-view")
             ),
    menuItem("Social", tabName = "soci", icon = icon("users")
             ),
    menuItem("Economics", tabName = "econ", icon = icon("bank")
    ),
    br(),
    br(),
    br(),
    menuItem("Comments or Feedback", icon = icon("envelope-o"),
             href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation"),
    menuItem("Data Source", icon = icon("file-code-o"), 
             href="https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml"),
    menuItem("Codes on Github", icon = icon("code-fork"), 
             href = "https://github.com/sEigmA/SEIGMA/tree/gh-pages/dashboard"),
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
                  h4(helpText(a("More information about Demographics.", href="https://seigma.shinyapps.io/demographics/", target="_blank",onclick="ga('send', 'event', 'click', 'link', 'dem_app', 1)")))
              )
            ),
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
                  box(width = 11,
                      plotOutput("plot_age")
                      )
                ),
                actionButton("age_info", "What's in Age Group?"),
                downloadButton(outputId = "age_down", label = "Download the plot")
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
                  box(width = 11,
                      plotOutput("plot_rac")
                  )
                ),
                actionButton("rac_info", "What's in Race?"),
                downloadButton(outputId = "rac_down", label = "Download the plot")
              )
            ),
            fluidRow(
              box(width = 6,
                plotOutput("plot_gen"),
                actionButton("gen_info", "What is Gender?"),
                downloadButton(outputId = "gen_down", label = "Download the plot")
              ),
              box(width = 6,
                plotOutput("plot_his"),
                actionButton("his_info", "What is Ethnicity?"),
                downloadButton(outputId = "his_down", label = "Download the plot")
              )
            )
    ),

    tabItem(tabName = "soci",
            # Educational Attainment, Marital Status, Schools, Suicide Rates, Veteran Status
            fluidRow(
              box(width = 12,
                  a(img(src = "logo.jpg", height=105, width=920), href="http://www.umass.edu/seigma/")
              )
            ),
            fluidRow(
              box(width = 6,
                  fluidRow(
                    box(width = 11,
                        selectInput("status", "Choose a Status of Interest:",
                                    c("Married" = "married",
                                      "Separated" = "separated",
                                      "Divorced" = "divorced",
                                      "Widowed" = "widowed",
                                      "Never" = "never"),
                                    selected = "married",
                                    multiple = FALSE)
                    )
                  ),
                  fluidRow(
                    box(width = 11,
                        plotOutput("plot_mar")
                        )
                    ),
                  actionButton("mar_info", "What is marital status?"),
                  downloadButton(outputId = "mar_down", label = "Download the plot"),
                  h4(helpText(a("More information about Marital Status.",
                                href="https://seigma.shinyapps.io/marital/")))
              ),
              box(width = 6,
                    fluidRow(
                      box(width = 11,
                          selectInput(
                            "education", "Choose a Level of Interest:",
                            c("High School" = "hs",
                              "Bachelor" = "bac",
                              "Graduate" = "grad"),
                            selected = "hs",
                            multiple = FALSE)
                          )
                    ),
                    fluidRow(
                      box(width = 11,
                          plotOutput("plot_edu")
                          )
                      ),
                  actionButton("edu_info", "What is Educational Attainment Rates?"),
                  downloadButton(outputId = "edu_down", label = "Download the plot"),
                    h4(helpText(a("More information about  Educational Attainment.", href="https://seigma.shinyapps.io/educational_attainment/")))
                    )
              ),
            fluidRow(
                box(width = 6,
                    plotOutput("plot_sui"),
                    actionButton("sui_info", "What is Age-adjusted Suicide Rate?"),
                    downloadButton(outputId = "sui_down", label = "Download the plot"),
                    h4(helpText(a("More information about Suicide Rate.", href="https://seigma.shinyapps.io/suicide/", target="_blank",onclick="ga('send', 'event', 'click', 'link', 'sui_app', 1)")))
                      ),
                box(width = 6,
                  plotOutput("plot_vet"),
                  actionButton("vet_info", "What is Veteran’s Status?"),
                  downloadButton(outputId = "vet_down", label = "Download the plot"),
                  h4(helpText(a("More information about Veteran’s Status.", href="https://seigma.shinyapps.io/va_status/")))
                    )
              )
            # fluidRow(
            #   box(width = 12,
            #       plotOutput("plot_sch"),
            #       actionButton("sch_info", "What is Interest Groups?"),
            #       downloadButton(outputId = "sch_down", label = "Download the plot"),
            #       h4(helpText(a("More information about Schools.", href="https://seigma.shinyapps.io/schools/")))
            #   )
            # )
    ),
    tabItem(tabName = "econ",
            # Bankruptcy, Building Permits, Employment, Household Income, Poverty Rates, Property Tax, Property Value, Rent, Unemployment
            fluidRow(
              box(width = 12,
                  a(img(src = "logo.jpg", height=105, width=920), href="http://www.umass.edu/seigma/")
              )
            ),
            fluidRow(
              box(width = 6,
                  plotOutput("plot_inc"),
                  actionButton("inc_info", "What is Median Annual Household Income?"),
                  downloadButton(outputId = "inc_down", label = "Download the plot"),
                  h4(helpText(a("More information about Household Income.", href="https://seigma.shinyapps.io/income/")))
              ),
              box(width = 6,
                  plotOutput("plot_ren"),
                  actionButton("ren_info", "What is Inflation-Adjusted Median Rent?"),
                  downloadButton(outputId = "ren_down", label = "Download the plot"),
                  h4(helpText(a("More information about Rent.", href="https://seigma.shinyapps.io/rent/")))
              )
            ),
            fluidRow(
              box(width = 6,
                  plotOutput("plot_pov"),
                  actionButton("pov_info", "What is Poverty Status?"),
                  downloadButton(outputId = "pov_down", label = "Download the plot"),
                  h4(helpText(a("More information about Poverty.", href="https://seigma.shinyapps.io/poverty/")))
              ),
              box(width = 6,
                  plotOutput("plot_ban"),
                  actionButton("ban_info", "What is Bankruptcy?"),
                  downloadButton(outputId = "ban_down", label = "Download the plot"),
                  h4(helpText(a("More information about Bankruptcy.", href="https://seigma.shinyapps.io/bankruptcy")))
              )
            )
  #           fluidRow(
  #             box(width = 6,
  #                 plotOutput("plot_emp"),
  #                 actionButton("emp_info", "What is Employment?"),
  #                 downloadButton(outputId = "emp_down", label = "Download the plot"),
  #                 h4(helpText(a("More information about Employment.", href="https://seigma.shinyapps.io/employment/")))
  #             ),
  #             box(width = 6,
  #                 plotOutput("plot_bui"),
  #                 actionButton("bui_info", "What is Building Permits?"),
  #                 downloadButton(outputId = "bui_down", label = "Download the plot"),
  #                 h4(helpText(a("More information about Building Permits.", href="https://seigma.shinyapps.io//BuildingPermits/")))
  #             )
  #           ),
  #           fluidRow(
  #             box(width = 6,
  #                 plotOutput("plot_val"),
  #                 actionButton("val_info", "What is Total Assessed Property Values?"),
  #                 downloadButton(outputId = "pro_down", label = "Download the plot"),
  #                 h4(helpText(a("More information about Property Value.", href="https://seigma.shinyapps.io/PropertyValue/")))
  #             ),
  #             box(width = 6,
  #                 plotOutput("plot_tax"),
  #                 actionButton("tax_info", "What is Poverty Tax?"),
  #                 downloadButton(outputId = "tax_down", label = "Download the plot"),
  #                 h4(helpText(a("More information about Property Tax.", href="https://seigma.shinyapps.io/PropertyValue/")))
  #             )
  #           )
  )
  )
)

##### SERVER #####
server <- function(input, output, session){
  
  place <- reactive({
    if(is.null(input$muni))
      my_place <- c("MA", "United States")
    if(!is.null(input$muni)){
      if(input$US_mean){
        if(input$MA_mean){
          if(input$CT_mean){
            county <- c()
            for(i in 1:length(input$muni)){
              county[i] <- as.character(muni_county[muni_county$Municipal == input$muni[i],]$County)
            }
            my_place <- c("United States", "MA", input$muni, county) 
          } else{
            my_place <- c("United States", "MA", input$muni)
          }
        } else 
          if(input$CT_mean){
            county <- c()
            for(i in 1:length(input$muni)){
              county[i] <- as.character(muni_county[muni_county$Municipal == input$muni[i],]$County)
            }
            my_place <- c("United States", input$muni, county)
          } else{
            my_place <- c("United States", input$muni)
          }
      } else{
        if(input$MA_mean){
          if(input$CT_mean){
            county <- c()
            for(i in 1:length(input$muni)){
              county[i] <- as.character(muni_county[muni_county$Municipal == input$muni[i],]$County)
            }
            my_place <- c("MA", input$muni, county)
          } else{
            my_place <- c("MA", input$muni)
          }
        } else{
          if(input$CT_mean){
            county <- c()
            for(i in 1:length(input$muni)){
              county[i] <- as.character(muni_county[muni_county$Municipal == input$muni[i],]$County)
            }
            my_place <- c(input$muni, county)
          } else{
            my_place <- c(input$muni)
          }
        }
      }
    }
    my_place
  })
  
  observeEvent(input$age_info, {
    showNotification("AGE", age_pop)
  })
  
  observeEvent(input$rac_info, {
    showNotification("RACE", rac_pop)
  })
  
  observeEvent(input$gen_info, {
    showNotification("GENDER", gen_pop)
  })
  
  observeEvent(input$his_info, {
    showNotification("ETHNICITY", his_pop)
  })
  
  observeEvent(input$edu_info, {
    showNotification("Educational Attainment Rates ", edu_pop)
  })
  
  observeEvent(input$mar_info, {
    showNotification("Marital Status Rates", mar_pop)
  })
  
  observeEvent(input$sui_info, {
    showNotification("Age-adjusted Suicide Rate", sui_pop)
  })
  
  observeEvent(input$vet_info, {
    showNotification("Veteran Status", vet_pop)
  })
  
  observeEvent(input$inc_info, {
    showNotification("Median Annual Household Income", inc_pop)
  })

  observeEvent(input$ren_info, {
    showNotification("Inflation-Adjusted (2015 $) Median Contract Rent", ren_pop)
  })

  observeEvent(input$pov_info, {
    showNotification("Poverty Status", pov_pop)
  })
  
  observeEvent(input$ban_info, {
    showNotification("Bankruptcy", ban_pop)
  })
  
  observeEvent(input$emp_info, {
    showNotification("Employment", emp_pop)
  })
  
  observeEvent(input$bui_info, {
    showNotification("Building", bui_pop)
  })
  
  observeEvent(input$val_info, {
    showNotification("Total Assessed Property Values", val_pop)
  })
  
  observeEvent(input$tax_info, {
    showNotification("Poverty Tax", tax_pop)
  })
  
  age_df <- reactive({
    my_place <- place()
    muni_df <- filter(dem_data, Region %in% my_place) %>% select(Region, Age_under_20_Pct_plot, Age_20_34_Pct_plot, Age_35_54_Pct_plot, Age_55_64_Pct_plot, Age_65_74_Pct_plot, Age_over_75_Pct_plot, Year)
    muni_df <- melt(muni_df)
    muni_df$variable <- gsub("0_3", "0 to 3", muni_df$variable)
    muni_df$variable[1:70] <- gsub("5_", "5 to ", muni_df$variable[1:70])
    muni_df$variable <- gsub("_", " ", muni_df$variable)
    muni_df$variable <- gsub("Pct plot", "", muni_df$variable)
    muni_df$Year <- gsub("20", "'", muni_df$Year)
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
      theme(axis.title = element_text(face="bold", size=18)) +
      theme(axis.text=element_text(size=14)) + 
      theme(legend.text = element_text(size = 12))
    print(p) 
  })
  
  output$age_down <- downloadHandler(
    filename = function() {
      "plot_age.png"
    },
    content = function(file) {
      png(file)
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
        theme(axis.title = element_text(face="bold", size=18)) +
        theme(axis.text=element_text(size=14)) + 
        theme(legend.text = element_text(size = 12))
      print(p) 
      dev.off()
    }
  )
  
  rac_df <- reactive({
    my_place <- place()
    muni_df <- filter(dem_data, Region %in% my_place) %>% select(Region, White_Pct, Black_Pct, American_Indian_and_Alaska_Native_Pct, Asian_Pct, Hawaiian_and_Other_Pacific_Islander_Pct, Others_Pct, Year)
    muni_df <- melt(muni_df)
    muni_df$variable <- gsub("_Pct", "", muni_df$variable)
    muni_df$variable <- gsub("_", " ", muni_df$variable)
    muni_df$Year <- gsub("20", "'", muni_df$Year)
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
      theme(axis.title = element_text(face="bold", size=18)) +
      theme(axis.text=element_text(size=14)) + 
      theme(legend.text = element_text(size = 12))
    print(p) 
  })
  
  output$rac_down <- downloadHandler(
    filename = function() {
      "plot_race.png"
    },
    content = function(file) {
      png(file)
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
        theme(axis.title = element_text(face="bold", size=18)) +
        theme(axis.text=element_text(size=14)) + 
        theme(legend.text = element_text(size = 12))
      print(p) 
      dev.off()
    }
  )
  
  gen_df <- reactive({
    my_place <- place()
    muni_df <- filter(dem_data, Region %in% my_place) %>% select(Region, Male_Pct, Female_Pct, Year)
    colnames(muni_df) <- gsub("_Pct", "", colnames(muni_df))
    muni_df <- melt(muni_df)
    muni_df$Year <- gsub("20", "'", muni_df$Year)
    muni_df
  })
  
  output$plot_gen <- renderPlot({
    dat <- gen_df() 
    theme_set(theme_classic())
    p<- ggplot(dat, aes(x=Year, y=value, group = interaction(Region,variable), colour = Region, label = value)) +
      geom_line() + 
      geom_point() + 
      facet_grid(. ~ variable) + 
      labs(title = "Gender Distribution", 
           x = "Mid-Year of Five Year Range",
           y = "% Population") + 
      theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
      theme(axis.title = element_text(face="bold", size=18)) +
      theme(axis.text=element_text(size=14)) + 
      theme(legend.text = element_text(size = 12)) 
    print(p) 
  })
  
  output$gen_down <- downloadHandler(
    filename = function() {
      "plot_gender.png"
    },
    content = function(file) {
      png(file)
      dat <- gen_df() 
      theme_set(theme_classic())
      p<- ggplot(dat, aes(x=Year, y=value, group = interaction(Region,variable), colour = Region, label = value)) +
        geom_line() + 
        geom_point() + 
        facet_grid(. ~ variable) + 
        labs(title = "Gender Distribution", 
             x = "Mid-Year of Five Year Range",
             y = "% Population") + 
        theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
        theme(axis.title = element_text(face="bold", size=18)) +
        theme(axis.text=element_text(size=14)) + 
        theme(legend.text = element_text(size = 12)) 
      print(p) 
      dev.off()
    }
  )
  
  his_df <- reactive({
    my_place <- place()
    muni_df <- filter(dem_data, Region %in% my_place) %>% select(Region, Hispanic_Pct, Not_Hispanic_Pct, Year)
    colnames(muni_df) <- gsub("_Pct", "", colnames(muni_df))
    colnames(muni_df) <- gsub("_", " ", colnames(muni_df))
    muni_df <- melt(muni_df)
    muni_df$Year <- gsub("20", "'", muni_df$Year)
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
      theme(axis.title = element_text(face="bold", size=18)) +
      theme(axis.text=element_text(size=14)) + 
      theme(legend.text = element_text(size = 12))
    print(p) 
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
        theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
        theme(axis.title = element_text(face="bold", size=18)) +
        theme(axis.text=element_text(size=14)) + 
        theme(legend.text = element_text(size = 12))
      print(p) 
      dev.off()
    }
  )
  
  edu_df <- reactive({
    my_place <- place()
    muni_df <- filter(edu_data, Region %in% my_place) %>% select(Region, HS_Pct, Bachelors_Pct, Grad_Pct, Year)
    muni_df <- melt(muni_df)
    muni_df$variable <- gsub("_Pct", " %", muni_df$variable)
    muni_df$variable <- gsub("HS", "High School", muni_df$variable)
    muni_df$Year <- gsub("20", "'", muni_df$Year)
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
      theme(axis.title = element_text(face="bold", size=18)) +
      theme(axis.text=element_text(size=14)) + 
      theme(legend.text = element_text(size = 12))
    print(p) 
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
        theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
        theme(axis.title = element_text(face="bold", size=18)) +
        theme(axis.text=element_text(size=14)) + 
        theme(legend.text = element_text(size = 12))
      print(p) 
      dev.off()
    }
  )
  
  mar_df <- reactive({
    my_place <- place()
    muni_df <- filter(mar_data, Region %in% my_place) %>% select(Region, Never_Married_pct, Now_Married_pct, Separated_pct, Widowed_pct, Divorced_pct, Gender, Year)
    names(muni_df) <- gsub("_", " ", names(muni_df))
    names(muni_df) <- gsub("pct", "%", names(muni_df))
    muni_df$Year <- gsub("20", "'", muni_df$Year)
    muni_df
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
      theme(axis.title = element_text(face="bold", size=18)) +
      theme(axis.text=element_text(size=14)) + 
      theme(legend.text = element_text(size = 12))
    print(p) 
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
        theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
        theme(axis.title = element_text(face="bold", size=18)) +
        theme(axis.text=element_text(size=14)) + 
        theme(legend.text = element_text(size = 12))
      print(p) 
      dev.off()
    }
  )
  
  vet_df <- reactive({
    my_place <- place()
    muni_df <- filter(vet_data, Region %in% my_place) %>% select(Region, Percent_Vet, Year)
    muni_df <- melt(muni_df)
    muni_df$Year <- gsub("20", "'", muni_df$Year)
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
      theme(axis.title = element_text(face="bold", size=18)) +
      theme(axis.text=element_text(size=14)) + 
      theme(legend.text = element_text(size = 12))
    print(p)  
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
        theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
        theme(axis.title = element_text(face="bold", size=18)) +
        theme(axis.text=element_text(size=14)) + 
        theme(legend.text = element_text(size = 12))
      print(p)  
      dev.off()
    }
  )
  
  sui_df <- reactive({
    if(is.null(input$muni))
      my_place <- c("MA", "United States")
    if(!is.null(input$muni)){
      county <- c()
      for (m in 1:length(input$muni)){
        county[m] <- as.character(muni_county$County[muni_county$Municipal==input$muni[m]])
      }
      county <- gsub(" County", "", county)
      if(input$US_mean){
        if(input$MA_mean){
          my_place <-  c(county, "MA", "United States")
        } else{
          my_place <-  c(county, "United States")
        }
      } else{
        if(input$MA_mean){
          my_place <-  c(county, "MA")
        } else{
          my_place <-  c(county)
        }
      }
    }
    muni_df <- filter(sui_data, Region %in% my_place) %>% select(Region, Age.Adjusted.Rate, Year)
    muni_df$Year <- gsub("20", "'", muni_df$Year)
    muni_df <- muni_df[muni_df$Year!="1999",]
    muni_df
  })
  
  output$plot_sui <- renderPlot({
    dat <- sui_df() 
    theme_set(theme_classic())
    p <- ggplot(dat, aes(x=Year, y=Age.Adjusted.Rate, group = Region, colour = Region)) + 
      geom_line() + 
      geom_point() + 
      labs(title = "Suicide Rate ", 
           x = "One Year Estimates",
           y = "% Population") + 
      theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
      theme(axis.title = element_text(face="bold", size=18)) +
      theme(axis.text=element_text(size=14)) + 
      theme(legend.text = element_text(size = 12))
    print(p)  
  })
  
  output$sui_down <- downloadHandler(
    filename = function() {
      "plot_suicide.png"
    },
    content = function(file) {
      png(file)
      dat <- sui_df() 
      theme_set(theme_classic())
      p <- ggplot(dat, aes(x=Year, y=Age.Adjusted.Rate, group = Region, colour = Region)) + 
        geom_line() + 
        geom_point() + 
        labs(title = "Suicide Rate ", 
             x = "One Year Estimates",
             y = "% Population") + 
        theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
        theme(axis.title = element_text(face="bold", size=18)) +
        theme(axis.text=element_text(size=14)) + 
        theme(legend.text = element_text(size = 12))
      print(p) 
      dev.off()
    }
  )
  
  inc_df <- reactive({
    my_place <- place()
    muni_df <- filter(inc_data, Region %in% my_place) %>% select(Region, Median_Annual_Household_Income, Five_Year_Average)
    muni_df$Year <- gsub("20", "'", muni_df$Five_Year_Average)
    muni_df
  })
  
  output$plot_inc <- renderPlot({
    dat <- inc_df() 
    theme_set(theme_classic())
    p <- ggplot(dat, aes(x=Year, y=Median_Annual_Household_Income, group = Region, colour = Region)) + 
      geom_line() + 
      geom_point() + 
      labs(title = "Median Annual Household Income", 
           x = "Mid-Year of Five Year Range",
           y = "Dollars") + 
      theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
      theme(axis.title = element_text(face="bold", size=18)) +
      theme(axis.text=element_text(size=14)) + 
      theme(legend.text = element_text(size = 12))
    print(p)  
  })
  
  output$inc_down <- downloadHandler(
    filename = function() {
      "plot_income.png"
    },
    content = function(file) {
      png(file)
      dat <- inc_df() 
      theme_set(theme_classic())
      p <- ggplot(dat, aes(x=Year, y=Median_Annual_Household_Income, group = Region, colour = Region)) + 
        geom_line() + 
        geom_point() + 
        labs(title = "Median Annual Household Income", 
             x = "Mid-Year of Five Year Range",
             y = "Dollars") + 
        theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
        theme(axis.title = element_text(face="bold", size=18)) +
        theme(axis.text=element_text(size=14)) + 
        theme(legend.text = element_text(size = 12))
      print(p)  
      dev.off()
    }
  )
  
  ren_df <- reactive({
    my_place <- place()
    muni_df <- filter(ren_data, Region %in% my_place) %>% select(Region, Median.Rent.2015.Dollar, Year)
    muni_df$Year <- gsub("20", "'", muni_df$Year)
    muni_df   
  })
  
  output$plot_ren <- renderPlot({
    dat <- ren_df() 
    theme_set(theme_classic())
    p <- ggplot(dat, aes(x=Year, y=Median.Rent.2015.Dollar, group = Region, colour = Region)) + 
      geom_line() + 
      geom_point() + 
      labs(title = "Inflation-Adjusted (2015 $) Median Rent", 
           x = "Mid-Year of Five Year Range",
           y = "Dollars") + 
      theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
      theme(axis.title = element_text(face="bold", size=18)) +
      theme(axis.text=element_text(size=14)) + 
      theme(legend.text = element_text(size = 12))
    print(p)  
  })
  
  pov_df <- reactive({
    my_place <- place()
    muni_df <- filter(pov_data, Region %in% my_place) %>% select(Region, Percent_Pov, Year)
    muni_df$Year <- gsub("20", "'", muni_df$Year)
    muni_df
  })
  
  output$plot_pov <- renderPlot({
    dat <- pov_df() 
    theme_set(theme_classic())
    p <- ggplot(dat, aes(x=Year, y=Percent_Pov, group = Region, colour = Region)) + 
      geom_line() + 
      geom_point() + 
      labs(title = "Poverty Rate", 
           x = "Mid-Year of Five Year Range",
           y = "% Population") + 
      theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
      theme(axis.title = element_text(face="bold", size=18)) +
      theme(axis.text=element_text(size=14)) + 
      theme(legend.text = element_text(size = 12))
    print(p)  
  })
  
  output$pov_down <- downloadHandler(
    filename = function() {
      "plot_poverty.png"
    },
    content = function(file) {
      png(file)
      dat <- pov_df() 
      theme_set(theme_classic())
      p <- ggplot(dat, aes(x=Year, y=Percent_Pov, group = Region, colour = Region)) + 
        geom_line() + 
        geom_point() + 
        labs(title = "Poverty Rate", 
             x = "Mid-Year of Five Year Range",
             y = "% Population") + 
        theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
        theme(axis.title = element_text(face="bold", size=18)) +
        theme(axis.text=element_text(size=14)) + 
        theme(legend.text = element_text(size = 12))
      print(p)  
      dev.off()
    }
  )
  
  ban_df <- reactive({
    if(is.null(input$muni))
      my_place <- c("MA", "United States")
    if(!is.null(input$muni)){
      county <- c()
      for (m in 1:length(input$muni)){
        county[m] <- as.character(muni_county$County[muni_county$Municipal==input$muni[m]])
      }
      if(input$US_mean){
        if(input$MA_mean){
          my_place <-  c(county, "MA", "United States")
        } else{
          my_place <-  c(county, "United States")
        }
      } else{
        if(input$MA_mean){
          my_place <-  c(county, "MA")
        } else{
          my_place <-  c(county)
        }
      }
    }
    muni_df <- filter(ban_data, Region %in% my_place) %>% select(Region, Business_Filings_Total, Personal_Filings_Total, Year)
    colnames(muni_df) <- gsub("_Filings_Total", "", colnames(muni_df))
    muni_df$Year <- as.factor(muni_df$Year)
    muni_df <- melt(muni_df)
    muni_df$Year <- gsub("20", "'", muni_df$Year)
    muni_df
  })
  
  output$plot_ban <- renderPlot({
    dat <- ban_df() 
    theme_set(theme_classic())
    p <- ggplot(dat, aes(x=Year, y=value, group = interaction(Region,variable), colour = Region, label = value)) + 
      geom_line() + 
      geom_point() + 
      facet_grid(. ~ variable) + 
      labs(title = "Bankruptcy Fillings", 
           x = "",
           y = "Count") + 
      theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
      theme(axis.title = element_text(face="bold", size=18)) +
      theme(axis.text=element_text(size=14)) + 
      theme(legend.text = element_text(size = 12))
    print(p)  
  })
 
  output$ban_down <- downloadHandler(
    filename = function() {
      "plot_bankruptcy.png"
    },
    content = function(file) {
      png(file)
      dat <- ban_df() 
      theme_set(theme_classic())
      p <- ggplot(dat, aes(x=Year, y=value, group = interaction(Region,variable), colour = Region, label = value)) + 
        geom_line() + 
        geom_point() + 
        facet_grid(. ~ variable) + 
        labs(title = "Bankruptcy Fillings", 
             x = "",
             y = "Count") + 
        theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
        theme(axis.title = element_text(face="bold", size=18)) +
        theme(axis.text=element_text(size=14)) + 
        theme(legend.text = element_text(size = 12))
      print(p)  
      dev.off()
    }
  )
   
}

##### RUN APP #####
shinyApp(
  ui = dashboardPage(header, sidebar, body, skin="red"),
  server = server 
)