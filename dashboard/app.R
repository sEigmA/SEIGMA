################################
## Title: SEIGMA dashboard    ##
## Author: Zhenning Kang      ##
## Date Created:  09/27/2017  ##
## Last Modified: 04/30/2018  ##
################################

##### SETTINGS #####
source("global.R")

##### UI #####
header <- dashboardHeader(title = "MASS-AT-A-GLANCE", disable = TRUE)

sidebar <- dashboardSidebar(
  sidebarMenu(
    fluidpage(
    downloadButton("report", "Generate Report"),
      ),
    actionButton("show", "How to use this application:"),
    br(),
    h4("Select Municipality"),
    menuItem("Municipality", icon = icon("address-book"),
             selectInput("muni", "Select Municipalities",
                         choices = MA_municipals,
                         selected = "Plainville",
                         multiple = TRUE)
    ),
    menuItem("County/MA/US Average", icon = icon("check-square-o"),
             checkboxInput("CT_mean", "Compare to County Average", TRUE),
             checkboxInput("MA_mean", "Compare to MA Average", TRUE),
             checkboxInput("US_mean", "Compare to US Average", FALSE)
             ),
    br(),
    h4("Select Data to Visualize"),
    menuItem("Demographics", tabName = "demo", icon = icon("street-view")
             ),
    menuItem("Social", tabName = "soci", icon = icon("users")
             ),
    menuItem("Economic", tabName = "econ", icon = icon("bank")
    ),
    br(),
    h4("Demo Data at a Glance"),
    column(5,
           actionButton("age_button", "Age Group")
           ),
    column(6,
           actionButton("rac_button", "Race Group")
           ),
    column(5,
           actionButton("gen_button", "Gender")
           ),
    column(6,
           actionButton("his_button", "Ethnicity")
           ),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    menuItem("Full Individual Apps", icon = icon("search"),
             menuSubItem("Demographics",
                         href="https://seigma.shinyapps.io/demographics/"),
             menuSubItem("Marital Status",
                         href="https://seigma.shinyapps.io/marital_status/"),
             menuSubItem("Educational Attainment",
                         href="https://seigma.shinyapps.io/educational_attainment/"),
             menuSubItem("Veterans Status",
                         href="https://seigma.shinyapps.io/va_status/"),
             menuSubItem("Suicide",
                         href="https://seigma.shinyapps.io/suicide/"),
             menuSubItem("School",
                         href="https://seigma.shinyapps.io/schools/"),
             menuSubItem("Household Income",
                         href="https://seigma.shinyapps.io/income/"),
             menuSubItem("Poverty",
                         href="https://seigma.shinyapps.io/poverty/"),
             menuSubItem("Employment",
                         href="https://seigma.shinyapps.io/employment/"),
             menuSubItem("Unemployment",
                         href="https://seigma.shinyapps.io/unemployment/"),
             menuSubItem("Bankruptcy",
                         href="https://seigma.shinyapps.io/bankruptcy/"),
             menuSubItem("Rent",
                         href="https://seigma.shinyapps.io/rent/"),
             menuSubItem("Building Permits",
                         href="https://seigma.shinyapps.io/BuildingPermits/"),
             menuSubItem("Property Value",
                         href="https://seigma.shinyapps.io/PropertyValue/"),
             menuSubItem("Property Tax",
                         href="https://seigma.shinyapps.io/PropertyTax/")
             ),
    menuItem("Data Source", icon = icon("file-code-o"), 
             menuSubItem("American Community Survey",  
                         href="https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml", 
                         newtab = TRUE),
             menuSubItem("CDC Wonder",  
                         href="https://wonder.cdc.gov/mortsql.html", 
                         newtab = TRUE),
             menuSubItem("MA Labor and Workforce",  
                         href="http://lmi2.detma.org/lmi/lmi_es_a.asp", 
                         newtab = TRUE),
             menuSubItem("Bureau of Labor Statistics",  
                         href="https://www.bls.gov/lau/data.htm",
                         newtab = TRUE),
             menuSubItem("United States Courts",  
                         href="http://www.uscourts.gov/statistics-reports/caseload-statistics-data-tables"),
             menuSubItem("MA State Data Center",  
                         href="http://www.massbenchmarks.org/statedata/data.htm", 
                         newtab = TRUE),
             menuSubItem("MA DOR",  
                         href="https://dlsgateway.dor.state.ma.us/reports/rdPage.aspx?rdReport=PropertyTaxInformation.taxratesbyclass.taxratesbyclass_main",
                         newtab = TRUE)
),
    menuItem("Comments or Feedback", icon = icon("envelope-o"),
             href="http://www.surveygizmo.com/s3/1832220/ShinyApp-Evaluation"),
    menuItem("Codes on Github", icon = icon("code-fork"), 
             href = "https://github.com/sEigmA/SEIGMA/tree/gh-pages/dashboard"),
    menuItem(
      helpText("Created by Zhenning Kang")
    )
  )
)

body <- dashboardBody(
  tags$head(includeScript("google-analytics.js")),
  bsModal("modal1Example", "Age Distribution", "age_button", size = "large",plotOutput("age_show")),
  bsModal("modal2Example", "Race Distribution", "rac_button", size = "large",plotOutput("rac_show")),
  bsModal("modal3Example", "Gender Distribution", "gen_button", size = "large",plotOutput("gen_show")),
  bsModal("modal4Example", "Ethnicity Distribution", "his_button", size = "large",plotOutput("his_show")),
  tabItems(
    tabItem(
      ### Demographic Tab UI ###
      tabName = "demo",
            fluidRow(
              box(width = 12,
                  a(img(src = "logo.jpg", height=105, width=920), href="http://www.umass.edu/seigma/", target = "_blank")
                  )
            ),
      fluidRow(
        # box(width = 6,
        #     column(6,
        #            radioButtons('format', 'Select a Document Format', c('PDF', 'HTML', 'Word'), inline = TRUE, selected = NA)
        #            ),
        #     column(6,
        #            downloadButton('downloadReport', 'Generate Demo Report')
        #            )),
        box(width = 12,
            h5("Population estimates can be found at the bottom of the page."))
      ),
            fluidRow(
              box(width = 6,
                fluidRow(
                  box(width = 12,
                      h4("Please Select an Age of Interest"),
                      column(width = 4,
                             checkboxInput("under20", "Age Under 20 ", FALSE),
                             checkboxInput("under35", "Age Under 35", TRUE),
                             checkboxInput("under65", "Age Under 65", FALSE)
                             ),
                      column(width = 4,
                             checkboxInput("under34", "Age 20 to 34 ", FALSE),
                             checkboxInput("under54", "Age 35 to 54 ", FALSE),
                             checkboxInput("under64", "Age 55 to 64 ", FALSE)
                             ),
                      column(width = 4,
                             checkboxInput("under74", "Age 65 to 74 ", FALSE),
                             checkboxInput("over75", "Age Over 75 ", FALSE)
                             )
                      )
                  ),
                fluidRow(
                  box(width = 12,
                      plotOutput("plot_age", click = "age_click"),
                      verbatimTextOutput("age_point")
                      )
                ),
                actionButton("age_info", "What is the Age variable?"),
                downloadButton(outputId = "age_down", label = "Download the plot")
              ),
              box(width = 6,
                fluidRow(
                  box(width = 12,
                      h4("Please Select a Race of Interest"),
                      column(width = 9,
                             checkboxInput("white", "White", TRUE),
                             checkboxInput("hawaiian", "Hawaiian and Other Pacific Islander (HOPI)", FALSE),
                             checkboxInput("native", "American Indian and Alaska Native (AIAN)", FALSE)
                             ),
                      column(width = 3,
                             checkboxInput("black", "Black", FALSE),
                             checkboxInput("asian", "Asian", FALSE),
                             checkboxInput("others", "Others", FALSE)
                      )
                  )
                ),
                fluidRow(
                  box(width = 12,
                      plotOutput("plot_rac", click = "rac_click"),
                      verbatimTextOutput("rac_point")
                  )
                ),
                actionButton("rac_info", "What is the Race variable?"),
                downloadButton(outputId = "rac_down", label = "Download the plot")
              )
            ),
            fluidRow(
              box(width = 6,
                  plotOutput("plot_gen", click = "gen_click"),
                  verbatimTextOutput("gen_point"),
                actionButton("gen_info", "What is the Gender variable?"),
                downloadButton(outputId = "gen_down", label = "Download the plot")
              ),
              box(width = 6,
                  plotOutput("plot_his", click = "his_click"),
                  verbatimTextOutput("his_point"),
                actionButton("his_info", "What is the Ethnicity variable?"),
                downloadButton(outputId = "his_down", label = "Download the plot")
              )
            ),
      fluidRow(
        box(width = 12,
            h4("Total Population in Selected Regions (Five Year Range)"),
            dataTableOutput("population")
        )
      ),
            fluidRow(
              box(width = 12,
                  h4(helpText(a("More information about Demographics.", href="https://seigma.shinyapps.io/demographics/", target="_blank",onclick="ga('send', 'event', 'click', 'link', 'dem_app', 1)")))
              )
            )
    ),
    ### Social Tab UI ###
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
                    box(width = 12,
                        h4("Please Select a Status of Interest"),
                        column(width = 4,
                               checkboxInput("married", "Now Married", FALSE),
                               checkboxInput("widowed", "Widowed", FALSE)
                        ),
                        column(width = 4,
                               checkboxInput("separated", "Seperated", FALSE),
                               checkboxInput("never", "Never Married", FALSE)
                               ),
                        column(width = 4,
                               checkboxInput("divorced", "Divorced", TRUE)
                        )
                    )
                  ),
                  fluidRow(
                    box(width = 12,
                        plotOutput("plot_mar", click = "mar_click"),
                        verbatimTextOutput("mar_point")
                        )
                    ),
                  actionButton("mar_info", "What is the Marital Status variable?"),
                  downloadButton(outputId = "mar_down", label = "Download the plot"),
                  h4(helpText(a("More information about Marital Status.",
                                href="https://seigma.shinyapps.io/marital_status/", target="_blank")))
              ),
              box(width = 6,
                    fluidRow(
                      box(width = 12,
                          h4("Please Select a Level of Interest"),
                          column(width = 6,
                                 checkboxInput("nohs", "Less Than High School", FALSE),
                                 checkboxInput("hs", "At Least High School", TRUE)
                          ),
                          column(width = 6,
                                 checkboxInput("bac", "At Least Bachelor", FALSE),
                                 checkboxInput("grad", "At Least Graduate", FALSE)
                          )
                      )
                    ),
                    fluidRow(
                      box(width = 12,
                          plotOutput("plot_edu", click = "edu_click"),
                          verbatimTextOutput("edu_point")
                          )
                      ),
                  actionButton("edu_info", "What is the Educational Attainment variable?"),
                  downloadButton(outputId = "edu_down", label = "Download the plot"),
                    h4(helpText(a("More information about  Educational Attainment.", href="https://seigma.shinyapps.io/educational_attainment/", target="_blank")))
                    )
              ),
            fluidRow(
                box(width = 6,
                    plotOutput("plot_vet", click = "vet_click"),
                    verbatimTextOutput("vet_point"),
                  actionButton("vet_info", "What is the Veterans Status variable?"),
                  downloadButton(outputId = "vet_down", label = "Download the plot"),
                  h4(helpText(a("More information about Veterans Status.", href="https://seigma.shinyapps.io/va_status/", target="_blank")))
                    ),
                box(width = 6,
                    plotOutput("plot_sui", click = "sui_click"),
                    verbatimTextOutput("sui_point"),
                    actionButton("sui_info", "What is the Age-adjusted Suicide Rate variable?"),
                    downloadButton(outputId = "sui_down", label = "Download the plot"),
                    h4(helpText(a("More information about Suicide Rate.", href="https://seigma.shinyapps.io/suicide/", target="_blank",onclick="ga('send', 'event', 'click', 'link', 'sui_app', 1)")))
                )
              ),
            fluidRow(
              box(width = 6,
                  plotOutput("plot_eng", click = "eng_click"),
                  verbatimTextOutput("eng_point"),
                  actionButton("eng_info", "What is the English Language Learners variable?"),
                  downloadButton(outputId = "eng_down", label = "Download the plot"),
                  h4(helpText(a("More information about Schools.", href="https://seigma.shinyapps.io/schools/", target="_blank",onclick="ga('send', 'event', 'click', 'link', 'sui_app', 1)")))
              ),
              box(width = 6,
                  plotOutput("plot_dis", click = "dis_click"),
                  verbatimTextOutput("dis_point"),
                  actionButton("dis_info", "What is the Students with Disabilities variable?"),
                  downloadButton(outputId = "dis_down", label = "Download the plot"),
                  h4(helpText(a("More information about Schools.", href="https://seigma.shinyapps.io/va_status/"), target = "_blank"))
              )
            )
    ),
    ### Economics Tab UI ###
    tabItem(tabName = "econ",
            # Bankruptcy, Building Permits, Employment, Household Income, Poverty Rates, Property Tax, Property Value, Rent, Unemployment
            fluidRow(
              box(width = 12,
                  a(img(src = "logo.jpg", height=105, width=920), href="http://www.umass.edu/seigma/")
              )
            ),
            fluidRow(
              box(width = 6,
                  plotOutput("plot_inc", click = "inc_click"),
                  verbatimTextOutput("inc_point"),
                  actionButton("inc_info", "What is the Median Annual Household Income variable?"),
                  downloadButton(outputId = "inc_down", label = "Download the plot"),
                  h4(helpText(a("More information about Household Income.", href="https://seigma.shinyapps.io/income/", target="_blank")))
              ),
              box(width = 6,
                  plotOutput("plot_pov", click = "pov_click"),
                  verbatimTextOutput("pov_point"),
                  actionButton("pov_info", "What is the Poverty Status variable?"),
                  downloadButton(outputId = "pov_down", label = "Download the plot"),
                  h4(helpText(a("More information about Poverty.", href="https://seigma.shinyapps.io/poverty/", target="_blank")))
              )
            ),
            fluidRow(
              box(width = 6,
                  plotOutput("plot_emp", click = "emp_click"),
                  verbatimTextOutput("emp_point"),
                  actionButton("emp_info", "What is the Monthly Employment variable?"),
                  downloadButton(outputId = "emp_down", label = "Download the plot"),
                  h4(helpText(a("More information about Employment.", href="https://seigma.shinyapps.io/employment/", target="_blank")))
              ),
              box(width = 6,
                  plotOutput("plot_une", click = "une_click"),
                  verbatimTextOutput("une_point"),
                  actionButton("une_info", "What is the Unemployment Rate variable?"),
                  downloadButton(outputId = "une_down", label = "Download the plot"),
                  h4(helpText(a("More information about Unemployment.", href="https://seigma.shinyapps.io/unemployment/", target="_blank")))
              )
            ),
            fluidRow(
              box(width = 6,
                  box(width = 12,
                      h4("Please Select a Chapter of Interest"),
                      column(3,
                             checkboxInput("buschp7", "Chapter 7", TRUE)
                      ),
                      column(3,
                             checkboxInput("buschp11", "Chapter 11", FALSE)
                      ),
                      column(3,
                             checkboxInput("buschp12", "Chapter 12", FALSE)
                      ),
                      column(3,
                             checkboxInput("buschp13", "Chapter 13", FALSE)
                      )
                      ),
                  fluidRow(
                    box(width = 12,
                        plotOutput("plot_bus", click = "bus_click"),
                        verbatimTextOutput("bus_point"),
                        actionButton("bus_info", "What is the Business Bankruptcy variable?"),
                        downloadButton(outputId = "bus_down", label = "Download the plot"),
                        h4(helpText(a("More information about Bankruptcy.", href="https://seigma.shinyapps.io/bankruptcy", target="_blank")))
                    ) 
                  )
                  ),
              box(width = 6,
                  box(width = 12,
                      h4("Please Select a Chapter of Interest"),
                      column(4,
                             checkboxInput("perchp7", "Chapter 7", TRUE)
                      ),
                      column(4,
                             checkboxInput("perchp11", "Chapter 11", FALSE)
                      ),
                      column(4,
                             checkboxInput("perchp13", "Chapter 13", FALSE)
                      )
                      ),
                  fluidRow(
                    box(width = 12,
                        plotOutput("plot_per", click = "per_click"),
                        verbatimTextOutput("per_point"),
                        actionButton("per_info", "What is the Personal Bankruptcy variable?"),
                        downloadButton(outputId = "per_down", label = "Download the plot"),
                        h4(helpText(a("More information about Bankruptcy.", href="https://seigma.shinyapps.io/bankruptcy", target="_blank")))
                    )

                    )
                  )
              ),
            fluidRow(
              box(width = 6,
                  plotOutput("plot_ren", click = "ren_click"),
                  verbatimTextOutput("ren_point"),
                  actionButton("ren_info", "What is the Inflation-Adjusted Median Rent variable?"),
                  downloadButton(outputId = "ren_down", label = "Download the plot"),
                  h4(helpText(a("More information about Rent.", href="https://seigma.shinyapps.io/rent/", target="_blank")))
              ),
              box(width = 6,
                  plotOutput("plot_bui", click = "bui_click"),
                  verbatimTextOutput("bui_point"),
                  actionButton("bui_info", "What is the Building Permits variable?"),
                  downloadButton(outputId = "bui_down", label = "Download the plot"),
                  h4(helpText(a("More information about Building Permits.", href="https://seigma.shinyapps.io/BuildingPermits/", target="_blank")))
              )
            ),
            fluidRow(
              box(width = 6,
                  box(width = 12,
                      h4("Please Select a Class of Interest"),
                      column(3,
                             checkboxInput("resval", "Residenial", TRUE)
                      ),
                      column(3,
                             checkboxInput("comval", "Commercial", FALSE)
                      ),
                      column(3,
                             checkboxInput("indval", "Industrial", FALSE)
                      ),
                      column(3,
                             checkboxInput("perval", "Personal", FALSE)
                      )
                  ),
                  fluidRow(
                    box(width = 12,
                        plotOutput("plot_val", click = "val_click"),
                        verbatimTextOutput("val_point"),
                  actionButton("val_info", "What is the Total Assessed Property Values variable?"),
                  downloadButton(outputId = "pro_down", label = "Download the plot"),
                  h4(helpText(a("More information about Property Value.", href="https://seigma.shinyapps.io/PropertyValue/", target="_blank")))
                    )
              )
              ),
              box(width = 6,
                  box(width = 12,
                      h4("Please Select a Class of Interest"),
                      column(3,
                             checkboxInput("restax", "Residenial", TRUE)
                      ),
                      column(3,
                             checkboxInput("comtax", "Commercial", FALSE)
                      ),
                      column(3,
                             checkboxInput("indtax", "Industrial", FALSE)
                      ),
                      column(3,
                             checkboxInput("pertax", "Personal", FALSE)
                      )
                  ),
                  fluidRow(
                    box(width = 12,
                        plotOutput("plot_tax", click = "tax_click"),
                        verbatimTextOutput("tax_point"),
                  actionButton("tax_info", "What is the Tax Levy variable?"),
                  downloadButton(outputId = "tax_down", label = "Download the plot"),
                  h4(helpText(a("More information about Tax Levy.", href="https://seigma.shinyapps.io/PropertyTax/", target="_blank")))
              )
            )
              )
            )
  )
  )
)

##### SERVER #####
server <- function(input, output, session){
  output$report <- downloadHandler(
    filename="report.pdf",
    content=function(file){
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite=TRUE)
      
      params <- list(n = input$slider)
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent=globalenv())
          )
  
  ##### Instruction Button #####
  observeEvent(input$show, {
    showModal(modalDialog(
      title = "How to use this application:",
      h4("1. Select Municipality of Interest"),
      "Please choose a  municipality by entering it in the text box or selecting it from the pull-down menu on the left sidebar.",
      br(),
      "- Municipalities are listed in alphabetical order.",
      br(),
      "- Multiple municipalities can be selected at the same time.",
      br(),
      "- Use 'backspace' on the keyboard to delete an existing municipality.",
      br(),
      h4("2. Choose a Comparison"),
      "Please select the corresponding check box to compare the County, Massachusetts, or United States average.",
      br(),
      h4("3. Select Data to Visualize"),
      "Please select the data of interest by choosing a variable category on the left sidebar.",
      br(),
      br(),
      "- Demographics: Age, Race, Gender, Ethnicity, and Population data.", 
      br(),
      br(),
      "- Social: Marital Status, Educational Attainment, Suicide Rate, Veterans Status, and Schools data.",
      br(),
      br(),
      "- Economic: Household Income, Poverty Rate, Monthly Employment, Unemployment Rate, Business and Personal Bankruptcy, Monthly Rent, Building Permits, Property Values, and Tax Levy data.",
      br(),
      br(),
      "Data with various categories can be selected through check boxes located at the top of the plot.",
      br(),
      br(),
      "Links to full applications for a particular dataset are indicated below each plot as “More information” button.",
      footer = modalButton("Close"),
      easyClose = TRUE
    ))
  })
  
  ##### GLOBAL FILTER #####
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
            my_place <- c(input$muni, county, "MA", "United States") 
          } else{
            my_place <- c(input$muni, "MA", "United States")
          }
        } else 
          if(input$CT_mean){
            county <- c()
            for(i in 1:length(input$muni)){
              county[i] <- as.character(muni_county[muni_county$Municipal == input$muni[i],]$County)
            }
            my_place <- c(input$muni, county, "United States")
          } else{
            my_place <- c(input$muni, "United States")
          }
      } else{
        if(input$MA_mean){
          if(input$CT_mean){
            county <- c()
            for(i in 1:length(input$muni)){
              county[i] <- as.character(muni_county[muni_county$Municipal == input$muni[i],]$County)
            }
            my_place <- c(input$muni, county, "MA")
          } else{
            my_place <- c(input$muni,"MA")
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
  
  ##### DEMOGRAPHICS TAB #####
  output$downloadReport <- downloadHandler(
    
    filename = function() {
      paste('demo_report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      src <- normalizePath('report.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      
      library(rmarkdown)
      out <- render('report.Rmd', switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )
  
  
  #### Age ####
  age_df <- reactive({
    my_place <- place()
    muni_df <- filter(dem_data, Region %in% my_place) %>% select(Region, Age_under_20_Pct_plot, Under35, Under65, Age_20_34_Pct_plot, Age_35_54_Pct_plot, Age_55_64_Pct_plot, Age_65_74_Pct_plot, Age_over_75_Pct_plot, Year)
    muni_df <- melt(muni_df)
    muni_df$variable <- gsub("_Pct_plot", "", muni_df$variable)
    muni_df$variable <- gsub("0_3", "0 to 3", muni_df$variable)
    muni_df$variable <- gsub("5_", "5 to ", muni_df$variable)
    muni_df$variable <- gsub("_", " ", muni_df$variable)
    muni_df$variable <- gsub("r", "r ", muni_df$variable)
    muni_df$Year <- gsub("20", "'", muni_df$Year)
    muni_df
  })
  
  output$plot_age <- renderPlot({
    dat <- age_df()
    age_var <- unique(dat$variable)
    age <- c()
    if(input$under20)
      age <- append(age, age_var[1])
    if(input$under35)
      age <- append(age, age_var[2])
    if(input$under65)
      age <- append(age, age_var[3])
    if(input$under34)
      age <- append(age, age_var[4])
    if(input$under54)
      age <- append(age, age_var[5])
    if(input$under64)
      age <- append(age, age_var[6])
    if(input$under74)
      age <- append(age, age_var[7])
    if(input$over75)
      age <- append(age, age_var[8])
    dat <- filter(dat, variable %in% age)
    theme_set(theme_classic())
    ggplot(dat, aes(x = Year, y = value, group = interaction(Region, variable), colour = interaction(Region, variable))) +
      geom_line(aes(linetype=Region), size = 1.25, show.legend = FALSE) + 
      geom_point(size = 3) + 
      labs(title = "Age Distribution", 
           x = "Mid-Year of Five Year Range",
           y = "% Population") + 
      theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
      theme(axis.title = element_text(face="bold", size=18)) +
      theme(axis.text=element_text(size=14)) + 
      theme(legend.text = element_text(size = 12)) + 
      scale_color_discrete("Region")
  })
  
  output$age_point <- renderPrint({
    dat <- age_df()
    age_var <- unique(dat$variable)
    age <- c()
    if(input$under20)
      age <- append(age, age_var[1])
    if(input$under35)
      age <- append(age, age_var[2])
    if(input$under65)
      age <- append(age, age_var[3])
    if(input$under34)
      age <- append(age, age_var[4])
    if(input$under54)
      age <- append(age, age_var[5])
    if(input$under64)
      age <- append(age, age_var[6])
    if(input$under74)
      age <- append(age, age_var[7])
    if(input$over75)
      age <- append(age, age_var[8])
    dat <- filter(dat, variable %in% age)
    nearPoints(dat, input$age_click,threshold = 30, maxpoints = 1, addDist = FALSE)
  })
  
  eventReactive(input$age_button, {
    dat <- age_df()
    age_var <- unique(dat$variable)
    age <- c()
    if(input$under20)
      age <- append(age, age_var[1])
    if(input$under35)
      age <- append(age, age_var[2])
    if(input$under65)
      age <- append(age, age_var[3])
    if(input$under34)
      age <- append(age, age_var[4])
    if(input$under54)
      age <- append(age, age_var[5])
    if(input$under64)
      age <- append(age, age_var[6])
    if(input$under74)
      age <- append(age, age_var[7])
    if(input$over75)
      age <- append(age, age_var[8])
    dat <- filter(dat, variable %in% age)
    theme_set(theme_classic())
    p<- ggplot(dat, aes(x = Year, y = value, group = interaction(Region, variable), colour = interaction(Region, variable))) +
      geom_line(aes(linetype=Region), size = 1.25, show.legend = FALSE) + 
      geom_point(size = 3) + 
      labs(title = "Age Distribution", 
           x = "Mid-Year of Five Year Range",
           y = "% Population") + 
      theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
      theme(axis.title = element_text(face="bold", size=18)) +
      theme(axis.text=element_text(size=14)) + 
      theme(legend.text = element_text(size = 12)) + 
      scale_color_discrete("Region")
    print(p) 
  })
  
  age_new <- eventReactive(input$age_button, {
    dat <- age_df()
    age_var <- unique(dat$variable)
    age <- c()
    if(input$under20)
      age <- append(age, age_var[1])
    if(input$under35)
      age <- append(age, age_var[2])
    if(input$under65)
      age <- append(age, age_var[3])
    if(input$under34)
      age <- append(age, age_var[4])
    if(input$under54)
      age <- append(age, age_var[5])
    if(input$under64)
      age <- append(age, age_var[6])
    if(input$under74)
      age <- append(age, age_var[7])
    if(input$over75)
      age <- append(age, age_var[8])
    dat <- filter(dat, variable %in% age)
    theme_set(theme_classic())
    p<- ggplot(dat, aes(x = Year, y = value, group = interaction(Region, variable), colour = interaction(Region, variable))) +
      geom_line(aes(linetype=Region), size = 1.25, show.legend = FALSE) + 
      geom_point(size = 3) + 
      labs(x = "Mid-Year of Five Year Range",
           y = "% Population") + 
      theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
      theme(axis.title = element_text(face="bold", size=18)) +
      theme(axis.text=element_text(size=14)) + 
      theme(legend.text = element_text(size = 12)) + 
      scale_color_discrete("Region")
    print(p) 
  })

  output$age_show <- renderPlot({
    age_new()
  })

  observeEvent(input$age_info, {
    showModal(modalDialog(
      title = "What is the Age variable?",
      age_pop,
      footer = modalButton("Close"),
      easyClose = TRUE
    ))
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
      if(input$under35)
        age <- append(age, age_var[2])
      if(input$under65)
        age <- append(age, age_var[3])
      if(input$under34)
        age <- append(age, age_var[4])
      if(input$under54)
        age <- append(age, age_var[5])
      if(input$under64)
        age <- append(age, age_var[6])
      if(input$under74)
        age <- append(age, age_var[7])
      if(input$over75)
        age <- append(age, age_var[8])
      dat <- filter(dat, variable %in% age)
      theme_set(theme_classic())
      p<- ggplot(dat, aes(x = Year, y = value, group = interaction(Region, variable), colour = interaction(Region, variable))) +
        geom_line(aes(linetype=Region), size = 1.25, show.legend = FALSE) + 
        geom_point(size = 3) + 
        labs(title = "Age Distribution", 
             x = "Mid-Year of Five Year Range",
             y = "% Population") + 
        theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
        theme(axis.title = element_text(face="bold", size=18)) +
        theme(axis.text=element_text(size=14)) + 
        theme(legend.text = element_text(size = 12))+ 
        scale_color_discrete("Region")
      print(p) 
      dev.off()
    }
  )
        
  #### Race ####
  rac_df <- reactive({
    my_place <- place()
    muni_df <- filter(dem_data, Region %in% my_place) %>% select(Region, White_Pct, Black_Pct, American_Indian_and_Alaska_Native_Pct, Asian_Pct, Hawaiian_and_Other_Pacific_Islander_Pct, Others_Pct, Year)
    names(muni_df) <- c("Region", "White", "Black", "AIAN", "Asian", "HOPI", "Others", "Year")
    muni_df$Year <- gsub("20", "'", muni_df$Year)
    muni_df <- melt(muni_df)
    muni_df
  })
  
  output$plot_rac <- renderPlot({
    dat <- rac_df()
    race_var <- as.character(unique(dat$variable))
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
    ggplot(dat, aes(x = Year, y = value, group = interaction(Region, variable), colour = interaction(Region, variable))) +
      geom_line(aes(linetype=Region), size = 1.25, show.legend = FALSE) + 
      geom_point(size = 3) + 
      labs(title = "Race Distribution", 
           x = "Mid-Year of Five Year Range",
           y = "% Population") + 
      theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
      theme(axis.title = element_text(face="bold", size=18)) +
      theme(axis.text=element_text(size=14)) + 
      theme(legend.text = element_text(size = 12))+ 
      scale_color_discrete("Region")
  })
  
  output$rac_point <- renderPrint({
    dat <- rac_df()
    race_var <- as.character(unique(dat$variable))
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
    nearPoints(dat, input$rac_click,threshold = 30, maxpoints = 1, addDist = FALSE)
  })
  
  rac_new <-     eventReactive(input$rac_button, {
    dat <- rac_df()
    race_var <- as.character(unique(dat$variable))
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
      geom_line(aes(linetype=Region), size = 1.25, show.legend = FALSE) + 
      geom_point(size = 3) + 
      labs(x = "Mid-Year of Five Year Range",
           y = "% Population") + 
      theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
      theme(axis.title = element_text(face="bold", size=18)) +
      theme(axis.text=element_text(size=14)) + 
      theme(legend.text = element_text(size = 12))+ 
      scale_color_discrete("Region")
    print(p) 
  })
  
  output$rac_show <- renderPlot({
    rac_new()
  })
    
  observeEvent(input$rac_info, {
    showModal(modalDialog(
      title = "What is the Race variable?",
      rac_pop,
      footer = modalButton("Close"),
      easyClose = TRUE
    ))
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
        geom_line(aes(linetype=Region), size = 1.25, show.legend = FALSE) + 
        geom_point(size = 3) + 
        labs(title = "Race Distribution", 
             x = "Mid-Year of Five Year Range",
             y = "% Population") + 
        theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
        theme(axis.title = element_text(face="bold", size=18)) +
        theme(axis.text=element_text(size=14)) + 
        theme(legend.text = element_text(size = 12))+ 
        scale_color_discrete("Region")
      print(p) 
      dev.off()
    }
  )
  
  #### Gender ####
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
    ggplot(dat, aes(x=Year, y=value, group = interaction(Region,variable), colour = Region, label = value)) +
      geom_line(aes(linetype=Region), size = 1.25) + 
      geom_point(size = 3) + 
      facet_grid(. ~ variable) + 
      labs(title = "Gender Distribution", 
           x = "Mid-Year of Five Year Range",
           y = "% Population") + 
      theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
      theme(axis.title = element_text(face="bold", size=18)) +
      theme(axis.text=element_text(size=14)) + 
      theme(legend.text = element_text(size = 12)) 
  })
  
  output$gen_point <- renderPrint({
    dat <- gen_df()
    nearPoints(dat, input$gen_click,threshold = 30, maxpoints = 1, addDist = FALSE)
  })
  
  gen_new <-     eventReactive(input$gen_button, {
    dat <- gen_df() 
    theme_set(theme_classic())
    p<- ggplot(dat, aes(x=Year, y=value, group = interaction(Region,variable), colour = Region, label = value)) +
      geom_line(aes(linetype=Region), size = 1.25) + 
      geom_point(size = 3) + 
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
  
  output$gen_show <- renderPlot({
    gen_new()
  })
  
  observeEvent(input$gen_info, {
    showModal(modalDialog(
      title = "What is the Gender variable?",
      gen_pop,
      footer = modalButton("Close"),
      easyClose = TRUE
    ))
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
        geom_line(aes(linetype=Region), size = 1.25) + 
        geom_point(size = 3) + 
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
  
  #### Ethnicity ####
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
    ggplot(dat, aes(x=Year, y=value, group = interaction(Region,variable), colour = Region)) +
      geom_line(aes(linetype=Region), size = 1.25) + 
      geom_point(size = 3) + 
      facet_grid(. ~ variable) + 
      labs(title = "Ethnicity Distribution", 
           x = "Mid-Year of Five Year Range",
           y = "% Population") + 
      theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
      theme(axis.title = element_text(face="bold", size=18)) +
      theme(axis.text=element_text(size=14)) + 
      theme(legend.text = element_text(size = 12))
  })
  
  output$his_point <- renderPrint({
    dat <- his_df()
    nearPoints(dat, input$his_click,threshold = 30, maxpoints = 1, addDist = FALSE)
  })
  
  his_new <-     eventReactive(input$his_button, {
    dat <- his_df() 
    theme_set(theme_classic())
    p<- ggplot(dat, aes(x=Year, y=value, group = interaction(Region,variable), colour = Region)) +
      geom_line(aes(linetype=Region), size = 1.25) + 
      geom_point(size = 3) + 
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
  
  output$his_show <- renderPlot({
    his_new()
  })
  
  observeEvent(input$his_info, {
    showModal(modalDialog(
      title = "What is the Ethnicity variable?",
      his_pop,
      footer = modalButton("Close"),
      easyClose = TRUE
    ))
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
        geom_line(aes(linetype=Region), size = 1.25) + 
        geom_point(size = 3) + 
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
  
  ### Population Table ###
  output$population <- renderDataTable({
    my_place <- place()
    my_place <- unique(my_place)
    pop_df <- filter(dem_data, Region %in% my_place) %>% 
      select(Region, Five_Year_Range, Total_Population) %>%
      arrange(Region)
    pop_df$Total_Population <- format(pop_df$Total_Population, big.mark=",", scientific=FALSE)
    pop_show <- c()
    for(i in 1:length(my_place)){
      pop_muni <- filter(pop_df, Region == my_place[i])
      pop_show <- rbind(pop_show, c(my_place[i], pop_muni$Total_Population))
    }
    pop_show <- as.data.frame(pop_show)
    colnames(pop_show) <- c("Region", "2005-2009", "2006-2010", "2007-2011", "2008-2012", "2009-2013", "2010-2014", "2011-2015")
    return(pop_show)
  }, options = list(searching = FALSE, orderClasses = TRUE)) 
  
  ##### SOCIAL TAB BELOW #####
  
  #### Marital App ####
  mar_df <- reactive({
    my_place <- place()
    muni_df <- filter(mar_data, Region %in% my_place) %>% select(Region, Never_Married_pct, Now_Married_pct, Separated_pct, Widowed_pct, Divorced_pct, Gender, Year)
    names(muni_df) <- gsub("_", " ", names(muni_df))
    names(muni_df) <- gsub(" pct", "", names(muni_df))
    muni_df$Year <- gsub("20", "'", muni_df$Year)
    muni_df <- melt(muni_df)
    muni_df
  })
  
  output$plot_mar <- renderPlot({
    dat <- mar_df()
    status_var <- as.character(unique(dat$variable))
    status <- c()
    if(input$divorced)
      status <- append(status, status_var[5])
    if(input$married)
      status <- append(status, status_var[2])
    if(input$separated)
      status <- append(status, status_var[3])
    if(input$widowed)
      status <- append(status, status_var[4])
    if(input$never)
      status <- append(status, status_var[1])
    dat <- filter(dat, variable %in% status)
    theme_set(theme_classic())
    ggplot(dat, aes(x = Year, y = value, group = interaction(Region, variable), colour = interaction(Region, variable))) +
      geom_line(aes(linetype=Region), size = 1.25, show.legend = FALSE) + 
      geom_point(size = 3) + 
      facet_grid(. ~ Gender) + 
      labs(title = "Marital Status", 
           x = "Mid-Year of Five Year Range",
           y = "% Population") + 
      theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
      theme(axis.title = element_text(face="bold", size=18)) +
      theme(axis.text=element_text(size=14)) + 
      theme(legend.text = element_text(size = 12))+ 
      scale_color_discrete("Region")
  })
  
  output$mar_point <- renderPrint({
    dat <- mar_df()
    status_var <- as.character(unique(dat$variable))
    status <- c()
    if(input$divorced)
      status <- append(status, status_var[5])
    if(input$married)
      status <- append(status, status_var[2])
    if(input$separated)
      status <- append(status, status_var[3])
    if(input$widowed)
      status <- append(status, status_var[4])
    if(input$never)
      status <- append(status, status_var[1])
    dat <- filter(dat, variable %in% status)
    nearPoints(dat, input$mar_click,threshold = 30, maxpoints = 1, addDist = FALSE)
  })
  
  observeEvent(input$mar_info, {
    showModal(modalDialog(
      title = "What is the Marital Status variable?",
      mar_pop,
      footer = modalButton("Close"),
      easyClose = TRUE
    ))
  })
  
  output$mar_down <- downloadHandler(
    filename = function() {
      "plot_marriage.png"
    },
    content = function(file) {
      png(file)
      dat <- mar_df()
      status_var <- as.character(unique(dat$variable))
      status <- c()
      if(input$divorced)
        status <- append(status, status_var[5])
      if(input$married)
        status <- append(status, status_var[2])
      if(input$separated)
        status <- append(status, status_var[3])
      if(input$widowed)
        status <- append(status, status_var[4])
      if(input$never)
        status <- append(status, status_var[1])
      dat <- filter(dat, variable %in% status)
      theme_set(theme_classic())
      p<- ggplot(dat, aes(x = Year, y = value, group = interaction(Region, variable), colour = interaction(Region, variable))) +
        geom_line(aes(linetype=Region), size = 1.25, show.legend = FALSE) + 
        geom_point(size = 3) + 
        facet_grid(. ~ Gender) + 
        labs(title = "Marital Status", 
             x = "Mid-Year of Five Year Range",
             y = "% Population") + 
        theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
        theme(axis.title = element_text(face="bold", size=18)) +
        theme(axis.text=element_text(size=14)) + 
        theme(legend.text = element_text(size = 12))+ 
        scale_color_discrete("Region")
      print(p) 
      dev.off()
    }
  )
  
  #### Education App ####
  edu_df <- reactive({
    my_place <- place()
    muni_df <- filter(edu_data, Region %in% my_place) %>% select(Region, No_HS_Pct, HS_Pct, Bachelors_Pct, Grad_Pct, Year)
    names(muni_df) <- c("Region", "Less Than High School", "At Least High School", "At Least Bachelor", "At Least Graduate", "Year")
    muni_df$Year <- gsub("20", "'", muni_df$Year)
    muni_df <- melt(muni_df)
    muni_df
  })
  
  output$plot_edu <- renderPlot({
    dat <- edu_df() 
    edu_var <- as.character(unique(dat$variable))
    edu <- c()
    if(input$hs)
      edu <- append(edu, edu_var[2])
    if(input$nohs)
      edu <- append(edu, edu_var[1])
    if(input$bac)
      edu <- append(edu, edu_var[3])
    if(input$grad)
      edu <- append(edu, edu_var[4])
    dat <- filter(dat, variable %in% edu)
    theme_set(theme_classic())
    ggplot(dat, aes(x = Year, y = value, group = interaction(Region, variable), colour = interaction(Region, variable))) +
      geom_line(aes(linetype=Region), size = 1.25, show.legend = FALSE) + 
      geom_point(size = 3) + 
      labs(title = "Educational Attainment", 
           x = "Mid-Year of Five Year Range",
           y = "% Population") + 
      theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
      theme(axis.title = element_text(face="bold", size=18)) +
      theme(axis.text=element_text(size=14)) + 
      theme(legend.text = element_text(size = 12))+ 
      scale_color_discrete("Region")
  })
  
  output$edu_point <- renderPrint({
    dat <- edu_df() 
    edu_var <- as.character(unique(dat$variable))
    edu <- c()
    if(input$hs)
      edu <- append(edu, edu_var[2])
    if(input$nohs)
      edu <- append(edu, edu_var[1])
    if(input$bac)
      edu <- append(edu, edu_var[3])
    if(input$grad)
      edu <- append(edu, edu_var[4])
    dat <- filter(dat, variable %in% edu)
    nearPoints(dat, input$edu_click,threshold = 30, maxpoints = 1, addDist = FALSE)
  })
  
  observeEvent(input$edu_info, {
    showModal(modalDialog(
      title = "What is the Educational Attainment variable?",
      edu_pop,
      footer = modalButton("Close"),
      easyClose = TRUE
    ))
  })
  
  output$edu_down <- downloadHandler(
    filename = function() {
      "plot_education.png"
    },
    content = function(file) {
      png(file)
      dat <- edu_df() 
      edu_var <- as.character(unique(dat$variable))
      edu <- c()
      if(input$hs)
        edu <- append(edu, edu_var[2])
      if(input$nohs)
        edu <- append(edu, edu_var[1])
      if(input$bac)
        edu <- append(edu, edu_var[3])
      if(input$grad)
        edu <- append(edu, edu_var[4])
      dat <- filter(dat, variable %in% edu)
      theme_set(theme_classic())
      p<- ggplot(dat, aes(x = Year, y = value, group = interaction(Region, variable), colour = interaction(Region, variable))) +
        geom_line(aes(linetype=Region), size = 1.25, show.legend = FALSE) + 
        geom_point(size = 3) + 
        labs(title = "Educational Attainment", 
             x = "Mid-Year of Five Year Range",
             y = "% Population") + 
        theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
        theme(axis.title = element_text(face="bold", size=18)) +
        theme(axis.text=element_text(size=14)) + 
        theme(legend.text = element_text(size = 12))+ 
        scale_color_discrete("Region")
      print(p) 
      dev.off()
    }
  )
  
  #### Suicide App ####
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
    ggplot(dat, aes(x=Year, y=Age.Adjusted.Rate, group = Region, colour = Region)) + 
      geom_line(aes(linetype=Region), size = 1.25) + 
      geom_point(size = 3) + 
      labs(title = "Suicide Rate (Age Adjusted) [County]", 
           x = "Year",
           y = "Per 100,000 Persons") + 
      theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
      theme(axis.title = element_text(face="bold", size=18)) +
      theme(axis.text=element_text(size=14)) + 
      theme(legend.text = element_text(size = 12))
  })
  
  output$sui_point <- renderPrint({
    dat <- sui_df()
    nearPoints(dat, input$sui_click,threshold = 30, maxpoints = 1, addDist = FALSE)
  })
  
  observeEvent(input$sui_info, {
    showModal(modalDialog(
      title = "What is the Age-adjusted Suicide Rate variable?",
      sui_pop,
      footer = modalButton("Close"),
      easyClose = TRUE
    ))
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
        geom_line(aes(linetype=Region), size = 1.25) + 
        geom_point(size = 3) + 
        labs(title = "Suicide Rate (Age Adjusted) [County]", 
             x = "Year",
             y = "Per 100,000 Persons") + 
        theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
        theme(axis.title = element_text(face="bold", size=18)) +
        theme(axis.text=element_text(size=14)) + 
        theme(legend.text = element_text(size = 12))
      print(p) 
      dev.off()
    }
  )
  
  #### Veteran App ####
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
    ggplot(dat, aes(x=Year, y=value, group = Region, colour=Region)) + 
      geom_line(aes(linetype=Region), size = 1.25) + 
      geom_point(size = 3) + 
      labs(title = "Civilian Veterans Status", 
           x = "Mid-Year of Five Year Range",
           y = "% Population") + 
      theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
      theme(axis.title = element_text(face="bold", size=18)) +
      theme(axis.text=element_text(size=14)) + 
      theme(legend.text = element_text(size = 12))
  })
  
  output$vet_point <- renderPrint({
    dat <- vet_df()
    nearPoints(dat, input$vet_click,threshold = 30, maxpoints = 1, addDist = FALSE)
  })
  
  observeEvent(input$vet_info, {
    showModal(modalDialog(
      title = "What is the Veterans Status variable?",
      vet_pop,
      footer = modalButton("Close"),
      easyClose = TRUE
    ))
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
        geom_line(aes(linetype=Region), size = 1.25) + 
        geom_point(size = 3) + 
        labs(title = "Civilian Veterans Status", 
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
  
  #### English Language Learner App ####
  eng_df <- reactive({
    muni_df <- filter(sch_data, Municipal %in% input$muni) %>% select(Municipal, English.Language.Learner...enrolled..1, school.year)
    names(muni_df) <- c("Municipal", "English_Language_Learner", "Year")
    muni_df$Year <- as.factor(muni_df$Year)
    muni_df$Year <- gsub("20", "'", muni_df$Year)
    dat_mean <- c()
    for (i in 1:length(input$muni)){
      sub_eng <- filter(muni_df, Municipal == input$muni[i])
      sub_mean <- aggregate(English_Language_Learner ~ Year, sub_eng, mean)
      Municipal <- rep(input$muni[i],nrow(sub_mean))
      muni_mean <- cbind(Municipal, sub_mean)
      dat_mean <- rbind(dat_mean, muni_mean)
    }
    dat_mean
  })  
  
  output$plot_eng <- renderPlot({
    dat <- eng_df() 
    theme_set(theme_classic())
    ggplot(dat, aes(x=Year, y=English_Language_Learner, group = Municipal, colour = Municipal)) + 
      geom_line(aes(linetype=Municipal), size = 1.25) + 
      geom_point(size = 3) + 
      labs(title = "English Language Learner [Municipal]", 
           x = "Year",
           y = "English Language Learner %") +  
      theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
      theme(axis.title = element_text(face="bold", size=18)) +
      theme(axis.text=element_text(size=14)) + 
      theme(legend.text = element_text(size = 12))
  })
  
  output$eng_point <- renderPrint({
    dat <- eng_df()
    nearPoints(dat, input$eng_click,threshold = 30, maxpoints = 1, addDist = FALSE)
  })
  
  observeEvent(input$eng_info, {
    showModal(modalDialog(
      title = "What is the English Language Learner variable?",
      eng_pop,
      footer = modalButton("Close"),
      easyClose = TRUE
    ))
  })
  
  output$eng_down <- downloadHandler(
    filename = function() {
      "plot_english.png"
    },
    content = function(file) {
      png(file)
      dat <- eng_df() 
      p <- ggplot(dat, aes(x=Year, y=English_Language_Learner, group = Municipal, colour = Municipal)) + 
        geom_line(aes(linetype=Municipal), size = 1.25) + 
        geom_point(size = 3) + 
        labs(title = "English Language Learner [Municipal]", 
             x = "Year",
             y = "English Language Learner %") +  
        theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
        theme(axis.title = element_text(face="bold", size=18)) +
        theme(axis.text=element_text(size=14)) + 
        theme(legend.text = element_text(size = 12))
      print(p)
      dev.off()
    }
  )
  
  #### Students with Disabilities App ####
  dis_df <- reactive({
    muni_df <- filter(sch_data, Municipal %in% input$muni) %>% select(Municipal, Students.With.Disabilities...enrolled..1, school.year)
    names(muni_df) <- c("Municipal", "Students_with_Disabilities", "Year")
    muni_df$Year <- as.factor(muni_df$Year)
    muni_df$Year <- gsub("20", "'", muni_df$Year)
    dat_mean <- c()
    for (i in 1:length(input$muni)){
      sub_eng <- filter(muni_df, Municipal == input$muni[i])
      sub_mean <- aggregate(Students_with_Disabilities ~ Year, sub_eng, mean)
      Municipal <- rep(input$muni[i],nrow(sub_mean))
      muni_mean <- cbind(Municipal, sub_mean)
      dat_mean <- rbind(dat_mean, muni_mean)
    }
    dat_mean
  })  
  
  output$plot_dis <- renderPlot({
    dat <- dis_df() 
    theme_set(theme_classic())
    ggplot(dat, aes(x=Year, y=Students_with_Disabilities, group = Municipal, colour = Municipal)) + 
      geom_line(aes(linetype=Municipal), size = 1.25) + 
      geom_point(size = 3) + 
      labs(title = "Students with Disabilities [Municipal]", 
           x = "Year",
           y = "Students with Disabilities %") +  
      theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
      theme(axis.title = element_text(face="bold", size=18)) +
      theme(axis.text=element_text(size=14)) + 
      theme(legend.text = element_text(size = 12))
  })
  
  output$dis_point <- renderPrint({
    dat <- dis_df()
    nearPoints(dat, input$dis_click,threshold = 30, maxpoints = 1, addDist = FALSE)
  })
  
  observeEvent(input$dis_info, {
    showModal(modalDialog(
      title = "What is the Students with Disabilities variable?",
      dis_pop,
      footer = modalButton("Close"),
      easyClose = TRUE
    ))
  })
  
  output$dis_down <- downloadHandler(
    filename = function() {
      "plot_disabilities.png"
    },
    content = function(file) {
      png(file)
      dat <- dis_df() 
      theme_set(theme_classic())
      p <- ggplot(dat, aes(x=Year, y=Students_with_Disabilities, group = Municipal, colour = Municipal)) + 
        geom_line(aes(linetype=Municipal), size = 1.25) + 
        geom_point(size = 3) + 
        labs(title = "Students with Disabilities [Municipal]", 
             x = "Year",
             y = "Students with Disabilities %") +  
        theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
        theme(axis.title = element_text(face="bold", size=18)) +
        theme(axis.text=element_text(size=14)) + 
        theme(legend.text = element_text(size = 12))
      print(p)
      dev.off()
    }
  )
  
  ##### ECONOMICS TAB BELOW #####
  #### Income App ####
  inc_df <- reactive({
    my_place <- place()
    muni_df <- filter(inc_data, Region %in% my_place) %>% select(Region, Median_Annual_Household_Income, Year)
    muni_df$Year <- gsub("20", "'", muni_df$Year)
    muni_df
  })
  
  output$plot_inc <- renderPlot({
    dat <- inc_df() 
    theme_set(theme_classic())
    ggplot(dat, aes(x=Year, y=Median_Annual_Household_Income, group = Region, colour = Region)) + 
      geom_line(aes(linetype=Region), size = 1.25) + 
      geom_point(size = 3) + 
      labs(title = "Median Annual Household Income", 
           x = "Mid-Year of Five Year Range",
           y = "Dollars") + 
      theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
      theme(axis.title = element_text(face="bold", size=18)) +
      theme(axis.text=element_text(size=14)) + 
      theme(legend.text = element_text(size = 12))
  })
  
  output$inc_point <- renderPrint({
    dat <- inc_df()
    nearPoints(dat, input$inc_click,threshold = 30, maxpoints = 1, addDist = FALSE)
  })
  
  observeEvent(input$inc_info, {
    showModal(modalDialog(
      title = "What is the Median Annual Household Income variable?",
      inc_pop,
      footer = modalButton("Close"),
      easyClose = TRUE
    ))
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
        geom_line(aes(linetype=Region), size = 1.25) + 
        geom_point(size = 3) + 
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

  #### Poverty App ####
  pov_df <- reactive({
    my_place <- place()
    muni_df <- filter(pov_data, Region %in% my_place) %>% select(Region, Percent_Pov, Year)
    muni_df$Year <- gsub("20", "'", muni_df$Year)
    muni_df
  })
  
  output$plot_pov <- renderPlot({
    dat <- pov_df() 
    theme_set(theme_classic())
    ggplot(dat, aes(x=Year, y=Percent_Pov, group = Region, colour = Region)) + 
      geom_line(aes(linetype=Region), size = 1.25) + 
      geom_point(size = 3) + 
      labs(title = "Poverty Rate", 
           x = "Mid-Year of Five Year Range",
           y = "% Population") + 
      theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
      theme(axis.title = element_text(face="bold", size=18)) +
      theme(axis.text=element_text(size=14)) + 
      theme(legend.text = element_text(size = 12))
  })
  
  output$pov_point <- renderPrint({
    dat <- pov_df()
    nearPoints(dat, input$pov_click,threshold = 30, maxpoints = 1, addDist = FALSE)
  })
  
  observeEvent(input$pov_info, {
    showModal(modalDialog(
      title = "What is the Poverty Status variable?",
      pov_pop,
      footer = modalButton("Close"),
      easyClose = TRUE
    ))
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
        geom_line(aes(linetype=Region), size = 1.25) + 
        geom_point(size = 3) + 
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

  #### Employment App ####
  emp_df <- reactive({
    my_place <- place()
    muni_df <- filter(emp_data, Municipal %in% my_place) %>% select(Municipal, Average_Monthly_Employment, Year)
    muni_df$Year <- gsub("20", "'", muni_df$Year)
    muni_df
  })
  
  output$plot_emp <- renderPlot({
    dat <- emp_df() 
    theme_set(theme_classic())
    ggplot(dat, aes(x=Year, y=Average_Monthly_Employment, group = Municipal, colour = Municipal)) + 
      geom_line(aes(linetype=Municipal), size = 1.25) + 
      geom_point(size = 3) + 
      labs(title = "Average Monthly Employment [Municipal]", 
           x = "Year",
           y = "Count") + 
      theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
      theme(axis.title = element_text(face="bold", size=18)) +
      theme(axis.text=element_text(size=14)) + 
      theme(legend.text = element_text(size = 12))
  })
  
  output$emp_point <- renderPrint({
    dat <- emp_df()
    nearPoints(dat, input$emp_click,threshold = 30, maxpoints = 1, addDist = FALSE)
  })
  
  observeEvent(input$emp_info, {
    showModal(modalDialog(
      title = "What is the Monthly Employment variable?",
      emp_pop,
      footer = modalButton("Close"),
      easyClose = TRUE
    ))
  })
  
  output$emp_down <- downloadHandler(
    filename = function() {
      "plot_employment.png"
    },
    content = function(file) {
      png(file)
      dat <- emp_df() 
      theme_set(theme_classic())
      p <- ggplot(dat, aes(x=Year, y=Average_Monthly_Employment, group = Municipal, colour = Municipal)) + 
        geom_line(aes(linetype=Municipal), size = 1.25) + 
        geom_point(size = 3) + 
        labs(title = "Average Monthly Employment [Municipal]", 
             x = "Year",
             y = "Count") + 
        theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
        theme(axis.title = element_text(face="bold", size=18)) +
        theme(axis.text=element_text(size=14)) + 
        theme(legend.text = element_text(size = 12))
      print(p)  
      dev.off()
    }
  )
    
  #### Unemployment App ####
  une_df <- reactive({
    my_place <- place()
    muni_df <- filter(une_data, Region %in% my_place) %>% select(Region, Unemployment_Rate_Avg, Year)
    muni_df$Year <- gsub("20", "'", muni_df$Year)
    muni_df
  })
  
  output$plot_une <- renderPlot({
    dat <- une_df() 
    theme_set(theme_classic())
    ggplot(dat, aes(x=Year, y=Unemployment_Rate_Avg, group = Region, colour = Region)) + 
      geom_line(aes(linetype=Region), size = 1.25) + 
      geom_point(size = 3) + 
      labs(title = "Annual Average Unemployment Rate [No US Avg]", 
           x = "Year",
           y = "% Population") + 
      theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
      theme(axis.title = element_text(face="bold", size=18)) +
      theme(axis.text=element_text(size=14)) + 
      theme(legend.text = element_text(size = 12))
  })
  
  output$une_point <- renderPrint({
    dat <- une_df()
    nearPoints(dat, input$une_click,threshold = 30, maxpoints = 1, addDist = FALSE)
  })
  
  observeEvent(input$une_info, {
    showModal(modalDialog(
      title = "What is the Unemployment Rate variable?",
      une_pop,
      footer = modalButton("Close"),
      easyClose = TRUE
    ))
  })
  
  output$une_down <- downloadHandler(
    filename = function() {
      "plot_unemployment.png"
    },
    content = function(file) {
      png(file)
      dat <- une_df() 
      theme_set(theme_classic())
      p <- ggplot(dat, aes(x=Year, y=Unemployment_Rate_Avg, group = Region, colour = Region)) + 
        geom_line(aes(linetype=Region), size = 1.25) + 
        geom_point(size = 3) + 
        labs(title = "Annual Average Unemployment Rate [No US Avg]", 
             x = "Year",
             y = "% Population") + 
        theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
        theme(axis.title = element_text(face="bold", size=18)) +
        theme(axis.text=element_text(size=14)) + 
        theme(legend.text = element_text(size = 12))
      print(p) 
      dev.off()
    }
  )
  
  #### Bankruptcy App ####
  ban_place <- reactive({
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
  })
  
  ban_df <- reactive({
    my_place <- ban_place()
    muni_df <- filter(ban_data, Region %in% my_place) %>% select(Region, Percentage_of_Chapter_7_in_Business_Filings, Percentage_of_Chapter_11_in_Business_Filings, Percentage_of_Chapter_12_in_Business_Filings, Percentage_of_Chapter_13_in_Business_Filings, Percentage_of_Chapter_7_in_Personal_Filings, Percentage_of_Chapter_11_in_Personal_Filings, Percentage_of_Chapter_13_in_Personal_Filings, Year)
    colnames(muni_df) <- gsub("Percentage_of_", "", colnames(muni_df))
    colnames(muni_df) <- gsub("_Filings", "", colnames(muni_df))
    colnames(muni_df) <- gsub("Chapter_", "Chp.", colnames(muni_df))
    muni_df$Year <- as.factor(muni_df$Year)
    muni_df <- melt(muni_df)
    muni_df$Year <- gsub("20", "'", muni_df$Year)
    muni_df
  })
  
  output$plot_bus <- renderPlot({
    dat <- ban_df()
    var <- c("Chp.7_in_Business", "Chp.11_in_Business", "Chp.12_in_Business", "Chp.13_in_Business")
    bus <- c()
    if(input$buschp7)
      bus <- append(bus, var[1])
    if(input$buschp11)
      bus <- append(bus, var[2])
    if(input$buschp12)
      bus <- append(bus, var[3])
    if(input$buschp13)
      bus <- append(bus, var[4])
    dat <- filter(dat, variable %in% bus)
    dat$variable <- gsub("_in_Business", "", dat$variable)
    theme_set(theme_classic())
    ggplot(dat, aes(x = Year, y = value, group = interaction(Region, variable), colour = interaction(Region, variable))) +
      geom_line(aes(linetype=Region), size = 1.25, show.legend = FALSE) + 
      geom_point(size = 3) + 
      labs(title = "Business Bankruptcy [County]", 
           x = "Year",
           y = "% Total Business Fillings") + 
      theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
      theme(axis.title = element_text(face="bold", size=18)) +
      theme(axis.text=element_text(size=14)) + 
      theme(legend.text = element_text(size = 12)) + 
      scale_color_discrete("Region")
  })
  
  output$bus_point <- renderPrint({
    dat <- ban_df()
    var <- c("Chp.7_in_Business", "Chp.11_in_Business", "Chp.12_in_Business", "Chp.13_in_Business")
    bus <- c()
    if(input$buschp7)
      bus <- append(bus, var[1])
    if(input$buschp11)
      bus <- append(bus, var[2])
    if(input$buschp12)
      bus <- append(bus, var[3])
    if(input$buschp13)
      bus <- append(bus, var[4])
    dat <- filter(dat, variable %in% bus)
    nearPoints(dat, input$bus_click,threshold = 30, maxpoints = 1, addDist = FALSE)
  })
  
  observeEvent(input$bus_info, {
    showModal(modalDialog(
      title = "What is the Business Bankruptcy variable?",
      bus_pop,
      footer = modalButton("Close"),
      easyClose = TRUE
    ))
  })
  
  output$bus_down <- downloadHandler(
    filename = function() {
      "plot_business.png"
    },
    content = function(file) {
      png(file)
      dat <- ban_df()
      var <- c("Chp.7_in_Business", "Chp.11_in_Business", "Chp.12_in_Business", "Chp.13_in_Business")
      bus <- c()
      if(input$buschp7)
        bus <- append(bus, var[1])
      if(input$buschp11)
        bus <- append(bus, var[2])
      if(input$buschp12)
        bus <- append(bus, var[3])
      if(input$buschp13)
        bus <- append(bus, var[4])
      dat <- filter(dat, variable %in% bus)
      dat$variable <- gsub("_in_Business", "", dat$variable)
      theme_set(theme_classic())
      p<- ggplot(dat, aes(x = Year, y = value, group = interaction(Region, variable), colour = interaction(Region, variable))) +
        geom_line(aes(linetype=Region), size = 1.25, show.legend = FALSE) + 
        geom_point(size = 3) + 
        labs(title = "Business Bankruptcy [County]", 
             x = "Year",
             y = "% Total Business Fillings") + 
        theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
        theme(axis.title = element_text(face="bold", size=18)) +
        theme(axis.text=element_text(size=14)) + 
        theme(legend.text = element_text(size = 12)) + 
        scale_color_discrete("Region")
      print(p) 
      dev.off()
    }
  )
  
  output$plot_per <- renderPlot({
    dat <- ban_df()
    var <- c("Chp.7_in_Personal", "Chp.11_in_Personal", "Chp.13_in_Personal")
    per <- c()
    if(input$perchp7)
      per <- append(per, var[1])
    if(input$perchp11)
      per <- append(per, var[2])
    if(input$perchp13)
      per <- append(per, var[3])
    dat <- filter(dat, variable %in% per)
    dat$variable <- gsub("_in_Personal", "", dat$variable)
    theme_set(theme_classic())
    ggplot(dat, aes(x = Year, y = value, group = interaction(Region, variable), colour = interaction(Region, variable))) +
      geom_line(aes(linetype=Region), size = 1.25, show.legend = FALSE) + 
      geom_point(size = 3) + 
      labs(title = "Personal Bankruptcy [County]", 
           x = "Year",
           y = "% Total Personal Fillings") + 
      theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
      theme(axis.title = element_text(face="bold", size=18)) +
      theme(axis.text=element_text(size=14)) + 
      theme(legend.text = element_text(size = 12)) + 
      scale_color_discrete("Region")
  })
  
  output$per_point <- renderPrint({
    dat <- ban_df()
    var <- c("Chp.7_in_Personal", "Chp.11_in_Personal", "Chp.13_in_Personal")
    per <- c()
    if(input$perchp7)
      per <- append(per, var[1])
    if(input$perchp11)
      per <- append(per, var[2])
    if(input$perchp13)
      per <- append(per, var[3])
    dat <- filter(dat, variable %in% per)
    dat$variable <- gsub("_in_Personal", "", dat$variable)
    nearPoints(dat, input$per_click,threshold = 30, maxpoints = 1, addDist = FALSE)
  })
  
  observeEvent(input$per_info, {
    showModal(modalDialog(
      title = "What is the Personal Bankruptcy variable?",
      per_pop,
      footer = modalButton("Close"),
      easyClose = TRUE
    ))
  })
  
  output$per_down <- downloadHandler(
    filename = function() {
      "plot_personal.png"
    },
    content = function(file) {
      png(file)
      dat <- ban_df()
      var <- c("Chp.7_in_Personal", "Chp.11_in_Personal", "Chp.13_in_Personal")
      per <- c()
      if(input$perchp7)
        per <- append(per, var[1])
      if(input$perchp11)
        per <- append(per, var[2])
      if(input$perchp13)
        per <- append(per, var[3])
      dat <- filter(dat, variable %in% per)
      dat$variable <- gsub("_in_Personal", "", dat$variable)
      theme_set(theme_classic())
      p<- ggplot(dat, aes(x = Year, y = value, group = interaction(Region, variable), colour = interaction(Region, variable))) +
        geom_line(aes(linetype=Region), size = 1.25, show.legend = FALSE) + 
        geom_point(size = 3) + 
        labs(title = "Personal Bankruptcy [County]", 
             x = "Year",
             y = "% Total Personal Fillings") + 
        theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
        theme(axis.title = element_text(face="bold", size=18)) +
        theme(axis.text=element_text(size=14)) + 
        theme(legend.text = element_text(size = 12)) + 
        scale_color_discrete("Region")
      print(p) 
      dev.off()
    }
  )
  
  #### Rent App ####
  ren_df <- reactive({
    my_place <- place()
    muni_df <- filter(ren_data, Region %in% my_place) %>% select(Region, Median.Rent.2015.Dollar, Year)
    muni_df$Year <- gsub("20", "'", muni_df$Year)
    muni_df   
  })
  
  output$plot_ren <- renderPlot({
    dat <- ren_df() 
    theme_set(theme_classic())
    ggplot(dat, aes(x=Year, y=Median.Rent.2015.Dollar, group = Region, colour = Region)) + 
      geom_line(aes(linetype=Region), size = 1.25) + 
      geom_point(size = 3) + 
      labs(title = "Median Monthly Rent(2015-$ Adjusted)", 
           x = "Mid-Year of Five Year Range",
           y = "Dollars") + 
      theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
      theme(axis.title = element_text(face="bold", size=18)) +
      theme(axis.text=element_text(size=14)) + 
      theme(legend.text = element_text(size = 12))
  })
  
  output$ren_point <- renderPrint({
    dat <- ren_df()
    nearPoints(dat, input$ren_click,threshold = 30, maxpoints = 1, addDist = FALSE)
  })
  
  observeEvent(input$ren_info, {
    showModal(modalDialog(
      title = "What is the Inflation-Adjusted Median Rent variable?",
      ren_pop,
      footer = modalButton("Close"),
      easyClose = TRUE
    ))
  })
  
  output$ren_down <- downloadHandler(
    filename = function() {
      "plot_rent.png"
    },
    content = function(file) {
      png(file)
      dat <- ren_df() 
      theme_set(theme_classic())
      p <- ggplot(dat, aes(x=Year, y=Median.Rent.2015.Dollar, group = Region, colour = Region)) + 
        geom_line(aes(linetype=Region), size = 1.25) + 
        geom_point(size = 3) + 
        labs(title = "Median Monthly Rent(2015-$ Adjusted)", 
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
  
  #### Building Permit App ####
  bui_df <- reactive({
    my_place <- place()
    muni_df <- filter(bui_data, Region %in% my_place) %>% select(Region, Permits_Per_1000_Population, Year)
    muni_df$Year <- gsub("20", "'", muni_df$Year)
    muni_df   
  })
  
  output$plot_bui <- renderPlot({
    dat <- bui_df() 
    theme_set(theme_classic())
    ggplot(dat, aes(x=Year, y=Permits_Per_1000_Population, group = Region, colour = Region)) + 
      geom_line(aes(linetype=Region), size = 1.25) + 
      geom_point(size = 3) + 
      labs(title = "Building Permits per 1000 Population [No US]", 
           x = "Year",
           y = "Permits per 1000 Population") + 
      theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
      theme(axis.title = element_text(face="bold", size=18)) +
      theme(axis.text=element_text(size=14)) + 
      theme(legend.text = element_text(size = 12))
  })
  
  output$bui_point <- renderPrint({
    dat <- bui_df()
    nearPoints(dat, input$bui_click,threshold = 30, maxpoints = 1, addDist = FALSE)
  })
  
  observeEvent(input$bui_info, {
    showModal(modalDialog(
      title = "What is the Building Permit variable?",
      bui_pop,
      footer = modalButton("Close"),
      easyClose = TRUE
    ))
  })
  
  output$bui_down <- downloadHandler(
    filename = function() {
      "plot_buiper.png"
    },
    content = function(file) {
      png(file)
      dat <- bui_df() 
      theme_set(theme_classic())
      p <- ggplot(dat, aes(x=Year, y=Permits_Per_1000_Population, group = Region, colour = Region)) + 
        geom_line(aes(linetype=Region), size = 1.25) + 
        geom_point(size = 3) + 
        labs(title = "Building Permits per 1000 Population [No US]", 
             x = "Year",
             y = "Permits per 1000 Population") + 
        theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
        theme(axis.title = element_text(face="bold", size=18)) +
        theme(axis.text=element_text(size=14)) + 
        theme(legend.text = element_text(size = 12))
      print(p)  
      dev.off()
    }
  )

  #### Property Value App ####
  val_df <- reactive({
    my_place <- place()
    # muni_df <- filter(val_data, Municipal %in% my_place) %>% select(Municipal, Percentage_of_Residential, Percentage_of_Commercial, Percentage_of_Industrial, Percentage_of_Personal_Property, Year)
    muni_df <- filter(val_data, Municipal %in% my_place) %>% select(Municipal, Residential_Million, Commercial_Million, Industrial_Million, Personal_Property_Million, Year)
    muni_df$Year <- gsub("20", "'", muni_df$Year)
    #names(muni_df) <- gsub("Percentage_of_", "", names(muni_df))
    names(muni_df) <- c("Municipal", "Residential", "Commercial", "Industrial", "Personal Property", "Year")
    muni_df <- melt(muni_df)
    muni_df   
  })
  
  output$plot_val <- renderPlot({
    dat <- val_df() 
    val_var <- as.character(unique(dat$variable))
    val <- c()
    if(input$resval)
      val <- append(val, val_var[1])
    if(input$comval)
      val <- append(val, val_var[2])
    if(input$indval)
      val <- append(val, val_var[3])
    if(input$perval)
      val <- append(val, val_var[4])
    dat <- filter(dat, variable %in% val)
    theme_set(theme_classic())
    ggplot(dat, aes(x = Year, y = value, group = interaction(Municipal, variable), colour = interaction(Municipal, variable))) +
      geom_line(aes(linetype=Municipal), size = 1.25, show.legend = FALSE) + 
      geom_point(size = 3) + 
      labs(title = "Assessed Property Values[Municipal]", 
           x = "Year",
           y = "Dollars (million)") + 
      theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
      theme(axis.title = element_text(face="bold", size=18)) +
      theme(axis.text=element_text(size=14)) + 
      theme(legend.text = element_text(size = 12))+ 
      scale_color_discrete("Municipal")
  })
  
  output$val_point <- renderPrint({
    dat <- val_df() 
    val_var <- as.character(unique(dat$variable))
    val <- c()
    if(input$resval)
      val <- append(val, val_var[1])
    if(input$comval)
      val <- append(val, val_var[2])
    if(input$indval)
      val <- append(val, val_var[3])
    if(input$perval)
      val <- append(val, val_var[4])
    dat <- filter(dat, variable %in% val)
    nearPoints(dat, input$val_click,threshold = 30, maxpoints = 1, addDist = FALSE)
  })
  
  observeEvent(input$val_info, {
    showModal(modalDialog(
      title = "What is the Total Assessed Property Values variable?",
      val_pop,
      footer = modalButton("Close"),
      easyClose = TRUE
    ))
  })
  
  output$val_down <- downloadHandler(
    filename = function() {
      "plot_value.png"
    },
    content = function(file) {
      png(file)
      dat <- val_df() 
      val_var <- as.character(unique(dat$variable))
      val <- c()
      if(input$resval)
        val <- append(val, val_var[1])
      if(input$comval)
        val <- append(val, val_var[2])
      if(input$indval)
        val <- append(val, val_var[3])
      if(input$perval)
        val <- append(val, val_var[4])
      dat <- filter(dat, variable %in% val)
      theme_set(theme_classic())
      p<- ggplot(dat, aes(x = Year, y = value, group = interaction(Municipal, variable), colour = interaction(Municipal, variable))) +
        geom_line(aes(linetype=Municipal), size = 1.25, show.legend = FALSE) + 
        geom_point(size = 3) + 
        labs(title = "Assessed Property Values [Municipal]", 
             x = "Year",
             y = "Dollars (million)") + 
        theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
        theme(axis.title = element_text(face="bold", size=18)) +
        theme(axis.text=element_text(size=14)) + 
        theme(legend.text = element_text(size = 12))+ 
        scale_color_discrete("Municipal")
      print(p)  
      dev.off()
    }
  )
  
  #### Property Tax App ####
  tax_df <- reactive({
    my_place <- place()
    # muni_df <- filter(tax_data, Municipal %in% my_place) %>% select(Municipal, Percentage_of_Residential, Percentage_of_Commercial, Percentage_of_Industrial, Percentage_of_Personal_Property, Year)
    muni_df <- filter(tax_data, Municipal %in% my_place) %>% select(Municipal, Residential_Million, Commercial_Million, Industrial_Million, Personal_Property_Million, Year)
    muni_df$Year <- gsub("20", "'", muni_df$Year)
    #names(muni_df) <- gsub("Percentage_of_", "", names(muni_df))
    names(muni_df) <- c("Municipal", "Residential", "Commercial", "Industrial", "Personal Property", "Year")
    muni_df <- melt(muni_df)
    muni_df   
  })
  
  output$plot_tax <- renderPlot({
    dat <- tax_df() 
    tax_var <- as.character(unique(dat$variable))
    tax <- c()
    if(input$restax)
      tax <- append(tax, tax_var[1])
    if(input$comtax)
      tax <- append(tax, tax_var[2])
    if(input$indtax)
      tax <- append(tax, tax_var[3])
    if(input$pertax)
      tax <- append(tax, tax_var[4])
    dat <- filter(dat, variable %in% tax)
    theme_set(theme_classic())
    ggplot(dat, aes(x = Year, y = value, group = interaction(Municipal, variable), colour = interaction(Municipal, variable))) +
      geom_line(aes(linetype=Municipal), size = 1.25, show.legend = FALSE) + 
      geom_point(size = 3) + 
      labs(title = "Tax Levy by Class [Municipal]", 
           x = "Year",
           y = "Dollars (million)") + 
      theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
      theme(axis.title = element_text(face="bold", size=18)) +
      theme(axis.text=element_text(size=14)) + 
      theme(legend.text = element_text(size = 12))+ 
      scale_color_discrete("Municipal")
  })
  
  output$tax_point <- renderPrint({
    dat <- tax_df() 
    tax_var <- as.character(unique(dat$variable))
    tax <- c()
    if(input$restax)
      tax <- append(tax, tax_var[1])
    if(input$comtax)
      tax <- append(tax, tax_var[2])
    if(input$indtax)
      tax <- append(tax, tax_var[3])
    if(input$pertax)
      tax <- append(tax, tax_var[4])
    dat <- filter(dat, variable %in% tax)
    nearPoints(dat, input$tax_click,threshold = 30, maxpoints = 1, addDist = FALSE)
  })
  
  observeEvent(input$tax_info, {
    showModal(modalDialog(
      title = "What is the Tax Levy variable?",
      tax_pop,
      footer = modalButton("Close"),
      easyClose = TRUE
    ))
  })
  
  output$tax_down <- downloadHandler(
    filename = function() {
      "plot_tax.png"
    },
    content = function(file) {
      png(file)
      dat <- tax_df() 
      tax_var <- as.character(unique(dat$variable))
      tax <- c()
      if(input$restax)
        tax <- append(tax, tax_var[1])
      if(input$comtax)
        tax <- append(tax, tax_var[2])
      if(input$indtax)
        tax <- append(tax, tax_var[3])
      if(input$pertax)
        tax <- append(tax, tax_var[4])
      dat <- filter(dat, variable %in% tax)
      theme_set(theme_classic())
      p<- ggplot(dat, aes(x = Year, y = value, group = interaction(Municipal, variable), colour = interaction(Municipal, variable))) +
        geom_line(aes(linetype=Municipal), size = 1.25, show.legend = FALSE) + 
        geom_point(size = 3) + 
        labs(title = "Tax Levy by Class [Municipal]", 
             x = "Year",
             y = "Dollars (million)") + 
        theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
        theme(axis.title = element_text(face="bold", size=18)) +
        theme(axis.text=element_text(size=14)) + 
        theme(legend.text = element_text(size = 12))+ 
        scale_color_discrete("Municipal")
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
