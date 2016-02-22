###########################################
## Title: Education Measures             ##
## Author(s): Xuelian Li, Emily Ramos,   ## 
##            Arvind Ramakrishnan,       ##
##            Jenna Kiridly, Steve Lauer,##
##            Justin Baldwin             ##
## Date Created:  12/10/2015             ##
## Date Modified: 12/10/2015             ##
###########################################


##First file run - Environment Setup
## load necessary libraries
require(dplyr)
##require(sp)
require(maptools)
##require(rgeos)
require(Hmisc)
require(reshape2)
require(shiny)
require(googleCharts)
require(googleVis)
require(leaflet)
require(RJSONIO)
require(tidyr)

# ## load map data
# #MA_map_county <- fromJSON("County_2010Census_DP1.geojson")
# MA_map_muni <- fromJSON("Muni_2010Census_DP1.geojson")
edu_data <- read.csv(file="edum_data.csv")
#put this in data cleaning
colnames(edu_data)[7:21]<-gsub(x=names(edu_data)[7:21],pattern=".", replacement=" ", fixed=T)
colnames(edu_data)[16]<-"Eighth Grade"

## Find order of municipals in geojson files
## Each municipal is a separate feature
# for(i in 1:length(MA_map_muni$features)){
#   MA_map_muni$features[[i]]$properties$NAMELSAD10 <- substr(MA_map_muni$features[[i]]$properties$NAMELSAD10, 1, nchar(MA_map_muni$features[[i]]$properties$NAMELSAD10)-5)
# }
# 
# MA_municipals_map <- c()
# for(i in 1:length(MA_map_muni$features)){
#   MA_municipals_map <- c(MA_municipals_map, MA_map_muni$features[[i]]$properties$NAMELSAD10)
# }
# 
# idx_leftovers <- which(!MA_municipals_map %in% emp_data$Municipal)
# leftover_munis <- MA_municipals_map[idx_leftovers]
# for(i in 1:length(leftover_munis)){
#   MA_map_muni$features[[idx_leftovers[i]]]$properties$NAMELSAD10 <-
#     substr(leftover_munis[i], 1, nchar(leftover_munis[i])-5)
# }
# 
# 
MA_county <- sort(as.character(unique(edu_data$County)))
 MA_municipals <- sort(as.character(unique(edu_data$Municipal)))
 all_schools<-as.character(unique(edu_data$school.name))
# for(i in 1:length(MA_map_muni$features)){
#   MA_municipals <- c(MA_municipals, MA_map_muni$features[[i]]$properties$NAMELSAD10)
# }
# 
# idx_leftovers2 <- which(!MA_municipals %in% emp_data$Municipal)
# leftover_munis_map <- MA_municipals[idx_leftovers2]
# MA_municipals <- sort(MA_municipals)
# 
# 
# 

## Set graph colors (special for colorblind people,turquoise)
cbbPalette <- c("#000000", "red", "green", "blue",
                "turquoise", "lightblue", "deeppink", "yellow")

## Create maxs and mins for googleCharts/Plot tab
xlim <- list(
  min = min(edu_data$school.year)-1,
  max = max(edu_data$school.year)+1
)
ylim <- list(
  min = min(edu_data$Females, na.rm=T)-5,

  ##+5 = max Avg monthly employment plus a little extra
  max = max(edu_data$Females, na.rm=T)+5
)
# ##when without boston, create ylim for googleCharts/plot
# ##ylim1<-list(
#   ##min = min(emp_data$Average_Monthly_Employment[which(emp_data$Municipal!="Boston")], na.rm=T)-5,
#   ##
#   ##+5 = max Avg monthly employment plus a little extra
#   ##max = max(emp_data$Average_Monthly_Employment[which(emp_data$Municipal!="Boston")], na.rm=T)+5
#   ##)
# ##Creat ylim for establishments plot
# ylim_est<-list(
#   min = min(emp_data$Number_of_Employer_Establishments, na.rm=T)-5,
#   
#   ##+5 = max Avg monthly employment plus a little extra
#   max = max(emp_data$Number_of_Employer_Establishments, na.rm=T)+5
# )
# ##when without Boston
# ylim1_est<-list(
#   min = min(emp_data$Number_of_Employer_Establishments[which(emp_data$Municipal!="Boston")], na.rm=T)-5,
#   
#   ##+5 = max Avg monthly employment plus a little extra
#   max = max(emp_data$Number_of_Employer_Establishments[which(emp_data$Municipal!="Boston")], na.rm=T)+5
# )
# ##Creat ylim for percentage change of employment plot
# ylim_pct<-list(
#   min = min(emp_data$Employment_difference, na.rm=T)-0.05,
#   
#   ##+5 = max Avg monthly employment plus a little extra
#   max = max(emp_data$Employment_difference, na.rm=T)+0.05
# ) 
# ##Creat ylim for percentage change of establishments plot
# ylim_pct_est<-list(
#   min = min(emp_data$Establishment_difference, na.rm=T)-0.05,
#   
#   ##+5 = max Avg monthly employment plus a little extra
#   max = max(emp_data$Establishment_difference, na.rm=T)+0.05
# ) 
# 
# ##Creat ylim for Average Weekly Wages plot
# ylim_wage<-list(
#   min = min(emp_data$Inflation_Adjusted_Average_Weekly_Wage, na.rm=T)-5,
#   
#   ##+5 = max Avg monthly employment plus a little extra
#   max = max(emp_data$Inflation_Adjusted_Average_Weekly_Wage, na.rm=T)+5
# )
# ##when without Boston
# ylim1_wage<-list(
#   min = min(emp_data$Inflation_Adjusted_Average_Weekly_Wage[which(emp_data$Municipal!="Boston")], na.rm=T)-5,
#   
#   ##+5 = max Avg monthly employment plus a little extra
#   max = max(emp_data$Inflation_Adjusted_Average_Weekly_Wage[which(emp_data$Municipal!="Boston")], na.rm=T)+5
# )
# ##Creat ylim for percentage change of Inflation_Adjusted_Average_Weekly_Wage plot
# ylim_pct_wage<-list(
#   min = min(emp_data$Average_Weekly_Wage_difference, na.rm=T)-0.05,
#   
#   ##+5 = max Avg monthly employment plus a little extra
#   max = max(emp_data$Average_Weekly_Wage_difference, na.rm=T)+0.05
# )
# 
# ####################################################
# 
# ## Colors for a Employment legend
# paint.brush <- colorRampPalette(colors=c("white", "violetred"))
# map_colors <- c(paint.brush(n=5), "black")
# 
# ## (split into quintiles).  Cuts are quintiles of the total data
# ## Cuts based on entire dataset - not year specific - This keeps colors consistent for maps year-to-year
# 
# empmax.val <- max(emp_data$Average_Monthly_Employment, na.rm=TRUE)
# empmin.val <- min(emp_data$Average_Monthly_Employment, na.rm=TRUE)
# 
# ## Puts each county year in between the cuts (n colors, n+1 cuts)
# ## length.out will make that many cuts
# # scuts <- seq(smin.val, smax.val, length.out = length(smap_colors))
# empcuts <- quantile(emp_data$Average_Monthly_Employment, probs = seq(0, 1, length.out = length(map_colors)), na.rm=TRUE)
# 
# ## Construct break ranges for displaying in the legend
# ## Creates a data frame
# ## head = scuts takes everything except for the last one,
# ## tails = same thing opposite
# 
# empcolorRanges <- data.frame(
#   from = head(empcuts, length(empcuts)-1),
#   to = tail(empcuts, length(empcuts)-1)
# )
# 
# ##Colors for a Establishments legend
# estcuts <- quantile(emp_data$Number_of_Employer_Establishments, probs = seq(0, 1, length.out = length(map_colors)), na.rm=TRUE)
# 
# ## Construct break ranges for displaying in the legend
# ## Creates a data frame
# ## head = scuts takes everything except for the last one,
# ## tails = same thing opposite
# estcolorRanges <- data.frame(
#   from = head(estcuts, length(estcuts)-1),
#   to = tail(estcuts, length(estcuts)-1)
# )
# 
# ##Colors for a Wages legend
# wagecuts <- quantile(emp_data$Inflation_Adjusted_Average_Weekly_Wage, probs = seq(0, 1, length.out = length(map_colors)), na.rm=TRUE)
# 
# ## Construct break ranges for displaying in the legend
# ## Creates a data frame
# ## head = scuts takes everything except for the last one,
# ## tails = same thing opposite
# wagecolorRanges <- data.frame(
#   from = head(wagecuts, length(wagecuts)-1),
#   to = tail(wagecuts, length(wagecuts)-1)
# )
# 
# ## colors fade from one color to white to another color, with gray for NAs
# ## colors for pecentage change since 2003 legend
# pctpaint.brush <- colorRampPalette(colors=c(cbbPalette[5], "white",cbbPalette[8]))
# pctmap_colors <- c(pctpaint.brush(n=6), "#999999")
# 
# ## find max and min (crude suicide rates) values for each county
# ##n.rm=FALSE = needed
# pctmax.val<-max(c(max(emp_data$Employment_difference, na.rm=FALSE),max(emp_data$Establishment_difference, na.rm=FALSE),max(emp_data$Average_Weekly_Wage_difference, na.rm=FALSE)))
# pctmin.val<-min(c(min(emp_data$Employment_difference, na.rm=FALSE),min(emp_data$Establishment_difference, na.rm=FALSE),min(emp_data$Average_Weekly_Wage_difference, na.rm=FALSE)))
# ##pctmin.val<--pctmax.val
# pctcuts1 <- seq(pctmin.val, 0, length.out = 4)
# pctcuts2 <- seq(0, pctmax.val, length.out = 4)
# pctcuts<-unique(c(pctcuts1, pctcuts2))
# # Construct break ranges for displaying in the legend
# 
# pctcolorRanges <- data.frame(
#   from = head(pctcuts, length(pctcuts)-1),
#   to = tail(pctcuts, length(pctcuts)-1)
# )
# # wage pct cut
# wagepctmax.val<-max(emp_data$Average_Weekly_Wage_difference, na.rm=FALSE)
# wagepctmin.val<-min(emp_data$Average_Weekly_Wage_difference, na.rm=FALSE)
# 
# wagepctcuts <- seq(wagepctmin.val, wagepctmax.val, length.out = length(pctmap_colors))
# 
# # Construct break ranges for displaying in the legend
# 
# wagepctcolorRanges <- data.frame(
#   from = head(wagepctcuts, length(wagepctcuts)-1),
#   to = tail(wagepctcuts, length(wagepctcuts)-1)
# )
# 
# 
# 
# #############################
# ### Large Text Block Area ###
# #############################
# 
# ## Generate map button
# gen_map_button <- HTML('<style type="text/css">
#                        .action-button {
#                        -moz-box-shadow:inset 0px 1px 0px 0px #54a3f7;
#                        -webkit-box-shadow:inset 0px 1px 0px 0px #54a3f7;
#                        box-shadow:inset 0px 1px 0px 0px #54a3f7;
#                        background:-webkit-gradient(linear, left top, left bottom, color-stop(0.05, #007dc1), color-stop(1, #0061a7));
#                        background:-moz-linear-gradient(top, #007dc1 5%, #0061a7 100%);
#                        background:-webkit-linear-gradient(top, #007dc1 5%, #0061a7 100%);
#                        background:-o-linear-gradient(top, #007dc1 5%, #0061a7 100%);
#                        background:-ms-linear-gradient(top, #007dc1 5%, #0061a7 100%);
#                        background:linear-gradient(to bottom, #007dc1 5%, #0061a7 100%);
#                        filter:progid:DXImageTransform.Microsoft.gradient(startColorstr="#007dc1", endColorstr="#0061a7",GradientType=0);
#                        background-color:#007dc1;
#                        -moz-border-radius:3px;
#                        -webkit-border-radius:3px;
#                        border-radius:3px;
#                        border:1px solid #124d77;
#                        display:inline-block;
#                        cursor:pointer;
#                        color:#ffffff;
#                        font-family:arial;
#                        font-size:16px;
#                        padding:12px 36px;
#                        text-decoration:none;
#                        text-shadow:0px 1px 0px #154682;
#                        }
#                        .action-button:hover {
#                        background:-webkit-gradient(linear, left top, left bottom, color-stop(0.05, #0061a7), color-stop(1, #007dc1));
#                        background:-moz-linear-gradient(top, #0061a7 5%, #007dc1 100%);
#                        background:-webkit-linear-gradient(top, #0061a7 5%, #007dc1 100%);
#                        background:-o-linear-gradient(top, #0061a7 5%, #007dc1 100%);
#                        background:-ms-linear-gradient(top, #0061a7 5%, #007dc1 100%);
#                        background:linear-gradient(to bottom, #0061a7 5%, #007dc1 100%);
#                        filter:progid:DXImageTransform.Microsoft.gradient(startColorstr="#0061a7", endColorstr="#007dc1",GradientType=0);
#                        background-color:#0061a7;
#                        }
#                        .action-button:active {
#                        position:relative;
#                        top:1px;
#                        }
# 
#                        </style>')

summary_side_text <- conditionalPanel(
  condition="input.tabs == 'summary'",
  ## h4 created 4th largest header
  h4("How to use this app:"),
  ## Creates text

  helpText(p(strong('Please select the years for which you are interested in viewing churn, intake, and stability rates for education measures of Massachusetts schools.'))),
  tags$br(),
  tags$ul(
    tags$br(),
    tags$li('Select a county then select one or multiple municipalities.'),
    tags$br(),
    tags$li('To look at churn, intake, and stability rates of an educational measure for a single year select single year from the drop down menu.'),
    tags$br(),
    tags$li('To look at churn, intake, and stability rates of an educational measure  over a specific time period select multiple years from the drop down menu.  Then use the sliding bar to select a range.'),
    tags$br(),
    tags$li('Sort churn, intake, and stability rates in ascending and descending order by clicking on the column or variable title.')

  )
)

## Same concept
plot_side_text <- conditionalPanel(
  condition="input.tabs == 'plot'",
  h4("How to use this app:"),
  p(strong('Please select the county for which you are interested in viewing churn, intake, and stability rates.')),
  tags$br(),
  tags$ul(
  tags$li("Once you have selected the county which you are interested in viewing churn, intake, and stability rates, select a school."),
  tags$br(),
  tags$li("To view percent race in the selected school select Race/Ethnicity."),
  tags$br(),
  tags$li("To view percent gender in the selected school select Gender."),
  tags$br(),
  tags$li("To view percent grade level in the selected school select Grade Level."),
  tags$br(),
  tags$li("To view churn and stability rates select either Enlgish Language Learners, Students with Disabilities, Low income, or High Needs."),
  tags$br(),
  tags$li("Then select a measure from the drop down list called Measures.")
  ))

# 
map_side_text <- conditionalPanel(
  condition="input.tabs == 'map'",
  h4("How to use this app:"),
  helpText(p(strong("Please select a year, and click on 'Generate Map' to get started"))),
  tags$br(),
  tags$ul(

    tags$li('To view average monthly employment select Employment, then click on a municipality for which you are interested in viewing average monthly employment.'),
    tags$br(),
    tags$li("To view the number of business establishments select Business Establishments, then click on a municipality for which you are interested in viewing the number of business establishments."),
    tags$br(),
    tags$li("To view average weekly wages select Wages, then click on a municipality for which you are interested in viewing the average weekly wages."),
    tags$br(),
    tags$li("To view the difference in average monthly employment, weekly wage, and number of business establishments between the year you selected and 2003 select Difference Compared to Year 2003.")
    
  ))

info_side_text <- conditionalPanel(
  condition="input.tabs == 'info'",
  h4("How to use this app:"),
  helpText(p(strong('This tab contains more detailed information regarding the variables of interest.'))))

 about_main_text <- p(strong("The SEIGMA Employment App"), "displays average monthly employment, weekly wage, and number of business establishments for Massachusetts municipalities.",
                      p(strong("Click on different tabs to view the data in different formats.")),
                      tags$br(),
                      tags$ul(
                        tags$li(p(strong("Summary"), "shows the source data in table format.")),
                        tags$li(p(strong("Plot"), "compares average monthly employment, weekly wage, and number of business establishments for each municipality.")),
                        tags$li(p(strong("Map"), "visually displays average monthly employment, weekly wage, and number of business establishments by municipality.")),
                        tags$li(p(strong("More Info"), "describes the average monthly employment estimate, weekly wage, and number of business establishments including formulas and calculations."))
                      ))

# plot_main_text <- p(strong("Variable Summary:"),
#                     ## breaks between paragraphs
#                     tags$br(),
#                     strong("Annual Average Monthly Employment-"),
#                     " Info about Avg. Monthly Employment.",
#                     tags$br(),
#                     strong("SEIGMA. Social and Economic Impacts of Gambling in Massachusetts, University of Massachusetts School of Public Health and Health Sciences. (2014). Report on the Social and Economic Impact of Gambling in Massachusetts SEIGMA Gambling study. Report to the Massachusetts Gaming Commission & the Massachusetts department of Public Health. Retrieved from:"), a("http://www.umass.edu/seigma/sites/default/files/March%202014%20SEIGMA%20Report_6-19_for%20website.pdf", align="center"))
# 
 font_size <- 14
# 
Emp_plot_options1 <- googleLineChart("Female_pct_plot", width="100%", height="475px", options = list(
                                    
                                    ## set fonts
                                    fontName = "Source Sans Pro",
                                    fontSize = 14,
                                    
                                    ## set axis titles, ticks, fonts, and ranges
                                    hAxis = list(
                                      title = "Year",
                                      format = "####",
                                      ticks = seq(2002, 2012, 2),
                                      viewWindow = xlim,
                                      textStyle = list(
                                        fontSize = 14),
                                      titleTextStyle = list(
                                        fontSize = 16,
                                        bold = TRUE,
                                        italic = FALSE)
                                    ),
                                    vAxis = list(
                                      title = "Average Monthly Employment",
                                      viewWindow = ylim,
                                      textStyle = list(
                                        fontSize = 14),
                                      titleTextStyle = list(
                                        fontSize = 16,
                                        bold = TRUE,
                                        italic = FALSE)
                                    ),
                                    
                                    ## set legend fonts
                                    legend = list(
                                      textStyle = list(
                                        fontSize=14)),
                                    
                                    ## set chart area padding
                                    chartArea = list(
                                      top = 50, left = 75,
                                      height = "75%", width = "70%"
                                    ),
                                    
                                    ## set colors
                                    colors = cbbPalette,
                                    
                                    ## set point size
                                    pointSize = 3,
                                    
                                    ## set tooltip font size
                                    ## Hover text font stuff
                                    tooltip = list(
                                      textStyle = list(
                                        fontSize = 14
                                      )
                                    )
                                  ))

# Est_plot_options1 <- googleLineChart("Est_plot1", width="100%", height="475px", options = list(
#                       
#                       ## set fonts
#                       fontName = "Source Sans Pro",
#                       fontSize = 14,
#                       
#                       ## set axis titles, ticks, fonts, and ranges
#                       hAxis = list(
#                         title = "Year",
#                         format = "####",
#                         ticks = seq(2002, 2012, 2),
#                         viewWindow = xlim,
#                         textStyle = list(
#                           fontSize = 14),
#                         titleTextStyle = list(
#                           fontSize = 16,
#                           bold = TRUE,
#                           italic = FALSE)
#                       ),
#                       vAxis = list(
#                         title = "Number of Business Establishments",
#                         viewWindow = ylim_est,
#                         textStyle = list(
#                           fontSize = 14),
#                         titleTextStyle = list(
#                           fontSize = 16,
#                           bold = TRUE,
#                           italic = FALSE)
#                       ),
#                       
#                       ## set legend fonts
#                       legend = list(
#                         textStyle = list(
#                           fontSize=14)),
#                       
#                       ## set chart area padding
#                       chartArea = list(
#                         top = 50, left = 75,
#                         height = "75%", width = "70%"
#                       ),
#                       
#                       ## set colors
#                       colors = cbbPalette,
#                       
#                       ## set point size
#                       pointSize = 3,
#                       
#                       ## set tooltip font size
#                       ## Hover text font stuff
#                       tooltip = list(
#                         textStyle = list(
#                           fontSize = 14
#                         )
#                       )
#                     ))
# 
# Emp_pct_plot_options <- googleLineChart("Emp_pct_plot", width="100%", height="475px",
#                                        options = list(
#                                          ## set fonts
#                                          fontName = "Source Sans Pro",
#                                          fontSize = font_size,
#                                          title = "",
#                                          ## set axis titles, ticks, fonts, and ranges
#                                          hAxis = list(
#                                            title = "Year",
#                                            format = "####",
#                                            ticks = seq(2002, 2012, 2),
#                                            viewWindow = xlim,
#                                            textStyle = list(
#                                              fontSize = 14),
#                                            titleTextStyle = list(
#                                              fontSize = 16,
#                                              bold = TRUE,
#                                              italic = FALSE)
#                                          ),
#                                          vAxis = list(
#                                            title = "Change in Employment since 2003 (%)",
#                                            viewWindow = ylim_pct,
#                                            textStyle = list(
#                                              fontSize = font_size),
#                                            titleTextStyle = list(
#                                              fontSize = font_size+2,
#                                              bold = TRUE,
#                                              italic = FALSE)
#                                          ),
#                                          
#                                          ## set legend fonts
#                                          legend = list(
#                                            textStyle = list(
#                                              fontSize=font_size),
#                                            position = "right"),
#                                          
#                                          ## set chart area padding
#                                          chartArea = list(
#                                            top = 50, left = 100,
#                                            height = "75%", width = "65%"
#                                          ),
#                                          
#                                          ## set colors
#                                          colors = cbbPalette[c(2:8)],
#                                          
#                                          ## set point size
#                                          pointSize = 3,
#                                          
#                                          ## set tooltip font size
#                                          ## Hover text font stuff
#                                          tooltip = list(
#                                            textStyle = list(
#                                              fontSize = font_size
#                                            )
#                                          )
#                                        ))
# Est_pct_plot_options <- googleLineChart("Est_pct_plot", width="100%", height="475px",
#                                           options = list(
#                                             ## set fonts
#                                             fontName = "Source Sans Pro",
#                                             fontSize = font_size,
#                                             title = "",
#                                             ## set axis titles, ticks, fonts, and ranges
#                                             hAxis = list(
#                                               title = "Year",
#                                               format = "####",
#                                               ticks = seq(2002, 2012, 2),
#                                               viewWindow = xlim,
#                                               textStyle = list(
#                                                 fontSize = 14),
#                                               titleTextStyle = list(
#                                                 fontSize = 16,
#                                                 bold = TRUE,
#                                                 italic = FALSE)
#                                             ),
#                                             vAxis = list(
#                                               title = "Change in Business Establishments since 2003 (%)",
#                                               ##ticks = seq(50, 250, 50),
#                                               viewWindow = ylim_pct_est,
#                                               textStyle = list(
#                                                 fontSize = font_size),
#                                               titleTextStyle = list(
#                                                 fontSize = font_size+2,
#                                                 bold = TRUE,
#                                                 italic = FALSE)
#                                             ),
#                                             
#                                             ## set legend fonts
#                                             legend = list(
#                                               textStyle = list(
#                                                 fontSize=font_size),
#                                               position = "right"),
#                                             
#                                             ## set chart area padding
#                                             chartArea = list(
#                                               top = 50, left = 100,
#                                               height = "75%", width = "65%"
#                                             ),
#                                             
#                                             ## set colors
#                                             colors = cbbPalette[c(2:8)],
#                                             
#                                             ## set point size
#                                             pointSize = 3,
#                                             
#                                             ## set tooltip font size
#                                             ## Hover text font stuff
#                                             tooltip = list(
#                                               textStyle = list(
#                                                 fontSize = font_size
#                                               )
#                                             )
#                                           ))
# Wage_plot_options1 <- googleLineChart("Wage_plot1", width="100%", height="475px", options = list(
#   
#   ## set fonts
#   fontName = "Source Sans Pro",
#   fontSize = 14,
#   
#   ## set axis titles, ticks, fonts, and ranges
#   hAxis = list(
#     title = "Year",
#     format = "####",
#     ticks = seq(2002, 2012, 2),
#     viewWindow = xlim,
#     textStyle = list(
#       fontSize = 14),
#     titleTextStyle = list(
#       fontSize = 16,
#       bold = TRUE,
#       italic = FALSE)
#   ),
#   vAxis = list(
#     title = "Average Weekly Wage (2012 dollars)",
#     viewWindow = ylim_wage,
#     textStyle = list(
#       fontSize = 14),
#     titleTextStyle = list(
#       fontSize = 16,
#       bold = TRUE,
#       italic = FALSE)
#   ),
#   
#   ## set legend fonts
#   legend = list(
#     textStyle = list(
#       fontSize=14)),
#   
#   ## set chart area padding
#   chartArea = list(
#     top = 50, left = 75,
#     height = "75%", width = "70%"
#   ),
#   
#   ## set colors
#   colors = cbbPalette,
#   
#   ## set point size
#   pointSize = 3,
#   
#   ## set tooltip font size
#   ## Hover text font stuff
#   tooltip = list(
#     textStyle = list(
#       fontSize = 14
#     )
#   )
# ))
# Wage_plot_options2 <- googleLineChart("Wage_plot2", width="100%", height="475px", options = list(
#   ## set fonts
#   fontName = "Source Sans Pro",
#   fontSize = 14,
#   
#   ## set axis titles, ticks, fonts, and ranges
#   hAxis = list(
#     title = "Year",
#     format = "####",
#     ticks = seq(2002, 2012, 2),
#     viewWindow = xlim,
#     textStyle = list(
#       fontSize = 14),
#     titleTextStyle = list(
#       fontSize = 16,
#       bold = TRUE,
#       italic = FALSE)
#   ),
#   vAxis = list(
#     title = "Average Weekly Wage (2012 dollars)",
#     viewWindow = ylim1_wage,
#     textStyle = list(
#       fontSize = 14),
#     titleTextStyle = list(
#       fontSize = 16,
#       bold = TRUE,
#       italic = FALSE)
#   ),
#   
#   ## set legend fonts
#   legend = list(
#     textStyle = list(
#       fontSize=14)),
#   
#   ## set chart area padding
#   chartArea = list(
#     top = 50, left = 75,
#     height = "75%", width = "70%"
#   ),
#   
#   ## set colors
#   colors = cbbPalette,
#   
#   ## set point size
#   pointSize = 3,
#   
#   ## set tooltip font size
#   ## Hover text font stuff
#   tooltip = list(
#     textStyle = list(
#       fontSize = 14
#     )
#   )
# ))
# Wage_pct_plot_options <- googleLineChart("Wage_pct_plot", width="100%", height="475px",
#                                         options = list(
#                                           ## set fonts
#                                           fontName = "Source Sans Pro",
#                                           fontSize = font_size,
#                                           title = "",
#                                           ## set axis titles, ticks, fonts, and ranges
#                                           hAxis = list(
#                                             title = "Year",
#                                             format = "####",
#                                             ticks = seq(2002, 2012, 2),
#                                             viewWindow = xlim,
#                                             textStyle = list(
#                                               fontSize = 14),
#                                             titleTextStyle = list(
#                                               fontSize = 16,
#                                               bold = TRUE,
#                                               italic = FALSE)
#                                           ),
#                                           vAxis = list(
#                                             title = "Change in Average Weekly Wage since 2003 (%)",
#                                             viewWindow = ylim_pct_wage,
#                                             textStyle = list(
#                                               fontSize = font_size),
#                                             titleTextStyle = list(
#                                               fontSize = font_size+2,
#                                               bold = TRUE,
#                                               italic = FALSE)
#                                           ),
#                                           
#                                           ## set legend fonts
#                                           legend = list(
#                                             textStyle = list(
#                                               fontSize=font_size),
#                                             position = "right"),
#                                           
#                                           ## set chart area padding
#                                           chartArea = list(
#                                             top = 50, left = 100,
#                                             height = "75%", width = "65%"
#                                           ),
#                                           
#                                           ## set colors
#                                           colors = cbbPalette[c(2:8)],
#                                           
#                                           ## set point size
#                                           pointSize = 3,
#                                           
#                                           ## set tooltip font size
#                                           ## Hover text font stuff
#                                           tooltip = list(
#                                             textStyle = list(
#                                               fontSize = font_size
#                                             )
#                                           )
#                                         ))


thebarplot <- googleColumnChart("chart",width="100%", height = "500px")

thelineplot <- googleLineChart("chart",width="100%", height = "500px")

raceplot_options<-list(isStacked="percent",
                                       # set fonts
                                       fontName = "Source Sans Pro",
                                       fontSize = font_size,
                                       title = "",
                                       ## set axis titles, ticks, fonts, and ranges
                                       hAxis = list(
                                         title = "Start of School Year",
                                         format = "####",
                                           textStyle = list(
                                           fontSize = 14),
                                         titleTextStyle = list(
                                           fontSize = 16,
                                           bold = TRUE,
                                           italic = FALSE)
                                       ),
                       vAxis = list(
                         title = "Number of students",
                         ticks = seq(0,1,0.2),
                         
                         #viewWindow = list(min=0, max=100),
                         textStyle = list(
                           fontSize = font_size),
                         titleTextStyle = list(
                           fontSize = font_size+2,
                           bold = TRUE,
                           italic = FALSE)
                       ),
                                       
                                       ## set legend fonts
                                       legend = list(
                                       textStyle = list(
                                       fontSize=font_size),
                                       position = "right"),
                                       
                                       ## set chart area padding
                                       chartArea = list(
                                       top = 50, left = 100,
                                       height = "75%", width = "65%"
                                       ),
                                       
                                                                                  
                                       ## set tooltip font size
                                       ## Hover text font stuff
                                       tooltip = list(
                                       textStyle = list(
                                       fontSize = font_size
                                       ))
                       )

genderplot_options <- list(isStacked="percent",
                             # set fonts
                             fontName = "Source Sans Pro",
                             fontSize = font_size,
                             title = "",
                             ## set axis titles, ticks, fonts, and ranges
                             hAxis = list(
                               title = "Start of School Year",
                               format = "####",
                               #  ticks = seq(2002, 2012, 2),
                               #  viewWindow = list(min=, max=),
                               textStyle = list(
                                 fontSize = 14),
                               titleTextStyle = list(
                                 fontSize = 16,
                                 bold = TRUE,
                                 italic = FALSE)
                             ),
                           vAxis = list(
                             title = "Number of students",
                             ticks = seq(0,1,0.2),
                             
                             #viewWindow = list(min=0, max=100),
                             textStyle = list(
                               fontSize = font_size),
                             titleTextStyle = list(
                               fontSize = font_size+2,
                               bold = TRUE,
                               italic = FALSE)
                           ))



       
gradelevelplot_options <- list(isStacked="percent",
                                   # set fonts
                                  fontName = "Source Sans Pro",
                                  fontSize = font_size,
                                  title = "",
                                  ## set axis titles, ticks, fonts, and ranges
                                  hAxis = list(
                                  title = "Start of School Year",
                                  format = "####",
                                #  ticks = seq(2002, 2012, 2),
                                #  viewWindow = list(min=, max=),
                                  textStyle = list(
                                  fontSize = 14),
                                  titleTextStyle = list(
                                  fontSize = 16,
                                  bold = TRUE,
                                  italic = FALSE)
                                  ),
                                  vAxis = list(
                                  title = "Number of students",
                                  ticks = seq(0,1,0.2),
                                  
                                 #viewWindow = list(min=0, max=100),
                                  textStyle = list(
                                  fontSize = font_size),
                                  titleTextStyle = list(
                                  fontSize = font_size+2,
                                  bold = TRUE,
                                  italic = FALSE)
                                  ))

mobenrollmentplot_options  <- list(isStacked="TRUE",
                           # set fonts
                           fontName = "Source Sans Pro",
                           fontSize = font_size,
                           title = "",
                           ## set axis titles, ticks, fonts, and ranges
                           hAxis = list(
                             title = "Start of School Year",
                             format = "####",
                             #  ticks = seq(2002, 2012, 2),
                             #  viewWindow = list(min=, max=),
                             textStyle = list(
                               fontSize = 14),
                             titleTextStyle = list(
                               fontSize = 16,
                               bold = TRUE,
                               italic = FALSE)
                           ),
                           vAxis = list(
                             title = "Number of Students",
                           #  ticks = seq(0,1,0.2),
                             
                             #viewWindow = list(min=0, max=100),
                             textStyle = list(
                               fontSize = font_size),
                             titleTextStyle = list(
                               fontSize = font_size+2,
                               bold = TRUE,
                               italic = FALSE)
                           ))


mobrateplot_options <- list(isStacked="TRUE",
                           # set fonts
                           fontName = "Source Sans Pro",
                           fontSize = font_size,
                           title = "",
                           ## set axis titles, ticks, fonts, and ranges
                           hAxis = list(
                             title = "Start of School Year",
                             format = "####",
                             #  ticks = seq(2002, 2012, 2),
                             #  viewWindow = list(min=, max=),
                             textStyle = list(
                               fontSize = 14),
                             titleTextStyle = list(
                               fontSize = 16,
                               bold = TRUE,
                               italic = FALSE)
                           ),
                           vAxis = list(
                             title = "Mobility Rate",
                           #  ticks = seq(0,1,0.2),
                             
                             #viewWindow = list(min=0, max=100),
                             textStyle = list(
                               fontSize = font_size),
                             titleTextStyle = list(
                               fontSize = font_size+2,
                               bold = TRUE,
                               italic = FALSE)
                           ))


enrolledplot_options  <- list(isStacked="percent",
                             # set fonts
                             fontName = "Source Sans Pro",
                             fontSize = font_size,
                             title = "",
                             ## set axis titles, ticks, fonts, and ranges
                             hAxis = list(
                               title = "Start of School Year",
                               format = "####",
                               #  ticks = seq(2002, 2012, 2),
                               #  viewWindow = list(min=, max=),
                               textStyle = list(
                                 fontSize = 14),
                               titleTextStyle = list(
                                 fontSize = 16,
                                 bold = TRUE,
                                 italic = FALSE)
                             ),
                             vAxis = list(
                               title = "Number of students",
                               ticks = seq(0,1,0.2),
                               
                               #viewWindow = list(min=0, max=100),
                               textStyle = list(
                                 fontSize = font_size),
                               titleTextStyle = list(
                                 fontSize = font_size+2,
                                 bold = TRUE,
                                 italic = FALSE)
                             ))

  
