###########################################
## Title: Education Measures             ##
## Author(s): Xuelian Li, Emily Ramos,   ## 
##            Arvind Ramakrishnan,       ##
##            Jenna Kiridly, Steve Lauer,##
##            Justin Baldwin             ##
## Date Created:  12/10/2015             ##
## Date Modified: 12/10/2015             ##
###########################################


shinyServer(function(input, output, session) {
  # emp_df is a reactive dataframe. Necessary for when summary/plot/map have common input (Multiple Variables). Not in this project


  #   sum_df <- emp_df %>%
  #     filter(Region %in% munis) %>%
  #     select(4:length(colnames(df)))

  edum<- reactive({
    ## Filter the data by the chosen Year
    edum <- edu_data ## %>%
    ##select(1:6, 9,11,13)

    ## Output reactive dataframe
    edum
  })
  
  
  ## Create summary table
  output$summary <- renderDataTable({
    ## Make reactive dataframe into regular dataframe
    edum <- edum()

    ## if a user chooses Single Year, display only data from that year (dpylr)
    if(input$sum_timespan == "sing.yr"){
      df <- filter(edum, school.year==input$sum_year)
    }

    ## if a user chooses Multiple Years, display data from all years in range
    if(input$sum_timespan == "mult.yrs"){
      range <- seq(min(input$sum_range), max(input$sum_range), 1)
      df <- c()

      for(i in 1:length(range)){
        bbb <- subset(edum, school.year==range[i])
        df <- rbind.data.frame(df, bbb)
      }
    }

    ## make municipals a vector based on input variable
    ## For when something is clicked

    if(!is.null(input$sum_muni))
      munis <- input$sum_muni
    ## if none selected, put all municipals in vector
    ## For nothing clicked
    if(is.null(input$sum_muni))
      munis <- MA_municipals
    
 
    
    ##select row according input$school_type
    grades<-c()
    if (input$school_type=="Pre-K") {
      grades<-c(7:14)
      grade_level_names<-c("Pre Kindergarten", "Kindergarten" ,"First Grade" ,"Second Grade" ,"Third Grade" ,"Fourth Grade","Fifth Grade" , "Sixth Grade")
      sum_df <- df %>%
        filter(Municipal %in% munis, PREK==1)
        } else 
    if (input$school_type=="Kindergarten") {
      grades<-c(7:14)
      grade_level_names<-c("Pre Kindergarten", "Kindergarten" ,"First Grade" ,"Second Grade" ,"Third Grade" ,"Fourth Grade","Fifth Grade" , "Sixth Grade")
      sum_df <- df %>%
        filter(Municipal %in% munis,  KIND==1)
        } else 
    if (input$school_type=="Elementary") {
      grades<-c(7:14)
      grade_level_names<-c("Pre Kindergarten", "Kindergarten" ,"First Grade" ,"Second Grade" ,"Third Grade" ,"Fourth Grade","Fifth Grade" , "Sixth Grade")
      sum_df <- df %>%
        filter(Municipal %in% munis, ELEM==1) 
        } else 
    if (input$school_type=="Middle") {
      grades<-c(13:17)
      grade_level_names<-c("Fifth Grade" , "Sixth Grade","Seventh Grade", "Eighth Grade", "Ninth Grade")
      sum_df <- df %>%
         filter(Municipal %in% munis, MIDD==1)
     } else {
      grades<-c(17:21)
      grade_level_names<-c("Ninth Grade", "Tenth Grade","Eleventh Grade" ,"Twelfth Grade","Special Ed Beyond 12th Grade")
       sum_df <- df %>%
         filter(Municipal %in% munis, HIGH==1)}
    

    ##select columns according input$radio
    sel_col_num<-c()
    df_colnames<-c()
    if (input$sum_radio=="Race/Ethnicity") {
      sel_col_num<-c(23:27, 30:31)
      df_colnames<-c("African American", "Asian", "Hispanic", "White", "Native American", "Native Hawaiian Pacific Islander", "Multi-race non-Hispanic")
    } else if (input$sum_radio=="Gender") {
      sel_col_num<-c(28:29)
      df_colnames<-c( "Males", "Females")
    } else if (input$sum_radio=="Grade Level") {
      sel_col_num<-grades
      df_colnames<-grade_level_names
    }  else if (input$sum_radio=="English Language Learners") {
        sel_col_num<-c(32, 34, 51:55)
        df_colnames<-c("First Language Not English Enrolled", "English Language Learner Enrolled", "Churn Enrollment for English Language Learning Students", "Churn Rate for English Language Learning Students", "Intake Rate for English Language Learning Students", "Stability Enrollment for English Language Learning Students", "Stability Rate for English Language Learning Students")
    } else if (input$sum_radio=="Students with Disabilities") {
      sel_col_num<-c(36, 46:50)
      df_colnames<-c(  "Students with Disabilities Enrolled", "Churn Enrollment for Students With Disabilites", "Churn Rate for Students With Disabilites", "Intake Rate for Students With Disabilites", "Stability Enrollment for Students with Disabilities", "Stability Rate for Students with Disabilities")
    } else if (input$sum_radio=="Low Income") {
      sel_col_num<-c(38, 61:65)
      df_colnames<-c(  "Low Income Enrolled","Churn Enrollment for Low Income Students", "Churn Rate for Low Income Students", "Intake Rate for Low Income Students", "Stability Enrollment for Low Income Students", "Stability Rate for Low Income Students")
    } else {
      sel_col_num<-c(44, 56:60)
      df_colnames<-c("High Needs Enrolled", "Churn Enrollmment for High Needs Students", "Churn Rate for High Needs Students", "Intake Rate for High Needs Students", "Stability Enrollment for High Needs Students", "Stability Rate for High Needs Students")}
    
    sum_df <- sum_df %>%
    arrange(Municipal, school.name)%>%
      select(Municipal, school.name, school.year, Total.Students.Enrolled, 
             sel_col_num)
    
    final_colnames<-c("Municipal","School Name","School Year", "Total Number Enrolled",
                      df_colnames)
  
    colnames(sum_df) <- final_colnames

    ## create a dataframe consisting only of counties in vector
return(sum_df)
  }, options = list(searching = FALSE, orderClasses = TRUE))


  
  
  # there are a bunch of options to edit the appearance of datatables, this removes one of the ugly features


  ## create the plot of the data
  
  output$Female_pct_plot<-reactive({
   
    emp_df <- edum()
    
    ## make region a vector based on input variable
    munis <- input$plot_muni
    
    ##selecting rows based on the chosen school name
    if(!is.null(input$plot_school))
      sname <- input$plot_school
    ## if none selected, put no schools in vector
    ## For nothing clicked
    if(is.null(input$plot_school))
      sname <- all_schools
    emp_df <- emp_df %>%
      filter(school.name %in% sname)
#     
#     ## make municipals a vector based on input variable
#     ## For when something is clicked
#     
#     if(!is.null(input$sum_muni))
#       munis <- input$sum_muni
#     ## if none selected, put all municipals in vector
#     ## For nothing clicked
#     if(is.null(input$sum_muni))
#       munis <- MA_municipals
#     
    
    
   
    
    
    ##select columns according input$radio
    sel_col_num<-c()
   if (input$sum_radio=="Gender") {
      sel_col_num<-c(28:29)
      df_colnames<-c( "Males", "Females")
    } 

    
    f <- sum_df %>%
      select(Municipal, school.name, school.year, 
             sel_col_num[2]) %>%
      spread(school.year, sel_col_num[2])
    
         list(
         data=googleDataTable(f))
  })
    

  
  output$Male_pct_plot<-reactive({
    
    emp_df <- edum()
    
    ## make region a vector based on input variable
    munis <- input$plot_muni
    
    ##selecting rows based on the chosen school name
    if(!is.null(input$plot_school))
      sname <- input$plot_school
    ## if none selected, put no schools in vector
    ## For nothing clicked
    if(is.null(input$plot_school))
      sname <- all_schools
    emp_df <- emp_df %>%
      filter(school.name %in% sname)
    #     
    #     ## make municipals a vector based on input variable
    #     ## For when something is clicked
    #     
    #     if(!is.null(input$sum_muni))
    #       munis <- input$sum_muni
    #     ## if none selected, put all municipals in vector
    #     ## For nothing clicked
    #     if(is.null(input$sum_muni))
    #       munis <- MA_municipals
    #     
    
    
    
    
    
    ##select columns according input$radio
    sel_col_num<-c()
    if (input$sum_radio=="Gender") {
      sel_col_num<-c(28:29)
      df_colnames<-c( "Males", "Females")
    } 
    m <- sum_df %>%
      select(Municipal, school.name, school.year, 
             sel_col_num[1]) %>%
      spread(school.year, sel_col_num[1])
    
    list(
      data=googleDataTable(m))
  })
  
  
    
#     
#     
#      wage_df<-emp_df()
#     munis <- input$plot_muni
#     w <- wage_df %>%
#       filter(Municipal %in% munis) %>%
#       select(Municipal, Year, Inflation_Adjusted_Average_Weekly_Wage) %>%
#       spread(Municipal, Inflation_Adjusted_Average_Weekly_Wage)
#     list(
#       data=googleDataTable(w))
  # })
#   
#   output$Wage_pct_plot<-reactive({
#     wage_df<-emp_df()
#     munis <- input$plot_muni
#     w_pct <- wage_df %>%
#       filter(Municipal %in% munis) %>%
#       select(Municipal, Year, Average_Weekly_Wage_difference) %>%
#       spread(Municipal, Average_Weekly_Wage_difference)
#     list(
#       data=googleDataTable(w_pct))
#   })
#     
#   ## for the Google charts plot
#   output$Emp_plot1 <- reactive({
# #   browser()
#     ## make reactive dataframe into regular dataframe
#     emp_df <- emp_df()
# 
#     ## make region a vector based on input variable
#     munis <- input$plot_muni
# 
#     ## if counties are selected and MA or US mean boxes are selected, add those to dataframe
# #     if(!is.null(input$plot_muni)){
# #       if(input$MA_mean)
# #         munis <- c(plot_muni, "MA")
# #
# #       if(input$US_mean)
# #         munis <- c(munis, "United States")
# #     }
#   
#       ##selecting rows based on the chosen school name
#             if(!is.null(input$school_name))
#           sname <- input$school_name
#         ## if none selected, put no schools in vector
#         ## For nothing clicked
#         if(is.null(input$school_name))
#          sname <- all_schools
#         sum_df <- sum_df %>%
#          filter(school.name %in% sname)
#   
# 
#     ## if no counties have been selected, just show the US average
# #     if(is.null(input$plot_muni)){
# #       ## make region a vector based on input variable
# #       munis <- "MA"
# #     }
# 
#     ## put data into form that googleCharts understands (this unmelts the dataframe)
# #      g <- dcast(emp_df, Year ~ munis, value.var="Average_Monthly_Employment")
# g <- emp_df %>%
#     filter(Municipal %in% munis) %>%
#     select( Municipal, Year, Average_Monthly_Employment) %>%
#     spread(Municipal, Average_Monthly_Employment)
# 
# #     g <- emp_df %>%
# #       filter(Municipal %in% munis) %>%
# #       select( Municipal, Year, Average_Monthly_Employment) %>%
# #       spread(Municipal, Average_Monthly_Employment)
# 
# 
#     ## this outputs the google data to be used in the UI to create the dataframe
#     list(
#       data=googleDataTable(g))
#   })
# output$Est_plot1 <- reactive({
#   #   browser()
#   ## make reactive dataframe into regular dataframe
#   emp_df <- emp_df()
#   
#   ## make region a vector based on input variable
#   munis <- input$plot_muni
# 
#   est <- emp_df %>%
#     filter(Municipal %in% munis) %>%
#     select( Municipal, Year, Number_of_Employer_Establishments) %>%
#     spread(Municipal, Number_of_Employer_Establishments)
#   
#     
#   ## this outputs the google data to be used in the UI to create the dataframe
#   list(
#     data=googleDataTable(est))
# })
# 
# output$Emp_pct_plot<-reactive({
#   emp_df<-emp_df()
#   munis <- input$plot_muni
#   emp_pct <- emp_df %>%
#     filter(Municipal %in% munis) %>%
#     select(Municipal, Year, Employment_difference) %>%
#     spread(Municipal, Employment_difference)
#   list(
#     data=googleDataTable(emp_pct))
# })
# 
# output$Est_pct_plot<-reactive({
#   emp_df<-emp_df()
#   munis <- input$plot_muni
#   est_pct <- emp_df %>%
#     filter(Municipal %in% munis) %>%
#     select(Municipal, Year, Establishment_difference) %>%
#     spread(Municipal, Establishment_difference)
#   list(
#     data=googleDataTable(est_pct))
# })
# 
# 
# 
#   ###################MAP CREATION##############
# 
#   ## set map colors
#   map_dat <- reactive({
#     ## Browser command - Stops the app right when it's about to break
#     ## make reactive dataframe into regular dataframe
#     emp_df <- emp_df()
#     ## subset the data by the year selected
#     emp_df <- filter(emp_df, Year==input$map_year)
#     ## get column name and cuts based on input
#     if (input$map_display_radio == "Actual Values"){
#       if (input$map_radio == "Employment") {
#         col<-"Average_Monthly_Employment"  
#         cuts<-empcuts
#       }
#       else if (input$map_radio == "Establishments"){
#         col<-"Number_of_Employer_Establishments"
#         cuts<-estcuts
#       }
#       else {
#         col<-"Inflation_Adjusted_Average_Weekly_Wage"
#         cuts<-wagecuts
#       }
#    
#     ## assign colors to each entry in the data frame
#     col_sel_num1<-which( colnames(emp_df)==col )
#     map_dat <- select(emp_df,c(1,3,col_sel_num1))
#     color <- as.integer(cut2(map_dat[,col],cuts=cuts))
#     map_dat <- cbind.data.frame(map_dat, color)
#     map_dat$color <- ifelse(is.na(map_dat$color), length(map_colors),
#                             map_dat$color)
#     map_dat$opacity <- 0.7
#     
#     ## find missing counties in data subset and assign NAs to all values
#     missing.munis <- setdiff(leftover_munis_map,emp_df$Municipal)
#     missing_df <-data.frame(Municipal=missing.munis, Year=input$map_year, Map_var=NA,
#                             color=length(map_colors), opacity = 0)
#     colnames(missing_df)[3]<-col
#     # combine data subset with missing counties data
#     map_dat <- rbind.data.frame(map_dat, missing_df)
#     map_dat$color <- map_colors[map_dat$color]
#     return(map_dat)
#     } 
#     if (input$map_display_radio == "Change_Pct"){
#       if (input$map_radio == "Employment") {
#         col<-"Employment_difference"  
#         cuts<-pctcuts
#       }
#       else if (input$map_radio == "Establishments"){
#         col<-"Establishment_difference"
#         cuts<-pctcuts
#       }
#       else {
#         col<-"Average_Weekly_Wage_difference"
#         cuts<-pctcuts
#       }
#       col_sel_num1<-which( colnames(emp_df)==col )
#       map_dat <- select(emp_df,c(1,3,col_sel_num1))
#       color <- as.integer(cut2(map_dat[,col],cuts=cuts))
#       map_dat <- cbind.data.frame(map_dat, color)
#       map_dat$color <- ifelse(is.na(map_dat$color), length(pctmap_colors),
#                               map_dat$color)
#       map_dat$opacity <- 0.7
#       
#       ## find missing counties in data subset and assign NAs to all values
#       missing.munis <- setdiff(leftover_munis_map,emp_df$Municipal)
#       missing_df <-data.frame(Municipal=missing.munis, Year=input$map_year, Map_var=NA,
#                               color=length(pctmap_colors), opacity = 0)
#       colnames(missing_df)[3]<-col
#       # combine data subset with missing counties data
#       map_dat <- rbind.data.frame(map_dat, missing_df)
#       map_dat$color <- pctmap_colors[map_dat$color]
#       return(map_dat)
#          }
#    })
# 
#   values <- reactiveValues(selectedFeature=NULL, highlight=c())
# 
#   ## draw leaflet map
#   map <- createLeafletMap(session, "map")
# 
#   ## the functions within observe are called when any of the inputs are called
# 
#   ## Does nothing until called (done with action button)
#   observe({
#     input$action
# 
#     ## load in relevant map data
#     map_dat <- map_dat()
#     col_name<-colnames(map_dat)[3]
# 
#     ## All functions which are isolated, will not run until the above observe function is activated
#     isolate({
#       ## Duplicate MAmap to x
#       x <- MA_map_muni
#       #     browser()
#       ## for each county in the map, attach the Crude Rate and colors associated
#       for(i in 1:length(x$features)){
#         ## Each feature is a Municipal
#         x$features[[i]]$properties[col_name] <- map_dat[match(x$features[[i]]$properties$NAMELSAD10, map_dat$Municipal), col_name]
#         ## Style properties
#         x$features[[i]]$properties$style <- list(
#           fill=TRUE,
# 
#           ## Fill color has to be equal to the map_dat color and is matched by county
#           fillColor = map_dat$color[match(x$features[[i]]$properties$NAMELSAD10, map_dat$Municipal)],
#           ## "#000000" = Black, "#999999"=Grey,
#           weight=1, stroke=TRUE,
#           opacity=map_dat$opacity[match(x$features[[i]]$properties$NAMELSAD10, map_dat$Municipal)],
#           color="#000000",
#           fillOpacity=map_dat$opacity[match(x$features[[i]]$properties$NAMELSAD10, map_dat$Municipal)])
#       }
# 
#       map$addGeoJSON(x) # draw map
#     })
#   })
# 
#   observe({
#     ## EVT = Mouse Click
#     evt <- input$map_click
#     if(is.null(evt))
#       return()
#     
#     isolate({
#       values$selectedFeature <- NULL
#     })
#   })
# 
#   observe({
#     evt <- input$map_geojson_click
#     if(is.null(evt))
#       return()
#     map_dat <- map_dat()
#     col_name<-colnames(map_dat)[3]
#     
#     isolate({
#       values$selectedFeature <- evt$properties
#       region <- evt$properties$NAMELSAD10
#       values$selectedFeature[col_name] <- map_dat[match(region, map_dat$Municipal), col_name]
#     })
#   })
#   ##  This function is what creates info box
# 
#   output$details <- renderText({
#     map_dat <- map_dat()
#     col_name<-colnames(map_dat)[3]
#     ## Before a county is clicked, display a message
#     if(is.null(values$selectedFeature)){
#       return(as.character(tags$div(
#         tags$div(
#           h4("Click on a town or city"))
#       )))
#     }
#     muni_name <- values$selectedFeature$NAMELSAD10
#     if(input$map_display_radio == "Actual Values"){
#     muni_value <- prettyNum(values$selectedFeature[col_name], big.mark = ",")
#     }
#     if(input$map_display_radio == "Change_Pct"){
#       muni_value <- prettyNum(values$selectedFeature[col_name],digits=4)
#     }
#     var_select <- gsub("Pct", "", col_name)
#     var_select <- gsub("_", " ", var_select)
#     var_select <- gsub("Employer", "Business", var_select)
# 
#     ## If clicked county has no crude rate, display a message
#     if(muni_value == "NULL"|| muni_value =="NA"){
#       return(as.character(tags$div(
#         tags$h4(var_select, muni_name, "is not available for this timespan"))))
#     }
#     ## For a single year when county is clicked, display a message
# 
#     if(input$map_display_radio == "Actual Values"){
#       if(input$map_radio =='Wages'){
#     return(as.character(tags$div(
#       tags$h4("Average Weekly Wage (2012 dollars)", "in", muni_name, " for ", input$map_year),
#       tags$h5("$",muni_value)
#     )))
#     }
#     else {
#       return(as.character(tags$div(
#         tags$h4(var_select, "in", muni_name, " for ", input$map_year),
#         tags$h5(muni_value)
#       )))
#     }
#     }
#     if(input$map_display_radio == "Change_Pct"){
#       
#       return(as.character(tags$div(
#         tags$h4(var_select, "in", muni_name, " for ", input$map_year,"compared to year 2003"),
#         tags$h5(muni_value,"%")
#       )))
#     }
# 
#   })
# 
  
  ###################################################################
  #  MAP in googlevis
  ####################################################################
  
  
  output$mapvar_levels <- renderUI({
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$map_radio,
           
           "Race/Ethnicity" = selectInput("map_level","Choose Level to map",
                                          choices = 
                                            c("African American" = "African.American",
                                              "Asian" = "Asian",
                                              "Hispanic" = "Hispanic",
                                              "White" = "White",
                                              "Native American" = "Native.American",
                                              "Native Hawaiian/Pacific Islander" = "Native.Hawaiian.Pacific.Islander",
                                              "Multi-Race Non-Hispanic" = "Multi.Race.Non.Hispanic"),
                                          selected = "African.American"
           ), 
           "Gender"= selectInput("map_level","Choose Level to map",
                                 choices = 
                                   c("Female" = "Females",
                                     "Males" = "Males"),
                                 selected = "Females"
           ), 
           "Grade Level"= selectInput("map_level","Choose Level to map",
                                      choices = 
                                        c("Pre-Kindergarden" = "Pre.Kindergarden",
                                          "Kindergarden" = "Kindergarden",
                                          "First Grade" = "First.Grade",
                                          "Second Grade" = "Second.Grade",
                                          "Third Grade" = "Third.Grade",
                                          "Fourth Grade" = "Fourth.Grade",
                                          "Fifth Grade" = "Fifth.Grade",
                                          "Sixth Grade" = "Sixth.Grade",
                                          "Seventh Grade" = "Seventh.Grade",
                                          "Eight Grade" = "Eight.Grade",
                                          "Ninth Grade" = "Ninth.Grade",
                                          "Tenth Grade" = "Tenth.Grade",
                                          "Eleventh Grade" = "Eleventh.Grade",
                                          "Twelfth Grade" = "Twelfth.Grade",
                                          "Special Education Beyond 12th Grade" = "Special.Ed.Beyond.12th.Grade"),
                                      selected = "Pre.Kindergarden"
           ), 
           "English Language Learners"= selectInput("map_level","Choose Level to map",
                                                    choices = 
                                                      c("Count of Students: English Language Learners" = "English.Language.Learner...enrolled.",
                                                        "Percent of Students: English Language Learners" = "English.Language.Learner...enrolled..1",
                                                        "Churn Enrollment: English Language Learners" = "Churn.Enrollment.for.English.Language.Learning.Students",
                                                        "Churn Rate: English Language Learners" = "Churn.Rate.for.English.Language.Learning.Students",
                                                        "Intake Rate: English Language Learners" = "Intake.Rate.for.English.Language.Learning.Students",
                                                        "Stability Enrollment: English Language Learners" = "Stability.Enrollment.for.English.Language.Learning.Students",
                                                        "Stability Rate: English Language Learners" = "Stability.Rate.for.English.Language.Learning.Students"),
                                                    selected = "English.Language.Learner...enrolled."
           ),
           "Students with Disabilities"= selectInput("map_level","Choose Level to map",
                                                     choices = 
                                                       c("Count of Students: Students with Disabilities" = "Students.with.Disabilities...enrolled.",
                                                         "Percent of Students: Students with Disabilities" = "Students.with.Disabilities...enrolled..1",
                                                         "Churn Enrollment: Students with Disabilities" = "Churn.Enrollment.for.Students.with.Disabilities",
                                                         "Churn Rate: Students with Disabilities" = "Churn.Rate.for.Students.with.Disabilities",
                                                         "Intake Rate: Students with Disabilities" = "Intake.Rate.for.Students.with.Disabilities",
                                                         "Stability Enrollment: Students with Disabilities" = "Stability.Enrollment.for.Students.with.Disabilities",
                                                         "Stability Rate: Students with Disabilities" = "Stability.Rate.for.Students.with.Disabilities"),
                                                     selected = "Students.with.Disabilities...enrolled."
           ), 
           "Low Income"= selectInput("map_level","Choose Level to map",
                                     choices = 
                                       c("Count of Students: Low Income" = "Low.Income...enrolled.",
                                         "Percent of Students: Low Income" = "Low.Income...enrolled..1",
                                         "Churn Enrollment: Low Income" = "Churn.Enrollment.for.Low.Income.Students",
                                         "Churn Rate: Low Income" = "Churn.Rate.for.Low.Income.Students",
                                         "Intake Rate: Low Income" = "Intake.Rate.for.Low.Income.Students",
                                         "Stability Enrollment: Low Income" = "Stability.Enrollment.for.Low.Income.Students",
                                         "Stability Rate: Low Income" = "Stability.Rate.for.Low.Income.Students"),
                                     selected = "Low.Income...enrolled."
           ), 
           "High Needs"= selectInput("map_level","Choose Level to map",
                                     choices = 
                                       c("Count of Students: High Needs" = "High.Needs.Students...enrolled.",
                                         "Percent of Students: High Needs" = "High.Needs.Students...enrolled..1",
                                         "Churn Enrollment: High Needs" = "Churn.Enrollment.for.High.Needs.Students",
                                         "Churn Rate: High Needs" = "Churn.Rate.for.High.Needs.Students",
                                         "Intake Rate: High Needs" = "Intake.Rate.for.High.Needs.Students",
                                         "Stability Enrollment: High Needs" = "Stability.Enrollment.for.High.Needs.Students",
                                         "Stability Rate: High Needs" = "Stability.Rate.for.High.Needs.Students"),
                                     selected = "High.Needs.Students...enrolled."
           )
           
    )
  })
  
  ##
  #
  # use input$map_year to choose rows
  # use input$map_schooltype to choose rows
  # use input$map_level to choose columns
  ##
  
  
  #title
  output$map_title <- renderText({
    paste(input$map_level, "in Massachusetts", input$map_schooltype, 
          "during the", input$map_year, "school year")
  })
  
  output$gvis<-renderGvis({
    
    ## filter dataframe
    
    map_df <- edu_data %>%
      select(1,3,4,6, which(names(map_df)==input$map_level), 22,67:74) %>%
      filter(school.year==input$map_year)
    switch(input$map_schooltype, 
           "prek"= map_df <- filter(map_df, PREK==1),
           "kindergarten"= map_df <-filter(map_df, KIND==1),
           "elementary"= map_df <-filter(map_df, ELEM==1),
           "middle"= map_df <-filter(map_df, MIDD==1),
           "high" = map_df <-filter(map_df, HIGH==1))
    
  
    
   gvisGeoChart(map_df,
                 locationvar="LatLong", sizevar="Total.Students.Enrolled",
                 colorvar=input$map_level, hovervar="school.name",
                 options=list(region="US-MA", displayMode="Markers", 
                              resolution="metros",
                              width=750, height=600,
                              magnifyingGlass=T,
                              colorAxis="{colors:['#FFFFFF', '#0000FF']}"
                 ))
    })
  
  
  
  
})
