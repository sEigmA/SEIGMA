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

    if(input$sum_county!=" "){
    county<-input$sum_county}
else if(input$sum_county==" "){
  county<-MA_county}


    if(!is.null(input$sum_muni))
      munis <- input$sum_muni
    ## if none selected, put all municipals in vector
    ## For nothing clicked
    if(is.null(input$sum_muni))
      munis <- MA_municipals
    
sum_df <- df %>% filter(County %in% county & Municipal %in% munis)
    

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
      sel_col_num<-c(7:21)
      df_colnames<-names(edu_data)[7:21]
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
    arrange(County, Municipal, school.name)%>%
      select(County, Municipal, school.name, school.year, Total.Students.Enrolled, 
             sel_col_num)
    
    final_colnames<-c("County", "Municipal","School Name","School Year", "Total Number Enrolled",
                      df_colnames)
  
    colnames(sum_df) <- final_colnames

    ## create a dataframe consisting only of counties in vector
return(sum_df)
  }, options = list(searching = FALSE, orderClasses = TRUE))

  
  output$sum_muniui <-renderUI({
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
  
    
    switch(input$sum_county,
           
           " "=selectInput("sum_muni", "Choose Municipality",
                          choices= c(MA_municipals, " " ), multiple = T),
           
           "Barnstable" = selectInput("sum_muni","Choose Municipality",
                                          choices = as.character(unique(edu_data$Municipal[which(edu_data$County=="Barnstable")])) 
                                            ,
                                          multiple = T
           ), 
           "Berkshire" = selectInput("sum_muni","Choose Municipality",
                                      choices = as.character(unique(edu_data$Municipal[which(edu_data$County=="Berkshire")])) 
                                      ,
                                      multiple = T
           ),
           "Bristol" = selectInput("sum_muni","Choose Municipality",
                                      choices = as.character(unique(edu_data$Municipal[which(edu_data$County=="Bristol")])) 
                                      ,
                                      multiple = T
           ), 
           "Dukes" = selectInput("sum_muni","Choose Municipality",
                                      choices = as.character(unique(edu_data$Municipal[which(edu_data$County=="Dukes")])) 
                                      ,
                                      multiple = T
           ), 
            "Essex" = selectInput("sum_muni","Choose Municipality",
                                        choices = as.character(unique(edu_data$Municipal[which(edu_data$County=="Essex")])) 
                                        ,
                                        multiple = T
           ),
            "Franklin" = selectInput("sum_muni","Choose Municipality",
                                        choices = as.character(unique(edu_data$Municipal[which(edu_data$County=="Franklin")])) 
                                        ,
                                        multiple = T
           ), 
           "Hampden" = selectInput("sum_muni","Choose Municipality",
                                         choices = as.character(unique(edu_data$Municipal[which(edu_data$County=="Hampden")])) 
                                         ,
                                         multiple = T
           ), 
           "Hampshire" = selectInput("sum_muni","Choose Municipality",
                                      choices = as.character(unique(edu_data$Municipal[which(edu_data$County=="Hampshire")])) 
                                      ,
                                      multiple = T
           ), 
           "Middlesex" = selectInput("sum_muni","Choose Municipality",
                                      choices = as.character(unique(edu_data$Municipal[which(edu_data$County=="Middlesex")])),
                                      multiple = T
           ),
           "Nantucket" = selectInput("sum_muni","Choose Municipality",
                                      choices = as.character(unique(edu_data$Municipal[which(edu_data$County=="Nantucket")])) ,
                                      multiple = T
           ),
           "Norfolk" = selectInput("sum_muni","Choose Municipality",
                                      choices = as.character(unique(edu_data$Municipal[which(edu_data$County=="Norfolk")])) 
                                      ,
                                      multiple = T
           ),
           "Plymouth" = selectInput("sum_muni","Choose Municipality",
                                      choices = as.character(unique(edu_data$Municipal[which(edu_data$County=="Plymouth")])) 
                                      ,
                                      multiple = T
           ),
           "Suffolk" = selectInput("sum_muni","Choose Municipality",
                                      choices = as.character(unique(edu_data$Municipal[which(edu_data$County=="Suffolk")])) 
                                      ,
                                      multiple = T
           ),
           "Worcester" = selectInput("sum_muni","Choose Municipality",
                                      choices = as.character(unique(edu_data$Municipal[which(edu_data$County=="Worcester")])) 
                                      ,
                                      multiple = T
           ))
  })

  
    ##end of summary

           
  #plot dataframe          
  r_plot_df<-reactive({
    
    
    sel_col_num<-c()
    df_colnames<-c()
    if (input$plot_radio=="Race/Ethnicity") {
      sel_col_num<-c(77:83)
      df_colnames<-c("African American", "Asian", "Hispanic", "White", "Native American", "Native Hawaiian Pacific Islander", "Multi-race non-Hispanic")
      
      plot_df <- edu_data %>%
        filter(school.name==input$plot_school) %>%
        select(1,3, 6, sel_col_num) %>% 
        arrange(school.year)
      colnames(plot_df)[4:ncol(plot_df)]<-c(df_colnames)
      
      col_sums<-apply(plot_df[,4:ncol(plot_df)], 2, FUN=function(x){sum(x, na.rm=T)})
      remove_cols<-as.numeric(which(col_sums==0))
      
    } else if (input$plot_radio=="Gender") {
      sel_col_num<-c(75:76)
      df_colnames<-c( "Males", "Females")
      
      plot_df <- edu_data %>%
        filter(school.name==input$plot_school) %>%
        select(1,3, 6, sel_col_num) %>% 
        arrange(school.year)
      colnames(plot_df)[4:ncol(plot_df)]<-c(df_colnames)
      
      col_sums<-apply(plot_df[,4:ncol(plot_df)], 2, FUN=function(x){sum(x, na.rm=T)})
      remove_cols<-as.numeric(which(col_sums==0))
      
    } else if (input$plot_radio=="Grade Level") {
      sel_col_num<-c(7:21)
      df_colnames<-names(edu_data)[7:21]
      
      plot_df <- edu_data %>%
        filter(school.name==input$plot_school) %>%
        select(1,3, 6, sel_col_num) %>% 
        arrange(school.year)
      colnames(plot_df)[4:ncol(plot_df)]<-c(df_colnames)
      
      col_sums<-apply(plot_df[,4:ncol(plot_df)], 2, FUN=function(x){sum(x, na.rm=T)})
      remove_cols<-as.numeric(which(col_sums==0))
      
      #ELL
    } else if (input$plot_radio=="English Language Learners" & input$plot_level=="English Language Learners Enrolled") {
      sel_col_num<-c(85, 34)
      df_colnames<-c("Others", "English Language Learners Enrolled")
      
      plot_df <- edu_data %>%
        filter(school.name==input$plot_school) %>%
        select(1,3, 6, sel_col_num) %>% 
        arrange(school.year)
      
      colnames(plot_df)[4:ncol(plot_df)]<-c(df_colnames)
      remove_cols<-NULL
    } else if (input$plot_radio=="English Language Learners" & input$plot_level=="First Language Not English Enrolled") {
      sel_col_num<-c(84, 32)
      df_colnames<-c("Others", "First Language Not English Enrolled")
      
      plot_df <- edu_data %>%
        filter(school.name==input$plot_school) %>%
        select(1,3, 6, sel_col_num) %>% 
        arrange(school.year)
      
      colnames(plot_df)[4:ncol(plot_df)]<-c(df_colnames)
      remove_cols<-NULL
    } else if (input$plot_radio=="English Language Learners" & input$plot_level=="English Language Learners Mobility Enrollment") {
      sel_col_num<-c(51, 54)
      df_colnames<-c("Churn Enrollment for English Language Learning Students",
                     "Stability Enrollment for English Language Learning Students")
      
      plot_df <- edu_data %>%
        filter(school.name==input$plot_school) %>%
        select(1,3, 6, sel_col_num) %>% 
        arrange(school.year)
      
      colnames(plot_df)[4:ncol(plot_df)]<-c(df_colnames)
      remove_cols<-NULL
      
    } else if (input$plot_radio=="English Language Learners" & input$plot_level=="English Language Learners Mobility Rate") {
      sel_col_num<-c(52, 53, 55)
      df_colnames<-c("Churn Rate for English Language Learning Students",
                     "Intake Rate for English Language Learning Students",
                     "Stability Rate for English Language Learning Students")
      
      plot_df <- edu_data %>%
        filter(school.name==input$plot_school) %>%
        select(1,3, 6, sel_col_num) %>% 
        arrange(school.year)
      
      colnames(plot_df)[4:ncol(plot_df)]<-c(df_colnames)
      remove_cols<-NULL
      
    #DISAB
  } else if (input$plot_radio=="Students with Disabilities" & input$plot_level=="Students with Disabilities Enrolled") {
    sel_col_num<-c(86, 36)
    df_colnames<-c("Others", "Students with Disabilities Enrolled")
    
    plot_df <- edu_data %>%
      filter(school.name==input$plot_school) %>%
      select(1,3, 6, sel_col_num) %>% 
      arrange(school.year)
    
    colnames(plot_df)[4:ncol(plot_df)]<-c(df_colnames)
    remove_cols<-NULL
    } else if (input$plot_radio=="Students with Disabilities" & input$plot_level=="Disabilities Mobility Enrollment") {
    sel_col_num<-c(46, 49)
    df_colnames<-c("Churn Enrollment for Students with Disabilities",
                   "Stability Enrollment for Students with Disabilities")
    
    plot_df <- edu_data %>%
      filter(school.name==input$plot_school) %>%
      select(1,3, 6, sel_col_num) %>% 
      arrange(school.year)
    
    colnames(plot_df)[4:ncol(plot_df)]<-c(df_colnames)
    remove_cols<-NULL
  
  } else if (input$plot_radio=="Students with Disabilities" & input$plot_level=="Disabilities Mobility Rate") {
    sel_col_num<-c(47, 48, 50)
    df_colnames<-c("Churn Rate for Students with Disabilities",
                   "Intake Rate for Students with Disabilities",
                   "Stability Rate for Students with Disabilities")
    
    plot_df <- edu_data %>%
      filter(school.name==input$plot_school) %>%
      select(1,3, 6, sel_col_num) %>% 
      arrange(school.year)
    colnames(plot_df)[4:ncol(plot_df)]<-c(df_colnames)
    remove_cols<-NULL
    
  #LowIncome
} else if (input$plot_radio=="Low Income" & input$plot_level=="Low Income Students Enrolled") {
  sel_col_num<-c(87, 38)
  df_colnames<-c("Others", "Low Income Students Enrolled")
  
  plot_df <- edu_data %>%
    filter(school.name==input$plot_school) %>%
    select(1,3, 6, sel_col_num) %>% 
    arrange(school.year)
  colnames(plot_df)[4:ncol(plot_df)]<-c(df_colnames)
  
  remove_cols<-NULL
  
} else if (input$plot_radio=="Low Income" & input$plot_level=="Low Income Mobility Enrollment") {
  sel_col_num<-c(61, 64)
  df_colnames<-c("Churn Enrollment for Low Income Students",
                 "Stability Enrollment for Low Income Students")
  
  plot_df <- edu_data %>%
    filter(school.name==input$plot_school) %>%
    select(1,3, 6, sel_col_num) %>% 
    arrange(school.year)
  colnames(plot_df)[4:ncol(plot_df)]<-c(df_colnames)
  
  remove_cols<-NULL
} else if (input$plot_radio=="Low Income" & input$plot_level=="Low Income Mobility Rate") {
  sel_col_num<-c(62, 63, 65)
  remove_cols<-NULL
  df_colnames<-c("Churn Rate for Low Income Students",
                 "Intake Rate for Low Income Students",
                 "Stability Rate for Low Income Students")
#High Needs
} else if (input$plot_radio=="High Needs" & input$plot_level=="High Needs Students Enrolled") {
  sel_col_num<-c(88, 44)
  df_colnames<-c("Others", "High Needs Students Enrolled")
  
  plot_df <- edu_data %>%
    filter(school.name==input$plot_school) %>%
    select(1,3, 6, sel_col_num) %>% 
    arrange(school.year)
  colnames(plot_df)[4:ncol(plot_df)]<-c(df_colnames)
  
  remove_cols<-NULL
} else if (input$plot_radio=="High Needs" & input$plot_level=="High Needs Mobility Enrollment") {
  sel_col_num<-c(56, 59)
  df_colnames<-c("Churn Enrollment for High Needs Students",
                 "Stability Enrollment for High Needs Students")
  
  plot_df <- edu_data %>%
    filter(school.name==input$plot_school) %>%
    select(1,3, 6, sel_col_num) %>% 
    arrange(school.year)
  colnames(plot_df)[4:ncol(plot_df)]<-c(df_colnames)
  remove_cols<-NULL
  
} else if (input$plot_radio=="High Needs" & input$plot_level=="High Needs Mobility Rate") {
  sel_col_num<-c(57, 58, 60)
  df_colnames<-c("Churn Rate for High Needs Students",
                 "Intake Rate for High Needs Students",
                 "Stability Rate for High Needs Students")
  
  plot_df <- edu_data %>%
    filter(school.name==input$plot_school) %>%
    select(1,3, 6, sel_col_num) %>% 
    arrange(school.year)
  colnames(plot_df)[4:ncol(plot_df)]<-c(df_colnames)
  remove_cols<-NULL
  }
    
    
    
    #drop columns that have nothing in them: only important for grade level, gender and race legend
    
    #otherwise keep all selected columns
    #if any mobilitygroup is chosen, refine columns to get 
    #-students enrolled (plus total-subgroup) [stacked bar, %]
    #-enrollment [stacked bar, %]
          #-rates [stacked bar]
          
        
          return(plot_df[complete.cases(plot_df),-c(1,2, remove_cols)])
  })
    
  
  
  output$plot_schoolui <-renderUI({
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    
    
    switch(input$plot_county,
           
           " "=selectInput("plot_school", "Choose School",
                           choices= c(all_schools), multiple = F, selected=NULL),
           
           "Barnstable" = selectInput("plot_school","Choose School",
                                      choices = as.character(all_school_table$"Barnstable"),
                                      selected=as.character(all_school_table$"Barnstable")[1],
                                      multiple = F), 
           "Berkshire" = selectInput("plot_school","Choose School",
                                     choices = as.character(all_school_table$"Berkshire"),
                                     selected=as.character(all_school_table$"Berkshire")[1],
                                     multiple = F),
           "Bristol" = selectInput("plot_school","Choose School",
                                   choices = as.character(all_school_table$"Bristol"),
                                   selected=as.character(all_school_table$"Bristol")[1],
                                   multiple = F), 
           "Dukes" = selectInput("plot_school","Choose School",
                                 choices = as.character(all_school_table$"Essex"),
                                 selected=as.character(all_school_table$"Essex")[1],
                                 multiple = F), 
           "Essex" = selectInput("plot_school","Choose School",
                                 choices = as.character(all_school_table$"Barnstable"),
                                 selected=as.character(all_school_table$"Barnstable")[1],
                                 multiple = F),
           "Franklin" = selectInput("plot_school","Choose School",
                                    choices = as.character(all_school_table$"Franklin"),
                                    selected=as.character(all_school_table$"Franklin")[1],
                                    multiple = F), 
           "Hampden" = selectInput("plot_school","Choose School",
                                   choices = as.character(all_school_table$"Hampden"),
                                   selected=as.character(all_school_table$"Hampden")[1],
                                   multiple = F), 
           "Hampshire" = selectInput("plot_school","Choose School",
                                     choices = as.character(all_school_table$"Middlesex"),
                                     selected=as.character(all_school_table$"Middlesex")[1],
                                     multiple = F), 
           "Middlesex" = selectInput("plot_school","Choose School",
                                     choices = as.character(all_school_table$"Barnstable"),
                                     selected=as.character(all_school_table$"Barnstable")[1],
                                     multiple = F),
           "Nantucket" = sselectInput("plot_school","Choose School",
                                      choices = as.character(all_school_table$"Nantucket"),
                                      selected=as.character(all_school_table$"Nantucket")[1],
                                      multiple = F),
           "Norfolk" = selectInput("plot_school","Choose School",
                                   choices = as.character(all_school_table$"Norfolk"),
                                   selected=as.character(all_school_table$"Norfolk")[1],
                                   multiple = F),
           "Plymouth" = selectInput("plot_school","Choose School",
                                    choices = as.character(all_school_table$"Plymouth"),
                                    selected=as.character(all_school_table$"Plymouth")[1],
                                    multiple = F),
           "Suffolk" = selectInput("plot_school","Choose School",
                                   choices = as.character(all_school_table$"Suffolk"),
                                   selected=as.character(all_school_table$"Suffolk")[1],
                                   multiple = F),
           "Worcester" = selectInput("plot_school","Choose School",
                                     choices = as.character(all_school_table$"Worcester"),
                                     selected=as.character(all_school_table$"Worcester")[1],
                                     multiple = F))
  })
  
  
  
  
    
    output$colplot<-reactive({
      #message to be displayed when no school selected
      
      r_plot_df<-r_plot_df()
      
      validate(
        need(is.null(input$plot_school)==FALSE, "Please select a school")
      )
     
      
#       if(input$plot_radio=="Race/Ethnicity"){
#         #call the plot for race/ethnicity
#         #first edit the options from the reactive dataframe
#         raceplot_options$options$hAxis$ticks<-seq(min(r_plot_df$school.year, na.rm=T), 
#                                           max(r_plot_df$school.year, na.rm=T), 1)
#         raceplot_options$options$hAxis$viewWindow$min<-min(r_plot_df$school.year, na.rm=T)
#         raceplot_options$options$hAxis$viewWindow$max<-max(r_plot_df$school.year, na.rm=T)
#       }
        #now call the plot using the options defined in global
        #and the edits provided from the reactive dataframe
        list(
               data=googleDataTable(r_plot_df),
               title=paste(input$plot_radio, "in", input$plot_school)
             )
        
      })
    
#     output$genderplot<-reactive({
#       #message to be displayed when no school selected
#       
#       r_plot_df<-r_plot_df()
#       
#       validate(
#         need(is.null(input$plot_school)==FALSE, "Please select a school")
#       )
#       
# #       if(input$plot_radio=="Gender"){
# #         
# #         #first edit the options from the reactive dataframe
# #         genderplot_options$options$hAxis$ticks<-seq(min(r_plot_df$school.year, na.rm=T), 
# #                                           max(r_plot_df$school.year, na.rm=T), 1)
# #         genderplot_options$options$hAxis$viewWindow$min<-min(r_plot_df$school.year, na.rm=T)
# #         genderplot_options$options$hAxis$viewWindow$max<-max(r_plot_df$school.year, na.rm=T)
# #       }
#       
#         list(
#                data=googleDataTable(r_plot_df),
#                title=paste(input$plot_radio, "in", input$plot_school)
#              )
#     })
#     
#     output$gradelevelplot<-reactive({
#       #message to be displayed when no school selected
#       
#       r_plot_df<-r_plot_df()
#       
#       validate(
#         need(is.null(input$plot_school)==FALSE, "Please select a school")
#       )
#       
# #     if(input$plot_radio=="Grade Level"){
# #       
# #       #first edit the options from the reactive dataframe
# #       gradelevelplot_options$options$hAxis$ticks<-seq(min(r_plot_df$school.year, na.rm=T), 
# #                                           max(r_plot_df$school.year, na.rm=T), 1)
# #       gradelevelplot_options$options$hAxis$viewWindow$min<-min(r_plot_df$school.year, na.rm=T)
# #       gradelevelplot_options$options$hAxis$viewWindow$max<-max(r_plot_df$school.year, na.rm=T)
# #     }
#       list(
#                data=googleDataTable(r_plot_df),
#                title=paste(input$plot_radio, "in", input$plot_school)
#              )
#       
#     })
#       
#     
#     output$ELL_enrolledplot<-reactive({
#       #message to be displayed when no school selected
#       
#       r_plot_df<-r_plot_df()
#       
#       validate(
#         need(is.null(input$plot_school)==FALSE, "Please select a school")
#       )
#       
#       list(
#         data=googleDataTable(r_plot_df),
#         title=paste(input$plot_radio, "in", input$plot_school)
#       )
#       
#     })
#     
#     output$FLNL_enrolledplot<-reactive({
#       #message to be displayed when no school selected
#       
#       r_plot_df<-r_plot_df()
#       
#       validate(
#         need(is.null(input$plot_school)==FALSE, "Please select a school")
#       )
#       
#       list(
#         data=googleDataTable(r_plot_df),
#         title=paste(input$plot_radio, "in", input$plot_school)
#       )
#       
#     })
#     
#     output$ELL_mobenrollmentplot<-reactive({
#       #message to be displayed when no school selected
#       
#       r_plot_df<-r_plot_df()
#       
#       validate(
#         need(is.null(input$plot_school)==FALSE, "Please select a school")
#       )
#       
# #       if(input$plot_level=="English Language Learners Mobility Enrollment"){
# #         #first edit the options from the reactive dataframe
# #         ELL_mobenrollmentplot_options$options$vAxis$ticks<-seq(0, 
# #                                                    sum(r_plot_df[,c(2,3)]), 10)
# #         ELL_mobenrollmentplot_options$options$vAxis$viewWindow$min<-0
# #         ELL_mobenrollmentplot_options$options$vAxis$viewWindow$max<-sum(r_plot_df[,c(2,3)])
# #       }
#         list(
#           data=googleDataTable(r_plot_df),
#           title=paste(input$plot_level, "of", input$plot_radio, "in", input$plot_school)
#         )
#       
#     })
#     
#     output$ELL_mobrateplot<-reactive({
#       #message to be displayed when no school selected
#       
#       r_plot_df<-r_plot_df()
#       
#       validate(
#         need(is.null(input$plot_school)==FALSE, "Please select a school")
#       )
#       
#         list(
#           data=googleDataTable(r_plot_df),
#           title=paste(input$plot_level, "of", input$plot_radio, "in", input$plot_school)
#         )
#         
#       })
#     
#     
#     output$DISAB_enrolledplot<-reactive({
#       #message to be displayed when no school selected
#       
#       r_plot_df<-r_plot_df()
#       
#       validate(
#         need(is.null(input$plot_school)==FALSE, "Please select a school")
#       )
#       
#       list(
#         data=googleDataTable(r_plot_df),
#         title=paste(input$plot_radio, "in", input$plot_school)
#       )
#       
#     })
#     
#     output$DISAB_mobenrollmentplot<-reactive({
#       #message to be displayed when no school selected
#       
#       r_plot_df<-r_plot_df()
#       
#       validate(
#         need(is.null(input$plot_school)==FALSE, "Please select a school")
#       )
#       
# #       if(input$plot_level=="Disabilities Mobility Enrollment"){
# #         #first edit the options from the reactive dataframe
# #         DISAB_mobenrollmentplot_options$options$vAxis$ticks<-seq(0, 
# #                                                            sum(r_plot_df[,c(2,3)]), 10)
# #         DISAB_mobenrollmentplot_options$options$vAxis$viewWindow$min<-0
# #         DISAB_mobenrollmentplot_options$options$vAxis$viewWindow$max<-sum(r_plot_df[,c(2,3)])
# #       }
#       list(
#         data=googleDataTable(r_plot_df),
#         title=paste(input$plot_level, "of", input$plot_radio, "in", input$plot_school)
#       )
#       
#     })
#     
#     output$DISAB_mobrateplot<-reactive({
#       #message to be displayed when no school selected
#       
#       r_plot_df<-r_plot_df()
#       
#       validate(
#         need(is.null(input$plot_school)==FALSE, "Please select a school")
#       )
#       
#       list(
#         data=googleDataTable(r_plot_df),
#         title=paste(input$plot_level, "of", input$plot_radio, "in", input$plot_school)
#       )
#       
#     })
#     
#     output$LOW_enrolledplot<-reactive({
#       #message to be displayed when no school selected
#       
#       r_plot_df<-r_plot_df()
#       
#       validate(
#         need(is.null(input$plot_school)==FALSE, "Please select a school")
#       )
#       
#       list(
#         data=googleDataTable(r_plot_df),
#         title=paste(input$plot_radio, "in", input$plot_school)
#       )
#       
#     })
#     
#     output$LOW_mobenrollmentplot<-reactive({
#       #message to be displayed when no school selected
#       
#       r_plot_df<-r_plot_df()
#       
#       validate(
#         need(is.null(input$plot_school)==FALSE, "Please select a school")
#       )
#       
# #       if(input$plot_level=="Low Income Mobility Enrollment"){
# #         #first edit the options from the reactive dataframe
# #         LOW_mobenrollmentplot_options$options$vAxis$ticks<-seq(0, 
# #                                                            sum(r_plot_df[,c(2,3)]), 10)
# #         LOW_mobenrollmentplot_options$options$vAxis$viewWindow$min<-0
# #         LOW_mobenrollmentplot_options$options$vAxis$viewWindow$max<-sum(r_plot_df[,c(2,3)])
# #       }
#       list(
#         data=googleDataTable(r_plot_df),
#         title=paste(input$plot_level, "of", input$plot_radio, "in", input$plot_school)
#       )
#       
#     })
#     
#     output$LOW_mobrateplot<-reactive({
#       #message to be displayed when no school selected
#       
#       r_plot_df<-r_plot_df()
#       
#       validate(
#         need(is.null(input$plot_school)==FALSE, "Please select a school")
#       )
#       
#       list(
#         data=googleDataTable(r_plot_df),
#         title=paste(input$plot_level, "of", input$plot_radio, "in", input$plot_school)
#       )
#       
#     })
#     
#     output$HINE_enrolledplot<-reactive({
#       #message to be displayed when no school selected
#       
#       r_plot_df<-r_plot_df()
#       
#       validate(
#         need(is.null(input$plot_school)==FALSE, "Please select a school")
#       )
#       
#       list(
#         data=googleDataTable(r_plot_df),
#         title=paste(input$plot_radio, "in", input$plot_school)
#       )
#       
#     })
#     
#     output$HINE_mobenrollmentplot<-reactive({
#       #message to be displayed when no school selected
#       
#       r_plot_df<-r_plot_df()
#       
#       validate(
#         need(is.null(input$plot_school)==FALSE, "Please select a school")
#       )
#       
# #       if(input$plot_level=="High Needs Mobility Enrollment"){
# #         #first edit the options from the reactive dataframe
# #         HINE_mobenrollmentplot_options$options$vAxis$ticks<-seq(0, 
# #                                                            sum(r_plot_df[,c(2,3)]), 10)
# #         HINE_mobenrollmentplot_options$options$vAxis$viewWindow$min<-0
# #         HINE_mobenrollmentplot_options$options$vAxis$viewWindow$max<-sum(r_plot_df[,c(2,3)])
# #       }
#       list(
#         data=googleDataTable(r_plot_df),
#         title=paste(input$plot_level, "of", input$plot_radio, "in", input$plot_school)
#       )
#       
#     })
#     
#     output$HINE_mobrateplot<-reactive({
#       #message to be displayed when no school selected
#       
#       r_plot_df<-r_plot_df()
#       
#       validate(
#         need(is.null(input$plot_school)==FALSE, "Please select a school")
#       )
#       
#       list(
#         data=googleDataTable(r_plot_df),
#         title=paste(input$plot_level, "of", input$plot_radio, "in", input$plot_school)
#       )
#       
#     })
#     
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
#   
#   ###################################################################
#   #  MAP in googlevis
#   ####################################################################
#   
#   
#   output$mapvar_levels <- renderUI({
#     
#     # Depending on input$input_type, we'll generate a different
#     # UI component and send it to the client.
#     switch(input$map_radio,
#            
#            "Race/Ethnicity" = selectInput("map_level","Choose Level to map",
#                                           choices = 
#                                             c("African American" = "African.American",
#                                               "Asian" = "Asian",
#                                               "Hispanic" = "Hispanic",
#                                               "White" = "White",
#                                               "Native American" = "Native.American",
#                                               "Native Hawaiian/Pacific Islander" = "Native.Hawaiian.Pacific.Islander",
#                                               "Multi-Race Non-Hispanic" = "Multi.Race.Non.Hispanic"),
#                                           selected = "African.American"
#            ), 
#            "Gender"= selectInput("map_level","Choose Level to map",
#                                  choices = 
#                                    c("Female" = "Females",
#                                      "Males" = "Males"),
#                                  selected = "Females"
#            ), 
#            "Grade Level"= selectInput("map_level","Choose Level to map",
#                                       choices = 
#                                         c("Pre-Kindergarden" = "Pre.Kindergarden",
#                                           "Kindergarden" = "Kindergarden",
#                                           "First Grade" = "First.Grade",
#                                           "Second Grade" = "Second.Grade",
#                                           "Third Grade" = "Third.Grade",
#                                           "Fourth Grade" = "Fourth.Grade",
#                                           "Fifth Grade" = "Fifth.Grade",
#                                           "Sixth Grade" = "Sixth.Grade",
#                                           "Seventh Grade" = "Seventh.Grade",
#                                           "Eight Grade" = "Eight.Grade",
#                                           "Ninth Grade" = "Ninth.Grade",
#                                           "Tenth Grade" = "Tenth.Grade",
#                                           "Eleventh Grade" = "Eleventh.Grade",
#                                           "Twelfth Grade" = "Twelfth.Grade",
#                                           "Special Education Beyond 12th Grade" = "Special.Ed.Beyond.12th.Grade"),
#                                       selected = "Pre.Kindergarden"
#            ), 
#            "English Language Learners"= selectInput("map_level","Choose Level to map",
#                                                     choices = 
#                                                       c("Count of Students: English Language Learners" = "English.Language.Learner...enrolled.",
#                                                         "Percent of Students: English Language Learners" = "English.Language.Learner...enrolled..1",
#                                                         "Churn Enrollment: English Language Learners" = "Churn.Enrollment.for.English.Language.Learning.Students",
#                                                         "Churn Rate: English Language Learners" = "Churn.Rate.for.English.Language.Learning.Students",
#                                                         "Intake Rate: English Language Learners" = "Intake.Rate.for.English.Language.Learning.Students",
#                                                         "Stability Enrollment: English Language Learners" = "Stability.Enrollment.for.English.Language.Learning.Students",
#                                                         "Stability Rate: English Language Learners" = "Stability.Rate.for.English.Language.Learning.Students"),
#                                                     selected = "English.Language.Learner...enrolled."
#            ),
#            "Students with Disabilities"= selectInput("map_level","Choose Level to map",
#                                                      choices = 
#                                                        c("Count of Students: Students with Disabilities" = "Students.with.Disabilities...enrolled.",
#                                                          "Percent of Students: Students with Disabilities" = "Students.with.Disabilities...enrolled..1",
#                                                          "Churn Enrollment: Students with Disabilities" = "Churn.Enrollment.for.Students.with.Disabilities",
#                                                          "Churn Rate: Students with Disabilities" = "Churn.Rate.for.Students.with.Disabilities",
#                                                          "Intake Rate: Students with Disabilities" = "Intake.Rate.for.Students.with.Disabilities",
#                                                          "Stability Enrollment: Students with Disabilities" = "Stability.Enrollment.for.Students.with.Disabilities",
#                                                          "Stability Rate: Students with Disabilities" = "Stability.Rate.for.Students.with.Disabilities"),
#                                                      selected = "Students.with.Disabilities...enrolled."
#            ), 
#            "Low Income"= selectInput("map_level","Choose Level to map",
#                                      choices = 
#                                        c("Count of Students: Low Income" = "Low.Income...enrolled.",
#                                          "Percent of Students: Low Income" = "Low.Income...enrolled..1",
#                                          "Churn Enrollment: Low Income" = "Churn.Enrollment.for.Low.Income.Students",
#                                          "Churn Rate: Low Income" = "Churn.Rate.for.Low.Income.Students",
#                                          "Intake Rate: Low Income" = "Intake.Rate.for.Low.Income.Students",
#                                          "Stability Enrollment: Low Income" = "Stability.Enrollment.for.Low.Income.Students",
#                                          "Stability Rate: Low Income" = "Stability.Rate.for.Low.Income.Students"),
#                                      selected = "Low.Income...enrolled."
#            ), 
#            "High Needs"= selectInput("map_level","Choose Level to map",
#                                      choices = 
#                                        c("Count of Students: High Needs" = "High.Needs.Students...enrolled.",
#                                          "Percent of Students: High Needs" = "High.Needs.Students...enrolled..1",
#                                          "Churn Enrollment: High Needs" = "Churn.Enrollment.for.High.Needs.Students",
#                                          "Churn Rate: High Needs" = "Churn.Rate.for.High.Needs.Students",
#                                          "Intake Rate: High Needs" = "Intake.Rate.for.High.Needs.Students",
#                                          "Stability Enrollment: High Needs" = "Stability.Enrollment.for.High.Needs.Students",
#                                          "Stability Rate: High Needs" = "Stability.Rate.for.High.Needs.Students"),
#                                      selected = "High.Needs.Students...enrolled."
#            )
#            
#     )
#   })
#   
#   ##
#   #
#   # use input$map_year to choose rows
#   # use input$map_schooltype to choose rows
#   # use input$map_level to choose columns
#   ##
#   
#   
#   #title
#   output$map_title <- renderText({
#     paste(input$map_level, "in Massachusetts", input$map_schooltype, 
#           "during the", input$map_year, "school year")
#   })
#   
#   output$gvis<-renderGvis({
#     
#     ## filter dataframe
#     
#     map_df <- edu_data %>%
#       select(1,3,4,6, which(names(edu_data)==input$map_level), 22,67:74) %>%
#       filter(school.year==input$map_year)
#     switch(input$map_schooltype, 
#            "prek"= map_df <- filter(map_df, PREK==1),
#            "kindergarten"= map_df <-filter(map_df, KIND==1),
#            "elementary"= map_df <-filter(map_df, ELEM==1),
#            "middle"= map_df <-filter(map_df, MIDD==1),
#            "high" = map_df <-filter(map_df, HIGH==1))
#     
#   
#     
#    gvisGeoChart(map_df,
#                  locationvar="LatLong", sizevar="Total.Students.Enrolled",
#                  colorvar=input$map_level, hovervar="school.name",
#                  options=list(region="US-MA", displayMode="Markers", 
#                               resolution="metros",
#                               width=750, height=600,
#                               magnifyingGlass=T,
#                               colorAxis="{colors:['#FFFFFF', '#0000FF']}"
#                  ))
#     })
#   
  ###############################################################
  # POINT MAP in LEAFLET
  #######################################################
  
  output$lmapvar_levels <- renderUI({
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$lmap_radio,
           
           "Race/Ethnicity" = selectInput("lmap_level","Choose Race/Ethnicity to map",
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
           "Gender"= selectInput("lmap_level","Choose Gender to map",
                                 choices = 
                                   c("Female" = "Females",
                                     "Males" = "Males"),
                                 selected = "Females"
           ), 
           "Grade Level"= selectInput("lmap_level","Choose Grade Level to map",
                            choices = 
                              c("Pre-Kindergarden" = "Pre Kindergarden",
                                "Kindergarden" = "Kindergarden",
                                "First Grade" = "First Grade",
                                "Second Grade" = "Second Grade",
                                "Third Grade" = "Third Grade",
                                "Fourth Grade" = "Fourth Grade",
                                "Fifth Grade" = "Fifth Grade",
                                "Sixth Grade" = "Sixth Grade", 
                                "Seventh Grade" = "Seventh Grade",
                                "Eighth Grade" = "Eighth Grade",
                                "Ninth Grade" = "Ninth Grade",
                                "Tenth Grade" = "Tenth Grade",
                                "Eleventh Grade" = "Eleventh Grade",
                                "Twelfth Grade" = "Twelfth Grade",
                                "Special Education Beyond 12th Grade" = "Special Ed Beyond 12th Grade"),
                            selected = "Kindergarten"
                          ), 
           "English Language Learners"= selectInput("lmap_level","Choose Level to map",
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
           "Students with Disabilities"= selectInput("lmap_level","Choose Level to map",
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
           "Low Income"= selectInput("lmap_level","Choose Level to map",
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
           "High Needs"= selectInput("lmap_level","Choose Level to map",
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
  
  #title
  output$lmap_title <- renderText({
    paste(input$lmap_level, "in Massachusetts", 
          "schools during the", input$lmap_year, "to", c(input$lmap_year+1), "school year")
  })
  
  
    
    ## filter dataframe
  
  map_df<-reactive({  
    map_df <- edu_data %>%
      select(1,3,4,6, which(names(edu_data)==input$lmap_level), 22,67,68,74) %>%
      filter(school.year==input$lmap_year)
    
    colnames(map_df)[5]<-"var"
    
    
    validate(
      need(!Inf %in% range(map_df$var) & !-Inf %in% range(map_df$var), 
           "Please choose another variable to display"))
    
      map_df
  })
  
  output$leafmap<-renderLeaflet({
    map_df<-map_df()
    pal <- colorNumeric(
      palette = c("white", "violetred"),
      domain = c(range(map_df[,"var"])))
    
    ## Map Creation
    leaflet(width="100%", height=500) %>%
      setView(lng = -71.65, lat = 42.08, zoom = 8) %>%
      addProviderTiles("Stamen.Toner") %>%
      addCircleMarkers(data=map_df,
              lng = ~lon, lat = ~lat, radius=~log(Total.Students.Enrolled), 
              color=~pal(var),
              popup = ~paste(as.character(school.name), "\n", 
                             as.character(var)))
  })
  
  
  
})
