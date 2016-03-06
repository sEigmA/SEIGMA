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
                                 choices = as.character(all_school_table$"Dukes"),
                                 selected=as.character(all_school_table$"Dukes")[1],
                                 multiple = F), 
           "Essex" = selectInput("plot_school","Choose School",
                                 choices = as.character(all_school_table$"Essex"),
                                 selected=as.character(all_school_table$"Essex")[1],
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
                                     choices = as.character(all_school_table$"Hampshire"),
                                     selected=as.character(all_school_table$"Hampshire")[1],
                                     multiple = F), 
           "Middlesex" = selectInput("plot_school","Choose School",
                                     choices = as.character(all_school_table$"Middlesex"),
                                     selected=as.character(all_school_table$"Middlesex")[1],
                                     multiple = F),
           "Nantucket" = selectInput("plot_school","Choose School",
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
  
  
  
           
  #plot dataframe for the percent plot    
  r_percentplot_df<-reactive({
    
    edum<-edum()
    
    
    sel_col_num<-c()
    df_colnames<-c()
    
    #
    #make dataframe for the enrolled profile plot: each column is a count that will converted to percent
    #
    
    
    if (input$plot_enrolled=="Race/Ethnicity") {
      sel_col_num<-c(77:83)
      df_colnames<-c("African American", "Asian", "Hispanic", "White", "Native American", "Native Hawaiian Pacific Islander", "Multi-race non-Hispanic")
      
      plot_df <- edum %>%
        filter(school.name==input$plot_school) %>%
        select(1,3, 6, sel_col_num) %>% 
        arrange(school.year)
      colnames(plot_df)[4:ncol(plot_df)]<-c(df_colnames)
      col_sums<-apply(plot_df[,4:ncol(plot_df)], 2, FUN=function(x){sum(x, na.rm=T)})
      remove_cols<-as.numeric(which(col_sums==0))
      return(plot_df[,-c(1,2, 3+remove_cols)])
      
      
    } else if (input$plot_enrolled=="Gender") {
      sel_col_num<-c(75:76)
      df_colnames<-c( "Males", "Females")
      
      plot_df <- edum %>%
        filter(school.name==input$plot_school) %>%
        select(1,3, 6, sel_col_num) %>% 
        arrange(school.year)
      colnames(plot_df)[4:ncol(plot_df)]<-c(df_colnames)
      col_sums<-apply(plot_df[,4:ncol(plot_df)], 2, FUN=function(x){sum(x, na.rm=T)})
      remove_cols<-as.numeric(which(col_sums==0))
      return(plot_df[,-c(1,2, 3+remove_cols)])
      
      
    } else if (input$plot_enrolled=="Grade Levels") {
      sel_col_num<-c(7:21)
      df_colnames<-names(edu_data)[7:21]
      
      plot_df <- edum %>%
        filter(school.name==input$plot_school) %>%
        select(1,3, 6, sel_col_num) %>% 
        arrange(school.year)
      colnames(plot_df)[4:ncol(plot_df)]<-c(df_colnames)
      col_sums<-apply(plot_df[,4:ncol(plot_df)], 2, FUN=function(x){sum(x, na.rm=T)})
      remove_cols<-as.numeric(which(col_sums==0))
      return(plot_df[,-c(1,2, 3+remove_cols)])
    
      } else if (input$plot_enrolled=="Focus Groups") {
      sel_col_num<-c(22, 34, 32, 36, 38, 44)
      df_colnames<-c("Total Students Enrolled", "English Language Learner", "First Language not English",
                     "Disabilities", "Low Income", "High Needs")
      
      plot_df <- edum %>%
        filter(school.name==input$plot_school) %>%
        select(1,3, 6, sel_col_num) %>% 
        arrange(school.year)
      colnames(plot_df)[4:ncol(plot_df)]<-c(df_colnames)
      
      
      ###melt
      
      return(plot_df[,-c(1,2)])
    }
    })
  
  
  r_mobenrollplot_df<-reactive({
    
    
    edum<-edum()
    
    
    sel_col_num<-c()
    df_colnames<-c()
    
    #
    #make dataframe for the enrolled profile plot: each column is a count that will converted to percent
    #
    
    
    if (input$plot_mobility=="English Language Learner Students") {
      sel_col_num<-c(51, 54)
              df_colnames<-c("Churn Enrollment for English Language Learning Students",
                             "Stability Enrollment for English Language Learning Students")
              
              plot_df <- edum %>%
                filter(school.name==input$plot_school) %>%
                select(1,3, 6, sel_col_num) %>% 
                arrange(school.year)
              colnames(plot_df)[4:ncol(plot_df)]<-c(df_colnames)
      remove_cols<-NULL
      return(plot_df[complete.cases(plot_df),-c(1,2, 3+remove_cols)])
      
      
    } else if (input$plot_mobility=="Students with Disabilities") {
      sel_col_num<-c(46, 49)
            df_colnames<-c("Churn Enrollment for Students with Disabilities",
                           "Stability Enrollment for Students with Disabilities")
            
            plot_df <- edum %>%
              filter(school.name==input$plot_school) %>%
              select(1,3, 6, sel_col_num) %>% 
              arrange(school.year)
            colnames(plot_df)[4:ncol(plot_df)]<-c(df_colnames)
      remove_cols<-NULL
      return(plot_df[complete.cases(plot_df),-c(1,2, 3+remove_cols)])
      
      
    } else if (input$plot_mobility=="Low Income Students") {
      sel_col_num<-c(61, 64)
            df_colnames<-c("Churn Enrollment for Low Income Students",
                           "Stability Enrollment for Low Income Students")
            
            plot_df <- edum %>%
              filter(school.name==input$plot_school) %>%
              select(1,3, 6, sel_col_num) %>% 
              arrange(school.year)
            colnames(plot_df)[4:ncol(plot_df)]<-c(df_colnames)
      remove_cols<-NULL
      return(plot_df[complete.cases(plot_df),-c(1,2, 3+remove_cols)])
      
      #ELL
    } else if (input$plot_mobility=="High Needs Students") {
      sel_col_num<-c(56, 59)
            df_colnames<-c("Churn Enrollment for High Needs Students",
                           "Stability Enrollment for High Needs Students")
            
            plot_df <- edum %>%
              filter(school.name==input$plot_school) %>%
              select(1,3, 6, sel_col_num) %>% 
              arrange(school.year)
            colnames(plot_df)[4:ncol(plot_df)]<-c(df_colnames)
      remove_cols<-NULL
      return(plot_df[complete.cases(plot_df),-c(1,2, 3+remove_cols)])
      
    } 
  })
  
  r_mobrateplot_df<-reactive({
    
    
    edum<-edum()
    
    
    sel_col_num<-c()
    df_colnames<-c()
    
    #
    #make dataframe for the enrolled profile plot: each column is a count that will converted to percent
    #
    
    
    if (input$plot_mobility=="English Language Learner Students") {
      sel_col_num<-c(52, 53, 55)
            df_colnames<-c("Churn Rate for English Language Learning Students",
                           "Intake Rate for English Language Learning Students",
                           "Stability Rate for English Language Learning Students")
            
            plot_df <- edum %>%
              filter(school.name==input$plot_school) %>%
              select(1,3, 6, sel_col_num) %>% 
              arrange(school.year)
            colnames(plot_df)[4:ncol(plot_df)]<-c(df_colnames)
      remove_cols<-NULL
      return(plot_df[complete.cases(plot_df),-c(1,2, 3+remove_cols)])
      
      
    } else if (input$plot_mobility=="Students with Disabilities") {
      sel_col_num<-c(47, 48, 50)
            df_colnames<-c("Churn Rate for Students with Disabilities",
                           "Intake Rate for Students with Disabilities",
                           "Stability Rate for Students with Disabilities")
            
            plot_df <- edum %>%
              filter(school.name==input$plot_school) %>%
              select(1,3, 6, sel_col_num) %>% 
              arrange(school.year)
            colnames(plot_df)[4:ncol(plot_df)]<-c(df_colnames)
      remove_cols<-NULL
      return(plot_df[complete.cases(plot_df),-c(1,2, 3+remove_cols)])
      
      
    } else if (input$plot_mobility=="Low Income Students") {
      sel_col_num<-c(62, 63, 65)
            df_colnames<-c("Churn Rate for Low Income Students",
                           "Intake Rate for Low Income Students",
                           "Stability Rate for Low Income Students")
            plot_df <- edum %>%
              filter(school.name==input$plot_school) %>%
              select(1,3, 6, sel_col_num) %>% 
              arrange(school.year)
            colnames(plot_df)[4:ncol(plot_df)]<-c(df_colnames)
            
      remove_cols<-NULL
      return(plot_df[complete.cases(plot_df),-c(1,2, 3+remove_cols)])
      
      #ELL
    } else if (input$plot_mobility=="High Needs Students") {
      sel_col_num<-c(57, 58, 60)
            df_colnames<-c("Churn Rate for High Needs Students",
                           "Intake Rate for High Needs Students",
                           "Stability Rate for High Needs Students")
            
            plot_df <- edum %>%
              filter(school.name==input$plot_school) %>%
              select(1,3, 6, sel_col_num) %>% 
              arrange(school.year)
            colnames(plot_df)[4:ncol(plot_df)]<-c(df_colnames)
      remove_cols<-NULL
      return(plot_df[complete.cases(plot_df),-c(1,2, 3+remove_cols)])
      
    } 
  })
  
  
    
  #make three plot objects in the output
  
  
  output$percentcolplot<-reactive({
    #message to be displayed when no school selected
    
    validate(
      need(is.null(input$plot_school)==FALSE, "Please select a school")
    )
    
    r_plot<-r_percentplot_df()
    
    
    list(
      data=googleDataTable(r_plot),
      options = list(title=paste("Enrollment Profile: Distribution of ", 
                                 input$plot_enrolled,
                                 "at ", 
                                 input$plot_school),
                     isStacked='percent',
                     # set fonts
                     fontName = "Source Sans Pro",
                     fontSize = font_size,
                     ## set axis titles, ticks, fonts, and ranges
                     hAxis = list(
                       title = "Start of School Year",
                       format = "####",
                       ticks = seq(2003,2012,1),
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
                     )
    ,
    
    ## set chart area padding
    chartArea = list(
      top = 50, left = 75,
      height = "75%", width = "70%"
    ),
    
    ## set colors
    colors = cbbPalette[c(1:9)]
    ))
    
  })
  
  output$countcolplot<-reactive({
    #message to be displayed when no school selected
    
    validate(
      need(is.null(input$plot_school)==FALSE, "Please select a school")
    )
    
    r_plot<-r_percentplot_df()
    tse<-max(r_plot$'Total Students Enrolled')
    
    list(
      data=googleDataTable(r_plot),
      options = list(title=paste("Enrollment Profile: Distribution of Focus Groups", 
                                 "at ", 
                                 input$plot_school),
                     isStacked=FALSE,
                     # set fonts
                     fontName = "Source Sans Pro",
                     fontSize = font_size,
                     ## set axis titles, ticks, fonts, and ranges
                     hAxis = list(
                       title = "Start of School Year",
                       format = "####",
                       ticks = seq(2003,2012,1),
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
                       ticks = seq(0,max(r_plot[,2]),100),
                       
                       #viewWindow = list(min=0, max=100),
                       textStyle = list(
                         fontSize = font_size),
                       titleTextStyle = list(
                         fontSize = font_size+2,
                         bold = TRUE,
                         italic = FALSE)
                     )
                     ,
                     
                     ## set chart area padding
                     chartArea = list(
                       top = 50, left = 75,
                       height = "75%", width = "70%"
                     ),
                     
                     ## set colors
                     colors = cbbPalette[c(1:9)]
      ))
    
  })
  
  
  output$mobenrollment_plot<-reactive({
    #message to be displayed when no school selected
    
    validate(
      need(is.null(input$plot_school)==FALSE, "Please select a school")
    )
    
    r_mobenrollplot<-r_mobenrollplot_df()
    ymax<-max(r_mobenrollplot[,2])
    
    list(
      data=googleDataTable(r_mobenrollplot),
      options = list(
                   title=paste("Student Mobility: Mobility Enrollment of ", 
                               input$plot_mobility,
                               "at ", 
                               input$plot_school),
                   isStacked=FALSE,
                   # set fonts
                   fontName = "Source Sans Pro",
                   fontSize = font_size,
                   ## set axis titles, ticks, fonts, and ranges
                   hAxis = list(
                     title = "Start of School Year",
                     ticks = seq(2003,2012,1),
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
                     ticks=seq(0, ymax, 10),
                     
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
                   ## set colors
                   colors = cbbPalette[c(1:9)],
                   
                   
                   ## set tooltip font size
                   ## Hover text font stuff
                   tooltip = list(
                     textStyle = list(
                       fontSize = font_size
                     ))
                   )
    )
    
  })
  
  output$mobrate_plot<-reactive({
    #message to be displayed when no school selected
    validate(
      need(is.null(input$plot_school)==FALSE, "Please select a school")
    )
    r_mobrateplot<-r_mobrateplot_df()
    
    
    list(
      data=googleDataTable(r_mobrateplot),
      options=list(title=paste("Student Mobility: Mobility Rates of ", 
                  input$plot_mobility,
                  "at ", 
                  input$plot_school),
                  isStacked=FALSE,
                  # set fonts
                  fontName = "Source Sans Pro",
                  fontSize = font_size,
                  ## set axis titles, ticks, fonts, and ranges
                  hAxis = list(
                    title = "Start of School Year",
                    ticks = seq(2003,2012,1),
                    format = "####",
                    textStyle = list(
                      fontSize = 14),
                    titleTextStyle = list(
                      fontSize = 16,
                      bold = TRUE,
                      italic = FALSE)
                  ),
                  vAxis = list(
                    title = "Rate in Percent",
                    ticks = seq(0,100,20),
                    
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
                  
                  ## set colors
                  colors = cbbPalette[c(1:9)],
                  
                  ## set tooltip font size
                  ## Hover text font stuff
                  tooltip = list(
                    textStyle = list(
                      fontSize = font_size
                    ))
      )
    )
    
  })
  
    
  ###############################################################
  # POINT MAP in LEAFLET
  #######################################################
  
  output$lmapvar_levels <- renderUI({
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$lmap_radio,
           
           "Race/Ethnicity" = selectInput("lmap_level","Choose Race/Ethnicity to map",
                                          choices = 
                                            c("African American" = "African American",
                                              "Asian" = "Asian",
                                              "Hispanic" = "Hispanic",
                                              "White" = "White",
                                              "Native American" = "Native American",
                                              "Native Hawaiian/Pacific Islander" = "Native Hawaiian/Pacific Islander",
                                              "Multi-Race Non-Hispanic" = "Multi-Race/Non-Hispanic"),
                                          selected = "African American"
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
                            selected = "Pre Kindergarden"
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
    
    switch(input$lmap_level, 
           "African American"=sel_col<-23,
           "Asian"=sel_col<-24,
           "Hispanic"=sel_col<-25,
           "White"=sel_col<-26,
           "Native American"=sel_col<-27,
           "Native Hawaiian/Pacific Islander"=sel_col<-30,
           "Multi-Race/Non-Hispanic"=sel_col<-31,
           "Male"=sel_col<-28,
           "Female"=sel_col<-29,
           "Pre Kindergarden"=sel_col<-7,
           "Kindergarden"=sel_col<-8,
           "First Grade"=sel_col<-9,
           "Second Grade"=sel_col<-10,
           "Third Grade"=sel_col<-11,
           "Fourth Grade"=sel_col<-12,
           "Fifth Grade"=sel_col<-13,
           "Sixth Grade"=sel_col<-14, 
           "Seventh Grade"=sel_col<-15,
           "Eighth Grade"=sel_col<-16,
           "Ninth Grade"=sel_col<-17,
           "Tenth Grade"=sel_col<-18,
           "Eleventh Grade"=sel_col<-19,
           "Twelfth Grade"=sel_col<-20,
           "Special Ed Beyond 12th Grade"=sel_col<-21
           )
           
    
    edu<-edum()
    map_df <- edu %>%
      select(1,3,4,6, sel_col, 22,67,68,74) %>%
      filter(school.year==input$lmap_year)
    
    colnames(map_df)[5]<-"var"
    colnames(map_df)[6]<-"TSE"
    
    
    validate(
      need(!Inf %in% range(map_df$var) & !-Inf %in% range(map_df$var), 
           "Please choose another variable to display"))
    
      map_df
  })
  
  output$leafmap<-renderLeaflet({
    map_df<-map_df()
    pal <- colorNumeric(
      palette = c("white", "violetred"),
      domain = c(range(map_df[,"var"], na.rm=T)))
    
    ## Map Creation
    leaflet(width="100%", height=500) %>%
      setView(lng = -71.65, lat = 42.08, zoom = 8) %>%
      addProviderTiles("Stamen.Toner") %>%
      addCircleMarkers(data=map_df,
              lng = ~lon, lat = ~lat, radius=~log(TSE), 
              color=~pal(var),
              popup = ~paste(as.character(school.name), 
                             "\n", 
                             as.character(var)))
  })
  
  
 
###map legend
## map legend
output$AAlegend <- renderPlot({

    paint.brush = colorRampPalette(colors=c("white", "violetred"))
    cols <- paint.brush(25)
    leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level], range.table[2,input$lmap_level],
                                  length.out=(length(map_colors)-1)), x = 1, col = cols)
    
    p <- ggplot(data = leg_dat) +
      geom_tile(aes(y = y, fill = reorder(col,y), x = x), show.legend = FALSE) +
      scale_fill_manual(values = leg_dat$col) + theme_bw() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.x = element_blank(),
            panel.border = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank())
   return(p)
  })
output$ASlegend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level], range.table[2,input$lmap_level],
                                length.out=(length(map_colors)-1)), x = 1, col = cols)
  
  p <- ggplot(data = leg_dat) +
    geom_tile(aes(y = y, fill = reorder(col,y), x = x), show.legend = FALSE) +
    scale_fill_manual(values = leg_dat$col) + theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())
  return(p)
})
output$HISPlegend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level], range.table[2,input$lmap_level],
                                length.out=(length(map_colors)-1)), x = 1, col = cols)
  
  p <- ggplot(data = leg_dat) +
    geom_tile(aes(y = y, fill = reorder(col,y), x = x), show.legend = FALSE) +
    scale_fill_manual(values = leg_dat$col) + theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())
  return(p)
})
output$WHlegend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level], range.table[2,input$lmap_level],
                                length.out=(length(map_colors)-1)), x = 1, col = cols)
  
  p <- ggplot(data = leg_dat) +
    geom_tile(aes(y = y, fill = reorder(col,y), x = x), show.legend = FALSE) +
    scale_fill_manual(values = leg_dat$col) + theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())
  return(p)
})
output$NAlegend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level], range.table[2,input$lmap_level],
                                length.out=(length(map_colors)-1)), x = 1, col = cols)
  
  p <- ggplot(data = leg_dat) +
    geom_tile(aes(y = y, fill = reorder(col,y), x = x), show.legend = FALSE) +
    scale_fill_manual(values = leg_dat$col) + theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())
  return(p)
})
output$NHlegend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level], range.table[2,input$lmap_level],
                                length.out=(length(map_colors)-1)), x = 1, col = cols)
  
  p <- ggplot(data = leg_dat) +
    geom_tile(aes(y = y, fill = reorder(col,y), x = x), show.legend = FALSE) +
    scale_fill_manual(values = leg_dat$col) + theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())
  return(p)
})
output$MRlegend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level], range.table[2,input$lmap_level],
                                length.out=(length(map_colors)-1)), x = 1, col = cols)
  
  p <- ggplot(data = leg_dat) +
    geom_tile(aes(y = y, fill = reorder(col,y), x = x), show.legend = FALSE) +
    scale_fill_manual(values = leg_dat$col) + theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())
  return(p)
})

output$FEMlegend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level], range.table[2,input$lmap_level],
                                length.out=(length(map_colors)-1)), x = 1, col = cols)
  
  p <- ggplot(data = leg_dat) +
    geom_tile(aes(y = y, fill = reorder(col,y), x = x), show.legend = FALSE) +
    scale_fill_manual(values = leg_dat$col) + theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())
  return(p)
})
output$MALlegend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level], range.table[2,input$lmap_level],
                                length.out=(length(map_colors)-1)), x = 1, col = cols)
  
  p <- ggplot(data = leg_dat) +
    geom_tile(aes(y = y, fill = reorder(col,y), x = x), show.legend = FALSE) +
    scale_fill_manual(values = leg_dat$col) + theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())
  return(p)
})

output$PREKlegend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level], range.table[2,input$lmap_level],
                                length.out=(length(map_colors)-1)), x = 1, col = cols)
  
  p <- ggplot(data = leg_dat) +
    geom_tile(aes(y = y, fill = reorder(col,y), x = x), show.legend = FALSE) +
    scale_fill_manual(values = leg_dat$col) + theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())
  return(p)
})
output$KINDlegend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level], range.table[2,input$lmap_level],
                                length.out=(length(map_colors)-1)), x = 1, col = cols)
  
  p <- ggplot(data = leg_dat) +
    geom_tile(aes(y = y, fill = reorder(col,y), x = x), show.legend = FALSE) +
    scale_fill_manual(values = leg_dat$col) + theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())
  return(p)
})
output$FIRSTlegend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level], range.table[2,input$lmap_level],
                                length.out=(length(map_colors)-1)), x = 1, col = cols)
  
  p <- ggplot(data = leg_dat) +
    geom_tile(aes(y = y, fill = reorder(col,y), x = x), show.legend = FALSE) +
    scale_fill_manual(values = leg_dat$col) + theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())
  return(p)
})
output$SECONDlegend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level], range.table[2,input$lmap_level],
                                length.out=(length(map_colors)-1)), x = 1, col = cols)
  
  p <- ggplot(data = leg_dat) +
    geom_tile(aes(y = y, fill = reorder(col,y), x = x), show.legend = FALSE) +
    scale_fill_manual(values = leg_dat$col) + theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())
  return(p)
})
output$THIRDlegend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level], range.table[2,input$lmap_level],
                                length.out=(length(map_colors)-1)), x = 1, col = cols)
  
  p <- ggplot(data = leg_dat) +
    geom_tile(aes(y = y, fill = reorder(col,y), x = x), show.legend = FALSE) +
    scale_fill_manual(values = leg_dat$col) + theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())
  return(p)
})
output$FOURTHlegend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level], range.table[2,input$lmap_level],
                                length.out=(length(map_colors)-1)), x = 1, col = cols)
  
  p <- ggplot(data = leg_dat) +
    geom_tile(aes(y = y, fill = reorder(col,y), x = x), show.legend = FALSE) +
    scale_fill_manual(values = leg_dat$col) + theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())
  return(p)
})
output$FIFTHlegend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level], range.table[2,input$lmap_level],
                                length.out=(length(map_colors)-1)), x = 1, col = cols)
  
  p <- ggplot(data = leg_dat) +
    geom_tile(aes(y = y, fill = reorder(col,y), x = x), show.legend = FALSE) +
    scale_fill_manual(values = leg_dat$col) + theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())
  return(p)
})
output$SIXTHlegend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level], range.table[2,input$lmap_level],
                                length.out=(length(map_colors)-1)), x = 1, col = cols)
  
  p <- ggplot(data = leg_dat) +
    geom_tile(aes(y = y, fill = reorder(col,y), x = x), show.legend = FALSE) +
    scale_fill_manual(values = leg_dat$col) + theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())
  return(p)
})
output$SEVENTHlegend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level], range.table[2,input$lmap_level],
                                length.out=(length(map_colors)-1)), x = 1, col = cols)
  
  p <- ggplot(data = leg_dat) +
    geom_tile(aes(y = y, fill = reorder(col,y), x = x), show.legend = FALSE) +
    scale_fill_manual(values = leg_dat$col) + theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())
  return(p)
})
output$EIGHTHlegend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level], range.table[2,input$lmap_level],
                                length.out=(length(map_colors)-1)), x = 1, col = cols)
  
  p <- ggplot(data = leg_dat) +
    geom_tile(aes(y = y, fill = reorder(col,y), x = x), show.legend = FALSE) +
    scale_fill_manual(values = leg_dat$col) + theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())
  return(p)
})
output$NINTHlegend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level], range.table[2,input$lmap_level],
                                length.out=(length(map_colors)-1)), x = 1, col = cols)
  
  p <- ggplot(data = leg_dat) +
    geom_tile(aes(y = y, fill = reorder(col,y), x = x), show.legend = FALSE) +
    scale_fill_manual(values = leg_dat$col) + theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())
  return(p)
})
output$TENTHlegend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level], range.table[2,input$lmap_level],
                                length.out=(length(map_colors)-1)), x = 1, col = cols)
  
  p <- ggplot(data = leg_dat) +
    geom_tile(aes(y = y, fill = reorder(col,y), x = x), show.legend = FALSE) +
    scale_fill_manual(values = leg_dat$col) + theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())
  return(p)
})
output$ELEVENTHlegend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level], range.table[2,input$lmap_level],
                                length.out=(length(map_colors)-1)), x = 1, col = cols)
  
  p <- ggplot(data = leg_dat) +
    geom_tile(aes(y = y, fill = reorder(col,y), x = x), show.legend = FALSE) +
    scale_fill_manual(values = leg_dat$col) + theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())
  return(p)
})
output$TWELFTHlegend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level], range.table[2,input$lmap_level],
                                length.out=(length(map_colors)-1)), x = 1, col = cols)
  
  p <- ggplot(data = leg_dat) +
    geom_tile(aes(y = y, fill = reorder(col,y), x = x), show.legend = FALSE) +
    scale_fill_manual(values = leg_dat$col) + theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())
  return(p)
})
output$SPEClegend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level], range.table[2,input$lmap_level],
                                length.out=(length(map_colors)-1)), x = 1, col = cols)
  
  p <- ggplot(data = leg_dat) +
    geom_tile(aes(y = y, fill = reorder(col,y), x = x), show.legend = FALSE) +
    scale_fill_manual(values = leg_dat$col) + theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())
  return(p)
})




#finish document
})