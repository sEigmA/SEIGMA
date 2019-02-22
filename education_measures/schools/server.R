###########################################
## Title: Schools                        ##
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
  output$summary <- DT::renderDataTable({
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
      df_colnames<-c("% African American", "% Asian", "% Hispanic", "% White", "% Native American", "% Native Hawaiian Pacific Islander", "% Multi-race non-Hispanic")
    } else if (input$sum_radio=="Gender") {
      sel_col_num<-c(28:29)
      df_colnames<-c( "% Males", "% Females")
    } else if (input$sum_radio=="Grade Level") {
      sel_col_num<-c(7:21)
      df_colnames<-paste("# Enrolled in",names(edu_data)[7:21], sep=" ")
    }  else if (input$sum_radio=="English Language Learners") {
        sel_col_num<-c(32, 34, 53:57)
        df_colnames<-c("First Language Not English Enrolled", "English Language Learner Enrolled", "Churn Enrollment for English Language Learning Students", "Churn Rate for English Language Learning Students %", "Intake Rate for English Language Learning Students %", "Stability Enrollment for English Language Learning Students", "Stability Rate for English Language Learning Students %")
    } else if (input$sum_radio=="Students with Disabilities") {
      sel_col_num<-c(36, 48:52)
      df_colnames<-c(  "Students with Disabilities Enrolled", "Churn Enrollment for Students With Disabilites", "Churn Rate for Students With Disabilites %", "Intake Rate for Students With Disabilites %", "Stability Enrollment for Students with Disabilities", "Stability Rate for Students with Disabilities %")
    } else if (input$sum_radio=="Low Income") {
      sel_col_num<-c(38, 63:67)
      df_colnames<-c(  "Low Income Enrolled","Churn Enrollment for Low Income Students", "Churn Rate for Low Income Students %", "Intake Rate for Low Income Students %", "Stability Enrollment for Low Income Students", "Stability Rate for Low Income Students %")
    } else if (input$sum_radio=="Economically Disadvantaged") {
      sel_col_num<-c(46, 68:72)
      df_colnames<-c(  "Economically Disadvantaged Enrolled","Churn Enrollment for Economically Disadvantaged Students", "Churn Rate for Economically Disadvantaged Students %", "Intake Rate for Low Economically Disadvantaged Students %", "Stability Enrollment for Economically Disadvantaged Students", "Stability Rate for Economically Disadvantaged Students %")
    } else {
      sel_col_num<-c(44, 58:62)
      df_colnames<-c("High Needs Enrolled", "Churn Enrollmment for High Needs Students", "Churn Rate for High Needs Students %", "Intake Rate for High Needs Students %", "Stability Enrollment for High Needs Students", "Stability Rate for High Needs Students %")}
    
    sum_df <- sum_df %>%
    arrange(County, Municipal, school.name)%>%
      select(County, Municipal, school.name, school.year, Total.Students.Enrolled, 
             sel_col_num)
    
    final_colnames<-c("County", "Municipal","School Name","School Year", "Total Number Enrolled",
                      df_colnames)
  
    colnames(sum_df) <- final_colnames

    ## create a dataframe consisting only of counties in vector
return(sum_df)
  }, options = list(
    searching = FALSE, 
    orderClasses = TRUE,
    autoWidth=TRUE,
    scrollX=TRUE,
    columnDefs = list(list(width = '300px', targets = 2))
    ), rownames=F)

  
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

#   #this makes the school selection dependent on the county selection

  chosenschools<-reactive({
    schoolz=all_school_table[input$plot_county]
    schoolz
  })
  observeEvent(input$plot_county,{
    # This will change the value of input$school 
      updateSelectizeInput(session, "plot_school", 
                      choices=chosenschools()
      )
    })
#     
  # I can't get googlecharts to use validate in order to deal with missing data
  # so I'll limit the selections of options to avoid passing an empty dataframe to googlecharts
   
#     observeEvent(input$plot_school,{
#       vars=school_availabilitytable[input$plot_school]
#       v3$chosenschool_vars<-vars
#     
#         # This will change the choices for input$plot_mobility2w 
#       updateSelectInput(session, "plot_mobility2", 
#                            choices=v3$chosenschool_vars)
#     })
    
    ########
    # create reactive dataframes
  r_percentplot_df<-reactive({
    edum<-edum() %>% filter(school.name==input$plot_school)
    
    sel_col_num<-c()
    df_colnames<-c()
    
    #
    #make dataframe for the enrolled profile plot: each column is a count that will converted to percent
    #
    
    
    if (input$plot_enrolled=="Race/Ethnicity") {
      sel_col_num<-c(81:87)
      df_colnames<-c("African American", "Asian", "Hispanic", "White", "Native American", "Native Hawaiian Pacific Islander", "Multi-race non-Hispanic")
      
      plot_df <- edum %>%
        
        select(1,3, 6, sel_col_num) %>% 
        arrange(school.year)
      colnames(plot_df)[4:ncol(plot_df)]<-c(df_colnames)
      col_sums<-apply(plot_df[,4:ncol(plot_df)], 2, FUN=function(x){sum(x, na.rm=T)})
      remove_cols<-as.numeric(which(col_sums==0))
      return(plot_df[,-c(1,2, 3+remove_cols)])
      
      
    } else if (input$plot_enrolled=="Gender") {
      sel_col_num<-c(80, 79)
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
    
      } else if (input$plot_enrolled=="Interest Groups") {
      sel_col_num<-c(34, 32, 36, 38, 46, 44)
      df_colnames<-c("English Language Learner", "First Language not English",
                     "Disabilities", "Low Income", "Economically Disadvantaged", "High Needs")
      
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
    
    edum<-edum() %>% filter(school.name==input$plot_school)
    
    sel_col_num<-c()
    df_colnames<-c()
    
    #
    #make dataframe for the enrolled profile plot: each column is a count that will converted to percent
    #
    
    
    if (input$plot_mobility=="English Language Learner Students") {
      sel_col_num<-c(53, 56)
              df_colnames<-c("Churn Enrollment for English Language Learning Students",
                             "Stability Enrollment for English Language Learning Students")
              plot_df <- edum %>%
                select(1,3, 6, sel_col_num) %>% 
                arrange(school.year)
              colnames(plot_df)[4:ncol(plot_df)]<-c(df_colnames)
              plot_df<-(plot_df[ ,-c(1,2)])
              
              return(plot_df)
      
    } else if (input$plot_mobility=="Students with Disabilities") {
      sel_col_num<-c(48, 51)
            df_colnames<-c("Churn Enrollment for Students with Disabilities",
                           "Stability Enrollment for Students with Disabilities")
            plot_df <- edum %>%
              select(1,3, 6, sel_col_num) %>% 
              arrange(school.year)
            colnames(plot_df)[4:ncol(plot_df)]<-c(df_colnames)
            plot_df<-(plot_df[ ,-c(1,2)])
            
            return(plot_df)
      
      
    } else if (input$plot_mobility=="Low Income Students") {
      sel_col_num<-c(63, 66)
            df_colnames<-c("Churn Enrollment for Low Income Students",
                           "Stability Enrollment for Low Income Students")
            plot_df <- edum %>%
              select(1,3, 6, sel_col_num) %>% 
              arrange(school.year)
            colnames(plot_df)[4:ncol(plot_df)]<-c(df_colnames)
            plot_df<-(plot_df[ ,-c(1,2)])
            
            return(plot_df)
            
    } else if (input$plot_mobility=="Economically Disadvantaged Students") {
      sel_col_num<-c(68, 71)
      df_colnames<-c("Churn Enrollment for Economically Disadvantaged Students",
                     "Stability Enrollment for Economically Disadvantaged Students")
      plot_df <- edum %>%
        select(1,3, 6, sel_col_num) %>% 
        arrange(school.year)
      colnames(plot_df)[4:ncol(plot_df)]<-c(df_colnames)
      plot_df<-(plot_df[ ,-c(1,2)])
      
      return(plot_df)
      
      #ELL
    } else if (input$plot_mobility=="High Needs Students") {
      sel_col_num<-c(58, 61)
            df_colnames<-c("Churn Enrollment for High Needs Students",
                           "Stability Enrollment for High Needs Students")
    
        
    
    
    plot_df <- edum %>%
              select(1,3, 6, sel_col_num) %>% 
              arrange(school.year)
            colnames(plot_df)[4:ncol(plot_df)]<-c(df_colnames)
            plot_df<-(plot_df[ ,-c(1,2)])
            
            
            return(plot_df)}
    
    
     
  })
  
  r_mobrateplot_df<-reactive({
  
  edum<-edum() %>% filter(school.name==input$plot_school)    
    
    sel_col_num<-c()
    df_colnames<-c()
    
    #
    #make dataframe for the enrolled profile plot: each column is a count that will converted to percent
    #
    
    
    if (input$plot_mobility=="English Language Learner Students") {
      sel_col_num<-c(54, 55, 57)
            df_colnames<-c("Churn Rate for English Language Learning Students",
                           "Intake Rate for English Language Learning Students",
                           "Stability Rate for English Language Learning Students")
            plot_df <- edum %>%
              
              select(1,3, 6, sel_col_num) %>% 
              arrange(school.year)
            colnames(plot_df)[4:ncol(plot_df)]<-c(df_colnames)
            plot_df<-(plot_df[ ,-c(1,2)])
            
            return(plot_df)
            
      
    } else if (input$plot_mobility=="Students with Disabilities") {
      sel_col_num<-c(49, 50, 51)
            df_colnames<-c("Churn Rate for Students with Disabilities",
                           "Intake Rate for Students with Disabilities",
                           "Stability Rate for Students with Disabilities")
            plot_df <- edum %>%
              
              select(1,3, 6, sel_col_num) %>% 
              arrange(school.year)
            colnames(plot_df)[4:ncol(plot_df)]<-c(df_colnames)
            plot_df<-(plot_df[ ,-c(1,2)])
            
            return(plot_df)
            
            
      
    } else if (input$plot_mobility=="Low Income Students") {
      sel_col_num<-c(64, 65, 67)
            df_colnames<-c("Churn Rate for Low Income Students",
                           "Intake Rate for Low Income Students",
                           "Stability Rate for Low Income Students")
            plot_df <- edum %>%
              
              select(1,3, 6, sel_col_num) %>% 
              arrange(school.year)
            colnames(plot_df)[4:ncol(plot_df)]<-c(df_colnames)
            plot_df<-(plot_df[ ,-c(1,2)])
            return(plot_df)
            
    } else if (input$plot_mobility=="Economically Disadvantaged Students") {
      sel_col_num<-c(69, 70, 72)
      df_colnames<-c("Churn Rate for Economically Disadvantaged Students",
                     "Intake Rate for Economically Disadvantaged Students",
                     "Stability Rate for Economically Disadvantaged Students")
      plot_df <- edum %>%
        
        select(1,3, 6, sel_col_num) %>% 
        arrange(school.year)
      colnames(plot_df)[4:ncol(plot_df)]<-c(df_colnames)
      plot_df<-(plot_df[ ,-c(1,2)])
      return(plot_df)
            
      #ELL
    } else if (input$plot_mobility=="High Needs Students") {
      sel_col_num<-c(59, 60, 62)
            df_colnames<-c("Churn Rate for High Needs Students",
                           "Intake Rate for High Needs Students",
                           "Stability Rate for High Needs Students")
        
            
            plot_df <- edum %>%
            
              select(1,3, 6, sel_col_num) %>% 
              arrange(school.year)
            colnames(plot_df)[4:ncol(plot_df)]<-c(df_colnames)
            plot_df<-(plot_df[ ,-c(1,2)])
            
           return(plot_df)
    }
     
    
    })
  #make three plot objects in the output
  

  
  output$percentcolplot<-reactive({
    if(is.null(input$plot_school)==F){
      
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
                       ticks = seq(2003,2018,1),
                       #  viewWindow = list(min=, max=),
                       textStyle = list(
                         fontSize = 14),
                       titleTextStyle = list(
                         fontSize = 16,
                         bold = TRUE,
                         italic = FALSE)
                     ),
                     vAxis = list(
                       title = "Proportion of students in %",
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
    }
  })
  
  output$countcolplot<-reactive({
    if(is.null(input$plot_school)==F){
      
    r_plot<-r_percentplot_df()
    
    tse<-max(apply(r_plot[,-1], 2, function(x){max(x, na.rm=T)}))
    
    list(
      data=googleDataTable(r_plot),
      options = list(title=paste("Enrollment Profile: Distribution of Interest Groups", 
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
                       ticks = seq(2003,2018,1),
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
                       ticks = seq(0,max(r_plot[,2], na.rm = T),100),
                       
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
    }
  })
  
  
  #observeEvent(input$plot_mobility,{
   #   observeEvent(input$plot_school,{
  output$mobenrollment_plot<-reactive({
    
    if(is.null(input$plot_school)==F){
      
    
    
    
#     
#     #message to be displayed when variable full of NAs selected
#     validate(
#       need(is.null(r_mobrateplot_df())==FALSE, "No data available, please select a different variable  and regenerate plot"),
#       need(nrow(r_mobrateplot_df())>0, "No data available, please select a different variable  and regenerate plot")
#     )
#     
    
    
    #if(is.null(r_mobenrollplot )==F & nrow(r_mobenrollplot)>0){
      maxout<-function(x){
        m<-max(x)
        if(is.na(m)==T | is.null(m)==T){m<-100}
        m
      }
    ymax<-maxout(unlist(r_mobenrollplot_df()[,-1]))

    list(
      data=googleDataTable(r_mobenrollplot_df()),
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
                     ticks = seq(2003,2018,1),
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
                     ticks=seq(0, ymax, 25),
                     
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
    }  
  })
    #  }) })
 
  
  
  
  output$mobrate_plot<-reactive({
    

    
    #message to be displayed when variable full of NAs selected
    #validate(
    #  need(is.null(r_mobenrollplot_df())==FALSE, "No data available, please select a different variable and regenerate plot"),
    #  need(nrow(r_mobenrollplot_df())>0, "No data available, please select a different variable  and regenerate plot")
    #  
    #)
#     
    
    
    #if(is.null(r_mobrateplot)==F & nrow(r_mobrateplot)>0){
    
    list(
      data=googleDataTable(r_mobrateplot_df()),
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
                    ticks = seq(2003,2018,1),
                    format = "####",
                    textStyle = list(
                      fontSize = 14),
                    titleTextStyle = list(
                      fontSize = 16,
                      bold = TRUE,
                      italic = FALSE)
                  ),
                  vAxis = list(
                    title = "Rate in Percent %",
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
    
    #}
    
  })
      
  ##################################
  
  
  ###############################################################
  # POINT MAP in LEAFLET
  #######################################################
  
  
  #titles: one per map
  output$lmap_title1 <- renderUI({
    HTML(paste("<b>%", 
               input$lmap_level1, "Students in Massachusetts", 
          "schools during the", input$lmap_year, "to", c(input$lmap_year+1), "school year", 
          "</b>")
    )
  })
  output$lmap_title2 <- renderUI({
    HTML(paste("<b>%", 
               input$lmap_level2, "Students in Massachusetts", 
          "schools during the", input$lmap_year, "to", c(input$lmap_year+1), "school year",
          "</b>")
    )
  })
  output$lmap_title3 <- renderUI({
    HTML(paste("<b>#", 
               input$lmap_level3, "Students in Massachusetts", 
          "schools during the", input$lmap_year, "to", c(input$lmap_year+1), "school year",
          "</b>")
    )
  })
  output$lmap_title4 <- renderUI({
    HTML(paste("<b>", 
               input$lmap_level4, "in Massachusetts", 
          "schools during the", input$lmap_year, "to", c(input$lmap_year+1), "school year", 
          "</b>")
    )
  })
  
  output$lmap_title5 <- renderUI({
    HTML(paste("<b>", 
               input$lmap_level5, "in Massachusetts", 
          "schools during the", input$lmap_year, "to", c(input$lmap_year+1), "school year", 
          "</b>")
         )
  })
  output$lmap_title6 <- renderUI({
    HTML(paste("<b>", 
               input$lmap_level6, "in Massachusetts", 
          "schools during the", input$lmap_year, "to", c(input$lmap_year+1), "school year", 
          "</b>")
         )
  })
  output$lmap_title7 <- renderUI({
    HTML(paste("<b>", 
               input$lmap_level7, "in Massachusetts", 
          "schools during the", input$lmap_year, "to", c(input$lmap_year+1), "school year", 
          "</b>"))
  })
  output$lmap_title8 <- renderUI({
    HTML(paste("<b>", 
               input$lmap_level8, "in Massachusetts", 
          "schools during the", input$lmap_year, "to", c(input$lmap_year+1), "school year", 
          "</b>")
         )
  })
  
  output$lmap_title9 <- renderUI({
    HTML(paste("<b>", 
               input$lmap_level9, "in Massachusetts", 
          "schools during the", input$lmap_year, "to", c(input$lmap_year+1), "school year", 
          "</b>")
         )
  })
  output$lmap_title10 <- renderUI({
    HTML(paste("<b>", 
               input$lmap_level10, "in Massachusetts", 
          "schools during the", input$lmap_year, "to", c(input$lmap_year+1), "school year", 
          "</b>")
         )
  })
  output$lmap_title11 <- renderUI({
    HTML(paste("<b>", 
               input$lmap_level11, "in Massachusetts", 
          "schools during the", input$lmap_year, "to", c(input$lmap_year+1), "school year", 
          "</b>")
         )
  })
  output$lmap_title12 <- renderUI({
    HTML(paste("<b>", 
               input$lmap_level12, "in Massachusetts", 
          "schools during the", input$lmap_year, "to", c(input$lmap_year+1), "school year", 
          "</b>")
         )
  })
  output$lmap_title13 <- renderUI({
    HTML(paste("<b>", 
               input$lmap_level13, "in Massachusetts", 
               "schools during the", input$lmap_year, "to", c(input$lmap_year+1), "school year", 
               "</b>")
    )
  })
  output$lmap_title14 <- renderUI({
    HTML(paste("<b>", 
               input$lmap_level14, "in Massachusetts", 
               "schools during the", input$lmap_year, "to", c(input$lmap_year+1), "school year", 
               "</b>")
    )
  })
    
    ## filter dataframes - one per map
  
  map_df1<-reactive({
         if(input$lmap_level1==" "){sel_col=23}
    else if(input$lmap_level1=="African American"){sel_col<-23}
    else if(input$lmap_level1=="Asian"){sel_col<-24}
    else if(input$lmap_level1=="Hispanic"){sel_col<-25}
    else if(input$lmap_level1=="White"){sel_col<-26}
    else if(input$lmap_level1=="Native American"){sel_col<-27}
    else if(input$lmap_level1=="Native Hawaiian/Pacific Islander"){sel_col<-30}
    else if(input$lmap_level1=="Multi-Race/Non-Hispanic"){sel_col<-31}
    
    edu<-edum()
    map_df <- edu %>%
      select(1,3,4,6, sel_col, 22,74,75) %>%
      filter(school.year==input$lmap_year)
    
    colnames(map_df)[5]<-"var"
    colnames(map_df)[6]<-"TSE"
    
    nullrow <- map_df[c(1:2),]
    nullrow$var <- c(0,100)
    nullrow$TSE <- 0
    
    rbind(
      map_df[complete.cases(map_df),],
      nullrow)  
    })
  
  map_df2<-reactive({
        if(input$lmap_level2==" "){sel_col=23}
    
    else if(input$lmap_level2=="Males"){sel_col<-28}
    else if(input$lmap_level2=="Females"){sel_col<-29}
    edu<-edum()
    map_df <- edu %>%
      select(1,3,4,6, sel_col, 22,74,75) %>%
      filter(school.year==input$lmap_year)
    
    colnames(map_df)[5]<-"var"
    colnames(map_df)[6]<-"TSE"
    
    
    nullrow <- map_df[c(1:2),]
    nullrow$var <- c(0,100)
    nullrow$TSE <- 0
    
    rbind(
    map_df[complete.cases(map_df),],
    nullrow)
    
  })
  map_df3<-reactive({
          if(input$lmap_level3==" "){sel_col=23}
    
    else if(input$lmap_level3=="Pre Kindergarten"){sel_col<-7}
    else if(input$lmap_level3=="Kindergarten"){sel_col<-8}
    else if(input$lmap_level3=="First Grade"){sel_col<-9}
    else if(input$lmap_level3=="Second Grade"){sel_col<-10}
    else if(input$lmap_level3=="Third Grade"){sel_col<-11}
    else if(input$lmap_level3=="Fourth Grade"){sel_col<-12}
    else if(input$lmap_level3=="Fifth Grade"){sel_col<-13}
    else if(input$lmap_level3=="Sixth Grade"){sel_col<-14}
    else if(input$lmap_level3=="Seventh Grade"){sel_col<-15}
    else if(input$lmap_level3=="Eighth Grade"){sel_col<-16}
    else if(input$lmap_level3=="Ninth Grade"){sel_col<-17}
    else if(input$lmap_level3=="Tenth Grade"){sel_col<-18}
    else if(input$lmap_level3=="Eleventh Grade"){sel_col<-19}
    else if(input$lmap_level3=="Twelfth Grade"){sel_col<-20}
    else if(input$lmap_level3=="Special Ed Beyond 12th Grade"){sel_col<-21}
    edu<-edum()
    map_df <- edu %>%
      select(1,3,4,6, sel_col, 22,74,75) %>%
      filter(school.year==input$lmap_year)
    
    colnames(map_df)[5]<-"var"
    colnames(map_df)[6]<-"TSE"
    map_df[complete.cases(map_df),]
  })
  map_df4<-reactive({
         if(input$lmap_level4==" "){sel_col=23}

    else if(input$lmap_level4=="English Language Learner Enrolled %"){sel_col<-35}
    else if(input$lmap_level4=="First Language Not English Enrolled %"){sel_col<-33}
    else if(input$lmap_level4=="Students With Disabilities Enrolled %"){sel_col<-37}
    else if(input$lmap_level4=="Low Income Students Enrolled %"){sel_col<-39}
    else if(input$lmap_level4=="Economically Disadvantaged Students Enrolled %"){sel_col<-47}
    else if(input$lmap_level4=="High Needs Students Enrolled %"){sel_col<-45}
    edu<-edum()
    map_df <- edu %>%
      select(1,3,4,6, sel_col, 22,74, 75) %>%
      filter(school.year==input$lmap_year)
    
    colnames(map_df)[5]<-"var"
    colnames(map_df)[6]<-"TSE"
    
    nullrow <- map_df[c(1:2),]
    nullrow$var <- c(0,100)
    nullrow$TSE <- 0
    
    rbind(
      map_df[complete.cases(map_df),],
      nullrow)
    
    
  })
 
  map_df9<-reactive({
        if(input$lmap_level9==" "){sel_col=23}
    
    else if(input$lmap_level9=="Churn Enrollment for English Language Learning Students"){sel_col<-53}
    else if(input$lmap_level9=="Stability Enrollment for English Language Learning Students"){sel_col<-56}
    edu<-edum()
    map_df <- edu %>%
      select(1,3,4,6, sel_col, 22,74, 75) %>%
      filter(school.year==input$lmap_year)
    
    colnames(map_df)[5]<-"var"
    colnames(map_df)[6]<-"TSE"
    map_df[complete.cases(map_df),]
  })
  map_df10<-reactive({   
          if(input$lmap_level10==" "){sel_col=23}
    
    else if(input$lmap_level10=="Churn Enrollment for Students with Disabilities"){sel_col<-48}
    else if(input$lmap_level10=="Stability Enrollment for Students with Disabilities"){sel_col<-51}
  edu<-edum()
  map_df <- edu %>%
    select(1,3,4,6, sel_col, 22,74, 75) %>%
    filter(school.year==input$lmap_year)
  
  colnames(map_df)[5]<-"var"
  colnames(map_df)[6]<-"TSE"
  map_df[complete.cases(map_df),]
  })
  map_df11<-reactive({  
    if(input$lmap_level11==" "){sel_col=23}
    
    else if(input$lmap_level11=="Churn Enrollment for Low Income Students"){sel_col<-63}
    else if(input$lmap_level11=="Stability Enrollment for Low Income Students"){sel_col<-66}
    edu<-edum()
    map_df <- edu %>%
      select(1,3,4,6, sel_col, 22,74, 75) %>%
      filter(school.year==input$lmap_year)
    
    colnames(map_df)[5]<-"var"
    colnames(map_df)[6]<-"TSE"
    rbind(
      map_df[complete.cases(map_df),],
      data.frame("school.name"=NA,
                 "Municipal"=NA,
                 "County"=NA,
                 "school.year"=input$lmap_year,
                 "var"=c(0),
                 "TSE"=c(0),
                 "Lat"=c(42),
                 "Lng"=c(-71.75))
    )  })
  map_df14<-reactive({  
    if(input$lmap_level14==" "){sel_col=23}
    
    else if(input$lmap_level14=="Churn Enrollment for Economically Disadvantaged Students"){sel_col<-68}
    else if(input$lmap_level14=="Stability Enrollment for Economically Disadvantaged Students"){sel_col<-71}
    edu<-edum()
    map_df <- edu %>%
      select(1,3,4,6, sel_col, 22,74, 75) %>%
      filter(school.year==input$lmap_year)
    
    colnames(map_df)[5]<-"var"
    colnames(map_df)[6]<-"TSE"
    rbind(
      map_df[complete.cases(map_df),],
      data.frame("school.name"=NA,
                 "Municipal"=NA,
                 "County"=NA,
                 "school.year"=input$lmap_year,
                 "var"=c(0),
                 "TSE"=c(0),
                 "Lat"=c(42),
                 "Lng"=c(-71.75))
    )  })
  
  map_df12<-reactive({  
          if(input$lmap_level12==" "){sel_col=23}
    
    else if(input$lmap_level12=="Churn Enrollment for High Needs Students"){sel_col<-58}
    else if(input$lmap_level12=="Stability Enrollment for High Needs Students"){sel_col<-61}
  edu<-edum()
  map_df <- edu %>%
    select(1,3,4,6, sel_col, 22,74,75) %>%
    filter(school.year==input$lmap_year)
  
  colnames(map_df)[5]<-"var"
  colnames(map_df)[6]<-"TSE"
  map_df[complete.cases(map_df),]
  })

  map_df5<-reactive({ 
    if(input$lmap_level5==" "){sel_col=23}
    
    else if(input$lmap_level5=="Churn Rate for English Language Learning Students"){sel_col<-54}
    else if(input$lmap_level5=="Stability Rate for English Language Learning Students"){sel_col<-57}
    else if(input$lmap_level5=="Intake Rate for English Language Learning Students"){sel_col<-55}
  edu<-edum()
  map_df <- edu %>%
    select(1,3,4,6, sel_col, 22,74,75) %>%
    filter(school.year==input$lmap_year)
  
  colnames(map_df)[5]<-"var"
  colnames(map_df)[6]<-"TSE"
  
  nullrow <- map_df[c(1:2),]
  nullrow$var <- c(0,100)
  nullrow$TSE <- 0
  
  rbind(
    map_df[complete.cases(map_df),],
    nullrow)
  })
  map_df6<-reactive({ 
    if(input$lmap_level6==" "){sel_col=23}
    
    else if(input$lmap_level6=="Churn Rate for Students with Disabilities"){sel_col<-49}
    else if(input$lmap_level6=="Stability Rate for Students with Disabilities"){sel_col<-52}
    else if(input$lmap_level6=="Intake Rate for Students with Disabilities"){sel_col<-50}
  edu<-edum()
  map_df <- edu %>%
    select(1,3,4,6, sel_col, 22,74,75) %>%
    filter(school.year==input$lmap_year)
  
  colnames(map_df)[5]<-"var"
  colnames(map_df)[6]<-"TSE"
  
  nullrow <- map_df[c(1:2),]
  nullrow$var <- c(0,100)
  nullrow$TSE <- 0
  
  rbind(
    map_df[complete.cases(map_df),],
    nullrow)  })
  map_df7<-reactive({   
    if(input$lmap_level7==" "){sel_col=23}
    else if(input$lmap_level7=="Churn Rate for Low Income Students"){sel_col<-64}
    else if(input$lmap_level7=="Stability Rate for Low Income Students"){sel_col<-67}
    else if(input$lmap_level7=="Intake Rate for Low Income Students"){sel_col<-65}
  edu<-edum()
  map_df <- edu %>%
    select(1,3,4,6, sel_col, 22,74,75) %>%
    filter(school.year==input$lmap_year)
  
  colnames(map_df)[5]<-"var"
  colnames(map_df)[6]<-"TSE"
  
  nullrow <- map_df[c(1:2),]
  nullrow$var <- c(0,100)
  nullrow$TSE <- 0
  rbind(
    map_df[complete.cases(map_df),],
    nullrow)  
  })
  map_df13<-reactive({   
    if(input$lmap_level13==" "){sel_col=23}
    else if(input$lmap_level13=="Churn Rate for Economically Disadvantaged Students"){sel_col<-69}
    else if(input$lmap_level13=="Stability Rate for Economically Disadvantaged Students"){sel_col<-72}
    else if(input$lmap_level13=="Intake Rate for Economically Disadvantaged Students"){sel_col<-70}
    edu<-edum()
    map_df <- edu %>%
      select(1,3,4,6, sel_col, 22,74,75) %>%
      filter(school.year==input$lmap_year)
    
    colnames(map_df)[5]<-"var"
    colnames(map_df)[6]<-"TSE"
    
    nullrow <- map_df[c(1:2),]
    nullrow$var <- c(0,100)
    nullrow$TSE <- 0
    
    rbind(
      map_df[complete.cases(map_df),],
      nullrow)  })
  map_df8<-reactive({  
    if(input$lmap_level8==" "){sel_col=23}
    else if(input$lmap_level8=="Churn Rate for High Needs Students"){sel_col<-59}
    else if(input$lmap_level8=="Stability Rate for High Needs Students"){sel_col<-62}
    else if(input$lmap_level8=="Intake Rate for High Needs Students"){sel_col<-60}
  edu<-edum()
  map_df <- edu %>%
    select(1,3,4,6, sel_col, 22,74,75) %>%
    filter(school.year==input$lmap_year)
  
  colnames(map_df)[5]<-"var"
  colnames(map_df)[6]<-"TSE"
  
  nullrow <- map_df[c(1:2),]
  nullrow$var <- c(0,100)
  nullrow$TSE <- 0
  
  rbind(
    map_df[complete.cases(map_df),],
    nullrow)
  })
   
  #create maps in leaflet
    
  #opacity (circle rim opacity)
  op=0.5
  #fill opacity (color of circles)
  fop=0.7
  
  
  output$leafmap1<-renderLeaflet({
    
      leaflet()%>% 
      setView(lng= -71.75, lat= 42, zoom=8) %>% 
      addProviderTiles("Stamen.TonerLite") %>%
      addMarkers(lng=~Lon, lat=~Lat, icon=star,data=MAcasinos, group="MAcasinos",
                 popup = ~paste(as.character(Name))) %>%
      addMarkers(lng=~Lon, lat=~Lat, data=casinosCLOSED, group="casinosCLOSED",
                 icon=gc1,
                       popup = ~paste(as.character(Name))) %>%
      addMarkers(lng=~Lon, lat=~Lat, data=casinosOPEN, group="casinosOPEN",
                       icon=gc2,
                       popup = ~paste(as.character(Name)))
  })
    
      
  
  
  observeEvent(input$lmap_cas1, {
     
        
      if(input$lmap_cas1) {leafletProxy("leafmap1")  %>% 
        showGroup('MAcasinos') %>%
        showGroup('casinosCLOSED') %>%
        showGroup('casinosOPEN')}
      else {leafletProxy("leafmap1")   %>% 
        hideGroup('MAcasinos') %>%
        hideGroup('casinosCLOSED') %>%
        hideGroup('casinosOPEN')}
  })
    
    
    
    
    
  
  observe({
    
      
    map_df<-map_df1()
    
    validate(
      need(!Inf %in% range(map_df$var, na.rm=T) & !-Inf %in% range(map_df$var, na.rm=T), 
           "Please choose another variable to display"),
      need(input$lmap_level1!=" ", "Please choose a variable to display")
      )
    
    pal <- colorNumeric(
      palette = c("white", "violetred"),
      domain = c(range(map_df[,"var"], na.rm=T)))
    
    ## Map Creation
    leafletProxy("leafmap1", data=map_df) %>% clearGroup(group="data")  %>% 
      addCircleMarkers(group="data",
        lng = ~Lng, lat = ~Lat, radius=~1.5*log(TSE), color="#000", 
        fillColor=~pal(var), stroke=T, weight=1, opacity=op,
        fillOpacity=fop,
        popup = ~paste(as.character(school.name), 
                       "\n", 
                       as.character(var), "%"))
    
  })
      
    
    
    
  
  output$leafmap2<-renderLeaflet({
    
    leaflet()%>% 
      setView(lng= -71.75, lat= 42, zoom=8) %>% 
      addProviderTiles("Stamen.TonerLite") %>%
      addMarkers(lng=~Lon, lat=~Lat, icon=star,data=MAcasinos, group="MAcasinos",
                 popup = ~paste(as.character(Name))) %>%
      addMarkers(lng=~Lon, lat=~Lat, data=casinosCLOSED, group="casinosCLOSED",
                 icon=gc1,
                 popup = ~paste(as.character(Name))) %>%
      addMarkers(lng=~Lon, lat=~Lat, data=casinosOPEN, group="casinosOPEN",
                 icon=gc2,
                 popup = ~paste(as.character(Name)))
  })
  
  
  
  
  observeEvent(input$lmap_cas2, {
    
    
    if(input$lmap_cas2) {leafletProxy("leafmap2")  %>% 
        showGroup('MAcasinos') %>%
        showGroup('casinosCLOSED') %>%
        showGroup('casinosOPEN')}
    else {leafletProxy("leafmap2")   %>% 
        hideGroup('MAcasinos') %>%
        hideGroup('casinosCLOSED') %>%
        hideGroup('casinosOPEN')}
  })
  
  
  
  
  
  
  observe({
    
    
    map_df<-map_df2()
    
    validate(
      need(!Inf %in% range(map_df$var, na.rm=T) & !-Inf %in% range(map_df$var, na.rm=T), 
           "Please choose another variable to display"),
      need(input$lmap_level2!=" ", "Please choose a variable to display")
    )
    
    pal <- colorNumeric(
      palette = c("white", "violetred"),
      domain = c(range(map_df[,"var"], na.rm=T)))
    
    ## Map Creation
    leafletProxy("leafmap2", data=map_df) %>% clearGroup("data")  %>% 
      addCircleMarkers(group="data",
                       lng = ~Lng, lat = ~Lat, radius=~1.5*log(TSE), color="#000", 
                       fillColor=~pal(var), stroke=T, weight=1, opacity=op,
                       fillOpacity=fop,
                       popup = ~paste(as.character(school.name), 
                                      "\n", 
                                      as.character(var), "%"))
   
  })
  
  output$leafmap3<-renderLeaflet({
    
    leaflet()%>% 
      setView(lng= -71.75, lat= 42, zoom=8) %>% 
      addProviderTiles("Stamen.TonerLite") %>%
      addMarkers(lng=~Lon, lat=~Lat, icon=star,data=MAcasinos, group="MAcasinos",
                 popup = ~paste(as.character(Name))) %>%
      addMarkers(lng=~Lon, lat=~Lat, data=casinosCLOSED, group="casinosCLOSED",
                 icon=gc1,
                 popup = ~paste(as.character(Name))) %>%
      addMarkers(lng=~Lon, lat=~Lat, data=casinosOPEN, group="casinosOPEN",
                 icon=gc2,
                 popup = ~paste(as.character(Name)))
  })
  
  
  
  
  observeEvent(input$lmap_cas3, {
    
    
    if(input$lmap_cas3) {leafletProxy("leafmap3")  %>% 
        showGroup('MAcasinos') %>%
        showGroup('casinosCLOSED') %>%
        showGroup('casinosOPEN')}
    else {leafletProxy("leafmap3")   %>% 
        hideGroup('MAcasinos') %>%
        hideGroup('casinosCLOSED') %>%
        hideGroup('casinosOPEN')}
  })
  
  
  
  
  
  
  observe({
    
    
    map_df<-map_df3()
    
    validate(
      need(!Inf %in% range(map_df$var, na.rm=T) & !-Inf %in% range(map_df$var, na.rm=T), 
           "Please choose another variable to display"),
      need(input$lmap_level3!=" ", "Please choose a variable to display")
    )
    
    pal <- colorNumeric(
      palette = c("white", "violetred"),
      domain = c(range(map_df[,"var"], na.rm=T)))
    
    ## Map Creation
    leafletProxy("leafmap3", data=map_df) %>% clearGroup("data")  %>% 
      addCircleMarkers(group="data",
                       lng = ~Lng, lat = ~Lat, radius=~1.5*log(TSE), color="#000", 
                       fillColor=~pal(var), stroke=T, weight=1, opacity=op,
                       fillOpacity=fop,
                       popup = ~paste(as.character(school.name), 
                                      "\n", 
                                      as.character(var)))
    
    
  })
  
  output$leafmap4<-renderLeaflet({
    
    leaflet()%>% 
      setView(lng= -71.75, lat= 42, zoom=8) %>% 
      addProviderTiles("Stamen.TonerLite") %>%
      addMarkers(lng=~Lon, lat=~Lat, icon=star,data=MAcasinos, group="MAcasinos",
                 popup = ~paste(as.character(Name))) %>%
      addMarkers(lng=~Lon, lat=~Lat, data=casinosCLOSED, group="casinosCLOSED",
                 icon=gc1,
                 popup = ~paste(as.character(Name))) %>%
      addMarkers(lng=~Lon, lat=~Lat, data=casinosOPEN, group="casinosOPEN",
                 icon=gc2,
                 popup = ~paste(as.character(Name)))
  })
  
  
  
  
  observeEvent(input$lmap_cas4, {
    
    
    if(input$lmap_cas4) {leafletProxy("leafmap4")  %>% 
        showGroup('MAcasinos') %>%
        showGroup('casinosCLOSED') %>%
        showGroup('casinosOPEN')}
    else {leafletProxy("leafmap4")   %>% 
        hideGroup('MAcasinos') %>%
        hideGroup('casinosCLOSED') %>%
        hideGroup('casinosOPEN')}
  })
  
  
  
  
  
  
  observe({
    
    
    map_df<-map_df4()
    
    validate(
      need(!Inf %in% range(map_df$var, na.rm=T) & !-Inf %in% range(map_df$var, na.rm=T), 
           "Please choose another variable to display"),
      need(input$lmap_level4!=" ", "Please choose a variable to display")
    )
    
    pal <- colorNumeric(
      palette = c("white", "violetred"),
      domain = c(range(map_df[,"var"], na.rm=T)))
    
    ## Map Creation
    proxy=leafletProxy("leafmap4", data=map_df) %>% clearGroup("data")  %>% 
      addCircleMarkers(group="data",
                       lng = ~Lng, lat = ~Lat, radius=~1.5*log(TSE), color="#000", 
                       fillColor=~pal(var), stroke=T, weight=1, opacity=op,
                       fillOpacity=fop,
                       popup = ~paste(as.character(school.name), 
                                      "\n", 
                                      as.character(var), "%"))
  
    
  })
  
  
  output$leafmap5<-renderLeaflet({
    
    leaflet()%>% 
      setView(lng= -71.75, lat= 42, zoom=8) %>% 
      addProviderTiles("Stamen.TonerLite") %>%
      addMarkers(lng=~Lon, lat=~Lat, icon=star,data=MAcasinos, group="MAcasinos",
                 popup = ~paste(as.character(Name))) %>%
      addMarkers(lng=~Lon, lat=~Lat, data=casinosCLOSED, group="casinosCLOSED",
                 icon=gc1,
                 popup = ~paste(as.character(Name))) %>%
      addMarkers(lng=~Lon, lat=~Lat, data=casinosOPEN, group="casinosOPEN",
                 icon=gc2,
                 popup = ~paste(as.character(Name)))
  })
  
  
  
  
  observeEvent(input$lmap_cas5, {
    
    
    if(input$lmap_cas5) {leafletProxy("leafmap5")  %>% 
        showGroup('MAcasinos') %>%
        showGroup('casinosCLOSED') %>%
        showGroup('casinosOPEN')}
    else {leafletProxy("leafmap5")   %>% 
        hideGroup('MAcasinos') %>%
        hideGroup('casinosCLOSED') %>%
        hideGroup('casinosOPEN')}
  })
  
  
  
  
  
  
  observe({
    
    
    map_df<-map_df5()
    
    validate(
      need(!Inf %in% range(map_df$var, na.rm=T) & !-Inf %in% range(map_df$var, na.rm=T), 
           "Please choose another variable to display"),
      need(input$lmap_level5!=" ", "Please choose a variable to display")
    )
    
    pal <- colorNumeric(
      palette = c("white", "violetred"),
      domain = c(range(map_df[,"var"], na.rm=T)))
    
    ## Map Creation
   leafletProxy("leafmap5", data=map_df) %>% clearGroup("data")  %>% 
      addCircleMarkers(group="data",
                       lng = ~Lng, lat = ~Lat, radius=~1.5*log(TSE), color="#000", 
                       fillColor=~pal(var), stroke=T, weight=1, opacity=op,
                       fillOpacity=fop,
                       popup = ~paste(as.character(school.name), 
                                      "\n", 
                                      as.character(var), "%"))
    
    
  })
  
  
  output$leafmap6<-renderLeaflet({
    
    leaflet()%>% 
      setView(lng= -71.75, lat= 42, zoom=8) %>% 
      addProviderTiles("Stamen.TonerLite") %>%
      addMarkers(lng=~Lon, lat=~Lat, icon=star,data=MAcasinos, group="MAcasinos",
                 popup = ~paste(as.character(Name))) %>%
      addMarkers(lng=~Lon, lat=~Lat, data=casinosCLOSED, group="casinosCLOSED",
                 icon=gc1,
                 popup = ~paste(as.character(Name))) %>%
      addMarkers(lng=~Lon, lat=~Lat, data=casinosOPEN, group="casinosOPEN",
                 icon=gc2,
                 popup = ~paste(as.character(Name)))
  })
  
  
  
  
  observeEvent(input$lmap_cas6, {
    
    
    if(input$lmap_cas6) {leafletProxy("leafmap6")  %>% 
        showGroup('MAcasinos') %>%
        showGroup('casinosCLOSED') %>%
        showGroup('casinosOPEN')}
    else {leafletProxy("leafmap6")   %>% 
        hideGroup('MAcasinos') %>%
        hideGroup('casinosCLOSED') %>%
        hideGroup('casinosOPEN')}
  })
  
  
  
  
  
  
  observe({
    
    
    map_df<-map_df6()
    
    validate(
      need(!Inf %in% range(map_df$var, na.rm=T) & !-Inf %in% range(map_df$var, na.rm=T), 
           "Please choose another variable to display"),
      need(input$lmap_level6!=" ", "Please choose a variable to display")
    )
    
    pal <- colorNumeric(
      palette = c("white", "violetred"),
      domain = c(range(map_df[,"var"], na.rm=T)))
    
    ## Map Creation
    leafletProxy("leafmap6", data=map_df) %>% clearGroup("data")  %>% 
      addCircleMarkers(group="data",
                       lng = ~Lng, lat = ~Lat, radius=~1.5*log(TSE), color="#000", 
                       fillColor=~pal(var), stroke=T, weight=1, opacity=op,
                       fillOpacity=fop,
                       popup = ~paste(as.character(school.name), 
                                      "\n", 
                                      as.character(var), "%"))
    
    
  })
  
  
  
  
  
  output$leafmap7<-renderLeaflet({
    
    leaflet()%>% 
      setView(lng= -71.75, lat= 42, zoom=8) %>% 
      addProviderTiles("Stamen.TonerLite") %>%
      addMarkers(lng=~Lon, lat=~Lat, icon=star,data=MAcasinos, group="MAcasinos",
                 popup = ~paste(as.character(Name))) %>%
      addMarkers(lng=~Lon, lat=~Lat, data=casinosCLOSED, group="casinosCLOSED",
                 icon=gc1,
                 popup = ~paste(as.character(Name))) %>%
      addMarkers(lng=~Lon, lat=~Lat, data=casinosOPEN, group="casinosOPEN",
                 icon=gc2,
                 popup = ~paste(as.character(Name)))
  })
  
  
  
  
  observeEvent(input$lmap_cas7, {
    
    
    if(input$lmap_cas7) {leafletProxy("leafmap7")  %>% 
        showGroup('MAcasinos') %>%
        showGroup('casinosCLOSED') %>%
        showGroup('casinosOPEN')}
    else {leafletProxy("leafmap7")   %>% 
        hideGroup('MAcasinos') %>%
        hideGroup('casinosCLOSED') %>%
        hideGroup('casinosOPEN')}
  })
  
  
  
  
  
  
  observe({
    
    
    map_df<-map_df7()
    
    validate(
      need(!Inf %in% range(map_df$var, na.rm=T) & !-Inf %in% range(map_df$var, na.rm=T), 
           "Please choose another variable to display"),
      need(input$lmap_level7!=" ", "Please choose a variable to display")
    )
    
    pal <- colorNumeric(
      palette = c("white", "violetred"),
      domain = c(range(map_df[,"var"], na.rm=T)))
    
    ## Map Creation
    leafletProxy("leafmap7", data=map_df) %>% clearGroup("data")  %>% 
      addCircleMarkers(group="data",
                       lng = ~Lng, lat = ~Lat, radius=~1.5*log(TSE), color="#000", 
                       fillColor=~pal(var), stroke=T, weight=1, opacity=op,
                       fillOpacity=fop,
                       popup = ~paste(as.character(school.name), 
                                      "\n", 
                                      as.character(var), "%"))
   
    
  })
  
  
  output$leafmap8<-renderLeaflet({
    
    leaflet()%>% 
      setView(lng= -71.75, lat= 42, zoom=8) %>% 
      addProviderTiles("Stamen.TonerLite") %>%
      addMarkers(lng=~Lon, lat=~Lat, icon=star,data=MAcasinos, group="MAcasinos",
                 popup = ~paste(as.character(Name))) %>%
      addMarkers(lng=~Lon, lat=~Lat, data=casinosCLOSED, group="casinosCLOSED",
                 icon=gc1,
                 popup = ~paste(as.character(Name))) %>%
      addMarkers(lng=~Lon, lat=~Lat, data=casinosOPEN, group="casinosOPEN",
                 icon=gc2,
                 popup = ~paste(as.character(Name)))
  })
  
  
  
  
  observeEvent(input$lmap_cas8, {
    
    
    if(input$lmap_cas8) {leafletProxy("leafmap8")  %>% 
        showGroup('MAcasinos') %>%
        showGroup('casinosCLOSED') %>%
        showGroup('casinosOPEN')}
    else {leafletProxy("leafmap8")   %>% 
        hideGroup('MAcasinos') %>%
        hideGroup('casinosCLOSED') %>%
        hideGroup('casinosOPEN')}
  })
  
  
  
  
  
  
  observe({
    
    
    map_df<-map_df8()
    
    validate(
      need(!Inf %in% range(map_df$var, na.rm=T) & !-Inf %in% range(map_df$var, na.rm=T), 
           "Please choose another variable to display"),
      need(input$lmap_level8!=" ", "Please choose a variable to display")
    )
    
    pal <- colorNumeric(
      palette = c("white", "violetred"),
      domain = c(range(map_df[,"var"], na.rm=T)))
    
    ## Map Creation
   leafletProxy("leafmap8", data=map_df) %>% clearGroup("data")  %>% 
      addCircleMarkers(group="data",
                       lng = ~Lng, lat = ~Lat, radius=~1.5*log(TSE), color="#000", 
                       fillColor=~pal(var), stroke=T, weight=1, opacity=op,
                       fillOpacity=fop,
                       popup = ~paste(as.character(school.name), 
                                      "\n", 
                                      as.character(var), "%"))
    
    
  })
  
  
  output$leafmap9<-renderLeaflet({
    
    leaflet()%>% 
      setView(lng= -71.75, lat= 42, zoom=8) %>% 
      addProviderTiles("Stamen.TonerLite") %>%
      addMarkers(lng=~Lon, lat=~Lat, icon=star,data=MAcasinos, group="MAcasinos",
                 popup = ~paste(as.character(Name))) %>%
      addMarkers(lng=~Lon, lat=~Lat, data=casinosCLOSED, group="casinosCLOSED",
                 icon=gc1,
                 popup = ~paste(as.character(Name))) %>%
      addMarkers(lng=~Lon, lat=~Lat, data=casinosOPEN, group="casinosOPEN",
                 icon=gc2,
                 popup = ~paste(as.character(Name)))
  })
  
  
  
  
  observeEvent(input$lmap_cas9, {
    
    
    if(input$lmap_cas9) {leafletProxy("leafmap9")  %>% 
        showGroup('MAcasinos') %>%
        showGroup('casinosCLOSED') %>%
        showGroup('casinosOPEN')}
    else {leafletProxy("leafmap9")   %>% 
        hideGroup('MAcasinos') %>%
        hideGroup('casinosCLOSED') %>%
        hideGroup('casinosOPEN')}
  })
  
  
  
  
  
  
  observe({
    
    
    map_df<-map_df9()
    
    validate(
      need(!Inf %in% range(map_df$var, na.rm=T) & !-Inf %in% range(map_df$var, na.rm=T), 
           "Please choose another variable to display"),
      need(input$lmap_level9!=" ", "Please choose a variable to display")
    )
    
    pal <- colorNumeric(
      palette = c("white", "violetred"),
      domain = c(range(map_df[,"var"], na.rm=T)))
    
    ## Map Creation
    leafletProxy("leafmap9", data=map_df) %>% clearGroup("data")  %>% 
      addCircleMarkers(group="data",
                       lng = ~Lng, lat = ~Lat, radius=~1.5*log(TSE), color="#000", 
                       fillColor=~pal(var), stroke=T, weight=1, opacity=op,
                       fillOpacity=fop,
                       popup = ~paste(as.character(school.name), 
                                      "\n", 
                                      as.character(var)))
    
    
  })
  
  
  output$leafmap10<-renderLeaflet({
    
    leaflet()%>% 
      setView(lng= -71.75, lat= 42, zoom=8) %>% 
      addProviderTiles("Stamen.TonerLite") %>%
      addMarkers(lng=~Lon, lat=~Lat, icon=star,data=MAcasinos, group="MAcasinos",
                 popup = ~paste(as.character(Name))) %>%
      addMarkers(lng=~Lon, lat=~Lat, data=casinosCLOSED, group="casinosCLOSED",
                 icon=gc1,
                 popup = ~paste(as.character(Name))) %>%
      addMarkers(lng=~Lon, lat=~Lat, data=casinosOPEN, group="casinosOPEN",
                 icon=gc2,
                 popup = ~paste(as.character(Name)))
  })
  
  
  
  
  observeEvent(input$lmap_cas10, {
    
    
    if(input$lmap_cas10) {leafletProxy("leafmap10")  %>% 
        showGroup('MAcasinos') %>%
        showGroup('casinosCLOSED') %>%
        showGroup('casinosOPEN')}
    else {leafletProxy("leafmap10")   %>% 
        hideGroup('MAcasinos') %>%
        hideGroup('casinosCLOSED') %>%
        hideGroup('casinosOPEN')}
  })
  
  
  
  
  
  
  observe({
    
    
    map_df<-map_df10()
    
    validate(
      need(!Inf %in% range(map_df$var, na.rm=T) & !-Inf %in% range(map_df$var, na.rm=T), 
           "Please choose another variable to display"),
      need(input$lmap_level10!=" ", "Please choose a variable to display")
    )
    
    pal <- colorNumeric(
      palette = c("white", "violetred"),
      domain = c(range(map_df[,"var"], na.rm=T)))
    
    ## Map Creation
   leafletProxy("leafmap10", data=map_df) %>% clearGroup("data")  %>% 
      addCircleMarkers(group="data",
                       lng = ~Lng, lat = ~Lat, radius=~1.5*log(TSE), color="#000", 
                       fillColor=~pal(var), stroke=T, weight=1, opacity=op,
                       fillOpacity=fop,
                       popup = ~paste(as.character(school.name), 
                                      "\n", 
                                      as.character(var)))
    
  })
  
  
  output$leafmap11<-renderLeaflet({
    
    leaflet()%>% 
      setView(lng= -71.75, lat= 42, zoom=8) %>% 
      addProviderTiles("Stamen.TonerLite") %>%
      addMarkers(lng=~Lon, lat=~Lat, icon=star,data=MAcasinos, group="MAcasinos",
                 popup = ~paste(as.character(Name))) %>%
      addMarkers(lng=~Lon, lat=~Lat, data=casinosCLOSED, group="casinosCLOSED",
                 icon=gc1,
                 popup = ~paste(as.character(Name))) %>%
      addMarkers(lng=~Lon, lat=~Lat, data=casinosOPEN, group="casinosOPEN",
                 icon=gc2,
                 popup = ~paste(as.character(Name)))
  })
  
  
  
  
  observeEvent(input$lmap_cas11, {
    
    
    if(input$lmap_cas11) {leafletProxy("leafmap11")  %>% 
        showGroup('MAcasinos') %>%
        showGroup('casinosCLOSED') %>%
        showGroup('casinosOPEN')}
    else {leafletProxy("leafmap11")   %>% 
        hideGroup('MAcasinos') %>%
        hideGroup('casinosCLOSED') %>%
        hideGroup('casinosOPEN')}
  })
  
  
  
  
  
  
  observe({
    
    
    map_df<-map_df11()
    
    validate(
      need(!Inf %in% range(map_df$var, na.rm=T) & !-Inf %in% range(map_df$var, na.rm=T), 
           "Please choose another variable to display"),
      need(input$lmap_level11!=" ", "Please choose a variable to display")
    )
    
    pal <- colorNumeric(
      palette = c("white", "violetred"),
      domain = c(range(map_df[,"var"], na.rm=T)))
    
    ## Map Creation
    leafletProxy("leafmap11", data=map_df) %>% clearGroup("data")  %>% 
      addCircleMarkers(group="data",
                       lng = ~Lng, lat = ~Lat, radius=~1.5*log(TSE), color="#000", 
                       fillColor=~pal(var), stroke=T, weight=1, opacity=op,
                       fillOpacity=fop,
                       popup = ~paste(as.character(school.name), 
                                      "\n", 
                                      as.character(var)))
    
    
  })
  
  
  
  
  
  
  output$leafmap12<-renderLeaflet({
    
    leaflet()%>% 
      setView(lng= -71.75, lat= 42, zoom=8) %>% 
      addProviderTiles("Stamen.TonerLite") %>%
      addMarkers(lng=~Lon, lat=~Lat, icon=star,data=MAcasinos, group="MAcasinos",
                 popup = ~paste(as.character(Name))) %>%
      addMarkers(lng=~Lon, lat=~Lat, data=casinosCLOSED, group="casinosCLOSED",
                 icon=gc1,
                 popup = ~paste(as.character(Name))) %>%
      addMarkers(lng=~Lon, lat=~Lat, data=casinosOPEN, group="casinosOPEN",
                 icon=gc2,
                 popup = ~paste(as.character(Name)))
  })
  
  
  
  
  observeEvent(input$lmap_cas12, {
    
    
    if(input$lmap_cas12) {leafletProxy("leafmap12")  %>% 
        showGroup('MAcasinos') %>%
        showGroup('casinosCLOSED') %>%
        showGroup('casinosOPEN')}
    else {leafletProxy("leafmap12")   %>% 
        hideGroup('MAcasinos') %>%
        hideGroup('casinosCLOSED') %>%
        hideGroup('casinosOPEN')}
  })
  
  
  
  
  
  
  observe({
    
    
    map_df<-map_df12()
    
    validate(
      need(!Inf %in% range(map_df$var, na.rm=T) & !-Inf %in% range(map_df$var, na.rm=T), 
           "Please choose another variable to display"),
      need(input$lmap_level12!=" ", "Please choose a variable to display")
    )
    
    pal <- colorNumeric(
      palette = c("white", "violetred"),
      domain = c(range(map_df[,"var"], na.rm=T)))
    
    ## Map Creation
    leafletProxy("leafmap12", data=map_df) %>% clearGroup("data")  %>% 
      addCircleMarkers(group="data",
                       lng = ~Lng, lat = ~Lat, radius=~1.5*log(TSE), color="#000", 
                       fillColor=~pal(var), stroke=T, weight=1, opacity=op,
                       fillOpacity=fop,
                       popup = ~paste(as.character(school.name), 
                                      "\n", 
                                      as.character(var)))
    
    
  })
  
  
  output$leafmap13<-renderLeaflet({
    
    leaflet()%>% 
      setView(lng= -71.75, lat= 42, zoom=8) %>% 
      addProviderTiles("Stamen.TonerLite") %>%
      addMarkers(lng=~Lon, lat=~Lat, icon=star,data=MAcasinos, group="MAcasinos",
                 popup = ~paste(as.character(Name))) %>%
      addMarkers(lng=~Lon, lat=~Lat, data=casinosCLOSED, group="casinosCLOSED",
                 icon=gc1,
                 popup = ~paste(as.character(Name))) %>%
      addMarkers(lng=~Lon, lat=~Lat, data=casinosOPEN, group="casinosOPEN",
                 icon=gc2,
                 popup = ~paste(as.character(Name)))
  })
  
  
  
  
  observeEvent(input$lmap_cas13, {
    
    
    if(input$lmap_cas13) {leafletProxy("leafmap13")  %>% 
        showGroup('MAcasinos') %>%
        showGroup('casinosCLOSED') %>%
        showGroup('casinosOPEN')}
    else {leafletProxy("leafmap13")   %>% 
        hideGroup('MAcasinos') %>%
        hideGroup('casinosCLOSED') %>%
        hideGroup('casinosOPEN')}
  })
  
  
  
  
  
  
  observe({
    
    
    map_df<-map_df13()
    
    validate(
      need(!Inf %in% range(map_df$var, na.rm=T) & !-Inf %in% range(map_df$var, na.rm=T), 
           "Please choose another variable to display"),
      need(input$lmap_level13!=" ", "Please choose a variable to display")
    )
    
    pal <- colorNumeric(
      palette = c("white", "violetred"),
      domain = c(range(map_df[,"var"], na.rm=T)))
    
    ## Map Creation
   leafletProxy("leafmap13", data=map_df) %>% clearGroup("data")  %>% 
      addCircleMarkers(group="data",
                       lng = ~Lng, lat = ~Lat, radius=~1.5*log(TSE), color="#000", 
                       fillColor=~pal(var), stroke=T, weight=1, opacity=op,
                       fillOpacity=fop,
                       popup = ~paste(as.character(school.name), 
                                      "\n", 
                                      as.character(var), "%"))
   
    
  })
  
  
  output$leafmap14<-renderLeaflet({
    
    leaflet()%>% 
      setView(lng= -71.75, lat= 42, zoom=8) %>% 
      addProviderTiles("Stamen.TonerLite") %>%
      addMarkers(lng=~Lon, lat=~Lat, icon=star,data=MAcasinos, group="MAcasinos",
                 popup = ~paste(as.character(Name))) %>%
      addMarkers(lng=~Lon, lat=~Lat, data=casinosCLOSED, group="casinosCLOSED",
                 icon=gc1,
                 popup = ~paste(as.character(Name))) %>%
      addMarkers(lng=~Lon, lat=~Lat, data=casinosOPEN, group="casinosOPEN",
                 icon=gc2,
                 popup = ~paste(as.character(Name)))
  })
  
  
  
  
  observeEvent(input$lmap_cas14, {
    
    
    if(input$lmap_cas14) {leafletProxy("leafmap14")  %>% 
        showGroup('MAcasinos') %>%
        showGroup('casinosCLOSED') %>%
        showGroup('casinosOPEN')}
    else {leafletProxy("leafmap14")   %>% 
        hideGroup('MAcasinos') %>%
        hideGroup('casinosCLOSED') %>%
        hideGroup('casinosOPEN')}
  })
  
  
  
  
  
  
  observe({
    
    
    map_df<-map_df14()
    
    validate(
      need(!Inf %in% range(map_df$var, na.rm=T) & !-Inf %in% range(map_df$var, na.rm=T), 
           "Please choose another variable to display"),
      need(input$lmap_level2!=" ", "Please choose a variable to display")
    )
    
    pal <- colorNumeric(
      palette = c("white", "violetred"),
      domain = c(range(map_df[,"var"], na.rm=T)))
    
    ## Map Creation
    leafletProxy("leafmap14", data=map_df) %>% clearGroup("data")  %>% 
      addCircleMarkers(group="data",
                       lng = ~Lng, lat = ~Lat, radius=~1.5*log(TSE), color="#000", 
                       fillColor=~pal(var), stroke=T, weight=1, opacity=op,
                       fillOpacity=fop,
                       popup = ~paste(as.character(school.name), 
                                      "\n", 
                                      as.character(var)))
    
    
  })
  
 
## map legend
  #1
output$AAlegend <- renderPlot({

    paint.brush = colorRampPalette(colors=c("white", "violetred"))
    cols <- paint.brush(25)
    leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level1], range.table[2,input$lmap_level1],
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
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level1], range.table[2,input$lmap_level1],
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
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level1], range.table[2,input$lmap_level1],
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
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level1], range.table[2,input$lmap_level1],
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
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level1], range.table[2,input$lmap_level1],
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
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level1], range.table[2,input$lmap_level1],
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
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level1], range.table[2,input$lmap_level1],
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
  #1
output$FEMlegend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level2], range.table[2,input$lmap_level2],
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
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level2], range.table[2,input$lmap_level2],
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
  #3
output$PREKlegend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level3], range.table[2,input$lmap_level3],
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
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level3], range.table[2,input$lmap_level3],
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
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level3], range.table[2,input$lmap_level3],
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
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level3], range.table[2,input$lmap_level3],
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
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level3], range.table[2,input$lmap_level3],
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
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level3], range.table[2,input$lmap_level3],
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
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level3], range.table[2,input$lmap_level3],
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
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level3], range.table[2,input$lmap_level3],
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
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level3], range.table[2,input$lmap_level3],
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
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level3], range.table[2,input$lmap_level3],
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
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level3], range.table[2,input$lmap_level3],
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
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level3], range.table[2,input$lmap_level3],
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
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level3], range.table[2,input$lmap_level3],
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
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level3], range.table[2,input$lmap_level3],
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
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level3], range.table[2,input$lmap_level3],
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
  #4
output$P_ELL_legend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level4], range.table[2,input$lmap_level4],
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
output$P_FLNE_legend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level4], range.table[2,input$lmap_level4],
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
output$P_DISAB_legend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level4], range.table[2,input$lmap_level4],
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
output$P_LOW_legend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level4], range.table[2,input$lmap_level4],
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
output$P_ECODIS_legend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level4], range.table[2,input$lmap_level4],
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
output$P_HIGH_legend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level4], range.table[2,input$lmap_level4],
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

  #5
output$CR_ELL_legend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level5], range.table[2,input$lmap_level5],
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
output$SR_ELL_legend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level5], range.table[2,input$lmap_level5],
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
output$IR_ELL_legend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level5], range.table[2,input$lmap_level5],
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
#6
output$CR_DISAB_legend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level6], range.table[2,input$lmap_level6],
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
output$SR_DISAB_legend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level6], range.table[2,input$lmap_level6],
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
output$IR_DISAB_legend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level6], range.table[2,input$lmap_level6],
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
#7
output$CR_LOW_legend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level7], range.table[2,input$lmap_level7],
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
output$SR_LOW_legend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level7], range.table[2,input$lmap_level7],
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
output$IR_LOW_legend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level7], range.table[2,input$lmap_level7],
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
#13
output$CR_ECODIS_legend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level13], range.table[2,input$lmap_level13],
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
output$SR_ECODIS_legend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level13], range.table[2,input$lmap_level13],
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
output$IR_ECODIS_legend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level13], range.table[2,input$lmap_level13],
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


#8
output$CR_HIGH_legend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level8], range.table[2,input$lmap_level8],
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
output$SR_HIGH_legend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level8], range.table[2,input$lmap_level8],
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
output$IR_HIGH_legend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level8], range.table[2,input$lmap_level8],
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

#9
output$CE_ELL_legend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level9], range.table[2,input$lmap_level9],
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
output$SE_ELL_legend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level9], range.table[2,input$lmap_level9],
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
#10
output$CE_DISAB_legend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level10], range.table[2,input$lmap_level10],
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
output$SE_DISAB_legend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level10], range.table[2,input$lmap_level10],
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
#11
output$CE_LOW_legend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level11], range.table[2,input$lmap_level11],
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
output$SE_LOW_legend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level11], range.table[2,input$lmap_level11],
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
#14
output$CE_ECODIS_legend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level14], range.table[2,input$lmap_level14],
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
output$SE_ECODIS_legend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level14], range.table[2,input$lmap_level14],
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


#12
output$CE_HIGH_legend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level12], range.table[2,input$lmap_level12],
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
output$SE_HIGH_legend <- renderPlot({
  
  paint.brush = colorRampPalette(colors=c("white", "violetred"))
  cols <- paint.brush(25)
  leg_dat <- data_frame(y = seq(range.table[1,input$lmap_level12], range.table[2,input$lmap_level12],
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