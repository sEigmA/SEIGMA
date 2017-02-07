#######################################
## Title: Marital server.R           ##
## Author(s): Emily Ramos, Arvind    ##
##            Ramakrishnan, Jenna    ##
##            Kiridly, Steve Lauer   ##
##            Xuelian Li, Justin     ##
##            Baldwin                ##
## Date Created:  10/22/2014         ##
## Date Modified: 01/31/2017  JB     ##
#######################################

shinyServer(function(input, output, session) {
  ## mar_df is a reactive dataframe. Necessary for when summary/plot/map have common input (Multiple Variables). Not in this project
  mar_df <- reactive({
    
    
    ## make year a vector based on input variable
    if(!is.null(input$sum_year))
      years <- input$sum_year
    ## if none selected, put all years in vector
    if(is.null(input$sum_year))
      years <- c("2006-2010","2007-2011", "2008-2012","2009-2013", "2010-2014",
                 "2011-2015")
    
    ## Filter the data by the chosen Five Year Range 
    mar_df <- mar_data %>%
      filter(Five_Year_Range %in% years) %>%
      select(1:4, Gender, Five_Year_Range, Population, Never_Married_pct, Never_Married_pct_error, 
             Married_pct,Married_pct_error,
             Separated_pct,Separated_pct_error, Widowed_pct, Widowed_pct_error, 
             Divorced_pct, Divorced_pct_error) %>%
      arrange(Region, Gender)
    ## Output reactive dataframe
    mar_df    
  })
  
  ## Create summary table
  output$summary <- renderDataTable({
    ## Make reactive dataframe into regular dataframe
    mar_df <- mar_df()
    
    ## make gender a vector based on input variable
    if(!is.null(input$sum_gender))
      genders <- input$sum_gender
    ## if none selected, put all genders in vector
    if(is.null(input$sum_gender))
      genders <- c("Female", "Male")
    
    ## make municipals a vector based on input variable
    if(!is.null(input$sum_muni))
      munis <- input$sum_muni
    ## if none selected, put all municipals in vector
    if(is.null(input$sum_muni))
      munis <- MA_municipals
    
    ## if the user checks the meanUS box or the meanMA box, add those to counties vector
    if(input$US_mean){
      if(input$MA_mean){
        munis <- c("United States", "MA", munis) ## US and MA  
      } else{
        munis <- c("United States", munis) ## US only
      }
    } else{
      if(input$MA_mean){
        munis <- c("MA", munis) ## US only ## MA only
      }
    }
    
    ## create a dataframe consisting only of counties in vector
    mar_df <- mar_df %>%
      filter(Gender %in% genders, Region %in% munis) %>%
      select(4:length(colnames(mar_df)))
    
    colnames(mar_df) <- gsub("_", " ", colnames(mar_df))
    colnames(mar_df) <- gsub("pct error", "error %", colnames(mar_df))
    colnames(mar_df) <- gsub("pct", "%", colnames(mar_df))
    
    return(mar_df)
  }, options=list(searching = FALSE, orderClasses = TRUE)) # there are a bunch of options to edit the appearance of datatables, this removes one of the ugly features
  
  # 
  # mar_plot_df <- reactive({
  #   ## Filter the data by the chosen Five Year Range 
  #   mar_plot_df <- mar_data %>%
  #     filter(Five_Year_Range == input$plot_year) %>%
  #     select(1:4, Gender, Five_Year_Range, Population, Never_Married_pct, Married_pct,
  #            Separated_pct, Widowed_pct, Divorced_pct) %>%
  #     arrange(Region, Gender)
  #   ## Output reactive dataframe
  #   mar_plot_df    
  # })
  # 
  # ## create the plot of the data
  # ## for the Google charts plot
  # output$plot_US <- reactive({
  #   ## make reactive dataframe into regular dataframe
  #   mar_plot_df <- mar_plot_df()
  #   
  #   ## make counties a vector based on input variable
  #   munis <- "United States"
  #   
  #   plot_df <- mar_plot_df %>%
  #     filter(Region %in% munis)
  #   
  #   ## put data into form that googleCharts understands (this unmelts the dataframe)
  #   melted_plot_df <- melt(plot_df, id.vars = "Gender", 
  #                          measure.vars = c("Never_Married_pct", "Married_pct", "Separated_pct", 
  #                                           "Widowed_pct", "Divorced_pct"),
  #                          variable.name = "Marital_Status", value.name = "Population_pct")
  #   
  #   g <- dcast(melted_plot_df, Marital_Status ~ Gender, 
  #              value.var = "Population_pct")
  #   
  #   g$Marital_Status <- gsub("_", " ", g$Marital_Status)
  #   g$Marital_Status <- gsub("Pct", "%", g$Marital_Status)
  #   
  #   ## this outputs the google data to be used in the UI to create the dataframe
  #   list(
  #     data=googleDataTable(g))
  # })
  # 
  # 
  # ## create the plot of the MA data
  # output$plot_MA <- reactive({
  #   ## make reactive dataframe into regular dataframe
  #   mar_plot_df <- mar_plot_df()
  #   
  #   ## make counties a vector based on input variable
  #   munis <- "MA"
  #   
  #   plot_df <- mar_plot_df %>%
  #     filter(Region %in% munis)
  #   
  #   ## put data into form that googleCharts understands (this unmelts the dataframe)
  #   melted_plot_df <- melt(plot_df, id.vars = "Gender", 
  #                          measure.vars = c("Never_Married_pct", "Married_pct", "Separated_pct", 
  #                                           "Widowed_pct", "Divorced_pct"),
  #                          variable.name = "Marital_Status", value.name = "Population_pct")
  #   
  #   g <- dcast(melted_plot_df, Marital_Status ~ Gender, 
  #              value.var = "Population_pct")
  #   
  #   g$Marital_Status <- gsub("_", " ", g$Marital_Status)
  #   g$Marital_Status <- gsub("Pct", "%", g$Marital_Status)
  #   
  #   ## this outputs the google data to be used in the UI to create the dataframe
  #   list(
  #     data=googleDataTable(g))
  # })
  # 
  # ## create the plot of the MA data
  # output$plot_county <- reactive({
  #   ## make reactive dataframe into regular dataframe
  #   mar_plot_df <- mar_plot_df()
  #   
  #   ## find the county of the municipal
  #   county <- mar_plot_df$County[which(mar_plot_df$Municipal==input$plot_muni)]
  #   ## make counties a vector based on input variable
  #   munis <- mar_plot_df$Region[match(county, mar_plot_df$Region)]
  #   
  #   plot_df <- mar_plot_df %>%
  #     filter(Region %in% munis)
  #   
  #   ## put data into form that googleCharts understands (this unmelts the dataframe)
  #   melted_plot_df <- melt(plot_df, id.vars = "Gender", 
  #                          measure.vars = c("Never_Married_pct", "Married_pct", "Separated_pct", 
  #                                           "Widowed_pct", "Divorced_pct"),
  #                          variable.name = "Marital_Status", value.name = "Population_pct")
  #   
  #   g <- dcast(melted_plot_df, Marital_Status ~ Gender, 
  #              value.var = "Population_pct")
  #   
  #   g$Marital_Status <- gsub("_", " ", g$Marital_Status)
  #   g$Marital_Status <- gsub("Pct", "%", g$Marital_Status)
  #   
  #   ## this outputs the google data to be used in the UI to create the dataframe
  #   list(
  #     data = googleDataTable(g), options = list(
  #       title = paste("Marital Status Statisics for", munis[1])))
  # })
  # 
  # ## create the plot of the MA data
  # output$plot_muni <- reactive({
  #   ## make reactive dataframe into regular dataframe
  #   mar_plot_df <- mar_plot_df()
  #   
  #   ## make counties a vector based on input variable
  #   munis <- input$plot_muni
  #   
  #   plot_df <- mar_plot_df %>%
  #     filter(Region %in% munis)
  #   
  #   ## put data into form that googleCharts understands (this unmelts the dataframe)
  #   melted_plot_df <- melt(plot_df, id.vars = "Gender", 
  #                          measure.vars = c("Never_Married_pct", "Married_pct", "Separated_pct", 
  #                                           "Widowed_pct", "Divorced_pct"),
  #                          variable.name = "Marital_Status", value.name = "Population_pct")
  #   
  #   g <- dcast(melted_plot_df, Marital_Status ~ Gender, 
  #              value.var = "Population_pct")
  #   
  #   g$Marital_Status <- gsub("_", " ", g$Marital_Status)
  #   g$Marital_Status <- gsub("Pct", "%", g$Marital_Status)
  #   
  #   ## this outputs the google data to be used in the UI to create the dataframe
  #   list(
  #     data = googleDataTable(g), options = list(
  #       title = paste("Marital Status Statistics for", munis)))
  # })
  # 
  #############################################################################################
  #       ggplot
  #       dataframe
  #       make one for males and one for females
  
  plot_mar_df <- reactive({
    munis_p<-input$plot_muni
    
    if(input$plotUS_mean){
      if(input$plotMA_mean){
        munis_p <- c("United States", "MA", munis_p) ## US and MA  
      } else{
        munis_p <- c("United States", munis_p) ## US only
      }
    } else{
      if(input$plotMA_mean){
        munis_p <- c("MA", munis_p) ## US only ## MA only
      }
    }
    
    ## Filter the data by the chosen Five Year Range
    if(is.null(munis_p)){munis_p <-"MA"}
    
    pvars <- c(input$plotvar, paste(input$plotvar, "error", sep="_"))
    vars <- which(names(mar_data) %in% pvars)
    
    plot_mar_df <- mar_data %>%
      filter(Region %in% munis_p) %>%
      select(c(22,4,5,vars)) 
    # %>%
    #   spread(Municipal, Median.Rent)
    # 
    plot_mar_df$Year <- as.integer(sapply(strsplit(as.character(plot_mar_df$Five_Year_Range), split="-"), FUN=function(x){x[1]}))+2
    names(plot_mar_df)[c(4,5)] <- c("Var", "Error")
    
    ## Output reactive dataframe
    plot_mar_df
  })
  
  output$fmplot <- renderPlot({
    
    #make one for males and one for females
    
    # 
    pdf <- plot_mar_df()

    ap=0.5
    sz=1
    
    p=ggplot(pdf, aes(x=Year, y=Var, colour=Region))+
      geom_errorbarh(aes(xmax = Year + 2, xmin = Year - 2, height = 0,colour=Region),alpha=ap/2, size=sz/2)+
      geom_errorbar(aes(ymin = Var-Error, ymax = Var+Error,colour=Region),alpha=ap,size=sz, width=0.125)+
      ylab("Percent of Marital Status (%)")+
      scale_color_manual(values=cbbPalette, guide="legend")+
      scale_x_continuous(breaks=c(2006, 2008, 2010, 2012, 2014))+
      geom_point(aes(colour=Region),size=4,alpha=1)+
      geom_line(aes(colour=Region, linetype=Gender),size=2,alpha=1)+
      theme_bw() + 
      theme(plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank() )+
      theme(panel.border= element_blank())+
      theme(axis.line.x = element_line(color="black", size = 0.5),
            axis.line.y = element_line(color="black", size = 0.5))+
      theme(axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14))+
      theme(legend.title=element_text(size=16),
            legend.text=element_text(size=14))+
      ggtitle("Marital Status")
    
    #guides(colour = guide_legend(override.aes = list(colour = NA)))+
    #guides(colour = guide_legend(override.aes = list(colour = cbbPalette[1:length(unique(pdf$Municipal))])))
    p
    
    
  })
  
  
  
  output$fplot <- renderPlot({
    
    #make one for males and one for females
    
    # 
    pdf <- plot_mar_df()
    pdff <- subset(pdf, pdf$Gender=="Female")
    
    ap=0.5
    sz=1
    
    p=ggplot(pdff, aes(x=Year, y=Var, colour=Region))+
      geom_errorbarh(aes(xmax = Year + 2, xmin = Year - 2, height = 0,colour=Region),alpha=ap/2, size=sz/2)+
      geom_errorbar(aes(ymin = Var-Error, ymax = Var+Error,colour=Region),alpha=ap,size=sz, width=0.125)+
      ylab("Percent of Female Marital Status (%)")+
      scale_color_manual(values=cbbPalette, guide="legend")+
      scale_x_continuous(breaks=c(2006, 2008, 2010, 2012, 2014))+
      geom_point(aes(colour=Region),size=4,alpha=1)+
      geom_line(aes(colour=Region),size=2,alpha=1)+
      theme_bw() + 
      theme(plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank() )+
      theme(panel.border= element_blank())+
      theme(axis.line.x = element_line(color="black", size = 0.5),
            axis.line.y = element_line(color="black", size = 0.5))+
      theme(axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14))+
      theme(legend.title=element_text(size=16),
            legend.text=element_text(size=14))+
      ggtitle("Female Marital Status")
    
    #guides(colour = guide_legend(override.aes = list(colour = NA)))+
    #guides(colour = guide_legend(override.aes = list(colour = cbbPalette[1:length(unique(pdf$Municipal))])))
    p
    
    
  })
  
  output$mplot <- renderPlot({
    
    #make one for males and one for females
    
    # 
    pdf <- plot_mar_df()
    pdfm <- subset(pdf, pdf$Gender=="Male")
    
    ap=0.5
    sz=1
    
    p=ggplot(pdfm, aes(x=Year, y=Var, colour=Region))+
      geom_errorbarh(aes(xmax = Year + 2, xmin = Year - 2, height = 0,colour=Region),alpha=ap/2, size=sz/2)+
      geom_errorbar(aes(ymin = Var-Error, ymax = Var+Error,colour=Region),alpha=ap,size=sz, width=0.125)+
      ylab("Percent of Male Marital Status (%)")+
      scale_color_manual(values=cbbPalette, guide="legend")+
      scale_x_continuous(breaks=c(2006, 2008, 2010, 2012, 2014))+
      geom_point(aes(colour=Region),size=4,alpha=1)+
      geom_line(aes(colour=Region),size=2,alpha=1)+
      theme_bw() + 
      theme(plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank() )+
      theme(panel.border= element_blank())+
      theme(axis.line.x = element_line(color="black", size = 0.5),
            axis.line.y = element_line(color="black", size = 0.5))+
      theme(axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14))+
      theme(legend.title=element_text(size=16),
            legend.text=element_text(size=14))+
      ggtitle("Male Marital Status")
    
    #guides(colour = guide_legend(override.aes = list(colour = NA)))+
    #guides(colour = guide_legend(override.aes = list(colour = cbbPalette[1:length(unique(pdf$Municipal))])))
    p
    
    
  })
  
  mar_map_df <- reactive({
    ## Filter the data by the chosen Five Year Range 
    mar_map_df <- mar_data %>%
      filter(Five_Year_Range == input$map_year) %>%
      select(1:4, Gender, Five_Year_Range, Population, Never_Married_pct, Married_pct,
             Separated_pct, Widowed_pct, Divorced_pct) %>%
      arrange(Region, Gender)
    ## Output reactive dataframe
    mar_map_df    
  })
  
  
  ## set map colors
  map_dat <- reactive({
    
    # browser()
    
    ## Browser command - Stops the app right when it's about to break
    ## make reactive dataframe into regular dataframe
    mar_map_df <- mar_map_df()
    
    ## take US, MA, and counties out of map_dat
    map_dat <- mar_map_df %>%
      filter(!is.na(Municipal), Gender == input$map_gender)
    
    ## for single year maps...
    if(input$var == "Married_pct"){
      
      ## subset the data by the var selected
      #      marmap_dat <- select(map_dat, Municipal, County, State, Region, Gender, Five_Year_Range, Married_pct)
      marmap_dat <- select(map_dat, Municipal, County, State, Region, Gender, Five_Year_Range, Population, Married_pct)
      
      ## assign colors to each entry in the data frame
      
      color <- as.integer(cut2(marmap_dat[,input$var],cuts=marcuts))
      marmap_dat <- cbind.data.frame(marmap_dat, color)
      marmap_dat$color <- ifelse(is.na(marmap_dat$color), length(map_colors), 
                                 marmap_dat$color)
      marmap_dat$opacity <- 0.7
      
      ## find missing counties in data subset and assign NAs to all values
      missing_munis <- setdiff(leftover_munis_map, marmap_dat$Region)
      missing_df <- data.frame(Municipal = missing_munis, County = NA, State = "MA", 
                               Region = missing_munis, Gender = input$map_gender, 
                               Five_Year_Range = input$map_year, Population = NA,
                               Married_pct = NA, color=length(map_colors), opacity = 0)
      # combine data subset with missing counties data
      marmap_dat <- rbind.data.frame(marmap_dat, missing_df)
      marmap_dat$color <- map_colors[marmap_dat$color]
      return(marmap_dat)
      
    }
    
    if(input$var == "Never_Married_pct"){
      
      ## subset the data by the var selected
      nevmap_dat <- select(map_dat, Municipal, County, State, Region, Gender, Five_Year_Range, Population, Never_Married_pct)
      
      ## assign colors to each entry in the data frame
      
      color <- as.integer(cut2(nevmap_dat[,input$var],cuts=nevcuts))
      nevmap_dat <- cbind.data.frame(nevmap_dat, color)
      nevmap_dat$color <- ifelse(is.na(nevmap_dat$color), length(map_colors), 
                                 nevmap_dat$color)
      nevmap_dat$opacity <- 0.7
      
      ## find missing counties in data subset and assign NAs to all values
      missing_munis <- setdiff(leftover_munis_map, nevmap_dat$Region)
      missing_df <- data.frame(Municipal = missing_munis, County = NA, State = "MA", 
                               Region = missing_munis, Gender = input$map_gender, 
                               Five_Year_Range = input$map_year, Population = NA, Never_Married_pct = NA,
                               color=length(map_colors), opacity = 0)
      # combine data subset with missing counties data
      nevmap_dat <- rbind.data.frame(nevmap_dat, missing_df)
      nevmap_dat$color <- map_colors[nevmap_dat$color]
      return(nevmap_dat)
      
    }
    
    if(input$var == "Separated_pct"){
      
      ## subset the data by the var selected
      sepmap_dat <- select(map_dat, Municipal, County, State, Region, Gender, Five_Year_Range, Population, Separated_pct)
      
      ## assign colors to each entry in the data frame
      
      color <- as.integer(cut2(sepmap_dat[,input$var],cuts=sepcuts))
      sepmap_dat <- cbind.data.frame(sepmap_dat, color)
      sepmap_dat$color <- ifelse(is.na(sepmap_dat$color), length(map_colors), 
                                 sepmap_dat$color)
      sepmap_dat$opacity <- 0.7
      
      ## find missing counties in data subset and assign NAs to all values
      missing_munis <- setdiff(leftover_munis_map, sepmap_dat$Region)
      missing_df <- data.frame(Municipal = missing_munis, County = NA, State = "MA", 
                               Region = missing_munis, Gender = input$map_gender, 
                               Five_Year_Range = input$map_year, Population = NA, Separated_pct = NA, color=length(map_colors), opacity = 0)
      # combine data subset with missing counties data
      sepmap_dat <- rbind.data.frame(sepmap_dat, missing_df)
      sepmap_dat$color <- map_colors[sepmap_dat$color]
      return(sepmap_dat)
    }
    
    if(input$var == "Widowed_pct"){
      
      ## subset the data by the year selected
      widmap_dat <- select(map_dat,  Municipal, County, State, Region, Gender, Five_Year_Range, Population, Widowed_pct)
      
      ## assign colors to each entry in the data frame
      
      color <- as.integer(cut2(widmap_dat[,input$var],cuts=widcuts))
      widmap_dat <- cbind.data.frame(widmap_dat, color)
      widmap_dat$color <- ifelse(is.na(widmap_dat$color), length(map_colors), 
                                 widmap_dat$color)
      widmap_dat$opacity <- 0.7
      
      ## find missing counties in data subset and assign NAs to all values
      missing_munis <- setdiff(leftover_munis_map, widmap_dat$Region)
      missing_df <- data.frame(Municipal = missing_munis, County = NA, State = "MA", 
                               Region = missing_munis, Gender = input$map_gender, 
                               Five_Year_Range = input$map_year, Population = NA, Widowed_pct = NA, 
                               color=length(map_colors), opacity = 0)
      # combine data subset with missing counties data
      widmap_dat <- rbind.data.frame(widmap_dat, missing_df)
      widmap_dat$color <- map_colors[widmap_dat$color]
      return(widmap_dat)
    }
    
    if(input$var == "Divorced_pct"){
      
      ## subset the data by the year selected
      divmap_dat <- select(map_dat, Municipal, County, State, Region, Gender, Five_Year_Range, Population, Divorced_pct)
      
      ## assign colors to each entry in the data frame
      
      color <- as.integer(cut2(divmap_dat[,input$var],cuts=divcuts))
      divmap_dat <- cbind.data.frame(divmap_dat, color)
      divmap_dat$color <- ifelse(is.na(divmap_dat$color), length(map_colors), 
                                 divmap_dat$color)
      divmap_dat$opacity <- 0.7
      
      ## find missing counties in data subset and assign NAs to all values
      missing_munis <- setdiff(leftover_munis_map, divmap_dat$Region)
      missing_df <- data.frame(Municipal = missing_munis, County = NA, State = "MA", 
                               Region = missing_munis, Gender = input$map_gender, 
                               Five_Year_Range = input$map_year, Population = NA, 
                               Divorced_pct = NA, color=length(map_colors), opacity = 0)
      # combine data subset with missing counties data
      divmap_dat <- rbind.data.frame(divmap_dat, missing_df)
      divmap_dat$color <- map_colors[divmap_dat$color]
      return(divmap_dat)
    }
  })
  
  #     ## assign colors to each entry in the data frame
  #     color <- as.integer(cut2(map_dat[,input$var],cuts=cuts))
  #     map_dat <- cbind.data.frame(map_dat, color)
  #     map_dat$color <- ifelse(is.na(map_dat$color), length(map_colors), 
  #                             map_dat$color)
  #     map_dat$opacity <- 0.7
  #     
  #     ## find missing counties in data subset and assign NAs to all values
  #     missing_munis <- setdiff(leftover_munis_map, map_dat$Region)
  #     missing_df <- data.frame(Municipal = missing_munis, County = NA, State = "MA", 
  #                              Region = missing_munis, Gender = input$map_gender, 
  #                              Five_Year_Range = input$year, Population = NA, Never_Married_pct = NA,
  #                              Married_pct = NA, Separated_pct = NA, Widowed_pct = NA, 
  #                              Divorced_pct = NA, color=length(map_colors), opacity = 0)
  #     
  #     # combine data subset with missing counties data
  #     map_dat <- rbind.data.frame(map_dat, missing_df)
  #     map_dat$color <- map_colors[map_dat$color]
  #     return(map_dat)
  #  }  )
  
  values <- reactiveValues(selectedFeature=NULL, highlight=c())
  
  #############################################
  # observe({
  #   values$highlight <- input$map_shape_mouseover$id
  #   #browser()
  # })
  
  # # Dynamically render the box in the upper-right
  # output$countyInfo <- renderUI({
  #   
  #   if (is.null(values$highlight)) {
  #     return(tags$div("Hover over a county"))
  #   } else {
  #     # Get a properly formatted state name
  #     countyName <- names(MAcounties)[values$highlight]
  #     return(tags$div(
  #       tags$strong(countyName),
  #       tags$div(density[countyName], HTML("people/m<sup>2</sup>"))
  #     ))
  #   }
  # })
  
  # lastHighlighted <- c()
  # # When values$highlight changes, unhighlight the old state (if any) and
  # # highlight the new state
  # observe({
  # #   if (length(lastHighlighted) > 0)
  # #     drawStates(getStateName(lastHighlighted), FALSE)
  #   lastHighlighted <<- values$highlight
  #   
  #   if (is.null(values$highlight))
  #     return()
  #   
  #   isolate({
  #     drawStates(getStateName(values$highlight), TRUE)
  #   })
  # })
  
  ###########################################
  
  ## draw leaflet map
  map <- createLeafletMap(session, "map")
  
  
  ## the functions within observe are called when any of the inputs are called
  
  ## Does nothing until called (done with action button)
  observe({
    input$action
    
    ## load in relevant map data
    map_dat <- map_dat()
    
    ## All functions which are isolated, will not run until the above observe function is activated
    isolate({
      ## Duplicate MAmap to x
      x <- MA_map_muni
      
      ## for each county in the map, attach the Crude Rate and colors associated
      for(i in 1:length(x$features)){
        ## Each feature is a county
        x$features[[i]]$properties[input$var] <- 
          map_dat[match(x$features[[i]]$properties$NAMELSAD10, map_dat$Region), input$var]
        ## Style properties
        x$features[[i]]$properties$style <- list(
          fill=TRUE, 
          ## Fill color has to be equal to the map_dat color and is matched by county
          fillColor = map_dat$color[match(x$features[[i]]$properties$NAMELSAD10, map_dat$Region)], 
          ## "#000000" = Black, "#999999"=Grey, 
          weight=1, stroke=TRUE, 
          opacity=map_dat$opacity[match(x$features[[i]]$properties$NAMELSAD10, map_dat$Region)], 
          color="#000000", 
          fillOpacity=map_dat$opacity[match(x$features[[i]]$properties$NAMELSAD10, map_dat$Region)])
      }
      
      map$addGeoJSON(x) # draw map
    })
  })
  
  observe({
    ## EVT = Mouse Click
    evt <- input$map_click
    if(is.null(evt))
      return()
    
    isolate({
      values$selectedFeature <- NULL
    })
  })
  
  observe({
    evt <- input$map_geojson_click
    if(is.null(evt))
      return()
    map_dat <- map_dat()
    isolate({
      values$selectedFeature <- evt$properties
      region <- evt$properties$NAMELSAD10
      values$selectedFeature[input$var] <- map_dat[match(region, map_dat$Region), input$var]
    })
  })
  
  
  
  observeEvent(input$lmap_cas, {
    
    leafletProxy("map")  %>% 
      addMarkers(lng=~Lon, lat=~Lat, icon=star,data=MAcasinos, group="MAcasinos",
                 popup = ~paste(as.character(Name))) %>%
      addMarkers(lng=~Lon, lat=~Lat, data=casinosCLOSED, group="casinosCLOSED",
                 icon=gc1,
                 popup = ~paste(as.character(Name))) %>%
      addMarkers(lng=~Lon, lat=~Lat, data=casinosOPEN, group="casinosOPEN",
                 icon=gc2,
                 popup = ~paste(as.character(Name)))
    
    if(input$lmap_cas) {leafletProxy("map")  %>% 
        showGroup('MAcasinos') %>%
        showGroup('casinosCLOSED') %>%
        showGroup('casinosOPEN')}
    else {leafletProxy("map")   %>% 
        hideGroup('MAcasinos') %>%
        hideGroup('casinosCLOSED') %>%
        hideGroup('casinosOPEN')}
  })
  
  
  ##  This function is what creates info box
  output$details <- renderText({
    
    ## Before a county is clicked, display a message
    if(is.null(values$selectedFeature)){
      return(as.character(tags$div(
        tags$div(
          h4("Click on a town or city"))
      )))
    }
    
    muni_name <- values$selectedFeature$NAMELSAD10
    muni_value <- values$selectedFeature[input$var]
    var_select <- gsub("_", " ", input$var)
    var_select <- gsub("Pct", "", var_select)
    
    ## If clicked county has no crude rate, display a message
    if(is.null(values$selectedFeature[input$var])){
      return(as.character(tags$div(
        tags$h5("% ", input$map_gender, var_select, " in ", muni_name, "is not available for this timespan"))))
    }
    
    ## For a single year when county is clicked, display a message
    as.character(tags$div(
      tags$h4("% ", input$map_gender, var_select, " in ", muni_name, " for ", input$map_year),
      tags$h5(muni_value, "%")
    ))
  })
  #legend
  output$legend1 <- renderPlot({  
    paint.brush = colorRampPalette(colors=c("white", "deeppink"))
    cols <- paint.brush(length(map_colors)-1)
    if(input$var =='Married_pct'){
      leg_dat<- data_frame(y = seq(marmin.val, marmax.val,length.out=(length(map_colors)-1)), x = 1, col = cols)
      
      q<- ggplot(data = leg_dat) +
        geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
        scale_y_continuous(limits = c(marmin.val, marmax.val), breaks = round(seq(marmin.val, marmax.val, length.out = 5),1)) +
        scale_fill_manual(values = leg_dat$col) + theme_bw() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_text(size = 12),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks.x = element_blank(),
              panel.border = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank())
    }
    else if(input$var == 'Never_Married_pct'){
      leg_dat<- data_frame(y = seq(nevmin.val, nevmax.val,length.out=(length(map_colors)-1)), x = 1, col = cols)
      
      q<- ggplot(data = leg_dat) +
        geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
        scale_y_continuous(limits = c(nevmin.val, nevmax.val), breaks = round(seq(nevmin.val, nevmax.val, length.out = 5),1)) +
        scale_fill_manual(values = leg_dat$col) + theme_bw() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_text(size = 12),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks.x = element_blank(),
              panel.border = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank())
    }
    else if(input$var=='Separated_pct'){
      leg_dat<- data_frame(y = seq(sepmin.val, sepmax.val,length.out=(length(map_colors)-1)), x = 1, col = cols)
      
      q<- ggplot(data = leg_dat) +
        geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
        scale_y_continuous(limits = c(sepmin.val, sepmax.val), breaks = round(seq(sepmin.val, sepmax.val, length.out = 5),1)) +
        scale_fill_manual(values = leg_dat$col) + theme_bw() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_text(size = 12),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks.x = element_blank(),
              panel.border = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank())
    }
    else if(input$var == 'Widowed_pct'){
      leg_dat<- data_frame(y = seq(widmin.val, widmax.val,length.out=(length(map_colors)-1)), x = 1, col = cols)
      
      q<- ggplot(data = leg_dat) +
        geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
        scale_y_continuous(limits = c(widmin.val, widmax.val), breaks = round(seq(widmin.val, widmax.val, length.out = 5),1)) +
        scale_fill_manual(values = leg_dat$col) + theme_bw() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_text(size = 12),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks.x = element_blank(),
              panel.border = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank())
    }
    else {
      leg_dat<- data_frame(y = seq(divmin.val, divmax.val,length.out=(length(map_colors)-1)), x = 1, col = cols)
      
      q<- ggplot(data = leg_dat) +
        geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
        scale_y_continuous(limits = c(divmin.val, divmax.val), breaks = round(seq(divmin.val, divmax.val, length.out = 5),1)) +
        scale_fill_manual(values = leg_dat$col) + theme_bw() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_text(size = 12),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks.x = element_blank(),
              panel.border = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank())
    }
    
    return(q)
    
  })
  output$text1<-renderText({
    var_s <- gsub("_pct", "", input$var)
    return(as.character(
      var_s
    ))
  })
})
