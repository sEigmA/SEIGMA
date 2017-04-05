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
  
  munis_p <- reactive({

    munis_p2 <- input$plot_muni
    
   #MA
      if(input$plotMA_mean==T && any(grepl(x=munis_p2, pattern = "MA"))==F){
        return(c("MA", munis_p2[!(munis_p2 =="MA")])) ## US only ## MA only
      }else if(input$plotMA_mean==T && any(grepl(x=munis_p2, pattern = "MA"))==T){
        return(c("MA", munis_p2[!(munis_p2 =="MA")])) ## US only ## MA only
      }
      else if(input$plotMA_mean==F && any(grepl(x=munis_p2, pattern = "MA"))==T){
        return(munis_p2[!(munis_p2 =="MA")]) ## remove MA
      } else if(input$plotMA_mean==F && any(grepl(x=munis_p2, pattern = "MA"))==F){
      return(munis_p2[!(munis_p2 =="MA")]) ## remove MA
      }
  })
  
  munis_pfinal <- reactive({
    munis_p3 <- munis_p()
    #AMERICA FWURST
    if(input$plotUS_mean==T && any(grepl(x=munis_p3, pattern = "United States"))==F){
      return(c("United States", munis_p3[!(munis_p3 =="United States")])) ##  United States
    }else if(input$plotUS_mean==T && any(grepl(x=munis_p3, pattern = "United States"))==T){
      return(c("United States", munis_p3[!(munis_p3 =="United States")])) ## US  United States
    }
    else if(input$plotUS_mean==F && any(grepl(x=munis_p3, pattern = "United States"))==T){
      return(munis_p3[!(munis_p3 =="United States")]) ## remove United States
    } else if(input$plotUS_mean==F && any(grepl(x=munis_p3, pattern = "United States"))==F){
      return(munis_p3[!(munis_p3 =="United States")]) ## remove  United States
    }
  
    
  })
  
  plot_mar_df <- reactive({
    
    ## Filter the data by the chosen Five Year Range
    
    pvars <- c(input$plotvar, paste(input$plotvar, "error", sep="_"))
    vars <- which(names(mar_data) %in% pvars)
    
    selmun <- munis_pfinal()
    plot_mar_df <- mar_data %>%
      filter(Region %in% selmun) %>%
      select(c(22,4,5,vars))
    # %>%
    #   spread(Municipal, Median.Rent)
    # 
    plot_mar_df$Year <- as.integer(sapply(strsplit(as.character(plot_mar_df$Five_Year_Range), split="-"), FUN=function(x){x[1]}))+2
    names(plot_mar_df)[c(4,5)] <- c("Var", "Error")
    
    ## Output reactive dataframe, sorted like selected munis
    #order=unlist(lapply(match(munis_p, plot_mar_df$Region), FUN=function(x){x+0:((nrow(plot_mar_df))/(length(munis_p))-1)}))
    return(plot_mar_df[order(match(plot_mar_df$Region, selmun)),])
    
  })
  
  # 
  # output$ordermunis <- renderPrint({
  #   
  # o=plot_mar_df()
  # unique(o$Region)
  #   
  # })
  # 
  # output$ordermunis2 <- renderPrint({
  #   
  #   unlist(munis_p())
  #   
  # })
  # 
  output$fmplot <- renderPlot({
    
    #make one for males and one for females
    
    # 
    pdf <- plot_mar_df()
    row.names(pdf) <- 1:nrow(pdf)
    pdf$Region <- factor(pdf$Region, levels = pdf$Region,ordered = TRUE)
    # fuck with the levels statement
    ##

    ap=0.5
    sz=1
    
    p=ggplot(pdf, aes(x=Year, y=Var, colour=Region))+
      geom_errorbarh(aes(xmax = Year + 2, xmin = Year - 2, height = 0,colour=Region),alpha=ap/2, size=sz/2)+
      geom_errorbar(aes(ymin = Var-Error, ymax = Var+Error,colour=Region),alpha=ap,size=sz, width=0.125)+
      ylab(paste("Percent of",gsub("_", " ", gsub("_pct", "", input$plotvar)),"Population (%)", sep=" "))+
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
      ggtitle(paste("Percent of",gsub("_", " ", gsub("_pct", "", input$plotvar)),"Population (%)", sep=" "))
      
    
    #guides(colour = guide_legend(override.aes = list(colour = NA)))+
    #guides(colour = guide_legend(override.aes = list(colour = cbbPalette[1:length(unique(pdf$Municipal))])))
    p
    
    
  })
  
  
  
  output$fplot <- renderPlot({
    
    #make one for males and one for females
    
    # 
    pdf <- plot_mar_df()
    #row.names(pdf) <- 1:nrow(pdf)
    pdff <- subset(pdf, pdf$Gender=="Female")
    pdff$Region <- factor(pdff$Region, levels = pdff$Region, ordered = TRUE)
    
    
    ap=0.5
    sz=1
    
    p=ggplot(pdff, aes(x=Year, y=Var, colour=Region))+
      geom_errorbarh(aes(xmax = Year + 2, xmin = Year - 2, height = 0,colour=Region),alpha=ap/2, size=sz/2)+
      geom_errorbar(aes(ymin = Var-Error, ymax = Var+Error,colour=Region),alpha=ap,size=sz, width=0.125)+
      ylab(paste("Percent of",gsub("_", " ", gsub("_pct", "", input$plotvar)),"Females (%)", sep=" "))+
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
      ggtitle(paste("Percent of",gsub("_", " ", gsub("_pct", "", input$plotvar)),"Females (%)", sep=" "))
      
    
    #guides(colour = guide_legend(override.aes = list(colour = NA)))+
    #guides(colour = guide_legend(override.aes = list(colour = cbbPalette[1:length(unique(pdf$Municipal))])))
    p
    
    
  })
  
  output$mplot <- renderPlot({
    
    #make one for males and one for females
    
    # 
    pdf <- plot_mar_df()
    #row.names(pdf) <- 1:nrow(pdf)
    pdfm <- subset(pdf, pdf$Gender=="Male")
    pdfm$Region <- factor(pdfm$Region, levels = pdfm$Region,ordered = TRUE)
    
    
    ap=0.5
    sz=1
    
    p=ggplot(pdfm, aes(x=Year, y=Var, colour=Region))+
      geom_errorbarh(aes(xmax = Year + 2, xmin = Year - 2, height = 0,colour=Region),alpha=ap/2, size=sz/2)+
      geom_errorbar(aes(ymin = Var-Error, ymax = Var+Error,colour=Region),alpha=ap,size=sz, width=0.125)+
      ylab(paste("Percent of",gsub("_", " ", gsub("_pct", "", input$plotvar)),"Males (%)", sep=" "))+
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
      ggtitle(paste("Percent of",gsub("_", " ", gsub("_pct", "", input$plotvar)),"Males (%)", sep=" "))
    
    #guides(colour = guide_legend(override.aes = list(colour = NA)))+
    #guides(colour = guide_legend(override.aes = list(colour = cbbPalette[1:length(unique(pdf$Municipal))])))
    p
    
    
  })
  
  mar_map_df <- reactive({
    ## Filter the data by the chosen Five Year Range 
    mar_map_df <- mar_data %>%
      filter(Five_Year_Range == input$map_year) %>%
      select(1:4, Gender, Five_Year_Range, Population, 
             Never_Married_pct,Never_Married_pct_error, Married_pct,Married_pct_error,
             Separated_pct,Separated_pct_error,  
             Widowed_pct, Widowed_pct_error, 
             Divorced_pct, Divorced_pct_error) %>%
      arrange(Region, Gender)
    ## Output reactive dataframe
    mar_map_df    
  })
  
  
  ## set map colors
  map_dat2 <- reactive({
    
    op <- 0.8
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
      marmap_dat <- select(map_dat, Municipal, County, State, Region, Gender, Five_Year_Range, Population, Married_pct, Married_pct_error)
      
      ## assign colors to each entry in the data frame
      
      color <- as.integer(cut2(marmap_dat[,input$var],cuts=marcuts))
      marmap_dat <- cbind.data.frame(marmap_dat, color)
      marmap_dat$color <- ifelse(is.na(marmap_dat$color), length(map_colors), 
                                 marmap_dat$color)
      marmap_dat$opacity <- op
      
      ## find missing counties in data subset and assign NAs to all values
      missing_munis <- setdiff(leftover_munis_map, marmap_dat$Region)
      missing_df <- data.frame(Municipal = missing_munis, County = NA, State = "MA", 
                               Region = missing_munis, Gender = input$map_gender, 
                               Five_Year_Range = input$map_year, Population = NA,
                               Married_pct = NA, Married_pct_error = NA, color=length(map_colors), opacity = 0)
      # combine data subset with missing counties data
      marmap_dat <- rbind.data.frame(marmap_dat, missing_df)
      marmap_dat$color <- map_colors[marmap_dat$color]
      return(marmap_dat)
      
    }
    
    if(input$var == "Never_Married_pct"){
      
      ## subset the data by the var selected
      nevmap_dat <- select(map_dat, Municipal, County, State, Region, Gender, Five_Year_Range, Population, Never_Married_pct,Never_Married_pct_error)
      
      ## assign colors to each entry in the data frame
      
      color <- as.integer(cut2(nevmap_dat[,input$var],cuts=nevcuts))
      nevmap_dat <- cbind.data.frame(nevmap_dat, color)
      nevmap_dat$color <- ifelse(is.na(nevmap_dat$color), length(map_colors), 
                                 nevmap_dat$color)
      nevmap_dat$opacity <- op
      
      ## find missing counties in data subset and assign NAs to all values
      missing_munis <- setdiff(leftover_munis_map, nevmap_dat$Region)
      missing_df <- data.frame(Municipal = missing_munis, County = NA, State = "MA", 
                               Region = missing_munis, Gender = input$map_gender, 
                               Five_Year_Range = input$map_year, Population = NA, Never_Married_pct = NA, Never_Married_pct_error=NA,
                               color=length(map_colors), opacity = 0)
      # combine data subset with missing counties data
      nevmap_dat <- rbind.data.frame(nevmap_dat, missing_df)
      nevmap_dat$color <- map_colors[nevmap_dat$color]
      return(nevmap_dat)
      
    }
    
    if(input$var == "Separated_pct"){
      
      ## subset the data by the var selected
      sepmap_dat <- select(map_dat, Municipal, County, State, Region, Gender, Five_Year_Range, Population, Separated_pct,Separated_pct_error)
      
      ## assign colors to each entry in the data frame
      
      color <- as.integer(cut2(sepmap_dat[,input$var],cuts=sepcuts))
      sepmap_dat <- cbind.data.frame(sepmap_dat, color)
      sepmap_dat$color <- ifelse(is.na(sepmap_dat$color), length(map_colors), 
                                 sepmap_dat$color)
      sepmap_dat$opacity <- op
      
      ## find missing counties in data subset and assign NAs to all values
      missing_munis <- setdiff(leftover_munis_map, sepmap_dat$Region)
      missing_df <- data.frame(Municipal = missing_munis, County = NA, State = "MA", 
                               Region = missing_munis, Gender = input$map_gender, 
                               Five_Year_Range = input$map_year, Population = NA, Separated_pct = NA,Separated_pct_error=NA, color=length(map_colors), opacity = 0)
      # combine data subset with missing counties data
      sepmap_dat <- rbind.data.frame(sepmap_dat, missing_df)
      sepmap_dat$color <- map_colors[sepmap_dat$color]
      return(sepmap_dat)
    }
    
    if(input$var == "Widowed_pct"){
      
      ## subset the data by the year selected
      widmap_dat <- select(map_dat,  Municipal, County, State, Region, Gender, Five_Year_Range, Population, Widowed_pct,Widowed_pct_error)
      
      ## assign colors to each entry in the data frame
      
      color <- as.integer(cut2(widmap_dat[,input$var],cuts=widcuts))
      widmap_dat <- cbind.data.frame(widmap_dat, color)
      widmap_dat$color <- ifelse(is.na(widmap_dat$color), length(map_colors), 
                                 widmap_dat$color)
      widmap_dat$opacity <- op
      
      ## find missing counties in data subset and assign NAs to all values
      missing_munis <- setdiff(leftover_munis_map, widmap_dat$Region)
      missing_df <- data.frame(Municipal = missing_munis, County = NA, State = "MA", 
                               Region = missing_munis, Gender = input$map_gender, 
                               Five_Year_Range = input$map_year, Population = NA, Widowed_pct = NA, Widowed_pct_error=NA,
                               color=length(map_colors), opacity = 0)
      # combine data subset with missing counties data
      widmap_dat <- rbind.data.frame(widmap_dat, missing_df)
      widmap_dat$color <- map_colors[widmap_dat$color]
      return(widmap_dat)
    }
    
    if(input$var == "Divorced_pct"){
      
      ## subset the data by the year selected
      divmap_dat <- select(map_dat, Municipal, County, State, Region, Gender, Five_Year_Range, Population, Divorced_pct,Divorced_pct_error)
      
      ## assign colors to each entry in the data frame
      
      color <- as.integer(cut2(divmap_dat[,input$var],cuts=divcuts))
      divmap_dat <- cbind.data.frame(divmap_dat, color)
      divmap_dat$color <- ifelse(is.na(divmap_dat$color), length(map_colors), 
                                 divmap_dat$color)
      divmap_dat$opacity <- op
      
      ## find missing counties in data subset and assign NAs to all values
      missing_munis <- setdiff(leftover_munis_map, divmap_dat$Region)
      missing_df <- data.frame(Municipal = missing_munis, County = NA, State = "MA", 
                               Region = missing_munis, Gender = input$map_gender, 
                               Five_Year_Range = input$map_year, Population = NA, 
                               Divorced_pct = NA,Divorced_pct_error = NA, color=length(map_colors), opacity = 0)
      # combine data subset with missing counties data
      divmap_dat <- rbind.data.frame(divmap_dat, missing_df)
      divmap_dat$color <- map_colors[divmap_dat$color]
      return(divmap_dat)
    }
  })
  
  # map_dat2<- reactive({
  #   map_dat <- map_dat()
  #     ## assign colors to each entry in the data frame
  #     color <- as.integer(cut2(map_dat[,input$var],cuts=cuts))
  #     map_dat <- cbind.data.frame(map_dat, color)
  #     map_dat$colori <- ifelse(is.na(map_dat$colori), length(map_colors),
  #                             map_dat$colori)
  #     map_dat$opacity <- 0.7
  # 
  #     ## find missing counties in data subset and assign NAs to all values
  #     missing_munis <- setdiff(leftover_munis_map, map_dat$Region)
  #     if(length(missing_munis)>0){
  #     missing_df <- data.frame(Municipal = missing_munis, County = NA, State = "MA",
  #                              Region = missing_munis, Gender = input$map_gender,
  #                              Five_Year_Range = input$map_year, Population = NA, Never_Married_pct = NA,
  #                              Married_pct = NA, Separated_pct = NA, Widowed_pct = NA,
  #                              Divorced_pct = NA, colori=length(map_colors), opacity = 0)
  # 
  #     # combine data subset with missing counties data
  #     map_dat <- rbind.data.frame(map_dat, missing_df)
  #     }
  #     map_dat$color <- map_colors[map_dat$colori]
  #     return(map_dat)
  #  })
  
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
  
  #line 632-735
  
  ## draw leaflet map
  output$map <- renderLeaflet({
  leaflet() %>% addTiles() %>% fitBounds(0,0,11,11) %>%  clearControls()
      # addMarkers(lng=MAcasinos$Lon, lat=MAcasinos$Lat,  icon=star, group="MAcasinos",
      # popup = paste(as.character(MAcasinos$Name))) %>% 
      
    })
  observeEvent(input$action,{
    
    leafletProxy("map", data=MAcasinos) %>% clearShapes() %>%  
     addMarkers(lng=~Lon, lat=~Lat,  icon=star, group="MAcasinos",
     popup = paste(as.character(~Name))) %>% clearControls()
    
  })
  
  



  ## the functions within observe are called when any of the inputs are called

  ## Does nothing until called (done with action button)
  # observe({
  #    
  # 
  #   ## load in relevant map data
  #   map_dat <- map_dat2()
  # 
  #   ## All functions which are isolated, will not run until the above observe function is activated
  #   isolate({
  #     ## Duplicate MAmap to x
  #     x <- MA_map_muni
  # 
  #     ## for each county in the map, attach the Crude Rate and colors associated
  #     for(i in 1:length(x$features)){
  #       ## Each feature is a county
  #       x$features[[i]]$properties[input$var] <-
  #         map_dat[match(x$features[[i]]$properties$NAMELSAD10, map_dat$Region), input$var]
  #       ## Style properties
  #       x$features[[i]]$properties$style <- list(
  #         fill=TRUE,
  #         ## Fill color has to be equal to the map_dat color and is matched by county
  #         fillColor = map_dat$color[match(x$features[[i]]$properties$NAMELSAD10, map_dat$Region)],
  #         ## "#000000" = Black, "#999999"=Grey,
  #         weight=1, stroke=TRUE,
  #         opacity=map_dat$opacity[match(x$features[[i]]$properties$NAMELSAD10, map_dat$Region)],
  #         color="#000000",
  #         fillOpacity=map_dat$opacity[match(x$features[[i]]$properties$NAMELSAD10, map_dat$Region)])
  #     }
  #     
  #     leafletProxy("map", data=mar_data) %>% clearShapes() %>% addGeoJSON(x)  
  #       # addMarkers(lng=MAcasinos$Lon, lat=MAcasinos$Lat,  icon=star, group="MAcasinos",
  #       # popup = paste(as.character(MAcasinos$Name))) %>% clearControls()
  # 
  #     
  #     # draw map
  # 
  # 
  #     # add_casinos_to_map(df=MAcasinos, icon=star, groupname="MAcasinos")
  #     # add_casinos_to_map(df=casinosCLOSED, icon=gc1, groupname="casinosCLOSED")
  #     # add_casinos_to_map(df=casinosOPEN, icon=gc1, groupname="casinosOPEN")
  #     # isolate({
  #     #   if(input$lmap_cas) {
  #     #     show.casinos("MAcasinos")
  #     #     show.casinos("casinosCLOSED")
  #     #     show.casinos("casinosOPEN")
  #     #   }
  #     #   else {
  #     #     hide.casinos("MAcasinos")
  #     #     hide.casinos("casinosCLOSED")
  #     #     hide.casinos("casinosOPEN")
  #     #   }
  #     # })
  #     #
  #  })
  #   
  # })

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
      values$selectedFeature[c(input$var,paste(input$var, c("error"), sep="_"))] <- map_dat[match(region, map_dat$Region), c(input$var,paste(input$var, c("error"), sep="_"))]


    })
  })

  # 
  # add_casinos_to_map <- function(df, icon, groupname){
  #   map$addMarkers(
  #     lng=df$Lon, lat=df$Lat, icon=icon, group=groupname,
  #     popup = paste(as.character(df$Name)))
  # }
  # show.casinos <- function(groupname){
  #   map$showGroup(groupname)}
  # hide.casinos <- function(groupname){
  #   map$hideGroup(groupname)}
  # 
  
#   output$maptab <- renderTable({
# map_dat()
#   })




  
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
    muni_value <- prettyNum(values$selectedFeature[input$var], big.mark = ",")
    muni_error <- prettyNum(values$selectedFeature[paste(input$var, c("error"), sep="_")], big.mark = ",")
    
    var_select <- gsub("_", " ", input$var)
    var_select <- gsub("pct", "", var_select)
    
    ## If clicked county has no crude rate, display a message
    if(is.null(values$selectedFeature[input$var])){
      return(as.character(tags$div(
        tags$h5("% ", input$map_gender, var_select, " in ", muni_name, "is not available for this timespan"))))
    }
    
    ## For a single year when county is clicked, display a message
    as.character(tags$div(
      tags$h4("% ", input$map_gender, var_select, " in ", muni_name, " for ", input$map_year),
      tags$h5(muni_value,"+-",muni_error,  "%")
    ))
  })
  
  
  #legend
  whichgender <- reactive({switch(input$map_gender,"Female"=1,  "Male"=2)})
  
  
  output$legend1 <- renderPlot({  
    paint.brush = colorRampPalette(colors=c("white", "deeppink"))
    cols <- paint.brush(length(map_colors)-1)
    if(input$var =='Married_pct'){
      leg_dat<- data_frame(y = seq(marmin.val[whichgender()], marmax.val[whichgender()],length.out=(length(map_colors)-1)), x = 1, col = cols)
      
      q<- ggplot(data = leg_dat) +
        geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
        scale_y_continuous(limits = c(marmin.val[whichgender()], marmax.val[whichgender()]), breaks = round(seq(marmin.val[whichgender()], marmax.val[whichgender()], length.out = 5),1)) +
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
      leg_dat<- data_frame(y = seq(nevmin.val[whichgender()], nevmax.val[whichgender()],length.out=(length(map_colors)-1)), x = 1, col = cols)
      
      q<- ggplot(data = leg_dat) +
        geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
        scale_y_continuous(limits = c(nevmin.val[whichgender()], nevmax.val[whichgender()]), breaks = round(seq(nevmin.val[whichgender()], nevmax.val[whichgender()], length.out = 5),1)) +
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
      leg_dat<- data_frame(y = seq(sepmin.val[whichgender()], sepmax.val[whichgender()],length.out=(length(map_colors)-1)), x = 1, col = cols)
      
      q<- ggplot(data = leg_dat) +
        geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
        scale_y_continuous(limits = c(sepmin.val[whichgender()], sepmax.val[whichgender()]), breaks = round(seq(sepmin.val[whichgender()], sepmax.val[whichgender()], length.out = 5),1)) +
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
      leg_dat<- data_frame(y = seq(widmin.val[whichgender()], widmax.val[whichgender()],length.out=(length(map_colors)-1)), x = 1, col = cols)
      
      q<- ggplot(data = leg_dat) +
        geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
        scale_y_continuous(limits = c(widmin.val[whichgender()], widmax.val[whichgender()]), breaks = round(seq(widmin.val[whichgender()], widmax.val[whichgender()], length.out = 5),1)) +
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
      leg_dat<- data_frame(y = seq(divmin.val[whichgender()], divmax.val[whichgender()],length.out=(length(map_colors)-1)), x = 1, col = cols)
      
      q<- ggplot(data = leg_dat) +
        geom_tile(aes(y = y, fill = reorder(col, y), x = x), show.legend = FALSE) +
        scale_y_continuous(limits = c(divmin.val[whichgender()], divmax.val[whichgender()]), breaks = round(seq(divmin.val[whichgender()], divmax.val[whichgender()], length.out = 5),1)) +
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
      gsub("_"," ", var_s)
    ))
  })
})

