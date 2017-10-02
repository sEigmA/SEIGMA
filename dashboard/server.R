################################
## Title: Dashboard server    ##
## Author: Zhenning Kang      ##
## Date Created:  09/27/2017  ##
## Date Modified: 10/01/2017  ##
################################

shinyServer(function(input, output, session) {
    ## rent_df is a reactive dataframe. Necessary for when summary/plot/map have common input (Multiple Variables). Not in this project
    rent_df <- reactive({
        years <- input$year
        if(is.null(years)==TRUE){years <- paste(c(2005:2011), c(2005:2011)+4, sep="-")}
        
        ## Filter the data by the chosen Five Year Range 
        rent_df <- rent %>%
            filter(Five.Year.Range %in% years) %>%
            select(c(1:5))
        
        ## Output reactive dataframe
        rent_df    
    })
    
    ## Create summary table
    output$summary_rent <- renderDataTable({
        ## Make reactive dataframe into regular dataframe
        
        rent_df <- rent_df()
        
        ## make municipals a vector based on input variable
        if(!is.null(input$muni))
            munis <- input$muni
        ## if none selected, put all municipals in vector
        if(is.null(input$muni))
            munis <- c("USA","Massachusetts")
        
        ## if the user checks the meanUS box or the meanMA box, add those to counties vector
        if(input$US_mean){
            if(input$MA_mean){
                munis <- c("USA", "Massachusetts", munis) ## US and MA  
            } else{
                munis <- c("USA", munis) ## US only
            }
        } else{
            if(input$MA_mean){
                munis <- c("Massachusetts", munis) ## US only ## MA only
            }
        }
        #     browser( )
        ## create a dataframe consisting only of counties in vector
        rent_df <- rent_df %>%
            filter(Municipal %in% munis) %>%
            select(c(2,1,3,4,5))
        
        colnames(rent_df) <- gsub(".", " ", colnames(rent_df), fixed=T)
        colnames(rent_df)[4] <- "Median Rent (2015$)"
        colnames(rent_df)[5] <- "Median Margin of Error (2015$)"
        
        
        #     rent_df[,3] <- prettyNum(rent_df[,3], big.mark=",")
        #     rent_df[,4] <- prettyNum(rent_df[,4], big.mark=",")
        #     
        return(rent_df)
    }, options = list(searching = FALSE, orderClasses = TRUE, scrollX = TRUE)) # there are a bunch of options to edit the appearance of datatables, this removes one of the ugly features
    
    ## create the plot of the data
    
    
    
    munis_p <- reactive({
        
        munis_p2 <- input$muni
        #MA
        if(input$MA_mean==T && any(grepl(x=munis_p2, pattern = "Massachusetts"))==F){
            return(c("Massachusetts", munis_p2[!(munis_p2 =="Massachusetts")])) ## US only ## MA only
        }else if(input$MA_mean==T && any(grepl(x=munis_p2, pattern = "Massachusetts"))==T){
            return(c("Massachusetts", munis_p2[!(munis_p2 =="Massachusetts")])) ## US only ## MA only
        }
        else if(input$MA_mean==F && any(grepl(x=munis_p2, pattern = "Massachusetts"))==T){
            return(munis_p2[!(munis_p2 =="Massachusetts")]) ## remove MA
        } else if(input$MA_mean==F && any(grepl(x=munis_p2, pattern = "Massachusetts"))==F){
            return(munis_p2[!(munis_p2 =="Massachusetts")]) ## remove MA
        }
        
        
    })
    
    munis_pfinal <- reactive({
        munis_p3 <- munis_p()
        #AMERICA FWURST
        if(input$US_mean_p==T && any(grepl(x=munis_p3, pattern = "United States"))==F){
            return(c("United States", munis_p3[!(munis_p3 =="United States")])) ##  United States
        }else if(input$US_mean_p==T && any(grepl(x=munis_p3, pattern = "United States"))==T){
            return(c("United States", munis_p3[!(munis_p3 =="United States")])) ## US  United States
        }
        else if(input$US_mean_p==F && any(grepl(x=munis_p3, pattern = "United States"))==T){
            return(munis_p3[!(munis_p3 =="United States")]) ## remove United States
        } else if(input$US_mean_p==F && any(grepl(x=munis_p3, pattern = "United States"))==F){
            return(munis_p3[!(munis_p3 =="United States")]) ## remove  United States
        }
        
        
    })
    
    plot_rent_df <- reactive({
        rent_for_plot <- rent  
        rent_for_plot$Year <- as.numeric(sapply(strsplit(as.character(rent_for_plot$Five.Year.Range), split="-"), FUN=function(x){x[1]}))+2    
        
        selmun <- munis_pfinal()
        rent_for_plot <- rent_for_plot %>%
            filter(Municipal %in% selmun) %>%
            select(c(1,6,4,5)) 
        names(rent_for_plot)[c(3,4)] <- c("Var", "Error")
        
        ## Output reactive dataframe, sorted like selected munis
        #order=unlist(lapply(match(munis_p, plot_mar_df$Region), FUN=function(x){x+0:((nrow(plot_mar_df))/(length(munis_p))-1)}))
        return(rent_for_plot[order(match(rent_for_plot$Municipal, selmun)),])
        
    })
    
    ## for the Google charts plot
    output$plot <- renderPlot({
        ## make reactive dataframe into regular dataframe
        
        pdf <- plot_rent_df()
        row.names(pdf) <- 1:nrow(pdf)
        pdf$Municipal <- factor(pdf$Municipal, levels = pdf$Municipal,ordered = TRUE)
        ap=0.5
        sz=1
        
        p=ggplot(pdf, aes(x=Year, y=Var, colour=Municipal)) +
            geom_errorbarh(aes(xmax = Year + 2, xmin = Year - 2, height = 0,colour=Municipal),alpha=ap/2, size=sz/2)+
            geom_errorbar(aes(ymin = Var-Error, ymax = Var+Error,colour=Municipal),alpha=ap,size=sz, width=0.125)+
            ylab("Median Rent (2015 $)")+
            scale_x_continuous(breaks=c(2006, 2008, 2010, 2012, 2014))+
            scale_color_manual(values=cbbPalette, guide="legend")+
            geom_point(aes(colour=Municipal),size=4,alpha=1)+
            geom_line(aes(colour=Municipal),size=2,alpha=1)+
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
                  legend.text=element_text(size=14))
        
        #guides(colour = guide_legend(override.aes = list(colour = NA)))+
        #guides(colour = guide_legend(override.aes = list(colour = cbbPalette[1:length(unique(pdf$Municipal))])))
        p
        
    })
    
    
    ## Dem_df is a reactive dataframe. Necessary for when summary/plot/map have common input (Multiple Variables). Not in this project
    
    Dem_df <- reactive({
        Dem_df <- Dem_data       
        ## Output reactive dataframe
        Dem_df
    })
    
    
    ## Create summary table
    output$summary_demo <- renderDataTable({
        ## Make reactive dataframe into regular dataframe
        Dem_df <- Dem_df()
        
        ## make municipals a vector based on input variable
        if(!is.null(input$muni)){
            munis <- input$muni
        }
        ## if none selected, put all municipals in vector
        else {
            munis <- c("United States", "MA")
        }
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
        ##select columns according input$radio
        sel_col_num<-c()
        if (input$demo=="Age") {
            sel_col_num<-c(12:37)
        } else if (input$demo=="Gender") {
            sel_col_num<-c(8:11)
        } else if (input$demo=="Race") {
            sel_col_num<-c(38:49)
        } else {sel_col_num<-c(50:53)}
        
        ## create a dataframe consisting only of counties in vector
        Dem_df <- Dem_df %>%
            filter(Region %in% munis,Five_Year_Range == input$year) %>%
            select(4:7, sel_col_num)
        
        colnames(Dem_df) <- gsub("_", " ", colnames(Dem_df))
        colnames(Dem_df) <- gsub("Pct", "Percentage", colnames(Dem_df))
        
        return(Dem_df)
    }, options=list(searching = FALSE, orderClasses = TRUE, scrollX = TRUE)) # there are a bunch of options to edit the appearance of datatables, this removes one of the ugly features
    ## create the plot of the data
    ## for the Google charts plot
    
    munis_df <- reactive({
        #     browser()
        ## make reactive dataframe into regular dataframe
        Dem_df <- Dem_df()%>%
            filter(Five_Year_Range == input$year) 
        ## find the county of the municipal
        county <- as.character(Dem_df$County[match(input$muni, Dem_df$Municipal)])
        
        ## make counties a vector based on input variable
        munis <- c(input$muni, county, "MA", "United States")
        
        muni_index <- c()
        for(i in 1:length(munis)){
            muni_index[i] <- match(munis[i], Dem_df$Region)
        }
        
        Dem_df$Region <- factor(Dem_df$Region, levels = c(munis, as.character(Dem_df$Region)[-muni_index]))
        
        sel_col_num1<-c()
        if (input$demo=="Age") {
            sel_col_num1<-c(54, 55, 56, 57, 58, 59)
        } else if (input$demo=="Gender") {
            sel_col_num1<-c(8,10)
        } else if (input$demo=="Race") {
            sel_col_num1<-c(38,40,42,44,46,48)
        } else {sel_col_num1<-c(50,52)}
        
        ## create a dataframe consisting only of counties in vector
        munis_df <- Dem_df %>%
            filter(Region %in% munis) %>%
            arrange(Region)%>%
            select(4, sel_col_num1)
        colnames(munis_df) <- gsub("_", " ", colnames(munis_df))
        colnames(munis_df) <- gsub("Pct", "", colnames(munis_df))
        colnames(munis_df) <- gsub("plot", "", colnames(munis_df))
        return(munis_df)
    })
    
    output$plot_demo<-reactive({list(
        data = googleDataTable(munis_df()), options = list(title=paste(input$demo, "as a Percentage of the Population by Region ", input$muni, "over selected five years ", input$year)))
        
    })
    
    ## edu_df is a reactive dataframe. Necessary for when summary/plot/map have common input (Multiple Variables). Not in this project
    edu_df <- reactive({
        ## Filter the data by the chosen Five Year Range 
        edu_df <- edu_data %>%
            arrange(Region)
        ## Output reactive dataframe
        edu_df    
    })
    
    ## Create summary table
    output$summary_edu <- renderDataTable({
        #     browser()
        ## Make reactive dataframe into regular dataframe
        edu_df <- edu_df()
        
        ## make municipals a vector based on input variable
        if(!is.null(input$muni))
            munis <- input$muni
        ## if none selected, put all municipals in vector
        if(is.null(input$muni))
            munis <- c("United States", "MA")
        
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
        edu_df <- edu_df %>%
            filter(Five_Year_Range == input$year) %>%
            filter(Region %in% munis) %>%
            select(4:length(colnames(edu_df)))
        
        colnames(edu_df) <- c("Region", "Five Year Range", "Population over 25", "Population Margin of Error",
                              "% High School Graduate or Greater", "High School Margin of Error", 
                              "% Bachelor's Degree or Greater", "Bachelor's Margin of Error",
                              "% Graduate or Professional Degree", "Graduate Margin of Error")
        
        return(edu_df)
    }, options=list(searching = FALSE, orderClasses = TRUE, scrollX = TRUE)) # there are a bunch of options to edit the appearance of datatables, this removes one of the ugly features
    
    ## create the plot of the data
    ## for the Google charts plot
    output$plot_edu <- reactive({
        #     browser()
        ## make reactive dataframe into regular dataframe
        edu_df <- edu_df()%>%
            filter(Five_Year_Range == input$year) 
        
        ## find the county of the municipal
        county <- as.character(edu_df$County[match(input$muni, edu_df$Municipal)])
        
        ## make counties a vector based on input variable
        munis <- c(input$muni, county, "MA", "United States")
        
        muni_index <- c()
        for(i in 1:length(munis)){
            muni_index[i] <- match(munis[i], edu_df$Region)
        }
        
        edu_df$Region <- factor(edu_df$Region, levels = c(munis, as.character(edu_df$Region)[-muni_index]))
        
        munis_df <- edu_df[muni_index,]
        
        ## put data into form that googleCharts understands (this unmelts the dataframe)
        melted_munis_df <- melt(munis_df, id.vars = "Region", 
                                measure.vars = c("HS_Pct", "Bachelors_Pct", "Grad_Pct"),
                                variable.name = "Education_Attainment",
                                value.name = "Population_Pct")
        
        levels(melted_munis_df$Region)[1:4] <- munis
        
        plot_df <- melted_munis_df %>%
            arrange(Region)
        
        g <- dcast(plot_df, Education_Attainment ~ Region, 
                   value.var = "Population_Pct")
        
        g$Education_Attainment <- c("% with High School Graduate or Greater",
                                    "% with Bachelor's Degree or Greater",
                                    "% with Graduate or Professional Degree")
        
        ## this outputs the google data to be used in the UI to create the dataframe
        list(
            data = googleDataTable(g))
    })
    
    ## mar_df is a reactive dataframe. Necessary for when summary/plot/map have common input (Multiple Variables). Not in this project
    mar_df <- reactive({
        
        
        ## make year a vector based on input variable
        if(!is.null(input$year))
            years <- input$year
        ## if none selected, put all years in vector
        if(is.null(input$year))
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
    output$summary_mar <- renderDataTable({
        ## Make reactive dataframe into regular dataframe
        mar_df <- mar_df()
        
        ## make gender a vector based on input variable
        if(!is.null(input$gender))
            genders <- input$gender
        ## if none selected, put all genders in vector
        if(is.null(input$gender))
            genders <- c("Female", "Male")
        
        ## make municipals a vector based on input variable
        if(!is.null(input$muni))
            munis <- input$muni
        ## if none selected, put all municipals in vector
        if(is.null(input$muni))
            munis <- c("United States", "MA")
        
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
    }, options=list(searching = FALSE, orderClasses = TRUE, scrollX = TRUE)) # there are a bunch of options to edit the appearance of datatables, this removes one of the ugly features
    
    
    
    munis_p <- reactive({
        
        munis_p2 <- input$muni
        
        #MA
        if(input$MA_mean==T && any(grepl(x=munis_p2, pattern = "MA"))==F){
            return(c("MA", munis_p2[!(munis_p2 =="MA")])) ## US only ## MA only
        }else if(input$MA_mean==T && any(grepl(x=munis_p2, pattern = "MA"))==T){
            return(c("MA", munis_p2[!(munis_p2 =="MA")])) ## US only ## MA only
        }
        else if(input$MA_mean==F && any(grepl(x=munis_p2, pattern = "MA"))==T){
            return(munis_p2[!(munis_p2 =="MA")]) ## remove MA
        } else if(input$plotMA_mean==F && any(grepl(x=munis_p2, pattern = "MA"))==F){
            return(munis_p2[!(munis_p2 =="MA")]) ## remove MA
        }
    })
    
    munis_pfinal <- reactive({
        munis_p3 <- munis_p()
        #AMERICA FWURST
        if(input$US_mean==T && any(grepl(x=munis_p3, pattern = "United States"))==F){
            return(c("United States", munis_p3[!(munis_p3 =="United States")])) ##  United States
        }else if(input$US_mean==T && any(grepl(x=munis_p3, pattern = "United States"))==T){
            return(c("United States", munis_p3[!(munis_p3 =="United States")])) ## US  United States
        }
        else if(input$US_mean==F && any(grepl(x=munis_p3, pattern = "United States"))==T){
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
        # mess with the levels statement
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
    
})