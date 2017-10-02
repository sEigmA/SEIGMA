################################
## Title: SEIGMA dashboard    ##
## Author: Zhenning Kang      ##
## Date Created:  09/27/2017  ##
## Date Modified: 10/01/2017  ##
################################

##### load in libraries #####
library(shiny)
library(shinydashboard)
library(dplyr)
library(maptools)
library(Hmisc)
library(reshape2)
library(shiny)
library(googleCharts)
library(leaflet)
library(RJSONIO)
library(googleCharts)


## Load formatted marital status data
mar_data <- read.csv(file="BA002_02_marriagedata.csv")

names(mar_data)[10:12] <- gsub("Now_", "", names(mar_data)[10:12])

##### following codes copied from demograpics global #####
## load map data
MA_map_county <- fromJSON("County_2010Census_DP1.geojson")
MA_map_muni <- fromJSON("Muni_2010Census_DP1.geojson")

## Load formatted marital status data
## -1 eliminates first column [rows,columns]
Dem_data <- read.csv(file="demodata.csv")
colnames(Dem_data)[12:37] <- c("Age_under_5_Pct","Margin_Error_under_5_Pct","Age_5-9_Pct", "Margin_Error_5-9_Pct",
                               "Age_10-14_Pct","Margin_Error_10-14_Pct","Age_15-19_Pct", "Margin_Error_15-19_Pct",
                               "Age_20-24_Pct","Margin_Error_20-24_Pct","Age_25-34_Pct", "Margin_Error_25-34_Pct",
                               "Age_35-44_Pct", "Margin_Error_35-44_Pct","Age_45-54_Pct", "Margin_Error_45-54_Pct",
                               "Age_55-59_Pct", "Margin_Error_55-59_Pct","Age_60-64_Pct", "Margin_Error_60-64_Pct",
                               "Age_65-74_Pct", "Margin_Error_65-74_Pct","Age_75-84_Pct", "Margin_Error_75-84_Pct",
                               "Age_85+Pct", "Margin_Error_85+_Pct")
colnames(Dem_data)[54:59] <- c("Age_under_20_Pct_plot","Age_20-34_Pct_plot","Age_35-54_Pct_plot", "Age_55-64_Pct_plot",
                               "Age_65-74_Pct_plot", "Age_75+Pct_plot")
## Find order of counties in geojson files
## Each county is a separate feature
MA_counties <- c()
for(i in 1:length(MA_map_county$features)){
  MA_counties <- c(MA_counties, MA_map_county$features[[i]]$properties$County)
}

## Find order of municipals in geojson files
## Each municipal is a separate feature
for(i in 1:length(MA_map_muni$features)){
  MA_map_muni$features[[i]]$properties$NAMELSAD10 <- substr(MA_map_muni$features[[i]]$properties$NAMELSAD10, 1, nchar(MA_map_muni$features[[i]]$properties$NAMELSAD10)-5)
}

MA_municipals_map <- c()
for(i in 1:length(MA_map_muni$features)){
  MA_municipals_map <- c(MA_municipals_map, MA_map_muni$features[[i]]$properties$NAMELSAD10)
}

idx_leftovers <- which(!MA_municipals_map %in% Dem_data$Region)
leftover_munis <- MA_municipals_map[idx_leftovers]
for(i in 1:length(leftover_munis)){
  MA_map_muni$features[[idx_leftovers[i]]]$properties$NAMELSAD10 <- 
    substr(leftover_munis[i], 1, nchar(leftover_munis[i])-5)
}

MA_municipals <- c()
for(i in 1:length(MA_map_muni$features)){
  MA_municipals <- c(MA_municipals, MA_map_muni$features[[i]]$properties$NAMELSAD10)
}
idx_leftovers2 <- which(!MA_municipals %in% Dem_data$Region)
leftover_munis_map <- MA_municipals[idx_leftovers2]
MA_municipals <- sort(MA_municipals[-idx_leftovers2])

########### UI ##########

header <- dashboardHeader(title = "SEIGMA Dashboard")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Municipality", icon = icon("address-book"),
             selectInput("plot_muni", "Select Your Municipality",
                         choices = MA_municipals,
                         selected = "Abington",
                         multiple = FALSE)
             ),
    
    menuItem("Year Range", icon = icon("calendar"),
             selectInput("plot_year", "Select Five Year Range",
                         choices = list("2005-2009" = "2005-2009", 
                                        "2006-2010" = "2006-2010", 
                                        "2007-2011" = "2007-2011",
                                        "2008-2012" = "2008-2012", 
                                        "2009-2013" = "2009-2013", 
                                        "2010-2014" = "2010-2014",
                                        "2011-2015" = "2011-2015"),
                         multiple = FALSE)
             ),
    
    menuItem("Demographics", icon = icon("area-chart"),
             radioButtons("plot_radio", "Select a Variable of Interest",
                         c("Age" = "Age", "Gender" = "Gender","Race" = "Race","Ethnicity" ="Ethnicity"),
                         selected="Age")
             ),
    
    menuItem("Employment", icon = icon("bank"),
            radioButtons("plot_empl", "Select Variable of Interest",
                         c("Employment and Business Establishments" = "Employment and Establishments","Wages" = "Wages"),
                         selected="Employment and Establishments",
                         width = '98%'),
            radioButtons("plot_display_empl", "Display Options",
                         c("Actual Values"="Actual Values", "Change Since 2003"="Change_Pct"),
                         selected="Actual Values",
                         width = '98%')
            ),
    menuItem("Marital Status", icon = icon("heart"),
             selectInput("plotvar", "Select Variable of Interest",
                         choices = list("Never Married" = "Never_Married_pct", 
                                        "Married" = "Married_pct",
                                        "Separated" = "Separated_pct",
                                        "Widowed" = "Widowed_pct",
                                        "Divorced" = "Divorced_pct"),
                         width = '98%'),
             radioButtons("plotcombine", "Show Plots by Gender", 
                         choices = list("Separated" = "Separate",
                                        "Together" = "Together"),
                         width = '98%')
    ),
    menuItem("Comparison", icon = icon("check-square-o"),
             checkboxInput("MA_mean", "Compare to MA Average", FALSE),
             checkboxInput("US_mean", "Compare to US Average", FALSE)
             ),
    menuItem("Distance", icon = icon("automobile"),
             sliderInput("slider", "Choose a Distance", min=0, max=10, value=0))
    )
  )

body <- dashboardBody(
  fluidRow(
      box(
          title = "Demographics", status = "primary", solidHeader = TRUE, collapsible = TRUE

      ),
      
      box(
          title = "Employment", status = "warning", solidHeader = TRUE,  collapsible = TRUE,
          "Box content here", br(), "More box content",
          sliderInput("slider", "Slider input:", 1, 100, 50),
          textInput("text", "Text input:")
      )
  ),
  fluidRow(
      box(
          title = "Marital Status", status = "primary", solidHeader = TRUE, collapsible = TRUE,
          plotOutput("fplot", height = 250),
          plotOutput("mplot", height = 250)
      ),
      
      box(
          title = "Household Income", status = "warning", solidHeader = TRUE,  collapsible = TRUE,
          "Box content here", br(), "More box content",
          sliderInput("slider", "Slider input:", 1, 100, 50),
          textInput("text", "Text input:")
      )
  )
)

########## SERVER ##########

server <- function(input, output, session) {
    
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
  
  Dem_df <- reactive({
    Dem_df <- Dem_data       
    ## Output reactive dataframe
    Dem_df
  })
  
  munis_df <- reactive({
    #     browser()
    ## make reactive dataframe into regular dataframe
    Dem_df <- Dem_df()%>%
      filter(Five_Year_Range == input$plot_year) 
    ## find the county of the municipal
    county <- as.character(Dem_df$County[match(input$plot_muni, Dem_df$Municipal)])
    
    ## make counties a vector based on input variable
    munis <- c(input$plot_muni, county, "MA", "United States")
    
    muni_index <- c()
    for(i in 1:length(munis)){
      muni_index[i] <- match(munis[i], Dem_df$Region)
    }
    
    Dem_df$Region <- factor(Dem_df$Region, levels = c(munis, as.character(Dem_df$Region)[-muni_index]))
    
    sel_col_num1<-c()
    if (input$plot_radio=="Age") {
      sel_col_num1<-c(54, 55, 56, 57, 58, 59)
    } else if (input$plot_radio=="Gender") {
      sel_col_num1<-c(8,10)
    } else if (input$plot_radio=="Race") {
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
  
  output$plot <- renderPlot({
    df.long<-melt(munis_df)
    ggplot(df.long,aes(Region,value,fill=variable))+
      geom_bar(stat="identity",position="dodge")
      # ggtitle(paste0(input$plot_radio," Disribution in ",input$plot_year)) + xlab("") + ylab("Percentile") + theme(plot.title = element_text(size=14, face="bold"))
  })
}

##### RUN APP #####
shinyApp(
  ui = dashboardPage(header, sidebar, body, skin="red"),
  server = server 
  )

