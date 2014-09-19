# libraries you need
require(ggplot2)
require(dplyr)
require(sp)
require(maptools)
require(Hmisc)
#require(GISTools)


MAmap <- readShapeSpatial("townssurvey_shp/TOWNSSURVEY_POLYM.shp")
munidata <- read.csv(file="munidata.csv")[,-1]
countydata <- read.csv(file="countydata.csv")[,-1]
madata <- read.csv(file="madata.csv")[,-1]
usdata <- read.csv(file="usdata.csv")[,-1]
mardata <- read.csv(file="marriagedata.csv")[,-1]
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                "#0072B2", "#D55E00", "#CC79A7")

shinyServer(function(input, output) {
  data <- reactive({
    if(input$type=="muni")
      df <- munidata
    if(input$type=="county")
      df <- countydata
    if(input$meanMA)
      df <- rbind.data.frame(madata, df)
    if(input$meanUS)
      df <- rbind.data.frame(usdata, df)
    colnames(df) <- gsub("_", " ", colnames(df))
    colnames(df) <- gsub("[.]", " ", colnames(df))
    
    if(input$var=="nm"){
      df <- cbind.data.frame(df[,c(1:6, 17)],
                             df[,grep("Never Married", colnames(df))])
    }
    if(input$var=="nmes"){
      df <- cbind.data.frame(df[,c(1:6, 17)],
                             df[,grep("Now Married", colnames(df))])
    }
    if(input$var=="sep"){
      df <- cbind.data.frame(df[,c(1:6, 17)],
                             df[,grep("Separated", colnames(df))])
    }
    if(input$var=="wid"){
      df <- cbind.data.frame(df[,c(1:6, 17)],
                             df[,grep("Widowed", colnames(df))])
    }
    if(input$var=="div"){
      df <- cbind.data.frame(df[,c(1:6, 17)],
                             df[,grep("Divorced", colnames(df))])
    }
    df[,9] <- round(df[,9],1)
    df    
  })
  
  output$summary <- renderDataTable({
    data <- data()

    if(input$timespan == "sing.yr")
      df <- filter(data, Year==input$year)
    
    if(input$timespan == "mult.yrs")
      df <- filter(data, Year>= min(input$range) & Year<=max(input$range))
    
    if(input$type=="muni")
      if(is.null(input$muni)==FALSE){
        munis <- c("MA"[input$meanMA], input$muni)
        df <- filter(df, Municipal %in% munis)
      }
    
    if(input$type=="county")
      if(is.null(input$county)==FALSE){
        counties <- c("MA"[input$meanMA], input$county)
        df <- filter(df, County %in% counties)
      }
    
    df[,c(1:6, 8:9)]
  })
  
  output$plot <- renderPlot({
    data <- data()
    if(input$var=="highneeds"){
      if(is.null(input$city)){
        boxhn <- qplot(data=data, x=as.factor(Year), y=data[,5],
                       geom="boxplot", outlier.size=0) +
          scale_x_discrete(name="Year") +
          scale_y_continuous(name="High Needs (%)", limits=c(0,100)) +
          ggtitle("Distribution of High Needs (%)") +
          theme_bw() +
          theme(axis.text.x = element_text(size=12),
                axis.title.x = element_text(size=14, face="bold"),
                axis.text.y = element_text(size=12),
                axis.title.y = element_text(size=14, face="bold"),
                plot.title = element_text(size=16, face="bold"),
                legend.title = element_text(size=12, face="bold"),
                legend.text = element_text(size = 12))
        
        print(boxhn)
      }
      else{
        cities <- input$city
        df <- c()
        for(i in 1:length(cities)){
          bbb <- subset(data, Municipal==cities[i])
          df <- rbind.data.frame(df, bbb)
        }
        
        df$Municipal <- factor(df$Municipal, levels = cities)
        boxhn <- qplot(data=data, x=as.factor(Year), y=data[,5], geom="boxplot",
                       outlier.size=0) +
          geom_point(data=df, aes(x=as.factor(Year), y=df[,5], fill=Municipal), 
                     size=5, shape=23)+
          scale_x_discrete(name="Year") +
          scale_y_continuous(name="High Needs (%)", limits=c(0,100)) +
          scale_fill_manual(name="Municipality", values=cbbPalette[2:8]) +
          ggtitle("Distribution of High Needs (%)") +
          theme_bw() +
          theme(axis.text.x = element_text(size=12),
                axis.title.x = element_text(size=14, face="bold"),
                axis.text.y = element_text(size=12),
                axis.title.y = element_text(size=14, face="bold"),
                plot.title = element_text(size=16, face="bold"),
                legend.title = element_text(size=12, face="bold"),
                legend.text = element_text(size = 11))
        print(boxhn)
      }
    }
    else{
      if(is.null(input$city)){
        df <- data[which(is.na(data[,4])==FALSE),]
        colnames(df)[5] <- "variable"
        df <- ddply(.data=df, .(Year), summarise, 
                    students = sum(Students, na.rm=TRUE),
                    var.w.m = weighted.mean(variable, Students, na.rm=TRUE)) 
        
        colnames(df)[3] <- colnames(data)[5]
        aaa <- qplot(data=df, x=Year, y=df[,3], 
                     geom=c("line", "point")) + 
          scale_x_continuous(breaks=seq(2003, 2012, 1)) +
          scale_y_continuous(name=colnames(df)[3], limits=c(0,100)) +
          ggtitle(paste("Average", colnames(df)[3], "Over Time")) +
          theme_bw() + 
          theme(axis.text.x = element_text(size=12),
                axis.title.x = element_text(size=14, face="bold"),
                axis.text.y = element_text(size=12),
                axis.title.y = element_text(size=14, face="bold"),
                plot.title = element_text(size=16, face="bold"),
                legend.title = element_text(size=12, face="bold"),
                legend.text = element_text(size = 11))
        print(aaa)
      }
      else{
        if(input$mean==FALSE){
          cities <- input$city
          df <- c()
          for(i in 1:length(cities)){
            bbb <- subset(data, Municipal==cities[i])
            df <- rbind.data.frame(df, bbb)
          }
          
          df$Municipal <- factor(df$Municipal, levels = cities)
          
          aaa <- qplot(data=df, x=Year, y=df[,5], color=Municipal, 
                       geom=c("line", "point")) + 
            scale_x_continuous(breaks=seq(2003, 2012, 1)) +
            scale_y_continuous(name=colnames(df)[5], limits=c(0,100)) +
            ggtitle(paste(colnames(df)[5], "Over Time")) +
            scale_color_manual(name="Municipality", values=cbbPalette[2:8]) +
            theme_bw() + 
            theme(axis.text.x = element_text(size=12),
                  axis.title.x = element_text(size=14, face="bold"),
                  axis.text.y = element_text(size=12),
                  axis.title.y = element_text(size=14, face="bold"),
                  plot.title = element_text(size=16, face="bold"),
                  legend.title = element_text(size=12, face="bold"),
                  legend.text = element_text(size = 11))
          print(aaa)}
        else{
          df <- data[which(is.na(data[,4])==FALSE),]
          colnames(df)[5] <- "variable"
          df <- ddply(.data=df, .(Year), summarise, 
                      students = sum(Students),
                      var.w.m = weighted.mean(variable, Students, na.rm=TRUE))
          colnames(df)[3] <- colnames(data)[5]
          
          cities <- input$city
          
          df1 <- c()
          for(i in 1:length(cities)){
            bbb <- subset(data, Municipal==cities[i])
            df1 <- rbind.data.frame(df1, bbb)
          }
          
          df2 <- data.frame(Municipal=c(rep("Average", length(df[,1])), 
                                        as.character(df1$Municipal)),
                            Year=c(df$Year, df1$Year), 
                            Students=c(df$students, df1$Students),
                            variable=c(df[,3], df1[,5]))
          colnames(df2)[4] <- colnames(df1)[5]
          
          df2$Municipal <- factor(df2$Municipal, 
                                  levels = c("Average", cities))
          
          aaa <- qplot(data=df2, x=Year, y=df2[,4], color=Municipal, 
                       size=Municipal, geom=c("line")) + 
            geom_point(size=2) +
            scale_x_continuous(breaks=seq(2003, 2012, 1)) +
            scale_y_continuous(name=colnames(df2)[4], limits=c(0,100)) +
            ggtitle(paste(colnames(df2)[4], "Over Time")) +
            scale_color_manual(name="Municipality", values=cbbPalette) +
            scale_size_manual(name="Municipality",
                              values=c(1, rep(.5,length(cities)))) +
            theme_bw() + 
            theme(axis.text.x = element_text(size=12),
                  axis.title.x = element_text(size=14, face="bold"),
                  axis.text.y = element_text(size=12),
                  axis.title.y = element_text(size=14, face="bold"),
                  plot.title = element_text(size=16, face="bold"),
                  legend.title = element_text(size=12, face="bold"),
                  legend.text = element_text(size = 11))
          print(aaa)
        }
      }}
  })
  
  output$map <- renderPlot({
    
    data <- data()
    data$Municipal <- ifelse(data$Municipal=="Manchester-by-the-Sea", 
                             "Manchester", as.character(data$Municipal))
    
    if(input$timespan == "sing.yr"){
      if(input$var == "flne"){
        paint.brush <- colorRampPalette(colors=c("white", cbbPalette[6]))
        map.colors <- c(paint.brush(n=50), "#999999")
      }
      if(input$var == "ell"){
        paint.brush <- colorRampPalette(colors=c("white", cbbPalette[7]))
        map.colors <- c(paint.brush(n=50), "#999999")
      }
      if(input$var == "dis"){
        paint.brush <- colorRampPalette(colors=c("white", cbbPalette[4]))
        map.colors <- c(paint.brush(n=20), "#999999")
      }
      if(input$var == "lowinc"){
        paint.brush <- colorRampPalette(colors=c("white", cbbPalette[2]))
        map.colors <- c(paint.brush(n=50), "#999999")
      }
      if(input$var == "highneeds"){
        paint.brush <- colorRampPalette(colors=c("white", "#CC0066"))
        map.colors <- c(paint.brush(n=50), "#999999")
      }
      max.val <- max(data[,5], na.rm=TRUE)
      min.val <- min(data[,5], na.rm=TRUE)
      data <- subset(data, Year==input$year)
      #map.colors <- c(heat.colors(n=20), "#999999")
      cuts <- seq(min.val, max.val, 
                  (max.val-min.val)/(length(map.colors)-1))
      #data[,5] <- ifelse(is.na(data[,5]), 0, data[,5])
      color <- as.integer(cut2(data[,5],cuts=cuts))
      data <- cbind.data.frame(data,color)
      missing.towns <- setdiff(MAmap$TOWN, toupper(data$Municipal))
      df <- data.frame(Municipal=missing.towns, County=NA, Year=NA, 
                       Students=NA, Fifth=NA, color=length(map.colors))
      colnames(df)[5] <- colnames(data)[5]
      data <- rbind.data.frame(data, df)
      MAmapA <- MAmap[match(toupper(data[,"Municipal"]), MAmap$TOWN),]
      
      layout(matrix(1:2, ncol=2), width=c(3,1), height=c(1,1))
      plot(MAmapA, col=map.colors[data[,"color"]], border=gray(.85))
      title(main=paste(input$year, colnames(df)[5], "by Municipality"))
      legend("bottom", legend="No Data Available", 
             fill = map.colors[length(map.colors)])
      legend_image <- as.raster(matrix(rev(
        map.colors[1:(length(map.colors)-1)]), ncol=1))
      plot(c(0,2),c(0,1), type='n', axes=F, xlab='', ylab='', main='')
      legend.label <- seq(round(min.val), round(max.val), l=5)
      legend.label <- paste0(as.character(round(legend.label)),"%")
      text(x=1.5, y=seq(0,1,l=5), 
           labels=legend.label)
      rasterImage(legend_image, 0, 0, 1, 1)
    }
    
    if(input$timespan == "mult.yrs"){
      if(input$var == "flne"){
        paint.brush <- colorRampPalette(colors=c(cbbPalette[5], "white", cbbPalette[6]))
        map.colors <- c(paint.brush(n=15), "#999999")
      }
      if(input$var == "ell"){
        paint.brush <- colorRampPalette(colors=c(cbbPalette[6],"white", cbbPalette[7]))
        map.colors <- c(paint.brush(n=15), "#999999")
      }
      if(input$var == "dis"){
        paint.brush <- colorRampPalette(colors=c(cbbPalette[8], "white", cbbPalette[4]))
        map.colors <- c(paint.brush(n=15), "#999999")
      }
      if(input$var == "lowinc"){
        paint.brush <- colorRampPalette(colors=c(cbbPalette[4], "white", cbbPalette[2]))
        map.colors <- c(paint.brush(n=15), "#999999")
      }
      if(input$var == "highneeds"){
        
      }
      
      df <- data
      df$Municipal <- ifelse(df$Municipal=="Manchester-by-the-Sea", 
                             "Manchester", as.character(df$Municipal))
      colnames(df)[5] <- "var"
      
      bound <- ddply(.data=df, .(Municipal), summarise, 
                     max.val = max(var, na.rm=TRUE),
                     min.val = min(var, na.rm=TRUE))
      bound$diff <- bound$max.val - bound$min.val
      
      max.val <- quantile(bound$diff, .95)
      min.val <- -1*max.val
      
      min.year <- min(input$range)
      max.year <- max(input$range)
      min.df <- subset(df, Year==min.year)
      max.df <- subset(df, Year==max.year)
      diff.df <- within(merge(min.df, max.df, by="Municipal"),{
        Difference <- var.y - var.x
      })[,c("Municipal", "Difference")]
      
      #max.val <- max(diff.df[,2], na.rm=TRUE)
      #min.val <- min(diff.df[,2], na.rm=TRUE)
      
      
      cuts <- seq(min.val, max.val, 
                  (max.val-min.val)/(length(map.colors)-1))
      #diff.df[,2] <- ifelse(is.na(diff.df[,2]), 0, diff.df[,2])
      color <- as.integer(cut2(diff.df[,2],cuts=cuts))
      diff.df <- cbind.data.frame(diff.df,color)
      
      missing.towns <- setdiff(MAmap$TOWN, toupper(diff.df$Municipal))
      df <- data.frame(Municipal=missing.towns, Difference=NA,
                       color=length(map.colors))
      diff.df <- rbind.data.frame(diff.df, df)
      MAmapA <- MAmap[match(toupper(diff.df[,"Municipal"]), MAmap$TOWN),]
      
      layout(matrix(1:2, ncol=2), width=c(3,1), height=c(1,1))
      
      plot(MAmapA, col=map.colors[diff.df[,"color"]], border=gray(.85))
      title(main=paste("Difference in", colnames(data)[5], "\nbetween", min.year, "and", max.year))
      legend("bottom", legend="No Data Available", 
             fill = map.colors[length(map.colors)])
      legend_image <- as.raster(matrix(rev(
        map.colors[1:(length(map.colors)-1)]), ncol=1))
      plot(c(0,2),c(0,1), type='n', axes=F, xlab='', ylab='', main='')
      legend.label <- seq(round(min.val), round(max.val), l=5)
      legend.label <- paste0(as.character(round(legend.label)),"%")
      text(x=1.5, y=seq(0,1,l=5), 
           labels=legend.label)
      rasterImage(legend_image, 0, 0, 1, 1)
    }
    
  })
})