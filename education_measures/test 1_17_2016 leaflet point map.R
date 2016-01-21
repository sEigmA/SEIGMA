plotcuts=c(seq(from=10, to=90, by=10))

df$varcuts<-cut(df$Females, plotcuts)

colordf=data.frame("varcuts"=unique(cut(df$Females, 
                                        plotcuts))[order(unique(cut(df$Females, 
                                                                    plotcuts)))], 
                   "colors"=c(pctpaint.brush(n=length(unique(cut(df$Females, plotcuts)))-1), 
                              "#999999"))
df<-merge(df, colordf, by="varcuts")



#
# or use colorNumeric
#

pal <- colorNumeric(
  palette = c("white", "violetred"),
  c(range(edu_data$African.American))
)

leaflet(df) %>% 
  +   addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
  +   addCircleMarkers(lng = ~lon, lat = ~lat, radius=~scale(Total.Students.Enrolled), 
                       +                    color=~pal(African.American))