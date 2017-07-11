#unified server

shinyServer(function(input, output, session) {
  ## Dem_df is a reactive dataframe. Necessary for when summary/plot/map have common input (Multiple Variables). Not in this project
  
  Dem_df <- reactive({
    Dem_df <- Dem_data       
    ## Output reactive dataframe
    Dem_df
  })
  
  output$map <- renderLeaflet({
    leaflet(MA_map_muni) %>% addTiles() %>% fitBounds(-73.5,41, -70,43) %>%
      addPolygons(stroke = TRUE, smoothFactor = 0.3, fillOpacity = 1,
                  label = ~NAMELSAD10)
  })
  
})