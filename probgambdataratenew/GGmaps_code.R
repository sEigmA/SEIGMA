
##London Code


require(ggmap)

mapImageData1 <- get_map(location = c(lon = -0.016179, lat = 51.538525),
                         color = "color",
                         source = "google",
                         maptype = "satellite",
                         zoom = 17)

ggmap(mapImageData1,
      extent = "device",
      ylab = "Latitude",
      xlab = "Longitude")


geocode("the white house")

geocode("massachusetts")
geocode("berkshire county")
