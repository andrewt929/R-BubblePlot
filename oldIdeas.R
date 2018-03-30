## Heat Map Examples: 
## 1) Basemap on Center: http://trucvietle.me/r/tutorial/2017/01/18/spatial-heat-map-plotting-using-r.html
## 2) Plot Pricing: https://rpubs.com/jpmurillo/homeprice_heatmap



#### OLD IDEAS ####

## Generate colors based on price
my.palette <- colorRamp(c("red", "yellow"))
## Normalize Prices
rr <- range(cleanData$`Rate ($/BBL/Month)`)
normalized.price <- (cleanData$`Rate ($/BBL/Month)`-rr[1])/diff(rr)
cleanData$colors <-  rgb(my.palette(normalized.price), maxColorValue = 255)

## Specify a map with its center at the center of all the coordinates
## Define geolocation functions (http://www.jose-gonzalez.org/using-google-maps-api-r/)
url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}
geoCode <- function(address,verbose=FALSE) {
  if(verbose) cat(address,"\n")
  u <- url(address)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    location_type <- x$results[[1]]$geometry$location_type
    formatted_address <- x$results[[1]]$formatted_address
    return(c(lat, lng, location_type, formatted_address))
    Sys.sleep(2)
  } else {
    return(c(NA,NA,NA,NA))
  }
}

mean.longitude <- mean(cleanData$lon)
mean.latitude <- mean(cleanData$lat)
price.map <- get_map(location = c(mean.longitude, mean.latitude), zoom = 4, scale = 1)
## Convert into ggmap object
price.map <- ggmap(price.map) + geom_point(data = cleanData, aes(x = cleanData$lon, y = cleanData$lat, colour = cleanData$`Rate ($/BBL/Month)`)) + scale_colour_gradient(low="red")
price.map <- price.map + plot_geo(cleanData, x = ~cleanData$lon, y = ~cleanData$lat, text = ~cleanData$`Formatted Location`, type = 'scatter', mode = 'markers', marker = list(size = 1:10, opacity = 0.5, color = 'rgb(255, 65, 54)'))
plot(price.map)


## Add Pricing data layer
price.map <- price.map + 
  geom_title(data = cleanData, aes(x = lon, y = lat, alpha = cleanData$`Ask Price
                                   On Shell Capacity
                                   $/BBL/Month`), fill = 'red') + 
  
  ## Plot a heat map layer: Polygons with fill colors based on relative frequency of events
  price.map <- price.map + stat_density2d(data = cleanData ,
                                          aes(x=cleanData$lon, y=cleanData$lat, colour=cleanData$`Ask Price
                                              On Shell Capacity
                                              $/BBL/Month`, alpha=..level..), geom="polygon")
## Define the spectral colors to fill the density contours
price.map <- price.map + scale_fill_gradientn(colours=cleanData$`Ask Price
                                              On Shell Capacity
                                              $/BBL/Month`)

## Add the strike points, color them red and define round shape
##price.map <- price.map + geom_point(data=cleanData,
##                                    aes(x=lat, y=lon), fill="red", shape=21, alpha=0.8)

## Remove any legends
## price.map <- price.map + guides(size=FALSE, alpha = FALSE)

## Give the map a title
price.map <- price.map + ggtitle("Bulk Tank Storage Prices from " + min(cleanData$`Y+Q Listed`) + " to " + max(cleanData$`Y+Q Listed`))

## Plot strikes by each year
## price.map <- price.map + facet_wrap(~`Y+Q Listed`)
print(price.map) # this is necessary to display the plot






jpeg(filename = "test.jpeg")

us_map <- get_map(location = "United States", maptype = "hybrid", zoom = 4)

ggmap(us_map, extent = "device") + geom_density2d(data = data, 
                                                  aes(x = lon, y = lat), size = 0.3) + stat_density2d(data = data, 
                                                                                                      aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), size = 0.1,
                                                                                                      bins = 5, geom = "polygon") + scale_fill_gradient(low = "green", high = "red", 
                                                                                                                                                        guide = FALSE) + scale_alpha(range = c(0, 0.3), guide = FALSE)

dev.off()