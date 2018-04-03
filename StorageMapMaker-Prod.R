## Require Packages <- This could be cleaner
if("XML" %in% rownames(installed.packages()) == FALSE) {install.packages("XML")}
if("RCurl" %in% rownames(installed.packages()) == FALSE) {install.packages("RCurl")}
if("RJSONIO" %in% rownames(installed.packages()) == FALSE) {install.packages("RJSONIO")}
if("plyr" %in% rownames(installed.packages()) == FALSE) {install.packages("plyr")}
if("plotly" %in% rownames(installed.packages()) == FALSE) {install.packages("plotly")}
require("XML")
require("RCurl")
require("RJSONIO")
require("plyr")
require("plotly")


## Get/Clean new data
newData <- readHTMLTable("http://www.thetanktiger.com/available-storage/", which = 1)
newData <- newData[grep("\\$",newData$`Ask Price\n On Shell Capacity\n$/BBL/Month`), ] ## Return only pricing instances formatted with "$X.XX" <- This could be improved
newData$`Ask Price\n On Shell Capacity\n$/BBL/Month` <- as.numeric(gsub("[$,]", "", newData$`Ask Price\n On Shell Capacity\n$/BBL/Month`)) ## Remove the "$" so you can have a numeric value
newData <- newData[!(is.na(newData$`Ask Price\n On Shell Capacity\n$/BBL/Month`)), ] ## Remove the coerced NA's from cells originally formated like "$X.XX <SOME WORD BULLSHIT HERE>" <- This could be improved
newData$`Date Listed` <- as.Date(newData$`Date Listed`, format = "%m/%d/%Y") ## Format the dates
newData$`Date Listed` <- gsub("00", "20", as.character(newData$`Date Listed`)) ## Fix data writen as "XX/XX/XX". Should be "XX/XX/20XX" <- This could be improved.


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


## Get goelocations of new data
address <- as.character(newData$Location)
locations <- ldply(newData$Location, function(x) geoCode(x))
names(locations) <- c("lat", "lon", "Location Type", "Formatted Location")
newData <- na.omit(cbind(newData, locations))
newData <- newData[newData$lat != 0 & newData$lon != 0, ]


## Add new data to running CSV
cleanData <- read.csv(file.path(getwd(),"StorageRateData.csv"))
names(cleanData) <- names(newData)
cleanData <- rbind(cleanData, newData)
cleanData <- unique(cleanData)
write.csv(cleanData, file.path(getwd(),"StorageRateData.csv"), row.names = FALSE)


## Prepare the data to make a heat map
colnames(cleanData)[11] <- "Rate ($/BBL/Month)" ## Clean up an obnoxiously long column name.
cleanData$lat <- as.numeric(cleanData$lat)
cleanData$lon <- as.numeric(cleanData$lon)
  ## Get frequency of individual locations & add to dataframe <- This will determine the size of the dot
  ## Note: In the future this should be determined by a regional capacity amount.
  location.frequency <- count(cleanData, "`Formatted Location`")
    scale.factor <- 5  
    ## Function to count the number of times a formatted loation occurs
    bind.frequency <- function(x){
      cleanData$frequency[grepl(location.frequency$Formatted.Location[x], cleanData$`Formatted Location`)] <- location.frequency$freq[x]+scale.factor
    }
  cleanData$frequency<- as.numeric(lapply(cleanData$`Formatted Location`, bind.frequency))


## Define the map location for plotly <- check "old ideas" for pulling a google map and setting the window by avg gps coordinate  
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)


## Plot the data
p <- plot_geo(cleanData, locationmode = 'USA-states', sizes = c(1, 250)) %>%
  add_markers(
    x = ~lon, y = ~lat, size = ~frequency, color = ~`Rate ($/BBL/Month)`, hoverinfo = "text",
    text = ~paste(cleanData$`Formatted Location`, "<br />", 
                  "Rate: ", round(mean(cleanData$`Rate ($/BBL/Month)`), 2), "+/-", round(sd(cleanData$`Rate ($/BBL/Month)`), 2), " $/BBL/Month", "<br />", 
                  "# of Offers: ", cleanData$frequency-scale.factor)
  ) %>%
  layout(title = 'Bulk Storage Rates by Location', geo = g)

print(p)

Sys.setenv("plotly_username"="YOURNAMEHERE")
Sys.setenv("plotly_api_key"="YOURKEYHERE") ## 'Cause who needs security?


## Upload plot to repository
chart_link = api_create(p, filename="scattergeo-bubble") ## gotta pay to add the *sharing = "private"* tag
chart_link