# Providence Real Estate Transactions over the last 40 days.
## Required Packages
require('XML')
require('RCurl')
require('RJSONIO')
require("RgoogleMaps")
## Functions
### Construct URL required to get the Lat and Long from Google Maps
construct.geocode.url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}
### Now that we have the proper Google Maps address, get the resulting latitude and
### longitude
gGeoCode <- function(address) {
  u <- construct.geocode.url(address)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  lat <- x$results[[1]]$geometry$location$lat
  lng <- x$results[[1]]$geometry$location$lng
  return(c(lat, lng))
}
# Roll through addresses to create lat long
latlongroll <- function(address){
  lat <- vector(mode = "numeric", length = length(address))
  lng <- vector(mode = "numeric", length = length(address))
  for(i in 1:length(address)){
    print(i)
    flush.console()
    latlong <- gGeoCode(address[i])
    lat[i]<-latlong[1]
    lng[i]<-latlong[2]
  Sys.sleep(0.2)
  }
  return(cbind(lat,lng))
}

# Open to the most recent real estate transactions for Providence on the Projo
site <- 'http://www.providencejournal.com/homes/real-estate-transactions/assets/pages/real-estate-transactions-providence.htm'
# Read in the table with the header as variable names.
realestate.table<-readHTMLTable(site,header=T,which=1,stringsAsFactors=F)
# Remove the $ sign before the price
realestate.table$Price <- gsub("([$]{1})([0-9]+)","\\2",realestate.table$Price)
# Cast price character as numeric
realestate.table$Price<-as.numeric(realestate.table$Price)
# Cast date string as date type (lowercase %y means 2-digit year, uppercase is 4 digit)
realestate.table$Date <- as.Date(realestate.table$Date,format='%m/%d/%y')
# Dummy transactions or title changes have a price of $1, removing those from data set
providence <- subset(realestate.table,Price>1)
# Removing properties that do not have an address that start with a street number
providence <- subset(providence, grepl("^[0-9]+", providence$Address))
# Add lat and lng coordinates to each address
providence<-cbind(providence, latlongroll(providence[,3]))
# Calculate boundary lat and long for map
bb <- qbbox(c(41.769,41.88), c(-71.5057,-71.3435))
# Gets a map from Google Maps
map <- GetMap.bbox(bb$lonR, bb$latR, zoom=12, maptype="mobile")
# plot the points
PlotOnStaticMap(map,lon=providence$lng,lat=providence$lat, pch=19, col='blue')
#MapProvidence <- GetMap.OSM(lonR= c(-71.5057, -71.3435),
#latR = c(41.7615,41.8725), scale = 7500, destfile = "Providence.png")
