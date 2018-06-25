# data preparation helper
# run these only if newer data is available from openflights.org
library(RCurl)
library(rgdal)
library(raster) # conflicts with data.table, so unload

airportspath <- "https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat"
airlinespath <- "https://raw.githubusercontent.com/jpatokal/openflights/master/data/airlines.dat"
routespath <- "https://raw.githubusercontent.com/jpatokal/openflights/master/data/routes.dat"

airline_headers <- c("Airline_ID", "Name", "Alias", "IATA", "ICAO", "Callsign", "Country", "Active")
airports_headers <- c("Airport_ID", "Name", "City", "Country", "IATA",
                      "ICAO", "Latitude", "Longitude", "Altitude", "Timezone",
                      "DST", "Tz", "Type", "Source")
routes_headers <- c("Airline", "Airline_ID", "Source_airport", "Source_airport_ID",
                    "Destination_airport", "Destination_airport_ID", "Codeshare", "Stops", "Equipment")

df <- map2(list(airportspath, airlinespath, routespath), 
           list(airports_headers, airline_headers, routes_headers), 
           function(x, y) {fread(getURL(x), col.names = y)})

airports <- df[[1]]
airports[airports$ICAO == "OTBD"]$IATA <- "DOH" #repair IATA code of Doha airport

airlines <- df[[2]] %>% filter(Active == "Y")
routes <- df[[3]]

save(airports, file = "data/airports.Rdata")
save(airlines, file = "data/airlines.Rdata")
save(routes, file = "data/routes.Rdata")

### pre-calculate routes for top50 airlines
top50AirlineRoutes <- map_df(top50airlines$IATA, airlinesx)
save(top50AirlineRoutes, file = "data/top50AirlinesRoutes.Rdata")


### process countries polygons
countries <- readOGR("data/countries.geo.json", layer = "OGRGeoJSON")
save(countries, file = "data/countries.Rdata")

# get min and max coords for fitBounds, using raster::extent
ext <- t(sapply(1:length(countries), function(i) as.vector(extent(countries[i, ]))))
countries.bounds <- data.frame(country = countries$name, 
                               lng1 = ext[ ,1], 
                               lat1 = ext[ ,3],
                               lng2 = ext[ ,2],
                               lat2 = ext[ ,4])
save(countries.bounds, file = "data/countries.bounds.Rdata")
