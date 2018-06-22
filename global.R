# helper functions for the airports app
# the routesx and airlinesx functions give interleaved df with the routes, the x argument is an airport or an airline (IATA code)

# get the routes from an airport
routesx <- function(x) {
  
  routesx <- routes[Source_airport == x, ]
  
  sourcedf <- routesx %>% 
    select(Airline, Source_airport) %>% 
    left_join(airports, by = c("Source_airport" = "IATA")) %>% 
    rename(airport = Source_airport)
  
  destdf <-  routesx %>% 
    select(Airline, Destination_airport) %>% 
    left_join(airports, by = c("Destination_airport" = "IATA")) %>% 
    rename(airport = Destination_airport)
  
  routesdf <- gdata::interleave(sourcedf, destdf)
  
  return(routesdf)
  
}

# get the routes an airline flies
airlinesx <- function(x) {
  routesx <- routes[Airline %in% x, ]
  
  sourcedf <- routesx %>% 
    select(Airline, Source_airport) %>% 
    left_join(airports, by = c("Source_airport" = "IATA")) %>% 
    rename(airport = Source_airport)
  
  destdf <-  routesx %>% 
    select(Airline, Destination_airport) %>% 
    left_join(airports, by = c("Destination_airport" = "IATA")) %>% 
    rename(airport = Destination_airport)
  
  routesdf <- gdata::interleave(sourcedf, destdf)
  
  
  return(routesdf)
  
}

# function to print airports info (click events)
showAirportInfo <- function(x) {
  if(is.null(x))
    return(paste0("Click on an airport or select an airline to see info"))
  name <- airports[airports$IATA == x, ]$Name # get the full name using the IATA code
  
  selected <- airports.info.destination[airports.info.destination$Source_airport == x, ]
  nroutes <- nrow(selected)
  ncities <- selected %>% count(City) %>% nrow()
  ncountries <- selected %>% count(Country) %>% nrow()
  nairlines <- selected %>% count(Airline) %>% nrow()
  
  paste0(tags$b(name), tags$br(), tags$br(),
         tags$b(nroutes), " routes to", tags$br(), 
         tags$b(ncities), " cities in", tags$br(),
         tags$b(ncountries), " countries, operated by", tags$br(),
         tags$b(nairlines), " airlines")
}

# function to print Airline info
showAirlineInfo <- function(x, filters) {
  if(is.null(x))
    return(paste0("Click on an airport on the map or select an airline to see the routes"))
  
  if(is.null(filters)) {
    selected <- airlines.routes.summary %>% filter(Airline == x)
    nroutes <- nrow(selected)
    ncities <- selected %>% count(City) %>% nrow()
    ncountries <- selected %>% count(Country) %>% nrow()
    name <- selected$Name[1]
  
    paste0(tags$b(name), tags$br(), tags$br(), 
           tags$b(nroutes), " routes to", tags$br(),
           tags$b(ncities), " cities in", tags$br(),
           tags$b(ncountries), " countries")
  } else {
    selected <- airlines.routes.summary %>% filter(Airline == x, Source_airport == filters)
    nroutes <- nrow(selected)
    ncities <- selected %>% count(City) %>% nrow()
    ncountries <- selected %>% count(Country) %>% nrow()
    name <- selected$Name[1]
    
    paste0(tags$b(name), tags$br(), tags$br(), 
           "has ", tags$b(nroutes), " routes from ", tags$br(),
           tags$b(filters), " to ",
           tags$b(ncities), " cities in", tags$br(),
           tags$b(ncountries), " countries")
 }
}
  
  
drawroutesAirportClear <- function(x, filters) {
  if(is.null(filters))
    dataforPolylines <- routesx(x) else {
      dataforPolylines <- routesx(x) %>% dplyr::filter(Airline %in% filters)
    }
  #this is to get a IATA codes for all destinations a "x" has and then join with the df containing "n" (number of outgoing routes)
  #destinations <- airports.info.destination[airports.info.destination$Source_airport == x, ]$Destination_airport
  destinations <- dataforPolylines$airport %>% unique()
  dataforCircles <- airports.source.summary[airports.source.summary$Source_airport %in% destinations, ] %>%
                      filter(!is.na(Longitude) | !is.na(Latitude))
  
  leafletProxy("map") %>%
    clearGroup("routes1") %>%
    #clearGroup("routes2") %>%
    addMapPane("newcircles", zIndex = 410) %>% # set zIndex so that the new points are below
    
    addGeodesicPolylines(group = "routes1", #with group assignment so that it can be cleared
                         lng = ~Longitude, lat = ~Latitude,
                         steps = 50,
                         weight = 0.4,
                         opacity = 0.6,
                         color = "orange",
                         data = dataforPolylines 
    ) %>%
    addCircles(group = "routes1",
               #layerId = ~Destination_airport,
              lng = ~Longitude, lat = ~Latitude,
              stroke = T,
              fillColor = "orange",
              weight = 3,
              color = "orange",
              opacity = 0.5,
              radius = ~n*100,
              fillOpacity = 1,
              data = dataforCircles,
              options = pathOptions(pane = "newcircles")
    ) #%>%
    #fitBounds(min(dataforCircles$Longitude), min(dataforCircles$Latitude), max(dataforCircles$Longitude), max(dataforCircles$Latitude))
  
}

drawroutesAirportKeep <- function(x, filters) {
  if(is.null(filters))
    dataforPolylines <- routesx(x) else {
  dataforPolylines <- routesx(x) %>% dplyr::filter(Airline %in% filters)
    }
  destinations <- airports.info.destination[airports.info.destination$Source_airport == x, ]$Destination_airport
  dataforCircles <- airports.source.summary[airports.source.summary$Source_airport %in% destinations, ] %>%
                      filter(!is.na(Longitude) | !is.na(Latitude))
  
    leafletProxy("map") %>%
      addMapPane("newcircles", zIndex = 410) %>% # set zIndex so that the new points are below
    #clearGroup("routes1") %>%
      addGeodesicPolylines(group = "routes2",
                         lng = ~Longitude, lat = ~Latitude,
                         steps = 50,
                         weight = 0.4,
                         opacity = 0.6,
                         color = "orange",
                         data = dataforPolylines
    ) %>%
      addCircles(group = "routes2",
                 
                 lng = ~Longitude, lat = ~Latitude,
                 stroke = T,
                 fillColor = "orange",
                 weight = 3,
                 color = "orange",
                 opacity = 0.5,
                 radius = ~n*100, # a bit bigger than the main circles, which are n * 100?
                 fillOpacity = 1,
                 data = dataforCircles,
                 options = pathOptions(pane = "newcircles")
      )
  
}

### function to draw routes of an airline (or many airlines if selected)
# using split and map2...

  
drawroutesAirlines <- function(x) {
  # 
  datax <- airlinesx(x)
  splitdf <- split(datax, datax$Airline)
  colors <- terrain_hcl(length(splitdf), alpha = 0.8)
  
  
  leafletProxy("map") %>%
    clearGroup("routes1") %>%
    clearGroup("routes2") %>%
    clearGroup("routes3")
  # 
  #actual drawing of polylines
  map2(splitdf, colors, function(y, z) {
                    leafletProxy("map") %>%
                    
                      # clearGroup("routes1") %>%
                      # clearGroup("routes2") %>%
                      addGeodesicPolylines(group = "routes3",
                         lng = ~Longitude, lat = ~Latitude,
                         steps = 50,
                         weight = 0.3,
                         opacity = 0.3,
                         color = z,
                         data = y)
                      
                      }
        )
                    
  
}

