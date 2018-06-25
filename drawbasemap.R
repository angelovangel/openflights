# function to draw base map
drawbasemap <- function() {
 
  leaflet(options = leafletOptions(zoomControl = FALSE,  # these options are just for fun and to test if they are working in R
                                   minZoom = 2.5, 
                                   maxZoom = 11, 
                                   zoomDelta = 0.5, 
                                   zoomSnap = 0.5, 
                                   worldCopyJump = T)) %>% 
  
  addProviderTiles("CartoDB.DarkMatter")
}

# function to draw main circles, the size of the circles corresponds to the number of outgoing routes from that airport
drawMainCircles <- function(size) {
  leafletProxy("map", data =  airports.source.summary) %>%
    clearGroup("mainCircles") %>%
     addMapPane("oldcircles", zIndex = 420) %>% #this pane is always on top so that the circles can be clicked
     addCircles(group = "mainCircles",
                layerId = ~Source_airport, # critical, this id is assigned to the circles and then is contained in event$id when clicked
                lng = ~Longitude, lat = ~Latitude,
                stroke = F,
                fillColor = "white",
                #weight = 30,
                radius = size, # is there a better, non-linear scaling?
                fillOpacity = 0.6,
                label = ~paste0(Name, " (", Country, ") ", n, " routes"),
                labelOptions = labelOptions(textsize = "14px"),
                options = pathOptions(pane = "oldcircles"),
                highlightOptions = highlightOptions(bringToFront = FALSE, fillColor = "orange",  fillOpacity = 0.8) #bringToFront is false, so that overlapping circles can be clicked
   )
}

# function for countries polygons 
drawCountries <- function() {
    leafletProxy("map", data = countries) %>%
      addPolygons(group = "countries",
                  layerId = ~name,
                  label = ~name,
                  labelOptions = labelOptions(textsize = "14px",
                                              direction = "top",
                                              textOnly = T, 
                                              style = list("color" = "grey")),
                  stroke = FALSE,
                  fillColor = "black",
                  fillOpacity = 0.1,
                  highlightOptions = highlightOptions(fillColor = "white"))
    
  }

