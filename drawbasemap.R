# function to draw base map, the size of the circles corresponds to the number of outgoing routes from that airport

drawbasemap <- function() {
  airports.source.summary %>%
  leaflet(options = leafletOptions(zoomControl = FALSE,  # these options are just for fun and to test if they are working in R
                                   minZoom = 2, 
                                   maxZoom = 10, 
                                   zoomDelta = 0.5, 
                                   zoomSnap = 0.5, 
                                   worldCopyJump = T)) %>% 
  
  addProviderTiles("CartoDB.DarkMatter") %>%
  clearGroup("mainCircles") %>%
  addMapPane("oldcircles", zIndex = 420) %>% #these pane is always on top so that the circles can be clicked
  addCircles(group = "mainCircles",
             layerId = ~Source_airport, # critical, this id is assigned to the circles and then is contained in event$id when clicked
             lng = ~Longitude, lat = ~Latitude,
             stroke = F,
             fillColor = "white",
             #weight = 30,
             radius = ~n*100, # is there a better, non-linear scaling?
             fillOpacity = 0.6,
             label = ~paste0(Name, " (", Country, ") ", n, " flights"),
             options = pathOptions(pane = "oldcircles")
  )
}