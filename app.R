# leaflet map vizualisation of the openflights.org data
# includes airports, airlines and routes (no schedules)
# 

##check if required packages are there and suggest install if not
packages <- c("shiny", "tidyverse", "devtools", "leaflet", "leaflet.extras", "colorspace")
lapply(packages, function(x) if(!require(x, character.only = TRUE)) install.packages(x))

# leaflet has to be devel version
if (packageVersion("leaflet") < "2.0.1.9000") {
  devtools::install_github("rstudio/leaflet")
}
# 
## load required libraries
library(shiny)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(data.table)

# the data processing steps are in the dataprep.R file, which can be executed when new data becomes available
load("data/airports.Rdata")
load("data/airlines.Rdata")
load("data/routes.Rdata")
load("data/countries.Rdata")
load("data/countries.bounds.Rdata")

routes <- data.table(routes) # for faster filtering
airports <- airports %>% filter(!is.na(IATA))

source("global.R", local = TRUE) # contains the functions routesx, airlinesx, drawroutesAirportClear and drawroutesAirportKeep
source("drawbasemap.R", local = TRUE) # the function to draw the base map, which is the tiles and the airports

# data summaries needed for labels etc.
########################################################################################################################
# this summary is used for addCircles in the main map, giving number of destinations per airport (n) for the circle sizes
# this is a tibble, because of summarize?
airports.source.summary <- routes %>% 
                            group_by(Source_airport) %>% 
                            summarise(n = n()) %>% 
                            arrange(desc(n)) %>%
                            left_join(airports, by = c("Source_airport" = "IATA")) %>%
                            filter(!is.na(Airport_ID))


# summary of destination airports per source airport
# this is a data.table, because routes is a data.table
airports.info.destination <- routes %>% 
                              left_join(airports, by = c("Destination_airport" = "IATA"))
                              
# summary for airlines
airlines.routes.summary <- airports.info.destination[, c("Airline","Source_airport" ,"City", "Country")] %>% 
                              left_join(airlines[, c("Name", "IATA")], by = c("Airline" = "IATA")) %>%
                              filter(!is.na(City))

# top 50 airlines by number of routes
airlines$Airline_ID <- as.character(airlines$Airline_ID)

top50airlines <- airlines %>% 
                  left_join(routes, by = "Airline_ID") %>% 
                  group_by(Name, IATA, ICAO) %>% 
                  summarise(n = n()) %>% arrange(desc(n)) %>% head(50)
top50List <- top50airlines$IATA

# UI
##############################################################################################
ui <- bootstrapPage(
  
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  
  absolutePanel(top = 15, right = 5, width = "15%",
                htmlOutput("airportSummary"),
                tags$head(tags$style("#airportSummary{color: white; font-size: 15px;}" # way to format html output
                         )
                )
  ),
  
  absolutePanel(top = 15, left = 5, width = "15%",
                h5(selectizeInput("airline", "Select airline (top 50 only)", 
                                  choices = setNames(object = top50airlines$IATA, nm = top50airlines$Name), # this is a named character vector, the names are visible to the user and the values (IATA) are returned
                                  multiple = T),
                   # selectizeInput("airport", "Select airport (start typing)", 
                   #                choices = setNames(object = airports$IATA, nm = airports$Name),
                   #                multiple = T),
                   style = "color:grey;")
  ),
  
  absolutePanel(bottom = 15, left = 5, width = "15%",
                h5(radioButtons("radio", label = "Routes selection mode",
                             choices = list("Keep routes" = 1, "Clear routes" = 2), selected = 2),
                   actionButton("clearall", "Clear all routes"),
                             style = "color:grey;")
                )
)

# server
###################################################################################################
server <- function(input, output, session) {
  
 
   # this stops the app when user closes browser window
  session$onSessionEnded(function() {
    stopApp()
  })
  
  output$map <- renderLeaflet({
    
    drawbasemap()
  })
  
  # observers ##############################
  # draw main circles, eventually this function can be extended to change the sizes according to, for example, airline selected
  observe({
    #if(is.null(input$airline))
    drawMainCircles(size = ~n*100)
    drawCountries()
  })
  
  # the clickdata reactive stores clicked airports, I am using it like this because I want to set it to NULL whenever needed (e.g. when clearall or when airline is selcted)
  clickdata <- reactiveValues(click = NULL)
  observeEvent(input$map_shape_click, {
    clickdata$click <- input$map_shape_click
    print(clickdata$click)
  })
  # if clearall is pressed, set clickdata$click to NULL
  observeEvent(input$clearall, {
    clickdata$click <- NULL
    leafletProxy("map") %>%
      flyToBounds(lng1 = -180, lat1 = -85, lng2 = 180, lat2 = 85)
    print(clickdata$click)
  })
  
  # observers to draw routes when airport is clicked or airline is selected
  observe({
    event <- clickdata$click
    if(is.null(event))
      return()
    if(event$group == "countries")
      leafletProxy("map", data = countries.bounds[countries.bounds$country == event$id, ]) %>%
        flyToBounds(lng1 = ~lng1, lat1 = ~lat1, lng2 = ~lng2, lat2 = ~lat2)
    
    isolate({
      ifelse(input$radio == 2,
        try(drawroutesAirportClear(event$id, filters = input$airline)),
        try(drawroutesAirportKeep(x = event$id, filters = input$airline))
      )
    })
  })
  
  observe({
    airlineEvent <- input$airline
    clickdata$click <- NULL
    if(is.null(airlineEvent))
      return(leafletProxy("map") %>% clearGroup("routes3"))
    
    isolate({
      drawroutesAirlines(input$airline)
    })
  })
  
  
  #observer to print clicked airport or airline info, renderText is an observer already
  #observe({
  output$airportSummary <- renderText({
     event <- clickdata$click
     
     if(!is.null(input$airline))
        return(showAirlineInfo(x = last(input$airline), filters = event$id))
     if(is.null(event))
       return(paste0("Click on a country, on an airport, or select an airline to see the routes"))
     if(event$group == "mainCircles")
        return(showAirportInfo(event$id))
     if(event$group == "countries")
       return(paste0(event$id)) 
    })
  #})
  
  #observer to clear all routes and airlines selections when the clear button is clicked
  observe({
    input$clearall
    updateSelectizeInput(session, "airline", selected = "")
    leafletProxy("map") %>%
      clearGroup("routes1") %>%
      clearGroup("routes2") %>%
      clearGroup("routes3")
  })
  
  
}#server


shinyApp(ui = ui, server = server)