
library("shiny")
library("tidyverse")
library("leaflet")
library("rgdal")
library("foreign")

#blocks <- readOGR(dsn="/Users/ctmccahill/Documents/Research/Madison studies/Madison parking study/GIS",
#                  layer="block_accessibility")
#blocks <- readOGR("/Users/ctmccahill/Documents/Research/Madison studies/Madison parking study/GIS/block_accessibility.geojson",
#                  "OGRGeoJSON")
#tracts <- readOGR(dsn="web", layer="tract_accessibility")

tracts <- readOGR(dsn = "./../data/shape", layer = "cb_2017_us_cbsa_500k")
bar <- read.csv(file = "~/NHTS/data/hhpub.csv")
bar <- bar %>% group_by(HH_CBSA) %>% summarise(BIKE = mean(BIKE)) %>% mutate(HH_CBSA = as.character(HH_CBSA))
foo <- within(tracts@data, GEOID <- as.character(GEOID))

tracts@data <- left_join(foo,bar,by=c("GEOID" = "HH_CBSA"))

shinyServer(function(input, output) {

  # Generate map
  output$map <- renderLeaflet({
    leaflet(tracts) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -96.6, lat = 39.5, zoom = 5) %>%
      addPolygons(
        stroke=TRUE, color = "black", weight = 1,
        fillOpacity=0.75,
        smoothFactor=1,
        fillColor= ~colorQuantile("Blues", BIKE, n = 5)(BIKE),
        layerId=tracts$GEOID
      )
  })

  # Function: show popup at the given location
  showPopup <- function(geoID, lat, lng) {
    selectedTract <- tracts[tracts$GEOID==geoID,]
    #popDens <- as.character(prettyNum(round(selectedTract$popDens,0),big.mark=","))
    BIKE <- selectedTract$BIKE
    content <- as.character(tagList(
      paste0("CBSA: ",selectedTract$NAME), tags$br(),
      paste0("Mean BIKE: ",BIKE)
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content) # remove quotes for actual layerId
  }

  # When map is clicked, show a popup with info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if(is.null(event))
      return()
    isolate({
      showPopup(event$id, event$lat, event$lng)
    })

  })

  # Generate parking occupancy estimate
  output$data1 <- renderText({
    parkStreetNum <- ifelse(input$parkStreet==T,1,0)
    parkEst <- round(
      0.583
      -0.0000172*input$popDens
      -0.00356*input$BIKE
      +0.000705*input$unitSize
      -0.00114*input$parkPrice
      -0.0578*parkStreetNum,
      2)
    if (parkEst<0) parkEst<-0
    paste(as.character(parkEst)," vehicles per unit")
  })

})
