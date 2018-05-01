
library(shiny)
library(leaflet)
library(rgdal)
library(foreign)

#blocks <- readOGR(dsn="/Users/ctmccahill/Documents/Research/Madison studies/Madison parking study/GIS",
#                  layer="block_accessibility")
#blocks <- readOGR("/Users/ctmccahill/Documents/Research/Madison studies/Madison parking study/GIS/block_accessibility.geojson",
#                  "OGRGeoJSON")
#tracts <- readOGR(dsn="web", layer="tract_accessibility")

tracts <- readOGR(dsn = "./../data/shape", layer = "cb_2017_us_cbsa_500k")

tracts@data <- within(tracts@data, pseudoData <- rnorm(nrow(tracts@data)))

shinyServer(function(input, output) {

  # Generate map
  output$map <- renderLeaflet({
    leaflet(tracts) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -89.400427, lat = 43.072536, zoom = 4) %>%
      addPolygons(
        stroke=TRUE, color = "black", weight = 1,
        fillOpacity=0.75,
        smoothFactor=1,
        fillColor= ~colorQuantile("Blues", pseudoData, n = 9)(pseudoData),
        layerId=tracts$GEOID
      )
  })

  # Function: show popup at the given location
  showPopup <- function(geoID, lat, lng) {
    selectedTract <- tracts[tracts$GEOID==geoID,]
    #popDens <- as.character(prettyNum(round(selectedTract$popDens,0),big.mark=","))
    pseudoData <- selectedTract$pseudoData
    content <- as.character(tagList(
      paste0("CBSA: ",selectedTract$NAME), tags$br(),
      paste0("Pseud-data: ",pseudoData)
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
      -0.00356*input$pseudoData
      +0.000705*input$unitSize
      -0.00114*input$parkPrice
      -0.0578*parkStreetNum,
      2)
    if (parkEst<0) parkEst<-0
    paste(as.character(parkEst)," vehicles per unit")
  })

})
