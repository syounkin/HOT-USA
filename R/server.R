
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

hh.csv <- read.csv(file = "~/NHTS/data/hhpub.csv", stringsAsFactors = FALSE)

hh.csv <- within(hh.csv,{
    HHSTFIPS <- as.numeric(HHSTFIPS)
    HH_CBSA <- as.numeric(HH_CBSA)
})


shinyServer(function(input, output) {



  # Generate map
    output$map <- renderLeaflet({
            GEO <- input$GEO
#GEO <- "state"

if(GEO == "state"){
    layer <- "cb_2017_us_state_500k"
    hh.df <- hh.csv %>% group_by(HHSTFIPS) %>% summarise(n = n(), BIKE = sum(BIKE == 5, na.rm = TRUE))
    ogr <- readOGR(dsn = "./../data/shape", layer = layer)
    ogr@data <- within(ogr@data, STATEFP <- as.numeric(as.character(STATEFP)))
    ogr@data <- left_join(ogr@data,hh.df,by=c("STATEFP" = "HHSTFIPS"))
}else if(GEO == "MSA"){
    layer <- "cb_2017_us_cbsa_500k"
    hh.df <- hh.csv %>% group_by(HH_CBSA) %>% summarise(n = n(), BIKE = sum(BIKE == 5, na.rm = TRUE))
    ogr <- readOGR(dsn = "./../data/shape", layer = layer)
    ogr@data <- within(ogr@data, CBSAFP <- as.numeric(as.character(CBSAFP)))
    ogr@data <- left_join(ogr@data,hh.df,by=c("CBSAFP" = "HH_CBSA"))
}


    leaflet(ogr) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -96.6, lat = 39.5, zoom = 5) %>%
      addPolygons(
        stroke=TRUE, color = "black", weight = 1,
        fillOpacity=0.75,
        smoothFactor=1,
        fillColor= ~colorQuantile("Blues", n, n = 9)(BIKE),
        layerId=ogr$GEOID
      )
  })

  # Function: show popup at the given location
  showPopup <- function(geoID, lat, lng) {
    selectedTract <- ogr[ogr$GEOID==geoID,]
    #popDens <- as.character(prettyNum(round(selectedTract$popDens,0),big.mark=","))
    n <- selectedTract$n
    content <- as.character(tagList(
      paste0("Name: ",selectedTract$NAME), tags$br(),
      paste0("Number of households sampled ", n)
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
