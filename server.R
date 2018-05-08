
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

hh.csv <- read.csv(file = "./data/hhpub.csv", stringsAsFactors = FALSE)

hh.csv <- within(hh.csv,{
    HHSTFIPS <- as.numeric(HHSTFIPS)
    HH_CBSA <- as.numeric(HH_CBSA)
})


shinyServer(function(input, output) {

    layer <- "cb_2017_us_state_500k"
    hh.df <- hh.csv %>% group_by(HHSTFIPS) %>% summarise(n = n(), BIKE = 100*sum(BIKE %in% 1:3, na.rm = TRUE)/sum(!is.na(BIKE)), WALK = 100*sum(WALK %in% 1:3, na.rm = TRUE)/sum(!is.na(WALK)))
    ogr <- readOGR(dsn = "./data/shape", layer = layer)
    ogr@data <- within(ogr@data,{
        STATEFP <- as.numeric(as.character(STATEFP))
        GEOID <- as.numeric(as.character(GEOID))
    })
    ogr@data <- left_join(ogr@data,hh.df,by=c("STATEFP" = "HHSTFIPS"))

    output$map <- renderLeaflet({

        if(input$colorBy == "cyclists"){

            dataVec <- ogr@data$BIKE

        }else if(input$colorBy == "peds"){

            dataVec <- ogr@data$WALK
        }else{
            dataVec <- NULL
        }

        pal <- colorNumeric(
            palette = "YlOrRd",
            domain = dataVec)

        leaflet(ogr) %>%
            addProviderTiles("CartoDB.Positron") %>%
            setView(lng = -96.6, lat = 39.5, zoom = 5) %>%
            addPolygons(
                stroke=TRUE, color = "black", weight = 1,
                fillOpacity=0.75,
                smoothFactor=1,
                fillColor= ~pal(dataVec),#~colorQuantile(pal, dataVec, n = 9)(dataVec),
                layerId=ogr$GEOID
            ) %>%
            addLegend("bottomright", pal = pal, values = ~dataVec, title = "")
   # title = "Est. GDP (2010)",
   # labFormat = labelFormat(prefix = "$"),
   # opacity = 1)
    })

    showPopup <- function(geoID, lat, lng) {
        #browser()
        #geoID <- as.numeric(geoID)
        #if( input$GEO == "state" ){
            #selectedTract <- ogr[as.numeric(ogr@data$GEOID)==geoID,]
        #}else if (input$GEO == "MSA" ){
            selectedTract <- ogr[ogr@data$GEOID == as.character(geoID),]
        #}

        #popDens <- as.character(prettyNum(round(selectedTract$popDens,0),big.mark=","))

        content <- as.character(tagList(
#            paste0("geoID = ", geoID), tags$br(),
#            paste0("GEOID = ", selectedTract$GEOID), tags$br(),
            paste0("Name: ",selectedTract$NAME), tags$br(),
            paste0("Number of households sampled: ", prettyNum(selectedTract$n,big.mark = ",")), tags$br(),
            paste0("Pedestrians: ", round(selectedTract$WALK,1), "%"), tags$br(),
            paste0("Cyclists: ", round(selectedTract$BIKE,1), "%")
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

})
