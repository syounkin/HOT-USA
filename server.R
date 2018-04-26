library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
#set.seed(100)
#zipdata <- allzips[sample.int(nrow(allzips), 10000),]
# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
#zipdata <- zipdata[order(zipdata$centile),]

zipdata <- allzips

countyData <- allzips %>% group_by(state.y, county) %>% summarise(latitude = mean(latitude), longitude = mean(longitude), population = sum(adultpop), nzip = n()) %>% mutate(countyAndState = paste0(county, ", ", state.y))

countyData <- cbind(countyData, walkTime = rnorm(n = nrow(countyData), mean = 10), cycleTime = rnorm(n = nrow(countyData), mean = 10))

NHTS.list <- readRDS(file = "/Users/syounkin/NHTS/R/data/NHTS.list.rds")


function(input, output, session) {

  ## Interactive Map ###########################################

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })

  # A reactive expression that returns the set of zips that are
  # in bounds right now
  countiesInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(countyData[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)

    subset(countyData,
      latitude >= latRng[1] & latitude <= latRng[2] &
        longitude >= lngRng[1] & longitude <= lngRng[2])
  })

  # Precalculate the breaks we'll need for the two histograms
  #centileBreaks <- hist(plot = FALSE, allzips$centile, breaks = 20)$breaks

  output$histCentile <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(countiesInBounds()) == 0)
      return(NULL)

    hist(countiesInBounds()$walkTime,
      #breaks = centileBreaks,
      main = "Average Walk Time",
      xlab = "Walk Time",
      xlim = range(countyData$walkTime),
      col = '#00DD00',
      border = 'white')
  })
  output$histCentile2 <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(countiesInBounds()) == 0)
      return(NULL)

    hist(countiesInBounds()$cycleTime,
      #breaks = centileBreaks,
      main = "Average Cycle Time",
      xlab = "Cycle Time",
      xlim = range(countyData$cycleTime),
      col = '#00FF00',
      border = 'white')
  })

  output$scatterCollegeIncome <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(countiesInBounds()) == 0)
      return(NULL)

    print(xyplot(population ~ nzip, data = countiesInBounds(), xlim = range(countyData$nzip), ylim = range(countyData$population)))
  })

  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    colorBy <- input$color
    sizeBy <- input$size

    #if (colorBy == "superzip") {
      # Color and palette are treated specially in the "superzip" case, because
      # the values are categorical instead of continuous.
     # colorData <- ifelse(zipdata$centile >= (100 - input$threshold), "yes", "no")
      #pal <- colorFactor("viridis", colorData)
    #} else {
      colorData <- countyData[[colorBy]]
      pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
    #}

#    if (sizeBy == "superzip") {
      # Radius is treated specially in the "superzip" case.
#      radius <- ifelse(zipdata$centile >= (100 - input$threshold), 30000, 3000)
#    } else {
    #radius <- countyData[["population"]] / max(countyData[["population"]]) * 5e6
    #radius <- ifelse(radius > 1e4, 1e4, radius)
    radius <- 1e4
#    }

    leafletProxy("map", data = countyData) %>%
      clearShapes() %>%
      addCircles(~longitude, ~latitude, radius=radius, layerId=~countyAndState,
        stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
      addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
        layerId="colorLegend")
  })

  # Show a popup at the given location
  showZipcodePopup <- function(county, lat, lng) {
    selectedCounty <- subset(countyData, county == county) #allzi[allzips$zipcode == zipcode,]
    content <- as.character(tagList(
      tags$h4("County:", county))#,
      #tags$h4(HTML(sprintf("Population: %s",
      #  selectedCounty$population #, selectedZip$state.x, selectedZip$zipcode
      #))), tags$br()
#      sprintf("Median household income: %s", dollar(selectedZip$income * 1000)), tags$br(),
#      sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$college)), tags$br(),
#      sprintf("Adult population: %s", selectedZip$adultpop)
                                        #))
      )
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = county)
  }

  # When map is clicked, show a popup with county info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()

    isolate({
      showZipcodePopup(event$id, event$lat, event$lng)
    })
  })


  ## Data Explorer ###########################################

##   observe({
##     cities <- if (is.null(input$states)) character(0) else {
##       filter(cleantable, State %in% input$states) %>%
##         `$`('City') %>%
##         unique() %>%
##         sort()
##     }
##     stillSelected <- isolate(input$cities[input$cities %in% cities])
##     updateSelectInput(session, "cities", choices = cities,
##       selected = stillSelected)
##   })

##   observe({
##     zipcodes <- if (is.null(input$states)) character(0) else {
##       cleantable %>%
##         filter(State %in% input$states,
##           is.null(input$cities) | City %in% input$cities) %>%
##         `$`('Zipcode') %>%
##         unique() %>%
##         sort()
##     }
##     stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
##     updateSelectInput(session, "zipcodes", choices = zipcodes,
##       selected = stillSelected)
##   })

##   observe({
##     if (is.null(input$goto))
##       return()
##     isolate({
##       map <- leafletProxy("map")
##       map %>% clearPopups()
##       dist <- 0.5
##       zip <- input$goto$zip
##       lat <- input$goto$lat
##       lng <- input$goto$lng
##       showZipcodePopup(zip, lat, lng)
##       map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
##     })
##   })

##   output$ziptable <- DT::renderDataTable({
##     df <- cleantable %>%
##       filter(
##         Score >= input$minScore,
##         Score <= input$maxScore,
##         is.null(input$states) | State %in% input$states,
##         is.null(input$cities) | City %in% input$cities,
##         is.null(input$zipcodes) | Zipcode %in% input$zipcodes
##       ) %>%
##       mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
##     action <- DT::dataTableAjax(session, df)

##     DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
##   })
}
