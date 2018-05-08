
library(shiny)
library(leaflet)
library(rgdal)
library(foreign)

fluidPage(

    tags$h3("HOT USA"),
    tags$p("Initiative for Health-Oriented Transportation at UW-Madison"),

    sidebarLayout(
        sidebarPanel(
            selectInput("colorBy", "Select:",
                        c("Pedestrians" = "peds",
                          "Cyclists" = "cyclists")), width = 1),

        mainPanel(
            tags$strong("Travel Activity Map"),
            tags$br(),
            tags$i("Click on the map for summary statistics."),
            leafletOutput("map", width = wd <- 1500, height = wd/phi)
        )
    )
)
