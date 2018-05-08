
library(shiny)
library(leaflet)
library(rgdal)
library(foreign)

## vars <- c(
##   "Is SuperZIP?" = "superzip",
##   "Centile score" = "centile",
##   "College education" = "college",
##   "Median income" = "income",
##   "Population" = "adultpop"
## )


fluidPage(




  # Application title
#  tags$h3("HOT USA"),
                                        #  tags$p("Initiative for Health-Oriented Transportation at UW-Madison"),

    sidebarLayout(
        sidebarPanel(
            selectInput("colorBy", "Select:",
                c("Pedestrians" = "peds",
                  "Cyclists" = "cyclists")), width = 1),
    #tableOutput("data")



      mainPanel(
#      tags$strong("Travel Activity Map"),
#      tags$br(),
#      tags$i("Click on the map for summary statistics."),
      leafletOutput("map", width = wd <- 1500, height = wd/phi)
      )
  )
)


 ##  # Sidebar with input
 ## sidebarLayout(
 ##   sidebarPanel(

 ##       selectInput("color", "Color", vars)
 ##       ))

##  Sidebar with input
  ## sidebarLayout(
  ##   sidebarPanel(
  ##     sliderInput("popDens",
  ##                 label="Population density (per sq. mi.)*",
  ##                 value=4000,
  ##                 min=100,
  ##                 max=20000,
  ##                 step=100,
  ##                 width=300))),
      # Show a map

##      tags$br(),
      ## tags$p("Foo",
      ##        tags$a(href="https://doi.org/10.3141/2651-08","Transportation Research Record 2651-08"),
      ##        " (refer to Model 4a). For more information, contact Chris McCahill at",
      ##        tags$a(href="mailto:syounkin@wisc.edu","syounkin@wisc.edu.")),
      ## tags$br(),
      ## tags$i("* Indicates significant variable")
   # )

  #)




  ##     sliderInput("accessWalk",
  ##                 label="Walking accessibility (0 to 100)*",
  ##                 value=50,
  ##                 min=0,
  ##                 max=100,
  ##                 step=1,
  ##                 width=300),

  ##     sliderInput("unitSize",
  ##                 label = "Average unit size (sq. ft.)*",
  ##                 value = 700,
  ##                 min=200,
  ##                 max=1500,
  ##                 step=10,
  ##                 width=300),

#      sliderInput("roomOccRate",
#                  label = "Average bedrooms per unit",
#                  value = 1,
#                  min=1,
#                  max=4,
#                  step=0.1,
#                  width=300),

    ##   sliderInput("parkPrice",
    ##               label = "Parking price per month*",
    ##               value = 0,
    ##               min=0,
    ##               max=200,
    ##               step=5,
    ##               width=300),

    ##   selectInput("parkStreet",
    ##               label="On-street parking",
    ##               choices=c("No","Yes"),
    ##               selected="Yes",
    ##               multiple=FALSE,
    ##               width=300),

    ##   tags$strong("Expected parking occupancy:"),
    ##   textOutput("data1")

    ## ),

