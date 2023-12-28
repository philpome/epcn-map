#epcn-map

library(shiny)
library(shinyWidgets)
library(leaflet)
library(htmltools)
library(DT)
library(dplyr)

loc_map <- read.csv("data/loc_map.csv",header=TRUE, check.names = FALSE)

ui <- fluidPage(
  titlePanel("EPCN Working Directory"),
  mainPanel(leafletOutput('mymap'),
            dataTableOutput('epcn_table'))
)

server <- function(input, output) {
  output$mymap <- renderLeaflet({
    leaflet(data=loc_map) %>% 
      setView(lng = -11.25000, lat = 17.97873, zoom = 1) %>% 
      addTiles() %>%
      addMarkers(~Longitude, ~Latitude, clusterOptions = markerClusterOptions(),
                 label = ~htmlEscape(Affiliation))
  })
  
  output$epcn_table <- renderDataTable({
    if(isTruthy(input$mymap_bounds)) {
      bounds = input$mymap_bounds
      loc_map %>% filter(
        between(Longitude, bounds$west, bounds$east),
        between(Latitude, bounds$south, bounds$north)
      )
    } else
    loc_map
#    colnames = c("Country", "City",
#                 "Affiliation", "Exceptional species worked on", 
#                 "Working areas", "Other capacity", 
#                 "Collaboration interests", "Needs")
#    filter = list(position = 'top', clear = FALSE)
#    options = list(
#      search = list(regex = TRUE),
#      pageLength = 5,
#      columnDefs = list(list(visible=FALSE, targets=c(8,9,10)))
#    )
  })
}

shinyApp(ui = ui, server = server)