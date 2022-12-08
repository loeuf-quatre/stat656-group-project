library(leaflet)
library(shiny)

mp <- mainPanel(
  div(
    class = 'outer',
    tags$style(
      type = 'text/css', '.outer {position: fixed; 
      top: 0px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}'
    ),
    leafletOutput(
      "map", 
      height = "100%"
    )
  )
)

ap <- absolutePanel(
  draggable = TRUE, 
  width = 450, 
  top = 5, 
  left = 50,
  style = 'padding: 8px; line-height: 50px; background: #FFFFFF; opacity: 0.8; text-align: center',
  h1(
    "STAT 656 Group Project"
  ),
  h3(
    "Top 100 Domestic Energy Plants"
  ),
  img(
    src = "group-photo.png", 
    height = 125, 
    width = 400
  )
)

ui <- fluidPage(
  mp,
  ap
)