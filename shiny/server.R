library(dplyr)
library(leaflet)
library(sf)
library(shiny)

ng <- readRDS("plant-generation-tidy.RData")

top10 <- ng %>%
  group_by(
    plant
  ) %>%
  mutate(
    total_output = sum(output)
  ) %>%
  ungroup() %>%
  # group_by(
  #   state
  # ) %>%
  mutate(
    rk = dense_rank(-total_output)
  ) %>%
  filter(
    rk <= 100 &
    output > 0
  ) %>%
  group_by(
    state,
    plant,
    lat,
    lon
  ) %>%
  summarize(
    energy_source = paste0(unique(energy_source), collapse = " | "),
    prime_mover = paste0(unique(prime_mover), collapse = " | "),
    output = max(output),
    .groups = "drop"
  )

tx <- top10

coords <- tx %>%
  mutate(
    label = paste(plant, paste0("Source: ", energy_source), paste0("Prime Mover: ", prime_mover), sep = "<br>"),
    label = lapply(label, htmltools::HTML)
  ) %>%
  group_by(
    label,
    lat,
    lon
  ) %>%
  summarize(
    output = sum(output),
    .groups = "drop"
  ) %>%
  st_as_sf(
    coords = c("lon", "lat")
  )

server <- function(input, output) {
  
  output$map <- renderLeaflet({
    leaflet(coords) %>%
      addProviderTiles(
        "Esri.WorldImagery"
      ) %>%
    addProviderTiles(
      "CartoDB.PositronOnlyLabels"
    ) %>%
    setView(
      lat = 39,
      lng = -99.9,
      zoom = 5
    ) %>%
    addCircleMarkers(
      data = coords,
      clusterOptions = markerClusterOptions(),
      popup = ~ label,
      popupOptions = popupOptions(
        noHide = TRUE, 
        direction = 'top', 
        textOnly = FALSE,
        opacity = .5,
        style = list(
          'color' = 'black',
          'font-family' = 'sans-serif',
          'font-size' = '12px',
          'background-color' = '#FBF9E6',
          'border-color' = 'rgba(0, 0, 0, .5)'
        )
      )
    )
  })
  
}