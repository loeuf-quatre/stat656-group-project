# Data viz --------------------------------------------------------------------

# Output by plant over time -------------------------------

top5 <- generation %>%
  filter(
    geography == "USA-UT"
  ) %>%
  group_by(
    plant
  ) %>%
  mutate(
    total_output = sum(output)
  ) %>%
  ungroup() %>%
  mutate(
    rk = dense_rank(-total_output)
  ) %>%
  filter(
    rk <= 10 &
    output > 0
  )

ggplot() +
  geom_point(
    data = top5,
    aes(
      x = ds,
      y = output,
      color = paste(energy_source, prime_mover, sep = " | ")
    )
  ) +
  facet_wrap(
    ~ plant,
    scales = "free_y"
  ) +
  labs(
    x = "Date",
    y = "Output",
    color = "Energy Source | Prime Mover"
  )

# Map coordinates -----------------------------------------

coords <- top5 %>%
  mutate(
    label = paste(plant, energy_source, prime_mover, sep = "<br>"),
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

leaflet(coords) %>% 
  addProviderTiles("Esri.WorldImagery") %>%
  addProviderTiles("CartoDB.PositronOnlyLabels") %>%
  addLabelOnlyMarkers(
    data = coords, 
    label = ~ label,
    labelOptions = labelOptions(
      noHide = TRUE, 
      direction = 'top', 
      textOnly = FALSE,
      style = list(
        'color' = 'black',
        'font-family' = 'sans-serif',
        'font-size' = '12px',
        'background-color' = '#FBF9E6',
        'border-color' = 'rgba(0, 0, 0, .5)'
      )
    )
  )
  