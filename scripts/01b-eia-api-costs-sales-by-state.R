###############################################################################
# Title: 01b-eia-api-costs-sales-by-state.R                                   #
# Description: Access EIA electric utility cost data by state via API         #
###############################################################################

# Housekeeping ---------------------------------------------------------------

library(cli) # Messages
library(dplyr) # Data wrangling
library(eia) # EIA API
library(glue) # String literals
library(janitor) # Data wrangling
library(keyring) # API key
library(knitr) # Display dataframes
library(leaflet) # Mapping
library(purrr) # Data wrangling
library(sf) # Mapping
library(tidyr) # Data wrangling
library(tigris) # Mapping

# API key stored in my Mac's Keychain app
api_eia <- key_get("EIA.gov")
eia_set_key(api_eia)

# Collect data ----------------------------------------------------------------

categories <- eia_cats()$childcategories

kable(categories, "simple")

#  category_id  name                                
# ------------  ------------------------------------
#            0  Electricity                         
#        40203  State Energy Data System (SEDS)     
#       714755  Petroleum                           
#       714804  Natural Gas                         
#       711224  Total Energy                        
#       717234  Coal                                
#       829714  Short-Term Energy Outlook           
#       964164  Annual Energy Outlook               
#      1292190  Crude Oil Imports                   
#      2123635  U.S. Electric System Operating Data 
#      2134384  International Energy Data           
#      2251604  CO2 Emissions                       
#      2631064  International Energy Outlook        
#      2889994  U.S. Nuclear Outages

electricity <- eia_cats(0)$childcategories

kable(electricity, "simple")

#  category_id  name                                                               
# ------------  -------------------------------------------------------------------
#            1  Net generation                                                     
#           35  Total consumption                                                  
#           32  Total consumption (Btu)                                            
#           36  Consumption for electricity generation                             
#           33  Consumption for electricity generation (Btu)                       
#           37  Consumption for useful thermal output                              
#           34  Consumption for useful thermal output (Btu)                        
#         1017  Plant level data                                                   
#           38  Retail sales of electricity                                        
#           39  Revenue from retail sales of electricity                           
#           40  Average retail price of electricity                                
#      1718389  Number of customer accounts                                        
#        41137  Fossil-fuel stocks for electricity generation                      
#        41138  Receipts of fossil fuels by electricity plants                     
#        41139  Receipts of fossil fuels by electricity plants (Btu)               
#        41140  Average cost of fossil fuels for electricity generation            
#        41141  Average cost of fossil fuels for electricity generation (per Btu)  
#        41142  Quality of fossil fuels in electricity generation : sulfur content 
#        41143  Quality of fossil fuels in electricity generation : ash content

vars <- lst(38, 39, 40, 1718389)

vars <- vars %>%
  map(
    ~ eia_cats(.x)$childcategories
  ) %>%
  bind_rows() %>%
  group_split(
    category_id
  ) %>%
  map(
    ~ eia_cats(.x)
  ) %>%
  map(
    ~ cbind(.x[[1]], .x[[2]])
  ) %>%
  map(
    ~ clean_names(.x)
  ) %>%
  map(
    ~ splitstackshape::cSplit(.x, "name_2", ":") # Split plant metadata field
  ) %>%
  map(
    ~ filter(.x, f == "M")
  ) %>%
  map(
    ~ select(
      .x,
      series_id,
      state = name_2_2,
      units,
      variable = name_2_1,
      sector = name_2_3,
      frequency = name_2_4,
      updated
    )
  ) %>%
  bind_rows()
  
# UDF to track progress
get_series_costs <- function(x) {
  print(x$series_id)
  eia_series(x$series_id, start = 201201) # 10y data vintage
}

costs <- vars %>%
  mutate(
    chunk_id = ceiling(seq_along(series_id) / 100)
  ) %>%
  group_split(
    chunk_id
  ) %>%
  map(
    ~ get_series_costs(.x)
  ) %>%
  bind_rows() %>%
  unnest(
    data
  ) %>%
  select(
    series_id,
    geography,
    ds = date,
    value
  )

costs <- left_join(vars, costs, by = "series_id")

costs <- costs %>%
  relocate(
    updated,
    .after = last_col()
  ) %>%
  arrange(
    series_id,
    ds
  ) %>%
  pivot_wider(
    id_cols = c(state, ds, sector), 
    names_from = c(variable, units), 
    values_from = value
  ) %>%
  clean_names()

saveRDS(costs, "./output/electricity-costs.RData")