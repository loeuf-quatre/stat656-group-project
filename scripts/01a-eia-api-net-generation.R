###############################################################################
# Title: 01a-eia-api-net-generation.R                                         #
# Description: Access EIA generation data via API                             #
###############################################################################

# Housekeeping ---------------------------------------------------------------

library(cli) # Messages
library(dplyr) # Data wrangling
library(eia) # EIA API
library(glue) # String literals
library(janitor) # Data wrangling
library(keyring) # API key
library(knitr) # Display dataframes
library(purrr) # Data wrangling
library(tidyr) # Data wrangling

# API key stored in Mac's Keychain app
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

states <- eia_cats(1017)$childcategories

kable(head(states), "simple")

#  category_id  name       
# ------------  -----------
#       902931  Alabama    
#       902932  Alaska     
#       902933  Arizona    
#       902934  Arkansas   
#       902935  California 
#       902936  Colorado

state_plants <- states %>%
  group_split(
    category_id
  ) %>%
  map(
    ~ eia_cats(.x$category_id)
  ) %>%
  map(
    ~ cbind(.x[[1]], .x[[2]])
  ) %>%
  map(
    ~ clean_names(.x)
  ) %>%
  bind_rows() %>%
  select(
    state_id = parent_category_id,
    state = name,
    plant_id = category_id_2,
    plant = name_2
  )

kable(head(state_plants, 10), "simple")

# state_id   state      plant_id  plant                       
# ---------  --------  ---------  ----------------------------
# 1017       Alabama        1024  (2) Bankhead Dam (2)        
# 1017       Alabama        1025  (3) Barry (3)               
# 1017       Alabama        1026  (4) Walter Bouldin Dam (4)  
# 1017       Alabama        1027  (7) Gadsden (7)             
# 1017       Alabama        1028  (8) Gorgas (8)              
# 1017       Alabama        1030  (10) Greene County (10)     
# 1017       Alabama        1031  (11) H Neely Henry Dam (11) 
# 1017       Alabama        1032  (12) Holt Dam (12)          
# 1017       Alabama        1033  (13) Jordan Dam (13)        
# 1017       Alabama        1034  (14) Logan Martin Dam (14)

"
It takes some time (~12h) to collect all the series IDs nested within 
plants given the API throttling. We'll loop through the plant IDs while 
checking for the count of API calls made within the last 60m. The loop 
will pause if it exceeds 900 (buffer for the hourly 1000 call limit)
"

state_plants <- group_split(state_plants, plant_id)

plant_series <- list()
calls <- data.frame(query_ts = Sys.time())

for (i in 1:length(state_plants)) {
  
  cli::cli_progress_message(
    glue(
      paste0(
        "{i} of {length(state_plants)}:",
        " {round(Sys.time() - calls$query_ts[1], 2)}"
      )
    )
  )
  
  trailing60m <- filter(calls, query_ts > (Sys.time() - 3600))
  
  while(nrow(trailing60m) > 900) {
    cli::cli_progress_message(
      paste0(
        "Cooling off to avert API error ",
        "{i} of {length(state_plants)}:",
        " {round(Sys.time() - calls$query_ts[1], 2)}"
      )
    )
    Sys.sleep(60)
    trailing60m <- filter(calls, query_ts > (Sys.time() - 3600))
  }
  
  plant_series[[i]] <- tryCatch(
    {
      eia_cats(state_plants[[i]]$plant_id)
    }, 
    error = function(e) NA
  )
  
  calls <- add_row(calls, query_ts = Sys.time())
  
}

plant_series <- plant_series %>%
  map(
    ~ cbind(.x[[1]], .x[[2]]) # Combine lists
  ) %>%
  map(
    ~ clean_names(.x)
  ) %>%
  map(
    ~ splitstackshape::cSplit(.x, "name_2", ":") # Split plant metadata field
  ) %>%
  map(
    ~ filter(
        .x,
        f == "M" & # Monthly reporting
        name_2_1 == "Net generation" & # Power generation
        !name_2_4 %in% "all primemovers" # No aggregation
    )
  ) %>%
  map(
    ~ select(
        .x,
        series_id,
        plant = name,
        units,
        energy_type = name_2_1,
        energy_source = name_2_3,
        prime_mover = name_2_4,
        frequency = name_2_5,
        updated
      )
  ) %>%
  bind_rows()

kable(head(plant_series, 10), "simple")

# series_id                   plant                        units           energy_type      energy_source                prime_mover                              frequency   updated               
# --------------------------  ---------------------------  --------------  ---------------  ---------------------------  ---------------------------------------  ----------  ----------------------
# ELEC.PLANT.GEN.2-WAT-HY.M   (2) Bankhead Dam (2)         megawatthours   Net generation   conventional hydroelectric   hydraulic turbine                        monthly     23-SEP-22 12.11.30 PM 
# ELEC.PLANT.GEN.3-BIT-ST.M   (3) Barry (3)                megawatthours   Net generation   bituminous coal              steam turbine                            monthly     23-SEP-22 12.11.30 PM 
# ELEC.PLANT.GEN.3-NG-CA.M    (3) Barry (3)                megawatthours   Net generation   natural gas                  combined-cycle - steam part              monthly     23-SEP-22 12.11.30 PM 
# ELEC.PLANT.GEN.3-NG-CT.M    (3) Barry (3)                megawatthours   Net generation   natural gas                  combined-cycle combustion turbine part   monthly     23-SEP-22 12.11.30 PM 
# ELEC.PLANT.GEN.3-NG-ST.M    (3) Barry (3)                megawatthours   Net generation   natural gas                  steam turbine                            monthly     23-SEP-22 12.11.30 PM 
# ELEC.PLANT.GEN.3-SUB-ST.M   (3) Barry (3)                megawatthours   Net generation   subbituminous coal           steam turbine                            monthly     23-SEP-22 12.11.30 PM 
# ELEC.PLANT.GEN.4-WAT-HY.M   (4) Walter Bouldin Dam (4)   megawatthours   Net generation   conventional hydroelectric   hydraulic turbine                        monthly     23-SEP-22 12.11.30 PM 
# ELEC.PLANT.GEN.7-AB-ST.M    (7) Gadsden (7)              megawatthours   Net generation   agricultural by-products     steam turbine                            monthly     06-MAR-17 04.49.40 PM 
# ELEC.PLANT.GEN.7-BIT-ST.M   (7) Gadsden (7)              megawatthours   Net generation   bituminous coal              steam turbine                            monthly     23-MAR-20 06.31.14 PM 
# ELEC.PLANT.GEN.7-DFO-ST.M   (7) Gadsden (7)              megawatthours   Net generation   distillate fuel oil          steam turbine                            monthly     06-MAR-17 04.49.40 PM

"
We can pass a vector of series IDs in as one API call, but there appears to be 
a limit in the number of rows it can return. Split into 100 length chunks to
stay below threshold
"

# UDF to track progress
get_series_generation <- function(x) {
  print(unique(x$chunk_id))
  eia_series(x$series_id, start = 201201) # 10y data vintage
}

generation <- plant_series %>%
  mutate(
    chunk_id = ceiling(seq_along(series_id) / 100)
  ) %>%
  group_split(
    chunk_id
  ) %>%
  map(
    ~ get_series_generation(.x)
  ) %>%
  bind_rows() %>%
  unnest(
    data
  ) %>%
  select(
    series_id,
    lat,
    lon,
    geography,
    ds = date,
    output = value
  )

generation <- left_join(plant_series, generation, by = "series_id")

generation <- generation %>%
  relocate(
    updated,
    .after = last_col()
  ) %>%
  arrange(
    series_id,
    ds
  )

kable(head(generation, 10), "simple")

# series_id                   plant                units           energy_type      energy_source         prime_mover                                   frequency   lat         lon           geography   ds             output  updated               
# --------------------------  -------------------  --------------  ---------------  --------------------  --------------------------------------------  ----------  ----------  ------------  ----------  -----------  --------  ----------------------
# ELEC.PLANT.GEN.1-DFO-IC.M   (1) Sand Point (1)   megawatthours   Net generation   distillate fuel oil   internal combustion (diesel, piston) engine   monthly     55.339722   -160.497222   USA-AK      2019-01-01    195.676  23-SEP-22 12.11.30 PM 
# ELEC.PLANT.GEN.1-DFO-IC.M   (1) Sand Point (1)   megawatthours   Net generation   distillate fuel oil   internal combustion (diesel, piston) engine   monthly     55.339722   -160.497222   USA-AK      2019-02-01    217.498  23-SEP-22 12.11.30 PM 
# ELEC.PLANT.GEN.1-DFO-IC.M   (1) Sand Point (1)   megawatthours   Net generation   distillate fuel oil   internal combustion (diesel, piston) engine   monthly     55.339722   -160.497222   USA-AK      2019-03-01    215.544  23-SEP-22 12.11.30 PM 
# ELEC.PLANT.GEN.1-DFO-IC.M   (1) Sand Point (1)   megawatthours   Net generation   distillate fuel oil   internal combustion (diesel, piston) engine   monthly     55.339722   -160.497222   USA-AK      2019-04-01    198.082  23-SEP-22 12.11.30 PM 
# ELEC.PLANT.GEN.1-DFO-IC.M   (1) Sand Point (1)   megawatthours   Net generation   distillate fuel oil   internal combustion (diesel, piston) engine   monthly     55.339722   -160.497222   USA-AK      2019-05-01    215.578  23-SEP-22 12.11.30 PM 
# ELEC.PLANT.GEN.1-DFO-IC.M   (1) Sand Point (1)   megawatthours   Net generation   distillate fuel oil   internal combustion (diesel, piston) engine   monthly     55.339722   -160.497222   USA-AK      2019-06-01    185.236  23-SEP-22 12.11.30 PM 
# ELEC.PLANT.GEN.1-DFO-IC.M   (1) Sand Point (1)   megawatthours   Net generation   distillate fuel oil   internal combustion (diesel, piston) engine   monthly     55.339722   -160.497222   USA-AK      2019-07-01    210.989  23-SEP-22 12.11.30 PM 
# ELEC.PLANT.GEN.1-DFO-IC.M   (1) Sand Point (1)   megawatthours   Net generation   distillate fuel oil   internal combustion (diesel, piston) engine   monthly     55.339722   -160.497222   USA-AK      2019-08-01    233.607  23-SEP-22 12.11.30 PM 
# ELEC.PLANT.GEN.1-DFO-IC.M   (1) Sand Point (1)   megawatthours   Net generation   distillate fuel oil   internal combustion (diesel, piston) engine   monthly     55.339722   -160.497222   USA-AK      2019-09-01    277.390  23-SEP-22 12.11.30 PM 
# ELEC.PLANT.GEN.1-DFO-IC.M   (1) Sand Point (1)   megawatthours   Net generation   distillate fuel oil   internal combustion (diesel, piston) engine   monthly     55.339722   -160.497222   USA-AK      2019-10-01    291.533  23-SEP-22 12.11.30 PM 

# Export data file ------------------------------------------------------------

saveRDS(generation, "./output/plant-generation.RData")