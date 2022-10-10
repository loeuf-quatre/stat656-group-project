###############################################################################
# Title: 02-eda.R                                                             #
# Description: EDA on EIA data                                                #
###############################################################################

# Housekeeping ----------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(knitr)

# Tidy data -------------------------------------------------------------------

ng <- readRDS("./output/plant-generation.RData")
ec <- readRDS("./output/electricity-costs.RData")

# Net generation ------------------------------------------

"
Investigate missing values
"

missing_counts <- data.frame(lapply(ng, function(x) length(x[is.na(x)])))

kable(missing_counts, "simple")

#  series_id   plant   units   energy_type   energy_source   prime_mover   frequency    lat    lon   geography     ds   output   updated
# ----------  ------  ------  ------------  --------------  ------------  ----------  -----  -----  ----------  -----  -------  --------
#          0       0       0             0               0             0           0   1509   1509        1509   1437     1437         0
 
missing_lats <- ng %>%
  filter(
    is.na(lat)
  ) 

missing_lats %>%
  head() %>%
  kable(
    "simple"
  )

# series_id                       plant                                             units           energy_type      energy_source           prime_mover                                   frequency   lat   lon   geography   ds    output  updated               
# ------------------------------  ------------------------------------------------  --------------  ---------------  ----------------------  --------------------------------------------  ----------  ----  ----  ----------  ---  -------  ----------------------
# ELEC.PLANT.GEN.10-DFO-ST.M      (10) Greene County (10)                           megawatthours   Net generation   distillate fuel oil     steam turbine                                 monthly     NA    NA    NA          NA        NA  07-JUL-16 05.18.42 PM 
# ELEC.PLANT.GEN.10-SC-ST.M       (10) Greene County (10)                           megawatthours   Net generation   synthetic coal          steam turbine                                 monthly     NA    NA    NA          NA        NA  07-JUL-16 05.18.42 PM 
# ELEC.PLANT.GEN.10003-SUB-ST.M   (10003) Colorado Energy Nations Company (10003)   megawatthours   Net generation   subbituminous coal      steam turbine                                 monthly     NA    NA    NA          NA        NA  07-JUL-16 05.18.42 PM 
# ELEC.PLANT.GEN.10004-OTH-ST.M   (10004) Mosaic South Pierce Operations (10004)    megawatthours   Net generation   other                   steam turbine                                 monthly     NA    NA    NA          NA        NA  28-JUN-17 04.07.58 PM 
# ELEC.PLANT.GEN.10008-NG-IC.M    (10008) Baptist Medical Center (10008)            megawatthours   Net generation   natural gas             internal combustion (diesel, piston) engine   monthly     NA    NA    NA          NA        NA  28-JUN-17 04.07.58 PM 
# ELEC.PLANT.GEN.10012-MSW-ST.M   (10012) Covanta Warren Energy (10012)             megawatthours   Net generation   municipal solid waste   steam turbine                                 monthly     NA    NA    NA          NA        NA  28-JUN-17 04.07.58 PM

ng %>%
  filter(
    plant == "(10) Greene County (10)"
  ) %>%
  head() %>%
  kable(
    "simple"
  )

# series_id                    plant                     units           energy_type      energy_source     prime_mover     frequency   lat       lon        geography   ds               output  updated               
# ---------------------------  ------------------------  --------------  ---------------  ----------------  --------------  ----------  --------  ---------  ----------  -----------  ----------  ----------------------
# ELEC.PLANT.GEN.10-BIT-ST.M   (10) Greene County (10)   megawatthours   Net generation   bituminous coal   steam turbine   monthly     32.6017   -87.7811   USA-AL      2012-01-01    149164.47  25-JUN-20 12.49.38 AM 
# ELEC.PLANT.GEN.10-BIT-ST.M   (10) Greene County (10)   megawatthours   Net generation   bituminous coal   steam turbine   monthly     32.6017   -87.7811   USA-AL      2012-02-01     99329.94  25-JUN-20 12.49.38 AM 
# ELEC.PLANT.GEN.10-BIT-ST.M   (10) Greene County (10)   megawatthours   Net generation   bituminous coal   steam turbine   monthly     32.6017   -87.7811   USA-AL      2012-03-01     93281.65  25-JUN-20 12.49.38 AM 
# ELEC.PLANT.GEN.10-BIT-ST.M   (10) Greene County (10)   megawatthours   Net generation   bituminous coal   steam turbine   monthly     32.6017   -87.7811   USA-AL      2012-04-01     84437.12  25-JUN-20 12.49.38 AM 
# ELEC.PLANT.GEN.10-BIT-ST.M   (10) Greene County (10)   megawatthours   Net generation   bituminous coal   steam turbine   monthly     32.6017   -87.7811   USA-AL      2012-05-01    183609.73  25-JUN-20 12.49.38 AM 
# ELEC.PLANT.GEN.10-BIT-ST.M   (10) Greene County (10)   megawatthours   Net generation   bituminous coal   steam turbine   monthly     32.6017   -87.7811   USA-AL      2012-06-01    222158.96  25-JUN-20 12.49.38 AM

"
Some of the missing plant coordinates seem specific to a particulate series
ID i.e. the same plant with a different energy source will report the
plant coordinates. Let's impute the missing coordinates with the ones
reported for the same plant with a different series ID
"

ng <- ng %>%
  group_by(
    plant
  ) %>%
  mutate(
    lat = max(lat, na.rm = TRUE),
    lon = max(lon, na.rm = TRUE),
    geography = max(geography, na.rm = TRUE)
  )

# Revisit missing counts
missing_counts <- data.frame(lapply(ng, function(x) length(x[is.na(x)])))

kable(missing_counts, "simple")

#  series_id   plant   units   energy_type   energy_source   prime_mover   frequency   lat   lon   geography     ds   output   updated
# ----------  ------  ------  ------------  --------------  ------------  ----------  ----  ----  ----------  -----  -------  --------
#          0       0       0             0               0             0           0   854   854         854   1437     1437         0

"
Still ~ 850 nulls present in the coordinates columns. Let's look at these
observations a little more closely
"

ng %>%
  filter(
    is.na(lat)
  ) %>%
  head() %>%
  kable(
    "simple"
  )

# series_id                       plant                                           units           energy_type      energy_source            prime_mover                                              frequency   lat   lon   geography   ds    output  updated               
# ------------------------------  ----------------------------------------------  --------------  ---------------  -----------------------  -------------------------------------------------------  ----------  ----  ----  ----------  ---  -------  ----------------------
# ELEC.PLANT.GEN.10037-NG-ST.M    (10037) Wheelabrator Hudson Energy (10037)      megawatthours   Net generation   natural gas              steam turbine                                            monthly     NA    NA    NA          NA        NA  28-JUN-17 04.07.58 PM 
# ELEC.PLANT.GEN.10037-WDS-ST.M   (10037) Wheelabrator Hudson Energy (10037)      megawatthours   Net generation   wood/wood waste solids   steam turbine                                            monthly     NA    NA    NA          NA        NA  28-JUN-17 04.07.58 PM 
# ELEC.PLANT.GEN.10047-LFG-GT.M   (10047) Orange County New York (10047)          megawatthours   Net generation   landfill gas             combustion (gas) turbine (including jet engine design)   monthly     NA    NA    NA          NA        NA  07-JUL-16 05.18.42 PM 
# ELEC.PLANT.GEN.10074-BLQ-ST.M   (10074) Pulp Mill Power House (10074)           megawatthours   Net generation   black liquour            steam turbine                                            monthly     NA    NA    NA          NA        NA  28-JUN-17 04.07.58 PM 
# ELEC.PLANT.GEN.10074-NG-ST.M    (10074) Pulp Mill Power House (10074)           megawatthours   Net generation   natural gas              steam turbine                                            monthly     NA    NA    NA          NA        NA  28-JUN-17 04.07.58 PM 
# ELEC.PLANT.GEN.10097-DFO-IC.M   (10097) Crozer Chester Medical Center (10097)   megawatthours   Net generation   distillate fuel oil      internal combustion (diesel, piston) engine              monthly     NA    NA    NA          NA        NA  07-JUL-16 05.18.42 PM

ng %>%
  filter(
    is.na(lat)
  ) %>%
  group_by(
    plant
  ) %>%
  summarize(
    n = n(),
    first_obs = min(ds, na.rm = TRUE),
    last_obs = max(ds, na.rm = TRUE),
    max_output = max(output, na.rm = TRUE),
    last_updated = max(updated, na.rm = TRUE)
  ) %>%
  arrange(
    -n
  ) %>%
  head(
    20
  ) %>%
  kable(
    "simple"
  )

# plant                                                     n  first_obs    last_obs      max_output  last_updated          
# ------------------------------------------------------  ---  -----------  -----------  -----------  ----------------------
# (55952) Las Vegas Cogeneration II LLC (55952)            72  2012-01-01   2014-12-01         58718  06-MAR-17 04.49.40 PM 
# (1613) Somerset Station (1613)                            9  NA           NA                  -Inf  28-JUN-17 04.07.58 PM 
# (54090) International Paper Louisiana Mill (54090)        6  NA           NA                  -Inf  28-JUN-17 04.07.58 PM 
# (10803) Ogdensburg Power (10803)                          5  NA           NA                  -Inf  07-JUL-16 05.18.42 PM 
# (3159) Cromby Generating Station (3159)                   5  NA           NA                  -Inf  07-JUL-16 05.18.42 PM 
# (50405) Warbasse Cogen Facility (50405)                   5  NA           NA                  -Inf  28-JUN-17 04.07.58 PM 
# (50438) S D Warren Muskegon (50438)                       5  NA           NA                  -Inf  07-JUL-16 05.18.42 PM 
# (50459) NRG Ilion LP (50459)                              5  NA           NA                  -Inf  07-JUL-16 05.18.42 PM 
# (54944) Albany Paper Mill (54944)                         5  NA           NA                  -Inf  28-JUN-17 04.07.58 PM 
# (56045) Millennium Hawkins Point (56045)                  5  NA           NA                  -Inf  07-JUL-16 05.18.42 PM 
# (864) Meredosia (864)                                     5  NA           NA                  -Inf  28-JUN-17 04.07.58 PM 
# (10344) Charleston Resource Recovery Facility (10344)     4  NA           NA                  -Inf  28-JUN-17 04.07.58 PM 
# (10522) Indeck Pepperell Power Facility (10522)           4  NA           NA                  -Inf  28-JUN-17 04.07.58 PM 
# (1058) Sixth Street (1058)                                4  NA           NA                  -Inf  28-JUN-17 04.07.58 PM 
# (10618) South Glens Falls Energy LLC (10618)              4  NA           NA                  -Inf  28-JUN-17 04.07.58 PM 
# (10701) Wausau Papers of New Hampshire (10701)            4  NA           NA                  -Inf  28-JUN-17 04.07.58 PM 
# (10792) Unifi Kinston LLC (10792)                         4  NA           NA                  -Inf  28-JUN-17 04.07.58 PM 
# (10793) Seaford Delaware Plant (10793)                    4  NA           NA                  -Inf  28-JUN-17 04.07.58 PM 
# (10796) Waynesboro Virginia Plant (10796)                 4  NA           NA                  -Inf  28-JUN-17 04.07.58 PM 
# (50483) Bryant Sugar House (50483)                        4  NA           NA                  -Inf  28-JUN-17 04.07.58 PM

"
The plants with missing coordinates have a small number of observations 
without any registered output and/or closed years ago
"

# Remove observations with missing coordinates
ng <- filter(ng, !is.na(lat))

# Check missing counts again
missing_counts <- data.frame(lapply(ng, function(x) length(x[is.na(x)])))

kable(missing_counts, "simple")

#  series_id   plant   units   energy_type   energy_source   prime_mover   frequency   lat   lon   geography    ds   output   updated
# ----------  ------  ------  ------------  --------------  ------------  ----------  ----  ----  ----------  ----  -------  --------
#          0       0       0             0               0             0           0     0     0           0   655      655         0

ng %>%
  filter(
    is.na(ds)
  ) %>%
  head() %>%
  kable(
    "simple"
  )

# series_id                       plant                                             units           energy_type      energy_source           prime_mover                                   frequency   lat         lon          geography   ds    output  updated               
# ------------------------------  ------------------------------------------------  --------------  ---------------  ----------------------  --------------------------------------------  ----------  ----------  -----------  ----------  ---  -------  ----------------------
# ELEC.PLANT.GEN.10-DFO-ST.M      (10) Greene County (10)                           megawatthours   Net generation   distillate fuel oil     steam turbine                                 monthly     32.6017     -87.7811     USA-AL      NA        NA  07-JUL-16 05.18.42 PM 
# ELEC.PLANT.GEN.10-SC-ST.M       (10) Greene County (10)                           megawatthours   Net generation   synthetic coal          steam turbine                                 monthly     32.6017     -87.7811     USA-AL      NA        NA  07-JUL-16 05.18.42 PM 
# ELEC.PLANT.GEN.10003-SUB-ST.M   (10003) Colorado Energy Nations Company (10003)   megawatthours   Net generation   subbituminous coal      steam turbine                                 monthly     39.7606     -105.215     USA-CO      NA        NA  07-JUL-16 05.18.42 PM 
# ELEC.PLANT.GEN.10004-OTH-ST.M   (10004) Mosaic South Pierce Operations (10004)    megawatthours   Net generation   other                   steam turbine                                 monthly     27.76522    -81.938787   USA-FL      NA        NA  28-JUN-17 04.07.58 PM 
# ELEC.PLANT.GEN.10008-NG-IC.M    (10008) Baptist Medical Center (10008)            megawatthours   Net generation   natural gas             internal combustion (diesel, piston) engine   monthly     30.314467   -81.662705   USA-FL      NA        NA  28-JUN-17 04.07.58 PM 
# ELEC.PLANT.GEN.10012-MSW-ST.M   (10012) Covanta Warren Energy (10012)             megawatthours   Net generation   municipal solid waste   steam turbine                                 monthly     40.8205     -75.0112     USA-NJ      NA        NA  28-JUN-17 04.07.58 PM

"
These look familiar. Seems geographic missingness is related
to date and output missingness
"

ng %>%
  filter(
    is.na(ds)
  ) %>%
  group_by(
    series_id
  ) %>%
  summarize(
    n = n(),
    last_updated = max(updated, na.rm = TRUE)
  ) %>%
  arrange(
    -n
  ) %>%
  head(
    20
  ) %>%
  kable(
    "simple"
  )

"
These also look like outdated series with no output for these specific plants
"

ng <- filter(ng, !is.na(ds))

# Last round
missing_counts <- data.frame(lapply(ng, function(x) length(x[is.na(x)])))

kable(missing_counts, "simple")

# series_id   plant   units   energy_type   energy_source   prime_mover   frequency   lat   lon   geography   ds   output   updated
# ----------  ------  ------  ------------  --------------  ------------  ----------  ----  ----  ----------  ---  -------  --------
#          0       0       0             0               0             0           0     0     0           0    0        0         0

"
Map generation data geo field to states field
"

states <- data.frame(state = state.name, abb = state.abb)
dc <- data.frame(state = "District Of Columbia", abb = "DC")
states <- rbind(states, dc)

ng <- ng %>%
  mutate(
    yr = format(ds, "%Y"),
    mth = format(ds, "%m"),
    abb = trimws(gsub(".*-", "", geography))
  ) %>%
  left_join(
    states,
    by = "abb"
  )

kable(head(ng), "simple")

# series_id                   plant                units           energy_type      energy_source         prime_mover                                   frequency   lat         lon           geography   ds             output  updated                 yr     mth   abb   state  
# --------------------------  -------------------  --------------  ---------------  --------------------  --------------------------------------------  ----------  ----------  ------------  ----------  -----------  --------  ----------------------  -----  ----  ----  -------
# ELEC.PLANT.GEN.1-DFO-IC.M   (1) Sand Point (1)   megawatthours   Net generation   distillate fuel oil   internal combustion (diesel, piston) engine   monthly     55.339722   -160.497222   USA-AK      2019-01-01    195.676  23-SEP-22 12.11.30 PM   2019   01    AK    Alaska 
# ELEC.PLANT.GEN.1-DFO-IC.M   (1) Sand Point (1)   megawatthours   Net generation   distillate fuel oil   internal combustion (diesel, piston) engine   monthly     55.339722   -160.497222   USA-AK      2019-02-01    217.498  23-SEP-22 12.11.30 PM   2019   02    AK    Alaska 
# ELEC.PLANT.GEN.1-DFO-IC.M   (1) Sand Point (1)   megawatthours   Net generation   distillate fuel oil   internal combustion (diesel, piston) engine   monthly     55.339722   -160.497222   USA-AK      2019-03-01    215.544  23-SEP-22 12.11.30 PM   2019   03    AK    Alaska 
# ELEC.PLANT.GEN.1-DFO-IC.M   (1) Sand Point (1)   megawatthours   Net generation   distillate fuel oil   internal combustion (diesel, piston) engine   monthly     55.339722   -160.497222   USA-AK      2019-04-01    198.082  23-SEP-22 12.11.30 PM   2019   04    AK    Alaska 
# ELEC.PLANT.GEN.1-DFO-IC.M   (1) Sand Point (1)   megawatthours   Net generation   distillate fuel oil   internal combustion (diesel, piston) engine   monthly     55.339722   -160.497222   USA-AK      2019-05-01    215.578  23-SEP-22 12.11.30 PM   2019   05    AK    Alaska 
# ELEC.PLANT.GEN.1-DFO-IC.M   (1) Sand Point (1)   megawatthours   Net generation   distillate fuel oil   internal combustion (diesel, piston) engine   monthly     55.339722   -160.497222   USA-AK      2019-06-01    185.236  23-SEP-22 12.11.30 PM   2019   06    AK    Alaska 

# Electricity costs ---------------------------------------

missing_counts <- data.frame(lapply(ec, function(x) length(x[is.na(x)])))

kable(missing_counts, "simple")

# state   ds   sector   number_of_customer_accounts_number_of_customers   average_retail_price_of_electricity_cents_per_kilowatthour   revenue_from_retail_sales_of_electricity_million_dollars   retail_sales_of_electricity_million_kilowatthours
# ------  ---  -------  ------------------------------------------------  -----------------------------------------------------------  ---------------------------------------------------------  --------------------------------------------------
#      0    0        0                                              8212                                                            2                                                         21                                                  16

missing_accounts <- ec %>%
  filter(
    is.na(number_of_customer_accounts_number_of_customers)
  ) %>%
  group_by(
    state,
    sector
  ) %>%
  summarize(
    n = n(),
    first_obs = min(ds, na.rm = TRUE),
    last_obs = max(ds, na.rm = TRUE),
    .groups = "drop"
  ) 

missing_accounts %>%
  arrange(
    -n
  ) %>%
  mutate(
    cume = cumsum(n),
    percent_cume = scales::percent(cume / max(cume), accuracy = .1)
  ) %>%
  kable(
    "simple"
  )

# state                           sector              n  first_obs    last_obs      cume  percent_cume 
# ------------------------------  ---------------  ----  -----------  -----------  -----  -------------
# Alabama                         other             127  2012-01-01   2022-07-01     127  1.5%         
# Alaska                          other             127  2012-01-01   2022-07-01     254  3.1%         
# Arizona                         other             127  2012-01-01   2022-07-01     381  4.6%         
# Arkansas                        other             127  2012-01-01   2022-07-01     508  6.2%         
# California                      other             127  2012-01-01   2022-07-01     635  7.7%         
# Colorado                        other             127  2012-01-01   2022-07-01     762  9.3%         
# Connecticut                     other             127  2012-01-01   2022-07-01     889  10.8%        
# Delaware                        other             127  2012-01-01   2022-07-01    1016  12.4%        
# District Of Columbia            other             127  2012-01-01   2022-07-01    1143  13.9%        
# East North Central (total)      other             127  2012-01-01   2022-07-01    1270  15.5%        
# East South Central (total)      other             127  2012-01-01   2022-07-01    1397  17.0%        
# Florida                         other             127  2012-01-01   2022-07-01    1524  18.6%        
# Georgia                         other             127  2012-01-01   2022-07-01    1651  20.1%        
# Hawaii                          other             127  2012-01-01   2022-07-01    1778  21.7%        
# Idaho                           other             127  2012-01-01   2022-07-01    1905  23.2%        
# Illinois                        other             127  2012-01-01   2022-07-01    2032  24.7%        
# Indiana                         other             127  2012-01-01   2022-07-01    2159  26.3%        
# Iowa                            transportation    127  2012-01-01   2022-07-01    2286  27.8%        
# Iowa                            other             127  2012-01-01   2022-07-01    2413  29.4%        
# Kansas                          other             127  2012-01-01   2022-07-01    2540  30.9%        
# Kentucky                        other             127  2012-01-01   2022-07-01    2667  32.5%        
# Louisiana                       other             127  2012-01-01   2022-07-01    2794  34.0%        
# Maine                           other             127  2012-01-01   2022-07-01    2921  35.6%        
# Maryland                        other             127  2012-01-01   2022-07-01    3048  37.1%        
# Massachusetts                   other             127  2012-01-01   2022-07-01    3175  38.7%        
# Michigan                        other             127  2012-01-01   2022-07-01    3302  40.2%        
# Middle Atlantic (total)         other             127  2012-01-01   2022-07-01    3429  41.8%        
# Minnesota                       other             127  2012-01-01   2022-07-01    3556  43.3%        
# Mississippi                     other             127  2012-01-01   2022-07-01    3683  44.8%        
# Missouri                        other             127  2012-01-01   2022-07-01    3810  46.4%        
# Montana                         other             127  2012-01-01   2022-07-01    3937  47.9%        
# Mountain (total)                other             127  2012-01-01   2022-07-01    4064  49.5%        
# Nebraska                        other             127  2012-01-01   2022-07-01    4191  51.0%        
# Nevada                          other             127  2012-01-01   2022-07-01    4318  52.6%        
# New England (total)             other             127  2012-01-01   2022-07-01    4445  54.1%        
# New Hampshire                   other             127  2012-01-01   2022-07-01    4572  55.7%        
# New Jersey                      other             127  2012-01-01   2022-07-01    4699  57.2%        
# New Mexico                      other             127  2012-01-01   2022-07-01    4826  58.8%        
# New York                        other             127  2012-01-01   2022-07-01    4953  60.3%        
# North Carolina                  other             127  2012-01-01   2022-07-01    5080  61.9%        
# North Dakota                    other             127  2012-01-01   2022-07-01    5207  63.4%        
# Ohio                            other             127  2012-01-01   2022-07-01    5334  65.0%        
# Oklahoma                        other             127  2012-01-01   2022-07-01    5461  66.5%        
# Oregon                          other             127  2012-01-01   2022-07-01    5588  68.0%        
# Pacific Contiguous (total)      other             127  2012-01-01   2022-07-01    5715  69.6%        
# Pacific Noncontiguous (total)   other             127  2012-01-01   2022-07-01    5842  71.1%        
# Pennsylvania                    other             127  2012-01-01   2022-07-01    5969  72.7%        
# Rhode Island                    other             127  2012-01-01   2022-07-01    6096  74.2%        
# South Atlantic (total)          other             127  2012-01-01   2022-07-01    6223  75.8%        
# South Carolina                  other             127  2012-01-01   2022-07-01    6350  77.3%        
# South Dakota                    other             127  2012-01-01   2022-07-01    6477  78.9%        
# Tennessee                       other             127  2012-01-01   2022-07-01    6604  80.4%        
# Texas                           other             127  2012-01-01   2022-07-01    6731  82.0%        
# United States                   other             127  2012-01-01   2022-07-01    6858  83.5%        
# Utah                            other             127  2012-01-01   2022-07-01    6985  85.1%        
# Vermont                         other             127  2012-01-01   2022-07-01    7112  86.6%        
# Virginia                        other             127  2012-01-01   2022-07-01    7239  88.2%        
# Washington                      other             127  2012-01-01   2022-07-01    7366  89.7%        
# West North Central (total)      other             127  2012-01-01   2022-07-01    7493  91.2%        
# West South Central (total)      other             127  2012-01-01   2022-07-01    7620  92.8%        
# West Virginia                   other             127  2012-01-01   2022-07-01    7747  94.3%        
# Wisconsin                       other             127  2012-01-01   2022-07-01    7874  95.9%        
# Wyoming                         other             127  2012-01-01   2022-07-01    8001  97.4%        
# Alaska                          industrial         82  2013-01-01   2019-12-01    8083  98.4%        
# Pacific Noncontiguous (total)   industrial         52  2015-01-01   2019-04-01    8135  99.1%        
# Alaska                          residential        12  2016-01-01   2016-12-01    8147  99.2%        
# Alaska                          commercial         12  2016-01-01   2016-12-01    8159  99.4%        
# New England (total)             transportation     11  2014-01-01   2017-05-01    8170  99.5%        
# Connecticut                     transportation     10  2014-01-01   2017-05-01    8180  99.6%        
# Michigan                        transportation      9  2014-01-01   2014-12-01    8189  99.7%        
# East North Central (total)      transportation      7  2014-01-01   2014-11-01    8196  99.8%        
# New Jersey                      transportation      7  2015-09-01   2016-03-01    8203  99.9%        
# Middle Atlantic (total)         transportation      5  2015-10-01   2016-03-01    8208  100.0%       
# Massachusetts                   transportation      3  2014-10-01   2014-12-01    8211  100.0%       
# United States                   transportation      1  2014-05-01   2014-05-01    8212  100.0%

"
Most states seem to not track customer accounts for the 'other' sector. Others
had some periods of data gaps e.g. Connecticut for transportation. Note that
Iowa straight-up doesn't report transportation for accounts, retail price,
revenue nor sales
"

check_cols <- c(
  "average_retail_price_of_electricity_cents_per_kilowatthour",
  "revenue_from_retail_sales_of_electricity_million_dollars",
  "retail_sales_of_electricity_million_kilowatthours"
)

ec %>% 
  rowwise() %>% 
  mutate(
    null_row = sum(c_across(check_cols))
  ) %>% 
  filter(
    is.na(null_row)
  ) %>%
  kable(
    "simple"
  )

# state                           ds           sector         number_of_customer_accounts_number_of_customers   average_retail_price_of_electricity_cents_per_kilowatthour   revenue_from_retail_sales_of_electricity_million_dollars   retail_sales_of_electricity_million_kilowatthours   null_row
# ------------------------------  -----------  ------------  ------------------------------------------------  -----------------------------------------------------------  ---------------------------------------------------------  --------------------------------------------------  ---------
# Alaska                          2016-01-01   residential                                                 NA                                                        19.14                                                         NA                                                  NA         NA
# Alaska                          2016-02-01   residential                                                 NA                                                        19.49                                                         NA                                                  NA         NA
# Alaska                          2016-03-01   residential                                                 NA                                                        19.73                                                         NA                                                  NA         NA
# Alaska                          2016-04-01   residential                                                 NA                                                        20.43                                                         NA                                                  NA         NA
# Alaska                          2016-05-01   residential                                                 NA                                                        20.61                                                         NA                                                  NA         NA
# Alaska                          2016-06-01   residential                                                 NA                                                        20.94                                                         NA                                                  NA         NA
# Alaska                          2016-07-01   residential                                                 NA                                                        21.03                                                         NA                                                  NA         NA
# Alaska                          2016-08-01   residential                                                 NA                                                           NA                                                         NA                                                  NA         NA
# Alaska                          2016-09-01   residential                                                 NA                                                           NA                                                         NA                                                  NA         NA
# Alaska                          2016-10-01   residential                                                 NA                                                        20.73                                                         NA                                                  NA         NA
# Alaska                          2016-11-01   residential                                                 NA                                                        20.93                                                         NA                                                  NA         NA
# Alaska                          2016-12-01   residential                                                 NA                                                        20.17                                                         NA                                                  NA         NA
# Pacific Noncontiguous (total)   2016-01-01   residential                                             712401                                                        23.09                                                         NA                                            436.8338         NA
# Pacific Noncontiguous (total)   2016-02-01   residential                                             713052                                                        23.21                                                         NA                                                  NA         NA
# Pacific Noncontiguous (total)   2016-03-01   residential                                             712095                                                        23.83                                                         NA                                                  NA         NA
# Pacific Noncontiguous (total)   2016-04-01   residential                                             712175                                                        24.10                                                         NA                                            356.1282         NA
# Pacific Noncontiguous (total)   2016-05-01   residential                                             712989                                                        24.30                                                         NA                                            349.1865         NA
# Pacific Noncontiguous (total)   2016-07-01   residential                                             714088                                                        25.38                                                         NA                                            372.0881         NA
# Pacific Noncontiguous (total)   2016-08-01   residential                                             715524                                                        24.95                                                         NA                                                  NA         NA
# Pacific Noncontiguous (total)   2016-09-01   residential                                             715918                                                        25.07                                                         NA                                                  NA         NA
# Pacific Noncontiguous (total)   2016-12-01   residential                                

"
Alaska and by extension the noncontiguous Pacific had a bad 2016 reporting-wise
"

ec <- ec %>%
  mutate(
    yr = format(ds, "%Y"),
    mth = format(ds, "%m")
  )

# Data viz -------------------------------------------------------------------

# Linear models by state ----------------------------------

ngsy <- ng %>%
  group_by(
    state,
    yr
  ) %>%
  summarize(
    output = sum(output, na.rm = TRUE),
    .groups = "drop"
  )

ecsy <- ec %>%
  rename(
    revenue = revenue_from_retail_sales_of_electricity_million_dollars,
    sales = retail_sales_of_electricity_million_kilowatthours
  ) %>%
  group_by(
    state,
    yr
  ) %>%
  summarize(
    average_price = sum(revenue, na.rm = TRUE) / sum(sales, na.rm = TRUE),
    .groups = "drop"
  )

lms <- left_join(ngsy, ecsy, by = c("state", "yr"))
lms <- filter(lms, yr < 2022) # Only full years

ggplot() +
  geom_point(
    data = lms,
    aes(
      x = output / 1e6,
      y = average_price
    )
  ) +
  geom_smooth(
    data = lms,
    aes(
      x = output / 1e6,
      y = average_price
    ),
    method = "lm",
    se = FALSE
  ) +
  facet_wrap(
    ~ state,
    scales = "free"
  ) +
  scale_y_continuous(
    labels = function(x) scales::dollar(x, accuracy = .001)
  ) +
  labs(
    x = "Output GWh",
    y = "Average Price"
  )