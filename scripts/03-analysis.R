###############################################################################
# Title: 02-eda.R                                                             #
# Description: EDA on EIA data                                                #
###############################################################################

# Housekeeping ----------------------------------------------------------------

library(dplyr) # Data wrangling
library(ggplot2) # Data viz
library(ggrepel) # Data viz
library(knitr) # Inline data tables
library(lmer) # Modeling
library(scales) # Data formatting
library(sf) # Mapping
library(tidyr) # Data wrangling

# Tidy data -------------------------------------------------------------------

ng <- readRDS("./output/plant-generation-tidy.RData")
ec <- readRDS("./output/electricity-costs-tidy.RData")

# By state category ---------------------------------------

ngsc <- ng %>%
  group_by(
    state,
    fuel_cat1
  ) %>%
  summarize(
    output = sum(output, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(
    state
  ) %>%
  mutate(
    state_output = sum(output)
  ) %>%
  ungroup() %>%
  filter(
    fuel_cat1 == "renewable fuels"
  ) %>%
  mutate(
    pct_renewable = output / state_output
  )

glimpse(ngsc, width = 75)

# Rows: 51
# Columns: 5
# $ state        <chr> "Alabama", "Alaska", "Arizona", "Arkansas", "Califor…
# $ fuel_cat1    <chr> "renewable fuels", "renewable fuels", "renewable fue…
# $ output       <dbl> 126955860.3, 15987427.8, 111103922.0, 44191243.0, 72…
# $ state_output <dbl> 1438127629.1, 58516148.2, 1102467406.5, 602180616.6,…
# $ pct_renwable <dbl> 0.08827858, 0.27321395, 0.10077751, 0.07338536, 0.37…

ecs <- ec %>%
  rename(
    revenue = revenue_from_retail_sales_of_electricity_million_dollars,
    sales = retail_sales_of_electricity_million_kilowatthours
  ) %>%
  group_by(
    state
  ) %>%
  summarize(
    average_price = sum(revenue, na.rm = TRUE) / sum(sales, na.rm = TRUE),
    .groups = "drop"
  )

glimpse(ecs, width = 75)

# Rows: 62
# Columns: 2
# $ state         <fct> Alabama, Alaska, Arizona, Arkansas, California, Col…
# $ average_price <dbl> 0.09677004, 0.18461676, 0.10446724, 0.08222758, 0.1…

dfs <- inner_join(ecs, ngsc, by = "state")
dfs <- select(dfs, state, average_price, pct_renewable)

kable(head(dfs, 10), "simple", align = "c")

#         state            average_price    pct_renewable 
# ----------------------  ---------------  ---------------
#        Alabama             0.0967700        0.0882786   
#         Alaska             0.1846168        0.2732139   
#        Arizona             0.1044672        0.1007775   
#        Arkansas            0.0822276        0.0733854   
#       California           0.1635743        0.3713578   
#        Colorado            0.1013453        0.2157960   
#      Connecticut           0.1773581        0.0456174   
#        Delaware            0.1085219        0.0167627   
#  District Of Columbia      0.1217548        0.3989857   
#        Florida             0.1049553        0.0356211

p01 <- ggplot() + 
  geom_point(
    data = dfs,
    aes(
      x = pct_renewable,
      y = average_price
    ),
    size = 3
  ) +
  geom_text_repel(
    data = dfs,
    aes(
      x = pct_renewable,
      y = average_price,
      label = state
    ),
    size = 5
  ) +
  geom_smooth(
    data = dfs,
    aes(
      x = pct_renewable,
      y = average_price
    ),
    method = "lm"
  ) +
  theme_light(
    base_size = 24
  )

ggsave("./output/p01.png", p01, width = 20, height = 12)

f01 <- lm(average_price ~ pct_renewable, data = dfs)

summary(f01)

Call:
lm(formula = average_price ~ pct_renewable, data = dfs)

# Residuals:
#       Min        1Q    Median        3Q       Max
# -0.033503 -0.022029 -0.015179  0.004372  0.185885
# 
# Coefficients:
#                Estimate Std. Error t value Pr(>|t|)
# (Intercept)    0.112572   0.007692  14.636   <2e-16 ***
# pct_renewable -0.007058   0.025997  -0.272    0.787
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.03949 on 49 degrees of freedom
# Multiple R-squared:  0.001502,	Adjusted R-squared:  -0.01888
# F-statistic: 0.07371 on 1 and 49 DF,  p-value: 0.7871

p02 <- resid_panel(f01, axis.text.size = 12) + 
  geom_point(
    size = 5
  )

ggsave("./output/p02.png", p02, width = 20, height = 12)

# By state-month category ---------------------------------

ngsmc <- ng %>%
  group_by(
    state,
    yr,
    mth,
    fuel_cat1
  ) %>%
  summarize(
    output = sum(output, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(
    state,
    yr,
    mth
  ) %>%
  mutate(
    state_output = sum(output)
  ) %>%
  ungroup() %>%
  filter(
    fuel_cat1 == "renewable fuels"
  ) %>%
  mutate(
    pct_renewable = output / state_output
  )

glimpse(ngsmc, width = 75)

# Rows: 6,079
# Columns: 7
# $ state         <chr> "Alabama", "Alabama", "Alabama", "Alabama", "Alabam…
# $ yr            <chr> "2012", "2012", "2012", "2012", "2012", "2012", "20…
# $ mth           <chr> "01", "02", "03", "04", "05", "06", "07", "08", "09…
# $ fuel_cat1     <chr> "renewable fuels", "renewable fuels", "renewable fu…
# $ output        <dbl> 1717347.2, 1243929.8, 1320451.4, 449827.2, 436306.9…
# $ state_output  <dbl> 13201660, 12049950, 11984838, 10219872, 12188003, 1…
# $ pct_renewable <dbl> 0.13008571, 0.10323112, 0.11017683, 0.04401496, 0.0…

ecsm <- ec %>%
  rename(
    revenue = revenue_from_retail_sales_of_electricity_million_dollars,
    sales = retail_sales_of_electricity_million_kilowatthours
  ) %>%
  group_by(
    state,
    yr,
    mth
  ) %>%
  summarize(
    average_price = sum(revenue, na.rm = TRUE) / sum(sales, na.rm = TRUE),
    .groups = "drop"
  )

glimpse(ecsm, width = 75)

# Rows: 7,874
# Columns: 4
# $ state         <fct> Alabama, Alabama, Alabama, Alabama, Alabama, Alabam…
# $ yr            <chr> "2012", "2012", "2012", "2012", "2012", "2012", "20…
# $ mth           <chr> "01", "02", "03", "04", "05", "06", "07", "08", "09…
# $ average_price <dbl> 0.08798521, 0.08862932, 0.08776485, 0.08767474, 0.0…

dfsm <- inner_join(ecsm, ngsmc, by = c("state", "yr", "mth"))
dfsm <- select(dfsm, state, yr, mth, average_price, pct_renewable)

kable(head(dfsm, 10), "simple", align = "c")

#   state      yr     mth    average_price    pct_renewable 
# ---------  ------  -----  ---------------  ---------------
#  Alabama    2012    01       0.0879852        0.1300857   
#  Alabama    2012    02       0.0886293        0.1032311   
#  Alabama    2012    03       0.0877649        0.1101768   
#  Alabama    2012    04       0.0876747        0.0440150   
#  Alabama    2012    05       0.0888631        0.0357981   
#  Alabama    2012    06       0.0975533        0.0373590   
#  Alabama    2012    07       0.0987498        0.0367265   
#  Alabama    2012    08       0.0975905        0.0473677   
#  Alabama    2012    09       0.0954514        0.0546681   
#  Alabama    2012    10       0.0894595        0.0633857 

p03 <- ggplot() + 
  geom_point(
    data = dfsm,
    aes(
      x = pct_renewable,
      y = average_price
    ),
    size = 3,
    alpha = .1
  ) +
  geom_text_repel(
    data = sample_n(dfsm, 20),
    aes(
      x = pct_renewable,
      y = average_price,
      label = paste0(state, " ", mth, "/", yr)
    ),
    size = 5
  ) +
  geom_smooth(
    data = dfsm,
    aes(
      x = pct_renewable,
      y = average_price
    ),
    method = "lm"
  ) +
  theme_light(
    base_size = 24
  )

ggsave("./output/p03.png", p03, width = 20, height = 12)

f02 <- lm(average_price ~ pct_renewable, data = dfsm)

summary(f02)

# Call:
# lm(formula = average_price ~ pct_renewable, data = dfsm)
# 
# Residuals:
#       Min        1Q    Median        3Q       Max 
# -0.047130 -0.022813 -0.013464  0.005129  0.253605 
# 
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    0.1103409  0.0006772 162.939   <2e-16 ***
# pct_renewable -0.0019748  0.0021250  -0.929    0.353    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.03949 on 6077 degrees of freedom
# Multiple R-squared:  0.0001421,	Adjusted R-squared:  -2.244e-05 
# F-statistic: 0.8636 on 1 and 6077 DF,  p-value: 0.3528

p04 <- resid_panel(f02, axis.text.size = 12) + 
  geom_point(
    size = 5
  )

ggsave("./output/p04.png", p04, width = 20, height = 12)

# By state-month category (panel) -------------------------

dfoh <- filter(dfsm, state == "Ohio")

p05 <- ggplot() +
  geom_point(
    data = dfoh,
    aes(
      x = mth,
      y = average_price,
      color = yr
    ),
    size = 5
  ) +
  geom_line(
    data = dfoh,
    aes(
      x = mth,
      y = average_price,
      color = yr,
      group = yr
    )
  ) +
  theme_light(
    base_size = 24
  )

ggsave("./output/p05.png", p05, width = 20, height = 12)

p06 <- ggplot() + 
  geom_point(
    data = dfoh,
    aes(
      x = pct_renewable,
      y = average_price
    ),
    size = 3
  ) +
  geom_text_repel(
    data = dfoh,
    aes(
      x = pct_renewable,
      y = average_price,
      label = month.abb[as.numeric(mth)]
    ),
    size = 5
  ) +
  geom_smooth(
    data = dfoh,
    aes(
      x = pct_renewable,
      y = average_price
    ),
    method = "lm"
  ) +
  facet_wrap(
    ~ yr
  ) +
  theme_light(
    base_size = 24
  )

ggsave("./output/p06.png", p06, width = 20, height = 12)

"
Mixed effects model where pct_renewable slope allowed to vary by state and
intercepts vary by year
"

f03 <- lmer(average_price ~ pct_renewable + (pct_renewable | state) + (pct_renewable | yr), data = dfsm)

summary(f03)

# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: average_price ~ pct_renewable + (pct_renewable | state) + (pct_renewable |      yr)
#    Data: dfsm
# 
# REML criterion at convergence: -40678.3
# 
# Scaled residuals: 
#      Min       1Q   Median       3Q      Max 
# -10.5412  -0.4373  -0.0277   0.4353   8.4561 
# 
# Random effects:
#  Groups   Name          Variance  Std.Dev. Corr 
#  state    (Intercept)   2.396e-03 0.048951      
#           pct_renewable 1.585e-02 0.125887 -0.56
#  yr       (Intercept)   1.562e-05 0.003952      
#           pct_renewable 1.488e-05 0.003858 0.91 
#  Residual               6.501e-05 0.008063      
# Number of obs: 6079, groups:  state, 51; yr, 10
# 
# Fixed effects:
#               Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)    0.11600    0.00698 52.72501  16.618   <2e-16 ***
# pct_renewable -0.04839    0.01842 48.41126  -2.626   0.0115 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr)
# pct_renewbl -0.523

cofs <- coef(f03)$state
cofs <- arrange(cofs, -pct_renewable)
cofs <- tibble::rownames_to_column(cofs, var = "state")
cofs$state <- factor(cofs$state, cofs$state)
cofs$mean_effect <- mean(cofs$pct_renewable)

p07 <- ggplot() +
  geom_hline(
    yintercept = 0,
    color = "grey",
    size = 1
  ) +
  geom_point(
    data = cofs,
    aes(
      x = state,
      y = pct_renewable
    ),
    size = 4
  ) +
  geom_hline(
    yintercept = unique(cofs$mean_effect),
    linetype = 2,
    size = 1
  ) +
  coord_flip() +
  labs(
    x = NULL,
    y = "pct_renewable_effect"
  ) +
  theme_light(
    base_size = 22
  )

ggsave("./output/p07.png", p07, width = 20, height = 12)

p08 <- resid_panel(f03, axis.text.size = 12) + 
  geom_point(
    size = 5
  )

ggsave("./output/p08.png", p08, width = 20, height = 12)

anova(f03, f02)

# Data: dfsm
# Models:
# f02: average_price ~ pct_renewable
# f03: average_price ~ pct_renewable + (pct_renewable | state) + (pct_renewable | 
# f03:     yr)
#     npar    AIC    BIC logLik deviance Chisq Df Pr(>Chisq)    
# f02    3 -22036 -22015  11021   -22042                        
# f03    9 -40675 -40614  20346   -40693 18651  6  < 2.2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1