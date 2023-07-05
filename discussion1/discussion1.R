# Author: Eric Kenney
# Date: 4 July 2023
# Purpose: R Script for Discussion #1
# Assumptions: n/a
# Datasets: Global Climate Change Data (https://data.world)

# Import libraries
library(fpp3)
library(rstudioapi)

# Read in data (https://data.world/data-society/global-climate-change-data)
temps = read.csv(file.path(dirname(documentPath()), "global_climate_data.csv"))

# Filter down to United States
temps_usa = temps %>% 
  filter(Country == "United States") %>% 
  select(-c(Country, AverageTemperatureUncertainty))

# Convert to tsibble
temps_usa = temps_usa %>% 
  mutate(Month = yearmonth(dt)) %>% 
  select(-dt) %>% 
  as_tsibble(key = State,
             index = Month)

# Reduce to the last 100 years
temps_usa = temps_usa %>% 
  filter(year(Month) >= 1915)

# Reduce to the state of Oregon, Nevada and autoplot
temps_usa %>% 
  filter(State == "Oregon") %>% 
  autoplot(AverageTemperature) +
  labs(title = "Average Temperature in Oregon (1915-2015)",
       y = "Temperature (Celcius)")
temps_usa %>% 
  filter(State == "Nevada") %>% 
  autoplot(AverageTemperature) +
  labs(title = "Average Temperature in Nevada (1915-2015)",
       y = "Temperature (Celcius)")

# Conduct Decomposition on Oregon for the last 50 years
temps_oregon = temps_usa %>% 
  filter(year(Month) >= 1965 & State == "Oregon")

# Additive Decomposition
temps_oregon %>% 
  model(classical_decomposition(AverageTemperature, type = "additive")) %>% 
  components() %>%
  autoplot() + 
  labs(title = "Additive Decomposition")

# Multiplicative Decomposition
temps_oregon %>% 
  model(classical_decomposition(AverageTemperature, type = "multiplicative")) %>% 
  components() %>% 
  autoplot() +
  labs(title = "Multiplicative Decomposition")
