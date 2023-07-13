# Author: Eric Kenney
# Date: 12 July 2023
# Purpose: R Script for Discussion #2
# Assumptions: n/a
# Datasets: Global Climate Change Data (https://data.world)

# Import libraries
library(fpp3)
library(rstudioapi)

output_path = dirname(documentPath())

# Read in the data
temps = read.csv(file.path(dirname(documentPath()), "global_climate_data.csv"))

# Filter down to Oregon
temps_or = temps %>% 
  filter(State == "Oregon") %>% 
  select(-c(Country, AverageTemperatureUncertainty, State))

# Convert to tsibble
temps_or = temps_or %>% 
  mutate(Month = yearmonth(dt)) %>% 
  select(-dt) %>% 
  as_tsibble(index = Month)

# Build Plot
png(filename = file.path(output_path, "temps_or_autoplot.png"))
autoplot(temps_or)
dev.off()

# Filter to after 1850 to remove the NAs
temps_or = temps_or %>% 
  filter(year(Month) >= 1850)

# Confirm all NAs are gone
sum(is.na(temps_or$AverageTemperature))

# Split Training and Test Sets (80/20 Split)
total_obs = dim(temps_or)[1]
train_obs = total_obs * 0.8
test_obs = total_obs - train_obs
train_or = head(temps_or, train_obs)
test_or = tail(temps_or, test_obs)

# See what ETS model is created based on information criterion
# ETS(A,A,A)
ets_auto = train_or %>% 
  model(ETS(AverageTemperature))

report(ets_auto)

png(filename = file.path(output_path,"components_ets_auto.png"), width = 960, height = 960)
components(ets_auto) %>% 
  autoplot()
dev.off()

# Forecast
ets_auto_forecast = ets_auto %>% 
  forecast(h = test_obs)

accuracy(object = ets_auto_forecast, data = temps_or)

png(filename = file.path(output_path,"forecast_ets_auto.png"), width = 960, height = 480)
ets_auto_forecast %>% 
  autoplot(test_or)
dev.off()

# Build 2 more models
# ETS(A,A,N)
ets_aan = train_or %>% 
  model(ETS(AverageTemperature ~ error("A") + trend("A") + season("N")))

report(ets_aan)

png(filename = file.path(output_path,"components_ets_aan.png"), width = 960, height = 960)
components(ets_aan) %>% 
  autoplot()
dev.off()

# Forecast
ets_aan_forecast = ets_aan %>% 
  forecast(h = test_obs)

accuracy(object = ets_aan_forecast, data = temps_or)

png(filename = file.path(output_path,"forecast_ets_aan.png"), width = 960, height = 480)
ets_aan_forecast %>% 
  autoplot(test_or)
dev.off()

# ETS(A,N,A)
ets_ana = train_or %>% 
  model(ETS(AverageTemperature ~ error("A") + trend("N") + season("A")))

report(ets_ana)

png(filename = file.path(output_path,"components_ets_ana.png"), width = 960, height = 960)
components(ets_ana) %>% 
  autoplot()
dev.off()

# Forecast
ets_ana_forecast = ets_ana %>% 
  forecast(h = test_obs)

accuracy(object = ets_ana_forecast, data = temps_or)

png(filename = file.path(output_path,"forecast_ets_ana.png"), width = 960, height = 480)
ets_ana_forecast %>% 
  autoplot(test_or)
dev.off()
