##### JOIN OTC AND WEATHER DATA #####
library("tidyverse")
library("lubridate")

# Merge otc and gradient data
# Tsoil5 L site gradient: varying variance
otcData <- otcc %>% 
  rename(Tair = Tair30) %>% 
  mutate(rain = NA, solarRadiation = NA, windSpeed = NA, windDirection = NA, RH = NA, UV = NA) %>% 
  select(site, dateTime, Tair, Tsoil0, Tsoil5, Tsoil20, waterContent5, waterContent20, rain, PAR, solarRadiation, windSpeed, windDirection, RH, UV) %>% 
  mutate(logger = "otc")
  
climate <- distinct_weather %>%
  spread(key = variable, value = value) %>% 
  select(site, dateTime, Tair, Tsoil0, Tsoil5, Tsoil20, waterContent5, waterContent20, rain, PAR, solarRadiation, windSpeed, windDirection, RH, UV) %>% 
  mutate(logger = "gradient") %>% 
  bind_rows(otcData) %>% 
  mutate(flag = NA, comment = NA, month = month(dateTime)) %>% 
  mutate(flag = ifelse(logger == "gradient" & site == "L", "Tsoil5 decreasing variance", flag))
  
save(climate, file = "climate/climate.Rdata")



#### MONTHLY DATA ####
climate_month <- climate %>%
  select(-month, -flag, -comment) %>% 
  mutate(month = lubridate::ymd(format(dateTime, "%Y-%m-15"))) %>% 
  select(-dateTime) %>%
  gather(key = variable, value = value, -site, -logger) %>% 
  group_by(site, month, variable, logger) %>%
  filter(!is.na(value)) %>%
  summarise(meanV = mean(value), sumV = sum(value), n = n()) %>%
  mutate(value = ifelse(variable == "rain", sumV, meanV)) %>%
  filter(n > 6 * 24 * 7 * 3) %>% #at least three weeks of data
  select(-meanV, -sumV, -n)

# add missing months
full_grid <- expand.grid(logger = unique(climate_month$logger), variable = unique(climate_month$variable), site = unique(climate_month$site), month = seq(min(climate_month$month), max(climate_month$month), by = "month"))

climate_month <- left_join(full_grid, climate_month) %>% tbl_df()

save(climate_month, file = "climate/climate_month.Rdata")


#### ANNUAL DATA ####
climate_annaul <- 
  
save(monthly, annual, file = paste0("climate/month_annual", Sys.Date(), ".Rdata"))
