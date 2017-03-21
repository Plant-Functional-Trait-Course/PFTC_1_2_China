##### JOIN OTC AND WEATHER DATA #####
library("tidyverse")

otcData <- otcc %>% 
  select(-file) %>% 
  rename(Tair = Tair30) %>% # rename 30cm logger to Tair, to match the gradient data
  gather(key = variable, value = value, -dateTime, -site) %>% 
  mutate(logger = "otc") # column to distinguish otc/gradient data

climate_unflagged <- distinct_weather %>% 
  gather(key = variable, value = value, -dateTime, -site) %>% 
  select(dateTime, site, variable, value) %>% 
  mutate(logger = "gradient") %>% # column to distinguish otc/gradient data
  bind_rows(otcData) %>% 
  mutate(flag = NA, comment = NA, month = month(dateTime))

save(climate, file = "climate/climate.Rdata")


# Flag Data
# Tsoil5 L site gradient: varying variance
climate <- climate_unflagged %>%
  mutate(flag = ifelse(logger == "gradient" & site == "L" & variable == "Tsoil5", "decreasing variance", flag))


ZoomIntoPlot(climate, date1 = as.POSIXct("2013-05-01 11:20:00", tz = "Asia/Shanghai"), date2 = as.POSIXct("2015-12-01 11:20:00", tz = "Asia/Shanghai"), site == "H", variable = "Tsoil0")



#### MONTHLY DATA ####
climate_month <- climate %>%
  select(-month) %>% 
  mutate(month = lubridate::ymd(format(dateTime, "%Y-%m-15"))) %>% 
  select(-dateTime) %>%
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
