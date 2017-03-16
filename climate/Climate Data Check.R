##### JOIN OTC AND WEATHER DATA #####

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


# Flag Data
# OTC air temperature data has too high maximum values in winter
climate <- climate_unflagged %>% 
  mutate(flag = ifelse(logger == "otc" & variable == "Tair", "unusable", flag))

ZoomIntoPlot(climate, date1 = as.POSIXct("2013-05-01 11:20:00", tz = "Asia/Shanghai"), date2 = as.POSIXct("2015-12-01 11:20:00", tz = "Asia/Shanghai"), site == "H", variable = "Tsoil0")

##### FIGURES TO CHECK DATA #####
## ----HourlyDataTair
climate %>% 
  filter(variable == "Tair") %>% 
  ggplot(aes(x = dateTime, y = value, colour = site)) + geom_line() + labs(y = "Air temperature in °C") + facet_grid(site ~ logger)

## ----HourlyDataTGround
climate %>% 
  filter(variable == "Tsoil0") %>% 
  ggplot(aes(x = dateTime, y = value, colour = site)) + geom_line() + labs(y = "Ground temperature in °C") + facet_grid(site ~ logger)

## ----HourlyDataTsoil5
climate %>% 
  filter(variable == "Tsoil5") %>% 
  ggplot(aes(x = dateTime, y = value, colour = site)) + geom_line() + labs(y = "Soil temperature in °C at -5cm") + facet_grid(site ~ logger)

## ----HourlyDataTsoil20
climate %>% 
  filter(variable == "Tsoil20") %>% 
  ggplot(aes(x = dateTime, y = value, colour = site)) + geom_line() + labs(y = "Soil temperature in °C at -20cm") + facet_grid(site ~ logger)

## ----HourlyDatawaterContent5
climate %>% 
  filter(variable == "waterContent5") %>% 
  ggplot(aes(x = dateTime, y = value, colour = site)) + geom_line() + labs(y = "Soil moisture in % at -5cm") + facet_grid(site ~ logger)


ZoomIntoPlot(climate, use.gather = "no", "2013-06-01 01:50:00", "2013-06-05 01:50:00", "H", "waterContent5")



monthly %>% 
  filter(variable %in% c("Tair", "Tsoil0", "Tsoil5")) %>% 
  ggplot(aes(x = month, y = value, colour = variable)) + geom_line() + facet_grid(~ site)

# Within sites
monthly %>% 
  spread(key = variable, value = value) %>%
  select(month, site, Tair, Tsoil0, Tsoil5) %>% 
  mutate(TAir_0 = Tair - Tsoil0, TAir_5 = Tair - Tsoil5, T0_5 = Tsoil0 - Tsoil5) %>% 
  gather(key = diff, value = value, -month, -site, -Tair, -Tsoil0, -Tsoil5) %>% 
  ggplot(aes(x = month, y = value, colour = diff)) + geom_point() + facet_grid(diff~site) + geom_hline(yintercept = 0)


# Daily Data
daily_dat <- distinct_weather %>% 
  mutate(day = ymd(format(dateTime, "%Y-%m-%d"))) %>%
  select(day, site, Tair, Tsoil0, Tsoil5) %>% 
  gather(key = variable, value = value, -day, -site) %>% 
  group_by(day, site, variable) %>% 
  summarise(mean = mean(value), min = min(value), max = max(value), diff = max - min) %>% 
  mutate(month= month(day))

ggplot(daily_dat, aes(x = day, y = mean, colour = site)) + geom_line() + facet_grid(site ~ variable)


ggplot(daily_dat, aes(x = day, y = diff, colour = month)) + geom_point(aes(colour = month)) + 
  scale_colour_gradient(low = "red", high = "blue") +
  facet_wrap(variable ~site)

ggplot(daily_dat, aes(x = day, y = diff)) + geom_point(aes(colour = factor(month))) + 
  scale_colour_brewer(palette = "Paired") +
  facet_wrap(variable ~site)





#### MONTHLY DATA ####

monthlyClimate <- climate %>%
  mutate(month = lubridate::ymd(format(dateTime, "%Y-%m-15"))) %>% 
  select(-dateTime) %>%
  filter(!is.na(value), is.na(flag)) %>%
  group_by(logger, site, month, variable) %>%
  summarise(meanV = mean(value), sumV = sum(value), n = n()) %>%
  mutate(value = ifelse(variable == "rain", sumV, meanV)) %>%
  filter(n > 6 * 24 * 7 * 3) %>% #at least three weeks of data
  select(-meanV, -sumV, -n)

# add missing months
full_grid <- expand.grid(logger = unique(monthlyClimate$logger), variable = unique(monthlyClimate$variable), site = unique(monthlyClimate$site), month = seq(min(monthlyClimate$month), max(monthlyClimate$month), by = "month"))

monthlyClimate <- left_join(full_grid, monthlyClimate) %>% tbl_df()
dim(monthlyClimate)

library("cowplot")
MonthlyAirTemperaturePlot <- monthlyClimate %>% 
  filter(logger == "gradient", variable == "Tair") %>% 
  mutate(site = plyr::mapvalues(site, c("H", "A", "M", "L"), c("4100 m", "3800 m", "3500 m", "3000 m"))) %>% 
  ggplot(aes(x = month, y = value, colour = site)) +
  geom_line() +
  scale_color_manual(values = c("blue", "lightblue", "orange", "red")) +
  labs(x = "", y = "Mean monthly \n air temperature in °C")


