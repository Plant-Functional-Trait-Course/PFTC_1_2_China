##### FIGURES TO CHECK DATA #####


## ----UncleanTair
ggplot(weather, aes(x = dateTime, y = Tair, colour = site)) + geom_line() + labs(y = "Air temperature in ...") + facet_grid(~ site) + theme_bw()

## ----UncleanwaterContent5
ggplot(weather, aes(x = dateTime, y = waterContent5, colour = site)) + geom_line() + labs(y = "Soil moisture at -5cm") + facet_grid(~ site) + theme_bw()


#### SOME PLOTS ####
## ----DataTair
p <- ggplot(climate, aes(x = dateTime, y = Tair, colour = site)) + geom_line() + labs(y = "Air temperature in °C") + facet_grid(site ~ logger) + theme_bw()
p

## ----DataTsoil0
p + aes(y = Tsoil0) + labs(y = "Ground temperature in °C")


## ----DataTsoil5
#p + aes(y = Tsoil5) + geom_point(size = 0.01) + labs(y = "Soil temperature in °C at -5cm") 
p + aes(y = Tsoil5) + geom_line() + labs(y = "Soil temperature in °C at -5cm") 


## ----DataTsoil20
p + aes(y = Tsoil20) + labs(y = "Soil temperature in °C at -20cm") 


## ----DatawaterContent5
p + aes(y = waterContent5) + labs(y = "Soil moisture in °C at -5cm") 


## ----Datarain
p + aes(y = rain) + geom_point(size = 0.01) + facet_grid(~site) + labs(y = "Rain in mm") 

## ----DataPAR
p + aes(y = PAR) + labs(y = "PAR") 

## ----DatasolarRadiation
p + aes(y = solarRadiation) + geom_point(size = 0.01) + facet_grid(~site)  + labs(y = "Solar Radiation") 

## ----DatawindSpeed
p + aes(y = windSpeed) + facet_grid(~site)  + labs(y = "Wind Speed") 

## ----DatawindDirection
p + aes(y = windDirection) + facet_grid(~site)  + labs(y = "Wind Direction") 

## ----DataRH
p + aes(y = RH) + facet_grid(~site) + geom_point(size = 0.01) + labs(y = "Relative humidity") 

## ----DataUV
p + aes(y = UV) + facet_grid(~site)  + geom_point(size = 0.01) + labs(y = "UV") 



## ----AirTemperatureGradient
#### MONTHLY DATA ####
library("cowplot")
MonthlyAirTemperaturePlot <- climate_month %>% 
  filter(logger == "gradient", variable == "Tair") %>% 
  mutate(site = plyr::mapvalues(site, c("H", "A", "M", "L"), c("4100 m", "3800 m", "3500 m", "3000 m"))) %>% 
  ggplot(aes(x = month, y = value, colour = site)) +
  geom_line() +
  scale_color_manual(values = c("blue", "lightblue", "orange", "red")) +
  labs(x = "", y = "Mean monthly \n air temperature in °C") + theme_bw()
MonthlyAirTemperaturePlot


## ----TableWarmestMonth
climate_month %>% 
  filter(variable %in% c("Tsoil0", "Tsoil5", "Tsoil20")) %>%
  mutate(variable = factor(variable, levels = c("Tsoil0", "Tsoil5", "Tsoil20"))) %>% 
  filter(month(month) %in% c("5", "6", "7", "8", "9")) %>% 
  group_by(logger, site, variable) %>% 
  summarise(mean = mean(value, na.rm = TRUE)) %>%
  spread(key = variable, value = mean)


## ----GradientVSOtc
climate_month %>% 
  filter(variable %in% c("Tair", "Tsoil0", "Tsoil5", "Tsoil20")) %>%
  mutate(variable = factor(variable, levels = c("Tair", "Tsoil0", "Tsoil5", "Tsoil20"))) %>% 
  mutate(elevation = plyr::mapvalues(site, c("H", "A", "M", "L"), c("4100", "3800", "3500", "3000"))) %>%
  mutate(elevation = as.numeric(as.character(elevation))) %>%
  filter(month(month) %in% c("7")) %>% 
  group_by(variable, elevation, logger) %>% 
  summarise(mean = mean(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = elevation, y = mean, colour = logger)) + geom_line() + facet_wrap(~variable) + theme_bw()





## ----MonthlyVarBetweenSite
# Between site
site.dat <- climate_month %>% 
  filter(variable %in% c("Tair", "Tsoil0", "Tsoil20")) %>% 
  spread(key = site, value = value)
p1 <- ggplot(site.dat, aes(x = H, y = A, colour = variable)) + geom_point() + geom_abline(intercept = 0, slope = 1, colour = "grey") + facet_wrap(~ logger)
p2 <- ggplot(site.dat, aes(x = H, y = M, colour = variable)) + geom_point() + geom_abline(intercept = 0, slope = 1, colour = "grey") + facet_wrap(~ logger)
p3 <- ggplot(site.dat, aes(x = H, y = L, colour = variable)) + geom_point() + geom_abline(intercept = 0, slope = 1, colour = "grey") + facet_wrap(~ logger)
p4 <- ggplot(site.dat, aes(x = A, y = M, colour = variable)) + geom_point() + geom_abline(intercept = 0, slope = 1, colour = "grey") + facet_wrap(~ logger)
p5 <- ggplot(site.dat, aes(x = A, y = L, colour = variable)) + geom_point() + geom_abline(intercept = 0, slope = 1, colour = "grey") + facet_wrap(~ logger)
p6 <- ggplot(site.dat, aes(x = M, y = L, colour = variable)) + geom_point() + geom_abline(intercept = 0, slope = 1, colour = "grey") + facet_wrap(~ logger)
cowplot::plot_grid(p1, p2, p3, p4, p5, p6, labels = c("a)", "b)", "c)", "d)", "e)", "f)"), nrow = 2, ncol = 3)


## ----MonthlyVarWithinSiteDiff
climate_month %>% 
  spread(key = variable, value = value) %>%
  select(month, site, logger, Tair, Tsoil0, Tsoil20) %>% 
  mutate(TAir_0 = Tair - Tsoil0, TAir_20 = Tair - Tsoil20, T0_20 = Tsoil0 - Tsoil20) %>% 
  gather(key = diff, value = value, -month, -site, -logger, -Tair, -Tsoil0, -Tsoil20) %>% 
  mutate(difflog = paste(diff, logger)) %>% 
  ggplot(aes(x = month, y = value, colour = month(month))) + geom_line() + facet_grid(diff ~ site) + geom_hline(yintercept = 0) + theme_bw()






## ----DailyDiffMinMax
#### DAILY DATA ####
### Daily data
daily_dat %>% 
  filter(logger == "gradient") %>% 
  ggplot(aes(x = day, y = diff, colour = month)) + geom_line() + labs(x = "", y = "Daily diff. (max - min)") + facet_grid(variable ~site) + theme_bw()

daily_dat %>% 
  filter(logger == "otc") %>% 
  ggplot(aes(x = day, y = diff, colour = month)) + geom_line() + labs(x = "", y = "Daily diff. (max - min)") + facet_grid(variable ~site) + theme_bw()



## ----DailyVarBetweenSite
site.daily <- daily_dat %>% 
  select(day, variable, site, mean) %>% 
  spread(key = site, value = mean)
p1 <- ggplot(site.daily, aes(x = H, y = A, colour = variable)) + geom_point() + geom_abline(intercept = 0, slope = 1, colour = "grey")
p2 <- ggplot(site.daily, aes(x = H, y = M, colour = variable)) + geom_point() + geom_abline(intercept = 0, slope = 1, colour = "grey")
p3 <- ggplot(site.daily, aes(x = H, y = L, colour = variable)) + geom_point() + geom_abline(intercept = 0, slope = 1, colour = "grey")
p4 <- ggplot(site.daily, aes(x = A, y = M, colour = variable)) + geom_point() + geom_abline(intercept = 0, slope = 1, colour = "grey")
p5 <- ggplot(site.daily, aes(x = A, y = L, colour = variable)) + geom_point() + geom_abline(intercept = 0, slope = 1, colour = "grey")
p6 <- ggplot(site.daily, aes(x = M, y = L, colour = variable)) + geom_point() + geom_abline(intercept = 0, slope = 1, colour = "grey")
cowplot::plot_grid(p1, p2, p3, p4, p5, p6, labels = c("a)", "b)", "c)", "d)", "e)", "f)"), nrow = 2, ncol = 3)




## ----testThings
climate %>% 
  filter(dateTime > as.POSIXct("2014-03-23 03:00:00", tz = "Asia/Shanghai"), dateTime < as.POSIXct("2014-03-23 04:00:00", tz = "Asia/Shanghai"), site == "H", variable == "Tsoil0", logger == "gradient") %>% 
  ggplot(aes(x = dateTime, y = value)) + geom_line()

+ stat_smooth(method = "loess", span = 0.005, n = 200)

climate %>% 
  filter(variable %in% c("Tair", "Tsoil0", "Tsoil20", "PAR")) %>% 
  filter(site == "H") %>% 
  filter(logger == "otc") %>% 
  filter(dateTime > as.POSIXct("2014-11-10 01:50:00", tz = "Asia/Shanghai"), dateTime < as.POSIXct("2014-11-23 01:50:00", tz = "Asia/Shanghai")) %>% 
  ggplot(aes(x = ymd_hm(paste("2017-1-1", format(dateTime, "%H:%M"))), y = value, colour = yday(dateTime), group = lubridate::yday(dateTime))) + geom_smooth() + facet_wrap(~ variable, scales = "free_y")

