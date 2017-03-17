
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


## ----irgendwas
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







## ----HourlyData
otcc %>% 
  select(dateTime, site, Tair30, Tsoil0, Tsoil5) %>% 
  gather(key = variable, value = value, -dateTime, -site) %>% 
  ggplot(aes(x = dateTime, y = value, colour = site)) + geom_line() + facet_grid(site ~ variable)

## ----CorrelationsWithinSite
### Correlations
# within sites
CorrT30_5 <- ggplot(otcc, aes(x = Tair30, y = Tsoil0)) + geom_point() + facet_wrap(~ site) + geom_abline(intercept = 0, slope = 1, colour = "grey")
CorrT30_0 <- ggplot(otcc, aes(x = Tair30, y = Tsoil5)) + geom_point() + facet_wrap(~ site) + geom_abline(intercept = 0, slope = 1, colour = "grey")
CorrT0_5 <- ggplot(otcc, aes(x = Tsoil0, y = Tsoil5)) + geom_point() + facet_wrap(~ site) + geom_abline(intercept = 0, slope = 1, colour = "grey")

## ----VariationLoggerSite
# Variation between loggers within site
diff.logger <- otcc %>% 
  select(dateTime, site, Tair30, Tsoil0, Tsoil5) %>% 
  mutate(T30_0 = Tair30 - Tsoil0, T30_5 = Tair30 - Tsoil5, T0_5 = Tsoil0 - Tsoil5) 

ggplot(diff.logger, aes(x = dateTime, y = T30_0)) + geom_point() + facet_wrap(~site)
ggplot(diff.logger, aes(x = dateTime, y = T30_5)) + geom_point() + facet_wrap(~site)
ggplot(diff.logger, aes(x = dateTime, y = T0_5)) + geom_point() + facet_wrap(~site)


## ----DailyData
### Daily data
daily_dat <- otcc %>% 
  mutate(day = ymd(format(dateTime, "%Y-%m-%d"))) %>%
  select(day, site, Tair30, Tsoil0, Tsoil5) %>% 
  gather(key = variable, value = value, -day, -site) %>% 
  group_by(day, site, variable) %>% 
  summarise(mean = mean(value), min = min(value), max = max(value), diff = max - min)

ggplot(daily_dat, aes(x = day, y = mean, colour = site)) + geom_line() + facet_grid(site ~ variable)


## ----DailyDiffMinMax
ggplot(daily_dat, aes(x = day, y = diff, colour = site)) + geom_point() + facet_wrap(variable ~site)

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


## ----DailyVarWithinSite
# Within sites
daily_dat %>% 
  select(day, variable, site, mean) %>% 
  spread(key = variable, value = mean) %>%
  ggplot(aes(x = Tair30, y = Tsoil0)) + geom_point() + facet_wrap(~ site) + geom_abline(intercept = 0, slope = 1, colour = "grey")

daily_dat %>% 
  select(day, variable, site, mean) %>% 
  spread(key = variable, value = mean) %>%
  ggplot(aes(x = Tair30, y = Tsoil5)) + geom_point() + facet_wrap(~ site) + geom_abline(intercept = 0, slope = 1, colour = "grey")

daily_dat %>% 
  select(day, variable, site, mean) %>% 
  spread(key = variable, value = mean) %>%
  ggplot(aes(x = Tsoil0, y = Tsoil5)) + geom_point() + facet_wrap(~ site) + geom_abline(intercept = 0, slope = 1, colour = "grey")






### Monthly data
otc_month$site <- factor(otc_month$site, levels = c("H", "A", "M", "L"))

## ----MonthlyData
otc_month %>% 
  filter(variable %in% c("Tair30", "Tsoil0", "Tsoil5")) %>% 
  ggplot(aes(x = month, y = value, colour = variable)) + geom_line() + facet_grid(~ site)

## ----MonthlyVarBetweenSite
# Between site
site.dat <- otc_month %>% 
  filter(variable %in% c("Tair30", "Tsoil0", "Tsoil5")) %>% 
  spread(key = site, value = value)
p1 <- ggplot(site.dat, aes(x = H, y = A, colour = variable)) + geom_point() + geom_abline(intercept = 0, slope = 1, colour = "grey")
p2 <- ggplot(site.dat, aes(x = H, y = M, colour = variable)) + geom_point() + geom_abline(intercept = 0, slope = 1, colour = "grey")
p3 <- ggplot(site.dat, aes(x = H, y = L, colour = variable)) + geom_point() + geom_abline(intercept = 0, slope = 1, colour = "grey")
p4 <- ggplot(site.dat, aes(x = A, y = M, colour = variable)) + geom_point() + geom_abline(intercept = 0, slope = 1, colour = "grey")
p5 <- ggplot(site.dat, aes(x = A, y = L, colour = variable)) + geom_point() + geom_abline(intercept = 0, slope = 1, colour = "grey")
p6 <- ggplot(site.dat, aes(x = M, y = L, colour = variable)) + geom_point() + geom_abline(intercept = 0, slope = 1, colour = "grey")
cowplot::plot_grid(p1, p2, p3, p4, p5, p6, labels = c("a)", "b)", "c)", "d)", "e)", "f)"), nrow = 2, ncol = 3)

## ----MonthlyVarWithinSite
# Within sites
otc_month %>% 
  spread(key = variable, value = value) %>%
  ggplot(aes(x = Tair30, y = Tsoil0)) + geom_point() + facet_wrap(~ site) + geom_abline(intercept = 0, slope = 1, colour = "grey")

otc_month %>% 
  spread(key = variable, value = value) %>%
  ggplot(aes(x = Tair30, y = Tsoil5)) + geom_point() + facet_wrap(~ site) + geom_abline(intercept = 0, slope = 1, colour = "grey")

otc_month %>% 
  spread(key = variable, value = value) %>%
  ggplot(aes(x = Tsoil0, y = Tsoil5)) + geom_point() + facet_wrap(~ site) + geom_abline(intercept = 0, slope = 1, colour = "grey")


## ----MonthlyVarWithinSiteDiff
# same plot but as difference between loggers over time
otc_month %>% 
  spread(key = variable, value = value) %>%
  select(month, site, Tair30, Tsoil0, Tsoil5) %>% 
  mutate(T30_0 = Tair30 - Tsoil0, T30_5 = Tair30 - Tsoil5, T0_5 = Tsoil0 - Tsoil5) %>% 
  gather(key = diff, value = value, -month, -site, -Tair30, -Tsoil0, -Tsoil5) %>% 
  ggplot(aes(x = month, y = value, colour = diff)) + geom_point() + facet_grid(diff~site) + geom_hline(yintercept = 0)






