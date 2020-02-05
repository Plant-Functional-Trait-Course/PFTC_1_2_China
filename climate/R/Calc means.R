
load(file = "climate/climate_month.Rdata", verbose = TRUE)

climate_month %>% 
  filter(variable %in% c("Tsoil0", "Tsoil5", "Tair"), !is.na(value)) %>% 
  group_by(logger, site, variable) %>% 
  summarise(n = n(), mean = mean(value, na.rm = TRUE)) %>%
  filter(n > 30) %>% 
  select(-n) %>% 
  spread(key = site, value = mean)


# only summer temp
climate_month %>% 
  filter(variable %in% c("Tsoil0", "Tsoil5", "Tair"), !is.na(value), month(month) %in% c(5, 6, 7, 8)) %>% 
  group_by(logger, site, variable) %>% 
  summarise(n = n(), mean = mean(value, na.rm = TRUE)) %>% 
  filter(n > 10) %>% 
  select(-n) %>% 
  spread(key = site, value = mean)


# Summer temp from iButtons
monthlyiButton %>% 
  select(-Tse, -Tmin, -Tmax) %>% 
  #gather(key = TempVar, value = value, Tmean, Tmin, Tmax) %>% 
  group_by(site, treatment, depth) %>% 
  summarise(n = n(), mean = mean(Tmean, na.rm = TRUE)) %>%
  select(-n) %>% 
  spread(key = site, value = mean) %>% print (n = 100)
