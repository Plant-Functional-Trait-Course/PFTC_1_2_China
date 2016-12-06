#read climate data

#load libraries
library("lubridate")

source("climate/read_badly_formatted_data.R") # badly formatted data in data/csv
source("climate/read_well_formatted_data.R") # better formatted data in data/2016
weather2 <- weather#make safe copy

#combine output of well and badly formatted dat
weather <- bind_rows(weather2, dat)

#convert site to factor (to ensure plotting order)
weather$site <- factor(weather$site, levels = c("H", "A", "M", "L"))

save(weather, file = "climate/weather_unclean.Rdata")

#unit conversion  - probably F to C
#clean climate

library("dplyr")
library("ggplot2")

F_C <- function(x)(x-32)/1.8

#assume files with median Tair >20 are in F, otherwise C
weather <- weather %>%
  group_by(file) %>%
  mutate(probableTempUnit = ifelse(median(Tair, na.rm = TRUE) > 20, "F", "C")) %>%
  mutate(Tair = ifelse(probableTempUnit == "F", F_C(Tair), Tair)) %>%
  mutate(Tsoil0 = ifelse(probableTempUnit == "F", F_C(Tsoil0), Tsoil0)) %>%
  mutate(Tsoil5 = ifelse(probableTempUnit == "F", F_C(Tsoil5), Tsoil5)) %>%
  mutate(Tsoil20 = ifelse(probableTempUnit == "F", F_C(Tsoil20), Tsoil20)) 


#zap impossible values
weather <- weather %>% 
  mutate(Tair = ifelse(Tair > 100 | Tair < -50, NA, Tair)) %>%
  mutate(Tsoil0 = ifelse(Tsoil0 > 60 | Tsoil0 < -50, NA, Tsoil0)) %>%
  mutate(Tsoil5 = ifelse(Tsoil5 > 50 | Tsoil5 < -5, NA, Tsoil5)) %>%
  mutate(Tsoil20 = ifelse(Tsoil20 > 50 | Tsoil20 < -5, NA, Tsoil20)) 

#remove minimal variance in Tsoil0 Toil5

weather <- weather %>% 
  group_by(file) %>%
  mutate(sd = sd(Tsoil5, na.rm = TRUE)) %>% #Tsoil5
  mutate(Tsoil5 = ifelse(sd < 0.02, NA, Tsoil5)) %>%
  mutate(sd = sd(Tsoil0, na.rm = TRUE)) %>% #Tsoil0
  mutate(Tsoil0 = ifelse(sd < 0.1, NA, Tsoil0)) %>%
  select(-sd)

#zap zero variance solarRadiation variability (all in site A)
weather <- weather %>% 
  group_by(file) %>%
  mutate(sd = sd(solarRadiation, na.rm = TRUE)) %>% #Tsoil5
  mutate(solarRadiation = ifelse(sd < 0.02, NA, solarRadiation)) %>%
  select(-sd)

#soil moisture - zap impossible values
ggplot(weather, aes(x = dateTime, y = waterContent20, group = file, colour = site)) + geom_path() + facet_wrap(~site)

weather <- weather %>%
  mutate(waterContent5 = ifelse(waterContent5 < 0 | waterContent5 > 1, NA, waterContent5)) %>%
  mutate(waterContent20 = ifelse(waterContent20 < 0 | waterContent20 > 1, NA, waterContent20))

save(weather, file = "climate/weather.Rdata")

## deal with duplicates (from overlapping files)

#check variance is low amongst duplicates

weather %>% 
  group_by(dateTime, site) %>% 
   ungroup() %>%
    mutate(dateTime = ymd(format(dateTime, "%Y-%m-%d"))) %>%
   select(-notes, -Batt, -probableTempUnit, -extra) %>%
  select(file, dateTime, site, Tsoil5, Tsoil0, Tsoil20, waterContent5, waterContent20) %>%
  group_by(file, dateTime, site) %>%
  mutate(n = n()) %>% 
  filter(n == 6*24) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup() %>%
  select(-file, -n) %>%
  gather(key = variable, value = value, - dateTime, -site) %>% 
  group_by(dateTime, variable, site) %>% 
  summarise(delta = diff(range(value))) %>% 
  arrange(desc(delta))



weather %>% filter(dateTime == ymd_hms("2014-04-18 13:30:00", tz = "Asia/Shanghai"), site == "H") %>% select(dateTime, site, PAR, Tair)


weather %>% 
  ungroup() %>% 
  filter(site == "H", dateTime > ymd("2015-06-10", tz = "Asia/Shanghai") & dateTime < ymd("2015-06-20", tz = "Asia/Shanghai")) %>%
  select(-notes, -Batt, -probableTempUnit, -extra) %>%
  gather(key = variable, value = value, -dateTime,-file, -site) %>% 
  filter(!is.na(value)) %>%
  ggplot(aes(x = dateTime, y = value, colour = file)) + geom_path(size = .1, show.legend = FALSE) + facet_wrap(~variable, scales = "free_y") 



weather %>% filter(site == "H") %>% group_by(file) %>% summarise(cor = cor(rain, windSpeed))

weather %>%
  ggplot(aes(x = windSpeed, y = rain, colour = site)) + 
    geom_point() +
    facet_wrap(~site, scales = "free_y") + 
    xlim(c(NA, 26)) 

weather %>%
  ggplot(aes(x =rain, fill = site)) + 
    geom_histogram() +
  facet_wrap(~site, scales = "free")

weather %>% filter(site == "A") %>% ggplot(aes(x = dateTime, y = windDirection, colour = site, group = file)) + geom_path() + facet_wrap(~site)

weather %>% filter(site == "A") %>% ggplot(aes(x = dateTime, y = windSpeed, colour = site, group = file)) + geom_path() + facet_wrap(~site)

weather %>% filter(site == "A") %>% ggplot(aes(x = windSpeed, y = windDirection, colour = site, group = file)) + geom_point() + facet_wrap(~site)

#UV
ggplot(weather, aes(x = dateTime, y = UV, colour = site, group  = file)) + geom_path() + facet_wrap(~site)

## delete bad data
weather <- weather %>% 
  # delete bad UV from H
  mutate(UV = ifelse(site == "H", NA, UV))

#remove duplicates - start with last file 

distinct_weather <- weather %>% 
  ungroup() %>% 
  mutate(file  = reorder(file, dateTime, max, na.rm = TRUE)) %>%
  arrange(file) %>%
  distinct(site, dateTime, .keep_all = TRUE) %>%
  select(-file, -probableTempUnit, -notes, -Batt, -extra)


save(distinct_weather, file = "climate/clean_weather.Rdata")

#### make monthly climate ####
monthly <- distinct_weather %>% 
  mutate(month = ymd(format(dateTime, "%Y-%m-15"))) %>%
  select(-dateTime) %>%
  gather(key = variable, value = value, -site, -month) %>%
  group_by(site, month, variable) %>%
  filter(!is.na(value)) %>%
  summarise(meanV = mean(value), sumV = sum(value), n = n()) %>%
  mutate(value = ifelse(variable == "rain", sumV, meanV)) %>%
  filter(n > 6 * 24 * 7 * 3) %>% #at least three weeks of data
  select(-meanV, -sumV, -n)

#add missing months
full_grid <- expand.grid(variable = unique(monthly$variable), site = unique(monthly$site), month = seq(min(monthly$month), max(monthly$month), by = "month"))

monthly <- left_join(full_grid, monthly) %>%tbl_df()

save(monthly, file = "climate/monthly_climate.Rdata")

####Annual mean####
annual1 <- monthly %>% 
  mutate(month = month(month, label = TRUE, abbr = FALSE)) %>%
  group_by(variable, site, month) %>%
  summarise(meanV = mean(value, na.rm = TRUE), n = sum(!is.na(value)))

#check data for all months
annual1 %>% filter(n >0) %>% 
  group_by(site, variable) %>% 
  summarise(n = n()) %>% 
  filter (n < 12) # should be empty

annual <- annual1 %>% 
  group_by(site, variable) %>% 
  summarise(value = mean(meanV)) %>%
  mutate(value = ifelse(variable == "rain", value * 12, value)) # rain should be sum not mean


annual %>% spread(key = variable, value = value)

#
save(monthly, annual, file = paste0("climate/month_annual", Sys.Date(), ".Rdata"))
