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
  mutate(Tsoil20 = ifelse(Tsoil20 > 50 | Tsoil20 < -5, NA, Tsoil20)) %>% 
  mutate(windSpeed = ifelse(windSpeed > 50, NA, windSpeed))

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

#zap zero values for PAR (all in site A)
weather <- weather %>% 
  group_by(file) %>%
  mutate(PAR = ifelse(PAR < 0, NA, PAR))

#soil moisture - zap impossible values
ggplot(weather, aes(x = dateTime, y = waterContent20, group = file, colour = site)) + geom_path() + facet_wrap(~site)

weather <- weather %>%
  mutate(waterContent5 = ifelse(waterContent5 < 0 | waterContent5 > 1, NA, waterContent5)) %>%
  mutate(waterContent20 = ifelse(waterContent20 < 0 | waterContent20 > 1, NA, waterContent20))

# remove remaining spikes in particular season
weather <- weather %>% 
  mutate(nummonth = month(as.POSIXlt(dateTime, format="%Y/%m/%d %H/%m/%s"))) %>% 
  mutate(season = ifelse(nummonth %in% c(12,1,2), "Winter",
                         ifelse(nummonth %in% c(3,4,5), "Spring", 
                                ifelse(nummonth %in% c(6,7,8), "Summer", "Autumn")))) %>% 
  mutate(season = factor(season, levels = c("Winter", "Spring", "Summer", "Autumn"))) %>% 
  mutate(Tsoil0 = ifelse(site == "H" & season == "Spring" & Tsoil0 < -15, NA, Tsoil0))

weather %>% 
  group_by(season, year(dateTime)) %>% 
  summarise(min = min(Tsoil20, na.rm = TRUE), max = max(Tsoil20, na.rm = TRUE))



#clean spikes
weather <- weather %>% 
  # add season to check remove spikes in different parts of the year
  mutate(nummonth = month(as.POSIXlt(dateTime, format="%Y/%m/%d %H/%m/%s"))) %>% 
  mutate(season = ifelse(nummonth %in% c(12,1,2), "Winter",
                         ifelse(nummonth %in% c(3,4,5), "Spring", 
                                ifelse(nummonth %in% c(6,7,8), "Summer", "Autumn")))) %>% 
  mutate(season = factor(season, levels = c("Winter", "Spring", "Summer", "Autumn"))) %>% 
  # Tsoil20: remove spikes in winter 2015
  mutate(Tsoil20 = ifelse(season == "Winter" & site == "H" & Tsoil20 > 2.5 , NA, Tsoil20)) %>% 
  mutate(Tsoil20 = ifelse(dateTime == "2014-11-15 04:30:00"  & site == "H" , NA, Tsoil20))  %>% 
  select(-nummonth, -season)

save(weather, file = "climate/weather.Rdata")

ggplot(weather, aes(x = dateTime, y = Tsoil20, colour = site)) + geom_line() + facet_wrap(~site)


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
monthly <- CalcMonthlyData(distinct_weather)

#add missing months
full_grid <- expand.grid(variable = unique(monthly$variable), site = unique(monthly$site), month = seq(min(monthly$month), max(monthly$month), by = "month"))

monthly <- left_join(full_grid, monthly) %>%tbl_df()

save(monthly, file = "climate/monthly_climate.Rdata")

####Annual mean####
annual <- CalcYearlyData(monthly)

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
