library(tidyverse)
library(raster)
library(sp)
library(rgdal)
library(data.table)


# Coordinates
coords <- data.frame(site = c("L", "M", "A", "H"),
                     lat = c(29.84347, 29.86192, 29.88911, 29.90742),
                     long = c(102.0343, 102.036, 102.0173, 102.0118))

coords <- data.frame(lat = c(29.84347, 29.86192, 29.88911, 29.90742, 29.647943), 
                     long = c(102.0343, 102.036, 102.0173, 102.0118, 102.117826))
coordinates(coords) <- ~ long + lat

# Annual precipitation
prec <- list.files(path = "~/Desktop/wc2.0_30s_prec/", full.names = TRUE, pattern = "tif$") %>% 
  map(raster) %>% 
  map(raster::extract, y = coords) %>% 
  unlist

crossing(Month = factor(month.abb, levels = month.abb),
         Site = c("L", "M", "A", "H", "Station")) %>% 
  mutate(MonthlyPrec = prec) %>% 
  group_by(Site) %>% 
  summarise(sum = sum(MonthlyPrec))


# Mean annual temperature
temp <- list.files(path = "~/Desktop/wc2.0_30s_tavg/", full.names = TRUE, pattern = "tif$") %>% 
  map(raster) %>% 
  map(raster::extract, y = coords) %>% 
  unlist

crossing(Month = factor(month.abb, levels = month.abb),
         Site = factor(c("L", "M", "A", "H", "Station"), levels = c("L", "M", "A", "H", "Station"))) %>% 
  mutate(MonthlyTemp = temp) %>% 
  filter(Month %in% c("May", "Jun", "Jul", "Aug")) %>% 
  group_by() %>% 
  summarise(mean = mean(MonthlyTemp))


load(file = "climate/climate.Rdata", verbose = TRUE)
load(file = "climate/climate_month.Rdata", verbose = TRUE)

climate <- setDT(climate)

# Air Temperature
climate %>% 
  select(site, dateTime, Tair) %>% 
  mutate(month = lubridate::ymd(format(dateTime, "%Y-%m-15"))) %>% 
  select(-dateTime) %>%
  #filter(month(month) %in% c(5:8)) %>% 
  # monthly values
  group_by(site, month) %>% 
  filter(!is.na(Tair), !is.na(year(month)), year(month) > 2012, year(month) < 2016) %>% 
  summarise(n = n(), Tmean = mean(Tair, na.rm = TRUE)) %>% 
  # annual values
  group_by(site, year = year(month)) %>% 
  summarise(n = n(), Tmean = mean(Tmean, na.rm = TRUE)) %>%
  filter(n > 4) %>% 
  group_by(site) %>% 
  summarise(Tmean = mean(Tmean, na.rm = TRUE))
  

# SoilMoisture
climate %>% 
  select(site, dateTime, waterContent5) %>% 
  mutate(month = lubridate::ymd(format(dateTime, "%Y-%m-15"))) %>% 
  select(-dateTime) %>%
  filter(month(month) %in% c(5:8)) %>% 
  # monthly values
  group_by(site, month) %>% 
  filter(!is.na(waterContent5), !is.na(year(month))) %>% 
  summarise(n = n(), waterContent5mean = mean(waterContent5, na.rm = TRUE)) %>% 
  # annual values
  group_by(site, year = year(month)) %>% 
  summarise(n = n(), waterContent5mean = mean(waterContent5mean)) %>%
  group_by(site) %>% 
  summarise(waterContent5mean = mean(waterContent5mean))


# Relative humidity
climate %>% 
  select(site, dateTime, RH) %>% 
  mutate(month = lubridate::ymd(format(dateTime, "%Y-%m-15"))) %>% 
  select(-dateTime) %>%
  filter(month(month) %in% c(5:8)) %>% 
  # monthly values
  group_by(site, month) %>% 
  filter(!is.na(RH), !is.na(year(month))) %>% 
  summarise(n = n(), RHmean = mean(RH, na.rm = TRUE)) %>% 
  # annual values
  group_by(site, year = year(month)) %>% 
  summarise(n = n(), RHmean = mean(RHmean)) %>%
  group_by(site) %>% 
  summarise(RHmean = mean(RHmean))


# Soil organic matter
soc <- read_excel(path = "SOC_Transplant.xlsx", skip = 2)

soc %>% 
  rename(depth = `depth(cm)`, SOC = `TOC(%)`) %>% 
  fill(elevation, replicate) %>% 
  group_by(elevation, depth) %>% 
  summarise(mean = mean(SOC, na.rm = TRUE)) %>% 
  filter(depth == 5) %>% 
  ggplot(aes(x = elevation, y = mean)) +
  geom_point() 
