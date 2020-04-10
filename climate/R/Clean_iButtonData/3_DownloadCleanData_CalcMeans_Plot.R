#######################################################
 ### DOWNLOAD CLEAN DATA, CALC MEANS AND MAKE PLOT ###
#######################################################

### Libraries
library("tidyverse")
library("lubridate")
library("readxl")
library("stringi")
library("data.table")
#devtools::install_github("Between-the-Fjords/dataDownloader")
library("dataDownloader")


#### Download cleaned data from OSF
get_file(node = "f3knq",
         file = "China_2019_TemperatureiButton.csv",
         path = "climate/data_cleaned",
         remote_path = "Climate")

# Monthly data
get_file(node = "f3knq",
         file = "China_2019_Monthly_TemperatureiButton.csv",
         path = "climate/data_cleaned",
         remote_path = "Climate")


# Read in clean data
iButton <- read_csv(file = "climate/data_cleaned/China_2019_TemperatureiButton.csv")
monthlyiButton <- read_csv(file = "climate/data_cleaned/China_2019_Monthly_TemperatureiButton.csv")


#### CALCULATE DAILY DATA ####
dailyiButton <- iButton %>%
  mutate(date = dmy(format(date, "%d.%b.%Y"))) %>%
  group_by(date, depth, site, treatment) %>%
  summarise(n = n(), mean = mean(value), se = sd(value)/sqrt(n), min = min(value), max = max(value)) %>%
  filter(n > 6) %>%
  select(-n)


# Check each turf
iButton %>% 
  filter(depth == "air") %>% 
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  #scale_color_manual(values = c("blue", "green", "brown")) +
  facet_wrap( ~ turfID)

# Plot data from any hours of the day
iButton %>% 
  filter(between(hour(date), 3,5), depth == "air") %>% 
  mutate(month = ymd(format(date, "%Y-%m-15"))) %>% 
  select(-date) %>%
  group_by(month, site, treatment, depth) %>%
  filter(!is.na(value)) %>%
  summarise(Tmean = mean(value, na.rm = TRUE), n = n()) %>% 
  ggplot(aes(x = month, y = Tmean, color = treatment)) +
  geom_line() +
  labs(y = "Mean monthly night temperature") +
  facet_grid(depth ~ site)


# Plot day and night temperature
iButton %>% 
  filter(depth == "soil") %>% 
  mutate(date = dmy(format(date, "%d.%b.%Y"))) %>%
  group_by(date, depth, site, treatment) %>%
  summarise(n = n(), min = min(value), max = max(value)) %>% 
  mutate(diff = max - min) %>% 
  ggplot(aes(x = date, y = diff, color = treatment)) +
  geom_line() +
  facet_grid(treatment ~ site)


#### CALCULATE MONTHLY DATA ####
monthlyiButton <- iButton %>% 
  mutate(month = lubridate::ymd(format(date, "%Y-%m-15"))) %>% 
  select(-date) %>%
  group_by(month, site, treatment, depth) %>%
  filter(!is.na(value)) %>%
  summarise(n = n(), Tmean = mean(value, na.rm = TRUE), Tse = sd(value)/sqrt(n), Tmin = min(value, na.rm = TRUE), Tmax = max(value, na.rm = TRUE)) %>% 
  # remove April and September at L site, because there is too little data
  filter(n > 100) %>% 
  select(-n) %>% 
  ungroup() %>% 
  mutate(site = factor(site, levels = c("H", "A", "M", "L")))


#write.csv(monthlyiButton, file = "climate/data_cleaned/China_2019_Monthly_TemperatureiButton.csv")

# Plot monthly data by site and depth
monthlyiButton %>% 
  ggplot(aes(x = month, y = Tmean, color = treatment)) +
  geom_line() +
  facet_grid(depth ~ site)



monthlyiButton %>% 
  group_by(site, month, depth, treatment) %>% 
  summarise(Mean = mean(Tmean)) %>% 
  spread(key = treatment, value = Mean) %>% 
  mutate(Difference = OTC - C) %>% 
  filter(month(month) != 4) %>% 
  arrange(site, depth, month) %>% pn
