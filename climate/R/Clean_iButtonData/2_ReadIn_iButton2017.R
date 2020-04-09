####################################
 ### CLEAN, CALC MEANS AND PLOT ###
####################################

### Libraries
library("tidyverse")
library("lubridate")
library("readxl")
library("stringi")
library("data.table")

pn <- . %>% print(n = Inf)

#### READ IN FILES ####
# Extract file names from all iButton files to create dictionary
myfiles <- dir(path = "climate/data_raw/Tomst/", pattern = "xls", recursive = TRUE, full.names = TRUE)

myfiles <- myfiles[!grepl("^l\\.xls", basename(myfiles), ignore.case = TRUE)] # remove l.xls file, is duplicate

### Read in iButtons Function
ReadIniButtons <- function(textfile){
  print(textfile)
  
  dat <- read_excel(textfile, sheet = 1, skip = 11, col_names = FALSE)
  
  dat <- dat %>%
    setNames(nm = c("date", "value"))
  
  # Extract siteID, iButtonID and Year from file name
  dat$ID <- basename(textfile)
  dat <- dat %>%
    mutate(ID = basename(textfile))
  return(dat)
}

# Read in iButton data
mdat <- map_df(myfiles, ReadIniButtons)


#### CLEAN DATA ####
# Extract turfID and depth from ID
iButton <- mdat %>% 
  mutate(ID = gsub("\\_", "\\-", ID)) %>% # make all lines lower case
  mutate(ID = gsub("cm", "CM", ID)) %>% # change cm to upper case
  mutate(turfID = gsub("^([^-]*-[^-]*)-.*$", "\\1", ID)) %>% # replace everything after second -, brackets show first capture group
  mutate(site = substring(turfID, 1, 1)) %>% 
  mutate(treatment = sub(".*\\-", "", turfID)) %>%
  mutate(depth = stri_extract(regex = "\\d+(?=\\)*CM)", ID)) %>%  # extract depth
  select(-ID) %>%  # remove ID
  mutate(depth = plyr::mapvalues(depth, c("5",  "0",  "30"), c("soil", "ground", "air"))) %>% 
  mutate(site = factor(site, levels = c("H",  "A",  "M", "L")))


iButton <- setDT(iButton)

# Check for duplicate values
iButton %>% 
  group_by(date, turfID, depth, value) %>% 
  summarise(n = n()) %>% 
  filter(n > 1)

# remove duplicates
iButton <- iButton %>%
  group_by(date, turfID, depth, value) %>% 
  slice(1)

# Cleaning
iButton <- iButton %>% 
  ungroup() %>% 
  # remove everything before 1. May in 2017 (before put to the field)
  filter(!date < "2017-05-01 01:00:00") %>% 
  # zap strongly negative and positive spikes
  mutate(value = ifelse(depth == "soil" & value > 25, NA, value)) %>% # soil +25
  mutate(value = ifelse(depth %in% c("soil", "ground") & value < -7, NA, value)) # soil -7

#write.csv(iButton, file = "climate/data_cleaned/China_2019_TemperatureiButton.csv")


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
