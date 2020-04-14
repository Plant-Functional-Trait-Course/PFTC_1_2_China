#######################
 ### CLEAN RAW DATA ###
#######################

### Libraries
library("tidyverse")
library("lubridate")
library("readxl")
library("stringi")
library("data.table")

pn <- . %>% print(n = Inf)

#### READ IN FILES ####
# Extract file names from all iButton files to create dictionary
myfiles <- dir(path = "climate/data_raw/iButton/", pattern = "xls", recursive = TRUE, full.names = TRUE)

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
