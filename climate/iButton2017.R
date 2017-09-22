### Reading in iButton data 2017
library("tidyverse")
library("lubridate")
library("readxl")
library("stringi")

pn <- . %>% print(n = Inf)

# Extract file names from all iButton files to create dictionary
myfiles <- dir(path = paste0("~/Dropbox/iButtonDataChina"), pattern = "xls", recursive = TRUE, full.names = TRUE)

myfiles <- myfiles[!grepl("^l\\.xls", basename(myfiles), ignore.case = TRUE)] # remove l.xls file, is duplicate

#### Read in iButtons Function
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
  mutate(site = plyr::mapvalues(site, c("H",  "A",  "M", "L"), c("H",  "A",  "M", "L")))


# Check for duplicate values
iButton %>% 
  group_by(date, turfID, depth, value) %>% 
  summarise(n = n()) %>% 
  filter(n > 1)

# remove duplicates
iButton <- iButton %>%
  group_by(date, turfID, depth, value) %>% 
  mutate(n = 1:n()) %>%
  filter(n == 1)

# Clean data
iButton <- iButton %>% 
  select(-n) %>% 
  ungroup() %>% 
  # zap strongly negative and positive spikes
  mutate(value = ifelse(depth == "soil" & value > 25, NA, value)) %>% # soil +25
  mutate(value = ifelse(depth %in% c("soil", "ground") & value < -7, NA, value)) # soil -7


save(iButton, file = "TemperatureiButton.RData")

# Check each turf
iButton %>% 
  ggplot(aes(x = date, y = value, color = depth)) +
  geom_line() +
  scale_color_manual(values = c("blue", "green", "brown")) +
  facet_wrap( ~ turfID)

  
ggplot(iButton, aes(x = date, y = value, color = depth)) +
  geom_line() +
  scale_color_manual(values = c("blue", "green", "brown")) +
  facet_grid(treatment ~ site)


monthlyiButton <- iButton %>% 
  mutate(month = lubridate::ymd(format(date, "%Y-%m-15"))) %>% 
  select(-date) %>%
  group_by(month, site, treatment, depth) %>%
  filter(!is.na(value)) %>%
  summarise(Tmean = mean(value, na.rm = TRUE), n = n()) %>% 
  # no need for this, very little failure
  # filter(n > 6 * 24 * 7 * 3)  #at least three weeks of data
  select(-n) %>% 
  ungroup() %>% 
  mutate(site = factor(site)) %>% 
  mutate(site = plyr::mapvalues(site, c("A", "H", "L", "M"), c("H", "A", "M", "L")))
 

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
