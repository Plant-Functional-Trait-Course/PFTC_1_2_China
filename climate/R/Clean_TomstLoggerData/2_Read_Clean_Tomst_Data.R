#############################
### READ IN TOMST LOGGERS ###
#############################

# load libraries
library("tidyverse")
library("readr")
library("readxl")
library("lubridate")
#devtools::install_github("Between-the-Fjords/dataDownloader")
library("dataDownloader")


### Read in files
files <- dir(path = "climate/data_raw/TomsT/", pattern = "\\.xlsx$", full.names = TRUE, recursive = TRUE)

# Function to read in data
temp <- map_df(set_names(files), function(file) {
  file %>% 
    set_names() %>% 
    map_df(~ read_excel(path = file, col_names = TRUE))
}, .id = "File") %>% 
  # rename column names
  rename("DateTime" = `Date & time`, "TMS3Number" = `TMS3 number`, "SoilTemperature" = "Soil temperature -6 cm", "GroundTemperature" = "Surface temperature", "AirTemperature" = "Air temperature +12 cm", "Period" = "period in 640 μs", "Error" = "error by probe", "TempCorrection" = "temperature correction", "RawSoilmoisture" = "Volumetric soil moisture without temp correction", "VolumetricMoisture" = "Vol. moisture") %>% 
  mutate(DateTime = ymd_hm(DateTime),
         Treatment = substr(basename(File), 1, 1),
         Plot = substr(basename(File), 2, 2),
         Site = "H") %>% 
  # remove befor
  filter(DateTime > "2019-09-13 00:00:00")


# Clean files
temp_clean <- temp %>% 
  pivot_longer(cols = c(SoilTemperature, GroundTemperature, AirTemperature), names_to = "Variable", values_to = "Temperature") %>% 
  mutate(Treatment = recode(Treatment, "C" = "Control", "W" = "OTC"),
         YearMonth = ymd(paste(year(DateTime), month(DateTime), "15"))) %>% 
  select(File, DateTime, VolumetricMoisture:YearMonth)
  
#write_csv(temp_clean, path = "climate/data_cleaned/China_2019_Climate_TomstLogger.csv")