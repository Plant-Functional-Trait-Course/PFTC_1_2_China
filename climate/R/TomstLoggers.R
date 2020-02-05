###########################
### READ IN TOMST LOGGERS ###
###########################


#load libraries
library("tidyverse")
library("readr")
library("readxl")
library("lubridate")
#devtools::install_github("Between-the-Fjords/dataDownloader")
library("dataDownloader")


### Read in files
files <- dir(path = "climate/data_raw/Tomst_2020/", pattern = "\\.xlsx$", full.names = TRUE, recursive = TRUE)

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
         Plot = substr(basename(File), 2, 2)) %>% 
  # remove befor
  filter(DateTime > "2019-09-13 00:00:00")



temp <- temp %>% 
  pivot_longer(cols = c(SoilTemperature, GroundTemperature, AirTemperature), names_to = "Variable", values_to = "Temperature") %>% 
  mutate(Treatment = recode(Treatment, "C" = "Control", "W" = "OTC"),
         YearMonth = ymd(paste(year(DateTime), month(DateTime), "15"))) 
  

## ----TomstOTCPlot
#### Load clean data
# Download OSF
#Download files from OSF
get_file(node = "f3knq",
         file = "China_2019_Climate_TomstLogger.csv",
         path = "climate/data_cleaned",
         remote_path = "Climate")

# Read in data
temp <- read_csv(file = "climate/data_cleaned/China_2019_Climate_TomstLogger.csv")


# Plotting  
# ggplot(temp, aes(x = DateTime, y = Temperature, colour = Treatment)) +
#   geom_line() +
#   scale_color_manual(values = c("grey50", "red")) +
#   facet_grid(Variable ~ Treatment) +
#   theme_bw()


TomstOTC <- temp %>% 
  group_by(YearMonth, Variable, Treatment) %>% 
  summarise(MeanTemperature = mean(Temperature), SETemperature = sd(Temperature)/sqrt(n())) %>% 
  ggplot(aes(x = YearMonth, y = MeanTemperature, ymin = MeanTemperature - SETemperature, ymax = MeanTemperature + SETemperature, colour = Treatment)) +
  #scale_x_yearmon(n = 2) +
  scale_x_date(date_breaks="1 month", date_labels="%Y %m") +
  geom_line() +
  geom_point() +
  geom_errorbar(width = 0.1) +
  labs(x = "", y = "Mean temperautre in °C") +
  scale_color_manual(values = c("grey50", "orange")) +
  facet_wrap(~ Variable) +
  theme_bw()
TomstOTC
#ggsave(TomstOTC, filename = "TomstOTC.jpg", height = 6, width = 10, dpi = 300)
