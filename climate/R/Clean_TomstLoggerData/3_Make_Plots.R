##################
### MAKE PLOTS ###
##################

# load libraries
library("tidyverse")
library("readr")
library("readxl")
library("lubridate")
#devtools::install_github("Between-the-Fjords/dataDownloader")
library("dataDownloader")

## ----TomstOTCPlot
#### Load clean data
# Download OSF
#Download cleaned data from OSF
get_file(node = "f3knq",
         file = "China_2019_Climate_TomstLogger.csv",
         path = "climate/data_cleaned",
         remote_path = "Climate")

# Read in data
temp <- read_csv(file = "climate/data_cleaned/China_2019_Climate_TomstLogger.csv")


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
