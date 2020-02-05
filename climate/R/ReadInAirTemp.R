# Read in climate data from weather station
## ----AirTempPlot
library("tidyverse")
library("lubridate")
#devtools::install_github("Between-the-Fjords/dataDownloader")
library("dataDownloader")


# Download OSF
#Download files from OSF
# get_file(node = "f3knq",
#          file = "China_2013_2016_AirTemp.csv",
#          path = "climate/data_cleaned",
#          remote_path = "Climate")


# Read in data
airtemp <- read_csv(file = "climate/data_cleaned/China_2013_2016_AirTemp.csv", col_names = TRUE)

# Plot data
sitenames <- c("H" = "High alpine", "A" = "Alpine", "M" = "Middle", "L" = "Lowland")
AirTempPlot <- ggplot(airtemp, aes(x = dateTime, y = Tair, colour = site)) +
  geom_line() +
  scale_color_brewer(palette = "RdBu", direction = -1, labels=c("High alpine", "Alpine", "Middle", "Lowland")) +
  facet_wrap(~ site, labeller=labeller(site = sitenames))
AirTempPlot
#ggsave(AirTempPlot, filename = "AirTempPlot.jpg", height = 6, width = 10, dpi = 300)
