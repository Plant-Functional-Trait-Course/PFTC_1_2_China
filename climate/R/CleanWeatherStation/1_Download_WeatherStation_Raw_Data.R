##############################
### DOWNLOAD DATA FROM OSF ###
##############################

#devtools::install_github("Between-the-Fjords/dataDownloader")
library("dataDownloader")

# Download zip file from OSF
get_file(node = "f3knq",
         file = "China_WeatherStation_RawData.zip",
         path = "climate/data_raw/WeatherStation",
         remote_path = "RawData/RawData_Climate")


# Unzip files
zipFile <- "climate/data_raw/WeatherStation/China_WeatherStation_RawData.zip"
outDir <- "climate/data_raw/WeatherStation/"
unzip(zipFile, exdir = outDir)

