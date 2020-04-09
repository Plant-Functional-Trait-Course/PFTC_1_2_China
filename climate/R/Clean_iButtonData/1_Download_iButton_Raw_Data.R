##############################
### DOWNLOAD DATA FROM OSF ###
##############################

#devtools::install_github("Between-the-Fjords/dataDownloader")
library("dataDownloader")

# Download zip file from OSF
get_file(node = "f3knq",
         file = "China_H_OTC_Tomst_2020.zip",
         path = "climate/data_raw/iButton",
         remote_path = "RawData/RawData_Climate")


# Unzip files
zipFile <- "climate/data_raw/iButton/China_H_OTC_Tomst_2020.zip"
outDir <- "climate/data_raw/iButton/"
unzip(zipFile, exdir = outDir)

