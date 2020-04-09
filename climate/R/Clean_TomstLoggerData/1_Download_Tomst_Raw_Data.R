#devtools::install_github("Between-the-Fjords/dataDownloader")
library("dataDownloader")

# Download zip file from OSF
get_file(node = "f3knq",
         file = "China_Gradient_OTC_iButton_2017.zip",
         path = "climate/data_raw/TomsT",
         remote_path = "RawData/RawData_Climate")


# Unzip files
zipFile <- "climate/data_raw/TomsT/China_Gradient_OTC_iButton_2017.zip"
outDir <- "climate/data_raw/TomsT/"
unzip(zipFile, exdir = outDir)

