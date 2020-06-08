### Download data from OSF

#devtools::install_github("Between-the-Fjords/dataDownloader")
library("dataDownloader")

#Download community data from OSF
get_file(node = "f3knq",
         file = "raw_community_data.zip",
         path = "community/databaseSetup/data/",
         remote_path = "RawData/RawData_Community")

# Unzip files
zipFile <- "community/databaseSetup/data/raw_community_data.zip"
outDir <- "community/databaseSetup/"
unzip(zipFile, exdir = outDir)
