#devtools::install_github("Between-the-Fjords/dataDownloader")
library("dataDownloader")

#Download community data from OSF
get_file(node = "f3knq",
         file = "transplant.sqlite",
         path = "community/Yanetal_2018/data/",
         remote_path = "Community")

#Download climate data from OSF
get_file(node = "f3knq",
         file = "China_2013_2016_AirTemp.csv",
         path = "climate/data_cleaned",
         remote_path = "Climate")