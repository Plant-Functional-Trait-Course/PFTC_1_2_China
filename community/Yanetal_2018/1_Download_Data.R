#devtools::install_github("Between-the-Fjords/dataDownloader")
library("dataDownloader")

#Download community data from OSF
get_file(node = "f3knq",
         file = "transplant.sqlite",
         path = "community/data/",
         remote_path = "Community")

#Download climate data from OSF
get_file(node = "4hjzu",
         file = "climate_month.Rdata",
         path = "community/Yanetal_2018/data/")
