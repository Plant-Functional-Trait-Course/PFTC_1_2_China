## ----downloadData
#devtools::install_github("Between-the-Fjords/dataDownloader")
library("dataDownloader")

#Download community data from OSF
get_file(node = "f3knq",
         file = "transplant.sqlite",
         path = "community/data/",
         remote_path = "Community")


#Download traits data from OSF
get_file(node = "f3knq",
         file = "PFTC1.2_China_2015_2016_LeafTraits.csv",
         path = "traits/data_cleaned/",
         remote_path = "Traits")

get_file(node = "f3knq",
         file = "PFTC1.2_China_2015_2016_ChemicalTraits.csv",
         path = "traits/data_cleaned/",
         remote_path = "Traits")


#Download climate data from OSF
get_file(node = "f3knq",
         file = "China_2013_2016_AirTemp.csv",
         path = "climate/data_cleaned/",
         remote_path = "Climate")

get_file(node = "f3knq",
         file = "China_2019_Climate_TomstLogger.csv",
         path = "climate/data_cleaned/",
         remote_path = "Climate")

get_file(node = "f3knq",
         file = "China_2019_Monthly_TemperatureiButton.csv",
         path = "climate/data_cleaned/",
         remote_path = "Climate")


#Download biomass data from OSF
get_file(node = "f3knq",
         file = "China_2016_Biomass_cleaned.csv",
         path = "biomass/",
         remote_path = "Biomass")
## ----