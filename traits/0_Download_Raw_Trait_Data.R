##################################
### DOWNLOAD RAW DATA FROM OSF ###
##################################

#devtools::install_github("Between-the-Fjords/dataDownloader")
library("dataDownloader")

# Download raw data from OSF
# Leaf Area
get_file(node = "f3knq",
         file = "Leaf.Area2015-20170717.csv",
         path = "traits/data/",
         remote_path = "RawData/RawData_Traits")

get_file(node = "f3knq",
         file = "LeafArea2015-20170721_SecondBatch.xlsx",
         path = "traits/data/",
         remote_path = "RawData/RawData_Traits")

# Traits 2015
get_file(node = "f3knq",
         file = "2015_ChinaLeafTraitData_corrCP_16032017.csv",
         path = "traits/data/",
         remote_path = "RawData/RawData_Traits")

get_file(node = "f3knq",
         file = "2015_ChinaLeafTraitData_corrCP_16032017 Halenia_elliptica.csv",
         path = "traits/data/",
         remote_path = "RawData/RawData_Traits")

# Traits 2016
get_file(node = "f3knq",
         file = "2016_PFTC2_Leaf_Area_corrCP_30032017.csv",
         path = "traits/data/",
         remote_path = "RawData/RawData_Traits")

get_file(node = "f3knq",
         file = "2016_China_envelope_names_CPcorr_30032017.csv",
         path = "traits/data/",
         remote_path = "RawData/RawData_Traits")


# Chemical traits
get_file(node = "f3knq",
         file = "ChinaLeafTraitData_senttogroup.csv",
         path = "traits/data/",
         remote_path = "RawData/RawData_Traits")

get_file(node = "f3knq",
         file = "2015 China leaves.xls",
         path = "traits/data/",
         remote_path = "RawData/RawData_Traits")

get_file(node = "f3knq",
         file = "CHINA_CNP_19January2018.xls",
         path = "traits/data/",
         remote_path = "RawData/RawData_Traits")
