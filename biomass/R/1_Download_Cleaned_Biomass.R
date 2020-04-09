#############################
### DOWNLOAD CLEANED DATA ### 
#############################

#devtools::install_github("Between-the-Fjords/dataDownloader")
library("dataDownloader")
library("tidyverse")

pn <- . %>% print(n = Inf)

# Download OSF
# Download raw data from OSF
get_file(node = "f3knq",
         file = "China_2016_Biomass_cleaned.csv",
         path = "biomass/clean_data",
         remote_path = "Biomass")

# Read in
biomass_cleaned <- read_csv(file = "biomass/clean_data/China_2016_Biomass_cleaned.csv")
