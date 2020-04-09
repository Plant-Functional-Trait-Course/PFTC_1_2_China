#########################
### DOWNLOAD RAW DATA ### 
#########################

#load packages
library("readxl")
library("tidyverse")
library("vegan")
library("ggvegan")
#devtools::install_github("Between-the-Fjords/dataDownloader")
library("dataDownloader")

pn <- . %>% print(n = Inf)

# Download OSF
#Download raw data from OSF
get_file(node = "f3knq",
         file = "biomass2015.xls",
         path = "biomass/raw_data",
         remote_path = "RawData/RawData_Biomass")


#read excel file
Biomass <- plyr::ldply(1:4, read_excel, path = "biomass/raw_data/biomass2015.xls")
names(Biomass) <- make.names(names(Biomass))


# There are duplicate species in the data set. Those are species that were thought to be different species, but turned out to be the same.

# Sum biomass and cover for duplicate species
BiomassCover <- Biomass %>% 
  mutate(
    site = factor(site, levels = c("H", "A", "M", "L")),
    species = trimws(species)
    ) %>% 
  rename(biomass = production) %>% 
  select(site, plot, species, cover, biomass) %>% 
  group_by(site, plot, species) %>% 
  summarize(biomass = sum(biomass), cover = sum(cover))

# Gather heights and calculate average height including duplicate species
Height <- Biomass %>% 
  mutate(
    site = factor(site, levels = c("H", "A", "M", "L")),
    species = trimws(species)
  ) %>% 
  select(site, plot, species, matches("^H\\d+$")) %>% 
  gather(key = individual, value = height, -site, -plot, -species) %>% 
  group_by(site, plot, species) %>% 
  filter(!is.na(height)) %>% 
  summarise(height = mean(height), n = n())
  
# Merge BiomassCover and Height
biomass <- BiomassCover %>% 
  left_join(Height, by = c("site", "plot", "species"))


# split authority from name
spNames <- strsplit(biomass$species, " ")
nameAuthority <- plyr::ldply(spNames, function(x){
  if(any(grepl("var.", x, fixed = TRUE))){
    speciesName <- paste(x[1:4], collapse = " ")
    authority <- paste(x[-(1:4)], collapse = " ")
  } else {
    speciesName <- paste(x[1:min(length(x), 2)], collapse = " ")  
    authority <- paste(x[-(1:2)], collapse = " ")
  }
  if(is.na(authority)) authority <- ""
  tibble(speciesName, authority)
})

#
getGenus <- function(x) {
  x <- strsplit(x, " ")
  unlist(lapply(x, "[[", 1))
}

# cbind to biomass data
biomass <- biomass %>% 
  bind_cols(nameAuthority) %>%
  select(-species) %>%
  mutate(
    speciesName = gsub("_", " ", speciesName),
    speciesName = gsub("\\.sp", " sp", speciesName),
    speciesName = gsub("\\.gra", " gra", speciesName),
    speciesName = stringi::stri_trans_totitle(speciesName, type= "sentence")
 ) %>%
  mutate(genus = getGenus(speciesName))

#import trait taxonomy dictionary
biomass_taxa <- read_delim("biomass/raw_data/biomass_taxonomic_corrections.csv", delim = ",", comment = "#")

biomass <- biomass %>%
  mutate(speciesName = plyr::mapvalues(speciesName, from = biomass_taxa$wrongName, to = biomass_taxa$correctName, warn_missing = FALSE)) %>% 
  group_by(site, plot)
          
#get family
biomass <- biomass %>% mutate(family = tpl::tpl.get(genus)$family)

# Cleaned data file
#write_csv(biomass, path = "biomass/China_2016_Biomass_cleanded.csv")
