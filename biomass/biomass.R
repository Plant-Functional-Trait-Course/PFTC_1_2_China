#load packages
library("readxl")
library("tidyverse")

#read excel file
biomass <- plyr::ldply(1:4, read_excel, path = "biomass/data/biomass2015.xls")
names(biomass) <- make.names(names(biomass))

biomass <- biomass %>% 
  mutate(site = factor(site, levels = c("H", "A", "M", "L"))) %>% 
  rename(biomass = production) %>% 
  select(site, plot, species, matches("^H\\d+$"), cover, biomass) %>% 
  mutate(mean_height = rowMeans(select(., matches("^H\\d+$")), na.rm = TRUE)) %>% 
  group_by(site, plot)


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
  data_frame(speciesName, authority)
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
  mutate(genus = getGenus(speciesName))



# make plots
ggplot(biomass, aes(x = cover, y = biomass, color = species)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~ site)

ggplot(biomass, aes(x = mean_height, y = biomass, color = species)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~ site)

biomass[which.max(biomass$biomass),]






#get taxonomy table
con2 <- src_sqlite(path = "community/data/transplant.sqlite", create = FALSE)# need to move all code to dplyr for consistancy

taxa <- tbl(con2, "taxon") %>%
  collect()


setdiff(biomass$speciesName, taxa$speciesName)
# 61 out of 123 species not in common with community data


## biomass by site
biomass %>% 
  summarise(totalBiomass = sum(biomass, na.rm = TRUE)) %>%
  ggplot(aes(x = site, y = totalBiomass)) + 
  geom_boxplot()

biomass %>% 
  group_by(site, genus) %>% 
  summarise(sppSum = sum(biomass, na.rm = TRUE)) %>% 
  arrange(sppSum) %>% 
  ggplot(aes(x = site, y = sppSum, fill = genus)) + 
  geom_bar(stat = "identity", position = "stack", show.legend = FALSE)
