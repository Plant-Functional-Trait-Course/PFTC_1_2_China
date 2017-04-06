##
source("community/start_here.R")
source("traits/trait_2017_analysis.R")

#problematic 2016 leaves 
trait2016 %>% mutate(loc1 = substr(Location, 1, 1)) %>%
  count(loc1, Site, Project) %>% print(n = 100)


traits %>% 
  filter(Project %in% c("LOCAL", "0", "C")) %>%
  mutate(year = year(Date)) %>% 
  group_by(year, Taxon, Site) %>% 
  summarise(n_leaves = n()) %>%
  spread(key = year, value = n_leaves, fill = "") %>%
  full_join(
    cover_thin %>% 
      filter(TTtreat %in% c("control", "local")) %>% # can filter by year here
      group_by(speciesName, originSiteID) %>% 
      summarise(nturfs = n(), max = max(cover)), 
    by = c("Taxon" = "speciesName", "Site" = "originSiteID")
      ) %>%
  arrange(Taxon) %>% 
  mutate_all(function(x) if_else(is.na(x), "", as.character(x))) %>% 
  # remove sp from Experiments
  filter(!Taxon %in% c("Potentilla leuconota", "Plantago asiatica", "Polygonum viviparum", "Veronica szechuanica", "Viola biflora var. rockiana", "Pedicularis davidii", "Hypericum wightianum", "Geranium pylzowianum", "Epilobium fangii", "Artemisia flaccida")) %>% 
#  slice(200:250) %>%
  print(n = 1000)





cover_thin %>% 
  
  
  

  