##
source("community/start_here.R")
source("traits/trait_2017_analysis.R")

#problematic 2016 leaves 
trait2016 %>% mutate(loc1 = substr(Location, 1, 1)) %>%
  count(loc1, Site, Project) %>% print(n = 100)


traits %>% 
  filter(Project %in% c("LOCAL", "0", "C")) %>%
  mutate(year = year(Date)) %>% 
  group_by(year, Taxon) %>% 
  summarise(n_leaves = n()) %>%
  spread(key = year, value = n_leaves, fill = "") %>%
  full_join(
    cover_thin %>% group_by(speciesName) %>% summarise(nturfs = n(), max = max(cover)), 
    by = c("Taxon" = "speciesName")
      ) %>%
  arrange(Taxon) %>% 
#  slice(200:250) %>%
  print(n = 1000)





cover_thin %>% 
  
  
  

  