##
source("community/start_here.R")
source("traits/trait_2017_analysis.R")

#problematic 2016 leaves 
trait2016 %>% mutate(loc1 = substr(Location, 1, 1)) %>%
  count(loc1, Site, Project) %>% print(n = 100)

noGraminoids <- taxa %>% filter(!functionalGroup %in% c("gramineae", "sedge")) %>% select(speciesName)

# Merge traits and cover
# Only Local, and controls from experiments
AllLeaves <- traits %>% 
  filter(Project %in% c("LOCAL", "0", "C")) %>%
  mutate(year = year(Date)) %>% 
  group_by(year, Taxon, Site) %>% 
  summarise(n_leaves = n()) %>% # count number of leaves
  spread(key = year, value = n_leaves, fill = 0) %>%
  # join cover data
  full_join(
    cover_thin %>% 
      inner_join(noGraminoids) %>% # remove graminoids
      filter(TTtreat %in% c("control", "local")) %>% # could filter by year here
      group_by(speciesName, originSiteID) %>% 
      summarise(nturfs = n(), max = max(cover), mean = round(mean(cover), 2)), 
    by = c("Taxon" = "speciesName", "Site" = "originSiteID") # calculate max and mean cover per turf
      ) %>%
  arrange(Taxon) %>% 
  mutate(sum.nLeaves = `2015` + `2016`) %>% # number of leaves in both years
  # remove sp from Experiments
  filter(!Taxon %in% c("Potentilla leuconota", "Plantago asiatica", "Polygonum viviparum", "Veronica szechuanica", "Viola biflora var. rockiana", "Pedicularis davidii", "Hypericum wightianum", "Geranium pylzowianum", "Epilobium fangii", "Artemisia flaccida"))
  
# Leaves with traits and cover data
TraitsAndCover <- AllLeaves %>%
  filter(nturfs > 0, `2015` > 0 | `2016` > 0) %>% 
  ungroup() %>% 
  mutate(Five = ifelse(sum.nLeaves >5, 5, sum.nLeaves)) %>% 
  mutate(TraitCover = "TraitAndCover") %>% 
  summarise(total = sum(Five))
 
# Cover but no trait data
NoTraitInSites <- AllLeaves %>% 
  filter(nturfs > 0, is.na(sum.nLeaves)) %>% arrange(Taxon) %>% filter(mean > 2)
NoTraitInSites %>% arrange(mean) %>% print(n = 61)

# How many leaves where cover is > threshold
traits %>%
  inner_join(select(NoTraitInSites, Taxon), by = "Taxon") %>% 
  filter(Project %in% c("LOCAL", "0", "C")) %>% 
  group_by(Taxon, Site) %>% 
  summarise(n = n()) %>% 
  mutate(Five = ifelse(n > 5, 5L, n)) %>%
  ungroup() %>% 
  summarise(sum = sum(Five))
  slice(1:5)

  
TraitsWithoutCover <- AllLeaves %>% 
  filter(!is.na(sum.nLeaves), is.na(nturfs)) %>% print(n = Inf)
  
  #slice(200:250) %>%
  #print(n = 1000)



ggplot(AllLeaves, aes(x = Site, y = mean, color = Taxon)) +
  geom_jitter(height = 0, width = 0.2, show.legend = FALSE)

  
  
dd <- cover_thin %>% 
  inner_join(noGraminoids) %>% 
  group_by(year, turfID) %>% 
  mutate(sumCover = sum(cover)) %>% 
  filter(speciesName %in% traits$Taxon) %>% 
  summarise(prop = sum(cover) / first(sumCover)) 

dd %>% filter(prop < 0.8) 


# Traits and cover, no Graminoids
CNAnalysis <- traits %>% 
  left_join(TraitsAndCover, by = c("Taxon", "Site")) %>% 
  filter(Project %in% c("LOCAL", "0", "C")) %>%
  filter(!is.na(TraitCover)) %>% 
  mutate(Year = year(Date)) %>% 
  filter(is.na(flag)) %>% 
  arrange(Site, desc(Year), Taxon, Individual_number, Leaf_number) %>% 
  select(Year, Site, Taxon, Individual_number, Leaf_number, flag, resid_Area_Dry, `2015`, `2016`, nturfs, max, mean, sum.nLeaves, Five) %>%
  ungroup() %>% group_by(Site, Taxon) %>% 
  summarise(mean(Five)) %>% 
  ungroup() %>% summarise(sum(`mean(Five)`))

table(CNAnalysis$Taxon, year(CNAnalysis$Date), CNAnalysis$Site)

write_csv(CNAnalysis, "CNAnalysis.csv", col_names = TRUE)
