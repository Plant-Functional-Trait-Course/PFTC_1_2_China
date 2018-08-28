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
  filter(Project %in% c(NA, "LOCAL", "0", "C")) %>%
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
  filter(!grepl("brown|yellow", allComments)) %>% 
  filter(mean > 4)
  #ungroup() %>% group_by(Site, Taxon, Individual_number) %>% 
  #summarise(mean(Five)) 
  #ungroup() %>% summarise(sum(`mean(Five)`))
  

CNAnalysis2015 <- CNAnalysis %>%
  filter(Year == 2015) %>% 
  select(-Full_Envelope_Name, -Envelope_Name_Corrected) %>% 
  mutate(Full_Envelope_Name = paste(Date, Site, Elevation, Taxon, Individual_number, Leaf_number, sep = "_"))

write_csv(CNAnalysis2015, "CNAnalysis2015.csv", col_names = TRUE)
  
  
# calculate how many
CNAnalysis %>% 
  arrange(Site, desc(Year), Taxon, Individual_number, Leaf_number) %>% 
  select(Year, Site, Taxon, Individual_number, Leaf_number, flag, `2015`, `2016`, nturfs, max, mean, sum.nLeaves, Five) %>%
  

table(CNAnalysis$Taxon, year(CNAnalysis$Date), CNAnalysis$Site)

write_csv(CNAnalysis, "CNAnalysis.csv", col_names = TRUE)


# CN Analysis Forbs
FiveOrMore <- AllLeaves %>% 
  filter(sum.nLeaves > 5) %>% 
  select(Taxon, Site, `2015`, `2016`) %>% 
  gather(key = Year, value = nLeaves, -Taxon, -Site)
  

# forbs (5 or more leaves, from gradient, not from last list, preferably 2016, then 2015)
traits %>% 
  filter(Project %in% c(NA, "LOCAL", "0", "C")) %>% 
  anti_join(CNAnalysis) %>% 
  filter(!grepl("brown|yellow", allComments)) %>% 
  mutate(Year = as.character(year(Date))) %>% 
  inner_join(FiveOrMore, by = c("Taxon", "Site", "Year")) %>% 
  filter(!Taxon %in% c("Potentilla leuconota", "Plantago asiatica", "Polygonum viviparum", "Veronica szechuanica", "Viola biflora var. rockiana", "Pedicularis davidii", "Hypericum wightianum", "Geranium pylzowianum", "Epilobium fangii", "Artemisia flaccida")) %>% 
  group_by(Site, Location, Project, Individual_number, Taxon) %>% 
  summarise(n = n())
  


# CN Analysis Grasses
onlyGraminoids <- taxa %>% filter(functionalGroup %in% c("gramineae", "sedge")) %>% select(speciesName)


CN_Graminoids <- traits %>% 
  mutate(Year = year(Date)) %>%
  filter(Project %in% c(NA, "LOCAL", "0", "C")) %>%
  inner_join(onlyGraminoids, by = c("Taxon" = "speciesName")) %>% 
  filter(!grepl("brown|yellow", allComments)) %>% 
  #select(Site, Year, Location, Individual_number, Leaf_number, Taxon) %>% 
  arrange(Site, Taxon, -Year, Individual_number) %>% # Project not needed all Local
  group_by(Site, Year, Taxon, Individual_number) %>% 
  summarise(n = n()) %>% pn

writexl::write_xlsx(x = CN_Graminoids, path = "traits/CN_Graminoids.xlsx")
