library("tidyverse")
library("lubridate")
library("broom")
#devtools::install_github("Between-the-Fjords/dataDownloader")
library("dataDownloader")

# Download cleaned file from OSF
get_file(node = "emzgf",
         file = "traits.Rdata",
         path = "traits/data")

#### import data ####
load(file = "traits/data/traits.RData")

#rename some columns
trait2016 <- traits %>%
  rename(SLA = SLA_cm2_g) %>% 
  rename(Cropped_Leaf_Area = Leaf_Area_cm2)

#### Prepare Data ####

trait2016$Project <- as.factor(trait2016$Project)
trait2016$Taxon <- as.factor(trait2016$Taxon)

#create data frame with correct leaves and treatment labels
exp.trait <- trait2016 %>%
  filter(Project != "LOCAL" ) %>% 
  filter(Project != "SEAN") %>% 
  filter(Project != "Unknown") %>% 
  mutate(Project = plyr::mapvalues(Project, c("OTC", "C", "0", "1", "2", "3", "4", "6"), c("OTC", "Control", "Control", "warm1", "cool1", "warm3", "cool3", "Control"))) %>% 
  droplevels() 

#remove species with few comparisons (at less than 3 sites), also calculates traits and removes unrealistic trait values
spp.select <- exp.trait %>% 
  group_by(Site, Taxon) %>% 
  summarize(n = n()) %>% 
  group_by(Taxon) %>% 
  mutate(nsites = n()) %>% 
  arrange(Taxon) %>% 
  filter(nsites > 2) %>% 
  semi_join(x = exp.trait, by = "Taxon") %>% 
  mutate(code = factor(paste(Elevation, Project), levels = c("3000 OTC", "3000 Control", "3000 warm1", "3500 OTC", "3500 cool1", "3500 Control", "3500 warm1", "3850 OTC", "3850 cool1", "3850 Control", "3850 warm1", "4100 OTC", "4100 cool1", "4100 Control", "3000 warm3", "4100 cool3"))) %>%
  filter(SLA < 500) %>% 
  filter(SLA > 5) %>% 
  filter(LDMC < 1) %>%
  filter(N_percent < 6.4 | is.na(N_percent)) %>%
  mutate(CN = C_percent/N_percent) %>% 
  mutate(NP = N_percent/P_AVG) %>% 
  filter(Taxon != "Aletris pauciflora") %>% 
  filter(Taxon != "Geranium pylzowianum" | Project != "Control" | Site != "L")

##Calculate mean trait value for each species in each plot where it was found
trait.mod <- spp.select %>% 
  mutate(origin = substr(Location, 0, 1)) %>% 
  mutate(rep = substr(Location, 2,2)) %>% 
  group_by(Project, Elevation, origin, Taxon, rep) %>% 
  summarize(mean.SLA = mean(SLA), mean.LDMC = mean(LDMC), mean.LA = mean(Cropped_Leaf_Area), mean.LT = mean(Leaf_Thickness_Ave_mm), mean.P = mean(P_AVG, na.rm = T), mean.N = mean(N_percent, na.rm = T), mean.C = mean(C_percent, na.rm = T), mean.N15 = mean(dN15_percent, na.rm = T), mean.C13 = mean(dC13_percent, na.rm = T), mean.CN = mean(CN, na.rm = T), n = n()) %>% 
  arrange(Taxon)


##calculate the mean trait value for each species only in the home site. This mean combines all measurements from all control plots at a site and is used to determine what the average trait value of a species at a site is like. When combined with variance from each site and number of samples from each site, the distribution and confidence intervals can be caluclated to determine when a transplanted individual traits moved outside of their home range.
trait.mod.mean <- spp.select %>% 
  mutate(origin = substr(Location, 0, 1)) %>% 
  mutate(rep = substr(Location, 2,2)) %>% 
  filter(Project == "Control") %>% 
  group_by(Elevation, Taxon) %>% 
  summarize(mean.SLA = mean(SLA), mean.LDMC = mean(LDMC), mean.LA = mean(Cropped_Leaf_Area), mean.LT = mean(Leaf_Thickness_Ave_mm), mean.P = mean(P_AVG, na.rm = T), mean.N = mean(N_percent, na.rm = T), mean.C = mean(C_percent, na.rm = T), mean.N15 = mean(dN15_percent, na.rm = T), mean.C13 = mean(dC13_percent, na.rm = T), mean.CN = mean(CN, na.rm = T), mean.NP = mean(NP, na.rm = T)) %>% 
  gather(key = Trait, value = mean, -Elevation, -Taxon)

#calculate the variance for each trait for each species at each elevation (site)
trait.mod.var <- spp.select %>% 
  mutate(origin = substr(Location, 0, 1)) %>% 
  mutate(rep = substr(Location, 2,2)) %>% 
  filter(Project == "Control") %>% 
  group_by(Elevation, Taxon) %>% 
  summarize(mean.SLA = var(SLA), mean.LDMC = var(LDMC), mean.LA = var(Cropped_Leaf_Area), mean.LT = var(Leaf_Thickness_Ave_mm), mean.P = var(P_AVG, na.rm = T), mean.N = var(N_percent, na.rm = T), mean.C = var(C_percent, na.rm = T), mean.N15 = var(dN15_percent, na.rm = T), mean.C13 = var(dC13_percent, na.rm = T), mean.CN = var(CN, na.rm = T), mean.NP = var(NP, na.rm = T)) %>% 
  gather(key = Trait, value = var, -Elevation, -Taxon)

#caluclate sample size for each trait for each species at each elevation (site)
trait.mod.n <- spp.select %>% 
  mutate(origin = substr(Location, 0, 1)) %>% 
  mutate(rep = substr(Location, 2,2)) %>% 
  filter(Project == "Control") %>% 
  select(Elevation, Taxon, SLA, LDMC, Cropped_Leaf_Area, Leaf_Thickness_Ave_mm, P_AVG, N_percent, C_percent, dN15_percent, dC13_percent, CN, NP) %>% 
  rename(mean.SLA = SLA, mean.LDMC = LDMC, mean.LA = Cropped_Leaf_Area, mean.LT = Leaf_Thickness_Ave_mm, mean.P = P_AVG, mean.N = N_percent, mean.C = C_percent, mean.N15 = dN15_percent, mean.C13 = dC13_percent, mean.CN = CN, mean.NP = NP) %>% 
  gather(key = Trait, value = var, -Elevation, -Taxon) %>% 
  group_by(Elevation, Taxon, Trait) %>% 
  filter(!is.na(var)) %>% 
  summarize(n = n())

#combine mean, var, and count
trait.mod.dist <- trait.mod.mean %>% 
  left_join(trait.mod.var) %>% 
  left_join(trait.mod.n) %>% 
  mutate(origin = plyr::mapvalues(Elevation, from = c(3000, 3500, 3850, 4100), to = c("L", "M", "A", "H"))) %>% 
  ungroup() %>% 
  select(-Elevation)

#save the species distributions for each trait at each site
write.csv(trait.mod.dist, file = "traits/data/cont_trait_dist.csv")

##put full dataset into long form
intra.trait.data <- trait.mod %>% 
  mutate(ID = paste(Project, origin, Taxon)) %>%
  ungroup(Elevation) %>% 
  select(-Elevation) %>% 
  gather(key = trait, value = mean.trait, -ID, -Project, -origin, -Taxon, -rep) %>% 
  select(-ID) 


#save long form full dataset
save(intra.trait.data, file = "traits/data/china_intra_trait.RData") 
