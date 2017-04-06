# Analyses for Yan et al
# Finse April 2017

#Effect of temperature treatments (gradient/transplant/OTC) on different responses (richness, evenness, grass/herb ratio. Change in effect over time

##load packages
library("vegan")
library("ggvegan")

##load data
#climate
load("climate/")

#community
source("community/start_here.R")
#source("community/ordinations.R")
cover_thin <- cover_thin %>% 
  filter(TTtreat %in% c("control", "local", "warm1", "OTC"))



#turf environment



## ordination
cover_fat <- cover_thin %>% 
  select(-speciesName) %>% 
  arrange(year) %>%
  filter(TTtreat %in% c("local", "control", "warm1", "OTC")) %>%
  spread(key = species, value = cover, fill = 0) %>%
  mutate(TTtreat = droplevels(TTtreat))

cover_fat_spp <- cover_fat %>% select(-(originSiteID:year))

NMDS <- metaMDS(cover_fat_spp)#DNC

fNMDS <- fortify(NMDS) %>% 
  filter(Score == "sites") %>%
  bind_cols(cover_fat %>% select(originSiteID:year))

treat_colours <- c("black", "grey50", "red", "green")

g <-ggplot(fNMDS, aes(x = Dim1, y = Dim2, shape = originSiteID, colour = TTtreat, group = originPlotID, fill = TTtreat)) +
  geom_point(aes(size = ifelse(year == min(year), "First", "Other"))) +
  geom_path() + 
  coord_fixed(ratio = 1) +
  scale_size_discrete(range = c(1, 3), limits = c("Other", "First"), breaks = c("First", "Other")) +
  scale_colour_manual(values = treat_colours) +
  scale_fill_manual(values = treat_colours) +
  scale_shape_manual(values = c(24, 22, 23, 25)) +
  guides(shape = guide_legend(override.aes = list(fill = "black"))) +
  labs(x = "NMDS 1", y = "NMDS 2", colour = "Treatment", fill = "Treatment", shape = "Site", size = "Year")
g



## responses
#richness
richnessC <- cover_thin %>% 
  group_by(turfID, originBlockID, TTtreat, originSiteID, year) %>% 
  summarise(n = n()) %>% 
  group_by(originBlockID, TTtreat, originSiteID, year) %>%
  select(-turfID) %>%
  spread(key = TTtreat, value = n)


ggplot(richnessC, aes(x = year, y = control, colour = originSiteID)) + 
  geom_jitter(height = 0, width = 0.2) + 
  geom_smooth(aes(group = originSiteID))




 