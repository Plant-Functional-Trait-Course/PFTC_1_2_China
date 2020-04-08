##load packages
library("tidyverse")
library("vegan")
library("ggvegan")

source("community/start_here.R")
source("community/yans_ms_functions.R")

## ordination
cover_fat <- cover_thin %>% 
  select(-speciesName, -flag) %>% 
  arrange(year) %>%
  filter(TTtreat %in% c("local", "control", "warm1", "OTC")) %>%
  spread(key = species, value = cover, fill = 0) %>%
  mutate(TTtreat = droplevels(TTtreat))

cover_fat_spp <- cover_fat %>% select(-(originSiteID:year))

set.seed(32)
NMDS <- metaMDS(cover_fat_spp, noshare = TRUE, try = 30)#DNC

fNMDS <- fortify(NMDS) %>% 
  filter(Score == "sites") %>%
  bind_cols(cover_fat %>% select(originSiteID:year))

treat_colours <- c("grey", "grey40", "orange", "purple")

g <- ggplot(fNMDS, aes(x = Dim1, y = Dim2, shape = originSiteID, colour = TTtreat, group = originPlotID, fill = TTtreat)) +
  geom_point(aes(size = ifelse(year == min(year), "First", "Other"))) +
  geom_path() + 
  coord_equal() +
  scale_size_discrete(range = c(1, 2.5), limits = c("Other", "First"), breaks = c("First", "Other")) +
  scale_colour_manual(values = treat_colours, limits = levels(cover_fat$TTtreat), labels=c("Control", "Local transplant", "Transplant", "OTC")) +
  scale_fill_manual(values = treat_colours, limits = levels(cover_fat$TTtreat), labels=c("Control", "Local transplant", "Transplant", "OTC")) +
  scale_shape_manual(values = c(24, 22, 23, 25), limits = levels(cover_fat$originSiteID), labels=c("High alpine", "Alpine", "Middle", "Low")) +
  guides(shape = guide_legend(override.aes = list(fill = "black"))) +
  labs(colour = "Treatment", fill = "Treatment", shape = "Site", size = "Year") 

set.seed(32)
HA <- two_sites_nmds("H", "A")
AM <- two_sites_nmds("A", "M")
ML <- two_sites_nmds("M", "L")
LM <- two_sites_nmds("L", "M") 

HA <- HA %>% mutate(Dim1 = -Dim1)
ML <- ML %>% mutate(Dim1 = -Dim1)
LM <- LM %>% mutate(Dim1 = -Dim1)


all_ord <- bind_rows(
  `H - A` = HA, 
  `A - M` = AM, 
  `M - L` = ML, 
  `L - ` = LM, .id = "which") %>% 
  mutate(which = factor(which, levels = c("H - A", "A - M", "M - L", "L - "), labels = c("High Alpine - Alpine", "Alpine - Middle", "Middle - Lowland", " - Lowland")))

OrdinationPlot <- g %+% all_ord +
  labs(x = "NMDS1", y = "NMDS2") +
  facet_wrap(~ which)

ggsave(OrdinationPlot, filename = "community/FinalFigures/OrdinationPlot.png", height = 7, width = 8, dpi = 300)
