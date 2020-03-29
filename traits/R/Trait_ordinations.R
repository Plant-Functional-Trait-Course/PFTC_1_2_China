# Trait ordinations
traitsCWM <- readRDS(file = "trait_distributions/data/China_pftc_cwm.rds") %>% 
  #mutate(mean = mean + 10) %>% 
  spread(key = trait, value = mean) %>% 
  as_tibble() %>% 
  mutate(treatment = recode(treatment, "C" = "control", "O" = "local", "1" = "warm1", "2" = "cool1", "3" = "warm3", "4" = "cool3"))
 
## colours
extreme_colours <- c("grey", "grey40", "red", "blue")
short_colours <- c("grey", "grey40", "orange", "lightblue")
otc_colours <- c("grey", "grey40", "purple")
all_colours <- c("grey", "grey40", "orange", "lightblue", "red", "blue", "purple")


# Extremes
trait_extreme <- traitsCWM %>% 
  arrange(year) %>%
  filter(treatment %in% c("local", "control", "warm3", "cool3")) %>%
  mutate(treatment = droplevels(treatment))

onlyTrait_extreme <- trait_extreme %>% select(-(turf:year))

set.seed(33)
pca_extreme <- rda(onlyTrait_extreme, scale = TRUE)

screeplot(pca_extreme, bstick = TRUE)

fNMDS2_extreme <- fortify(pca_extreme, axes = 1:4) %>% 
  filter(Score == "sites") %>% 
  bind_cols(trait_extreme %>% select(turf:year))


ggplot(fNMDS2_extreme, aes(x = PC1, y = PC2, shape = site, colour = treatment)) +
  geom_point(aes(size = ifelse(year == min(year), "First", "Other"))) +
  geom_path() +
  coord_equal() +
  scale_size_discrete(range = c(1, 3), limits = c("Other", "First"), breaks = c("First", "Other")) +
  scale_colour_manual(values = extreme_colours, limits = levels(trait_extreme$treatment), labels=c("Control", "Local transplant", "Extreme warming", "Extreme cooling")) +
  scale_shape_manual(values = c(24, 22, 23, 25), limits = levels(trait_extreme$site), labels=c("High alpine", "Alpine", "Middle", "Low")) +
  guides(shape = guide_legend(override.aes = list(fill = "black"))) +
  labs(colour = "Treatment", fill = "Treatment", shape = "Site", size = "Year") +
  theme(axis.text=element_text(size = 10),
        axis.title=element_text(size = 12))




# Extremes
cover_fat <- traitsCWM %>% 
  arrange(year) %>%
  filter(treatment %in% c("local", "control", "warm3", "cool3")) %>%
  mutate(treatment = droplevels(treatment))

cover_fat_spp <- cover_fat %>% select(-(turf:year))

set.seed(33)
NMDS <- metaMDS(cover_fat_spp, noshare = TRUE, try = 30)#DNC
# Kontrollene vekt 0

extremefNMDS <- fortify(NMDS) %>% 
  filter(Score == "sites") %>%
  bind_cols(cover_fat %>% select(turf:year))


extremes <- ggplot(extremefNMDS, aes(x = NMDS1, y = NMDS2, shape = site, colour = treatment, fill = treatment)) +
  geom_point(aes(size = ifelse(year == min(year), "First", "Other"))) +
  geom_path() + 
  coord_equal() +
  scale_size_discrete(range = c(1, 2), limits = c("Other", "First"), breaks = c("First", "Other")) +
  scale_colour_manual(values = extreme_colours, limits = levels(cover_fat$TTtreat), labels=c("Control", "Local transplant", "Extreme warming", "Extreme cooling")) +
  scale_fill_manual(values = extreme_colours, limits = levels(cover_fat$TTtreat), labels=c("Control", "Local transplant", "Extreme warming", "Extreme cooling")) +
  scale_shape_manual(values = c(24, 22, 23, 25), limits = levels(cover_fat$originSiteID), labels=c("High alpine", "Alpine", "Middle", "Low")) +
  labs(x = "") +
  guides(shape = guide_legend(override.aes = list(fill = "black"))) +
  labs(colour = "Treatment", fill = "Treatment", shape = "Site", size = "Year") +
  theme(legend.position = "none",
        axis.text=element_text(size = 10),
        axis.title=element_text(size = 12))
