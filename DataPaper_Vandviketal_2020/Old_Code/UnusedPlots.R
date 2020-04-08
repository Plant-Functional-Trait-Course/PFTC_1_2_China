## ordination
extreme_colours <- c("grey", "grey40", "red", "blue")
short_colours <- c("grey", "grey40", "orange", "lightblue")
otc_colours <- c("grey", "grey40", "purple")
all_colours <- c("grey", "grey40", "orange", "lightblue", "red", "blue", "purple")

# Extremes
cover_fat <- cover_thin %>% 
  select(-speciesName, -flag) %>% 
  arrange(year) %>%
  filter(TTtreat %in% c("local", "control", "warm3", "cool3")) %>%
  spread(key = species, value = cover, fill = 0) %>%
  mutate(TTtreat = droplevels(TTtreat))

cover_fat_spp <- cover_fat %>% select(-(originSiteID:year))

set.seed(33)
NMDS <- metaMDS(cover_fat_spp, noshare = TRUE, try = 30)#DNC
# Kontrollene vekt 0

extremefNMDS <- fortify(NMDS) %>% 
  filter(Score == "sites") %>%
  bind_cols(cover_fat %>% select(originSiteID:year))


extremes <- ggplot(extremefNMDS, aes(x = NMDS1, y = NMDS2, shape = originSiteID, colour = TTtreat, group = originPlotID, fill = TTtreat)) +
  geom_point(aes(size = ifelse(year == min(year), "First", "Other"))) +
  geom_path() + 
  coord_equal() +
  scale_size_discrete(range = c(1, 2), limits = c("Other", "First"), breaks = c("First", "Other")) +
  scale_colour_manual(values = extreme_colours, limits = levels(cover_fat$TTtreat), labels=c("Control", "Local transplant", "Extreme warming", "Extreme cooling")) +
  scale_fill_manual(values = extreme_colours, limits = levels(cover_fat$TTtreat), labels=c("Control", "Local transplant", "Extreme warming", "Extreme cooling")) +
  scale_shape_manual(values = c(24, 22, 23, 25), limits = levels(cover_fat$originSiteID), labels=c("High alpine", "Alpine", "Middle", "Lowland")) +
  labs(x = "") +
  guides(shape = guide_legend(override.aes = list(fill = "black"))) +
  labs(colour = "Treatment", fill = "Treatment", shape = "Site", size = "Year") +
  theme(legend.position = "none",
        axis.text=element_text(size = 10),
        axis.title=element_text(size = 12))


# Short
cover_fat <- cover_thin %>% 
  select(-speciesName, -flag) %>% 
  arrange(year) %>%
  filter(TTtreat %in% c("local", "control", "warm1", "cool1")) %>%
  spread(key = species, value = cover, fill = 0) %>%
  mutate(TTtreat = droplevels(TTtreat))

cover_fat_spp <- cover_fat %>% select(-(originSiteID:year))

set.seed(33)
NMDS <- metaMDS(cover_fat_spp, noshare = TRUE, try = 30)#DNC
# Kontrollene vekt 0

shortfNMDS <- fortify(NMDS) %>% 
  filter(Score == "sites") %>%
  bind_cols(cover_fat %>% select(originSiteID:year))


short <- ggplot(shortfNMDS, aes(x = NMDS1, y = NMDS2, shape = originSiteID, colour = TTtreat, group = originPlotID, fill = TTtreat)) +
  geom_point(aes(size = ifelse(year == min(year), "First", "Other"))) +
  geom_path() + 
  coord_equal() +
  scale_size_discrete(range = c(1, 2), limits = c("Other", "First"), breaks = c("First", "Other")) +
  scale_colour_manual(values = short_colours, limits = levels(cover_fat$TTtreat), labels=c("Control", "Local transplant", "Warming", "Cooling")) +
  scale_fill_manual(values = short_colours, limits = levels(cover_fat$TTtreat), labels=c("Control", "Local transplant", "Warming", "Cooling")) +
  scale_shape_manual(values = c(24, 22, 23, 25), limits = levels(cover_fat$originSiteID), labels=c("High alpine", "Alpine", "Middle", "Lowland")) +
  labs(x = "", y = "") +
  guides(shape = guide_legend(override.aes = list(fill = "black"))) +
  labs(colour = "Treatment", fill = "Treatment", shape = "Site", size = "Year") +
  theme(legend.position = "none",
        axis.text=element_text(size = 10),
        axis.title=element_text(size = 12))


# OTC
cover_fat <- cover_thin %>% 
  select(-speciesName, -flag) %>% 
  arrange(year) %>%
  filter(TTtreat %in% c("local", "control", "OTC")) %>%
  spread(key = species, value = cover, fill = 0) %>%
  mutate(TTtreat = droplevels(TTtreat))

cover_fat_spp <- cover_fat %>% select(-(originSiteID:year))

set.seed(33)
NMDS <- metaMDS(cover_fat_spp, noshare = TRUE, try = 30)#DNC
# Kontrollene vekt 0

otcfNMDS <- fortify(NMDS) %>% 
  filter(Score == "sites") %>%
  bind_cols(cover_fat %>% select(originSiteID:year))


otc <- ggplot(otcfNMDS, aes(x = NMDS1, y = NMDS2, shape = originSiteID, colour = TTtreat, group = originPlotID, fill = TTtreat)) +
  geom_point(aes(size = ifelse(year == min(year), "First", "Other"))) +
  geom_path() + 
  coord_equal() +
  scale_size_discrete(range = c(1, 2), limits = c("Other", "First"), breaks = c("First", "Other")) +
  scale_colour_manual(values = otc_colours, limits = levels(cover_fat$TTtreat), labels=c("Control", "Local transplant", "OTC")) +
  scale_fill_manual(values = otc_colours, limits = levels(cover_fat$TTtreat), labels=c("Control", "Local transplant", "OTC")) +
  scale_shape_manual(values = c(24, 22, 23, 25), limits = levels(cover_fat$originSiteID), labels=c("High alpine", "Alpine", "Middle", "Lowland")) +
  labs(y = "") +
  guides(shape = guide_legend(override.aes = list(fill = "black"))) +
  labs(colour = "Treatment", fill = "Treatment", shape = "Site", size = "Year") +
  theme(legend.position = "none",
        axis.text=element_text(size = 10),
        axis.title=element_text(size = 12))



# Legend
cover_fat <- cover_thin %>% 
  select(-speciesName, -flag) %>% 
  arrange(year) %>%
  spread(key = species, value = cover, fill = 0) %>%
  mutate(TTtreat = droplevels(TTtreat))

cover_fat_spp <- cover_fat %>% select(-(originSiteID:year))

set.seed(33)
NMDS <- metaMDS(cover_fat_spp, noshare = TRUE, try = 30)#DNC
# Kontrollene vekt 0

lfNMDS <- fortify(NMDS) %>% 
  filter(Score == "sites") %>%
  bind_cols(cover_fat %>% select(originSiteID:year))

legend <- ggplot(lfNMDS, aes(x = NMDS1, y = NMDS2, shape = originSiteID, colour = TTtreat, group = originPlotID, fill = TTtreat)) +
  geom_point(aes(size = ifelse(year == min(year), "First", "Other"))) +
  geom_path() + 
  coord_equal() +
  scale_size_discrete(range = c(1, 2), limits = c("Other", "First"), breaks = c("First", "Other")) +
  scale_colour_manual(values = all_colours, limits = levels(cover_fat$TTtreat), labels=c("Control", "Local transplant", "Warming", "Cooling", "Extreme warming", "Extreme cooling", "OTC")) +
  scale_fill_manual(values = all_colours, limits = levels(cover_fat$TTtreat), labels=c("Control", "Local transplant", "Warming", "Cooling", "Extreme warming", "Extreme cooling", "OTC")) +
  scale_shape_manual(values = c(24, 22, 23, 25), limits = levels(cover_fat$originSiteID), labels=c("High alpine", "Alpine", "Middle", "Lowland")) +
  guides(shape = guide_legend(override.aes = list(fill = "black"))) +
  labs(colour = "Treatment", fill = "Treatment", shape = "Site", size = "Year") +
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 12))

l <- get_legend(legend)

# plot 3 plots together, plus legend
p <- plot_grid(short, extremes, otc, ncol = 1, align = TRUE)
OrdinationPlot <- plot_grid(p, l, ncol = 2, rel_widths = c(2, 1))

ggsave(OrdinationPlot, filename = "OrdinationPlot.pdf", height = 10, width = 6, dpi = 300)



## ----TraitsPlots

DryWet <- traitsWideL %>% 
  ggplot(aes(x = log(Dry_Mass_g), y = log(Wet_Mass_g), colour = Site)) +
  geom_point() +
  scale_color_brewer(palette = "RdBu", direction = -1, labels=c("High alpine", "Alpine", "Middle", "Lowland")) +
  theme(legend.position = "none")

DryArea <- traitsWide %>% 
  ggplot(aes(x = log(Dry_Mass_g), y = log(Leaf_Area_cm2), colour = Site)) +
  geom_point(alpha = 0.4) +
  scale_color_brewer(palette = "RdBu", direction = -1, labels=c("High alpine", "Alpine", "Middle", "Lowland")) +
  theme(legend.position = "none")

AreaSLA <- traitsWide %>% 
  ggplot(aes(x = Leaf_Area_cm2, y = SLA_cm2_g, colour = Site)) +
  geom_point() +
  scale_color_brewer(palette = "RdBu", direction = -1, labels=c("High alpine", "Alpine", "Middle", "Lowland")) +
  theme(legend.position = "none")

LDMCThick <- traitsWide %>% 
  ggplot(aes(x = LDMC, y = Leaf_Thickness_Ave_mm, colour = Site)) +
  geom_point() +
  scale_color_brewer(palette = "RdBu", direction = -1, labels=c("High alpine", "Alpine", "Middle", "Lowland")) +
  theme(legend.position = "none")

Legend <- traitsWide %>% 
  ggplot(aes(x = log(Leaf_Area_cm2), y = log(SLA_cm2_g), colour = Site)) +
  geom_point() +
  scale_color_brewer(palette = "RdBu", direction = -1, labels=c("High alpine", "Alpine", "Middle", "Lowland"))

l2 <- get_legend(Legend)

p3 <- plot_grid(DryWet, DryArea, AreaSLA, LDMCThick, ncol = 2)
Traits <- plot_grid(p3, l2, ncol = 2, rel_widths = c(1, 0.2))
Traits
#ggsave(Traits, filename = "Traits.pdf", height = 10, width = 10, dpi = 300)