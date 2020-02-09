#### China Data paper

##load packages
library("tidyverse")
library("vegan")
library("ggvegan")
library("cowplot")

source("community/start_here.R")

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

ggsave(OrdinationPlot, filename = "OrdinationPlot.jpg", height = 10, width = 6, dpi = 300)


## ----DiversityPlot
# Richness, evenness etc.
#turf environment
turf_env <- tbl(con, "turfEnvironment") %>% collect()


## functional groups
fun_gp <- tbl(con, "taxon") %>% 
  select(species, functionalGroup) %>% 
  collect()

elev <- tibble(destSiteID = c("H", "A", "M", "L"),
               Elevation = c(4130, 3850, 3500, 3000))

## Calculate responses
responses <- cover_thin %>% 
  left_join(fun_gp, by = "species") %>% 
  left_join(turf_env, by = c("turfID", "year")) %>% 
  left_join(elev, by = "destSiteID") %>% 
  group_by(turfID, originBlockID, TTtreat, originSiteID, year, Elevation) %>%  
  summarise(Richness = n(), 
            diversity = diversity(cover), 
            N1 = exp(diversity),
            Evenness = diversity/log(Richness),
            SumofCover = sum(cover),
            ProportionGraminoid = sum(cover[functionalGroup %in% c("gramineae", "sedge")])/SumofCover,
            total_vascular = first(totalVascular),
            vegetationHeight = mean(vegetationHeight)
  )

# Gradient
ResponsesGradient <- responses %>% 
  filter(TTtreat %in% c("local", "control"), year == 2016) %>% 
  select(-N1, -diversity, -total_vascular, -vegetationHeight) %>% 
  gather(key = Index, value = Value, -turfID, -originBlockID, -TTtreat, -originSiteID,  -year, -Elevation) %>% 
  mutate(Index = factor(Index, levels = c("Richness", "Evenness", "SumofCover", "ProportionGraminoid")))

MeanGradResponse <- ResponsesGradient %>% 
  group_by(Index, Elevation, TTtreat) %>% 
  summarise(Ave_value = mean(Value)) %>% 
  ungroup() %>% 
  filter(!is.na(Ave_value))

GradientPlot <- ResponsesGradient %>% 
  ggplot(aes(x = Elevation, y = Value, colour = originSiteID, shape = TTtreat)) +
  geom_jitter(height = 0, width = 0.1, size = 1.8) +
  geom_line(data = MeanGradResponse, aes(x = Elevation, y = Ave_value), colour = "grey", linetype = "dashed", size = 0.8) +
  scale_color_brewer(palette = "RdBu", direction = -1) +
  scale_shape_manual(values = c(1, 16)) +
  scale_x_continuous(breaks = c(3000, 3200, 3400, 3600, 3800, 4000, 4200)) +
  labs(x = "Elevation m a.s.l.", y = "Average value", shape = "Treatment", colour = "Site") +
  facet_wrap( ~ Index, scale = "free", ncol = 1) +
  theme(legend.position = "none")


# Experiments
responsesExp <- responses %>% 
  filter(year == 2016) %>% 
  ungroup() %>% 
  mutate(contrast = plyr::mapvalues(TTtreat, c("control", "local", "warm1", "cool1", "warm3", "cool3", "OTC"), c(0, 0, -1, 1, -3, 3, -1)),
         contrast = as.numeric(as.character(contrast))) %>% 
  select(-N1, -total_vascular, -diversity, -vegetationHeight) %>% 
  gather(key = Index, value = Value, Richness, Evenness, SumofCover, ProportionGraminoid)

otc <- responsesExp %>% 
  filter(TTtreat %in% c("control", "OTC")) %>% 
  mutate (experiment = "OTC")

extremeTransplant <- responsesExp %>% 
  filter(TTtreat %in% c("control", "warm3", "cool3")) %>% 
  mutate (experiment = "Extreme transplant")

transplant <- responsesExp %>% 
  filter(TTtreat %in% c("control", "warm1", "cool1")) %>% 
  mutate (experiment = "Transplant")

mean_responses <- otc %>% 
  bind_rows(transplant, extremeTransplant) %>% 
  group_by(TTtreat, originSiteID, contrast, experiment, Index) %>% 
  summarise(Ave_value = mean(Value)) %>% 
  mutate(Index = factor(Index, levels = c("Richness", "Evenness", "SumofCover", "ProportionGraminoid")))


# treatments
ContrastPlot <- responses %>% 
  filter(year == 2016) %>% 
  select(-N1, -total_vascular, -diversity, -vegetationHeight) %>% 
  gather(key = Index, value = Value, Richness, Evenness, SumofCover, ProportionGraminoid) %>% 
  mutate(Index = factor(Index, levels = c("Richness", "Evenness", "SumofCover", "ProportionGraminoid"))) %>% 
  mutate(contrast = plyr::mapvalues(TTtreat, c("control", "local", "warm1", "cool1", "warm3", "cool3", "OTC"), c(0, 0, -1, 1, -3, 3, -1)),
         contrast = as.numeric(as.character(contrast))) %>% 
  ggplot(aes(x = contrast, y = Value, colour = originSiteID, shape = TTtreat)) +
  geom_jitter(height = 0, width = 0.1, size = 1.8) +
  geom_line(data = mean_responses, aes(y = Ave_value, x = contrast, colour = originSiteID, linetype = experiment), inherit.aes = FALSE, size = 0.8) +
  scale_color_brewer(palette = "RdBu", direction = -1, labels=c("High alpine", "Alpine", "Middle", "Lowland")) +
  scale_linetype_manual(values = c("dashed", "solid", "dotted")) +
  scale_shape_manual(values = c(1, 16, 15, 17, 18, 6, 7), labels=c("Control", "Local transplant", "Warming", "Cooling", "Extreme warming", "Extreme cooling", "OTC")) +
  labs(x = "Contrast levels", y = "", colour = "Site", linetype = "Experiment", shape = "Treatment") +
  scale_x_continuous(breaks = c(-3, 0, 3), labels = c("warmer", "control", "cooler")) +
  facet_wrap(~ Index, scales = "free", ncol = 1)
#+ theme(axis.text.x = element_text(values = c("cooler", "control", "warmer")))


plot_grid(GradientPlot, ContrastPlot, ncol = 2, rel_widths = c(1.3, 2))
#Diversity <- plot_grid(GradientPlot, ContrastPlot, ncol = 2, rel_widths = c(1.3, 2))
#ggsave(Diversity, filename = "Diversity.jpg", height = 10, width = 10, dpi = 300)


## ----TraitDistribution
# Trait distributions
traits <- read_csv(file = "traits/data_cleaned/PFTC1.2_China_2015_2016_Traits.csv", col_names = TRUE)

traitsWide <- traits %>% 
  filter(!Treatment %in% c("SEAN", "6")) %>% 
  mutate(Treatment = ifelse(is.na(Treatment), "LOCAL", Treatment)) %>% 
  filter(Treatment == "LOCAL") %>%
  mutate(Wet_Mass_g.log = log(Wet_Mass_g),
         Dry_Mass_g.log = log(Dry_Mass_g),
         Leaf_Thickness_Ave_mm.log = log(Leaf_Thickness_Ave_mm),
         Leaf_Area_cm2.log = log(Leaf_Area_cm2)) %>% 
  mutate(Site = factor(Site, levels = c("H", "A", "M", "L"))) %>% 
  mutate(LDMC = ifelse(Site == "M" & Taxon == "Trifolium repens" & Date == "2015-08-20", NA, LDMC),
         Leaf_Area_cm2 = ifelse(Site == "M" & Taxon == "Arisaema parvum" & Date == "2015-08-20" & Individual_number == 2, NA, Leaf_Area_cm2),
         Wet_Mass_g = ifelse(Site == "M" & Taxon == "Arisaema parvum" & Date == "2015-08-20" & Individual_number == 2, NA, Wet_Mass_g))

traitsLong <- traitsWide %>% 
  select(Date, Elevation, Site, Taxon, Individual_number, Leaf_number, Wet_Mass_g.log, Dry_Mass_g.log, Leaf_Thickness_Ave_mm.log, Leaf_Area_cm2.log, SLA_cm2_g, LDMC, C_percent, N_percent, CN_ratio, dN15_percent, dC13_percent, P_percent) %>% 
  gather(key = Traits, value = Value, -Date, -Elevation, -Site, -Taxon, -Individual_number, -Leaf_number)


controlTraitDist <- traitsLong %>% 
  filter(!is.na(Value)) %>% 
  mutate(Traits = factor(Traits, levels = c("Wet_Mass_g.log", "Dry_Mass_g.log", "Leaf_Thickness_Ave_mm.log", "Leaf_Area_cm2.log", "SLA_cm2_g", "LDMC", "C_percent", "N_percent", "CN_ratio", "P_percent", "dN15_percent", "dC13_percent"))) %>% 
  ggplot(aes(x = Value, fill = Site)) +
  geom_density(alpha = 0.5) +
  scale_fill_brewer(palette = "RdBu", direction = -1, labels=c("High alpine", "Alpine", "Middle", "Lowland")) +
  facet_wrap( ~ Traits, scales = "free") +
  theme(legend.position="top")
controlTraitDist
#ggsave(controlTraitDist, filename = "controlTraitDist.jpg", height = 10, width = 10, dpi = 300)

## ----Stuff
traits2 %>% 
  filter(!is.na(Value)) %>% 
  group_by(Traits) %>% 
  summarise(min = min(Value), max = max(Value))


## ----TraitsPlots

DryWet <- traitsWide %>% 
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
#ggsave(Traits, filename = "Traits.jpg", height = 10, width = 10, dpi = 300)


## ----OtherStuff
traits %>% 
  select(Elevation, Site,  Location, Project, Taxon, StoichLabel, C_percent, N_percent, CN_ratio, dN15_percent, dC13_percent, P_AVG) %>% 
  gather(key = Trait, value = Value, C_percent, N_percent, CN_ratio, dN15_percent, dC13_percent, P_AVG) %>% 
  filter(!is.na(Value)) %>% 
  filter(!Location %in% c("LO", "MO", "HO", "AO")) %>% 
  group_by(StoichLabel) %>% 
  summarise(n())

104 Local leaves
324 Experiment