##load packages
library("tidyverse")
library("vegan")
library("ggvegan")
library("gridExtra")
library("cowplot")

source("community/start_here.R")
source("community/yans_ms_functions.R")
source("trait_distributions/r_scripts/autoplot.prc.R")
source("community/prcPlotwithoutSP.R")
source("community/prcAutoplotCustom.R")
all_taxa <- read_rds("trait_distributions/data/China_pftc_cwm.rds")

#### PRC IN TRAIT SPACE ####
meta <- cover_thin %>% 
  distinct(originSiteID, turfID, destSiteID, TTtreat) %>% 
  mutate(TTtreat = recode(TTtreat, "warm1" = "Warm1", "cool1" = "Cool1", "control" = "Control", "local" = "Local", "warm3" = "Warm3", "cool3" = "Cool3"))


### ALL TAXA
## prep data for ordination
trait_thin <- all_taxa %>%
  rename("turfID" = "turf", "originSiteID" = "site") %>% 
  left_join(meta, by = c("turfID", "originSiteID")) %>% 
  mutate(newTT = recode(TTtreat, "local" = "Control")) %>% 
  select(turfID:year, destSiteID:newTT, trait, mean) %>% 
  filter(year %in% c(2012, 2014, 2016),
         !trait %in% c("Dry_Mass_g", "Wet_Mass_g")) %>% 
  droplevels() %>% 
  mutate(trait = recode(trait, "C_percent" = "C", "CN_ratio" = "CN ratio", "dC13_percent" = "C13", "dN15_percent" = "N15", "Leaf_Area_cm2" = "Leaf area", "Leaf_Thickness_Ave_mm" = "Thickness", "N_percent" = "N", "NP_ratio" = "NP ratio", "P_AVG" = "P", "SLA_cm2_g" = "SLA"))

  
# Warming
traitFat_Warming <- trait_thin %>% 
  arrange(year) %>%
  filter(newTT %in% c("Control", "Warm1", "Warm3", "OTC")) %>%
  filter(originSiteID == "H" | originSiteID == "L" & newTT == "Control") %>% 
  spread(key = trait, value = mean, fill = 0) %>% 
  mutate(newTT = as.character(newTT)) %>% 
  mutate(newTT = ifelse(newTT == "Control" & originSiteID == "L", "ControlL", newTT)) %>% 
  mutate(year = factor(year), newTT = factor(newTT, levels = c("Control", "OTC", "Warm1", "Warm3", "ControlL")))


fit_Warming <- prc(response = traitData_Warming, treatment = traitFat_Warming$newTT, time = traitFat_Warming$year, scale = TRUE)


# Make figures
p1 <- autoplot.prcCustom(fit_Warming, xlab = "", ylab = "Effect of treatment", legend.position="top") +
  scale_colour_manual(values = c("orange", "pink2", "red", "red")) +
  scale_linetype_manual(values = c("solid", "dashed", "dashed", "solid")) +
  scale_y_continuous(breaks = pretty(fortify(fit_Warming)$Response, n = 5), trans = "reverse")

p2 <- autoplot.prcWithoutSP(fit_Warming, xlab = "", ylab = "Treatment effect on \n  trait composition") +
  scale_colour_manual(values = c("orange", "pink2", "red", "red")) +
  scale_linetype_manual(values = c("solid", "dashed", "dashed", "solid")) +
  scale_y_continuous(breaks = pretty(fortify(fit_Warming)$Response, n = 5), trans = "reverse") +
  theme(legend.position = c(0.7, 0.3), legend.title = element_blank())

p3 <- fortify(fit_Warming) %>% 
  filter(Score == "Species") %>% 
  mutate(X = 1) %>% 
  ggplot(aes(x = X, y = (Response), label = Label)) +
  geom_text(aes(x = X), size = 4) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = pretty(fortify(fit_Warming)$Response, n = 5), trans = "reverse") +
  labs(x = "", y = "") +
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

prcLegend <- cowplot::get_legend(p1)


TraitAllTaxa_Warming <- grid.arrange(p2, p3,
             layout_matrix = rbind(c(2,2,2,2,2,3,3)))

# Testing
traitFat_Warming2 <- traitFat_Warming %>% 
  filter(newTT != "Warm3")
traitData_Warming2 <- traitFat_Warming2 %>% 
  select(-(turfID:newTT))

anova(prc(response = traitData_Warming2, treatment = traitFat_Warming2$newTT, time = traitFat_Warming2$year, scale = TRUE))



## Cooling
traitFat_Cool <- trait_thin %>% 
  arrange(year) %>%
  filter(newTT %in% c("Control", "Cool1", "Cool3")) %>%
  filter(originSiteID == "L" | originSiteID == "H" & newTT == "Control") %>% 
  spread(key = trait, value = mean, fill = 0) %>% 
  mutate(newTT = as.character(newTT)) %>% 
  mutate(newTT = ifelse(newTT == "Control" & originSiteID == "H", "ControlH", newTT)) %>% 
  mutate(year = factor(year), newTT = factor(newTT, levels = c("Control", "Cool1", "Cool3", "ControlH")))

traitData_Cool <- traitFat_Cool %>% select(-(turfID:newTT))

fit_Cool <- prc(response = traitData_Cool, treatment = traitFat_Cool$newTT, time = traitFat_Cool$year, scale = TRUE)

# Make figures
p2 <- autoplot.prcWithoutSP(fit_Cool, xlab = "", ylab = "") +
  scale_colour_manual(values = c("steelblue2", "blue", "blue")) +
  scale_linetype_manual(values = c("dashed", "dashed", "solid")) +
  scale_y_continuous(breaks = pretty(fortify(fit_Cool)$Response, n = 5), trans = "reverse") +
  theme(legend.position = c(0.7, 0.6), legend.title = element_blank())

p3 <- fortify(fit_Cool) %>% 
  filter(Score == "Species") %>% 
  mutate(X = 1) %>% 
  ggplot(aes(x = X, y = Response, label = Label)) +
  geom_text(aes(x = X), size = 4) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = pretty(fortify(fit_Cool)$Response, n = 5), trans = "reverse") +
  labs(x = "", y = "") +
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

TraitAllTaxa_Cooling <- grid.arrange(p2, p3,
             layout_matrix = rbind(c(1,1,1,1,1,2,2)))


# Testing
traitFat_Cool2 <- traitFat_Cool %>% 
  filter(newTT != "Cool3")
traitData_Cool2 <- traitFat_Cool2 %>% 
  select(-(turfID:newTT))

anova(prc(response = traitData_Cool2, treatment = traitFat_Cool2$newTT, time = traitFat_Cool2$year, scale = TRUE))



### WITHOUTH GRAMINOIDS
## prep data for ordination
traitNG_thin <- no_graminoids %>% 
  rename("turfID" = "turf", "originSiteID" = "site") %>% 
  left_join(meta, by = c("turfID", "TTtreat", "originSiteID")) %>% 
  mutate(newTT = recode(TTtreat, "local" = "Control")) %>% 
  select(turfID:year, destSiteID:newTT, trait, mean) %>% 
  filter(year %in% c(2012, 2014, 2016),
         !trait %in% c("Dry_Mass_g", "Wet_Mass_g")) %>% 
  droplevels()


# Warming
traitFatNG_Warming <- traitNG_thin %>% 
  arrange(year) %>%
  filter(newTT %in% c("Control", "OTC", "Warm1", "Warm3")) %>%
  filter(originSiteID == "H" | originSiteID == "L" & newTT == "Control") %>% 
  spread(key = trait, value = mean, fill = 0) %>% 
  mutate(newTT = as.character(newTT)) %>% 
  mutate(newTT = ifelse(newTT == "Control" & originSiteID == "L", "ControlL", newTT)) %>% 
  mutate(year = factor(year), newTT = factor(newTT, levels = c("Control", "OTC", "Warm1", "Warm3", "ControlL")))

traitDataNG_Warming <- traitFatNG_Warming %>% select(-(turfID:newTT))

fitNG_Warming <- prc(response = traitDataNG_Warming, treatment = traitFatNG_Warming$newTT, time = traitFatNG_Warming$year, scale = TRUE)

# Make figures
p1 <- autoplot.prc(fitNG_Warming, xlab = "", ylab = "Effect of treatment", legend.position="top") +
  scale_colour_manual(values = c("orange", "pink2", "red", "red")) +
  scale_linetype_manual(values = c("solid", "dashed", "dashed", "solid")) +
  scale_y_continuous(breaks = pretty(fortify(fitNG_Warming)$Response, n = 5), trans = "reverse")

p2 <- autoplot.prc(fitNG_Warming, xlab = "", ylab = "Effect of treatment") +
  scale_colour_manual(values = c("orange", "pink2", "red", "red")) +
  scale_linetype_manual(values = c("solid", "dashed", "dashed", "solid")) +
  scale_y_continuous(breaks = pretty(fortify(fitNG_Warming)$Response, n = 5), trans = "reverse") +
  theme(legend.position = "none")

p3 <- fortify(fitNG_Warming) %>% 
  filter(Score == "Species") %>% 
  mutate(X = 1) %>% 
  ggplot(aes(x = X, y = Response, label = Label)) +
  geom_text(aes(x = X), size = 3) +
  scale_y_continuous(breaks = pretty(fortify(fitNG_Warming)$Response, n = 5), trans = "reverse") +
  labs(x = "", y = "") +
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

prcLegend <- cowplot::get_legend(p1)


TraitForbs_Warming <- grid.arrange(prcLegend, p2, p3, 
                                     heights = 0.2:2,
                                     layout_matrix = rbind(c(1),cbind(c(2), c(2), c(2), c(2), c(2), c(3))))




# Cooling
traitFatNG_Cool <- traitNG_thin %>% 
  arrange(year) %>%
  filter(newTT %in% c("Control", "Cool1", "Cool3")) %>%
  filter(originSiteID == "L" | originSiteID == "H" & newTT == "Control") %>% 
  spread(key = trait, value = mean, fill = 0) %>% 
  mutate(newTT = as.character(newTT)) %>% 
  mutate(newTT = ifelse(newTT == "Control" & originSiteID == "H", "ControlH", newTT)) %>% 
  mutate(year = factor(year), newTT = factor(newTT, levels = c("Control", "Cool1", "Cool3", "ControlH")))

traitDataNG_Cool <- traitFatNG_Cool %>% select(-(turfID:newTT))

fitNG_Cool <- prc(response = traitDataNG_Cool, treatment = traitFatNG_Cool$newTT, time = traitFatNG_Cool$year, scale = TRUE)

# Make figures
p1 <- autoplot.prc(fitNG_Cool, xlab = "", ylab = "Effect of treatment", legend.position="top") +
  scale_colour_manual(values = c("steelblue2", "blue", "blue")) +
  scale_linetype_manual(values = c("dashed", "dashed", "solid")) +
  scale_y_continuous(breaks = pretty(fortify(fitNG_Cool)$Response, n = 5), trans = "reverse")

p2 <- autoplot.prc(fitNG_Cool, xlab = "", ylab = "Effect of treatment") +
  scale_colour_manual(values = c("steelblue2", "blue", "blue")) +
  scale_linetype_manual(values = c("dashed", "dashed", "solid")) +
  scale_y_continuous(breaks = pretty(fortify(fitNG_Cool)$Response, n = 5), trans = "reverse") +
  theme(legend.position = "none")

p3 <- fortify(fitNG_Cool) %>% 
  filter(Score == "Species") %>% 
  mutate(X = 1) %>% 
  ggplot(aes(x = X, y = Response, label = Label)) +
  geom_text(aes(x = X), size = 3) +
  scale_y_continuous(breaks = pretty(fortify(fitNG_Cool)$Response, n = 5), trans = "reverse") +
  labs(x = "", y = "") +
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

prcLegend <- cowplot::get_legend(p1)


TraitForbs_Cooling <- grid.arrange(prcLegend, p2, p3, 
                                     heights = 0.2:2,
                                     layout_matrix = rbind(c(1),cbind(c(2), c(2), c(2), c(2), c(2), c(3))))


ggsave(TraitAllTaxa_Warming, filename = "trait_distributions/TraitAllTaxa_Warming.jpg", dpi = 300)
ggsave(TraitAllTaxa_Cooling, filename = "trait_distributions/TraitAllTaxa_Cooling.jpg", dpi = 300)
ggsave(TraitForbs_Warming, filename = "trait_distributions/TraitForbs_Warming.jpg", dpi = 300)
ggsave(TraitForbs_Cooling, filename = "trait_distributions/TraitForbs_Cooling.jpg", dpi = 300)



#### PRC IN COMMUNITY SPACE ####

## functional groups
fun_gp <- tbl(con, "taxon") %>% 
  select(species, functionalGroup) %>% 
  collect()

### ALL TAXA
## prep data for ordination
coverFatComm_Warming <- cover_thin %>% 
  select(-speciesName, -flag) %>% 
  left_join(fun_gp) %>% 
  # filter(functionalGroup == "forb") %>% 
  select(-functionalGroup) %>% 
  filter(year %in% c(2012, 2014, 2016)) %>% 
  arrange(year) %>%
  filter(TTtreat %in% c("control", "warm1", "warm3", "OTC")) %>%
  mutate(newTT = plyr::mapvalues(TTtreat, c("warm1", "warm3", "control", "local", "OTC"), c("Warm1", "Warm3", "Control", "Control", "OTC"))) %>% 
  filter(originSiteID == "H" | originSiteID == "L" & newTT == "Control") %>% 
  # remove rare species
  group_by(species) %>%
  filter(n() > 3) %>% 
  spread(key = species, value = cover, fill = 0) %>% 
  mutate(newTT = as.character(newTT)) %>% 
  mutate(newTT = ifelse(newTT == "Control" & originSiteID == "L", "ControlL", newTT)) %>% 
  mutate(year = factor(year), newTT = factor(newTT, levels = c("Control", "OTC", "Warm1", "Warm3", "ControlL")))

communityData_Warming <- coverFatComm_Warming %>% select(-(originSiteID:newTT))

fitCommunity_Warming <- prc(response = communityData_Warming, treatment = coverFatComm_Warming$newTT, time = coverFatComm_Warming$year)

p2 <- autoplot.prcWithoutSP(fitCommunity_Warming, xlab = "", ylab = "Treatment effect on \n species composition", legend.position = "none") +
  scale_colour_manual(values = c("orange", "pink2", "red", "red")) +
  scale_linetype_manual(values = c("solid", "dashed", "dashed", "solid")) +
  scale_y_continuous(trans = "reverse")

p3 <- fortify(fit_Warming) %>% 
  filter(Score == "Species") %>% 
  mutate(X = 1) %>% 
  ggplot(aes(x = X, y = (Response), label = Label)) +
  theme_void()

CommunityAllTaxa_Warming <- grid.arrange(p2, p3,
                                     layout_matrix = rbind(c(2,2,2,2,2,3,3)))


# Testing
coverFatComm_Warming2 <- coverFatComm_Warming %>% 
  filter(newTT != "Warm3")
communityData_Warming2 <- coverFatComm_Warming2 %>% 
  select(-(originSiteID:newTT))

anova(prc(response = communityData_Warming2, treatment = coverFatComm_Warming2$newTT, time = coverFatComm_Warming2$year)
)



## Cooling
coverFatComm_Cooling <- cover_thin %>% 
  select(-speciesName, -flag) %>% 
  left_join(fun_gp) %>% 
  # filter(functionalGroup == "forb") %>% 
  select(-functionalGroup) %>% 
  filter(year %in% c(2012, 2014, 2016)) %>% 
  arrange(year) %>%
  filter(TTtreat %in% c("control", "cool1", "cool3")) %>%
  mutate(newTT = plyr::mapvalues(TTtreat, c("cool1", "cool3", "control", "local"), c("Cool1", "Cool3", "Control", "Control"))) %>% 
  filter(originSiteID == "L" | originSiteID == "H" & newTT == "Control") %>% 
  # remove rare species
  group_by(species) %>%
  filter(n() > 3) %>% 
  spread(key = species, value = cover, fill = 0) %>% 
  mutate(newTT = as.character(newTT)) %>% 
  mutate(newTT = ifelse(newTT == "Control" & originSiteID == "H", "ControlH", newTT)) %>% 
  mutate(year = factor(year), newTT = factor(newTT, levels = c("Control", "Cool1", "Cool3", "ControlH")))

communityData_Cooling <- coverFatComm_Cooling %>% select(-(originSiteID:newTT))

fitCommunity_Cooling <- prc(response = communityData_Cooling, treatment = coverFatComm_Cooling$newTT, time = coverFatComm_Cooling$year)

p2 <- autoplot.prcWithoutSP(fitCommunity_Cooling, xlab = "", ylab = "", legend.position = "none") +
  scale_colour_manual(values = c("steelblue2", "blue", "blue")) +
  scale_linetype_manual(values = c("dashed", "dashed", "solid")) +
  scale_y_continuous(trans = "reverse")

p3 <- fortify(fit_Warming) %>% 
  filter(Score == "Species") %>% 
  mutate(X = 1) %>% 
  ggplot(aes(x = X, y = (Response), label = Label)) +
  theme_void()

CommunityAllTaxa_Cooling <- grid.arrange(p2, p3,
                                         layout_matrix = rbind(c(2,2,2,2,2,3,3)))


# Testing
coverFatComm_Cooling2 <- coverFatComm_Cooling %>% 
  filter(newTT != "Cool3")
communityData_Cooling2 <- coverFatComm_Cooling2 %>% 
  select(-(originSiteID:newTT))

anova(prc(response = communityData_Cooling2, treatment = coverFatComm_Cooling2$newTT, time = coverFatComm_Cooling2$year)
)


TDTFinalFig <- plot_grid(CommunityAllTaxa_Warming, CommunityAllTaxa_Cooling, TraitAllTaxa_Warming, TraitAllTaxa_Cooling,
          labels = c("a)", "b)", "c)", "d)"),
          ncol = 2)

save_plot("traits/TDTFinalFig.jpeg", TDTFinalFig, base_height = 10, base_width = 15)


### ONLY FORBS
## prep data for ordination
coverFatCommNG_Warming <- cover_thin %>% 
  select(-speciesName, -flag) %>% 
  left_join(fun_gp) %>% 
  filter(functionalGroup == "forb") %>% 
  select(-functionalGroup) %>% 
  filter(year %in% c(2012, 2014, 2016)) %>% 
  arrange(year) %>%
  filter(TTtreat %in% c("control", "warm1", "warm3", "OTC")) %>%
  mutate(newTT = plyr::mapvalues(TTtreat, c("warm1", "warm3", "control", "local", "OTC"), c("Warm1", "Warm3", "Control", "Control", "OTC"))) %>% 
  filter(originSiteID == "H" | originSiteID == "L" & newTT == "Control") %>% 
  # remove rare species
  group_by(species) %>%
  filter(n() > 3) %>% 
  spread(key = species, value = cover, fill = 0) %>% 
  mutate(newTT = as.character(newTT)) %>% 
  mutate(newTT = ifelse(newTT == "Control" & originSiteID == "L", "ControlL", newTT)) %>% 
  mutate(year = factor(year), newTT = factor(newTT, levels = c("Control", "OTC", "Warm1", "Warm3", "ControlL")))

communityDataNG_Warming <- coverFatCommNG_Warming %>% select(-(originSiteID:newTT))

fitCommunityNG_Warming <- prc(response = communityDataNG_Warming, treatment = coverFatCommNG_Warming$newTT, time = coverFatCommNG_Warming$year)

CommunityForbs_Warming <- autoplot.prcWithoutSP(fitCommunityNG_Warming, xlab = "", ylab = "Effect of treatment", legend.position="top") +
  scale_colour_manual(values = c("orange", "pink", "red", "red")) +
  scale_linetype_manual(values = c("solid", "dashed", "dashed", "solid"))



## Cooling
coverFatCommNG_Cooling <- cover_thin %>% 
  select(-speciesName, -flag) %>% 
  left_join(fun_gp) %>% 
  filter(functionalGroup == "forb") %>% 
  select(-functionalGroup) %>% 
  filter(year %in% c(2012, 2014, 2016)) %>% 
  arrange(year) %>%
  filter(TTtreat %in% c("control", "cool1", "cool3")) %>%
  mutate(newTT = plyr::mapvalues(TTtreat, c("cool1", "cool3", "control", "local"), c("Cool1", "Cool3", "Control", "Control"))) %>% 
  filter(originSiteID == "L" | originSiteID == "H" & newTT == "Control") %>% 
  # remove rare species
  group_by(species) %>%
  filter(n() > 3) %>% 
  spread(key = species, value = cover, fill = 0) %>% 
  mutate(newTT = as.character(newTT)) %>% 
  mutate(newTT = ifelse(newTT == "Control" & originSiteID == "H", "ControlH", newTT)) %>% 
  mutate(year = factor(year), newTT = factor(newTT, levels = c("Control", "Cool1", "Cool3", "ControlH")))

communityDataNG_Cooling <- coverFatCommNG_Cooling %>% select(-(originSiteID:newTT))

fitCommunityNG_Cooling <- prc(response = communityDataNG_Cooling, treatment = coverFatCommNG_Cooling$newTT, time = coverFatCommNG_Cooling$year)

CommunityForbs_Cooling <- autoplot.prcWithoutSP(fitCommunityNG_Cooling, xlab = "", ylab = "Effect of treatment", legend.position="top") +
  scale_colour_manual(values = c("lightblue", "blue", "blue")) +
  scale_linetype_manual(values = c("dashed", "dashed", "solid"))  +
  scale_y_continuous(trans = "reverse")



ggsave(CommunityAllTaxa_Warming, filename = "trait_distributions/CommunityAllTaxa_Warming.jpg", dpi = 300)
ggsave(CommunityAllTaxa_Cooling, filename = "trait_distributions/CommunityAllTaxa_Cooling.jpg", dpi = 300)
ggsave(CommunityForbs_Warming, filename = "trait_distributions/CommunityForbs_Warming.jpg", dpi = 300)
ggsave(CommunityForbs_Cooling, filename = "trait_distributions/CommunityForbs_Cooling.jpg", dpi = 300)


### Traits
tt <- fortify(fitNG_Warming) %>% 
  rbind(fortify(fitNG_Cool)) %>% 
  filter(Score == "Sample",
         Time == 2016) %>% 
  select(-Score, -Label, -Time) %>% 
  mutate(Test = c(rep("Warm", 4), rep("Cool", 3)),
         Response = abs(Response)) %>% 
  mutate(All = ifelse(Test == "Warm", 0.50180259, 0.48543069),
         Proportion = round(Response * 100 / All, 2),
         Analysis = "Trait")

         

### Community
cc <- fortify(fitCommunityNG_Warming) %>% 
  rbind(fortify(fitCommunityNG_Cooling)) %>% 
  filter(Score == "Sample",
         Time == 2016) %>% 
  select(-Score, -Label, -Time) %>% 
  mutate(Test = c(rep("Warm", 4), rep("Cool", 3)),
         Response = abs(Response)) %>% 
  mutate(All = ifelse(Test == "Warm", 0.68228820, 0.63840699),
         Proportion = round(Response * 100 / All, 2),
         Analysis = "Community")


tt %>% 
  rbind(cc) %>% 
  select(-Response, -Test, -All) %>%
  filter(!Treatment %in% c("ControlL", "ControlH")) %>% 
  spread(key = Treatment, value = Proportion)
