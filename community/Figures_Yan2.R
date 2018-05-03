##load packages
library("tidyverse")
library("vegan")
library("ggvegan")
library("lubridate")
library("gridExtra")
library("cowplot")
library("nlme")
library("broom")

source("community/yans_ms_functions.R")


##load data
#climate
load("climate/climate_month.Rdata")
summer_temp <- climate_month %>% 
  filter(variable == "Tair") %>% 
  mutate(month = month(month)) %>%
  filter(month %in% 6:8) %>% 
  group_by(logger, site) %>% 
  summarise(mean = mean(value, na.rm = TRUE)) %>% 
  arrange(site) %>%
  ungroup()

#control = Tair
#OTC = OTC - Tair
#Warm1 = TairD - TairO  

pred_temp <- bind_rows(
  summer_temp %>% 
    filter(logger == "gradient") %>% 
    mutate(TTtreat = "control"),#control
  summer_temp %>% 
    filter(logger == "gradient") %>% 
    mutate(TTtreat = "local"),#local transplant (== control)
  summer_temp %>% 
    spread(key = logger, value = mean) %>% 
    mutate(mean = otc - gradient, TTtreat = "OTC") %>% 
    select(-gradient, -otc),
  summer_temp %>% 
    filter(logger == "gradient") %>% 
    mutate(mean = lead(mean) - mean, TTtreat = "warm1") %>% 
    filter(site != "L")
)  %>% 
  select(-logger) %>%
  mutate(mean = if_else(site == "L" & TTtreat == "OTC", 2, mean))
warning("using estimated OTC effect for site L")


#community
cover_thin <- cover_thin %>% 
  filter(TTtreat %in% c("control", "local", "warm1", "OTC"))

#turf environment
turf_env <- tbl(con, "turfEnvironment") %>% collect()


## functional groups
fun_gp <- tbl(con, "taxon") %>% 
  select(species, functionalGroup) %>% 
  collect()

## ordination
cover_fat <- cover_thin %>% 
  select(-speciesName, -flag) %>% 
  arrange(year) %>%
  filter(TTtreat %in% c("local", "control", "warm1", "OTC")) %>%
  spread(key = species, value = cover, fill = 0) %>%
  mutate(TTtreat = droplevels(TTtreat))

cover_fat_spp <- cover_fat %>% select(-(originSiteID:year))

NMDS <- metaMDS(cover_fat_spp, noshare = TRUE, try = 30)#DNC

fNMDS <- fortify(NMDS) %>% 
  filter(Score == "sites") %>%
  bind_cols(cover_fat %>% select(originSiteID:year))

treat_colours <- c("black", "grey50", "red", "green")

g <- ggplot(fNMDS, aes(x = Dim1, y = Dim2, shape = originSiteID, colour = TTtreat, group = originPlotID, fill = TTtreat)) +
  geom_point(aes(size = ifelse(year == min(year), "First", "Other"))) +
  geom_path() + 
  coord_equal() +
  scale_size_discrete(range = c(1, 2.5), limits = c("Other", "First"), breaks = c("First", "Other")) +
  scale_colour_manual(values = treat_colours, limits = levels(cover_fat$TTtreat)) +
  scale_fill_manual(values = treat_colours, limits = levels(cover_fat$TTtreat)) +
  scale_shape_manual(values = c(24, 22, 23, 25), limits = levels(cover_fat$originSiteID)) +
  guides(shape = guide_legend(override.aes = list(fill = "black"))) +
  labs(x = "NMDS 1", y = "NMDS 2", colour = "Treatment", fill = "Treatment", shape = "Site", size = "Year")

HA <- two_sites_nmds("H", "A")
AM <- two_sites_nmds("A", "M")
ML <- two_sites_nmds("M", "L")
LM <- two_sites_nmds("L", "M")


gg <- ggplotGrob(g)$grobs
legend <- gg[[which(sapply(gg, function(x) x$name) == "guide-box")]]
p <- g + theme(legend.position = "none", axis.title = element_blank())
OrdinationPlot <- grid.arrange(p %+% HA + ggtitle("H - A"),
             p %+% AM + ggtitle("A - M"),
             p %+% ML + ggtitle("M - L"),
             p %+% LM + ggtitle("L - "),
             legend,
             layout_matrix = rbind(c(1, 2, 5), c(3, 4, 5)), widths = c(.4, .4, .2),
             bottom = "NMDS1", left = "NMDS2")


HA <- p %+% HA + ggtitle("H - A")
AM <- p %+% AM + ggtitle("A - M")
ML <- p %+% ML + ggtitle("M - L")
LM <- p %+% LM + ggtitle("L - ")
pp <- plot_grid(HA, AM, ML, LM, nrow = 2, align = "hv")
ppp <- plot_grid(pp, legend, rel_widths = c(1, .27))

OrdinationPlot <- ggdraw(ppp) + 
  draw_label("NMDS2", x = 0.02, y = 0.55, angle = 90, vjust = 1, hjust = 1, size = 14) +
  draw_label("NMDS1", x = 0.45, y = 0.03, vjust = 1, hjust = 1, size = 14)
ggsave(OrdinationPlot, filename = "community/FinalFigures/OrdinationPlot.jpg", height = 7, width = 8, dpi = 300)




## responses
responses <- cover_thin %>% 
  left_join(fun_gp) %>% 
  left_join(turf_env) %>% 
  group_by(turfID, originBlockID, TTtreat, originSiteID, year) %>%  
  summarise(richness = n(), 
            diversity = diversity(cover), 
            N1 = exp(diversity),
            evenness = diversity/log(richness),
            sumCover = sum(cover),
            propGraminoid = sum(cover[functionalGroup %in% c("gramineae", "sedge")])/sumCover,
            total_vascular = first(totalVascular)
  ) %>% 
  group_by(originBlockID, TTtreat, originSiteID, year) %>%
  select(-turfID) %>% 
  full_join(pred_temp, by = c("originSiteID" = "site", "TTtreat" = "TTtreat")) %>% 
  ungroup() %>% 
  mutate(contrast = if_else(TTtreat %in% c("local", "control"), 0, mean), 
         TTtreat = factor(TTtreat, levels = c("control", "local", "warm1", "OTC"))) 



Gradient <- responses %>% 
  filter(year == 2016, TTtreat %in% c("local", "control")) %>% 
  mutate(experiment = "Gradient")
OTC <- responses %>% 
  filter(year == 2016, TTtreat %in% c("local", "OTC")) %>% 
  mutate(experiment = "OTC")
Transplant <- responses %>% 
  filter(year == 2016, TTtreat %in% c("local", "warm1")) %>% 
  mutate(experiment = "Transplant") %>%  
  rbind(Gradient, OTC)

dd <- Transplant %>% 
  select(-diversity, -N1, -total_vascular) %>% 
  mutate(xvalue = ifelse(experiment == "Gradient", mean, contrast)) %>% 
  gather(key = response, value = value, richness, evenness, sumCover, propGraminoid) %>% 
  mutate(response = plyr::mapvalues(response, c("richness", "evenness", "sumCover", "propGraminoid"), c("Richness", "Evenness", "Sum of Cover", "Proportion Graminoid"))) %>% 
  mutate(response = factor(response, levels = c("Richness", "Evenness", "Sum of Cover", "Proportion Graminoid"))) %>% 
  mutate(dummycolor = ifelse(experiment == "Gradient", "Gradient", as.character(originSiteID)))

p <- ggplot(dd, aes(x = xvalue, y = value, colour = originSiteID, shape = TTtreat)) + 
  geom_jitter(height = 0, width = 0.1, size = 1.8) +
  geom_smooth(data = filter(dd, experiment != "Gradient"), method = "lm", se = FALSE, aes(x = xvalue, y = value, colour = dummycolor), inherit.aes = FALSE, size = 0.6) +
  geom_smooth(data = filter(dd, experiment == "Gradient"), method = "lm", se = FALSE, aes(x = xvalue, y = value), inherit.aes = FALSE, size = 0.6, colour = "grey40") +
  facet_grid(response ~experiment, scales = "free", space = "free_x") +
  scale_x_continuous(breaks = c(0,2,8,10,12)) +
  scale_color_brewer(palette = "Set1") +
  scale_shape_manual(values = c(1, 16, 17, 18)) +
  labs(x = "", y = "", colour = "Site", shape = "Treatment")
p

CommunityPlot <- ggdraw(p) + 
  draw_label("Number", x = 0.015, y = 0.87, angle = 90,
           vjust = 1, hjust = 1, size = 14) +
  draw_label("Index", x = 0.015, y = 0.62, angle = 90,
             vjust = 1, hjust = 1, size = 14) +
  draw_label("Sum", x = 0.015, y = 0.45, angle = 90,
             vjust = 1, hjust = 1, size = 14) +
  draw_label("Proportion", x = 0.015, y = 0.25, angle = 90,
             vjust = 1, hjust = 1, size = 14) +
  draw_label("Temperature °C", x = 0.36, y = 0.03,
             vjust = 1, hjust = 1, size = 14) +
  draw_label("Contrasts °C", x = 0.71, y = 0.03,
             vjust = 1, hjust = 1, size = 14)
ggsave(filename = "community/FinalFigures/CommunityPlot.jpg", height = 8, width = 10, dpi = 300)




# table of effects
## ---- regressions
gradient <- responses %>% 
  filter(year == 2016, TTtreat %in% c("local", "control")) %>% 
  gather(key = variable, value = value, -(originBlockID:year), -mean, -contrast) %>% 
  filter(!is.na(value)) %>% 
  group_by(variable) %>% 
  mutate(value = scale(value)) %>% 
  do({
    mod = lme(value ~ mean, random =  ~ 1|originSiteID, data = .)
    tidy(mod, effects = "fixed")
  })

transplant <- responses %>%
  filter(year == 2016, TTtreat %in% c("local", "warm1"), originSiteID != "L") %>% 
  gather(key = variable, value = value, -(originBlockID:year), -mean, -contrast) %>% 
  filter(!is.na(value)) %>% 
  group_by(variable) %>% 
  mutate(value = scale(value)) %>% 
  do({
    mod = lme(value ~ contrast, random =  ~ 1|originSiteID, data = .)
    tidy(mod, effects = "fixed")
  })

otc <- responses %>%
  filter(year == 2016, TTtreat %in% c("local", "OTC")) %>% 
  gather(key = variable, value = value, -(originBlockID:year), -mean, -contrast) %>% 
  filter(!is.na(value)) %>% 
  group_by(variable) %>% 
  mutate(value = scale(value)) %>% 
  do({
    mod = lme(value ~ contrast, random =  ~ 1|originSiteID, data = .)
    tidy(mod, effects = "fixed")
  })

effects <- bind_rows(Gradient = gradient, Transplant = transplant, OTC = otc, .id = "Experiment") %>% 
  mutate(Experiment = factor(Experiment, levels = c("Gradient", "OTC", "Transplant" ))) %>% 
  filter(term != "(Intercept)") 


EffectPlot <- effects %>% 
  filter(!variable %in% c("N1", "diversity", "total_vascular")) %>% 
  ungroup() %>% 
  mutate(variable = plyr::mapvalues(variable, c("richness", "evenness", "sumCover", "propGraminoid"), c("Richness", "Evenness", "Sum of Cover", "Proportion Graminoid"))) %>% 
  mutate(variable = factor(variable, levels = c("Richness", "Evenness", "Sum of Cover", "Proportion Graminoid"))) %>% 
  ggplot(aes(x = Experiment, y = estimate, ymax = estimate + 2 * std.error, ymin = estimate - 2 * std.error, shape = Experiment)) + 
  geom_point(size = 3) +
  geom_errorbar(width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_shape_manual(values = c(16, 18, 17)) +
  labs(y = "Estimate", x = "") +
  facet_wrap(~ variable) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

ggsave(EffectPlot, filename = "community/FinalFigures/EffectPlot.jpg", height = 5, width = 5, dpi = 300)
