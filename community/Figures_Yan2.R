##load packages
library("tidyverse")
library("vegan")
library("ggvegan")
library("lubridate")
library("gridExtra")
library("cowplot")
library("nlme")
library("broom")

source("community/start_here.R")
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
    mutate(mean = otc - gradient, TTtreat = "OTC") %>% #replaced with default later
    select(-gradient, -otc),
  summer_temp %>% 
    filter(logger == "gradient") %>% 
    mutate(mean = lead(mean) - mean, TTtreat = "warm1") %>% 
    filter(site != "L")
)  %>% 
  select(-logger) %>%
  mutate(mean = if_else(TTtreat == "OTC", 1.5, mean))
warning("using estimated OTC effect for all sites")


#community
cover_thin <- cover_thin %>% 
  filter(TTtreat %in% c("control", "local", "warm1", "OTC"))

#turf environment
turf_env <- tbl(con, "turfEnvironment") %>% collect()


## functional groups
fun_gp <- tbl(con, "taxon") %>% 
  select(species, functionalGroup) %>% 
  collect()


## Calculate responses
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


# make separate data sets for 2016 and different treatments
Gradient <- responses %>% 
  filter(year == 2016, TTtreat %in% c("local", "control")) 
OTC <- responses %>% 
  filter(year == 2016, TTtreat %in% c("local", "OTC")) 
Transplant <- responses %>% 
  filter(year == 2016, TTtreat %in% c("local", "warm1")) 

Transplant <- bind_rows(Transplant = Transplant, Gradient = Gradient, OTC = OTC, .id = "experiment")

# function to get aic values
## ---- regressions_lines
augment_aic <- function(x){
  augment(x) %>% 
    mutate(aic = AIC(x))
}

# compare different models
gradient <- responses %>% 
  filter(year == 2016, TTtreat %in% c("local", "control")) %>% 
  gather(key = variable, value = value, -(originBlockID:year), -mean, -contrast) %>% 
  filter(!is.na(value)) %>% 
  group_by(variable) %>% 
  # mutate(value = scale(value)) %>% 
  do(mod0 = lm(value ~ 1 + mean - mean, data = .),
     mod1 = lm(value ~ mean, data = .),
     mod2 = lm(value ~ mean, data = .) # to have the same number of models (not sure if needed)
  )

gradient %>% 
  ungroup() %>% 
  group_by(variable) %>%
  do(AIC(.$mod0[[1]], .$mod1[[1]]))


transplant <- responses %>%
  filter(year == 2016, TTtreat %in% c("local", "warm1"), originSiteID != "L") %>% 
  gather(key = variable, value = value, -(originBlockID:year), -mean, -contrast) %>% 
  filter(!is.na(value)) %>% 
  group_by(variable) %>% 
  #  mutate(value = scale(value)) %>% 
  do(mod0 = lm(value ~ originSiteID + contrast - contrast, data = .),
     mod1 = lm(value ~ contrast + originSiteID, data = .),
     mod2 = lm(value ~ contrast * originSiteID, data = .)
  )

transplant %>% 
  ungroup() %>% 
  group_by(variable) %>%
  do(AIC(.$mod0[[1]], .$mod1[[1]], .$mod2[[1]])) %>% 
  spread(key = df, value = AIC)


otc <- responses %>%
  filter(year == 2016, TTtreat %in% c("local", "OTC")) %>% 
  gather(key = variable, value = value, -(originBlockID:year), -mean, -contrast) %>% 
  filter(!is.na(value)) %>% 
  group_by(variable) %>% 
  #mutate(value = scale(value)) %>% 
  do(mod0 = lm(value ~ originSiteID + contrast - contrast, data = .),
     mod1 = lm(value ~ contrast + originSiteID, data = .),
     mod2 = lm(value ~ contrast * originSiteID, data = .)
  )

otc %>% 
  ungroup() %>% 
  group_by(variable) %>%
  do(AIC(.$mod0[[1]], .$mod1[[1]], .$mod2[[1]])) %>% 
  spread(key = df, value = AIC)

# bind model outputs and filter for relevant models, tidy up
augmented_reg <- 
  bind_rows(Transplant = transplant, OTC = otc, Gradient = gradient, .id = "experiment") %>% 
  ungroup() %>% 
  group_by(variable, experiment) %>%
  do(
    bind_rows(
      null = augment_aic(.$mod0[[1]]), 
      effect = augment_aic(.$mod1[[1]]), 
      interaction = augment_aic(.$mod2[[1]]), 
      .id = "model")
  ) %>% 
  filter(!(experiment == "Gradient" & model == "interaction")) %>% 
  filter(aic == min(aic)) %>% 
  ungroup() %>% 
  rename(response = "variable") %>% 
  filter(response %in% c("richness", "evenness", "propGraminoid")) %>%
  mutate(
    originSiteID = plyr::mapvalues(originSiteID, c("H", "A", "M", "L"), c("High alpine", "Alpine", "Middle", "Lowland")),
    response = plyr::mapvalues(response, c("richness", "evenness", "propGraminoid"), c("Richness", "Evenness", "Proportion Graminoid")),
    response = factor(response, levels = c("Richness", "Evenness", "Proportion Graminoid")),
    contrast = if_else(experiment == "Gradient", mean, contrast),
    model = factor(model, levels = c("null", "effect", "interaction"), labels = c("No effect", "Effect", "Interaction")))



# preprocess points for plot
dd <- Transplant %>% 
  select(-diversity, -N1, -total_vascular) %>% 
  mutate(xvalue = ifelse(experiment == "Gradient", mean, contrast)) %>% 
  gather(key = response, value = value, richness, evenness, propGraminoid) %>% 
  mutate(response = plyr::mapvalues(response, c("richness", "evenness", "propGraminoid"), c("Richness", "Evenness", "Proportion Graminoid"))) %>% 
  mutate(response = factor(response, levels = c("Richness", "Evenness", "Proportion Graminoid"))) %>% 
  mutate(dummycolor = ifelse(experiment == "Gradient", "Gradient", as.character(originSiteID))) %>% 
  mutate(originSiteID = plyr::mapvalues(originSiteID, c("H", "A", "M", "L"), c("High alpine", "Alpine", "Middle", "Lowland"))) %>% 
  mutate(originSiteID = factor(originSiteID, levels = c("High alpine", "Alpine", "Middle", "Lowland"))) %>% 
  mutate(TTtreat = plyr::mapvalues(TTtreat, c("warm1", "local", "control", "OTC"), c("Transplant", "Local transplant", "Control", "OTC"))) %>% 
  mutate(TTtreat = factor(TTtreat, levels = c("Control", "Local transplant", "OTC", "Transplant"))) %>% 
  filter(!(experiment == "Transplant" & originSiteID == "Lowland"))

# plot Figure 1
p <- ggplot(dd, aes(x = xvalue, y = value, colour = originSiteID, shape = TTtreat)) + 
  geom_jitter(height = 0, width = 0.1, size = 1.8) +
  geom_line(data = filter(augmented_reg, experiment != "Gradient"), aes(y = .fitted, x = contrast, colour = originSiteID, linetype = model), inherit.aes = FALSE) +
  geom_line(data = filter(augmented_reg, experiment == "Gradient") , aes(y = .fitted, x = contrast, linetype = model), colour = "grey40", inherit.aes = FALSE) +
  facet_grid(response ~experiment, scales = "free", space = "free_x") +
  scale_x_continuous(breaks = c(0,1,2,8,10,12)) +
  scale_color_brewer(palette = "RdBu", direction = -1) +
  scale_shape_manual(values = c(1, 16, 18, 17)) +
  scale_linetype_manual(values = c("dotted", "dashed", "solid")) + 
  labs(x = "", y = "", colour = "Site", shape = "Treatment", linetype = "Model")

CommunityPlot <- ggdraw(p) + 
  draw_label("Number of species", x = 0.015, y = 0.925, angle = 90,
           vjust = 1, hjust = 1, size = 14) +
  draw_label("Index", x = 0.015, y = 0.52, angle = 90,
             vjust = 1, hjust = 1, size = 14) +
  draw_label("Proportion", x = 0.015, y = 0.3, angle = 90,
             vjust = 1, hjust = 1, size = 14) +
  draw_label("Temperature °C", x = 0.36, y = 0.03,
             vjust = 1, hjust = 1, size = 14) +
  draw_label("Contrasts °C", x = 0.68, y = 0.03,
             vjust = 1, hjust = 1, size = 14)
CommunityPlot
ggsave(CommunityPlot, filename = "community/FinalFigures/CommunityPlot.jpg", height = 8, width = 10, dpi = 300)




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
  ggplot(aes(x = Experiment, y = estimate, ymax = estimate + 2 * std.error, ymin = estimate - 2 * std.error, shape = Experiment, color = Experiment)) + 
  geom_point(size = 3) +
  geom_errorbar(width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_shape_manual(values = c(16, 18, 17)) +
  scale_color_manual(values = c("grey", "purple", "orange")) +
  labs(y = "Estimate", x = "") +
  facet_wrap(~ variable) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

ggsave(EffectPlot, filename = "community/FinalFigures/EffectPlot.jpg", height = 5, width = 5, dpi = 300)


effects_Table <- bind_rows(Gradient = gradient, Transplant = transplant, OTC = otc, .id = "Experiment") %>% 
  mutate(Experiment = factor(Experiment, levels = c("Gradient", "OTC", "Transplant" ))) %>% 
  ungroup %>% 
  filter(variable %in% c("richness", "evenness", "sumCover", "propGraminoid")) %>% 
  mutate(variable = plyr::mapvalues(variable, c("richness", "evenness", "sumCover", "propGraminoid"), c("Richness", "Evenness", "Sum of Cover", "Proportion Graminoid"))) %>% 
  mutate(variable = factor(variable, levels = c("Richness", "Evenness", "Sum of Cover", "Proportion Graminoid"))) %>% 
  mutate(term = plyr::mapvalues(term, c("(Intercept)", "mean", "contrast"), c("Intercept", "Slope", "Slope"))) %>% 
  select(-std.error) %>% 
  mutate(estimate = round(estimate, 2), statistic = round(statistic, 2), p.value = round(p.value, 3)) %>% 
  rename(Response = variable, Term = term, Estimate = estimate, Statistic = statistic, P.value = p.value)

write.csv(effects_Table, "Effects_Table.csv", row.names = FALSE)



#### Colonization Plot

# dest controls in 2016 compared to origin control in 2012
destC16 <- cover_thin %>% 
  filter(year %in% c(2016)) %>% # first and last year
  filter(TTtreat %in% c("control")) %>% 
  select(turfID, destSiteID, TTtreat, species, cover)

temp <- cover_thin %>% 
  filter(year %in% c(2012)) %>% # first and last year
  filter(TTtreat %in% c("control")) %>% 
  select(turfID, originSiteID, TTtreat, species, cover) %>% 
  full_join(destC16, by = c("originSiteID" = "destSiteID", "turfID", "TTtreat", "species"), suffix = c(".12", ".16")) %>% 
  filter(is.na(cover.12)) %>% 
  mutate(TTtreat = "temporal") %>% 
  rename(First = cover.12, Last = cover.16)


cover_thin %>% 
  filter(year %in% c(2012, 2016)) %>% # first and last year
  filter(TTtreat %in% c("control", "local", "warm1", "OTC")) %>% # 4 treatments
  mutate(year = plyr::mapvalues(year, c(2012, 2016), c("First", "Last"))) %>% 
  spread(key = year, value = cover) %>% # spread by year
  select(turfID, originSiteID, TTtreat, species, First, Last) %>% 
  filter(is.na(First)) %>% # filter all species which were not there in the first year
  bind_rows(temp) %>% 
  ggplot(aes(x = TTtreat, y = Last)) +
  geom_boxplot() +
  facet_wrap(~ originSiteID)
  


  
