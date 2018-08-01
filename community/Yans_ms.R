# Analyses for Yan et al
# Finse April 2017

#Effect of temperature treatments (gradient/transplant/OTC) on different responses (richness, evenness, grass/herb ratio. Change in effect over time

##load packages
library("tidyverse")
library("vegan")
library("ggvegan")
library("lubridate")
library("gridExtra")
library("broom")

## ---- load_data

##load data
#climate
load("../climate/climate_month.Rdata")
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

fun_gp %>% count(functionalGroup)

## ---- ordination
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

g <- ggplot(fNMDS, aes(x = NMDS1, y = NMDS2, shape = originSiteID, colour = TTtreat, group = originPlotID, fill = TTtreat)) +
  geom_point(aes(size = ifelse(year == min(year), "First", "Other"))) +
  geom_path() + 
  coord_equal() +
  scale_size_discrete(range = c(1, 2.5), limits = c("Other", "First"), breaks = c("First", "Other")) +
  scale_colour_manual(values = treat_colours, limits = levels(cover_fat$TTtreat)) +
  scale_fill_manual(values = treat_colours, limits = levels(cover_fat$TTtreat)) +
  scale_shape_manual(values = c(24, 22, 23, 25), limits = levels(cover_fat$originSiteID)) +
  guides(shape = guide_legend(override.aes = list(fill = "black"))) +
  labs(x = "NMDS 1", y = "NMDS 2", colour = "Treatment", fill = "Treatment", shape = "Site", size = "Year")
#g


 
HA <- two_sites_nmds("H", "A")
AM <- two_sites_nmds("A", "M")
ML <- two_sites_nmds("M", "L")
LM <- two_sites_nmds("L", "M")


gg <- ggplotGrob(g)$grobs
legend <- gg[[which(sapply(gg, function(x) x$name) == "guide-box")]]
g <- g + theme(legend.position = "none", axis.title = element_blank())
grid.arrange(g %+% HA + ggtitle("H - A"),
             g %+% AM + ggtitle("A - M"),
             g %+% ML + ggtitle("M - L"),
             g %+% LM + ggtitle("L - "),
             legend,
             layout_matrix = rbind(c(1, 2, 5), c(3, 4, 5)), widths = c(.4, .4, .2),
             bottom = "NMDS1", left = "NMDS2")


## ---- responses
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

plot_three_treatments(responses, column = "richness", ylab = "Species Richness")
plot_three_treatments(responses, column = "evenness", ylab = "Species Evenness")
plot_three_treatments(responses, column = "diversity", ylab = "Diversity")
plot_three_treatments(responses, column = "sumCover", ylab = "Sum of Covers")
plot_three_treatments(responses, column = "propGraminoid", ylab = "Proportional Graminoids")
plot_three_treatments(responses, column = "total_vascular", ylab = "Total vascular cover")

# table of effects
## ---- regressions

augment_aic <- function(x){
  augment(x) %>% 
    mutate(aic = AIC(x))
}


gradient <- responses %>% 
  filter(year == 2016, TTtreat %in% c("local", "control")) %>% 
  gather(key = variable, value = value, -(originBlockID:year), -mean, -contrast) %>% 
  filter(!is.na(value)) %>% 
  group_by(variable) %>% 
 # mutate(value = scale(value)) %>% 
  do(
       mod0 = lm(value ~ originSiteID + mean - mean, data = .),
       mod1 = lm(value ~ mean + originSiteID, data = .),
       mod2 = lm(value ~ mean, data = .)
    )

gradient %>% 
  ungroup() %>% 
  group_by(variable) %>%
  do(AIC(.$mod0[[1]], .$mod1[[1]], .$mod2[[1]]))


  

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
  filter(!(experiment == "Gradient" & model != "interaction")) %>% 
  filter(aic == min(aic)) %>% 
  ungroup() %>% 
  rename(response = "variable") %>% 
  filter(response %in% c("richness", "evenness", "sumCover", "propGraminoid")) %>% 
  mutate(
    originSiteID = plyr::mapvalues(originSiteID, c("H", "A", "M", "L"), c("High alpine", "Alpine", "Middle", "Lowland")),
    response = plyr::mapvalues(response, c("richness", "evenness", "sumCover", "propGraminoid"), c("Richness", "Evenness", "Sum of Cover", "Proportion Graminoid")),
    response = factor(response, levels = c("Richness", "Evenness", "Sum of Cover", "Proportion Graminoid")),
    contrast = if_else(experiment == "Gradient", mean, contrast),
    model = factor(model, levels = c("null", "effect", "interaction"), labels = c("Null", "Effect", "Interaction"))) 
  





effects <- bind_rows(Gradient = gradient, Transplant = transplant, OTC = otc, .id = "Experiment") %>% 
  mutate(Experiment = factor(Experiment, levels = c("Gradient", "OTC", "Transplant" ))) %>% 
  filter(term != "(Intercept)")  

effects %>% 
  ggplot(aes(x = Experiment, y = estimate, ymax = estimate + 2 * std.error, ymin = estimate - 2 * std.error)) + 
  geom_point() +
  geom_errorbar() +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  facet_wrap(~ variable) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Estimate")

effects %>% arrange(variable) %>% select(variable, Experiment, estimate, std.error, p.value) %>% knitr::kable()

## ---- other 

## turnover on gradient
turnover <- data_frame(
  deltaT =  cover_fat %>% 
    filter(TTtreat %in% c("control", "local")) %>% 
    left_join(pred_temp, by = c("originSiteID" = "site", "TTtreat" = "TTtreat")) %>% 
    select(mean) %>% 
    dist() %>% as.vector(),
  
  deltaC = cover_fat_spp %>%
    filter(cover_fat$TTtreat %in% c("control", "local")) %>%
    vegdist() %>% as.vector()
)

ggplot(turnover, aes(x = deltaT, y = deltaC)) + 
  geom_jitter(height = 0, width = 0.5) + 
  geom_smooth(method = "lm", colour = 2) 
  