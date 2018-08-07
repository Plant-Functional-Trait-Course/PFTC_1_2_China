### Supporting Information 1: Species list

spList <- cover_thin %>% 
  left_join(fun_gp, by = "species") %>% 
  select(originSiteID, functionalGroup, speciesName) %>% 
  group_by(originSiteID) %>% 
  arrange(originSiteID, functionalGroup, speciesName) %>% 
  distinct(originSiteID, functionalGroup, speciesName) %>% 
  mutate(value = "x") %>% 
  spread(key = originSiteID, value = value) %>% 
  rename(FunctionalGroup = functionalGroup, Species = speciesName, HighAlpine = H, Alpine = A, Middle = M, Lowland = L) %>% 
  replace(., is.na(.), "-")


#write_csv(spList, path = "community/FinalFigures/spList.csv", col_names = TRUE)


### Supporting Information 2: 
# Regression output
tidy_model <- function(x){
  tidy(x)
}

tidy_reg <- 
  bind_rows(Transplant = transplant, OTC = otc, Gradient = gradient, .id = "experiment") %>% 
  ungroup() %>% 
  group_by(variable, experiment) %>%
  do(
    bind_rows(
      null = tidy_model(.$mod0[[1]]), 
      effect = tidy_model(.$mod1[[1]]), 
      interaction = tidy_model(.$mod2[[1]]), 
      .id = "model")
  ) %>% 
  filter(!(experiment == "Gradient" & model == "interaction")) %>% 
  ungroup() %>% 
  rename(response = "variable") %>% 
  filter(response %in% c("richness", "evenness", "propGraminoid")) %>% 
  select(-p.value) %>% 
  mutate(estimate = round(estimate, 2), std.error = round(std.error, 2), statistic = round(statistic, 2)) %>% 
  mutate(
    term = plyr::mapvalues(term, c("(Intercept)", "originSiteIDA", "originSiteIDM", "originSiteIDL", "contrast", "contrast:originSiteIDA", "contrast:originSiteIDM", "contrast:originSiteIDL"), c("Intercept", "Alpine", "Middle", "Lowland", "Contrast", "Contrast:Alpine", "Contrast:Middle", "Contrast:Lowland")),
    response = factor(response, levels = c("richness", "evenness", "propGraminoid"), labels = c("Richness", "Evenness", "Proportion Graminoid")),
    model = factor(model, levels = c("null", "effect", "interaction"), labels = c("No effect", "Effect", "Interaction"))) %>% 
  rename(Response = response, Experiment = experiment, Model = model, Term = term, Estimate = estimate, Std.error = std.error, Tstatistic = statistic)

#write_csv(tidy_reg, path = "community/FinalFigures/RegressionOutput.csv", col_names = TRUE)


# AIC output
aic_model_ouput <- function(x){
  glance(x)
}

aic_reg_output <- 
  bind_rows(Transplant = transplant, OTC = otc, Gradient = gradient, .id = "experiment") %>% 
  ungroup() %>% 
  group_by(variable, experiment) %>%
  do(
    bind_rows(
      null = aic_model_ouput(.$mod0[[1]]), 
      effect = aic_model_ouput(.$mod1[[1]]), 
      interaction = aic_model_ouput(.$mod2[[1]]), 
      .id = "model")
  ) %>% 
  select(-adj.r.squared, -sigma, -statistic, -p.value, -logLik, -BIC, -deviance) %>%
  filter(!(experiment == "Gradient" & model == "interaction")) %>% 
  mutate(r.squared = round(r.squared, 3), AIC = round(AIC, 1)) %>% 
  ungroup() %>% 
  rename(response = "variable") %>% 
  filter(response %in% c("richness", "evenness", "propGraminoid")) %>% 
  mutate(
    response = plyr::mapvalues(response, c("richness", "evenness", "propGraminoid"), c("Richness", "Evenness", "Proportion Graminoid")),
    response = factor(response, levels = c("Richness", "Evenness", "Proportion Graminoid")),
    model = factor(model, levels = c("null", "effect", "interaction"), labels = c("No effect", "Effect", "Interaction"))) %>% 
  rename(Response = response, Experiment = experiment, Model = model, Rsquared = r.squared)
#write_csv(aic_reg_output, path = "community/FinalFigures/AICOutput.csv", col_names = TRUE)



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
