source("community/Yanetal_2018/2_CommunityChange.R")

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
