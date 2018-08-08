# load libraries
library("vegan")
library("ggvegan")
#example(prc)

# prepare data
cover_fat <- cover_fat %>% 
  mutate(newTT = plyr::mapvalues(TTtreat, c("warm1", "control", "local", "OTC"), c("warm1", "control", "control", "OTC")))



coverFat <- cover_fat %>% 
  filter(originSiteID == "H", TTtreat != "OTC") %>% 
  mutate(newTT = plyr::mapvalues(TTtreat, c("warm1", "control", "local", "OTC"), c("warm1", "control", "control", "OTC")))
communitydata <- coverFat %>% select(-(originSiteID:year), -newTT)
treatment <- factor(coverFat$newTT)
time <- factor(coverFat$year)

mod <- prc(response = sqrt(communitydata), treatment = treatment, time = time)
mod
summary(mod)$sp %>% sort
plot(mod)
anova(mod)

# run all models separate (site, treatment), then extract constrained proportion from mod, and look if anove(mod) is significant. Make a table.


rda(response ~ treatment * time + Condition(time + site))




cover_fat %>% 
  filter(TTtreat != "warm1") %>% 
  group_by(originSiteID) %>% 
  do(
    # run prc
    mod = prc(response = . %>% select(-(originSiteID:year), -newTT), treatment = factor(.$newTT), time = factor(.$year)),
    
    # explained variance by treatment
    VarianceTreat <- mod$CCA$tot.chi/mod$tot.chi
    )
  
