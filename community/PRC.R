# load libraries
library("vegan")
library("ggvegan")
#example(prc)

source("community/start_here.R")

## prep data for ordination
cover_fat <- cover_thin %>% 
  select(-speciesName, -flag) %>% 
  arrange(year) %>%
  filter(TTtreat %in% c("local", "control", "warm1", "OTC")) %>%
  spread(key = species, value = cover, fill = 0) %>%
  mutate(TTtreat = droplevels(TTtreat)) %>% 
  mutate(newTT = plyr::mapvalues(TTtreat, c("warm1", "control", "local", "OTC"), c("warm1", "control", "control", "OTC")))


# Community change AWAY from origin control  (OTC and warm1)
# Transplant
cover_fat %>% 
  filter(newTT != "warm1") %>% 
  # make year and treatment a factor, sort levels, so that control comes first
  mutate(year = factor(year), newTT = factor(newTT, levels = c("control", "OTC", "warm1"))) %>% 
  group_by(originSiteID) %>% 
  do({
    # run prc
    mod = prc(response = (.) %>% select(-(originSiteID:TTtreat), -year, -newTT), treatment = .$newTT, time = .$year)
    
    # explained variance by treatment and p value
    data_frame(VarianceTreat = mod$CCA$tot.chi/mod$tot.chi,
               Pvalue = anova(mod)$'Pr(>F)'[1])
  })

# OTC
cover_fat %>% 
  filter(TTtreat %in% c("OTC", "control")) %>% 
  mutate(TTtreat = factor(TTtreat), year = factor(year)) %>% 
  group_by(originSiteID) %>% 
  do({
    # run prc
    mod = prc(response = (.) %>% select(-(originSiteID:TTtreat), -year, -newTT), treatment = .$TTtreat, time = .$year)
    
    # explained variance by treatment and p value
    data_frame(VarianceTreat = mod$CCA$tot.chi/mod$tot.chi,
               Pvalue = anova(mod)$'Pr(>F)'[1])
  })



# Full model
# Transplant
dd <- cover_fat %>% 
  filter(newTT != "OTC") %>%
  # make year and treatment a factor, sort levels, so that control comes first
  mutate(year = factor(year), newTT = factor(newTT, levels = c("control", "warm1")), originSiteID = factor(originSiteID))
communitydata <- dd %>% select(-(originSiteID:year), -newTT)

rda(communitydata ~ newTT * year + Condition(year + originSiteID), data = dd)



# Community change TOWARDS destination control

coverFat <- cover_fat %>% 
  mutate(year = factor(year), newTT = factor(newTT, levels = c("control", "OTC", "warm1"))) %>% 
#Towards
filter(destSiteID == "A", newTT != "OTC") %>% 
  droplevels()
#filter(TTtreat == "OTC" & originSiteID == "M" | TTtreat == "control" & originSiteID == "L")

communitydata <- coverFat %>% select(-(originSiteID:year), -newTT)

fit <- rda(communitydata ~ newTT:year + Condition(newTT + year), data = coverFat)
fit
anova(fit)




# test stuff
coverFat <- cover_fat %>% 
  mutate(newTT = plyr::mapvalues(TTtreat, c("warm1", "control", "local", "OTC"), c("warm1", "control", "control", "OTC"))) %>% 
  #Away
  #filter(newTT != "warm1") %>% 
  #Towards
  #filter(newTT != "OTC") %>% 
  #filter(newTT == "OTC" & originSiteID == "M" | newTT == "control" & originSiteID == "L") %>% 
  filter(TTtreat %in% c("control", "OTC")) %>% 
  filter(!(originSiteID == "H" & TTtreat == "control")) %>% 
  filter(!(originSiteID == "L" & TTtreat == "OTC")) %>% 
  mutate(destSiteID2 = case_when(TTtreat == "OTC" & destSiteID == "H" ~ "A",
                                 TTtreat == "OTC" & destSiteID == "A" ~ "M",
                                 TTtreat == "OTC" & destSiteID == "M" ~ "L",
                                 TRUE ~ as.character(destSiteID))) %>% 
  droplevels()

communitydata <- coverFat %>% select(-(originSiteID:year), -newTT, -destSiteID2)
treatment <- factor(coverFat$newTT)
time <- factor(coverFat$year)
site <- factor(coverFat$destSiteID2)

mod <- prc(response = (communitydata), treatment = treatment, time = time)
mod
plot(mod)
anova(mod)
tidy(anova(mod))$p.value
summary(mod)$sp %>% sort
mod$CCA$tot.chi/mod$tot.chi


# run all models separate (site, treatment), then extract constrained proportion from mod, and look if anove(mod) is significant. Make a table.

# Full model
#rda(response ~ treatment * time + Condition(time + site))


mod0 <- rda(communitydata ~ site)
mod1 <- rda(communitydata ~ time + Condition(site))
mod2 <- rda(communitydata ~ treatment + Condition(site + time))
mod3 <- rda(communitydata ~ treatment * time + Condition(time + site))





  

