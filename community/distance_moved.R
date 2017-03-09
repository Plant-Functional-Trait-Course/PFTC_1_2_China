#load libraries
library("vegan")
library("tidyverse")

source("community/start_here.R")

## find taxonomic distance moved
dist_moved <- cover_thin %>% 
  group_by(turfID, TTtreat, originSiteID) %>%
  select(-speciesName) %>%
  do({
    tdata <- spread(., key = species, value = cover, fill = 0) %>%
      arrange(year)
    vdata <-  select(tdata, -(originSiteID:year))
    d <- as.matrix(vegdist(vdata))
    data.frame(year = tdata$year, d = d[, 1])
  }) %>%
  filter(year > min(year))


dist_moved %>% ggplot(aes(x = as.factor(year), y = d, fill = originSiteID)) +
  geom_boxplot() +
  facet_wrap(~TTtreat)

## find directional distance moved towards target

#find distance to control in first year - d1
#find distance to control in subsequent years - d2
#difference delta = d1 - d2
#relative delta/d1 

dist_to_control <- cover_thin %>%
  group_by(turfID, destBlockID, destSiteID, year, TTtreat) %>%
  select(-speciesName) %>%
  spread(key = species, value = cover, fill = 0) %>%
  group_by(destBlockID, destSiteID, year) %>%
  arrange(TTtreat) %>%
  do({
    vdata <- as.data.frame(.) %>% 
      column_to_rownames(var = "TTtreat") %>%
      select(-(originSiteID:year))
    d <- vegdist(vdata) %>% 
      as.matrix() %>% 
      as.data.frame() 
    if(any(rownames(d) == "control")){
      d <- d %>% 
        mutate(TTtreat = rownames(d)) %>%
        filter(TTtreat != "control") %>%
        select(TTtreat, control) %>%
        rename(dist_to_control = control)
    } else {#no control - drop block * year
      d <- data.frame()
    }
    d
  }) %>%
  group_by(destBlockID, destSiteID, TTtreat)

dist_to_control2 <- left_join(
  dist_to_control %>% filter(year != min(year)), 
  dist_to_control %>% filter(year == min(year)), 
  by = c("destBlockID" = "destBlockID", "destSiteID" = "destSiteID", "TTtreat" = "TTtreat"), 
  suffix = c("_", "_original")
) %>% 
  select(-year_original) %>%
  rename(year = year_, dist_to_control = dist_to_control_) %>%
  mutate(delta_dist = dist_to_control_original - dist_to_control, 
         relative_dist = delta_dist / dist_to_control_original) %>%
  ungroup() %>%
  mutate(TTtreat = factor(TTtreat, levels = levels(cover_thin$TTtreat)))
  

## plot
ggplot(dist_to_control2, aes(x = as.factor(year), y = relative_dist)) + 
  geom_boxplot() + 
  facet_wrap(~TTtreat)

ggplot(dist_to_control2, aes(x = as.factor(year), y = delta_dist, fill = destSiteID)) + 
  geom_boxplot() + 
  facet_wrap(~TTtreat)

ggplot(filter(dist_to_control2, year == 2013), aes(x = TTtreat, y = dist_to_control_original, fill = destSiteID)) + 
  geom_boxplot() +
  labs(x = "Treatment", ylab = "Original distance to destination control")
