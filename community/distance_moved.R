#load packages
library("vegan")
library("tidyverse")

source("community/start_here.R")

#get functional groups

noGraminoids <- taxa %>% 
  filter(!functionalGroup %in% c("gramineae", "sedge")) %>% 
  select(species)


## find taxonomic distance moved
dist_moved <- cover_thin  %>% 
  left_join(noGraminoids) %>%
  group_by(turfID, TTtreat, originSiteID) %>%
  select(-speciesName, -flag) %>%
  do({
    tdata <- spread(., key = species, value = cover, fill = 0) %>%
      arrange(year)
    vdata <-  select(tdata, -(originSiteID:year))
    d <- as.matrix(vegdist(vdata))
    data_frame(year = tdata$year, d = d[, 1])
  }) %>%
  filter(year > min(year))


dist_moved %>% 
  filter(!TTtreat %in% c("cool1", "cool3", "warm3")) %>% 
  ggplot(aes(x = originSiteID, y = d, fill = as.factor(year))) +
  geom_boxplot() +
  facet_wrap(~TTtreat) +
  labs(fill = "Year", y = "Bray-Curtis distance moved", x = "Original site")
  
dist_moved %>% 
  filter(!TTtreat %in% c("cool1", "cool3", "warm3"), year == 2016) %>% 
  ggplot(aes(x = originSiteID, fill = TTtreat, y = d)) +
  geom_boxplot() +
  labs(fill = "Year", y = "Bray-Curtis distance moved", x = "Original site")


dist_moved %>% 
  filter(!TTtreat %in% c("cool1", "cool3", "warm3")) %>% 
  ggplot(aes(x = year, y = d, colour = originSiteID, group = turfID)) +
  geom_line() +
  facet_wrap( ~ TTtreat) +
  labs(x = "Year", y = "Bray-Curtis distance moved", colour = "Original site")


## find directional distance moved towards target

#find distance to control in first year - d1
#find distance to control in subsequent years - d2
#difference delta = d1 - d2
#relative delta/d1 

dist_to_control <- cover_thin %>%
  left_join(noGraminoids) %>%
  group_by(turfID, destBlockID, destSiteID, year, TTtreat) %>%
  select(-speciesName, -flag) %>%
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
  ungroup()

dist_to_control2 <- left_join(
  dist_to_control %>% filter(year != min(year)), 
  dist_to_control %>% filter(year == min(year)) %>% select(-year), 
  by = c("destBlockID" = "destBlockID", "destSiteID" = "destSiteID", "TTtreat" = "TTtreat"), 
  suffix = c("", "_original")
) %>% 
  mutate(delta_dist = dist_to_control_original - dist_to_control, 
         relative_dist = delta_dist / dist_to_control_original) %>%
  mutate(TTtreat = factor(TTtreat, levels = levels(cover_thin$TTtreat))) #%>% 
#  left_join(cover_meta %>% select(-year) %>% distinct())
  


## plot
dist_to_control2 %>% 
  filter(!TTtreat %in% c("cool1", "cool3", "warm3")) %>% 
  ggplot(aes(x = as.factor(year), y = relative_dist, fill = destSiteID)) + 
  geom_boxplot() + 
  facet_wrap(~TTtreat) +
  labs(x = "Year", y  = "Relative Bray-Curtis distance")

dist_to_control2 %>% 
  filter(!TTtreat %in% c("cool1", "cool3", "warm3"), 
        year == 2016) %>% 
  ggplot(aes(x = as.factor(year), y = delta_dist, fill = destSiteID)) + 
  geom_boxplot() + 
  facet_wrap(~TTtreat) +
  labs(x = "Year", y = expression(Delta~distance), fill = "Destination Site")


dist_to_control2 %>% 
  filter(
    year == 2013, 
    !TTtreat %in% c("cool1", "cool3", "warm3")) %>% 
ggplot(aes(x = TTtreat, y = dist_to_control_original, fill = destSiteID)) + 
  geom_boxplot() +
  labs(x = "Treatment", ylab = "Original distance to destination control")
