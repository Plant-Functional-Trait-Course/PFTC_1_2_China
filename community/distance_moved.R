#load libraries
library("vegan")

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

## find directional distance moved

## plot