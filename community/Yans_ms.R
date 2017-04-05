# Analyses for Yan et al
# Finse April 2017

#Effect of temperature treatments (gradient/transplant/OTC) on different responses (richness, evenness, grass/herb ratio. Change in effect over time

##load packages


##load data
#climate

#community
source("community/start_here.R")
cover_thin <- cover_thin %>% 
  filter(TTtreat %in% c("control", "local", "warm1", "OTC")) %>% 
  as_tibble()



#turf environment



## ordination
cover_thin %>% select(-speciesName) %>% spread(key = species, value = cover)




## responses
richnessC <- 
cover_thin %>% group_by(turfID, TTtreat, originSiteID, year) %>% summarise(n = n())


ggplot(filter(richnessC, TTtreat == "control"), aes(x = year, y = n, colour = originSiteID)) + 
  geom_jitter(height = 0, width = 0.2) + 
  geom_smooth(aes(group = originSiteID))


 