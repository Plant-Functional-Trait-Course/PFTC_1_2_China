# Analyses for Yan et al
# Finse April 2017

#Effect of temperature treatments (gradient/transplant/OTC) on different responses (richness, evenness, grass/herb ratio. Change in effect over time

##load packages
library("vegan")
library("ggvegan")
library("lubridate")

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
  summer_temp %>% filter(logger == "gradient") %>% mutate(TTtreat = "control"),#control
  summer_temp %>% filter(logger == "gradient") %>% mutate(TTtreat = "local"),#local transplant (== control)
  summer_temp %>% spread(key = logger, value = mean) %>% mutate(mean = otc - gradient, TTtreat = "OTC") %>% select(-gradient, -otc),
  summer_temp %>% filter(logger == "gradient") %>% mutate(mean = lead(mean) - mean, TTtreat = "warm1") %>% filter(site != "L")
)  %>% select(-logger) %>%
  mutate(mean = if_else(site == "L" & TTtreat == "OTC", 2, mean))
warning("using estiated OTC effect for site L")


#community
source("community/start_here.R")
#source("community/ordinations.R")
cover_thin <- cover_thin %>% 
  filter(TTtreat %in% c("control", "local", "warm1", "OTC"))



#turf environment



## ordination
cover_fat <- cover_thin %>% 
  select(-speciesName) %>% 
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

g <-ggplot(fNMDS, aes(x = Dim1, y = Dim2, shape = originSiteID, colour = TTtreat, group = originPlotID, fill = TTtreat)) +
  geom_point(aes(size = ifelse(year == min(year), "First", "Other"))) +
  geom_path() + 
  coord_fixed(ratio = 1) +
  scale_size_discrete(range = c(1, 3), limits = c("Other", "First"), breaks = c("First", "Other")) +
  scale_colour_manual(values = treat_colours) +
  scale_fill_manual(values = treat_colours) +
  scale_shape_manual(values = c(24, 22, 23, 25)) +
  guides(shape = guide_legend(override.aes = list(fill = "black"))) +
  labs(x = "NMDS 1", y = "NMDS 2", colour = "Treatment", fill = "Treatment", shape = "Site", size = "Year")
g



## responses
#richness
richnessC <- cover_thin %>% 
  group_by(turfID, originBlockID, TTtreat, originSiteID, year) %>%   summarise(n = n()) %>% 
  group_by(originBlockID, TTtreat, originSiteID, year) %>%
  select(-turfID) %>% 
  full_join(pred_temp, by = c("originSiteID" = "site", "TTtreat" = "TTtreat")) %>%
  mutate(contrast = if_else(TTtreat %in% c("local", "control"), 0, mean))

gc <- ggplot(richnessC %>% filter(TTtreat %in% c("local", "control"), year == 2016), aes(x = mean, y = n, colour = originSiteID, shape = TTtreat, group = 1)) + 
  geom_jitter(height = 0, width = 0.1) +
  geom_smooth(method = "lm") +
  ggtitle("Gradient")

gw <- gc %+% (richnessC %>% filter(TTtreat %in% c("local", "warm1"), year == 2016)) + aes(x = contrast, group = originSiteID) +
  ggtitle("Transplant")

go <- gw %+% (richnessC %>% filter(TTtreat %in% c("control", "OTC"), year == 2016)) + 
  ggtitle("OTC")
                                  
gridExtra::grid.arrange(gc, gw, go)
# remove duplicate legends
# shink Transplant and OTC plots so temp scale is ~ equal
# more responses
# table of effects
