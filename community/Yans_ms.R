# Analyses for Yan et al
# Finse April 2017

#Effect of temperature treatments (gradient/transplant/OTC) on different responses (richness, evenness, grass/herb ratio. Change in effect over time

##load packages
library("tidyverse")
library("vegan")
library("ggvegan")
library("lubridate")
library("gridExtra")


## ---- setup
source("yans_ms_functions.R")

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
source("start_here.R")
cover_thin <- cover_thin %>% 
  filter(TTtreat %in% c("control", "local", "warm1", "OTC"))



#turf environment

## functional groups
fun_gp <- tbl(con, "taxon") %>% 
  select(species, functionalGroup) %>% 
  collect()

fun_gp %>% count(functionalGroup)

## ---- ordination
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

g <- ggplot(fNMDS, aes(x = Dim1, y = Dim2, shape = originSiteID, colour = TTtreat, group = originPlotID, fill = TTtreat)) +
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
  group_by(turfID, originBlockID, TTtreat, originSiteID, year) %>%  
  summarise(richness = n(), 
            diversity = diversity(cover), 
            N1 = exp(diversity),
            evenness = diversity/log(richness),
            sumCover = sum(cover),
            propGraminoid = sum(cover[functionalGroup %in% c("gramineae", "sedge")])/sumCover
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

# table of effects

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
  