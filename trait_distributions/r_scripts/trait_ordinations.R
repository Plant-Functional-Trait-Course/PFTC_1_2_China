library("tidyverse")
library("vegan")
library("ggvegan")

cwm <- readRDS("trait_distributions/data/China_pftc_cwm.rds") %>% 
  as_tibble() %>% 
  mutate(site = factor(site, levels = c("H", "A", "M", "L"))) %>% 
  rename(TTtreat = treatment) %>% 
  mutate(TTtreat = factor(TTtreat, levels = c("C", "O", 1:4, "OTC"), labels = c("Control", "Local", "Warm1", "Cool1", "Warm3", "Cool3", "OTC")))

#histograms
cwm %>% 
  filter(TTtreat %in% c("Control", "Local")) %>% 
  ggplot(aes(x = mean, fill = site)) +
  geom_histogram() +
  facet_wrap(~ trait, scales = "free_x")

cwm %>% 
  filter(TTtreat %in% c("OTC")) %>% 
  ggplot(aes(x = mean, fill = site)) +
  geom_histogram() +
  facet_wrap(~ trait, scales = "free_x")


cwm %>% 
  filter(TTtreat %in% c("Warm1")) %>% 
  ggplot(aes(x = mean, fill = site)) +
  geom_histogram() +
  facet_wrap(~ trait, scales = "free_x")

cwm %>% 
  filter(TTtreat %in% c("Cool1")) %>% 
  ggplot(aes(x = mean, fill = site)) +
  geom_histogram() +
  facet_wrap(~ trait, scales = "free_x")

### ordinations
cwm_fat <- cwm %>% spread(key = trait, value = mean) 

## control plots
controls <- cwm_fat %>% filter(TTtreat %in% c("Control", "Local"))

controls_pca <- rda(controls %>% select(-(turf:year)), scale = TRUE)

control_sites <- bind_cols(
  controls %>% select(turf:year), 
  fortify(controls_pca, display = "sites"))

control_traits <- fortify(controls_pca, display = "species")

treat_colours <- c("black", "grey50", "pink", "lightgreen", "red", "green", "orange")


ggplot(control_sites, aes(x = PC1, y = PC2, shape = TTtreat, colour = site, group = turf)) +  
  geom_point(aes(size = ifelse(year == min(year), "First", "Other"))) +
  geom_path() +
  #arrows
  geom_segment(data = control_traits, 
               aes(x = 0, y = 0, xend = PC1, yend = PC2), 
               arrow = arrow(length = unit(0.2, "cm")), 
               colour = "black",
               inherit.aes = FALSE) +
  geom_text(data = control_traits, 
            aes(x = PC1 * 1.1,y = PC2 * 1.1, label = Label), 
            size = 3,
            inherit.aes = FALSE) +
  coord_equal() +
  scale_size_discrete(range = c(1, 2.5), limits = c("Other", "First"), breaks = c("First", "Other")) +
  
  scale_colour_brewer(palette = "RdBu", direction = -1) +
  scale_shape_manual(values = c(16, 21)) +
  scale_x_continuous(expand = c(.15, 0)) +
  labs(x = "PC 1", y = "PC 2", shape = "Treatment", colour = "Site", size = "Year", title = "Controls")

ggsave(filename = "cwm_trait_control_pca.png", width = 6, height = 5)




