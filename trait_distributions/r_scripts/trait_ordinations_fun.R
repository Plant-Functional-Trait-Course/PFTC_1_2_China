

read_cwm <- function(f){
  x <- readRDS(f) %>% 
  as_tibble() %>% 
  mutate(site = factor(site, levels = c("H", "A", "M", "L"))) %>% 
  rename(TTtreat = treatment) %>% 
  mutate(TTtreat = factor(TTtreat, levels = c("C", "O", 1:4, "OTC"), labels = c("Control", "Local", "Warm1", "Cool1", "Warm3", "Cool3", "OTC")))
  return(x)
}


trait_histograms <- function(cwm){#histograms
  cwm %>% 
  filter(TTtreat %in% c("Control", "Local")) %>% 
  ggplot(aes(y = mean,x = trait, fill = site)) +
  geom_violin() +
  facet_wrap(~ trait, scales = "free_x")


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
}



## pair site ordinations
twoSites <- function(cwm, low, high, treat_colours){
  cwm_fat <- cwm %>% spread(key = trait, value = mean) 
  if(low == "L" & high == "H"){
    cwm_sites <- cwm_fat %>% 
      filter(site %in% c(low, high), !TTtreat %in% c("Warm1", "OTC", "Cool1"))
  } else {
    cwm_sites <- cwm_fat %>% 
      filter(site %in% c(low, high), !TTtreat %in% c("Warm3", "OTC", "Cool3"))
  }
  
  high_pca <-  cwm_sites %>% 
    select(-(turf:year)) %>% rda(scale = TRUE)
  
  high_sites <- bind_cols(
    cwm_sites %>% select(turf:year), 
    fortify(high_pca, display = "sites")
  )
  
  high_traits <- fortify(high_pca, display = "species")
  
  g <- ggplot(high_sites, aes(x = PC1, y = PC2, colour = TTtreat, shape = site, group = turf, linetype = site)) +  
    geom_point(aes(size = ifelse(year == min(year), "First", "Other"))) +
    geom_path() +
    #arrows
    geom_segment(data = high_traits, 
                 aes(x = 0, y = 0, xend = PC1, yend = PC2), 
                 arrow = arrow(length = unit(0.2, "cm")), 
                 colour = "darkblue",
                 inherit.aes = FALSE) +
    geom_text(data = high_traits, 
              aes(x = PC1 * 1.1,y = PC2 * 1.1, label = Label), 
              size = 3,
              inherit.aes = FALSE, colour = "darkblue") +
    coord_equal() +
    scale_size_discrete(range = c(1, 2.5), limits = c("Other", "First"), breaks = c("First", "Other")) +
    scale_colour_manual(values = treat_colours) +
    #  scale_colour_brewer(palette = "RdBu", direction = -1) +
    #  scale_shape_manual(values = c(16, 21)) +
    scale_x_continuous(expand = c(.15, 0)) +
    labs(x = "PC 1", y = "PC 2", shape = "Site", colour = "Treatment", size = "Year", linetype = "Site", title = paste(high, "-", low)) +
    theme_bw()
  return(g)
}



# ## control plots
# controls <- cwm_fat %>% filter(TTtreat %in% c("Control", "Local"))
# 
# controls_pca <- rda(controls %>% select(-(turf:year)), scale = TRUE)
# 
# control_sites <- bind_cols(
#   controls %>% select(turf:year), 
#   fortify(controls_pca, display = "sites"))
# 
# control_traits <- fortify(controls_pca, display = "species")
# 
# 
# 


# ggplot(control_sites, aes(x = PC1, y = PC2, shape = TTtreat, colour = site, group = turf)) +  
#   geom_point(aes(size = ifelse(year == min(year), "First", "Other"))) +
#   geom_path() +
#   #arrows
#   geom_segment(data = control_traits, 
#                aes(x = 0, y = 0, xend = PC1, yend = PC2), 
#                arrow = arrow(length = unit(0.2, "cm")), 
#                colour = "black",
#                inherit.aes = FALSE) +
#   geom_text(data = control_traits, 
#             aes(x = PC1 * 1.1,y = PC2 * 1.1, label = Label), 
#             size = 3,
#             inherit.aes = FALSE) +
#   coord_equal() +
#   scale_size_discrete(range = c(1, 2.5), limits = c("Other", "First"), breaks = c("First", "Other")) +
#   
#   scale_colour_brewer(palette = "RdBu", direction = -1) +
#   scale_shape_manual(values = c(16, 21)) +
#   scale_x_continuous(expand = c(.15, 0)) +
#   labs(x = "PC 1", y = "PC 2", shape = "Treatment", colour = "Site", size = "Year", title = "Controls")
