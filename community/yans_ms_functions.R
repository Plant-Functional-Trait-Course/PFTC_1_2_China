plot_three_treatments <- function(data, column, ylab = "")  {
  data <- data %>% filter(year == 2016, TTtreat %in% c("local", "control", "OTC", "warm1"))
  ymin <- min(data[, column])
  ymax <- max(data[, column])
  
    gc <- ggplot(data %>% filter(TTtreat %in% c("local", "control")), 
                 aes_string(x = "mean", y = column, colour = "originSiteID", shape = "TTtreat", group = 1)) + 
    geom_jitter(height = 0, width = 0.1) +
    geom_smooth(method = "lm") +
    scale_shape(limits = levels(data$TTtreat)) +
    labs(x = "Temperature °C", y = ylab, colour = "Site", shape = "Treatment") +
    ylim(ymin, ymax) +  
    ggtitle("Gradient")
  
  gw <- gc %+% (data %>% filter(TTtreat %in% c("local", "warm1"))) + 
    aes(x = contrast, group = originSiteID) +
    ggtitle("Transplant") + 
    scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
    theme(legend.position = "none") + 
    labs(x = "Temperature Contrast °C")
  
  
  go <- gw %+% (data %>% filter(TTtreat %in% c("control", "OTC"))) + 
    ggtitle("OTC")
  
  g <- ggplotGrob(gc)$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  grid.arrange(gc + theme(legend.position = "none"), gw, go,legend, layout_matrix = rbind(c(1, 1, 4), c(2, 3, 4)), widths = c(.4, .4, .2))
}

#nmds of two sites
two_sites_nmds <- function(site1, site2){
  cover_fat <- cover_fat %>% 
    filter(originSiteID == site1 | (originSiteID == site2 & TTtreat  %in% c("control", "local")))
  
  cover_fat_spp <- cover_fat %>% 
    select(-(originSiteID:year))
  
  NMDS <- metaMDS(cover_fat_spp, noshare = TRUE, try = 30)#DNC
  
  fNMDS <- fortify(NMDS) %>% 
    filter(Score == "sites") %>%
    bind_cols(cover_fat %>% select(originSiteID:year))
  
  fNMDS
}




