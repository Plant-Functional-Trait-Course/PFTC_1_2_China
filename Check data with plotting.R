#### Check data with some plots ####


# Dry_Mass_2015 vs Dry_Mass_2016
trait2015_all %>% filter(year(ymd(Date)) == "2015") %>%
  ggplot(aes(x = Dry_Mass_g, y = Dry_Mass_2016_g)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  scale_x_log10() + 
  scale_y_log10()
  

## WET VS. DRY MASS
# colour Tiny wet mass: Wet_Mass_g < 0.0005
traits %>% mutate(year = as.factor(year(Date))) %>%
  ggplot(aes(x = Wet_Mass_g, y = Dry_Mass_g, colour = Wet_Mass_g < 0.0005)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  scale_x_log10() + 
  scale_y_log10() + 
  facet_wrap(~ year)



## DRY VS. AREA  
traits %>% mutate(year = as.factor(year(Date))) %>%
  ggplot(aes(x = Dry_Mass_g, y = Leaf_Area_cm2, color = AreaFlag)) + 
  geom_point() +   
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  scale_x_log10() + 
  scale_y_log10() + 
  facet_wrap(~ year)


## SLA
traits %>% 
  mutate(year = year(Date)) %>% 
  arrange(SLA_cm2_g) %>%
  mutate(flag = cut(SLA_cm2_g, breaks = c(0, 5, 500, Inf))) %>% 
  ggplot(aes(x = Leaf_Area_cm2, y = Dry_Mass_g, colour = AreaFlag)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  scale_x_log10() + 
  scale_y_log10() + 
  facet_wrap(~ year)


traits %>% 
  mutate(year = year(Date)) %>% 
  arrange(SLA_cm2_g) %>%
  mutate(flag = cut(SLA_cm2_g, breaks = c(0, 5, 500, Inf))) %>% 
  ggplot(aes(x = Leaf_Area_cm2, y = Dry_Mass_g, colour = SLA_cm2_g > 500)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  scale_x_log10() + 
  scale_y_log10() + 
  facet_wrap(~ year)


#thickness
ggplot(traits, aes(y = Leaf_Thickness_Ave_mm, x = Site, fill = as.factor(year(Date)))) + 
  geom_boxplot()

traits %>% group_by(Taxon) %>% filter(n() > 100) %>%
  ggplot(aes(y = Leaf_Thickness_Ave_mm, x = Site, fill = as.factor(year(Date)))) + 
  geom_boxplot(show.legend = FALSE) +
  facet_wrap(~ Taxon)

traits %>% group_by(Taxon) %>% filter(n() > 100) %>%
  select(Date, Site, Taxon, matches("Leaf_Thickness_\\d_mm")) %>%
  gather(key = measurement, value = thickness, -Date, -Site, -Taxon) %>% 
  ggplot(aes(x = measurement, y = thickness, colour = as.factor(year(Date)))) + 
  geom_boxplot(show.legend = FALSE) + 
  facet_grid(Taxon ~ Site, scales = "free")



# large resids
LargeResid %>% mutate(year = as.factor(year(Date))) %>%
  ggplot(aes(x = Dry_Mass_g, y = Leaf_Area_cm2, color = abs(resid_Area_Dry) > 1.2)) + 
  geom_point() +   
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  scale_x_log10() + 
  scale_y_log10() + 
  facet_wrap(~ year)


