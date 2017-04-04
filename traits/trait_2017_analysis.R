#import packages
library("tidyverse")

#import data

trait2015 <- read.table("traits/data/2015_ChinaLeafTraitData_corrCP_16032017.csv", sep = ",", header = TRUE, comment = "", stringsAsFactors = FALSE) %>% as_tibble()

#fix character variables
trait2015 %>% filter(is.na(as.numeric(Dry_Mass_2016_g))) %>% distinct(Dry_Mass_2016_g) 
trait2015 %>% filter(is.na(as.numeric(Leaf_Area_m2))) %>% distinct(Leaf_Area_m2) 

trait2015 <- trait2015 %>% 
  mutate(Dry_Mass_2016_g = as.numeric(Dry_Mass_2016_g)) %>%
  mutate(Leaf_Area_m2 = as.numeric(Leaf_Area_m2)) %>%
  mutate(Leaf_Thickness_Ave_mm = rowMeans(select(., matches("Leaf_Thickness_\\d_mm")), na.rm = TRUE))


##some plots
#wet vs dry
ggplot(trait2015, aes(x = Wet_Mass_g, y = Dry_Mass_2016_g, colour = Site)) + 
  geom_point() +   
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  scale_x_log10() + 
  scale_y_log10()

# dry vs area  
ggplot(trait2015, aes(x = Dry_Mass_2016_g, y = Leaf_Area_cm2, colour = Site)) + 
  geom_point() +   
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  scale_x_log10() + 
  scale_y_log10()


ddd <- trait2015 %>% 
  filter(!is.na(Dry_Mass_2016_g)) %>% 
  filter(!is.na(Leaf_Area_cm2))

fit <- lm(log(Leaf_Area_cm2) ~ log(Dry_Mass_2016_g), ddd)
ddd$res <- resid(fit)

ddd %>% 
  #filter(abs(res) > 4) %>% 
  #select(Envelope_Name_Corrected) %>% print(n = 41)
  mutate(resHL = ifelse(abs(res) > 4, "High", "Low")) %>% 
  ggplot(aes(x = Dry_Mass_2016_g, y = Leaf_Area_cm2, color = resHL)) + 
  geom_point() +   
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  scale_x_log10() + 
  scale_y_log10()


#thickness
ggplot(trait2015, aes(y = Leaf_Thickness_Ave_mm, x = Site)) + 
  geom_boxplot()



