#import packages
library("tidyverse")
library("readr")

#import data

trait2015 <- read_delim(file = "traits/data/2015_ChinaLeafTraitData_corrCP_16032017.csv", delim = ",", comment = "")

#fix character variables
trait2015 %>% filter(is.na(as.numeric(Dry_Mass_2016_g))) %>% distinct(Dry_Mass_2016_g) 
trait2015 %>% filter(is.na(as.numeric(Leaf_Area_m2))) %>% distinct(Leaf_Area_m2) 

trait2015 <- trait2015 %>% 
  mutate(Dry_Mass_2016_g = as.numeric(Dry_Mass_2016_g)) %>%
  mutate(Leaf_Area_m2 = as.numeric(Leaf_Area_m2)) %>%
  mutate(Leaf_Thickness_Ave_mm = rowMeans(select(., matches("Leaf_Thickness_\\d_mm")), na.rm = TRUE))



#import data
# leaf area
trait2016LeafArea <- read_delim("traits/data/2016_PFTC2_Leaf_Area_corrCP_30032017.csv", delim = ",", comment = "")

trait2016LeafArea <- trait2016LeafArea %>% 
  mutate(Date = ymd(Date))


# leaf traits
trait2016LeafTrait <- read_delim("traits/data/2016_China_envelope_names_CPcorr_30032017.csv", delim = ";", comment = "")

trait2016 <- trait2016LeafTrait %>% 
  mutate(Date = ymd(Date)) %>% 
  # NA's are created, because there is text in these columns
  rename(Elevation = Elevation_m, Individual_number = Individual_plant_number, Taxon = Plant_species) %>% 
  inner_join(trait2016LeafArea, by = c("Envelope_Name_Corrected", "Date", "Elevation", "Site", "Location", "Project", "Taxon", "Individual_number", "Leaf_number")) %>%  # retains rows in both data sets. Needs to be changes once all the names are correct!!!!
  mutate(Leaf_Thickness_Ave_mm = rowMeans(select(., matches("Leaf_Thickness_\\d_mm")), na.rm = TRUE))

##combine 2015 & 2016 trait data
#remove unneeded columns, rename columns
setdiff(names(trait2015), names(trait2016))
setdiff(names(trait2016), names(trait2015))

trait2015 <- trait2015 %>%
  mutate(Date = ymd(Date)) %>%
  select(-Leaf_Area_m2, -Wet_Mass_WeighingScale, -Dry_Mass_WeighingScale, -Dry_Mass_g, -`LMA g -m2`, -`log LMA`, -`wet-dry`, -notes, -corrections) %>%
  rename(Taxon = Taxon_FoC_corrected, Leaf_number = Leaf_Number, Individual_number = Individual_Number, Dry_Mass_g = Dry_Mass_2016_g, SLA_cm2_g = `SLA_cm2-g`) %>%
  mutate(Individual_number = as.character(Individual_number))
  



trait2016 <- trait2016 %>%
  select(-`Difference_(Uncropped_minus_Cropped)`, -Uncropped_Leaf_Area, -X20,  -Notes, -Dry_Mass_g_Multiple2, -Dry_Mass_g_Multiple3, -Corrections.x, -`dry:wet`, -Corrections.y) %>%
  rename(Leaf_Area_cm2 = Cropped_Leaf_Area) %>%
  mutate(Leaf_number = as.character(Leaf_number))

traits <- bind_rows(trait2016, trait2015) %>%
  mutate(SLA_cm2_g = Leaf_Area_cm2 / Dry_Mass_g,
         LDMC = Dry_Mass_g / Wet_Mass_g)

##some plots
#wet vs dry
traits %>% mutate(year = as.factor(year(Date))) %>%
ggplot(aes(x = Wet_Mass_g, y = Dry_Mass_g, colour = Site)) + 
  geom_point() +   
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  scale_x_log10() + 
  scale_y_log10() + 
  facet_wrap(~ year)

# dry vs area  
traits %>% mutate(year = as.factor(year(Date))) %>%
  ggplot(aes(x = Dry_Mass_g, y = Leaf_Area_cm2, colour = Site)) + 
  geom_point() +   
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  scale_x_log10() + 
  scale_y_log10() + 
  facet_wrap(~ year)


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



