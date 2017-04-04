library(tidyverse)
library(lubridate)

#import data
# leaf area
trait2016LeafArea <- read.table("traits/data/2016_PFTC2_Leaf_Area_corrCP_30032017.csv", sep = ",", header = TRUE, comment = "", stringsAsFactors = FALSE) %>% as_tibble()

trait2016LeafArea <- trait2016LeafArea %>% 
  mutate(Date = ymd(Date))


# leaf traits
trait2016LeafTrait <- read.table("traits/data/2016_China_envelope_names_CPcorr_30032017.csv", sep = ";", header = TRUE, comment = "", stringsAsFactors = FALSE) %>% as_tibble()

head(trait2016LeafTrait)
str(trait2016LeafTrait)


trait2016 <- trait2016LeafTrait %>% 
  mutate(Date = ymd(Date)) %>% 
  # NA's are created, because there is text in these columns
  rename(Elevation = Elevation_m, Individual_number = Individual_plant_number, Taxon = Plant_species) %>% 
 inner_join(trait2016LeafArea, by = c("Envelope_Name_Corrected", "Date", "Elevation", "Site", "Location", "Project", "Taxon", "Individual_number", "Leaf_number")) %>%  # retains rows in both data sets. Needs to be changes once all the names are correct!!!!
  mutate(Leaf_Thickness_Ave_mm = rowMeans(select(., matches("Leaf_Thickness_\\d_mm")), na.rm = TRUE))
  

# Antijoin - check how many Envelopes have no scan and vice versa
trait2016LeafTrait2 <- trait2016LeafTrait %>% 
  mutate(Date = ymd(Date)) %>% 
  # NA's are created, because there is text in these columns
  rename(Elevation = Elevation_m, Individual_number = Individual_plant_number, Taxon = Plant_species)

# Rows from LeafTrait with no matching LeafArea: 57
anti_join(trait2016LeafTrait2, trait2016LeafArea, by = c("Envelope_Name_Corrected")) %>% select(Envelope_Name_Corrected) %>% print(n = 57)

# Rows from LeafArea with no match LeafTrait: 9
anti_join(trait2016LeafArea, trait2016LeafTrait2, by = c("Envelope_Name_Corrected")) %>% select(Envelope_Name_Corrected) %>% print(n = 9)



dim(trait2016)


# some plots
# wet vs dry mass
ggplot(trait2016, aes(x = Wet_Mass_g, y = Dry_Mass_g)) + 
  geom_point() +   
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  scale_x_log10() + 
  scale_y_log10()

# dry vs area  
trait2016 %>%
  ggplot(aes(x = Dry_Mass_g, y = Cropped_Leaf_Area)) + 
  geom_point() +   
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  scale_x_log10() + 
  scale_y_log10()

trait2016 %>%
  ggplot(aes(x = Leaf_Thickness_1_mm, y = Leaf_Thickness_2_mm)) + 
  geom_point() +   
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  scale_x_log10() + 
  scale_y_log10()



# DataCheck
# outlier dry vs. leaf area  
ddd <- trait2016 %>% 
  filter(!is.na(Dry_Mass_g)) %>% 
  filter(!is.na(Cropped_Leaf_Area))

fit <- lm(log(Cropped_Leaf_Area) ~ log(Dry_Mass_g), ddd)
ddd$res <- resid(fit)

ddd %>% 
  filter(abs(res) > 4) %>% 
  select(Envelope_Name_Corrected) %>% print(n = 41)
  mutate(resHL = ifelse(abs(res) > 4, "High", "Low")) %>% 
  ggplot(aes(x = Dry_Mass_g, y = Cropped_Leaf_Area, color = resHL)) + 
  geom_point() +   
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  scale_x_log10() + 
  scale_y_log10()


  
# Wet vs dry
ddd <- trait2016 %>% 
    filter(!is.na(Wet_Mass_g)) %>% 
    filter(!is.na(Dry_Mass_g))
  
fit <- lm(log(Dry_Mass_g) ~ log(Wet_Mass_g), ddd)
ddd$res <- resid(fit)
  
ddd %>% 
  filter(abs(res) > 1.2) %>% 
  select(Envelope_Name_Corrected, Wet_Mass_g, Dry_Mass_g) %>%
  mutate(Ratio = Dry_Mass_g/Wet_Mass_g*100) %>% 
  filter(Ratio < 6) %>% 
  arrange(-Wet_Mass_g) %>% print(n = 20)

ddd %>% 
    #mutate(resHL = ifelse(abs(res) > 1.2, "High", "Low")) %>%
    mutate(Ratio = Dry_Mass_g/Wet_Mass_g*100) %>% 
    mutate(resHL = ifelse(Ratio < 6, "High", "Low")) %>% 
    ggplot(aes(x = Wet_Mass_g, y = Dry_Mass_g, color = resHL)) + 
    geom_point() +   
    geom_abline(intercept = 0, slope = 1, colour = "red") +
    scale_x_log10() + 
    scale_y_log10()
  
  