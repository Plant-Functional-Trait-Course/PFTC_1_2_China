library(lubridate)

#import data
# leaf area
trait2016LeafArea <- read.table("traits/data/2016_PFTC2_Leaf_Area_corrCP_23032017.csv", sep = ",", header = TRUE, comment = "", stringsAsFactors = FALSE) %>% as_tibble()

# replace strange values with NA
trait2016LeafArea[2081,"Cropped_Leaf_Area"] <- NA
trait2016LeafArea[2081,"Uncropped_Leaf_Area"] <- NA

trait2016LeafArea <- trait2016LeafArea %>% 
  mutate(Date = ymd(Date)) %>% 
  mutate(Cropped_Leaf_Area = as.numeric(Cropped_Leaf_Area)) %>% 
  mutate(Uncropped_Leaf_Area = as.numeric(Uncropped_Leaf_Area))


# leaf traits
trait2016LeafTrait <- read.table("traits/data/2016_China_envelope_names_CPcorr_23032017.csv", sep = ";", header = TRUE, comment = "", stringsAsFactors = FALSE) %>% as_tibble()

head(trait2016LeafTrait)
str(trait2016LeafTrait)


trait2016 <- trait2016LeafTrait %>% 
  mutate(Date = ymd(Date)) %>% 
  # NA's are created, because there is text in these columns
  mutate(Leaf_Thickness_1_mm = as.numeric(Leaf_Thickness_1_mm)) %>% 
  mutate(Leaf_Thickness_2_mm = as.numeric(Leaf_Thickness_2_mm)) %>% 
  mutate(Leaf_Thickness_3_mm = as.numeric(Leaf_Thickness_3_mm)) %>% 
  mutate(Dry_Mass_g = as.numeric(Dry_Mass_g)) %>% 
  rename(Elevation = Elevation_m, Individual_number = Individual_plant_number, Taxon = Plant_species) %>% 
 inner_join(trait2016LeafArea, by = c("Envelope_Name_Corrected", "Date", "Elevation", "Site", "Location", "Project", "Taxon", "Individual_number", "Leaf_number")) # retains rows in both data sets. Needs to be changes once all the names are correct!!!!
  

# some plots
# wet vs dry mass
ggplot(trait2016, aes(x = Wet_Mass_g, y = Dry_Mass_g)) + 
  geom_point() +   
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  scale_x_log10() + 
  scale_y_log10()


# dry vs area  
ggplot(trait2016, aes(x = Dry_Mass_g, y = Cropped_Leaf_Area)) + 
  geom_point() +   
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  scale_x_log10() + 
  scale_y_log10()


ggplot(trait2016, aes(x = Cropped_Leaf_Area, y = Leaf_Thickness_1_mm)) + 
  geom_point() +   
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  scale_x_log10() + 
  scale_y_log10()
