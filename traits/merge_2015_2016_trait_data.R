#trait data 2015/6

library(readxl)
library(dplyr)

fl <- dir("traits/data/", pattern = "LeafTrait.*xlsx$", full.names = TRUE)

allNames <- plyr::llply(fl, function(x){
  names(read_excel(x, sheet = 1, skip = 1)  )
})
allNames <- unique(unlist(allNames))



cat(paste0("\n'", allNames, "',"))
library(readr)
newNames <- read_delim(delim = ",", trim_ws = TRUE, file = 
"old, new
V-number, 
Date, 
Site, 
Plot, 
Elevation, 
Taxon_TNRS_corrected, 
Taxon_written_on_envelopes, 
Individual_Number, 
Leaf_Number, 
Leaf_Area_cm2, 
Wet_Mass_g, 
Wet_Mass_WeighingScale, 
Dry_Mass_g, 
Dry_Mass_WeighingScale, 
Leaf_Thickness_1_mm, 
Leaf_Thickness_2_mm, 
Leaf_Thickness_3_mm, 
Leaf_Thickness_4_mm, 
Leaf_Thickness_5_mm, 
Leaf_Thickness_6_mm, 
Dry_Mass_g_2015_weight, Dry_Mass_g_2015
Dry_Mass_g_2016_weight, Dry_Mass_g_2016
Taxon TNRS corrected, Taxon_TNRS_corrected
Wet_Mass_WeighingScale (1 or 2), Wet_Mass_WeighingScale 
Dry_Mass_WeighingScale (1 or 2), Dry_Mass_WeighingScale
Leaf_Thickness_Ave_mm, 
SLA, 
LDMC, 
Dry_Mass_g_2015, 
dry weight_2016, Dry_Mass_g_2016
NA, 
Dry_Mass_g_2015_weigh, Dry_Mass_g_2015
Dry_Mass_g_2016_weigh, Dry_Mass_g_2016 
")
newNames <- newNames[!is.na(newNames$new), ]



mass2015 <- plyr::ldply(fl, function(x){
  x <- read_excel(x, sheet = 1, skip = 1)
  x <- x[, !names(x) %in% c("Dry_Mass_g", "LDMC", "SLA")]
  colnames(x) <- plyr::mapvalues(colnames(x), from = newNames$old, to = newNames$new, warn_missing = FALSE)
  x[is.na(names(x))] <- NULL
  x
})

mass2015 <- mass2015 %>% 
  mutate(Leaf_Thickness_1_mm = as.numeric(Leaf_Thickness_1_mm), Dry_Mass_g_2015 = as.numeric(Dry_Mass_g_2015), Dry_Mass_g_2016 = as.numeric(Dry_Mass_g_2016)) %>%
  mutate(Project = "LOCAL")
  
##Import other 2015 trait files here



#some plots
library(ggplot2)
ggplot(mass2015, aes(Dry_Mass_g_2015, Dry_Mass_g_2016)) + geom_point()
ggplot(mass2015, aes(Wet_Mass_g, Dry_Mass_g_2016)) + geom_point() + geom_abline(slope = 1, intercept = 0) + scale_x_log10() + scale_y_log10()
ggplot(mass2015, aes(Wet_Mass_g, Dry_Mass_g_2015)) + geom_point() + geom_abline(slope = 1, intercept = 0) + scale_x_log10() + scale_y_log10()





ggplot(mass2015, aes(Leaf_Area_cm2, Dry_Mass_g_2016)) + geom_point() + geom_abline(slope = 1, intercept = 0) + scale_x_log10() + scale_y_log10()

#### 2016 data ####
chinatraitsdata1 <- read_csv("traits/data/2016August_China_leaf_trait_envelope_names_LPEdit2.csv")
leafarea <- read_csv("traits/data/2016_PFTC2_Leaf_Area_11062016.csv")
#chinatraitsdatajoin <- full_join(chinatraitsdata1, leafarea, by = "Full_Envelope_Name") 
sum(is.na(leafarea$Date))



traits2016 <- read_csv("traits/data/2016_ChinaLeafTraitData_FlaggedErrors_11062016.csv")

#remove redundant columns
with(traits2016, all.equal(Project.x, Project.y))
with(traits2016, all.equal(Date.x, Date.y))
with(traits2016, all.equal(Location.x, Location.y))
with(traits2016, all.equal(Site.x, Site.y))
with(traits2016, all.equal(Leaf_number.x, Leaf_number.y))
with(traits2016, table(is.na(Leaf_number.x), is.na(Leaf_number.y), useNA = "ifany"))
with(traits2016, all.equal(Elevation_m, Elevation))
with(traits2016, table(is.na(Elevation_m), is.na(Elevation), useNA = "ifany"))
with(traits2016, all.equal(Individual_plant_number, Individual_number))
with(traits2016, table(is.na(Individual_plant_number), is.na(Individual_number), useNA = "ifany"))
with(traits2016, all.equal(Plant.species, Taxon))


traits2016 <- traits2016 %>% 
  select(-Project.y, -Date.y, -Location.y, -Site.y, -Leaf_number.y, -Elevation, -Individual_number, -Taxon, -Entire_File_Name, -Uncropped_Leaf_Area, -Difference_.Uncropped_minus_Cropped.) %>%
  rename(Project = Project.x, Date = Date.x, Location = Location.x, Site = Site.x, Leaf_number = Leaf_number.x)

names(traits2016)

## clean meta data
traits2016 %>% group_by(Site) %>% count()
traits2016 %>% group_by(Location) %>% count() %>% as.data.frame()##problems for later
traits2016 %>% group_by(Project) %>% count() 

traits2016 <- traits2016 %>% 
  mutate(Site = recode(Site, l = "L")) %>%
  mutate(Project = recode(Project, LOCOL = "LOCAL", O = "0")) #more problems here
  

###Correct dry mass that appear to be in mg
traits2016 <- traits2016 %>%
  mutate(Dry_Mass_was_probably_mg = log10(Dry_Mass_g) > log10(Wet_Mass_g) + 1) %>% #dry mass more than 10X wet mass
  mutate(Dry_Mass_g = ifelse(log10(Dry_Mass_g) < log10(Wet_Mass_g) + 1, Dry_Mass_g, Dry_Mass_g/1000))

#### Merge 2015 & 2016 data ####
#make new columns to match
mass2015 <- mass2015 %>% 
  rename(Elevation_m = Elevation, Dry_Mass_g = Dry_Mass_g_2016) %>%
  rename(TNRS_Corrected_Plant_species = Taxon_TNRS_corrected) %>%
  rename(Individual_Plant_Number = Individual_Number) %>%
  rename(Location = Plot) %>% # ALL NA!
  mutate(Location = as.character(Location)) %>% #to keep code below happy
  select(-Dry_Mass_g_2015, -Dry_Mass_WeighingScale, -Wet_Mass_WeighingScale) %>%
  mutate(Dry_Mass_was_probably_mg = FALSE) %>%
  mutate(TNRS_genus = gsub("^(\\w+)\\s.*$", "\\1", TNRS_Corrected_Plant_species)) %>%
  mutate(TNRS_species = gsub("^(\\w+)\\s(.*)$", "\\2", TNRS_Corrected_Plant_species))
  
traits2016 <- traits2016 %>%
  rename(Leaf_Area_cm2 = Cropped_Leaf_Area) %>%
  rename(Leaf_Number = Leaf_number) %>%
  rename(Taxon_written_on_envelopes = Plant.species) %>%
  mutate(Leaf_Thickness_1_mm = as.numeric(Leaf_Thickness_1_mm)) %>%
  select(-X, -X.1)

traits <- bind_rows(traits2016, mass2015)
names(traits)

#### recalculate LDMC & SLA ####
traits <- traits %>%
  mutate(SLA_cm2.g = Leaf_Area_cm2/Dry_Mass_g) %>%
  mutate(LDMC = Dry_Mass_g/Wet_Mass_g) %>%
  mutate(Leaf_Thickness_Ave_mm = rowSums(select(traits, matches("Leaf_Thickness_._mm")), na.rm = TRUE)) %>%
  #FLAGS
  mutate(Error_wet_equal_lessthan_dry = Dry_Mass_g >= Wet_Mass_g) %>%
  mutate(Error_SLA_greater_500 = SLA_cm2.g > 500) %>%
  mutate(Error_SLA_lessthan_5 = SLA_cm2.g <5)
    

#### plots ####
ggplot(traits, aes(x = Wet_Mass_g, y = Dry_Mass_g)) + geom_point() + geom_abline(slope = 1, intercept = 0) + scale_x_log10() + scale_y_log10()

ggplot(traits, aes(x = Dry_Mass_g, y = Leaf_Area_cm2, colour = grepl(2015, Date))) + geom_point() + geom_abline(slope = 1, intercept = 0) + scale_x_log10() + scale_y_log10()

ggplot(traits, aes(x = Leaf_Area_cm2, y = SLA_cm2.g, colour = grepl(2015, Date))) + geom_point() + geom_abline(slope = 1, intercept = 0) + scale_x_log10() + scale_y_log10()
ggplot(traits, aes(x = SLA_cm2.g, colour = grepl(2015, Date))) + geom_histogram() + scale_x_log10() + geom_vline(xintercept = 500)

#### Save output
save(traits, file = "traits/transplant_traits_2015_2016_20161124.Rdata")
