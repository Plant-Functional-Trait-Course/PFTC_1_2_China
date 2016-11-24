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

mass2015 <- mass2015 %>% mutate(Leaf_Thickness_1_mm = as.numeric(Leaf_Thickness_1_mm), Dry_Mass_g_2015 = as.numeric(Dry_Mass_g_2015), Dry_Mass_g_2016 = as.numeric(Dry_Mass_g_2016))

names(mass2015)



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


#### Merge 2015 & 2016 data ####



newNames2 <- read_delim(file = "old, new
V-number,
Date, Date
Site, Site
Plot,
Elevation, Elevation_m
Taxon_TNRS_correcte d, TNRS_Corrected_Plant_species
Taxon_written_on_envelopes, Envelope_Plant_species
Individual_Number,
Leaf_Number, Leaf_number
Leaf_Area_cm2,
Wet_Mass_g,
Wet_Mass_WeighingScale,
Dry_Mass_WeighingScale,
Leaf_Thickness_1_mm, Leaf_Thickness_1_mm
Leaf_Thickness_2_mm, Leaf_Thickness_2_mm
Leaf_Thickness_3_mm, Leaf_Thickness_3_mm
Leaf_Thickness_4_mm,
Leaf_Thickness_5_mm,
Leaf_Thickness_6_mm,
Dry_Mass_g_2015,
Dry_Mass_g_2016, Dry_Mass_g
Leaf_Thickness_Ave_mm,
)
 [1] "Full_Envelope_Name"                   "Date.x"                              
 [3] "Elevation_m"                          "Site.x"                              
 [5] "Location.x"                           "Project.x"                           
 [7] "Plant.species"                        "TNRS_Corrected_Plant_species"        
 [9] "TNRS_genus"                           "TNRS_species"                        
[11] "TNRS_family"                          "Individual_plant_number"             
[13] "Leaf_number.x"                        "Wet_Mass_g"                          
[15] "Dry_Mass_g"                           "Dry_Mass_g_Multiple2"                
[17] "Dry_Mass_g_Multiple3"                 "Leaf_Thickness_1_mm"                 
[19] "Leaf_Thickness_2_mm"                  "Leaf_Thickness_3_mm"                 
[21] "Notes"                                "X"                                   
[23] "X.1"                                  "Entire_File_Name"                    
[25] "Date.y"                               "Elevation"                           
[27] "Site.y"                               "Location.y"                          
[29] "Project.y"                            "Taxon"                               
[31] "Individual_number"                    "Leaf_number.y"                       
[33] "Cropped_Leaf_Area"                    "Uncropped_Leaf_Area"                 
[35] "Difference_.Uncropped_minus_Cropped." "SLA_cm2.g"                           
[37] "LDMC"                                 "Error_wet_equal_lessthan_dry"        
[39] "Error_SLA_greater_500"                "Error_SLA_lessthan_5"  