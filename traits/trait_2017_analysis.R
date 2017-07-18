#import packages
library("tidyverse")
library("readr")
library("lubridate")

#import data 2015
trait2015 <- read_delim(file = "traits/data/2015_ChinaLeafTraitData_corrCP_16032017.csv", delim = ",", comment = "")


#fix character variables
trait2015 %>% filter(is.na(as.numeric(Dry_Mass_2016_g))) %>% distinct(Dry_Mass_2016_g) 
trait2015 %>% filter(is.na(as.numeric(Leaf_Area_m2))) %>% distinct(Leaf_Area_m2) 

trait2015 <- trait2015 %>% 
  mutate(Dry_Mass_2016_g = as.numeric(Dry_Mass_2016_g)) %>%
  mutate(Leaf_Area_m2 = as.numeric(Leaf_Area_m2)) %>%
  mutate(Leaf_Thickness_Ave_mm = rowMeans(select(., matches("Leaf_Thickness_\\d_mm")), na.rm = TRUE)) %>% #mean thickness
  # Fix wrong variables
  mutate(Taxon_FoC_corrected = gsub("\xa0", " ", Taxon_FoC_corrected),
         Taxon_FoC_corrected = trimws(Taxon_FoC_corrected)) %>%  #remove non-breaking space
  mutate(Individual_Number = ifelse(Taxon_FoC_corrected == "Hemiphragma heterophyllum" & Individual_Number == "3.2", "3", Individual_Number),
         Taxon_FoC_corrected = ifelse(Taxon_written_on_envelopes %in% c("Alapharis_nepalensis", "Alaphanis_nepalensis"), "Anaphalis nepalensis", Taxon_FoC_corrected)) %>%
  mutate(Leaf_Number = ifelse(Taxon_FoC_corrected == "Festuca sinensis" & Leaf_Area_cm2 == 0.3169, "1_1", Leaf_Number)) %>% 
  mutate(Individual_Number = as.character(Individual_Number))


# merge Newleafarea2015 with trait2015 data
trait2015 <- trait2015 %>% 
    full_join(Newleafarea2015, by = c("Site", "Elevation", "Taxon_FoC_corrected" = "Taxon", "Individual_Number", "Leaf_Number")) 

# Check if there are areas that do not match with traits
trait2015 %>% filter(is.na(Taxon_written_on_envelopes)) %>% pn  

#import data 2016
# leaf area
trait2016LeafArea <- read_delim("traits/data/2016_PFTC2_Leaf_Area_corrCP_30032017.csv", delim = ",", comment = "")

trait2016LeafArea <- trait2016LeafArea %>% 
  mutate(Date = ymd(Date))


# leaf traits
trait2016LeafTrait <- read_delim("traits/data/2016_China_envelope_names_CPcorr_30032017.csv", delim = ",", comment = "")

trait2016 <- trait2016LeafTrait %>% 
  mutate(Date = ymd(Date)) %>% 
  # NA's are created, because there is text in these columns
  rename(Elevation = Elevation_m, Individual_number = Individual_plant_number, Taxon = Plant_species) %>% 
  inner_join(trait2016LeafArea, by = c("Envelope_Name_Corrected", "Date", "Elevation", "Site", "Location", "Project", "Taxon", "Individual_number", "Leaf_number")) %>%  # retains rows in both data sets. Needs to be changes once all the names are correct!!!!
  mutate(Leaf_Thickness_Ave_mm = rowMeans(select(., matches("Leaf_Thickness_\\d_mm")), na.rm = TRUE))

#import trait taxonomy dictionary
trait_taxa <- read_delim("traits/data/trait_name_changes.csv", delim = ",", comment = "#")


##combine 2015 & 2016 trait data
#remove unneeded columns, rename columns

trait2015 <- trait2015 %>%
  mutate(Date = ymd(Date)) %>%
  select(-Leaf_Area_cm2, -Leaf_Area_m2, -Wet_Mass_WeighingScale, -Dry_Mass_WeighingScale, -Dry_Mass_g, -`LMA g -m2`, -`log LMA`, -`wet-dry`, -notes, -corrections) %>%
  rename(Taxon = Taxon_FoC_corrected, Leaf_number = Leaf_Number, Individual_number = Individual_Number, Dry_Mass_g = Dry_Mass_2016_g, SLA_cm2_g = `SLA_cm2-g`, Leaf_Area_cm2 = LeafArea2) %>%
  mutate(
    Individual_number = as.character(Individual_number),
    Date = if_else(is.na(Date), ymd("20150101"), Date),# fill missing dates
    Project = "LOCAL"
    )
  

trait2016 <- trait2016 %>%
  select(-`Difference_(Uncropped_minus_Cropped)`, -Uncropped_Leaf_Area, -X20,  -Notes, -Dry_Mass_g_Multiple2, -Dry_Mass_g_Multiple3, -Corrections.x, -`dry:wet`, -Corrections.y) %>%
  rename(Leaf_Area_cm2 = Cropped_Leaf_Area) %>%
  mutate(Leaf_number = as.character(Leaf_number)) %>% 
  mutate(allComments = NA)

# Combine and recalculate SLA and LDMC
traits_raw <- bind_rows(trait2016, trait2015) %>%
  mutate(SLA_cm2_g = Leaf_Area_cm2 / Dry_Mass_g,
         LDMC = Dry_Mass_g / Wet_Mass_g,
         Site = factor(Site, levels = c("H", "A", "M", "L")), 
         #Clean Taxon
         Taxon = gsub("_", " ", Taxon),# replace _ with " " in Taxon
         Taxon = gsub("\xa0", " ", Taxon), #remove non-breaking space
         Taxon = trimws(Taxon),
         Taxon = gsub(" Var.", " var. ", Taxon), 
         Taxon = gsub("var ", "var. ", Taxon), 
         Taxon = gsub("var\\.", "var\\. ", Taxon),
         Taxon = gsub("  ", " ", Taxon),
         Taxon = plyr::mapvalues(Taxon, from = trait_taxa$wrongName, to = trait_taxa$correctName)
         ) 


# Clean the trait data
traits <- ...


traits_raw %>%  
  mutate(year = as.factor(year(Date))) %>%
  filter(year == "2015") %>% 
  mutate(allComments = ifelse(allComments == "NA", NA, allComments)) %>% 
  mutate(allComments = gsub("_NA", "", allComments)) %>%  
  mutate(allComments = gsub(", add", "", allComments)) %>%  
  filter(grepl("black line", allComments)|is.na(allComments)) %>% 
  ggplot(aes(x = Dry_Mass_g, y = Leaf_Area_cm2, color = allComments)) + 
  geom_point() +   
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  scale_x_log10() + 
  scale_y_log10() + 
  facet_wrap(~ year)




##some plots
#wet vs dry
traits %>% mutate(year = as.factor(year(Date))) %>%
ggplot(aes(x = Wet_Mass_g, y = Dry_Mass_g, colour = Wet_Mass_g < 0.0005)) + 
  geom_point() +   
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  scale_x_log10() + 
  scale_y_log10() + 
  facet_wrap(~ year)

# dry vs area  
traits %>% mutate(year = as.factor(year(Date))) %>%
  ggplot(aes(x = Dry_Mass_g, y = Leaf_Area_cm2, color = Site)) + 
  geom_point() +   
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  scale_x_log10() + 
  scale_y_log10() + 
  facet_wrap(~ year) +
  theme(legend.position="none")


# calculate large residuals: Area vs. DryMass
LargeResid <- traits %>% filter(!is.na(Leaf_Area_cm2), !is.na(Dry_Mass_g))
fit <- lm(log(Leaf_Area_cm2) ~ log(Dry_Mass_g), data = LargeResid)
LargeResid$resid_Area_Dry <- resid(fit)
LargeResid <- LargeResid %>% select(Date, Site, Elevation, Taxon, Individual_number, Leaf_number, Dry_Mass_g, Leaf_Area_cm2, resid_Area_Dry)

LargeResid %>% mutate(year = as.factor(year(Date))) %>%
  ggplot(aes(x = Dry_Mass_g, y = Leaf_Area_cm2, color = abs(resid_Area_Dry) > 1.2)) + 
  geom_point() +   
  geom_abline(intercept = 0, slope = 1, colour = "red") +
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


traits %>% 
  mutate(year = year(Date)) %>% 
  arrange(SLA_cm2_g) %>% filter(SLA_cm2_g > 500)
  ggplot(aes(x = Wet_Mass_g, y = Dry_Mass_g, colour = SLA_cm2_g > 500)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  scale_x_log10() + 
  scale_y_log10() + 
  facet_wrap(~ year)


# yellow leaf|leaf yellow|brown|not Rumex
  
### FLAG DATA
# Wet mass > Dry mass
# Very small wet mass
# large residual for dry mass vs. leaf area
# SLA > 500 and < 5
traits<- traits %>% 
  mutate(flag = NA) %>% 
  mutate(flag = ifelse(Dry_Mass_g - Wet_Mass_g > 0, "Wet>Dry", NA)) %>% 
  mutate(flag = ifelse(Wet_Mass_g < 0.0005, paste(flag, "WetTiny", sep = "_"), NA)) %>% 
  left_join(LargeResid, by = c("Date", "Site", "Elevation", "Taxon", "Individual_number", "Leaf_number", "Dry_Mass_g", "Leaf_Area_cm2")) %>% 
  mutate(flag = ifelse(abs(resid_Area_Dry) > 1.2, paste(flag, "LargeResid", sep = "_"), NA)) %>% 
  mutate(flag = ifelse(SLA_cm2_g > 500, paste(flag, "LargeSLA", sep = "_"), NA))
  


