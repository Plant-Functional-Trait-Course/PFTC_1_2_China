#import packages
library("tidyverse")
library("readr")
library("lubridate")

pn <- . %>% print(n = Inf)

# Import recalculation of leaf area data 2015
source("traits/LeafArea2_2015.R")

#### IMPORT DATA 2015 ####
trait2015 <- read_delim(file = "traits/data/2015_ChinaLeafTraitData_corrCP_16032017.csv", delim = ",", comment = "")
trait2015_Halenia <- read_delim(file = "traits/data/2015_ChinaLeafTraitData_corrCP_16032017 Halenia_elliptica.csv", delim = ";", comment = "")

trait2015 <- trait2015 %>% 
  bind_rows(trait2015_Halenia %>% mutate(Leaf_Number = as.character(Leaf_Number)))

#fix character variables
trait2015 %>% filter(is.na(as.numeric(Dry_Mass_2016_g))) %>% distinct(Dry_Mass_2016_g) 
trait2015 %>% filter(is.na(as.numeric(Leaf_Area_m2))) %>% distinct(Leaf_Area_m2) 

trait2015 <- trait2015 %>% 
  mutate(Dry_Mass_2016_g = as.numeric(Dry_Mass_2016_g)) %>%
  mutate(Leaf_Area_m2 = as.numeric(Leaf_Area_m2)) %>%
  # if Dry_Mass_2016 is missing
  mutate(flag = ifelse(is.na(Dry_Mass_2016_g), "DryMass2015", NA)) %>% 
  mutate(Dry_Mass_2016_g = ifelse(is.na(Dry_Mass_2016_g), Dry_Mass_g, Dry_Mass_2016_g)) %>% 
  mutate(Leaf_Thickness_Ave_mm = rowMeans(select(., matches("Leaf_Thickness_\\d_mm")), na.rm = TRUE)) %>% #mean thickness
  # Fix wrong variables
  mutate(Taxon_FoC_corrected = gsub("\xa0", " ", Taxon_FoC_corrected),
         Taxon_FoC_corrected = trimws(Taxon_FoC_corrected),
         Taxon_FoC_corrected = gsub("Codonopsis foetens subsp. Nervosa", "Codonopsis foetens subsp. nervosa", Taxon_FoC_corrected)) %>%  #remove non-breaking space
  mutate(Individual_Number = ifelse(Taxon_FoC_corrected == "Hemiphragma heterophyllum" & Individual_Number == "3.2", "3", Individual_Number),
         Taxon_FoC_corrected = ifelse(Taxon_written_on_envelopes %in% c("Alapharis_nepalensis", "Alaphanis_nepalensis"), "Anaphalis nepalensis", Taxon_FoC_corrected)) %>%
  mutate(Leaf_Number = ifelse(Taxon_FoC_corrected == "Festuca sinensis" & Leaf_Area_cm2 == 0.3169, "1_1", Leaf_Number)) %>% 
  # Fixing duplicate individuals
  mutate(flag = ifelse(Taxon_FoC_corrected %in% c("Rhodiola yunnanensis", "Gentiana yunnanensis") & Site == "M", paste(flag, "Old ind nr.", Individual_Number, sep = "_"), flag)) %>% 
  mutate(Individual_Number = ifelse(Taxon_FoC_corrected %in% c("Rhodiola yunnanensis", "Gentiana yunnanensis") & Site == "M", substr(Individual_Number, 1, 1), Individual_Number))


# CHECKS!!!
setdiff(trait2015$Taxon_FoC_corrected, Newleafarea2015$Taxon)
setdiff(Newleafarea2015$Taxon, trait2015$Taxon_FoC_corrected)


# merge Newleafarea2015 with trait2015 data
trait2015 <- trait2015 %>% 
  # Dealing with duplicates: sort data by DryMass, give them id
  group_by(Site, Elevation, Taxon_FoC_corrected, Individual_Number, Leaf_Number) %>% 
  arrange(Dry_Mass_2016_g) %>% 
  mutate(LeafID = 1:n()) %>% 
  full_join(Newleafarea2015, by = c("Site", "Elevation", "Taxon_FoC_corrected" = "Taxon", "Individual_Number", "Leaf_Number", "LeafID")) %>% 
  ungroup()


# Check if there are areas that do not match with traits
trait2015 %>% filter(is.na(Taxon_written_on_envelopes)) %>% select(2:7, 21) %>% pn



#### IMPORT DATA 2016 ####
# leaf area
trait2016LeafArea <- read_delim("traits/data/2016_PFTC2_Leaf_Area_corrCP_30032017.csv", delim = ",", comment = "")

trait2016LeafArea <- trait2016LeafArea %>% 
  ### Fixing wrong variables
  mutate(Envelope_Name_Corrected = gsub("-", "_", Envelope_Name_Corrected),
         Individual_number = ifelse(is.na(Individual_number), "", Individual_number),
         Leaf_number = ifelse(Envelope_Name_Corrected == "20160811_3500_M_MO_LOCAL_Epilobium_fangii_1_3 (2)", "3 (2)", Leaf_number),
         Leaf_number = ifelse(Envelope_Name_Corrected == "20160811_3000_L_M2_1_Veronica_szechuanica_Unknown_Unknown", "Unknown", Leaf_number),
         Location = ifelse(grepl("20160815_4100_H_A5_C_Viola_biflora_var_rockiana_U_", Envelope_Name_Corrected), "H5", Location),
         Date = ifelse(grepl("20160815_4100_H_HO_LOCAL_Gentiana_trichomata_", Envelope_Name_Corrected), 20160815L, Date)) %>% 
  mutate(Date = ymd(Date))


# leaf traits
trait2016LeafTrait <-readLines(con = "traits/data/2016_China_envelope_names_CPcorr_30032017.csv") %>% 
  gsub("elevation C2, entered values", "elevation C2; entered values", .) %>% 
  read.table(text = ., sep = ",", comment = "", header = TRUE, fill = TRUE, stringsAsFactors = FALSE) %>% 
  as_tibble()
# trait2016LeafTrait <- read_delim("traits/data/2016_China_envelope_names_CPcorr_30032017.csv", delim = ",", comment = "")

trait2016LeafTrait <- trait2016LeafTrait %>% 
  mutate(Date = ymd(Date)) %>% 
  # NA's are created, because there is text in these columns
  rename(Elevation = Elevation_m, Individual_number = Individual_plant_number, Taxon = Plant_species) %>% 
  mutate(Leaf_Thickness_Ave_mm = rowMeans(select(., matches("Leaf_Thickness_\\d_mm")), na.rm = TRUE)) %>% 
  mutate(Leaf_number = as.character(Leaf_number)) %>% 
  ### Fixing wrong variables
  mutate(Envelope_Name_Corrected = gsub("-", "_", Envelope_Name_Corrected),
         Taxon = ifelse(grepl("Carex_nibigella", Envelope_Name_Corrected), "Carex_nibigella", Taxon),
         Taxon = ifelse(grepl("Cyanthus_husincans", Envelope_Name_Corrected), "Cyanthus_husincans", Taxon),
         Project = ifelse(grepl("20160812_3850_A_H3_2_Hypericum_wightianum_U", Envelope_Name_Corrected), "2", Project)) 



### Check scans with missing traits and vice versa
ScansNoTraits <- trait2016LeafArea %>% 
  anti_join(trait2016LeafTrait, by = c("Envelope_Name_Corrected", "Date", "Elevation", "Site", "Location", "Project", "Taxon", "Individual_number", "Leaf_number"))

TraitsNoScans <- trait2016LeafTrait %>%
  anti_join(trait2016LeafArea, by = c("Envelope_Name_Corrected", "Date", "Elevation", "Site", "Location", "Project", "Taxon", "Individual_number", "Leaf_number"))

allTheCrap <- bind_rows(ScansNoTraits %>% mutate(scan = TRUE),
          TraitsNoScans %>% mutate(scan = FALSE)) %>% select(-FileName)
  
### ADD COMMENTS TO ALL ALLTHECRAP FILES

# Useful code to check allTheCrap
#allTheCrap %>% filter(grepl("Galium_hoffmeisteri", Envelope_Name_Corrected)) %>% as.data.frame()
#dir("/Volumes/My Passport/Traits - scans and envelopes/China Leaf Scans 2016/", pattern = "M6.OTC.Epilobium.fangii", recursive = TRUE, full.names = TRUE)


# join Trait and Area data for 2016
trait2016 <- trait2016LeafTrait %>% 
    # retains rows in both data sets. Needs to be changes once all the names are correct!!!!
  inner_join(trait2016LeafArea, by = c("Envelope_Name_Corrected", "Date", "Elevation", "Site", "Location", "Project", "Taxon", "Individual_number", "Leaf_number"))
  


#import trait taxonomy dictionary
trait_taxa <- read_delim("traits/data/trait_name_changes.csv", delim = ",", comment = "#")


#### COMBINE 2015 & 2016 TRAIT DATA ####
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
  select(-`Difference_(Uncropped_minus_Cropped)`, -Uncropped_Leaf_Area, -X, -X.1, -Notes, -Dry_Mass_g_Multiple2, -Dry_Mass_g_Multiple3, -Corrections.x, -dry.wet, -Corrections.y) %>%
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

### NEED TO FIX THIS!!! FULL_ENV_NAME DOES NOT MATCH?!?
# CN Analysis
# read in ID
CN_ID <- read.csv("traits/data/ChinaLeafTraitData_senttogroup.csv", sep = ";", fill = TRUE, stringsAsFactors = FALSE)

CN_ID <- CN_ID %>% 
  as_tibble() %>% 
  filter(stoich.vial.label != "") %>% 
  mutate(Full_Envelope_Name = gsub("-", "_", Full_Envelope_Name)) %>% 
  select(Full_Envelope_Name, stoich.vial.label)

# CN data
CNdata <- read_excel(path = "traits/data/China_CNP_July18_2017.xls", col_types = c(rep("text", 2), rep("numeric", 5), "text", rep("numeric", 3), "text"))

CNdata <- CNdata %>% 
  select(-SITE) %>%
  rename(StoichLabel = `STOICH LABEL`, C_percent = `%C`, N_percent = `%N`, C_percent = `%C`, CN_ratio = `C/N`, dN15_percent = `δ15N ‰`, dC13_percent = `δ13C ‰`, P_Std_Dev = `P_STD DEV`, P_Co_Var = `P_CO VAR`) %>% 
  mutate(StoichLabel = gsub("\\.000000", "", StoichLabel)) %>% 
  left_join(CN_ID, by = c(StoichLabel = "stoich.vial.label")) %>% 
  mutate(Full_Envelope_Name = gsub("\\-O\\-", "\\-0\\-", Full_Envelope_Name)
         #Full_Envelope_Name = gsub("Viola\\_biflora\\_var\\_rockiana", "Viola\\_biflora\\_var\\.\\_rockiana", Full_Envelope_Name)
         )

setdiff(CNdata$Full_Envelope_Name, traits_raw$Full_Envelope_Name)

# Merge CN Data with traits
traits_raw <- traits_raw %>% 
  full_join(CNdata, by = c(Envelope_Name_Corrected = "Full_Envelope_Name"))


# Check duplicate rows
traits_raw %>% 
  group_by(Site, Taxon, Individual_number, Leaf_number, Project, Location) %>% 
  filter(n() > 1) %>% filter(year(Date) == 2016)


trait2016LeafTrait %>% 
  group_by(Site, Plant_species, Individual_plant_number, Leaf_number, Project, Location) %>% 
  filter(n() > 1) %>% group_by(Full_Envelope_Name, Site, Plant_species, Individual_plant_number, Leaf_number, Project, Location) %>% count() %>% arrange(n) %>% pn


# Cleaning
traits_raw %>% 
  

# Need fixing? Date, Individual_number; Leaf_number; Project: O-0
# What to do with "2015-01-01"  
# Also check if problematic 2016 leaves, where project and location do not match
  trait2016 %>% mutate(loc1 = substr(Location, 1, 1)) %>%
  count(loc1, Site, Project) %>% print(n = 100)
  

# Clean the trait data
# remove yellow leaf|leaf yellow|brown|not Rumex
# eaten, folded, cut leaves seem not problematic, but some with part of leaf too white!!!
traits <- traits_raw %>% 
  # Remove unneeded comments
  mutate(allComments = gsub("NA", NA, allComments)) %>% # remove "NA" in comments
  mutate(allComments = gsub("add", NA, allComments)) %>% # remove add in comments
  mutate(allComments = gsub(", add, |, add|add _ add, |add _ |add, ", "", allComments)) %>%
  # remove problematic leaves
  filter(!grepl("|not Rumex|black", allComments)) %>% 
  #filter(!grepl("yellow leaf|leaf yellow|brown", allComments)) %>% 
  #filter(!grepl("^leaf folded$", allComments)) %>% 
# FLAG DATA
# Wet mass > Dry mass
# Very small wet mass
# large residual for dry mass vs. leaf area
# SLA > 500 and < 5
  mutate(flag = ifelse(Dry_Mass_g - Wet_Mass_g > 0, paste("Wet>Dry", sep = "_"), NA)) %>% 
  mutate(flag = ifelse(Wet_Mass_g < 0.0005, paste(flag, "WetTiny", sep = "_"), NA)) %>% 
  mutate(flag = ifelse(SLA_cm2_g > 500, paste(flag, "LargeSLA", sep = "_"), NA))

### Remove leaf area for brown leafs, yellow, eaten etc. But other data can still be good.


# calculate large residuals: Area vs. DryMass
LargeResid <- traits %>% filter(!is.na(Leaf_Area_cm2), !is.na(Dry_Mass_g))
fit <- lm(log(Leaf_Area_cm2) ~ log(Dry_Mass_g), data = LargeResid)
LargeResid$resid_Area_Dry <- resid(fit)
LargeResid <- LargeResid %>% select(Date, Site, Elevation, Taxon, Individual_number, Leaf_number, Dry_Mass_g, Leaf_Area_cm2, resid_Area_Dry)

traits <- traits %>% 
  left_join(LargeResid, by = c("Date", "Site", "Elevation", "Taxon", "Individual_number", "Leaf_number", "Dry_Mass_g", "Leaf_Area_cm2")) %>% 
  mutate(flag = ifelse(abs(resid_Area_Dry) > 1.2, paste(flag, "LargeResid", sep = "_"), NA))
  

## Check data with some plots
# wet vs dry
# colour: Wet_Mass_g < 0.0005
traits %>% mutate(year = as.factor(year(Date))) %>%
ggplot(aes(x = Wet_Mass_g, y = Dry_Mass_g, colour = allComments)) + 
  geom_point() +   
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  scale_x_log10() + 
  scale_y_log10() + 
  facet_wrap(~ year)

# dry vs area  
traits %>% mutate(year = as.factor(year(Date))) %>%
  filter(year == 2015) %>% 
  ggplot(aes(x = Dry_Mass_g, y = Leaf_Area_cm2, color = allComments)) + 
  geom_point() +   
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  scale_x_log10() + 
  scale_y_log10() + 
  facet_wrap(~ year)


# large resids
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
  arrange(SLA_cm2_g) %>%
  ggplot(aes(x = Wet_Mass_g, y = Dry_Mass_g, colour = SLA_cm2_g > 500)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  scale_x_log10() + 
  scale_y_log10() + 
  facet_wrap(~ year)



  


