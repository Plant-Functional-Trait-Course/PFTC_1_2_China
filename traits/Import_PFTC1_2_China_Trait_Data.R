#load libraries
library("tidyverse")
library("readr")
library("lubridate")

pn <- . %>% print(n = Inf)
`%g%` <- function(x, pattern){grepl(pattern, x)}

#### IMPORT DATA 2015 ####

# Import leaf area
source("traits/LeafArea2_2015.R")

# import trait data
trait2015_all <- read_delim(file = "traits/data/2015_ChinaLeafTraitData_corrCP_16032017.csv", delim = ",", comment = "")
# import missing trait data from Halenia leaves and r_bind to data set
trait2015_Halenia <- read_delim(file = "traits/data/2015_ChinaLeafTraitData_corrCP_16032017 Halenia_elliptica.csv", delim = ";", comment = "")

trait2015_all <- trait2015_all %>% 
  bind_rows(trait2015_Halenia %>% mutate(Leaf_Number = as.character(Leaf_Number)))

#check character variables
trait2015_all %>% filter(is.na(as.numeric(Dry_Mass_2016_g))) %>% distinct(Dry_Mass_2016_g) 
trait2015_all %>% filter(is.na(as.numeric(Leaf_Area_m2))) %>% distinct(Leaf_Area_m2) 

# fix some variables, replace missing 2016 dry mass with 2015 dry mass
trait2015 <- trait2015_all %>% 
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


# Check if species in traits but not in area and vice versa (there should be no leafs in area and missing in trait!)
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


# Check if there are areas that do not match with traits (should be zero!)
trait2015 %>% filter(is.na(Taxon_written_on_envelopes)) %>% select(2:7, 21) %>% pn



#### IMPORT DATA 2016 ####
# import leaf area
trait2016LeafArea <- read_delim("traits/data/2016_PFTC2_Leaf_Area_corrCP_30032017.csv", delim = ",", comment = "")

# fix some ind. and leaf_numbers, location and date
trait2016LeafArea <- trait2016LeafArea %>% 
  ### Fixing wrong variables
  mutate(Envelope_Name_Corrected = gsub("-", "_", Envelope_Name_Corrected),
         Individual_number = ifelse(is.na(Individual_number), "", Individual_number),
         Leaf_number = ifelse(Envelope_Name_Corrected == "20160811_3500_M_MO_LOCAL_Epilobium_fangii_1_3 (2)", "3 (2)", Leaf_number),
         Leaf_number = ifelse(Envelope_Name_Corrected == "20160811_3000_L_M2_1_Veronica_szechuanica_Unknown_Unknown", "Unknown", Leaf_number),
         Location = ifelse(grepl("20160815_4100_H_A5_C_Viola_biflora_var_rockiana_U_", Envelope_Name_Corrected), "H5", Location),
         Date = ifelse(grepl("20160815_4100_H_HO_LOCAL_Gentiana_trichomata_", Envelope_Name_Corrected), 20160815L, Date)) %>% 
  mutate(Date = ymd(Date))


# import leaf trait data
trait2016LeafTrait <-readLines(con = "traits/data/2016_China_envelope_names_CPcorr_30032017.csv") %>% 
  gsub("elevation C2, entered values", "elevation C2; entered values", .) %>% 
  read.table(text = ., sep = ",", comment = "", header = TRUE, fill = TRUE, stringsAsFactors = FALSE) %>% 
  as_tibble()

# fix some variables
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
         Project = ifelse(grepl("20160812_3850_A_H3_2_Hypericum_wightianum_U", Envelope_Name_Corrected), "2", Project)) %>% 
  # Create flag columns
  mutate(AreaFlag = "", WetFlag = "", DryFlag = "", ThickFlag = "", GeneralFlag = "")


### Check for scans with missing traits and vice versa
ScansNoTraits <- trait2016LeafArea %>% 
  anti_join(trait2016LeafTrait, by = c("Envelope_Name_Corrected", "Date", "Elevation", "Site", "Location", "Project", "Taxon", "Individual_number", "Leaf_number"))

TraitsNoScans <- trait2016LeafTrait %>%
  anti_join(trait2016LeafArea, by = c("Envelope_Name_Corrected", "Date", "Elevation", "Site", "Location", "Project", "Taxon", "Individual_number", "Leaf_number"))

# contains leaves with missing scans etc.
allTheCrap <- bind_rows(ScansNoTraits %>% mutate(scan = TRUE),TraitsNoScans %>% mutate(scan = FALSE)) %>% 
  # remove from list, because leaf area needs calculation
  filter(!Envelope_Name_Corrected %in% c("20160810_3000_L_LO_LOCAL_Athyrium_davidii_1_1", "20160810_3500_M_MO_LOCAL_Fargesia_sp_1_4")) %>% 
  # remove because no envelope
  filter(Envelope_Name_Corrected != "20160812_3850_A_M2_2_Geranium_pylzowianum_U_5") %>% 
  # remove because no leaf and no envelope
  filter(Envelope_Name_Corrected != "20160811_3000_L_L7_0_Galium_hoffmeisteri_2_2") %>% 
  filter(!grepl("20160813_4100_H_HO_LOCAL_Carex_sp1_5_", Envelope_Name_Corrected))
# remaining leaves have no scan
  

# Useful code to check allTheCrap
#allTheCrap %>% filter(grepl("Galium_hoffmeisteri", Envelope_Name_Corrected)) %>% as.data.frame()
#dir("/Volumes/My Passport/Traits - scans and envelopes/China Leaf Scans 2016/", pattern = "M6.OTC.Epilobium.fangii", recursive = TRUE, full.names = TRUE)


# Add flags for missing traits and area
trait2016LeafTrait <- trait2016LeafTrait %>% 
  # next line can go if we have calculated leaf area!!!
  mutate(AreaFlag = ifelse(Envelope_Name_Corrected %in% c("20160810_3000_L_LO_LOCAL_Athyrium_davidii_1_1", "20160810_3500_M_MO_LOCAL_Fargesia_sp_1_4"), "leaf area missing; need scaning", AreaFlag),
         WetFlag = ifelse(Envelope_Name_Corrected == "20160812_3850_A_M2_2_Geranium_pylzowianum_U_5", "envelope missing", WetFlag),
         DryFlag = ifelse(Envelope_Name_Corrected == "20160812_3850_A_M2_2_Geranium_pylzowianum_U_5", "envelope missing", DryFlag),
         ThickFlag = ifelse(Envelope_Name_Corrected == "20160812_3850_A_M2_2_Geranium_pylzowianum_U_5", "envelope missing", ThickFlag),
         AreaFlag = ifelse(Envelope_Name_Corrected == "20160811_3000_L_L7_0_Galium_hoffmeisteri_2_2" | Corrections == "T and scan missing", "scan missing", AreaFlag),
         WetFlag = ifelse(Envelope_Name_Corrected == "20160811_3000_L_L7_0_Galium_hoffmeisteri_2_2" | Corrections == "T and scan missing", "envelope missing", WetFlag),
         DryFlag = ifelse(Envelope_Name_Corrected == "20160811_3000_L_L7_0_Galium_hoffmeisteri_2_2" | Corrections == "T and scan missing", "envelope missing", DryFlag),
         ThickFlag = ifelse(Envelope_Name_Corrected == "20160811_3000_L_L7_0_Galium_hoffmeisteri_2_2" | Corrections == "T and scan missing", "envelope missing", ThickFlag),
         AreaFlag = ifelse(Envelope_Name_Corrected %in% allTheCrap$Envelope_Name_Corrected, "scan missing", AreaFlag))

# Remove duplicates
trait2016LeafTrait <- trait2016LeafTrait %>%
  group_by(Envelope_Name_Corrected, Site, Taxon, Individual_number, Leaf_number, Project, Location) %>% 
  mutate(LeafID = 1:n()) %>% 
  filter(!(grepl("_M5_0_Geranium_pylzowianum_U", Envelope_Name_Corrected) & LeafID == 2)) %>% 
  filter(!(grepl("L3_0_Geranium_pylzowianum_1_1", Envelope_Name_Corrected) & LeafID == 2)) %>% 
  select(-LeafID)

# Check duplicate rows
DuplicatesRow2016 <- trait2016LeafTrait %>%
  group_by(Envelope_Name_Corrected, Site, Taxon, Individual_number, Leaf_number, Project, Location) %>% 
  filter(n() > 1) %>% group_by(Envelope_Name_Corrected, Site, Taxon, Individual_number, Leaf_number, Project, Location) %>%
  count() %>% arrange(n)

# add AreaFlag for duplicate envelope with only one area; It is very hard to assign area to leaf!
trait2016LeafTrait <- trait2016LeafTrait %>% 
  mutate(AreaFlag = ifelse(Envelope_Name_Corrected %in% DuplicatesRow2016$Envelope_Name_Corrected, "Duplicate envelop only one area #zap", AreaFlag))



### Join Trait and Area data for 2016
trait2016 <- trait2016LeafTrait %>% 
  full_join(trait2016LeafArea, by = c("Envelope_Name_Corrected", "Date", "Elevation", "Site", "Location", "Project", "Taxon", "Individual_number", "Leaf_number"))
  

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
  ungroup() %>% 
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
         Taxon = gsub(" Var.", " var. ", Taxon), 
         Taxon = gsub("var ", "var. ", Taxon), 
         Taxon = gsub("var\\.", "var\\. ", Taxon),
         Taxon = gsub("  ", " ", Taxon),
         Taxon = plyr::mapvalues(Taxon, from = trait_taxa$wrongName, to = trait_taxa$correctName),
         Taxon = trimws(Taxon)
         )

  
#************************************************************************** 
#### CN ANALYSIS ####
# read in ID for 2016
CN_ID2016 <- read.csv("traits/data/ChinaLeafTraitData_senttogroup.csv", sep = ";", fill = TRUE, stringsAsFactors = FALSE)

CN_ID2016 <- CN_ID2016 %>% 
  as_tibble() %>% 
  filter(stoich.vial.label != "") %>% 
  select(Full_Envelope_Name, stoich.vial.label) %>% 
  mutate(Full_Envelope_Name = gsub("-", "_", Full_Envelope_Name)) %>% 
  mutate(Full_Envelope_Name = gsub("-O-", "-0-", Full_Envelope_Name))


# 2015 ID
CN_ID2015 <- read_excel(path = "traits/data/2015 China leaves.xls", col_names = TRUE)

CN_ID <- CN_ID2015 %>% 
  as_tibble() %>% 
  filter(`stoich vial label` != "") %>% 
  select(Date, Elevation, Site, Taxon, Individual_number, Leaf_number, `stoich vial label`) %>% 
  mutate(stoich.vial.label = as.character(`stoich vial label`)) %>% 
  select(-`stoich vial label`) %>% 
  bind_rows(CN_ID2016)

# CN data
CNdata <- read_excel(path = "traits/data/CHINA_CNP_19January2018.xls")

# Still a whole batch that does not match
CNdata <- CNdata %>% 
  select(-SITE) %>%
  rename(StoichLabel = `STOICH LABEL`, C_percent = `%C`, N_percent = `%N`, C_percent = `%C`, CN_ratio = `C/N`, dN15_percent = `δ15N ‰`, dC13_percent = `δ13C ‰`, P_Std_Dev = `P_STD DEV`, P_Co_Var = `P_CO VAR`) %>% 
  #mutate(StoichLabel = gsub("\\.000000", "", StoichLabel)) %>% 
  full_join(CN_ID, by = c("StoichLabel" = "stoich.vial.label"))

CN2015 <- CNdata %>% 
  filter(is.na(Full_Envelope_Name)) %>% 
  select(-Full_Envelope_Name) %>% 
  mutate(Date = dmy(Date), Site = as.character(Site), Leaf_number = as.character(Leaf_number)) 
  
CN2016 <- CNdata %>% 
  filter(!is.na(Full_Envelope_Name)) %>% 
  select(-Date, -Elevation, -Site, -Taxon, -Individual_number, -Leaf_number)


# Merge CN Data with traits data separate for each year
# Using left join, because not all leaves have CN data
# 2015 data
traits_raw2015 <- traits_raw %>% 
  filter(is.na(Full_Envelope_Name)) %>% 
  left_join(CN2015, by = c("Date", "Elevation", "Site", "Taxon", "Individual_number", "Leaf_number"))
 

# 2016 data
traits_raw2016 <- traits_raw %>% 
  filter(!is.na(Full_Envelope_Name)) %>% 
  mutate(Full_Envelope_Name = gsub("-", "_", Full_Envelope_Name)) %>% 
  mutate(Full_Envelope_Name = gsub("-O-", "-0-", Full_Envelope_Name)) %>% 
  left_join(CN2016, by = c("Full_Envelope_Name"))


traits_raw2 <- traits_raw2015 %>% 
  bind_rows(traits_raw2016) %>%
  # remove duplicates
  distinct()


#*************************************************************************************
#### FLAG DATA ####
# And more Cleaning
# Check for problematic 2016 leaves, where site, project and location do not match
#traits_raw %>% mutate(loc1 = substr(Location, 1, 1)) %>% count(loc1, Site, Project) %>% pn

traits_raw2 <- traits_raw2 %>% 
  mutate(GeneralFlag = ifelse(Envelope_Name_Corrected == "20160812_3850_A_A7_2_Potentilla_leuconota_U_1", paste(GeneralFlag, "Wrong Site Location or Project #zap"), GeneralFlag),
         GeneralFlag = ifelse(Envelope_Name_Corrected == "20160812_3500_M_A5_2_Artemisia_flaccida_U_1", paste(GeneralFlag, "Wrong Site Location or Project #zap"), GeneralFlag),
         GeneralFlag = ifelse(grepl("20160815_4100_H_H2_6_Polygonum_viviparum_", Envelope_Name_Corrected), paste(GeneralFlag, "Wrong Site Location or Project #zap"), GeneralFlag),
         Project = ifelse(grepl("20160812_3850_A_H3_2_Hypericum_wightianum_U_", Envelope_Name_Corrected), "1", Project),
         GeneralFlag = ifelse(grepl("20160812_3850_A_H3_2_Hypericum_wightianum_U_", Envelope_Name_Corrected), paste(GeneralFlag, "Possible wrong Site Location Project; changed Project from 2 to 1"), GeneralFlag),
         Project = ifelse(grepl("20160815_4100_H_H2_6_Allium_prattii_U_|20160815_4100_H_H2_6_Polygonum_macrophyllum_U_|20160815_4100_H_H2_6_Viola_biflora_var_rockiana_1_", Envelope_Name_Corrected), "C", Project),
         GeneralFlag = ifelse(grepl("20160815_4100_H_H2_6_Allium_prattii_U_|20160815_4100_H_H2_6_Polygonum_macrophyllum_U_|20160815_4100_H_H2_6_Viola_biflora_var_rockiana_1_", Envelope_Name_Corrected), paste(GeneralFlag, "Project might be wrong changed 6 to C"), GeneralFlag),
         
         Project = ifelse(Envelope_Name_Corrected == "20160812_3850_A_A7_Unknown_Veronica_szechuanica_U_1", "0", Project),
         GeneralFlag = ifelse(Envelope_Name_Corrected == "20160812_3850_A_A7_Unknown_Veronica_szechuanica_U_1", paste(GeneralFlag, "Project might be wrong changed Unknown to 0"), GeneralFlag)
         )




# separate flag for Area (AreaFlag), Wet mass (WetFlag), Dry mass (DryFlag) and Leaf Thickness (ThickFlag) and one for general comments (GeneralFlag)
# add #zap in the comment if it should be removed
# all other comments are only warnings
traits <- traits_raw2 %>% 
  # Remove unneeded comments
  mutate(allComments = gsub("NA", NA, allComments)) %>% # remove "NA" in comments
  # remove add in comments
  mutate(allComments = gsub("add|, add, |, add|add _ add, |add _ |add, | _ add|add_", "", allComments)) %>%
  # remove problematic leaves
  filter(!grepl("not Rumex", allComments)) %>% 
  # Flag possibly problematic leaves: eaten, folded, cut, too white, etc.
  mutate(AreaFlag = ifelse(grepl("leaf eaten|white|stalk missing|folded|cut|leaf not recognised", allComments), paste(AreaFlag, "Area might be incorrect eaten cut white etc"), AreaFlag),
         # SLA wrong
         AreaFlag = ifelse(SLA_cm2_g > 500, paste(AreaFlag, "Area might be incorrect SLA too large"), AreaFlag),
         AreaFlag = ifelse(SLA_cm2_g < 5, paste(AreaFlag, "Area might be incorrect SLA too small"), AreaFlag),
         DryFlag = ifelse(SLA_cm2_g > 500, paste(DryFlag, "Dry mass might be incorrect SLA too large"), DryFlag),
         DryFlag = ifelse(SLA_cm2_g < 5, paste(DryFlag, "Dry mass might be incorrect SLA too small"), DryFlag),
         # brown and yellow leaves
         WetFlag = ifelse(grepl("brown|yellow|eaten", allComments), paste(WetFlag, "Wet mass might be too low"), WetFlag),
         # Wet < Dry
         WetFlag = ifelse(Wet_Mass_g < Dry_Mass_g, paste(WetFlag, "Wet mass might be incorrect Wet < Dry #zap"), WetFlag),
         # Tiny Wet mass
         WetFlag = ifelse(Wet_Mass_g < 0.0005, paste(WetFlag, "Wet mass might be incorrect Tiny Wet mass"), WetFlag),
         DryFlag = ifelse(Wet_Mass_g < Dry_Mass_g, paste(DryFlag, "Wet mass might be incorrect Wet < Dry"), DryFlag),
         DryFlag = ifelse(grepl("brown|yellow|eaten", allComments), paste(DryFlag, "Dry mass might be too low"), DryFlag),
         # Missing Dry_Mass_2016_g for 2015 leaves
         DryFlag = ifelse(year(Date) == "2015" & flag == "DryMass2015", paste(DryFlag, "Area might be incorrect 2015 weighing"), DryFlag)) %>% 
  # Flag wrong Individual and leaf number
  mutate(GeneralFlag = ifelse(Individual_number %in% c("", "Unknown", "U", "1.1", "1.2", "2.1", "2.2", "3.1", "3.2", "4.2", "5.2"), paste(GeneralFlag, "Wrong Ind. number due to duplicate FileName"), GeneralFlag),
         GeneralFlag = ifelse(Individual_number %in% c(""), paste(GeneralFlag, "Missing Ind. number"), GeneralFlag),
         GeneralFlag = ifelse(Leaf_number %in% c("Unknown", "3 (2)", "4 (2)", "5 (2)", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25"), paste(GeneralFlag, "Wrong leaf number due to duplicate or missing Ind. nr"), GeneralFlag),
         GeneralFlag = ifelse(is.na(Leaf_number), paste(GeneralFlag, "Missing leaf number"), GeneralFlag)) %>% 
  # Flag duplicate leaves where the area was merged by arranging by arranging dry mass and size
  mutate(AreaFlag = ifelse(LeafID == 2 & year(Date) == 2015, paste(AreaFlag, "Area possibly wrong; duplicate EnvelopeName; matching area and mass arranged by size and mass"), AreaFlag),
         DryFlag = ifelse(LeafID == 2 & year(Date) == 2015, paste(DryFlag, "Area possibly wrong; duplicate EnvelopeName; matching area and mass arranged by size and mass"), DryFlag)) %>% 
  mutate(DryFlag = gsub("NA |^ ", "", DryFlag),
         AreaFlag = gsub("NA |^ ", "", AreaFlag),
         WetFlag = gsub("NA |^ ", "", WetFlag),
         ThickFlag = gsub("NA |^ ", "", ThickFlag),
         GeneralFlag = gsub("NA |^ ", "", GeneralFlag)) %>% 
  # Remove high N values: > 6.4 (Henn et al paper)
  mutate(N_percent = ifelse(N_percent > 6.4, NA, N_percent)) %>% 
  #  # Remove SLA > 2000
  mutate(SLA_cm2_g = ifelse(SLA_cm2_g > 2000, NA, SLA_cm2_g)) 
 
# fix names
traits <- traits %>% 
  rename("P_percent" = "P_AVG", "BlockID" = "Location", "Treatment" = "Project") %>% 
  select(Envelope_Name_Corrected:Leaf_Thickness_3_mm, Leaf_Thickness_4_mm:Leaf_Thickness_6_mm, Leaf_Thickness_Ave_mm, Leaf_Area_cm2, SLA_cm2_g:LDMC, C_percent:P_Co_Var, WetFlag, DryFlag, ThickFlag, AreaFlag, GeneralFlag, allComments)


#Check all combinaitons of Flags, maybe one can be removed
# Check figures and remove
# Remove impossible values
#traits %>% filter(grepl("#zap", AreaFlag))
write_csv(traits, path = "traits/data_cleaned/PFTC1.2_China_2015_2016_Traits.csv", col_names = TRUE)


### Is this needed???
# calculate large residuals: Area vs. DryMass
LargeResid <- traits %>% filter(!is.na(Leaf_Area_cm2), !is.na(Dry_Mass_g))
fit <- lm(log(Leaf_Area_cm2) ~ log(Dry_Mass_g), data = LargeResid)
LargeResid$resid_Area_Dry <- resid(fit)
LargeResid <- LargeResid %>% select(Date, Site, Elevation, Taxon, Individual_number, Leaf_number, Dry_Mass_g, Leaf_Area_cm2, resid_Area_Dry)

#traits <- traits %>% left_join(LargeResid, by = c("Date", "Site", "Elevation", "Taxon", "Individual_number", "Leaf_number", "Dry_Mass_g", "Leaf_Area_cm2")) %>% mutate(flag = ifelse(abs(resid_Area_Dry) > 1.2, paste(flag, "LargeResid", sep = "_"), NA))
  

