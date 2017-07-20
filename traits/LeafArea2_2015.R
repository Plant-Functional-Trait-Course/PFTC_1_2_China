#### IMPORT LEAF AREA 2 CALCULATION FROM 2015 LEAVES ####
library(tidyverse)
library(lubridate)
library(readr)

leafarea2015 <- read_csv(file = "traits/data/Leaf.Area2015-20170717.csv")

# black line unsure which area, double scan!!!
# comment: not Rumex, check this!
# check cut, folded, eaten, black line leaves

Newleafarea2015 <- leafarea2015 %>% 
  select(-X1) %>% 
  rename(Taxon = X5) %>% 
  filter(!is.na(LeafArea)) %>% # remove files without area
  # fix comment for Arisima_parvum at M; for Ind 2, change Leaf nr to 1.1 to merge single scanned leaflets
  mutate(comment = ifelse(grepl("Arisima", File_Name) & grepl("-M-", File_Name) & LeafArea < 14.7, "dirt", comment)) %>% 
  mutate(File_Name = ifelse(grepl("Arisima", File_Name) & grepl("-M-", File_Name), 
                            gsub("\\.\\d", "", File_Name), 
                            File_Name)) %>% 
  # remove dirt, black lines and double and empty scans
  filter(!grepl("dirt|black line|empty|did not work|part of leaf too white, two leaves on scan?|double", comment)) %>% 
  # split File name and Area 1, 2, 3
  separate(col = File_Name, into = c("File_Name", "Area"), sep = "\\.txt\\.") %>% 
  # summarize areas from different part of  leaves
  group_by(File_Name, Taxon) %>%
  summarise(LeafArea2 = sum(LeafArea), allComments = paste(unique(comment), collapse = " _ ")) %>% 
  ungroup() %>% 
  filter(!grepl("Gentiana_yunnanensis-\\d\\.", File_Name)) %>% 
  mutate(File_Name = gsub("(.*)\\.jpe*g$", "\\1", File_Name)) %>% # remove jpg or jpeg
  mutate(File_Name = gsub("3850_Potentilla-stenophylla", "3850-Potentilla_stenophylla", File_Name)) %>%
  mutate(File_Name = gsub("jpg", "", File_Name)) %>% # zap jpg without dot
  mutate(File_Name = if_else(
    !grepl("\\d-\\d$", File_Name) & grepl("Halenis_eleptica|Gentiana_crassulides", File_Name), 
    gsub("(.*)(-\\d$)", "\\1-2\\2",  File_Name),
    File_Name)) %>% # Fix missing Ind nr for Halensis e.
  mutate(File_Name = if_else(grepl("\\d-\\d$", File_Name), File_Name, gsub("(.*)(-\\d$)", "\\1-1\\2",  File_Name))) %>% # add missing Individual_numbers
  mutate(File_Name = gsub("20150820-m-3500-Gentiana_crassuloides-S-1-1", "20150820-m-3500-Gentiana_crassuloides-1-1", File_Name)) %>%  # remove -S in 20150820-m-3500-Gentiana_crassuloides-S-1-1 Gentiana crassuloides
  separate(col = File_Name, into = c("Date", "Site", "Elevation", "Species", "Individual_Number", "Leaf_Number"), sep = "-") %>% 
  # fixing wrong variables
  filter(!(Elevation == 300 & Species == "Hemiphragma_heterophyllum")) %>% # duplicates with wrong file names
  filter(!(Elevation == 3580 & Species == "Juncus_himalescens")) %>% # additional files without traits
  mutate(
    Site = gsub("m", "M", Site), # replace m with M
    # fix variables
    Elevation = gsub("^300$", "3000", Elevation), # replace 300 with 3000, all are Site L
    Taxon = gsub("Neottianthe cucullata var.camcola", "Neottianthe cucullata Var.Camcola", Taxon),
    Taxon = gsub("Youngia prattii", "Youngia racemifera", Taxon),
    Taxon = gsub("Carex nubigena", "Carex nubige", Taxon),
    Taxon = gsub("\xa0", "", Taxon),
    Elevation = as.numeric(Elevation),
    Leaf_Number = gsub("1 004", "1", Leaf_Number), # Thalictrum javanicum
    Leaf_Number = gsub("2 007", "2", Leaf_Number) # Arundinaria faberi
    ) %>% 
  mutate(Individual_Number = ifelse(Site == "L" & Taxon == "Thalictrum javanicum" & Individual_Number == "1", "1.1", Individual_Number)) %>% 
  select(-Species, -Date) %>%  # remove columns before merging
  # Dealing with duplicates: sort data by leaf area, give them id
  group_by(Site, Elevation, Taxon, Individual_Number, Leaf_Number) %>% 
  arrange(LeafArea2) %>% 
  mutate(LeafID = 1:n())

# No leaf area calculated
# Leaf area caclculation did not work: Swertia macrosperma, Prenanthes macrophylla

# Check where the scans are for these species!
# c("Polygonum macrophyllum", "Prenanthes macrophylla", "Swertia macrosperma", "Berberis dictyophylla", "Geranium donianum", "Anemone obtusiloba", "Chamaesium viridiflorum", "Codonopsis foetens subsp. Nervosa", "Codonopsis foetens subsp. nervosa", "Gentiana trichotoma", "Pedicularis roylei", "Pedicularis trichoglossa", "Kobresia cercostachys", "Saussurea stella")

# Check large Anaphalis flavescense leaves!

setdiff(trait2015$Taxon_FoC_corrected, Newleafarea2015$Taxon)
setdiff(Newleafarea2015$Taxon, trait2015$Taxon_FoC_corrected)

  