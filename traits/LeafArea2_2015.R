#### IMPORT LEAF AREA 2 CALCULATION FROM 2015 LEAVES ####
library(tidyverse)
library(lubridate)
library(readr)

leafarea2015 <- read_csv(file = "traits/data/Leaf.Area2015-20170714.csv")

Newleafarea2015 <- leafarea2015 %>% 
  select(-X1) %>% 
  rename(Taxon = X5) %>% 
  filter(!is.na(LeafArea)) %>% # remove files without area
  filter(!grepl("black|dirt|empty|double|brown|yellow|folded", comment)) %>% # remove black lines, dirt and douple scans
  # split File name and Area 1, 2, 3
  separate(col = File_Name, into = c("File_Name", "Area"), sep = "\\.txt\\.") %>% 
  # summarize areas from different part of  leaves
  group_by(File_Name, Taxon) %>%
  summarise(LeafArea2 = sum(LeafArea), allComments = paste(comment, collapse = " _ ")) %>% 
  ungroup() %>% 
  mutate(File_Name = gsub("(.*)\\.jpe*g$", "\\1", File_Name)) %>% # remove jpg or jpeg
  mutate(File_Name = gsub("3850_Potentilla-stenophylla", "3850-Potentilla_stenophylla", File_Name)) %>%
  mutate(File_Name = gsub("jpg", "", File_Name)) %>% # zap jpg without dot
  mutate(File_Name = if_else(grepl("\\d-\\d$", File_Name), File_Name, gsub("(.*)(-\\d$)", "\\1-1\\2",  File_Name))) %>% # add missing Individual_numbers
  mutate(File_Name = gsub("20150820-m-3500-Gentiana_crassuloides-S-1-1", "20150820-m-3500-Gentiana_crassuloides-1-1", File_Name)) %>%  # remove -S in 20150820-m-3500-Gentiana_crassuloides-S-1-1 Gentiana crassuloides
  separate(col = File_Name, into = c("Date", "Site", "Elevation", "Species", "Individual_Number", "Leaf_Number"), sep = "-") %>% 
  mutate(
    Site = gsub("m", "M", Site), # replace m with M
    Elevation = gsub("300", "3000", Elevation), # replace 300 with 3000, all are Site L
    Elevation = gsub("3580", "3500", Elevation), # replace 3580 with 3500, only occurs at site M
    # fix species names
    Taxon = gsub("Neottianthe cucullata var.camcola", "Neottianthe cucullata Var.Camcola", Taxon),
    Taxon = gsub("Carex nubigena", "Carex nubige", Taxon),
    Taxon = gsub("\xa0", " ", Taxon),
    Elevation = as.numeric(Elevation)
    ) %>% 
  select(-Species, -Date) # remove columns before merging

# Leaf area caclculation did not work: Swertia macrosperma, Prenanthes macrophylla


setdiff(trait2015$Taxon_FoC_corrected, Newleafarea2015$Taxon)
setdiff(Newleafarea2015$Taxon, trait2015$Taxon_FoC_corrected)
  
  