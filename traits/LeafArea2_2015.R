#### IMPORT LEAF AREA 2 CALCULATION FROM 2015 LEAVES ####

library(lubridate)
library(readr)

leafarea2015 <- read_csv(file = "~/Desktop/Leaf.Area2015.csv")

Newleafarea2015 <- leafarea2015 %>% 
  select(-X1) %>% 
  rename(Taxon = X5) %>% 
  filter(!is.na(LeafArea)) %>% # remove strange files
  filter(!grepl("black|dirt|empty|double", comment)) %>% # remove black lines, dirt and douple scans
  # split File name and Area 1, 2, 3
  separate(col = File_Name, into = c("File_Name", "Area"), sep = "\\.txt\\.") %>% 
  # summarize areas 
  group_by(File_Name, Taxon) %>%
  summarise(LeafArea2 = sum(LeafArea), allComments = paste(comment, collapse = " _ ")) %>% 
  ungroup() %>% 
  mutate(File_Name = gsub("(.*)\\.jpe*g$", "\\1", File_Name)) %>% # remove jpg or jpeg
  mutate(File_Name = gsub("3850_Potentilla-stenophylla", "3850-Potentilla_stenophylla", File_Name)) %>%
  mutate(File_Name = gsub("jpg", "", File_Name)) %>% # zap jpg without dot
  mutate(File_Name = if_else(grepl("\\d-\\d$", File_Name), File_Name, gsub("(.*)(-\\d$)", "\\1-1\\2",  File_Name))) %>% # add missing Individual_numbers
  separate(col = File_Name, into = c("Date", "Site", "Elevation", "Species", "Individual_number", "Leaf_number"), sep = "-") %>% 
  mutate(Elevation = as.numeric(Elevation))