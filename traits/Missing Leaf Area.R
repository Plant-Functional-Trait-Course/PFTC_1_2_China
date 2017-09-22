### FINDING ALL THE SCANS
library("tidyverse")
library("lubridate")


### All scans from 2015
AllImages2015 <- dir(path = paste0("/Volumes/My Passport/2015 ChinaDataAllComputers/leaftraitdata"), pattern = "jpeg|jpg", recursive = TRUE, full.names = TRUE)

save(AllImages2015, file = "AllImages2015.Rdata")

FixedNames <- data_frame(full = AllImages2015, x = basename(AllImages2015)) %>% 
                           mutate(x = gsub(" 00\\d\\.", ".", x),
                                  x = gsub("_\\d{8}_000\\d", "", x),
                                  x = gsub(" \\(2\\)", "", x),
                                  x = gsub("\\-NA$", "", x),
                                  x = gsub(" ", "_", x),
                                  x = gsub("-", "_", x))
  

# All file names from newleafarea2015
LA2015 <- leafarea2015 %>% 
  filter(!is.na(LeafArea)) %>%
  separate(col = File_Name, into = c("File_Name", "Area"), sep = "\\.txt\\.") %>% 
  mutate(File_Name = gsub("-", "_", File_Name))

# find scans not in leafarea2015
ExtraScans <- FixedNames %>% anti_join(LA2015, by = c(x = "File_Name")) %>% 
  filter(!grepl("_M\\d", x, ignore.case = TRUE)) %>% 
  mutate(x = gsub("\\d{4,8}_(.*)\\.jpe*g", "\\1", x),
         x = tolower(x))

# Missing Leaf Area in 2015 (from traits_raw)
MissingLeafArea <- traits_raw %>% filter(is.na(Leaf_Area_cm2), year(Date) == 2015) %>% 
  mutate(Taxon = gsub(" ", "_", Taxon)) %>% 
  mutate(FileName = paste(Site, Elevation, Taxon, Individual_number, Leaf_number, sep = "_"), 
         FileName = tolower(FileName))
  
ExpectedMissingScans <- ExtraScans %>% semi_join(MissingLeafArea, by = c(x = "FileName"))
UnknownMissingScans <- ExtraScans %>% anti_join(MissingLeafArea, by = c(x = "FileName"))

# Drop unwanted scans
UnknownMissingScans2 <- UnknownMissingScans %>% 
  filter(!grepl("\\d\\_tr", x)) %>% # remove experimental leaves
  filter(!grepl("^l\\_ac\\_3\\.jpg$", x)) %>% # remove experimental leaves
  filter(!grepl("swertsia|swerfia", x)) %>% # remove all Swertia leaves, need to be done by hand
  filter(!grepl("hemiph", x)) %>% # wrong Ind. nr
  filter(!grepl("juncus\\_h", x)) %>% # lowercase M
  filter(!grepl("euphorbia\\_sp\\#", x)) %>% # hashtag
  filter(!grepl("3850_galium\\_asp", x)) %>% # 
  filter(!grepl("festuoca\\_ovina\\_1\\_4", x)) %>% # empty scan
  filter(!grepl("rubus\\_pinectus\\_1\\_1", x)) %>% # empty scan
  filter(!grepl("arundinana\\_faberi\\_1\\_2", x)) %>% # 007 in leafarea2015
  filter(!grepl("thalictrum\\_javanicum", x)) %>% # 004 in leafarea2015 and wrong Ind nr.
  filter(!grepl("thalictrum\\_javenicum", x)) %>% # 004 in leafarea2015 and wrong Ind nr.
  filter(!grepl("arisima\\_sp", x)) %>% #
  filter(!grepl("gentiana\\_crassulides", x)) %>% # missing Ind nr.
  filter(!grepl("lonicera\\_sp\\#", x)) %>%  # missing Ind nr.
  filter(!grepl("bistorta_macrophyllerm", x)) %>%  # missing Ind nr.
  filter(!grepl("3000_prenanthes_macrophylla", x)) %>%  # missing Ind nr.
  filter(!grepl("3500_parasenecio_palamatisectus", x)) %>%  # missing Ind nr.
  filter(!grepl("^20150823\\.jpg$", x)) %>%  # Unknown file
  filter(!grepl("^a\\_3850$", x)) %>%  # Unknown file
  filter(!grepl("^a\\_3850\\_$", x)) %>%   # Unknown file
  filter(!grepl("^08\\_21\\_001$", x)) %>%   # Unknown file
  filter(!grepl("test\\.jpg", x)) %>%   # Unknown file
  filter(!grepl("halenis_eleptica_2_\\d", x)) # Ind 2 already done. Has not ind nr
  

# add Bistorta, Prenanthes_macrophylla Parasenecio_palamatisectus, with triple scans, empty scans etc.
SomeAdditionalScans <- UnknownMissingScans %>% 
  filter(full %in% c("/Volumes/My Passport/2015 ChinaDataAllComputers/leaftraitdata/All_Images_cleaned-Maitner/Lowland_LeafImages_withoutfolders/20150820-L-3000-Bistorta_macrophyllerm-1-1.jpeg", "/Volumes/My Passport/2015 ChinaDataAllComputers/leaftraitdata/All_Images_cleaned-Maitner/Lowland_LeafImages_withoutfolders/20150820-L-3000-Bistorta_macrophyllerm-1-2.jpeg", "/Volumes/My Passport/2015 ChinaDataAllComputers/leaftraitdata/All_Images_cleaned-Maitner/Lowland_LeafImages_withoutfolders/20150820-L-3000-Bistorta_macrophyllerm-1-3.jpeg", 
  
  "/Volumes/My Passport/2015 ChinaDataAllComputers/leaftraitdata/All_Images_cleaned-Maitner/Elevation_leaf_images_without_folders_cleaned/20150820-L-3000-Prenanthes_macrophylla(cf)-1-1.jpg", "/Volumes/My Passport/2015 ChinaDataAllComputers/leaftraitdata/All_Images_cleaned-Maitner/Elevation_leaf_images_without_folders_cleaned/20150820-L-3000-Prenanthes_macrophylla(cf)-1-2.jpg", "/Volumes/My Passport/2015 ChinaDataAllComputers/leaftraitdata/All_Images_cleaned-Maitner/Elevation_leaf_images_without_folders_cleaned/20150820-L-3000-Prenanthes_macrophylla(cf)-1-3.jpg", "/Volumes/My Passport/2015 ChinaDataAllComputers/leaftraitdata/All_Images_cleaned-Maitner/Elevation_leaf_images_without_folders_cleaned/20150820-L-3000-Prenanthes_macrophylla(cf)-1-4.jpg", "/Volumes/My Passport/2015 ChinaDataAllComputers/leaftraitdata/All_Images_cleaned-Maitner/Elevation_leaf_images_without_folders_cleaned/20150820-L-3000-Prenanthes_macrophylla(cf)-1-5.jpg", 
  
  "/Volumes/My Passport/2015 ChinaDataAllComputers/leaftraitdata/Kina leaf traits-vigdis/Medium/20150820-M-3500-Parasenecio_palamatisectus-1-3/20150820-M-3500-Parasenecio_palamatisectus-1-3 003.jpg"))
    

# Swertia do by hand, but check names. Swerfia 3000: Ind 1-5 2x

# Have to calc Leaf Area
# Combine data sets
ToDo <- ExpectedMissingScans %>% 
  bind_rows(UnknownMissingScans2) %>% 
  bind_rows(SomeAdditionalScans) 

# DO NOT WORK; TO DO BY HAND
# All_Images_cleaned-Maitner/Elevation_leaf_images_without_folders_cleaned/20150820-L-3000-Prenanthes_macrophylla(cf)-1: leaf 1-5
# All_Images_cleaned-Maitner/Lowland_LeafImages_withoutfolders/20150820-L-3000-Bistorta_macrophyllerm-1: Leaf 1-3
# leafscans_0825/20150820-H-4100-Polygonum_macrophyllum-1: Leaf 1-3

# Swerfias -L- in 4 different places
# /Volumes/My Passport/2015 ChinaDataAllComputers/leaftraitdata/All_Images_cleaned-Maitner/Elevation_leaf_images_without_folders_cleaned/20150820-L-3000-Swerfia_macrosperma-1-1.jpeg
# /Volumes/My Passport/2015 ChinaDataAllComputers/leaftraitdata/All_Images/Lowland_LeafImages_withoutfolders/20150820-L-3000-Swerfia_macrosperma-1-1.jpeg
# /Volumes/My Passport/2015 ChinaDataAllComputers/leaftraitdata/All_Images/Lowland_LeafImages_withoutfolders/rescans/swerfia/20150820-L-3000-Swerfia_macrosperma-1-1.jpeg
# /Volumes/My Passport/2015 ChinaDataAllComputers/leaftraitdata/JESSLYN/Leaf Traits SCANS_21-08-15_Jesslyn/20150820-L-3000-Swerfia_macrosperma-1-1.jpeg
# Swertsia -m- 




# All scans form 2016
AllImages2016 <- dir(path = paste0("/Volumes/My Passport/Traits - scans and envelopes/China Leaf Scans 2016"), pattern = "jpeg|jpg", recursive = TRUE, full.names = TRUE)

save(AllImages2016, file = "AllImages2016.Rdata")

# remove duplicates, fix names
AllImages2016 <- AllImages2016 %>% 
  data_frame(full = AllImages2016, x = basename(AllImages2016)) %>% 
  group_by(x) %>% 
  arrange(x) %>% 
  mutate(ID = 1:n()) %>% filter(ID == 1) %>% 
  ungroup(x) %>% 
  mutate(x = gsub("-", "_", x),
         x = gsub(" ", "_", x),
         x = gsub(".jpg|.jpeg", "", x),
         x = gsub("^\\d{4,8}\\_", "", x))


AllImages2016$x <- tolower(AllImages2016$x)
AllImages2016 <- AllImages2016 %>% 
  mutate(x = gsub("rockii", "rockiana", x),
         x = gsub("var._rockiana", "var_rockiana", x),
         x = gsub("_usincans", "_incans", x),
         x = gsub("_husincans", "_incans", x),
         x = gsub("sp\\._", "sp_", x),
         x = gsub("leuconota", "leuconata", x),
         x = gsub("trichomata", "trichotoma", x),
         x = gsub("saussurea_gramina", "saussurea_graminea", x),
         x = gsub("fustidola", "fastidiola", x))


# all files from trait2016
Trait2016 <- trait2016 %>% 
  mutate(FileName = gsub("-", "_", FileName),
         FileName = gsub(" ", "_", FileName),
         FileName = gsub(".jpg|.jpeg", "", FileName),
         FileName = gsub("^\\d{4,8}\\_", "", FileName),
         FileName = gsub("leuconota", "leuconata", FileName),
         FileName = tolower(FileName))

#Scans not in trait; appr. 500 more that do not match, but all misspellings
AllImages2016 %>% anti_join(Trait2016, by = c(x = "FileName")) %>% select(x) %>% pn


# Missing Leaf Area in 2016 (from traits): need to be scanned again
traits_raw %>% filter(is.na(Leaf_Area_cm2), year(Date) == 2016)
