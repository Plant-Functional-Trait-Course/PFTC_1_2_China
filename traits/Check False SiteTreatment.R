trait2016 %>% filter(!Project %in% c("SEAN", "OTC", "LOCAL")) %>% mutate(loc1 = substr(Location, 1, 1)) %>% count(loc1, Site, Project) %>% arrange(Project) %>% print(n = 99)

      loc1  Site Project     n
11     A     A       2     1
13     A     M       2     2
18     H     H       6    12
24     A     A Unknown     1



trait2016 %>% 
  filter(grepl("A", Location), Site == "M", Project == "2") %>% 
  select(Full_Envelope_Name, Envelope_Name_Corrected, Taxon, Individual_number, Leaf_number)

trait2016 %>%
  filter(Location == "A", Taxon == "Geranium_pylzowianum")

# create temp directory
dir.create(path = "xxx")

file.list <- list.files(path = "")

AudsFunction <- function(files){
  fl <- file.copy(from = "", to = "", overwrite = TRUE)
  area <- run.ij(set.directory, distance.pixel, low.size)
  return(area)
}

# loop over one file, and delete it
ldply(file.list, AudsFunction)



