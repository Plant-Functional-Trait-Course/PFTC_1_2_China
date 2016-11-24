##2015 data for students

source("community/start_here.R")

#only 2015
cover_thin <- cover_thin %>% filter(year == 2015)

save(cover_thin, file = "cover_thin_20161124.Rdata")

#missing taxa by importance
cover_thin %>% filter(!speciesName %in% traits$TNRS_Corrected_Plant_species) %>% group_by(speciesName) %>% summarise(n = n(), sum = sum(cover)) %>% arrange(desc(n)) %>% print(n =100)

#proportion cover captured by trait data
cover_thin %>% group_by(turfID) %>% mutate(scover = sum(cover)) %>% filter(speciesName %in% traits$TNRS_Corrected_Plant_species) %>% mutate(got = sum(cover)) %>% mutate(prop = got/scover * 100)   %>% summarise(prop = mean(prop)) %>% arrange(prop) %>% as.data.frame()

#test specimen turf
cover_thin %>% filter(turfID == "M4-1") %>% mutate(got = speciesName %in% traits$TNRS_Corrected_Plant_species) %>% select(speciesName, cover, got)

