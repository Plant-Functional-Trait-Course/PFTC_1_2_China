#compare data base version, pre and post proof reading

#libraries
library("DBI")
library("dplyr")

#make connections
new <- dbConnect(RMySQL::MySQL(), group = "transplant")
old <- dbConnect(RMySQL::MySQL(), user = "gbsrt", host = "127.0.0.1", password = "b5b5b5", dbname = "oldTransplant")


newcover <- dbGetQuery(new, "select * from turfCommunity")
oldcover <- dbGetQuery(old, "select * from turfCommunity")

oldcover <- oldcover %>% 
  filter(!species == "RTtreat") %>%
 mutate(species = ifelse(species == "Car.L", "Car.spp", species))

summary(newcover)
summary(oldcover)

newcover %>% group_by(year) %>% summarise(n = n())
oldcover %>% group_by(year) %>% summarise(n = n())

newcover %>% 
  group_by(species) %>% 
  summarise(n_new = n()) %>% 
  full_join(
    y = oldcover %>% group_by(species) %>% summarise(n_old = n()), 
    by = "species") %>% 
  print(n = 200)

full_join(
    select(newcover, -cf),
    select(oldcover, -cf), 
    by = c("year", "turfID", "species"),
    suffix = c(".new", ".old")
  ) %>%
  filter(cover.new != cover.old | is.na(cover.new) | is.na(cover.old)) %>%
  arrange(turfID, year)

newcover %>% group_by(year, turfID) %>% summarise(n = n()) %>% tidyr::spread(key = year, value = n) %>% print(n = 200)

oldcover %>% group_by(year, turfID) %>% summarise(n = n()) %>% tidyr::spread(key = year, value = n) %>% print(n = 200)


