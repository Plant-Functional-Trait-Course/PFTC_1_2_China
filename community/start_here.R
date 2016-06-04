#load packages
require("tidyr")
require("plyr")
require("DBI")
library(dplyr)

#source functions
fl <- list.files("community/R/", full.names = TRUE)
sapply(fl, source)

#make database connection
con <- make_connection(username = "gbsrt", password = "b5b5b5", dbname = "transplant")

#load cover data and metadata
cover_thin <- load_comm(con = con)


# make fat table
cover <- cover_thin %>% 
  select(-speciesName) %>%
  spread(key = species, value = cover, fill = 0)


#make meta data
cover_meta <- cover[, 1:which(names(cover) == "year")]

#make turf list
turfs <- cover_meta[!duplicated(cover_meta$turfID),]

#remove meta from cover
cover <- cover[, -(1:which(names(cover) == "year"))]
