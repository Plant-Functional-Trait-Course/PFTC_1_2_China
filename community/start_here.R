#load packages
library("tidyverse")
library("DBI")# also needs RSQLite installed


fl <- list.files("community/R/", full.names = TRUE)
sapply(fl, source)
path <- "community/"

## ---- load_community

if(!exists("path")) {
  path <- ""
}

#make database connection
#con <- dbConnect(RSQLite::SQLite(), dbname = paste0(path, "data/transplant.sqlite"))
con <- src_sqlite(path = paste0(path, "data/transplant.sqlite"), create = FALSE)# need to move all code to dplyr for consistancy

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

#save(cover, cover_meta, cover_thin, file = "cover.RData")

#get taxonomy table

taxa <- tbl(con, "taxon") %>%
  collect()
