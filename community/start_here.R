#load packages
library("tidyr")
library("DBI")# also needs RSQLite installed
library("dplyr")

#source functions 
if(interactive()){#this is needed because if this is called from a markdown doc the path changes
  path <- "community/"
}else{
  path <- ""
}

fl <- list.files(paste0(path, "R/"), full.names = TRUE)
sapply(fl, source)



#make database connection
con <- dbConnect(RSQLite::SQLite(), dbname = paste0(path, "data/transplant.sqlite"))


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

con2 <- src_sqlite(path = paste0(path, "data/transplant.sqlite"), create = FALSE)# need to move all code to dplyr for consistancy
taxa <- tbl(con2, "taxon")
