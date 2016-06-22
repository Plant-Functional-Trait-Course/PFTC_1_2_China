#load packages
library("tidyr")
library("plyr")
library("DBI")# also needs RMySQL installed
library("dplyr")

#source functions
fl <- list.files("community/R/", full.names = TRUE)
sapply(fl, source)

#make database connection
# uses username, password, host etc (including sock) information from file .my.cnf that should be in your root directory. 
#See http://www.inside-r.org/packages/cran/rmysql/docs/MySQL
#for Macs see here http://stackoverflow.com/questions/10757169/mysql-my-cnf-location
# put my.cnf file here: /etc
# My .my.cnf looks like this (without the comments)
# [transplant]
# user = gbsrt
# password = b5b5b5
# host = localhost
# port = 3306
# database = transplant
# socket = /Applications/MAMP/tmp/mysql/mysql.sock # (MAMP users only)


con <- dbConnect(RMySQL::MySQL(), group = "transplant")


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
