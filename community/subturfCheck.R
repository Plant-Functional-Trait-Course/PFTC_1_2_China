#load packages
library("tidyr")
library("plyr")
library("DBI")# also needs RMySQL installed
library("dplyr")
library(ggplot2)

#source functions
source("community/R/load_subturfcomm.R")
source("community/R/load_comm.R")

#make database connection
con <- dbConnect(RMySQL::MySQL(), group = "transplant")


#load cover data and metadata
subturf_thin <- load_subturfcomm(con = con)
#  spread(subturf.thin, key = "subTurf", value = "adult", fill = 0)
cover_thin <-load_comm(con = con)

#check subturf and cover contain same species
subturf_freq <- subturf_thin %>% 
  group_by(turfID, species, year) %>%
  summarise(n = n())

#merge cover and subturf
mergedCoverFreq <- merge(cover_thin, subturf_freq, all = TRUE)

mergedCoverFreq %>% filter(is.na(cover)) %>% select(turfID, year, species, n)
mergedCoverFreq %>% filter(is.na(n)) %>% select(turfID, year, species, cover)

#subturf stability

subturfChange <- function(dat, turfID, start, end){
  #extract turf & years
  subturf <- dat[dat$turfID == turfID & (dat$year == start | dat$year == end), ] 
  
  #check bothyears present
  if(!all(c(start, end) %in% subturf$year)){
    warning("no data found for one year in ", turfID)
    return()
  }
  subturf <- spread(subturf, key = year, value = adult, fill = 0)
  
  subturf$change <- "absent"
  
  #find s & e = persistence
  subturf$change[subturf[, as.character(start)] & subturf[, as.character(end)]] <- "persist"
  # s & !e = extinction
  subturf$change[subturf[, as.character(start)] & !subturf[, as.character(end)]] <- "extinct"  
  # !s & e = recruitment
  subturf$change[!subturf[, as.character(start)] & subturf[, as.character(end)]] <- "recruit"  
    
  subturf$x <- (subturf$subTurf - 1) %% 5 + 1
  subturf$y <- (subturf$subTurf - 1) %/% 5 + 1

  ggplot(subturf, aes(x = x, y = y, fill = change)) + 
    geom_raster() +
    facet_wrap(~species) +
    labs(title = paste(turfID, start, "-", end), x = NULL, y = NULL)
}

subturfChange(dat = subturf_thin, turfID = "L1-C", start = 2012, end = 2013)

sapply(unique(subturf_thin$turfID[subturf_thin$TTtreat == "control"]), function(turfID) {
  x11()  
  print(turfID)
  g <- subturfChange(subturf_thin, turfID = turfID, start = 2012, end = 2013)
  print(g)
  })