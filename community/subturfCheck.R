#load packages
library("tidyr")
library("plyr")
library("DBI")# also needs RMySQL installed
library("dplyr")
library(ggplot2)

#source functions
source("community/R/load_subturfcomm.R")

#make database connection
con <- dbConnect(RMySQL::MySQL(), group = "transplant")


#load cover data and metadata
subturf_thin <- load_subturfcomm(con = con)
#  spread(subturf.thin, key = "subTurf", value = "adult", fill = 0)


#subturf stability

subturfChange <- function(dat, turfID, start, end){
  #extract turf & years
  subturf <- dat[dat$turfID == turfID & (dat$year == start | dat$year == end), ] %>%
    spread(key = year, value = adult, fill = 0)
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
