#implement corrections
#could be applied at several stages in pipeline from excel files to analysis. 
#want to have database corrected, so don't need to always remember to apply corrections
#complicated by the need to add co-occuring synonyms.

#best options are to either 
# - apply to huge CSV file before injecting into database
# - or to run update queries on database.

#former choice is probably easier to implement (because of the complication)
#database will need to be recompiled with each (hopefully rare) set of corrections.

#need to add new species to taxon sheet by hand. Code will check.


#Designed to be run through inject_site_taxa
############
#global edits - apply to all sites/turfs/years
global <- read.table("community/databaseSetup/data/globalCorrections.csv", sep = ",", header  = TRUE, stringsAsFactors = FALSE)
global$new <- trimws(global$new)
#check names in taxonomy stopifnot
setdiff(global$old, taxonomy$speciesName)
setdiff(global$new, taxonomy$speciesName)

stopifnot(all(c(global$old, global$new) %in% taxonomy$speciesName))


#convert names to code
global$old <- mapvalues(global$old, from = taxonomy$speciesName, to = taxonomy$species, warn_missing = FALSE)
global$new <- mapvalues(global$new, from = taxonomy$speciesName, to = taxonomy$species, warn_missing = FALSE)

stopifnot(all(global$old %in% names(dat)))
(newTaxa <- setdiff(global$new, names(dat)))
#add extra columns to dat
newTaxa <- as.data.frame(setNames(as.list(rep(NA, length(newTaxa))), newTaxa))
#need to insert new taxa before moss column
moss <- which(names(dat) == "moss")
dat <- cbind(dat[, 1:(moss - 1)], newTaxa, dat[, moss:ncol(dat)])

combinePA <- function(x){
  x[is.na(x)] <- 0
  (x[, 1] | x[, 2]) + 0
}

#add old to new
for(i in 1:nrow(global)){ 
  dat[dat$Measure == "cover%", global$new[i]] <- rowSums(dat[dat$Measure == "cover%", unlist(global[i, ])], na.rm = TRUE) # sum covers
  dat[dat$Measure == "Presence", global$new[i]] <- combinePA(dat[dat$Measure == "Presence", unlist(global[i, ])]) # combine presences
}
#remove old
dat <- dat[, !names(dat) %in% global$old]



#local edits - apply to particular sites/turfs/years
local <- read.table("community/databaseSetup/data/localDatacorrections_plots_China.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
setdiff(local$turfID, dat$turfID)
local$turfID <- trimws(local$turfID)#zap trailing space

#check names
setdiff(local$old, taxonomy$speciesName)
setdiff(local$new, taxonomy$speciesName)
local[local == "Potentilla stenophylla"] <- "Potentilla stenophylla var. emergens"
local[local == "Ligularia subspicata"] <- "Ligularia pleurocaulis"

stopifnot(all(c(local$old, local$new) %in% c("", taxonomy$speciesName)))

#convert names to code
local$old <- mapvalues(local$old, from = taxonomy$speciesName, to = taxonomy$species, warn_missing = FALSE)
local$new <- mapvalues(local$new, from = taxonomy$speciesName, to = taxonomy$species, warn_missing = FALSE)

merge(local, dat, by.x = c("turfID", "year", "site"), by.y = c("turfID", "year", "DestinationSite"), all.x = TRUE)%>%
  filter(is.na(DestinationBlock))%>%select(1:9)

##split off specials
local$special[local$special == ""] <- NA
special <- local[!is.na(local$special), ]
local <- local[is.na(local$special), 1:5]

#correct turfID
local$turfID[local$turfID == "M5-2" & local$year =="2015" & local$old == "Ana.fla"] <- "L5-2"

#correct years of duplicates
local$year[local$turfID == "L6-OTC" & local$year == 2014 & local$old == "Aju.dec"][2] <- 2015
local$year[local$turfID == "L6-2" & local$year == 2014 & local$old == "Aju.dec"][2] <- 2015

#fix species code to corrections made by global corrections
local$old[local$old == "Par.pus"] <- "Par.spp"

#remove duplicate corrections
local %>% ungroup() %>% group_by(turfID, year, old) %>% mutate(n = n()) %>% filter (n >1) %>% arrange(turfID)# duplicates
local <- unique(local)

##make sure corrections are in correct order so that
# b -> c
# a -> b
#rather than
# a -> b
# b -> c (ie a & b become c)

notChains <- local %>% group_by(turfID, year) %>% filter(xor(new %in% old, old %in% new)) %>% arrange(!old  %in% new)

local <- local %>% group_by(turfID, year) %>% filter(!xor(new %in% old, old %in% new))
local <- rbind(local, notChains)

## find taxon swaps and break
# a -> b
# b -> a
#becomes
#a  -> X
#b -> a
#X -> b

swaps <- local %>% group_by(turfID, year) %>% filter(new %in% old & old %in% new) %>% arrange(turfID, year, old)
local <- local %>% group_by(turfID, year) %>% filter(!(new %in% old & old %in% new)) # without swaps

swapsA <- swaps[seq(1, nrow(swaps), by = 2), ]
swaps2 <- swaps[seq(2, nrow(swaps), by = 2), ]

swaps1 <- swapsA %>% mutate(new = "dummyTaxon")
swaps3 <- swapsA %>% mutate(old = "dummyTaxon")

swaps <- rbind(swaps1, swaps2, swaps3)

local <- rbind(local, swaps)

#add dummy taxon
dat$dummyTaxon <- NA


#loop over rows in data
for(i in 1:nrow(local)) {
  target <- dat$year == local$year[i] &
    dat$turfID == local$turfID[i] #&
   # dat$DestinationSite == local$site[i]
  if (sum(target) == 0) {
    warning(local[i,], "not located")
    break()
  } else{
    old <- dat[target, ]
    old <- old[old$Measure == "cover%", local$old[i]]
    if(old == 0 | is.na(old)) {
      warning(local[i,], "old species not present") 
    }
    #cover
    dat[target & dat$Measure == "cover%", local$new[i]] <- sum(c(dat[target & dat$Measure == "cover%", local$new[i]], old), na.rm = TRUE)   
    dat[target & dat$Measure == "cover%", local$old[i]] <- 0
    #pa
    dat[target & dat$Measure == "Presence", local$new[i]] <- combinePA(dat[target & dat$Measure == "Presence", unlist(local[i, c("old", "new")])])
    dat[target & dat$Measure == "Presence", local$old[i]] <- 0
  }
}

#remove dummy taxon
dat$dummyTaxon <- NULL


#special edits (manually as hopefully few)
special$special <- mapvalues(special$special, from = taxonomy$speciesName, to = taxonomy$species, warn_missing = FALSE)
special$special2 <- mapvalues(special$special2, from = taxonomy$speciesName, to = taxonomy$species, warn_missing = FALSE)
special

for(i in 1:nrow(special)) {
  target <- dat$year == special$year[i] &
    dat$turfID == special$turfID[i] &
    dat$DestinationSite == special$site[i]
  if (sum(target) == 0) {
    warning(special[i,], "not located")
    break()
  } else{
    old <- dat[target, ]
    old <- old[old$Measure == "cover%", special$old[i]]
    if(old == 0 | is.na(old)) {
      warning(special[i,], " old species not present") 
    }
    if(old != special$cover1[i] + special$cover2[i]) {
      stop(special[i,], " new sum cover not equal to old cover") 
    }
    
    #cover
    dat[target & dat$Measure == "cover%", special$old[i]] <- 0
    dat[target & dat$Measure == "cover%", special$special[i]] <- special$cover1[i]
    dat[target & dat$Measure == "cover%", special$special2[i]] <- special$cover2[i]    
    #pa
    warning("PA undefined")
    #dat[target & dat$Measure == "presence", local$new[i]] <- combinePA(dat[target & dat$Measure == "presence", unlist(local[i, c("old", "new")])])
    #dat[target & dat$Measure == "presence", local$old[i]] <- 0
  }
}

##manual presence absence corrections for aju.dec/pru.his/cli.pol L1-C
target <- dat$Measure == "Presence" & dat$turfID == "L1-C" & dat$year == 2014

dat$Cli.pol[target] <- dat$Aju.dec[target]# Cli.pol get all Aju.dec subplots
dat$Pru.his[target] <- dat$Pru.his[dat$Measure == "Presence" & dat$turfID == "L1-C" & dat$year == 2013]# Pru.his gets previous years subplots 

dat$Aju.dec[target] <- NA#Aju.dec is wiped

