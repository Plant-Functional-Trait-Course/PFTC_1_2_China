#Implement corrections
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
global <- read_csv("community/databaseSetup/data/globalCorrections.csv")
global$new <- trimws(global$new)
#check names in taxonomy stopifnot
setdiff(global$old, taxonomy$speciesName)
setdiff(global$new, taxonomy$speciesName)

assert_that(all(c(global$old, global$new) %in% taxonomy$speciesName))


#convert names to code
global$old <- plyr::mapvalues(global$old, from = taxonomy$speciesName, to = taxonomy$species, warn_missing = FALSE)
global$new <- plyr::mapvalues(global$new, from = taxonomy$speciesName, to = taxonomy$species, warn_missing = FALSE)

assert_that(all(global$old %in% names(dat)))
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


#don't flag global edits (equivalent to taxonomic table merges)

message("Swe.mac not deleted")#?

#### local edits - apply to particular sites/turfs/years
local <- read_csv("community/databaseSetup/data/localDatacorrections_plots_China.csv", comment = "#")
local <- local[!is.na(local$new) | !is.na(local$special), ]#remove extra rows
setdiff(local$turfID, dat$turfID)
local$turfID <- trimws(local$turfID)#zap trailing space
assert_that(all(local$turfID %in% dat$turfID))

#check names
setdiff(local$old, c(taxonomy$speciesName, taxonomy$species))
setdiff(local$new, c(taxonomy$speciesName, taxonomy$species))
local[local == "Kob.pgy"] <- "Kob.pyg"
local[local == "pol.cya"] <- "Pol.cya"
local[local == "Potentilla stenophylla"] <- "Potentilla stenophylla var. emergens"
local[local == "Ligularia subspicata"] <- "Ligularia pleurocaulis"
local[local == "Juncus leucanthus"] <- "Juncus leucomelas"
local[local == "Pru.his"] <- "Pru.vul"
local[local == "Prunella hispida"] <- "Pru.vul"
local[local == "Smi.jap"] <- "Mai.hen"
local[local == "Hal.cor"] <- "Hal.ell"
all_local <- c(local$old, local$new)
all_local <- all_local[!is.na(all_local)]

assert_that(all(all_local %in% c("", taxonomy$speciesName, taxonomy$species)))

#convert names to code
local$old <- plyr::mapvalues(local$old, from = taxonomy$speciesName, to = taxonomy$species, warn_missing = FALSE)
local$new <- plyr::mapvalues(local$new, from = taxonomy$speciesName, to = taxonomy$species, warn_missing = FALSE)

full_join(local, dat, by = c("turfID" = "turfID", "year" = "year", "site" = "DestinationSite"))%>%
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

#check for reversed corrections, chaining
inner_join(rownames_to_column(local), rownames_to_column(local), by = c("turfID" = "turfID", "year" = "year", "new" = "old"), suffix = c(".v1", ".v2"))

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

if(nrow(swaps) > 0){
  swapsA <- swaps[seq(1, nrow(swaps), by = 2), ]
  swaps2 <- swaps[seq(2, nrow(swaps), by = 2), ]
  
  swaps1 <- swapsA %>% mutate(new = "dummyTaxon")
  swaps3 <- swapsA %>% mutate(old = "dummyTaxon")
  
  swaps <- rbind(swaps1, swaps2, swaps3)
  
  local <- rbind(local, swaps)
}

#number of cases
local %>% ungroup() %>% count(old, new)


#add dummy taxon
dat$dummyTaxon <- NA


#loop over rows in data
for(i in 1:nrow(local)) {print(i)
  target <- dat$year == local$year[i] & dat$turfID == local$turfID[i] #&
   # dat$DestinationSite == local$site[i]
  if (!any(target)) {
    warning(local[i,], "not located - local")
    next()
  } else {
    old <- dat[target, ]
    old <- old[old$Measure == "cover%", local$old[i]]
    if(length(old) > 1){
      old <- old[1]# may be duplicate because of extra excel files with corrections
    }
    if(old == 0 | is.na(old)) {
      warning(local[i,], "old species not present - local") 
    }
    #cover
    dat[target & dat$Measure == "cover%", local$new[i]][1] <- sum(dat[target & dat$Measure == "cover%", local$new[i]][1], old, na.rm = TRUE)   
    dat[target & dat$Measure == "cover%", local$old[i]] <- 0
    #browser(condition = "zero result", expr = {!dat[target & dat$Measure == "cover%", local$new[i]][1] > 0})
    #pa
    dat[target & dat$Measure == "Presence", local$new[i]][1:25] <- combinePA(dat[target & dat$Measure == "Presence", unlist(local[i, c("old", "new")])][1:25, ])
    dat[target & dat$Measure == "Presence", local$old[i]] <- 0
  }
}

#remove dummy taxon
dat$dummyTaxon <- NULL

##local flags
flags <- local %>% 
  filter(new != "dummyTaxon") %>% 
  rowwise() %>% 
  do({
    row <- .
    dat %>% 
      select_("year", "turfID", "subPlot", row$new) %>% 
      filter(year == row$year, turfID == row$turfID) %>% 
      gather(key = species, value = plant, -year, -turfID, -subPlot) %>% 
      filter(plant > 0) %>% 
      mutate(
        flag = "Yan's local correction"
        ) %>% 
      select(year, turfID, subPlot, species, flag) 
  }) %>% 
  ungroup() %>% 
  distinct()


#special edits (manually as hopefully few)
special$special <- plyr::mapvalues(special$special, from = taxonomy$speciesName, to = taxonomy$species, warn_missing = FALSE)
special$special2 <- plyr::mapvalues(special$special2, from = taxonomy$speciesName, to = taxonomy$species, warn_missing = FALSE)
special <- special %>% filter(!is.na(old))#need to know what is being corrected!!!


for(i in 1:nrow(special)) {
  target <- dat$year == special$year[i] &
    dat$turfID == special$turfID[i] &
    dat$DestinationSite == special$site[i]
  if (sum(target) == 0) {
    warning(special[i,], "not located - special")
    next()
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



##manual presence absence corrections for aju.dec/pru.vul/cli.pol L1-C
target <- dat$Measure == "Presence" & dat$turfID == "L1-C" & dat$year == 2014

dat$Cli.pol[target] <- dat$Aju.dec[target]# Cli.pol get all Aju.dec subplots
dat$Pru.vul[target] <- dat$Pru.vul[dat$Measure == "Presence" & dat$turfID == "L1-C" & dat$year == 2013]# Pru.vul gets previous years subplots 

dat$Aju.dec[target] <- NA#Aju.dec is wiped

#manual flags
flags <- flags %>% bind_rows(
  crossing(year = 2014, turfID = "L1-C", species = c("Cli.pol", "Pru.vul"), subPlot = c(1:25, "cover%")))  


####corrections to percent values 
perc_subplot <- read_csv("community/databaseSetup/data/cover_correction.csv", col_types = "iccddc")

#zap blank rows
perc_subplot <- perc_subplot[!is.na(perc_subplot$year), ]

assert_that(all(perc_subplot$`#turf` %in% dat$turfID))#check no bad turfs
assert_that(all(perc_subplot$species %in%  taxonomy$species))#check no bad turfs

dat$comment[is.na(dat$comment)] <- "" #cannot have NAs for logical test below

perc <- perc_subplot[!is.na(perc_subplot$`should be`), ]

for(i in 1:nrow(perc)){
  dat[dat$year == perc$year[i] &
        dat$turfID == perc$`#turf`[i] &
        dat$Measure == "cover%" & 
        dat$comment != "Yans correction", perc$species[i]] <- perc$`should be`[i] # don't 'correct' corrections
}

flags <- flags %>% bind_rows(
  perc %>% 
    select(year, turfID = `#turf`, species, old = `old cover`) %>%
    mutate(
      subPlot = "cover%",
      flag = if_else(is.na(old), "imputation", "corrected cover")
    ) %>%
    select(-old)
)

####additional subplots
subplot <- perc_subplot[!is.na(perc_subplot$`updated #subplot`), ]

for(i in 1:nrow(subplot)){
  dat[dat$year == subplot$year[i] &
        dat$turfID == subplot$`#turf`[i] &
        dat$Measure == "Presence" & 
        dat$comment != "Yans correction" &
        as.numeric(dat$subPlot) %in% as.numeric(strsplit(subplot$`updated #subplot`[i], ",")[[1]]) &
        dat$comment != "correction", subplot$species[i]] <- 1
}

#extrasubplot flags
flags <- bind_rows(
  flags,
  subplot %>%
    rowwise() %>%
    do({
      cbind(., data_frame(subPlot = str_split(.$`updated #subplot`, ",")[[1]]))
    }) %>%
    select(year, turfID = `#turf`, subPlot, species) %>%
    mutate(flag = "extra subplot") %>%
    ungroup()
)


flags %>% group_by(year, turfID, species, subPlot) %>% filter(n() > 1)

flags <- flags %>% 
  group_by(year, turfID, species, subPlot) %>% 
  summarise(flag = paste(flag, collapse = "/"))
