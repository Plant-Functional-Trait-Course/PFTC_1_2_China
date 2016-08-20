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
global <- read.table("community/databaseSetup/data/globalCorrections.csv", sep = ",", header  = TRUE)
global$new <- trimws(global$new)
#check names in taxonomy stopifnot
setdiff(global$old, taxonomy$speciesName)
stopifnot(all(global$old %in% taxonomy$speciesName))
setdiff(global$new, taxonomy$speciesName)
stopifnot((global$new %in% taxonomy$speciesName))



#convert names to code
global2 <- merge(global, taxonomy[, 1:2], by.x = "old", by.y = "speciesName")
global2 <- setNames(global2[, -1], c("new", "old"))
global2 <- merge(global, taxonomy[, 1:2], by.x = "new", by.y = "speciesName")
global2 <- setNames(global2[, -1], c("old", "new"))
global2

combinePA <- function(x){
  x[is.na(x)] <- 0
  (x[, 1] | x[, 2]) + 0
}

#add old to new
for(i in 1:nrow(global)){ 
  dat[dat$Measure == "cover%", global$new[i]] <- rowSums(dat[dat$Measure == "cover%", unlist(global[i, ])], na.rm = TRUE) # sum covers
  dat[dat$Measure == "Presence", global$new[i]] <- combinePA(dat[dat$Measure == "Presence", unlist(global[i, ])], na.rm = TRUE) # combine presences
}
#remove old
dat <- dat[, !names(dat) %in% global$old]



#local edits - apply to particular sites/turfs/years
local <- read.table("community/databaseSetup/data/local.csv")



#special edits (manually as hopefully few)