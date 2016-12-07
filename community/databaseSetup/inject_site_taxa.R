library("DBI")
library("RMySQL")
library("readxl")
library("taxize")

#load csv file
dat <- read.csv ("community/databaseSetup/data/allsites.csv")

#make connection to transplant database

con <- dbConnect(RMySQL::MySQL(), group = "transplant")



#taxa - replace with full info

taxonomy0 <- read_excel("community/databaseSetup/data/Full name and code.xlsx", sheet = "Sheet1")
taxonomy0 <- taxonomy0[, c("oldCode", "newCode", "fullName")]

#keep only correct names
keep <- !is.na(taxonomy0$fullName)
taxonomy <- setNames(taxonomy0[keep, c("newCode", "fullName")], c("species", "speciesName"))

#check for duplicates
any(duplicated(taxonomy$species))
taxonomy[duplicated(taxonomy$species),]

# split authority from name
spNames <- strsplit(taxonomy$speciesName, " ")
nameAuthority <- plyr::ldply(spNames, function(x){
  if(any(grepl("var.", x, fixed = TRUE))){
    speciesName <- paste(x[1:4], collapse = " ")
    authority <- paste(x[-(1:4)], collapse = " ")
  } else {
    speciesName <- paste(x[1:2], collapse = " ")  
    authority <- paste(x[-(1:2)], collapse = " ")
  }
  if(is.na(authority)) authority <- ""
  data.frame(speciesName, authority, stringsAsFactors = FALSE)
})

taxonomy <- cbind(species = taxonomy$species, nameAuthority, stringsAsFactors = FALSE)

#taxize to check names correctly spelt
#get_tsn(searchterm = taxonomy$speciesName)
#gnr <- gnr_resolve(names = taxonomy$speciesName)

#catch any extras (none now)

spp <- names(dat)
meta <-c("DestinationSite", "DestinationBlock", "originPlotID", "TTtreat", "destinationPlotID", "turfID", "RTtreat", "GRtreat", "subPlot", "year", "date", "Measure", "recorder", "moss", "lichen", "litter", "soil", "rock", "totalVascular", "totalBryophytes", "totalLichen", "vegetationHeight", "mossHeight", "litterThickness", "comment"  )
spp <- spp[!spp %in% meta]
extras <- spp[!spp %in% c(taxonomy0$oldCode, taxonomy0$newCode)]

if(length(extras) != 0){
  taxonomy <- rbind(taxonomy, cbind(species = extras, speciesName = extras, authority  = ""))  
  warning("Not found in species list: ", paste(extras, collapse = " "))
}


#add to database
dbWriteTable(con, "taxon", value = as.data.frame(taxonomy), row.names = FALSE, append = TRUE)

dbGetQuery(con, "select * from taxon;")


#sites
sites <- data.frame(siteID = unique(dat$DestinationSite))
dbWriteTable(con, "sites", value = sites, row.names = FALSE, append = TRUE)

#blocks
blocks <- setNames(data.frame(unique(dat[, c("DestinationBlock", "DestinationSite")])), c("blockID", "siteID"))
dbWriteTable(con, "blocks", value = blocks, row.names = FALSE, append = TRUE)

#plots
plots <- setNames(data.frame(unique(dat[, c("destinationPlotID", "DestinationBlock")])), c("plotID", "blockID"))
plots1 <- data.frame(plotID = unique(dat$"originPlotID"), blockID = substr(unique(dat$"originPlotID"), 1, 2))
dbWriteTable(con, "plots", value = rbind(plots, plots1), row.names = FALSE, append = TRUE)

#turfs
turfs <- setNames(data.frame(unique(dat[, c("turfID", "TTtreat", "originPlotID", "destinationPlotID")])), c("turfID", "TTtreat", "originPlotID", "destinationPlotID"))
dbWriteTable(con, "turfs", value = turfs, row.names = FALSE, append = TRUE)

#do taxonomic corrections
source("community/databaseSetup/doCorrections.R")

#import community and environment data
source("community/databaseSetup/importcommunity.r")
import.data(dat, mergedictionary = setNames(taxonomy0[, c("oldCode", "newCode")], c("oldID", "newID")))

dbDisconnect(con)

