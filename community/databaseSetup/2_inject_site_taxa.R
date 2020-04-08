#### Build sqlite database from seedclimstructure.txt
### Populate with data
### do corrections

library("DBI")
library("RSQLite")
library("readxl")
library("taxize")
library("tidyverse")
library("assertthat")
library("readr")

#compile all data into one csv file
#source("community/databaseSetup/1_xls_to_csv.R")#uncomment to re-compile excel sheets

#function to add data to database - padding for missing columns
dbPadWriteTable <- function(conn, table, value, row.names = FALSE, append = TRUE, ...){
  #get extra columns from DB
  allCols <- dbGetQuery(con, paste("select * from", table, "limit 0;"))
  value <- bind_rows(allCols, value)
  
  #add to database
  dbWriteTable(con, table, value = value, row.names = row.names, append = append, ...)
}


#load csv file made by 1_xls_to_csv.R
dat <- read_csv("community/databaseSetup/data/allsites.csv", guess_max = 20000)

#fix ".." error
dat <- dat %>% mutate(
  litterThickness = gsub("\\.\\.", "\\.",litterThickness),
  litterThickness = as.numeric(litterThickness)
  )

# make database
if(file.exists("community/data/transplant.sqlite")){
  file.remove("community/data/transplant.sqlite")
}
con <- dbConnect(SQLite(), dbname = "community/data/transplant.sqlite")

#set up structure (may need editing into SQLite dialect)

setup <- readChar("community/databaseSetup/seedclimstructure.txt", nchar = 100000)
sapply(paste("CREATE", strsplit(setup, "CREATE")[[1]][-(1:2)]), dbExecute, conn = con)

dbListTables(con)



#make connection to transplant database




#taxa - replace with full info

taxonomy0 <- read_csv("community/databaseSetup/data/transplant_taxonomy.csv")
taxonomy0 <- taxonomy0[, names(taxonomy0) != ""]#zap blank columns

#keep only correct names
taxonomy <- taxonomy0 %>%
  filter(!is.na(fullName), is.na(keep)) %>% # drop blank row and unwanted duplicate taxa
  select(-keep, -oldCode) %>%
  rename(species = newCode, speciesName = fullName, family = Family) %>%
  mutate(family = trimws(family))

#check for duplicates
assert_that(!any(duplicated(taxonomy$species)))
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
  data_frame(speciesName, authority)
})

taxonomy <- bind_cols(select(taxonomy, -speciesName), nameAuthority)

#taxize to check names correctly spelt
#get_tsn(searchterm = taxonomy$speciesName)
#gnr <- gnr_resolve(names = taxonomy$speciesName)

#catch any extras (none now)

spp <- names(dat)
meta <-c("DestinationSite", "DestinationBlock", "originPlotID", "TTtreat", "destinationPlotID", "turfID", "RTtreat", "GRtreat", "subPlot", "year", "date", "Measure", "recorder", "moss", "lichen", "litter", "soil", "rock", "totalVascular", "totalBryophytes", "totalLichen", "vegetationHeight", "mossHeight", "litterThickness", "comment"  )
spp <- spp[!spp %in% meta]
extras <- spp[!spp %in% c(taxonomy0$oldCode, taxonomy0$newCode)]

if(length(extras) != 0){
  stop("Species not found in species list: ", paste(extras, collapse = " "))
}

#add to database
dbPadWriteTable(con, table = "taxon", value = taxonomy)

dbGetQuery(con, "select * from taxon;")

rm(spp, meta)

#sites
sites <- data.frame(siteID = unique(dat$DestinationSite))
dbPadWriteTable(con, table = "sites", value = sites)

#blocks
blocks <- setNames(data.frame(unique(dat[, c("DestinationBlock", "DestinationSite")])), c("blockID", "siteID"))
dbPadWriteTable(con, "blocks", value = blocks)

#plots
plots <- setNames(data.frame(unique(dat[, c("destinationPlotID", "DestinationBlock")])), c("plotID", "blockID"))
plots1 <- data.frame(plotID = unique(dat$"originPlotID"), blockID = substr(unique(dat$"originPlotID"), 1, 2))
dbPadWriteTable(con, "plots", value = unique(rbind(plots, plots1)))

#turfs
turfs <- setNames(data.frame(unique(dat[, c("turfID", "TTtreat", "originPlotID", "destinationPlotID")])), c("turfID", "TTtreat", "originPlotID", "destinationPlotID"))
dbPadWriteTable(con, "turfs", value = turfs)

#do taxonomic corrections
source("community/databaseSetup/R/doCorrections.R")

#import community and environment data
source("community/databaseSetup/R/importcommunity.r")
import.data(dat, mergedictionary = select(taxonomy0, oldID = oldCode, newID = newCode), flags = flags)

dbDisconnect(con)

## upload to dropbox

upload <- readline("type 'yes' to upload file to dropbox for distribution")
if(grepl("yes", upload, ignore.case = TRUE)){
  rdrop2::drop_upload(
    file = "community/data/transplant.sqlite", 
    dest = "transplant/communityDatabase/"
    )
  
}

