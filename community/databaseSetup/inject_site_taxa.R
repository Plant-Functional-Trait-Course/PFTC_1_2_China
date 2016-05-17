library("DBI")
library("RMySQL")
source(file = "community/make_connection.R")


#load csv file
dat <- read.csv ("community/databaseSetup/data/allsites.csv")

#make connection to transplant database

con <- make_connection(username = "gbsrt", password = "b5b5b5", dbname = "transplant")




#taxa - replace with full info

taxonomy <- read_excel("community/databaseSetup/data/Full name and code.xlsx", sheet = "Sheet1")
taxonomy <- taxonomy[, c("oldCode", "newCode", "full name")]

 #discard duplicate taxa (which have oldCode != newCode or singletons) #NB multiple changes to new code will fail
keep <-
  taxonomy$oldCode == taxonomy$newCode |
  !(duplicated(taxonomy$newCode) |
      duplicated(taxonomy$newCode, fromLast = TRUE))

taxonomy$newCode[keep]

#
taxonomy <- setNames(taxonomy[keep, c("newCode", "full name")], c("species", "speciesName"))

#catch any extras (none now)

spp <- names(dat)
meta <-c("DestinationSite", "DestinationBlock", "originPlotID", "TTtreat", "destinationPlotID", "turfID", "RTtreat", "GRtreat", "subPlot", "year", "date", "Measure", "recorder", "moss", "lichen", "litter", "soil", "rock", "totalVascular", "totalBryophytes", "totalLichen", "vegetationHeight", "mossHeight", "litterThickness", "comment"  )
spp <- spp[!spp %in% meta]
extras <- spp[!spp %in% taxonomy$species]

if(length(extras) != 0){
  taxonomy <- rbind(taxonomy, cbind(species = extras, speciesName = extras))  
  warning("Not found in species list: ", paste(extras, collapse = " "))
}
#add to database
dbWriteTable(con, "taxon", value = as.data.frame(taxonomy), row.names = FALSE, append = TRUE)

dbGetQuery(con, "select * from taxon;")


#sites
sites <- data.frame(siteID = unique(dat$DestinationSite))
#sqlAppendTable(con, "sites", values = sites, row.names = FALSE)# why doesn't this work
dbWriteTable(con, "sites", value = sites, row.names = FALSE, append = TRUE)

#blocks
blocks <- setNames(data.frame(unique(dat[, c("DestinationBlock", "DestinationSite")])), c("blockID", "siteID"))
dbWriteTable(con, "blocks", value = blocks, row.names = FALSE, append = TRUE)

#plots
plots <- setNames(data.frame(unique(dat[, c("destinationPlotID", "DestinationBlock")])), c("plotID", "blockID"))
dbWriteTable(con, "plots", value = plots, row.names = FALSE, append = TRUE)

#turfs
turfs <- setNames(data.frame(unique(dat[, c("turfID", "TTtreat", "originPlotID", "destinationPlotID")])), c("turfID", "TTtreat", "originPlotID", "destinationPlotID"))
dbWriteTable(con, "turfs", value = turfs, row.names = FALSE, append = TRUE)

#import community and environment data
import.data(dat)

dbDisconnect(con)

