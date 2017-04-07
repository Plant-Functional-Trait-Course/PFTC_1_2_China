#' Load transplant community 
#'
#' @param con connection to database
#' @param cover logical cover or subturf frequency (only cover implemented)
#' 


load_subturfcomm <- function(con, cover = TRUE) {
  require("tidyverse")
  require("DBI")
  
  ##cover data
  subturfQ <-
    "SELECT sites.siteID AS originSiteID, blocks.blockID AS originBlockID, plots.plotID AS originPlotID, turfs.turfID, plots_1.plotID AS destPlotID, blocks_1.blockID AS destBlockID, sites_1.siteID AS destSiteID, turfs.TTtreat, subTurfCommunity.year, subTurfCommunity.subTurf, subTurfCommunity.species, subTurfCommunity.adult, taxon.speciesName
  FROM blocks, sites, plots, turfs, subTurfCommunity, plots AS plots_1, blocks AS blocks_1, sites AS sites_1,  taxon
  WHERE blocks.siteID = sites.siteID AND plots.blockID = blocks.blockID AND turfs.originPlotID = plots.plotID AND subTurfCommunity.turfID = turfs.turfID AND turfs.destinationPlotID = plots_1.plotID AND blocks_1.siteID = sites_1.siteID AND plots_1.blockID = blocks_1.blockID AND subTurfCommunity.species = taxon.species;"
  
  subturf.thin <- dbGetQuery(con, subturfQ)
  
  #recode TTtreat
  subturf.thin$TTtreat <- plyr::mapvalues(
      subturf.thin$TTtreat,
      from = c("C", "O", "1", "2", "3", "4", "OTC")  ,
      to  = c("control", "local", "warm1", "cool1", "warm3", "cool3", "OTC")
    )
  subturf.thin$TTtreat <- factor(
      subturf.thin$TTtreat,
      levels = c("control", "local", "warm1", "cool1", "warm3", "cool3", "OTC")
    )
  subturf.thin$adult <- as.numeric(subturf.thin$adult)
  subturf.thin$originSiteID <- factor(subturf.thin$originSiteID, levels = c("H", "A", "M", "L"))
  subturf.thin$destSiteID <- factor(subturf.thin$destSiteID, levels = c("H", "A", "M", "L"))
  subturf.thin %>% as_tibble()
}