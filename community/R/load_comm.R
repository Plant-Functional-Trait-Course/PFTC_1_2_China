#' Load transplant community 
#'
#' @param con connection to database
#' @param cover logical cover or subturf frequency (only cover implemented)
#' 


load_comm <- function(con, cover = TRUE) {
  require("tidyr")
  require("DBI")
  
  ##cover data
  coverQ <-
    "SELECT `sites`.`siteID` AS originSiteID, `blocks`.`blockID` AS originBlockID, `plots`.`plotID` AS originPlotID, `turfs`.`turfID`, `plots_1`.`plotID` AS destPlotID, `blocks_1`.`blockID` AS destBlockID, `sites_1`.`siteID` AS destSiteID, `turfs`.`TTtreat`, `turfCommunity`.`year`, `turfCommunity`.`species`, `turfCommunity`.`cover`, `taxon`.`speciesName`
  FROM `blocks`, `sites`, `plots`, `turfs`, `turfCommunity`, `plots` AS `plots_1`, `blocks` AS `blocks_1`, `sites` AS `sites_1`, `taxon`
  WHERE `blocks`.`siteID` = `sites`.`siteID` AND `plots`.`blockID` = `blocks`.`blockID` AND `turfs`.`originPlotID` = `plots`.`plotID` AND `turfCommunity`.`turfID` = `turfs`.`turfID` AND `turfs`.`destinationPlotID` = `plots_1`.`plotID` AND `blocks_1`.`siteID` = `sites_1`.`siteID` AND `plots_1`.`blockID` = `blocks_1`.`blockID` AND `turfCommunity`.`species` = `taxon`.`species`;"
  
  cover.thin <- dbGetQuery(con, coverQ)
  
  #recode TTtreat
  cover.thin$TTtreat <- plyr::mapvalues(
      cover.thin$TTtreat,
      from = c("C", "O", "1", "2", "3", "4", "OTC")  ,
      to  = c("control", "local", "warm1", "cool1", "warm3", "cool3", "OTC")
    )
  cover.thin$TTtreat <- factor(
      cover.thin$TTtreat,
      levels = c("control", "local", "warm1", "cool1", "warm3", "cool3", "OTC")
    )
  cover.thin$originSiteID <- factor(cover.thin$originSiteID, levels = c("L", "M", "A", "H"))
  cover.thin$destSiteID <- factor(cover.thin$destSiteID, levels = c("L", "M", "A", "H"))
  
  cover.thin
}