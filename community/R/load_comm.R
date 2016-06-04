load_comm <- function(con, cover = TRUE, OTC = TRUE, transplant = TRUE){
  require(tidyr)
  require(plyr)
  
    # 
  # if(cover){
  #   comm <- turfCommunity
  # }else{
  #   comm <-subTurfCommunity
  # }  
  
  ##cover data
  coverQ <- "SELECT `sites`.`siteID` AS originSiteID, `blocks`.`blockID` AS originBlockID, `plots`.`plotID` AS originPlotID, `turfs`.`turfID`, `plots_1`.`plotID` AS destPlotID, `blocks_1`.`blockID` AS destBlockID, `sites_1`.`siteID` AS destSiteID, `turfs`.`TTtreat`, `turfCommunity`.`year`, `turfCommunity`.`species`, `turfCommunity`.`cover` 
FROM `transplant`.`blocks` AS `blocks`, `transplant`.`sites` AS `sites`, `transplant`.`plots` AS `plots`, `transplant`.`turfs` AS `turfs`, `transplant`.`turfCommunity` AS `turfCommunity`, `transplant`.`plots` AS `plots_1`, `transplant`.`blocks` AS `blocks_1`, `transplant`.`sites` AS `sites_1` WHERE `blocks`.`siteID` = `sites`.`siteID` AND `plots`.`blockID` = `blocks`.`blockID` AND `turfs`.`originPlotID` = `plots`.`plotID` AND `turfCommunity`.`turfID` = `turfs`.`turfID` AND `turfs`.`destinationPlotID` = `plots_1`.`plotID` AND `blocks_1`.`siteID` = `sites_1`.`siteID` AND `plots_1`.`blockID` = `blocks_1`.`blockID`;"
  
  cover.thin <- dbGetQuery(con, coverQ)
  
  #recode TTtreat
  cover.thin$TTtreat <- mapvalues(cover.thin$TTtreat, from = c("C", "O", "1", "2", "3", "4", "OTC" )  , to  = c("control", "local", "warm1", "cool1", "warm3", "cool3", "OTC"))
  cover.thin$TTtreat <- factor(cover.thin$TTtreat, levels = c("control", "local", "warm1", "cool1", "warm3", "cool3", "OTC"))

  
  # make fat table
  cover <- spread(cover.thin, key = species, value = cover, fill = 0)
  
  #make meta data
  cover.meta <- cover[, 1:which(names(cover) == "year")]

  #make turf list
  turfs <- cover.meta[!duplicated(cover.meta$turfID),]
  turfs <- turfs[order(turfs$turfID),]
  
  
  list(meta = meta, spp = spp, turfs = turfs)
}