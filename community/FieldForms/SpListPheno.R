# Quiery for sp table: 6 sites, only 2009 and 2013 data, only herbs and grasses, excluding RTC


con <- make_connection(username = "gbsrt", password = "b5b5b5", dbname = "transplant")


##not moss data
phenQ <-"SELECT sites_1.siteID, blocks.blockID as origblock, blocks_1.blockID as destblock, turfs.TTtreat, turfs.turfID, turfCommunity.year, turfCommunity.species, turfCommunity.cover
FROM taxon INNER JOIN (sites AS sites_1 INNER JOIN (sites INNER JOIN ((blocks AS blocks_1 INNER JOIN plots AS plots_1 ON blocks_1.blockID = plots_1.blockID) INNER JOIN (((blocks INNER JOIN plots ON blocks.blockID = plots.blockID) INNER JOIN turfs ON plots.plotID = turfs.originPlotID) INNER JOIN turfCommunity ON turfs.turfID = turfCommunity.turfID) ON plots_1.plotID = turfs.destinationPlotID) ON sites.siteID = blocks.siteID) ON sites_1.siteID = blocks_1.siteID) ON taxon.species = turfCommunity.species;"
phen <- dbGetQuery(con, phenQ)

head(phen)
audsfun <- function(site, year = 2015) {
  phen <- subset(phen, phen$siteID == site)
  splist <- by(phen, phen$turfID, function(turf) {
    sp2012 <- turf$species[turf$year == 2012]
    sp2015 <- turf$species[turf$year == year]
    sp2015 <- setdiff(sp2015, sp2012)
    sp2012 <- paste("*", sp2012, "*")
    sp <- c(as.character(turf$turfID[1]), sp2012, sp2015, rep("", 3))
  })
  splist <- unlist(sapply(splist, I))
  df <- data.frame(
    splist,
    A = "",
    B = "",
    C = "",
    D = ""
  )
  df
}

h <- audsfun("H")
a <- audsfun("A")
m<-audsfun("M")
l<-audsfun("L")

save(h, a, m, l, file = "audsSpeciesLists.Rdata")


#########control cover by site

phenCont <- subset(phen, TTtreat %in% c("C", "O"))
phenCont$pa <-phenCont$cover > 0

sites <- list("H", "A", "M", "L")
audslist <- llply(sites, function(site){print(site)
  xtabs(pa ~ species + year, data = subset(phenCont, siteID == site))
})
names(audslist) <- sites

save(audslist, file = "audsInControl.Rdata")
