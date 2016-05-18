# Quiery for sp table: 6 sites, only 2009 and 2013 data, only herbs and grasses, excluding RTC


db<-"O:\\Aud\\Projects work\\Postdoc -Bergen\\Seedclim data\\seedclim_2014-5-20 - Copy.mdb" #edit this line with correct location
con<-odbcConnectAccess2007(db)

##moss data
phenQ<-"SELECT sites.siteID, blocks.blockID, blocks_1.blockID, turfs.TTtreat, turfs.turfID, new_TurfCommunity.Year, new_TurfCommunity.species, new_TurfCommunity.cover, sites.Temperature_level, sites.Precipitation_level, taxon.functionalGroup
FROM taxon INNER JOIN (sites AS sites_1 INNER JOIN (sites INNER JOIN ((blocks AS blocks_1 INNER JOIN plots AS plots_1 ON blocks_1.blockID = plots_1.blockID) INNER JOIN (((blocks INNER JOIN plots ON blocks.blockID = plots.blockID) INNER JOIN turfs ON plots.plotID = turfs.originPlotID) INNER JOIN new_TurfCommunity ON turfs.turfID = new_TurfCommunity.turfID) ON plots_1.plotID = turfs.destinationPlotID) ON sites.siteID = blocks.siteID) ON sites_1.siteID = blocks_1.siteID) ON taxon.species = new_TurfCommunity.species
WHERE ((Not (turfs.TTtreat)='') AND ((new_TurfCommunity.Year)=2009 Or (new_TurfCommunity.Year)=2013) AND ((sites.Temperature_level)<3) AND ((sites.Precipitation_level)>1) AND (Not (taxon.functionalGroup)='woody' And Not (taxon.functionalGroup)='pteridophyte'))
ORDER BY taxon.functionalGroup;"
phen<- sqlQuery(con,phenQ)



phen <- subset(phen, phen$siteID.1=="Skjellingahaugen") # Hogsete, Rambera, Veskre, Gudmedalen, Lavisdalen, Skjellingahaugen
remove <- c("NID.seedling", "NID.gram", "Eup.sp", "Rhi.min", "Hyp.mac")
phen <- phen[!(phen$species %in% remove),]
splist<-by(phen,phen$turfID, function(turf){
  sp2009<-turf$species[turf$Year==2009]
  sp2013<-turf$species[turf$Year==2013]
  sp2013<-setdiff(sp2013,sp2009)
  sp2009<-paste("*",sp2009,"*")
  sp<-c(as.character(turf$turfID[1]),sp2009, sp2013,rep("",3))
  
})
splist<-unlist(sapply(splist,I))
df<-data.frame(splist, A="", B="", C="", D="")
kable(df, row.names=FALSE)


