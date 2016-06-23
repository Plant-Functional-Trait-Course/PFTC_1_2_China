#######################################################
###########   China Leaf Trait  2015      ##############
###########                               ##############
#######################################################
library(lubridate)

#####Data by Site
trait.site<- read.csv(file="traits/data/ChinaLeafTraitData20151102Site.csv", stringsAsFactors = FALSE)
trait.site$Taxon_TNRS_corrected <- gsub("\xa0", "", trait.site$Taxon_TNRS_corrected)
head(trait.site)
str(trait.site)

trait.site$Date <- ymd(trait.site$Date) # convert date
trait.site$Leaf_Thickness_Ave_mm <- rowMeans(trait.site[,c("Leaf_Thickness_1_mm", "Leaf_Thickness_2_mm", "Leaf_Thickness_3_mm","Leaf_Thickness_4_mm","Leaf_Thickness_5_mm","Leaf_Thickness_6_mm")], na.rm = TRUE) # calculate average leaf thickness

# Change Bistorta to Polygonum
trait.site$Taxon_TNRS_corrected[trait.site$Taxon_TNRS_corrected=="Bistorta vivipara"] <- "Polygonum viviparum"

# Replace missing site values from elevation
index <- as.vector(unique(trait.site$Elevation))
values <- c("L", "M", "A", "H")
trait.site[trait.site$Site == "", "Site"] <- values[match(trait.site$Elevation, index)][trait.site$Site==""]


# Get an idea how many species are measured in common with trait data set
taxa <- dbGetQuery(con, "SELECT * FROM taxon")

sp.comparison <- plyr::ldply(unique(trait.site$Taxon_TNRS_corrected), function(x){
  code <- taxa$species[grep(x, taxa$speciesName, ignore.case = TRUE)]
  if(length(code) > 1) warning("Too many", code)
  data.frame(traitName = x, commCode = ifelse(length(code) == 1, code, NA))
})

ggplot(data.frame(noccur = colSums(cover > 0), inTraits = names(cover) %in% sp.comparison$commCode), aes(x = inTraits, y = noccur)) + geom_boxplot()


# calculate nr traits measured
# Number of traits measured per site and species
traits <- trait.site[,c(8:9,11,19:21)]

by(trait.site, list(trait.site$Site, trait.site$Taxon_TNRS_corrected), function(x){
  ...
})

# Community species list not in trait data:
...

# How many species have no traits? Which species have more than 3 or 5 trait values.
# Number of indivivduals and leaves per site and species
by(trait.site, trait.site$Site, function(x){
  table(x$Taxon_TNRS_corrected, x$Leaf_Number)
  
})


