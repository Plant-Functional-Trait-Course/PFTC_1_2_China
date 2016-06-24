#######################################################
###########   China Leaf Trait  2015     ##############
###########                              ##############
#######################################################
library(lubridate); library(ggplot2)

#### DATA BY SITE ####
trait.site<- read.csv(file="traits/data/ChinaLeafTraitData20160623Site.csv", stringsAsFactors = FALSE)
trait.site$Taxon_TNRS_corrected <- gsub("\xa0", "", trait.site$Taxon_TNRS_corrected)
head(trait.site)
str(trait.site)

trait.site$Date <- ymd(trait.site$Date) # convert date
trait.site$Leaf_Thickness_Ave_mm <- rowMeans(trait.site[,c("Leaf_Thickness_1_mm", "Leaf_Thickness_2_mm", "Leaf_Thickness_3_mm","Leaf_Thickness_4_mm","Leaf_Thickness_5_mm","Leaf_Thickness_6_mm")], na.rm = TRUE) # calculate average leaf thickness

# Change Bistorta to Polygonum
trait.site$Taxon_TNRS_corrected[trait.site$Taxon_TNRS_corrected=="Bistorta vivipara"] <- "Polygonum viviparum"

# Replace missing site values from elevation (not needed anymore)
index <- as.vector(unique(trait.site$Elevation))
values <- c("L", "M", "A", "H")
trait.site[trait.site$Site == "", "Site"] <- values[match(trait.site$Elevation, index)][trait.site$Site==""]



#### SUMMARY ####
# Replace strange values to calculate summary
trait.site$Leaf_Number <- gsub("4_(2)", "6", trait.site$Leaf_Number, fixed=TRUE)
trait.site$Leaf_Number <- gsub("5_(2)", "7", trait.site$Leaf_Number, fixed=TRUE)

trait.site$Individual_Number[trait.site$Individual_Number=="1.1"] <- 4
trait.site$Individual_Number[trait.site$Individual_Number=="2.1"] <- 5
trait.site$Individual_Number[trait.site$Individual_Number=="3.1"] <- 6
trait.site$Individual_Number[trait.site$Individual_Number=="1.2"] <- 7
trait.site$Individual_Number[trait.site$Individual_Number=="2.2"] <- 8
trait.site$Individual_Number[trait.site$Individual_Number=="3.2"] <- 9
trait.site$Individual_Number[trait.site$Individual_Number=="4.2"] <- 10
trait.site$Individual_Number[trait.site$Individual_Number=="5.2"] <- 11

# MISSING SPECIES FROM COMMUNITY DATA
# Get community data form data base
taxa <- dbGetQuery(con, "SELECT * FROM taxon")

# Get an idea how many species are measured in common with trait data set
sp.comparison <- plyr::ldply(unique(trait.site$Taxon_TNRS_corrected), function(x){
  code <- taxa$species[grep(x, taxa$speciesName, ignore.case = TRUE)]
  if(length(code) > 1) warning("Too many", code)
  data.frame(traitName = x, commCode = ifelse(length(code) == 1, code, NA))
})

ggplot(data.frame(noccur = colSums(cover > 0), inTraits = names(cover) %in% sp.comparison$commCode), aes(x = inTraits, y = noccur)) + geom_boxplot()

# How many species (from community data) have no traits? 
...


# INCOMPLETE TRAITS
# Number of traits measured per site and species
# 6 traits measured: Leaf_Area_cm2, Wet_Mass_g, Dry_Mass_g, Leaf_Thickness_Ave_mm, SLA_cm2.g, LDMC
traits <- trait.site[,c(4,8:9,11,19:21)]
traits.incomplete <- traits[!complete.cases(traits),]
unique(traits.incomplete$Taxon_TNRS_corrected) # from 52 species not complete trait measures


# Which species have more than 3 or 5 trait values.
# Number of indivivduals and leaves per site and species
by(trait.site, trait.site$Site, function(x){
  table(x$Taxon_TNRS_corrected, x$Individual_Number)
  
})


