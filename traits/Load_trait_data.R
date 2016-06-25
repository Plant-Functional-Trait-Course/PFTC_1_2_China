#######################################################
###########   China Leaf Trait  2015     ##############
###########                              ##############
#######################################################
library("lubridate")
library("ggplot2")
library("plyr")
library("dplyr")
library("tidyr")

#### DATA BY SITE ####
# read in file
trait.site<- read.csv(file="traits/data/ChinaLeafTraitData20160623Site.csv", stringsAsFactors = FALSE)
trait.site$Taxon_TNRS_corrected <- gsub("\xa0", "", trait.site$Taxon_TNRS_corrected)
head(trait.site)
str(trait.site)

trait.site$Date <- ymd(trait.site$Date) # convert date
trait.site$Leaf_Thickness_Ave_mm <- rowMeans(trait.site[,c("Leaf_Thickness_1_mm", "Leaf_Thickness_2_mm", "Leaf_Thickness_3_mm","Leaf_Thickness_4_mm","Leaf_Thickness_5_mm","Leaf_Thickness_6_mm")], na.rm = TRUE) # calculate average leaf thickness

# Change Bistorta to Polygonum
trait.site$Taxon_TNRS_corrected[trait.site$Taxon_TNRS_corrected=="Bistorta vivipara"] <- "Polygonum viviparum"


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


# MISSING TRAITS
# Number of traits measured per site and species
# 6 traits measured: Leaf_Area_cm2, Wet_Mass_g, Dry_Mass_g, Leaf_Thickness_Ave_mm, SLA_cm2.g, LDMC
missing.trait <- trait.site %>%
  select(Site, Taxon_TNRS_corrected, SLA_cm2.g, Leaf_Thickness_Ave_mm, Leaf_Area_cm2, LDMC, Wet_Mass_g, Dry_Mass_g, Individual_Number) %>%
  group_by(Site, Taxon_TNRS_corrected) %>%
  mutate(NoPlants = n_distinct(Individual_Number), nLeaves = n()) %>%
  select(-Individual_Number) %>%
  gather(key = "trait", value = value, -Site, -Taxon_TNRS_corrected, -nLeaves, -NoPlants) %>%
  group_by(Site, Taxon_TNRS_corrected, trait, NoPlants, nLeaves) %>%
  summarise (missing = sum(is.na(value))) %>%
 # filter(missing > 0) %>% # including this line gives all incomplete cases, if line is excluded you get all species
  spread(key = trait, value = missing, fill = 0)

# make csv file
write.csv(missing.trait, file = "Missing.Trait.csv", row.names = FALSE)


# Number of indivivduals and leaves per site and species
trait.sum <- trait.site %>%
  filter(!is.na(SLA_cm2.g)) %>%
  group_by(Site, Taxon_TNRS_corrected, Individual_Number) %>%
  summarise(n = n()) %>%
  group_by(Site, Taxon_TNRS_corrected) %>%
  mutate(N = n()) %>%
  
  spread(key = Individual_Number, value = n) %>%
  as.data.frame()

