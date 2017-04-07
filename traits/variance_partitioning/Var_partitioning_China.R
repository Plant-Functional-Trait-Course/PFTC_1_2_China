###########################################
# 4-4-17
# Lorah Patterson - Variance partitioning of 2016 China leaf trait data

library(tidyverse)
library(taxize)
library(lme4)
library(varComp)
library(jsonlite)
library(ape)
library(BIEN)
library(nlme)

# Load 2015 and 2016 China trait data files and merge them using trait_2017_analysis.R in transplant github repository.
source("traits/trait_2017_analysis.R")

### Subset data to only contain trait data collected in 2016 (because as of April 2017, 2015 trait data needs further error correction) ###
traits$year <- year(traits$Date) # make a column containing the year
varpart2016 <- subset(traits, traits$year=="2016") # subset so that only 2016 trait data remains

### Get a column with names of genera from trait data ###
specgen <- strsplit(varpart2016$Taxon," ") # split the specific epithet into genus and species
specgen$genus <- as.character(sapply(specgen, head, 1)) # get only the genera data out so that the next step to extract plant family is quicker and matches easier will less NAs
varpart2016$genus <- specgen$genus # add genus column to trait data
 
### Get family and order from genus using BIEN ###
taxspecgen<-BIEN_taxonomy_genus(genus = specgen$genus)
taxspecgen2 <- taxspecgen %>% select(order, scrubbed_family, scrubbed_genus) # select order, family, genus columns only
taxspecgen2$genus <- taxspecgen2$scrubbed_genus # rename genus column
taxspecgen2$family <- taxspecgen2$scrubbed_family # rename family column
taxspecgen3 <- taxspecgen2 %>% select(order,family,genus) # select relevant columns
taxspecgen4 <- distinct(taxspecgen3) # Remove duplicate rows
# The results of this specify two families for Carex and Athyrium. Delete the incorrect rows. CAUTION: You may have to modify this if using different data.
taxspecgen5 <- subset(taxspecgen4, family!="Athyriaceae" & family!="Unknown")

### Join trait data to BIEN taxonomic data ###
varpart2016join <- left_join(taxspecgen5, varpart2016, by = "genus")
# There are still some species naming errors in here, which make it seem like the join resulted in lost data. Will be fixed in the future.

### Change letters representing sites (L,M,H,A) to real names (Lower, Middle, High, Alpine) ###
levels(varpart2016join$Site) <- c(levels(varpart2016join$Site), "lower",
                                 "middle", "high", "alpine")

varpart2016join$Site[varpart2016join$Site == "L"] <- "lower"
varpart2016join$Site[varpart2016join$Site == "M"] <- "middle"
varpart2016join$Site[varpart2016join$Site == "H"] <- "high"
varpart2016join$Site[varpart2016join$Site == "A"] <- "alpine"

### Rename column name "taxon" to "species" ###
varpart2016join$species <- varpart2016join$Taxon


### Clean data errors. ###
varpart2016join$LDMC[varpart2016join$Wet_Mass_g <= varpart2016join$Dry_Mass_g] <- NA # when there is an error that wet mass is less than or equals dry mass, change the LDMC value to NA so that it is not considered in the analysis

varpart2016join$SLA_cm2_g[varpart2016join$Wet_Mass_g <= varpart2016join$Dry_Mass_g] <- NA # when there is an error that wet mass is less than or equals dry mass, change the SLA value to NA so that it is not considered in the analysis

varpart2016join$SLA_cm2_g[varpart2016join$SLA_cm2_g >500] <- NA # when there is an error that SLA is greater than 500, change the SLA value to NA so that it is not considered in the analysis

varpart2016join$SLA_cm2_g[varpart2016join$SLA_cm2_g <5] <- NA # when there is an error that SLA is less than 5, change the SLA value to NA so that it is not considered in the analysis



### If necessary, remove gymnosperms and equisetum from data set so that we are only analyzing data for angiosperms ###
# varpart2016filter <- filter(varpart2016join, varpart2016join$family!= "Pinaceae", varpart2016join$family!="Cupressaceae", varpart2016join$family!= "Equisetaceae")



### Subset by only those leaves collected in project=LOCAL (i.e. gradient outside the fence, grazed) or project=C (control plots within the fence, ungrazed) ###
varpart <- subset(varpart2016join, Project=="LOCAL" | Project=="C")

varpartLocal <- subset(varpart2016join, Project=="LOCAL") # only leaves outside fence, grazed
varpartControl <- subset(varpart2016join, Project=="C") # only leaves inside fence, ungrazed
##############################################################
### lme and varcomp analysis ###

#bootvchinatraitsSLA <- boot(data=varpart, vchinatraitsSLA, R=5, statistic=)

### Variance partitioning by taxonomic level: order, family, genus, species, within species ###
vchinatraitsSLA <- varcomp(lme(SLA_cm2_g~1, random=~1|order/family/genus/species, data=varpart, na.action = na.omit), 1)
vchinatraitsArea <- varcomp(lme(Leaf_Area_cm2~1, random=~1|order/family/genus/species, data=varpart, na.action = na.omit), 1)
vchinatraitsThick <- varcomp(lme(Leaf_Thickness_Ave_mm~1, random=~1|order/family/genus/species, data=varpart, na.action = na.omit), 1)
vchinatraitsLDMC <- varcomp(lme(LDMC~1, random=~1|order/family/genus/species, data=varpart, na.action = na.omit), 1)

# show results
vchinatraitsSLA
vchinatraitsArea
vchinatraitsThick
vchinatraitsLDMC

### Variance partitioning of ONLY project=LOCAL leaves (i.e. gradient outside the fence, grazed) then ONLY project=C (control plots within the fence, ungrazed) to see if there is any difference ###
vchinatraitsSLALocal <- varcomp(lme(SLA_cm2_g~1, random=~1|order/family/genus/species, data=varpartLocal, na.action = na.omit), 1)
vchinatraitsAreaLocal <- varcomp(lme(Leaf_Area_cm2~1, random=~1|order/family/genus/species, data=varpartLocal, na.action = na.omit), 1)
vchinatraitsThickLocal <- varcomp(lme(Leaf_Thickness_Ave_mm~1, random=~1|order/family/genus/species, data=varpartLocal, na.action = na.omit), 1)
vchinatraitsLDMCLocal <- varcomp(lme(LDMC~1, random=~1|order/family/genus/species, data=varpartLocal, na.action = na.omit), 1)
# show results
vchinatraitsSLALocal
vchinatraitsAreaLocal
vchinatraitsThickLocal
vchinatraitsLDMCLocal

vchinatraitsSLAControl <- varcomp(lme(SLA_cm2_g~1, random=~1|order/family/genus/species, data=varpartControl, na.action = na.omit), 1)
vchinatraitsAreaControl <- varcomp(lme(Leaf_Area_cm2~1, random=~1|order/family/genus/species, data=varpartControl, na.action = na.omit), 1)
vchinatraitsThickControl <- varcomp(lme(Leaf_Thickness_Ave_mm~1, random=~1|order/family/genus/species, data=varpartControl, na.action = na.omit), 1)
vchinatraitsLDMCControl <- varcomp(lme(LDMC~1, random=~1|order/family/genus/species, data=varpartControl, na.action = na.omit), 1)
# show results
vchinatraitsSLAControl
vchinatraitsAreaControl
vchinatraitsThickControl
vchinatraitsLDMCControl

#####################
### Variance partitioning by spatial level ###
vchinatraitsSiteSLA <- varcomp(lme(SLA_cm2.g~1, random=~1|Site, data=chinatraitsfinal, na.action = na.omit), 1)

vchinatraitsSiteArea <- varcomp(lme(Leaf_Area_cm2~1, random=~1|Site, data=chinatraitsfinal, na.action = na.omit), 1)

vchinatraitsSiteThick <- varcomp(lme(Leaf_Thickness_Ave_mm~1, random=~1|Site, data=chinatraitsfinal, na.action = na.omit), 1)

vchinatraitsSiteLDMC <- varcomp(lme(LDMC~1, random=~1|Site, data=chinatraitsfinal, na.action = na.omit), 1)

# show results
vchinatraitsSiteSLA
vchinatraitsSiteArea
vchinatraitsSiteThick
vchinatraitsSiteLDMC

#####################################################
### PLOTS ###

### Stacked bar graph of variance partitioning: taxonomic levels ###

vchinatraitsSLA2 <- data.frame(as.list(vchinatraitsSLA)) # turns vector into data frame
vchinatraitsSLA2$trait <- "SLA" # add a column called trait with SLA

vchinatraitsLDMC2 <- data.frame(as.list(vchinatraitsLDMC)) # turns vector into data frame
vchinatraitsLDMC2$trait <- "LDMC" # add a column called trait with LDMC

vchinatraitsThick2 <- data.frame(as.list(vchinatraitsThick)) # turns vector into data frame
vchinatraitsThick2$trait <- "Thick" # add a column called trait with LDMC

vchinatraitsArea2 <- data.frame(as.list(vchinatraitsArea)) # turns vector into data frame
vchinatraitsArea2$trait <- "Area" # add a column called trait with LDMC

# Mutate the wide data frames into long data frames.
vchinatraitsSLA3 <- gather(vchinatraitsSLA2, "Taxonomic.level", "Percent.variance", 1:5)
vchinatraitsLDMC3 <- gather(vchinatraitsLDMC2, "Taxonomic.level", "Percent.variance", 1:5)
vchinatraitsThick3 <-  gather(vchinatraitsThick2, "Taxonomic.level", "Percent.variance", 1:5)
vchinatraitsArea3 <-  gather(vchinatraitsArea2, "Taxonomic.level", "Percent.variance", 1:5)

# Bind all the data frames
varcompanalysis <- bind_rows(vchinatraitsSLA3, vchinatraitsLDMC3, vchinatraitsThick3, vchinatraitsArea3)
varcompanalysis$Percent.variance.100 <- varcompanalysis$Percent.variance*100 # add a new column where Percent.variance is multiplied by 100 to get actual percent and not decimal.

# Change "Within" to "within species"
levels(varcompanalysis$Taxonomic.level) <- c(levels(varcompanalysis$Taxonomic.level), "within species")
varcompanalysis$Taxonomic.level[varcompanalysis$Taxonomic.level=="Within"] <- "within species"

# Change the order of the taxonomic levels
varcompanalysis$Taxonomic.level <- factor(varcompanalysis$Taxonomic.level, levels= c("order", "family", "genus", "species", "within species"))

# Change the order of the traits
varcompanalysis$trait <- factor(varcompanalysis$trait, levels= c ("SLA", "LDMC", "Thick", "Area"))

# Make the stacked bar graph
ggplot(data = varcompanalysis, aes(x = trait, y = Percent.variance.100, fill = Taxonomic.level)) + geom_bar(stat="identity") + labs(title = "Variance Analysis", x= "Leaf trait" , y = "Percent variance" , fill="Taxonomic level") + scale_fill_manual(values=c("#003366", "#0066CC", "#3399FF", "#99CCFF", "#FF9900")) + theme(text = element_text(size=20))


### Stacked bar graph of variance partitioning: spatial levels ###

