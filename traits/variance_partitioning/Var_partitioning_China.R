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

# Load 2015 and 2016 China trait data files and merge them using trait_2017_analysis.R in transplant github repository.
source("traits/trait_2017_analysis.R")
write.csv(traits, file="traits.csv")
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
varpart2016$family <- taxspecgen$scrubbed_family
write.csv(varpart2016, file="traits2.csv")

### ~OR~ get family and order from genus using taxize ###
#family <- tax_name(query = specgen$genus, get = "family", db = "ncbi", verbose=TRUE, ask = FALSE) 
# If there are multiple UID, it will skip those species by using ask = FALSE.
# Running through taxize will require you to fix when there is an NA for family by first finding the NAs, then running taxize again on just those with NA and **manually** selecting which family is correct. If genus cannot be found, the row will be removed. This will take 20 minutes or so depending on the size of your data because it is partially manual. (You always select 2, so I wonder if there is a way to make this automatic in future.)
# #for(i in 1:length(chinatraitsfam$family)) {
#   if(is.na(chinatraitsfam$family[i])) {
#     family2 <- tax_name(query = chinatraitsfam$genus[i], get = "family", db = "ncbi", verbose=TRUE)
#     levels(chinatraitsfam$family) <- c(levels(chinatraitsfam$family), family2$family)
#     chinatraitsfam$family[i]<- family2$family
#   }
# }
# add taxonomic order as well.
# order <- tax_name(query = chinatraitsfam$family, get = "order", db = "ncbi", verbose=TRUE, ask = FALSE) 
# chinatraitsfam$order <- order
# chinatraitsfam2 <- flatten(chinatraitsfam, recursive = TRUE) # To flatten nested data frames (the taxize function puts order data into its own data frame along with database type and query type)
# colnames(chinatraitsfam2)[26] <-"taxon_database_2"
# colnames(chinatraitsfam2)[27] <-"family_2"
# colnames(chinatraitsfam2)[28] <- "order"

# Change letters representing sites (L,M,H,A) to real names (Lower, Middle, High, Alpine)
levels(chinatraitsfam2$Site) <- c(levels(chinatraitsfam$Site), "lower",
                                 "middle", "high", "alpine")

chinatraitsfam$Site[chinatraitsfam2$Site == "L"] <- "lower"
chinatraitsfam$Site[chinatraitsfam2$Site == "M"] <- "middle"
chinatraitsfam$Site[chinatraitsfam2$Site == "H"] <- "high"
chinatraitsfam$Site[chinatraitsfam2$Site == "A"] <- "alpine"

# Remove gymnosperms and equisetum from data set so that we are only analyzing data for angiosperms
chinatraitsfam2 <- filter(chinatraitsfam2, chinatraitsfam$family!= "Pinaceae", chinatraitsfam$family!="Cupressaceae", chinatraitsfam$family!= "Equisetaceae")

# Write all changes and additions to a .csv
write.table(chinatraitsfam2, file = "ChinaLeafTraitData_final.csv", sep = ",", row.names = FALSE)
chinatraitsfinal <-read.csv("ChinaLeafTraitData_final.csv")

##############################################################
### lme and varcomp analysis ###

# Variance partitioning by order, family, genus, species
vchinatraitsSLA <- varcomp(lme(SLA_cm2.g~1, random=~1|order/family/genus/species, data=chinatraitsfinal, na.action = na.omit), 1)

vchinatraitsArea <- varcomp(lme(Leaf_Area_cm2~1, random=~1|order/family/genus/species, data=chinatraitsfinal, na.action = na.omit), 1)

vchinatraitsThick <- varcomp(lme(Leaf_Thickness_Ave_mm~1, random=~1|order/family/genus/species, data=chinatraitsfinal, na.action = na.omit), 1)

vchinatraitsLDMC <- varcomp(lme(LDMC~1, random=~1|order/family/genus/species, data=chinatraitsfinal, na.action = na.omit), 1)

# Variance partitioning between and within site (Not sure if this is the correct way to assess this)
vchinatraitsSiteSLA <- varcomp(lme(SLA_cm2.g~1, random=~1|Site, data=chinatraitsfinal, na.action = na.omit), 1)

vchinatraitsSiteArea <- varcomp(lme(Leaf_Area_cm2~1, random=~1|Site, data=chinatraitsfinal, na.action = na.omit), 1)

vchinatraitsSiteThick <- varcomp(lme(Leaf_Thickness_Ave_mm~1, random=~1|Site, data=chinatraitsfinal, na.action = na.omit), 1)

vchinatraitsSiteLDMC <- varcomp(lme(LDMC~1, random=~1|Site, data=chinatraitsfinal, na.action = na.omit), 1)

#show results
vchinatraitsSLA
vchinatraitsArea
vchinatraitsThick
vchinatraitsLDMC

vchinatraitsSiteSLA
vchinatraitsSiteArea
vchinatraitsSiteThick
vchinatraitsSiteLDMC

#####################################################
### PLOTS ###
ggplot(data = chinatraitsfinal, aes(x = Site, y = SLA_cm2.g))

plot(chinatraitsfinal$Site, chinatraitsfinal$SLA_cm2.g)
#Variance component analysis scatter plot

plot.varcomp(vchinatraitsSLA, xlab = "Levels", ylab = "Variance", type="p")
plot.varcomp(vchinatraitsArea, xlab = "Levels", ylab = "Variance", type="p")
plot.varcomp(vchinatraitsThick, xlab = "Levels", ylab = "Variance", type = "p")
plot.varcomp(vchinatraitsLDMC, xlab = "Levels", ylab = "Variance", type = "p")

plot.varcomp(vchinatraitsSiteSLA, xlab = "Levels", ylab = "Variance", type = "p")
plot.varcomp(vchinatraitsSiteArea, xlab = "Levels", ylab = "Variance", type = "p")
plot.varcomp(vchinatraitsSiteThick, xlab = "Levels", ylab = "Variance", type = "p")
plot.varcomp(vchinatraitsSiteLDMC, xlab = "Levels", ylab = "Variance", type = "p")

# Scatterplots to check for incorrect original trait data.

pairs(~ log10(SLA_cm2.g) + log10(Leaf_Area_cm2) + log10(Leaf_Thickness_Ave_mm) + log10(LDMC), data=chinatraitsfinal)

####Bar Graphs to show variance component analysis####
# Probably a better way to get the data into shape than what I did since it took so many lines... but hey it worked!

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


#### Linear and linear mixed-effects - assessing effects of elevation on leaf area using lme
# 
# la.lm<-lm(Leaf_Area_cm2~Elevation, data=chinatraitsfam, na.action=na.omit)
# summary(la.lm)
#   # Elevation has no effect on leaf area 
# 
# library(nlme)
# la.lme<-lme(Leaf_Area_cm2~Elevation, random=~1|Taxon_written_on_envelopes, data=chinatraitsfam, na.action=na.omit)
# summary(la.lme)


