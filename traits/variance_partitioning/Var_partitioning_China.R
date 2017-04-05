###########################################
# 4-4-17
# Lorah Patterson - Variance partitioning of 2016 China leaf trait data

# Install packages:
install.packages("jsonlite")
install.packages("taxize")
install.packages("tidyr")
install.packages("lme4")
install.packages("varComp")
install.packages("ggplot2")
install.packages("BIEN")
library(dplyr)
library(tidyr)
library(taxize)
library(lme4)
library(varComp)
library(nlme)
library(jsonlite)
library(ape)
library(ggplot2)
library(BIEN)

# Load 2016 China trait data csv files and merge them.

setwd("~/Documents/R_data/China_2016_partitioning/Finse_traitdata/")
chinatraits1 <- read.csv("2016_PFTC2_Leaf_Area_corrCP_30032017_LPEdit.csv")
chinatraits2 <- read.csv("2016_China_envelope_names_CPcorr_30032017.csv")

chinatraits <- full_join(chinatraits2, chinatraits1, by="Envelope_Name_Corrected")
#################################################################

### Get family and order for each species using taxize and further data management ###

str(chinatraits) # see what types of data you have

chinatraits %>% mutate_if(is.factor, as.character) -> chinatraits # convert factors to characters

specgen <- strsplit(chinatraits$Plant_species,"_") # split the specific epithet into genus and species

specgen$genus <- as.character(sapply(specgen, head, 1)) # get only the genera data out so that the next step to extract plant family is quicker and matches easier will less NA's
#specgen$species <- as.character(sapply(specgen, tail, 1)) Don't need species yet.

## optional: write.table(specgen$genus,  file = "2016tax.csv", sep = ",", row.names = FALSE)
taxon1 <- read.csv("2016tax.csv")
taxon1 %>% mutate_if(is.factor, as.character) -> taxon1
# Get family and order using BIEN
tax<-BIEN_taxonomy_genus(genus = taxon1$genus)
tax<-BIEN_taxonomy_genus(genus = genus)


# Get family and order using taxize
family <- tax_name(query = specgen$genus, get = "family", db = "ncbi", verbose=TRUE, ask = FALSE) # Use the tax_name function of taxize to extract family data from genus. If there are multiple UID, it will skip those species by using ask = FALSE. This will take a long time.

chinatraits$family <- family # add the family names to your data
chinatraitsfam <- flatten(chinatraits, recursive = TRUE) # To flatten nested data frames (the taxize function puts family data into its own data frame along with database type and query type)

# change the column names of family, genus, species
colnames(chinatraitsfam)[23]<-"taxon_database"
colnames(chinatraitsfam)[24]<-"genus"
colnames(chinatraitsfam)[25]<-"family"
colnames(chinatraitsfam)[4]<-"species"
colnames(chinatraitsfam)

# Fix when there is an NA for family by first finding the NAs, then running taxize again on just those with NA and **manually** selecting which family is correct. If genus cannot be found, the row will be removed. This will take 20 minutes or so depending on the size of your data because it is partially manual. (You always select 2, so I wonder if there is a way to make this automatic in future.)
for(i in 1:length(chinatraitsfam$family)) {
  if(is.na(chinatraitsfam$family[i])) {
    family2 <- tax_name(query = chinatraitsfam$genus[i], get = "family", db = "ncbi", verbose=TRUE)
    levels(chinatraitsfam$family) <- c(levels(chinatraitsfam$family), family2$family)
    chinatraitsfam$family[i]<- family2$family
  }
}

# add taxonomic order as well. This also takes a long time. There isn't the problem with NAs for order since the NAs were fixed above.
order <- tax_name(query = chinatraitsfam$family, get = "order", db = "ncbi", verbose=TRUE, ask = FALSE) 
chinatraitsfam$order <- order
chinatraitsfam2 <- flatten(chinatraitsfam, recursive = TRUE) # To flatten nested data frames (the taxize function puts order data into its own data frame along with database type and query type)
colnames(chinatraitsfam2)[26] <-"taxon_database_2"
colnames(chinatraitsfam2)[27] <-"family_2"
colnames(chinatraitsfam2)[28] <- "order"

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


