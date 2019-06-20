###########################################
#8-31-18
# Lorah Patterson and Jon Henn - Variance partitioning of 2015 and 2016 China leaf trait data


library(tidyverse)
library(taxize)
library(lme4)
library(varComp)
library(jsonlite)
library(ape)
library(BIEN)
library(nlme)
library(ggplot2)

#Get most recent trait data from dropbox (as of June 2018, most recent data was uploaded in January 2018)
#setwd("~/Documents/R_data/China_2016_partitioning/012018_data_update/")

load("data/traits.RData") #the data frame is called "traits"

###############################################################
##### Add taxonomic data to trait data

#Get a column with names of genera from trait data
specgen <- strsplit(traits$Taxon," ") # split the specific epithet into genus and species
specgen$genus <- as.character(sapply(specgen, head, 1)) # get only the genera data out so that the next step to extract plant family is quicker and matches easier will less NAs
traits$genus <- specgen$genus # add genus column to trait data

#Get family and order from genus using BIEN
taxspecgen<-BIEN_taxonomy_genus(genus = specgen$genus)
taxspecgen2 <- taxspecgen %>% select(order, scrubbed_family, scrubbed_genus) # select order, family, genus columns only
taxspecgen2$genus <- taxspecgen2$scrubbed_genus # rename genus column
taxspecgen2$family <- taxspecgen2$scrubbed_family # rename family column
taxspecgen3 <- taxspecgen2 %>% select(order,family,genus) # select relevant columns
taxspecgen4 <- distinct(taxspecgen3) # Remove duplicate rows
# The results of this specify two families for Carex and Athyrium. Delete the incorrect rows. CAUTION: You may have to modify this if using different data.
taxspecgen5 <- subset(taxspecgen4, family!="Athyriaceae" & family!="Unknown")

#Join trait data to BIEN taxonomic data
varpart2016join <- full_join(taxspecgen5, traits, by = "genus")

#Change letters representing sites (L,M,H,A) to real names (Lower, Middle, High, Alpine)
levels(varpart2016join$Site) <- c(levels(varpart2016join$Site), "lower",
                                  "middle", "high", "alpine")

varpart2016join$Site[varpart2016join$Site == "L"] <- "lower"
varpart2016join$Site[varpart2016join$Site == "M"] <- "middle"
varpart2016join$Site[varpart2016join$Site == "H"] <- "high"
varpart2016join$Site[varpart2016join$Site == "A"] <- "alpine"

#Rename column name "taxon" to "species"
varpart2016join$species <- varpart2016join$Taxon
write.csv(varpart2016join, "varpart2016join.csv")
###############################################################
### Clean data

# Data I imported into this was already cleaned by Aud and Richard, so I just have to remove all flag rows that have been flagged #zap (which signifies an error that can't be corrected in the data. See file:///Users/lorahpatterson/Documents/R_data/China_2016_partitioning/102017_data_updates/Data_Cleaning_documentation.html). This leaves some data flagged with comments that are warnings.

traitsclean <- filter(varpart2016join, !grepl("#zap", AreaFlag)) %>% filter(!grepl("#zap", WetFlag)) %>% filter(!grepl("#zap", DryFlag)) %>% filter(!grepl("#zap", ThickFlag)) %>% filter(!grepl("#zap", GeneralFlag)) %>% filter(SLA_cm2_g < 500) %>% filter(SLA_cm2_g > 5) %>% filter(LDMC < 1) %>% filter(N_percent < 10 | is.na(N_percent))

write.csv(traitsclean, "traitsclean.csv")
### If necessary, remove gymnosperms and equisetum from data set so that we are only analyzing data for angiosperms ###
traitsangio <- traitsclean %>% filter(!family %in% c("Pinaceae", "Cupressaceae", "Equisetaceae"))

#write.csv(traitsangio, "traitsangio.csv")

### Subset by only those leaves collected in project=LOCAL (i.e. gradient outside the fence, grazed) or project=C or 0 (control plots within the fence, ungrazed) ###
varpart <- filter(traitsangio,Project=="LOCAL" | Project=="C" | Project=="0" | is.na(Project))

#write this to file to use in R markdown so that you don't need to create cleaned file again.
save(varpart, file= "varpart.RData")

# Subset varpart into different objects based on leaf collection location: local or control (optional if you want to compare)
varpartLocal <- subset(traitsangio, Project=="LOCAL") # only leaves outside fence, grazed
varpartControl <- subset(traitsangio, Project=="C") # only leaves inside fence, ungrazed

varpartLower <- subset(varpart, Site=="lower")
varpartMiddle <- subset(varpart, Site=="middle")
varpartAlpine <- subset(varpart, Site=="alpine")
varpartHigh <- subset(varpart, Site="high")

###############################################################
### Variance partitioning analysis

# Variance partitioning by taxonomic level of all non-experimental leaves collected in 2015 and 2016 inside and outside fence (i.e., in Control and Local projects). Site represents the within population variance. 

#load the cleaned traits data you created above
load("varpart.RData")
#write.csv(varpart, "varpart.csv")
# Reduce trait data to only include one of each StoichLabel (because stoich values were repeated across multiple leaves)
varpartStoich <- varpart[!duplicated(varpart$StoichLabel), ]
varpartStoich$NP <- varpartStoich$N_percent/varpartStoich$P_AVG
write.csv(varpartStoich, "varpartStoich.csv")
# Without log transformation
vchinatraitsSLAsite <- varcomp(lme(SLA_cm2_g~1, random=~1|order/family/genus/species/Site, data=varpart, na.action = na.omit), 1)
vchinatraitsAreasite <- varcomp(lme(Leaf_Area_cm2~1, random=~1|order/family/genus/species/Site, data=varpart, na.action = na.omit), 1)
vchinatraitsThicksite <- varcomp(lme(Leaf_Thickness_Ave_mm~1, random=~1|order/family/genus/species/Site, data=varpart, na.action = na.omit), 1)
vchinatraitsLDMCsite <- varcomp(lme(LDMC~1, random=~1|order/family/genus/species/Site, data=varpart, na.action = na.omit), 1)
vchinatraitsCPercentsite <- varcomp(lme(C_percent~1, random=~1|order/family/genus/species/Site, data=varpartStoich, na.action = na.omit), 1)
vchinatraitsNPercentsite <- varcomp(lme(N_percent~1, random=~1|order/family/genus/species/Site, data=varpartStoich, na.action = na.omit), 1)
vchinatraitsPPercentsite <- varcomp(lme(P_AVG~1, random=~1|order/family/genus/species/Site, data=varpartStoich, na.action = na.omit), 1)
vchinatraitsCNRatiosite <- varcomp(lme(CN_ratio~1, random=~1|order/family/genus/species/Site, data=varpartStoich, na.action = na.omit), 1)
vchinatraitsDC13site <- varcomp(lme(dC13_percent~1, random=~1|order/family/genus/species/Site, data=varpartStoich, na.action = na.omit), 1)
vchinatraitsDN15site <- varcomp(lme(dN15_percent~1, random=~1|order/family/genus/species/Site, data=varpartStoich, na.action = na.omit), 1)
vchinatraitsNPsite <- varcomp(lme(NP~1, random=~1|order/family/genus/species/Site, data=varpartStoich, na.action = na.omit), 1)

vchinatraitsSLAsite
vchinatraitsLDMCsite
vchinatraitsThicksite
vchinatraitsAreasite
vchinatraitsCPercentsite
vchinatraitsNPercentsite
vchinatraitsPPercentsite
vchinatraitsCNRatiosite
vchinatraitsDC13site
vchinatraitsDN15site
vchinatraitsNPsite 

# With log transformation
vchinatraitsSLAlog <- varcomp(lme(log(SLA_cm2_g)~1, random=~1|order/family/genus/species/Site, data=varpart, na.action = na.omit), 1)
vchinatraitsArealog <- varcomp(lme(log(Leaf_Area_cm2)~1, random=~1|order/family/genus/species/Site, data=varpart, na.action = na.omit), 1)
vchinatraitsThicklog <- varcomp(lme(log(Leaf_Thickness_Ave_mm)~1, random=~1|order/family/genus/species/Site, data=varpart, na.action = na.omit), 1)
vchinatraitsLDMClog <- varcomp(lme(log(LDMC)~1, random=~1|order/family/genus/species/Site, data=varpart, na.action = na.omit), 1)

vchinatraitsSLAlog
vchinatraitsLDMClog
vchinatraitsThicklog
vchinatraitsArealog

###############################################################
### Plotting variance partitioning (using all log transformed results, except for with SLA)


vchinatraitsSLAsite2<- data.frame(as.list(vchinatraitsSLAsite)) # turns vector into data frame
vchinatraitsSLAsite2$trait <- "SLA" # add a column called trait with SLA

vchinatraitsLDMClog2 <- data.frame(as.list(vchinatraitsLDMClog)) # turns vector into data frame
vchinatraitsLDMClog2$trait <- "LDMC" # add a column called trait with LDMC

vchinatraitsThicklog2 <- data.frame(as.list(vchinatraitsThicklog)) # turns vector into data frame
vchinatraitsThicklog2$trait <- "LT" # add a column called trait with leaf thickness (LT)

vchinatraitsArealog2<- data.frame(as.list(vchinatraitsArealog)) # turns vector into data frame
vchinatraitsArealog2$trait <- "LA" # add a column called trait with leaf area (LA)

vchinatraitsCPercentsite2 <- data.frame(as.list(vchinatraitsCPercentsite)) # turns vector into data frame
vchinatraitsCPercentsite2$trait <- "%C" # add a column called trait with percent carbon

vchinatraitsNPercentsite2 <- data.frame(as.list(vchinatraitsNPercentsite)) # turns vector into data frame
vchinatraitsNPercentsite2$trait <- "%N" # add a column called trait with percent nitrogen

vchinatraitsPPercentsite2 <- data.frame(as.list(vchinatraitsPPercentsite)) # turns vector into data frame
vchinatraitsPPercentsite2$trait <- "%P" # add a column called trait with percent phosphorus

vchinatraitsCNRatiosite2 <- data.frame(as.list(vchinatraitsCNRatiosite)) # turns vector into data frame
vchinatraitsCNRatiosite2$trait <- "C:N" # add a column called trait with C to N ratio

vchinatraitsDC13site2 <- data.frame(as.list(vchinatraitsDC13site)) # turns vector into data frame
vchinatraitsDC13site2$trait <- "dC13" # add a column called trait with dC13

vchinatraitsDN15site2 <- data.frame(as.list(vchinatraitsDN15site)) # turns vector into data frame
vchinatraitsDN15site2$trait <- "dN15" # add a column called trait with dN15

vchinatraitsNPsite2 <- data.frame(as.list(vchinatraitsNPsite)) # turns vector into data frame
vchinatraitsNPsite2$trait <- "NP"

# Mutate the wide data frames into long data frames.
vchinatraitsSLAsite3 <- gather(vchinatraitsSLAsite2, "Level", "Percent.variance", 1:6)
vchinatraitsLDMClog3 <- gather(vchinatraitsLDMClog2, "Level", "Percent.variance", 1:6)
vchinatraitsThicklog3 <-  gather(vchinatraitsThicklog2, "Level", "Percent.variance", 1:6)
vchinatraitsArealog3 <-  gather(vchinatraitsArealog2, "Level", "Percent.variance", 1:6)
vchinatraitsCPercentsite3 <- gather(vchinatraitsCPercentsite2, "Level", "Percent.variance", 1:6)
vchinatraitsNPercentsite3 <- gather(vchinatraitsNPercentsite2, "Level", "Percent.variance", 1:6)
vchinatraitsPPercentsite3 <- gather(vchinatraitsPPercentsite2, "Level", "Percent.variance", 1:6)
vchinatraitsCNRatiosite3 <- gather(vchinatraitsCNRatiosite2, "Level", "Percent.variance", 1:6)
vchinatraitsDC13site3 <- gather(vchinatraitsDC13site2, "Level", "Percent.variance", 1:6)
vchinatraitsDN15site3 <- gather(vchinatraitsDN15site2, "Level", "Percent.variance", 1:6)
vchinatraitsNPsite3 <- gather(vchinatraitsNPsite2, "Level", "Percent.variance", 1:6)

# Bind all the data frames
varcompanalysislog <- bind_rows(vchinatraitsSLAsite3, vchinatraitsLDMClog3, vchinatraitsThicklog3, vchinatraitsArealog3, vchinatraitsCPercentsite3, vchinatraitsNPercentsite3, vchinatraitsPPercentsite3, vchinatraitsCNRatiosite3, vchinatraitsDC13site3, vchinatraitsDN15site3, vchinatraitsNPsite3)
varcompanalysislog$Percent.variance.100 <- varcompanalysislog$Percent.variance*100 # add a new column where Percent.variance is multiplied by 100 to get actual percent and not decimal.

# Change "Within" to "within species"
levels(varcompanalysislog$Level) <- c(levels(varcompanalysislog$Level), "within species")
varcompanalysislog$Level[varcompanalysislog$Level=="Within"] <- "within species"

# Change "Site" to "site"
levels(varcompanalysislog$Level) <- c(levels(varcompanalysislog$Level), "site")
varcompanalysislog$Level[varcompanalysislog$Level=="Site"] <- "site"

# Change the order of the taxonomic levels
varcompanalysislog$Level <- factor(varcompanalysislog$Level, levels= c("order", "family", "genus", "species", "site", "within species"))

# Change the order of the traits
varcompanalysislog$trait <- factor(varcompanalysislog$trait, levels= c ("SLA", "LDMC", "LT", "LA", "%C", "%N", "%P", "C:N", "dC13", "dN15", "NP"))

# Make the stacked bar graph

varcompanalysislog1 <- varcompanalysislog %>% 
  select(-Percent.variance) %>% 
  spread(key = Level, value = Percent.variance.100) %>% 
  arrange(-`within species`) %>% 
  mutate(trait = factor(trait, trait)) %>% 
  gather(key = Level, value = Percent.variance.100, -trait)

varcompanalysislog1$Level <- factor(varcompanalysislog1$Level, levels= c("order", "family", "genus", "species", "site", "within species"))

varcompanalysislog1$test <- "1"

varcompanalysislog1 <- varcompanalysislog1 %>% 
  mutate(trait = plyr::mapvalues(trait, from = c ("SLA", "LDMC", "LT", "LA", "%C", "%N", "%P", "C:N", "dC13", "dN15", "NP"), to = c ("SLA", "LDMC", "LT", "LA", "'%'*'C'", "'%'*'N'", "'%'*'P'", "'C'*':'*'N'", "paste(delta^13, 'C')", "paste(delta^15, 'N')", "'N'*':'*'P'")))

ggplot(data = varcompanalysislog1, aes(x = test, y = Percent.variance.100, fill = Level)) + 
  geom_bar(stat="identity") + 
  facet_wrap(~trait, nrow = 1, switch  = "x", labeller = label_parsed) + 
  labs(x= NULL , y = "Percent variance" , fill="Level") + 
  scale_fill_manual(values=c("#003366", "#0066CC", "#3399FF", "#99CCFF", "#000000", "#FF9900")) + 
  theme_bw() + 
  theme(text = element_text(size = 12),
        axis.text.x  = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        strip.text = element_text(size = 8)) #+ 
# scale_x_discrete(labels = c("%N", "N:P", "C:N", "%P", "SLA", expression(paste(delta^{13}, "C")), "LDMC", "LT", expression(paste(delta^{15}, "N")), "%C", "LA")) 
