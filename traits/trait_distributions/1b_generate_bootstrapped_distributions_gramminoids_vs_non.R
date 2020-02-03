#Bootstrapping analyses for Enquist

#For each of the 4 sites
    #Generate distributions of 1) controls, 2) transplants
    #Do this using donor, recipient site 
#Store the distributions at each site/time as separate CSV file

#Update to make:
  #Modify code to rarify genus-level trait data to ensure equal sampling of species within genus


##################################################



library(moments)
library(BIEN)

#Load biomass data
load("C:/Users/Brian/Dropbox/transplant/USE THIS DATA/traits.Rdata")
load("C:/Users/Brian/Dropbox/transplant/USE THIS DATA/Community.Rdata")
source("trait_distributions/r_scripts/trait_distribution_fx.R")
source("trait_distributions/r_scripts/trait_selecting_fx.R")

#Fix spelling error (Cyanthus husincans => Cyananthus incanus)
traits$Taxon[grep(x = traits$Taxon,pattern = "Cyanthus husincans")] <- "Cyananthus incanus"

#Remove N outliers beyond BIEN max for study genera
#Preliminary analyses showed that outliers for Lowland sites were greatly influencing lowland distributions and means.
#Comparison with BIEN data suggested these values are likely outliers
#Consequently, we'll prune N data beyond the maximum value for these genera recorded in BIEN
china_spp<-unique(traits$Taxon)
china_genera<-unlist(lapply(X = china_spp,FUN = function(X){
  strsplit(x = X,split = " ")[[1]][1]
}))


BIENN<-BIEN_trait_traitbygenus(genus = china_genera,trait = "leaf nitrogen content per leaf dry mass")
BIEN_max_N<-max(as.numeric(as.character(BIENN$trait_value,na.rm = T)))*.1
rm(BIENN,china_spp)
traits$N_percent[which(traits$N_percent>BIEN_max_N)]<-NA

#We'll also remove outliers of SLA values which are beyond range reported in BIEN
BIENsla<-BIEN_trait_traitbygenus(genus = china_genera,trait = "leaf area per leaf dry mass")
BIENsla$trait_value<-as.numeric(as.character(BIENsla$trait_value))
BIENsla$trait_value<-BIENsla$trait_value*10000*(1/1000) #convert to cm2/kg
BIENsla$unit<-"cm2.kg-1"
BIEN_max_sla<-max(BIENsla$trait_value)
#hist(BIENsla$trait_value)
#hist(traits$SLA_cm2_g)
traits$SLA_cm2_g[which(traits$SLA_cm2_g>BIEN_max_sla)]<-NA


#remove extreme ldmc
BIENldmc<-BIEN_trait_traitbygenus(genus = china_genera,trait = "leaf dry mass per leaf fresh mass")
BIENldmc$trait_value<-as.numeric(as.character(BIENldmc$trait_value))
BIENldmc$trait_value<-BIENldmc$trait_value*(1/1000)#convert to g/g
BIENldmc$unit<-"g.g-1"
BIEN_max_ldmc<-max(BIENldmc$trait_value)
traits$LDMC[which(traits$LDMC>BIEN_max_ldmc)]<-NA
rm(BIENsla,BIEN_max_sla,china_genera,BIEN_max_N,BIENldmc,BIEN_max_ldmc)




#Add N:P ratio

traits$NP_ratio <- (traits$N_percent/traits$P_AVG)
traits_leaf<-traits[c("Taxon","Site","Project","Wet_Mass_g", "Dry_Mass_g", "Leaf_Thickness_Ave_mm", "Leaf_Area_cm2", "SLA_cm2_g","LDMC" )]
traits_chem<-traits[c("Taxon","Site","Project","C_percent", "N_percent","CN_ratio", "dN15_percent", "dC13_percent", "P_AVG","NP_ratio" )]

#Log transform leaf size variables
traits_leaf$Wet_Mass_g <- log(traits_leaf$Wet_Mass_g)
traits_leaf$Dry_Mass_g <- log(traits_leaf$Dry_Mass_g)
traits_leaf$Leaf_Area_cm2 <- log(traits_leaf$Leaf_Area_cm2)




#Figure out which spp are gramminoids
#Add genus to data for convenience
cover_thin$genus <- unlist(lapply(X = cover_thin$speciesName,FUN = function(x){strsplit(x,split = " ")[[1]][1]}))
traits_chem$genus <- unlist(lapply(X = traits_chem$Taxon,FUN = function(x){strsplit(x,split = " ")[[1]][1]}))
traits_leaf$genus <- unlist(lapply(X = traits_leaf$Taxon,FUN = function(x){strsplit(x,split = " ")[[1]][1]}))

genus_list<-unique(c(traits_chem$genus,cover_thin$genus,traits_leaf$genus))

genus_taxonomy<-BIEN_taxonomy_genus(genus = genus_list)
genus_taxonomy<-unique(genus_taxonomy[c('order','scrubbed_family','scrubbed_genus')])
genus_list[which(!genus_list %in% genus_taxonomy$scrubbed_genus)]#good, all genera appear in BIEN taxonomy info
gram_genera<-genus_taxonomy$scrubbed_genus[which(genus_taxonomy$scrubbed_family %in% c("Poaceae","Cyperaceae","Juncaceae"))]

#Split data into gramminoids vs non-gramminoid
cover_thin_gramminoids<-cover_thin[which(cover_thin$genus%in%gram_genera),]
cover_thin_non_gramminoids<-cover_thin[which(!cover_thin$genus%in%gram_genera),]

traits_chem_gramminoids<-traits_chem[which(traits_chem$genus %in% gram_genera),]
traits_leaf_gramminoids<-traits_leaf[which(traits_leaf$genus %in% gram_genera),]

traits_chem_non_gramminoids<-traits_chem[which(!traits_chem$genus %in% gram_genera),]
traits_leaf_non_gramminoids<-traits_leaf[which(!traits_leaf$genus %in% gram_genera),]

#cleanup spandrels
rm(traits_chem,traits_leaf,cover_thin,genus_taxonomy,BIEN_max_N,genus_list,gram_genera,traits)

traits_leaf_gramminoids<-traits_leaf_gramminoids[which(colnames(traits_leaf_gramminoids)!="genus")]
traits_leaf_non_gramminoids<-traits_leaf_non_gramminoids[which(colnames(traits_leaf_non_gramminoids)!="genus")]
traits_chem_gramminoids<-traits_chem_gramminoids[which(colnames(traits_chem_gramminoids)!="genus")]
traits_chem_non_gramminoids<-traits_chem_non_gramminoids[which(colnames(traits_chem_non_gramminoids)!="genus")]
cover_thin_gramminoids<-cover_thin_gramminoids[which(colnames(cover_thin_gramminoids)!="genus")]
cover_thin_non_gramminoids<-cover_thin_non_gramminoids[which(colnames(cover_thin_non_gramminoids)!="genus")]

# rescale variables
traits_leaf_gramminoids$Wet_Mass_g <- scale(traits_leaf_gramminoids$Wet_Mass_g)
traits_leaf_gramminoids$Dry_Mass_g <- scale(traits_leaf_gramminoids$Dry_Mass_g)
traits_leaf_gramminoids$Leaf_Thickness_Ave_mm <- scale(traits_leaf_gramminoids$Leaf_Thickness_Ave_mm)
traits_leaf_gramminoids$Leaf_Area_cm2 <- scale(traits_leaf_gramminoids$Leaf_Area_cm2)
traits_leaf_gramminoids$SLA_cm2_g <- scale(traits_leaf_gramminoids$SLA_cm2_g)
traits_leaf_gramminoids$LDMC <- scale(traits_leaf_gramminoids$LDMC)

traits_chem_gramminoids$C_percent<-scale(traits_chem_gramminoids$C_percent)
traits_chem_gramminoids$N_percent<-scale(traits_chem_gramminoids$N_percent)
traits_chem_gramminoids$CN_ratio<-scale(traits_chem_gramminoids$CN_ratio)
traits_chem_gramminoids$dN15_percent<-scale(traits_chem_gramminoids$dN15_percent)
traits_chem_gramminoids$dC13_percent<-scale(traits_chem_gramminoids$dC13_percent)
traits_chem_gramminoids$P_AVG<-scale(traits_chem_gramminoids$P_AVG)
traits_chem_gramminoids$NP_ratio<-scale(traits_chem_gramminoids$NP_ratio)

traits_leaf_non_gramminoids$Wet_Mass_g <- scale(traits_leaf_non_gramminoids$Wet_Mass_g)
traits_leaf_non_gramminoids$Dry_Mass_g <- scale(traits_leaf_non_gramminoids$Dry_Mass_g)
traits_leaf_non_gramminoids$Leaf_Thickness_Ave_mm <- scale(traits_leaf_non_gramminoids$Leaf_Thickness_Ave_mm)
traits_leaf_non_gramminoids$Leaf_Area_cm2 <- scale(traits_leaf_non_gramminoids$Leaf_Area_cm2)
traits_leaf_non_gramminoids$SLA_cm2_g <- scale(traits_leaf_non_gramminoids$SLA_cm2_g)
traits_leaf_non_gramminoids$LDMC <- scale(traits_leaf_non_gramminoids$LDMC)

traits_chem_non_gramminoids$C_percent<-scale(traits_chem_non_gramminoids$C_percent)
traits_chem_non_gramminoids$N_percent<-scale(traits_chem_non_gramminoids$N_percent)
traits_chem_non_gramminoids$CN_ratio<-scale(traits_chem_non_gramminoids$CN_ratio)
traits_chem_non_gramminoids$dN15_percent<-scale(traits_chem_non_gramminoids$dN15_percent)
traits_chem_non_gramminoids$dC13_percent<-scale(traits_chem_non_gramminoids$dC13_percent)
traits_chem_non_gramminoids$P_AVG<-scale(traits_chem_non_gramminoids$P_AVG)
traits_chem_non_gramminoids$NP_ratio<-scale(traits_chem_non_gramminoids$NP_ratio)


#######################################################################################

#######################################################################################
#Gramminoids

#script to make bootstrapped trait distributions for each turf at each time point


n_replicates<-200 #number of replicated draws of traits per turf, per year # Use 200 so that the 95 percent ci is whole numbers (is 5 obs removed from each end)
multiplier<-1 #number of draws from the trait pool per percent cover.  1 is normal, 10 samples 10xpercent cover, etc.

for(i in 1: nrow(unique(cbind(cover_thin_gramminoids$turfID,cover_thin_gramminoids$year)))){

print(paste(  round(i/  nrow(unique(cbind(cover_thin_gramminoids$turfID,cover_thin_gramminoids$year)))*100,digits = 2  ),"percent finished" )) 
  
turfID<-unique(cbind(cover_thin_gramminoids$turfID,cover_thin_gramminoids$year))[i,][1]

year<-unique(cbind(cover_thin_gramminoids$turfID,cover_thin_gramminoids$year))[i,][2]
origin_site<-as.character(unique(cover_thin_gramminoids$originSiteID[which(cover_thin_gramminoids$turfID==turfID)]  ))
destination_site<-as.character(unique(cover_thin_gramminoids$destSiteID[which(cover_thin_gramminoids$turfID==turfID)]  ))  

pct_cover<-cover_thin_gramminoids[which(cover_thin_gramminoids$turfID==turfID & cover_thin_gramminoids$year==year),c('speciesName','cover')]
pct_cover$cover<-pct_cover$cover*multiplier
pct_cover<-as.data.frame(pct_cover)




#Select the traits for use for each species
#fixed_traits<-select_traits(species = pct_cover$speciesName,site = origin_site,traits_dataframe = traits)  
#plastic_traits<-select_traits(species = pct_cover$speciesName,site = destination_site,traits_dataframe = traits)    

fixed_traits_leaf_gramminoids<-select_traits(species = pct_cover$speciesName,site = origin_site,traits_dataframe = traits_leaf_gramminoids)  
fixed_traits_chem_gramminoids<-select_traits(species = pct_cover$speciesName,site = origin_site,traits_dataframe = traits_chem_gramminoids)
plastic_traits_leaf_gramminoids<-select_traits(species = pct_cover$speciesName,site = destination_site,traits_dataframe = traits_leaf_gramminoids)  
plastic_traits_chem_gramminoids<-select_traits(species = pct_cover$speciesName,site = destination_site,traits_dataframe = traits_chem_gramminoids)


#unique(pct_cover$speciesName)
#unique(fixed_traits$assigned_species)
#sum(pct_cover$cover )


#toss out data according to flags


#Calculate distributions

# Reformat trait data to match expected input for trait_dist function
fixed_traits_leaf_gramminoids<-fixed_traits_leaf_gramminoids[,c("assigned_species","Wet_Mass_g", "Dry_Mass_g", "Leaf_Thickness_Ave_mm", "Leaf_Area_cm2", "SLA_cm2_g", "LDMC" )]
fixed_traits_chem_gramminoids<-fixed_traits_chem_gramminoids[,c("assigned_species", "C_percent", "N_percent","CN_ratio", "dN15_percent", "dC13_percent", "P_AVG","NP_ratio" )]
plastic_traits_leaf_gramminoids<-plastic_traits_leaf_gramminoids[,c("assigned_species","Wet_Mass_g", "Dry_Mass_g", "Leaf_Thickness_Ave_mm", "Leaf_Area_cm2", "SLA_cm2_g", "LDMC" )]
plastic_traits_chem_gramminoids<-plastic_traits_chem_gramminoids[,c("assigned_species", "C_percent", "N_percent","CN_ratio", "dN15_percent", "dC13_percent", "P_AVG","NP_ratio" )]



fixed_trait_distributions_leaf<-trait_distributions(number_replicates = n_replicates,abundance_data = pct_cover,trait_data = fixed_traits_leaf_gramminoids)
try(fixed_trait_distributions_chem<-trait_distributions(number_replicates = n_replicates,abundance_data = pct_cover,trait_data = fixed_traits_chem_gramminoids))

plastic_trait_distributions_leaf<-trait_distributions(number_replicates = n_replicates,abundance_data = pct_cover,trait_data = plastic_traits_leaf_gramminoids)
try(plastic_trait_distributions_chem<-trait_distributions(number_replicates = n_replicates,abundance_data = pct_cover,trait_data = plastic_traits_chem_gramminoids))



rm(fixed_traits_chem_gramminoids,fixed_traits_leaf_gramminoids,plastic_traits_chem_gramminoids,plastic_traits_leaf_gramminoids,pct_cover)
#Save outputs

#Chem traits

if(exists("fixed_trait_distributions_chem")){

for(f in 1:length(fixed_trait_distributions_chem)){

  
trait_f<-names(fixed_trait_distributions_chem)[[f]]    
dist_f<-fixed_trait_distributions_chem[[f]]  
if(nrow(dist_f)!=n_replicates){stop("Number of rows in distribution does not equal number of replicates")}  

trait_f_plastic<-names(plastic_trait_distributions_chem)[[f]]    
dist_f_plastic<-plastic_trait_distributions_chem[[f]]  
if(nrow(dist_f_plastic)!=n_replicates){stop("Number of rows in distribution does not equal number of replicates")}  

#print(trait_f)
write.csv(x = dist_f,file = paste("trait_distributions/gramminoids/using_native_site/",turfID,".",year,".",trait_f,".","Fixed",".csv",sep = ""),row.names = F  )
write.csv(x = dist_f_plastic,file = paste("trait_distributions/gramminoids/using_recipient site/",turfID,".",year,".",trait_f_plastic,".","Plastic",".csv",sep = ""),row.names = F  )



rm(trait_f,dist_f,trait_f_plastic,dist_f_plastic)  

}}

#Leaf traits
for(f in 1:length(fixed_trait_distributions_leaf)){
  
  
  trait_f<-names(fixed_trait_distributions_leaf)[[f]]    
  dist_f<-fixed_trait_distributions_leaf[[f]]  
  if(nrow(dist_f)!=n_replicates){stop("Number of rows in distribution does not equal number of replicates")}  
  
  trait_f_plastic<-names(plastic_trait_distributions_leaf)[[f]]    
  dist_f_plastic<-plastic_trait_distributions_leaf[[f]]  
  if(nrow(dist_f_plastic)!=n_replicates){stop("Number of rows in distribution does not equal number of replicates")}  
  
  #print(trait_f)
  write.csv(x = dist_f,file = paste("trait_distributions/gramminoids/using_native_site/",turfID,".",year,".",trait_f,".","Fixed",".csv",sep = ""),row.names = F  )
  write.csv(x = dist_f_plastic,file = paste("trait_distributions/gramminoids/using_recipient_site/",turfID,".",year,".",trait_f_plastic,".","Plastic",".csv",sep = ""),row.names = F  )
  
  
  
  rm(trait_f,dist_f,trait_f_plastic,dist_f_plastic)  
  
}






rm(year,origin_site,turfID,destination_site,fixed_trait_distributions_chem,fixed_trait_distributions_leaf,plastic_trait_distributions_chem,plastic_trait_distributions_leaf)



}#for i loop

rm(f,i,multiplier,n_replicates)



#######################################################################################

#######################################################################################
#Non-Gramminoids

#script to make bootstrapped trait distributions for each turf at each time point


n_replicates<-200 #number of replicated draws of traits per turf, per year # Use 200 so that the 95 percent ci is whole numbers (is 5 obs removed from each end)
multiplier<-1 #number of draws from the trait pool per percent cover.  1 is normal, 10 samples 10xpercent cover, etc.
for(i in 1: nrow(unique(cbind(cover_thin_non_gramminoids$turfID,cover_thin_non_gramminoids$year)))){
  
  print(paste(  round(i/  nrow(unique(cbind(cover_thin_non_gramminoids$turfID,cover_thin_non_gramminoids$year)))*100,digits = 2  ),"percent finished" )) 
  
  turfID<-unique(cbind(cover_thin_non_gramminoids$turfID,cover_thin_non_gramminoids$year))[i,][1]
  
  year<-unique(cbind(cover_thin_non_gramminoids$turfID,cover_thin_non_gramminoids$year))[i,][2]
  origin_site<-as.character(unique(cover_thin_non_gramminoids$originSiteID[which(cover_thin_non_gramminoids$turfID==turfID)]  ))
  destination_site<-as.character(unique(cover_thin_non_gramminoids$destSiteID[which(cover_thin_non_gramminoids$turfID==turfID)]  ))  
  
  pct_cover<-cover_thin_non_gramminoids[which(cover_thin_non_gramminoids$turfID==turfID & cover_thin_non_gramminoids$year==year),c('speciesName','cover')]
  pct_cover$cover<-pct_cover$cover*multiplier
  pct_cover<-as.data.frame(pct_cover)
  
  
  
  
  #Select the traits for use for each species
  #fixed_traits<-select_traits(species = pct_cover$speciesName,site = origin_site,traits_dataframe = traits)  
  #plastic_traits<-select_traits(species = pct_cover$speciesName,site = destination_site,traits_dataframe = traits)    
  
  fixed_traits_leaf_non_gramminoids<-select_traits(species = pct_cover$speciesName,site = origin_site,traits_dataframe = traits_leaf_non_gramminoids)  
  fixed_traits_chem_non_gramminoids<-select_traits(species = pct_cover$speciesName,site = origin_site,traits_dataframe = traits_chem_non_gramminoids)
  plastic_traits_leaf_non_gramminoids<-select_traits(species = pct_cover$speciesName,site = destination_site,traits_dataframe = traits_leaf_non_gramminoids)  
  plastic_traits_chem_non_gramminoids<-select_traits(species = pct_cover$speciesName,site = destination_site,traits_dataframe = traits_chem_non_gramminoids)
  
  
  #unique(pct_cover$speciesName)
  #unique(fixed_traits$assigned_species)
  #sum(pct_cover$cover )
  
  
  #toss out data according to flags
  
  
  #Calculate distributions
  
  # Reformat trait data to match expected input for trait_dist function
  fixed_traits_leaf_non_gramminoids<-fixed_traits_leaf_non_gramminoids[,c("assigned_species","Wet_Mass_g", "Dry_Mass_g", "Leaf_Thickness_Ave_mm", "Leaf_Area_cm2", "SLA_cm2_g", "LDMC" )]
  fixed_traits_chem_non_gramminoids<-fixed_traits_chem_non_gramminoids[,c("assigned_species", "C_percent", "N_percent","CN_ratio", "dN15_percent", "dC13_percent", "P_AVG","NP_ratio" )]
  plastic_traits_leaf_non_gramminoids<-plastic_traits_leaf_non_gramminoids[,c("assigned_species","Wet_Mass_g", "Dry_Mass_g", "Leaf_Thickness_Ave_mm", "Leaf_Area_cm2", "SLA_cm2_g", "LDMC" )]
  plastic_traits_chem_non_gramminoids<-plastic_traits_chem_non_gramminoids[,c("assigned_species", "C_percent", "N_percent","CN_ratio", "dN15_percent", "dC13_percent", "P_AVG","NP_ratio" )]
  
  
  
  fixed_trait_distributions_leaf<-trait_distributions(number_replicates = n_replicates,abundance_data = pct_cover,trait_data = fixed_traits_leaf_non_gramminoids)
  fixed_trait_distributions_chem<-trait_distributions(number_replicates = n_replicates,abundance_data = pct_cover,trait_data = fixed_traits_chem_non_gramminoids)
  
  plastic_trait_distributions_leaf<-trait_distributions(number_replicates = n_replicates,abundance_data = pct_cover,trait_data = plastic_traits_leaf_non_gramminoids)
  plastic_trait_distributions_chem<-trait_distributions(number_replicates = n_replicates,abundance_data = pct_cover,trait_data = plastic_traits_chem_non_gramminoids)
  
  
  
  rm(fixed_traits_chem_non_gramminoids,fixed_traits_leaf_non_gramminoids,plastic_traits_chem_non_gramminoids,plastic_traits_leaf_non_gramminoids,pct_cover)
  #Save outputs
  
  #Chem traits
  for(f in 1:length(fixed_trait_distributions_chem)){
    
    
    trait_f<-names(fixed_trait_distributions_chem)[[f]]    
    dist_f<-fixed_trait_distributions_chem[[f]]  
    if(nrow(dist_f)!=n_replicates){stop("Number of rows in distribution does not equal number of replicates")}  
    
    trait_f_plastic<-names(plastic_trait_distributions_chem)[[f]]    
    dist_f_plastic<-plastic_trait_distributions_chem[[f]]  
    if(nrow(dist_f_plastic)!=n_replicates){stop("Number of rows in distribution does not equal number of replicates")}  
    
    #print(trait_f)
    write.csv(x = dist_f,file = paste("trait_distributions/non_gramminoids/native_site/",turfID,".",year,".",trait_f,".","Fixed",".csv",sep = ""),row.names = F  )
    write.csv(x = dist_f_plastic,file = paste("trait_distributions/non_gramminoids/recipient_site/",turfID,".",year,".",trait_f_plastic,".","Plastic",".csv",sep = ""),row.names = F  )
    
    
    
    rm(trait_f,dist_f,trait_f_plastic,dist_f_plastic)  
    
  }
  
  #Leaf traits
  for(f in 1:length(fixed_trait_distributions_leaf)){
    
    
    trait_f<-names(fixed_trait_distributions_leaf)[[f]]    
    dist_f<-fixed_trait_distributions_leaf[[f]]  
    if(nrow(dist_f)!=n_replicates){stop("Number of rows in distribution does not equal number of replicates")}  
    
    trait_f_plastic<-names(plastic_trait_distributions_leaf)[[f]]    
    dist_f_plastic<-plastic_trait_distributions_leaf[[f]]  
    if(nrow(dist_f_plastic)!=n_replicates){stop("Number of rows in distribution does not equal number of replicates")}  
    
    #print(trait_f)
    write.csv(x = dist_f,file = paste("trait_distributions/non_gramminoids/native_site/",turfID,".",year,".",trait_f,".","Fixed",".csv",sep = ""),row.names = F  )
    write.csv(x = dist_f_plastic,file = paste("trait_distributions/non_gramminoids/recipient_site/",turfID,".",year,".",trait_f_plastic,".","Plastic",".csv",sep = ""),row.names = F  )
    
    
    
    rm(trait_f,dist_f,trait_f_plastic,dist_f_plastic)  
    
  }
  
  
  
  
  
  
  rm(year,origin_site,turfID,destination_site,fixed_trait_distributions_chem,fixed_trait_distributions_leaf,plastic_trait_distributions_chem,plastic_trait_distributions_leaf)
  
  
  
}#for i loop

rm(f,i,multiplier,n_replicates)

