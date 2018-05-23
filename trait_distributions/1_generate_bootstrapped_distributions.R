#Bootstrapping analyses for Enquist

#For each of the 4 sites
    #Generate distributions of 1) controls, 2) transplants
    #Do this using donor, recipient site 
#Store the distributions at each site/time as separate CSV file

#Update to make:
  #Modify code to rarify genus-level trait data to ensure equal sampling of species within genus


##################################################



library(moments)

#Load biomass data
load("C:/Users/Brian/Dropbox/transplant/USE THIS DATA/traits.Rdata")
load("C:/Users/Brian/Dropbox/transplant/USE THIS DATA/Community.Rdata")
source("trait_distributions/r_scripts/trait_distribution_fx.R")
source("trait_distributions/r_scripts/trait_selecting_fx.R")


#Add N:P ratio

traits$NP_ratio <- (traits$N_percent/traits$P_AVG)
traits_leaf<-traits[c("Taxon","Site","Project","Wet_Mass_g", "Dry_Mass_g", "Leaf_Thickness_Ave_mm", "Leaf_Area_cm2", "SLA_cm2_g","LDMC" )]
traits_chem<-traits[c("Taxon","Site","Project","C_percent", "N_percent","CN_ratio", "dN15_percent", "dC13_percent", "P_AVG","NP_ratio" )]
rm(traits)


#script to make bootstrapped trait distributions for each turf at each time point


n_replicates<-200 #number of replicated draws of traits per turf, per year # Use 200 so that the 95 percent ci is whole numbers (is 5 obs removed from each end)
multiplier<-1 #number of draws from the trait pool per percent cover.  1 is normal, 10 samples 10xpercent cover, etc.
for(i in 1: nrow(unique(cbind(cover_thin$turfID,cover_thin$year)))){

print(paste(  round(i/  nrow(unique(cbind(cover_thin$turfID,cover_thin$year)))*100,digits = 2  ),"percent finished" )) 
  
turfID<-unique(cbind(cover_thin$turfID,cover_thin$year))[i,][1]

year<-unique(cbind(cover_thin$turfID,cover_thin$year))[i,][2]
origin_site<-as.character(unique(cover_thin$originSiteID[which(cover_thin$turfID==turfID)]  ))
destination_site<-as.character(unique(cover_thin$destSiteID[which(cover_thin$turfID==turfID)]  ))  

pct_cover<-cover_thin[which(cover_thin$turfID==turfID & cover_thin$year==year),c('speciesName','cover')]
pct_cover$cover<-pct_cover$cover*multiplier
pct_cover<-as.data.frame(pct_cover)




#Select the traits for use for each species
#fixed_traits<-select_traits(species = pct_cover$speciesName,site = origin_site,traits_dataframe = traits)  
#plastic_traits<-select_traits(species = pct_cover$speciesName,site = destination_site,traits_dataframe = traits)    

fixed_traits_leaf<-select_traits(species = pct_cover$speciesName,site = origin_site,traits_dataframe = traits_leaf)  
fixed_traits_chem<-select_traits(species = pct_cover$speciesName,site = origin_site,traits_dataframe = traits_chem)
plastic_traits_leaf<-select_traits(species = pct_cover$speciesName,site = destination_site,traits_dataframe = traits_leaf)  
plastic_traits_chem<-select_traits(species = pct_cover$speciesName,site = destination_site,traits_dataframe = traits_chem)


#unique(pct_cover$speciesName)
#unique(fixed_traits$assigned_species)
#sum(pct_cover$cover )


#toss out data according to flags


#Calculate distributions

# Reformat trait data to match expected input for trait_dist function
fixed_traits_leaf<-fixed_traits_leaf[,c("assigned_species","Wet_Mass_g", "Dry_Mass_g", "Leaf_Thickness_Ave_mm", "Leaf_Area_cm2", "SLA_cm2_g", "LDMC" )]
fixed_traits_chem<-fixed_traits_chem[,c("assigned_species", "C_percent", "N_percent","CN_ratio", "dN15_percent", "dC13_percent", "P_AVG","NP_ratio" )]
plastic_traits_leaf<-plastic_traits_leaf[,c("assigned_species","Wet_Mass_g", "Dry_Mass_g", "Leaf_Thickness_Ave_mm", "Leaf_Area_cm2", "SLA_cm2_g", "LDMC" )]
plastic_traits_chem<-plastic_traits_chem[,c("assigned_species", "C_percent", "N_percent","CN_ratio", "dN15_percent", "dC13_percent", "P_AVG","NP_ratio" )]



fixed_trait_distributions_leaf<-trait_distributions(number_replicates = n_replicates,abundance_data = pct_cover,trait_data = fixed_traits_leaf)
fixed_trait_distributions_chem<-trait_distributions(number_replicates = n_replicates,abundance_data = pct_cover,trait_data = fixed_traits_chem)

plastic_trait_distributions_leaf<-trait_distributions(number_replicates = n_replicates,abundance_data = pct_cover,trait_data = plastic_traits_leaf)
plastic_trait_distributions_chem<-trait_distributions(number_replicates = n_replicates,abundance_data = pct_cover,trait_data = plastic_traits_chem)



rm(fixed_traits_chem,fixed_traits_leaf,plastic_traits_chem,plastic_traits_leaf,pct_cover)
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
write.csv(x = dist_f,file = paste("trait_distributions/using_native_site/",turfID,".",year,".",trait_f,".","Fixed",".csv",sep = ""),row.names = F  )
write.csv(x = dist_f_plastic,file = paste("trait_distributions/using_recipient site/",turfID,".",year,".",trait_f_plastic,".","Plastic",".csv",sep = ""),row.names = F  )



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
  write.csv(x = dist_f,file = paste("trait_distributions/using_native_site/",turfID,".",year,".",trait_f,".","Fixed",".csv",sep = ""),row.names = F  )
  write.csv(x = dist_f_plastic,file = paste("trait_distributions/using_recipient site/",turfID,".",year,".",trait_f_plastic,".","Plastic",".csv",sep = ""),row.names = F  )
  
  
  
  rm(trait_f,dist_f,trait_f_plastic,dist_f_plastic)  
  
}






rm(year,origin_site,turfID,destination_site,fixed_trait_distributions_chem,fixed_trait_distributions_leaf,plastic_trait_distributions_chem,plastic_trait_distributions_leaf)



}#for i loop

rm(f,i,multiplier,n_replicates)
