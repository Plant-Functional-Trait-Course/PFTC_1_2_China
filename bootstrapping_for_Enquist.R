#Bootstrapping analyses for Enquist

#For each of the 4 sites
    #Generate distributions of 1) controls, 2) transplants
    #Do this using donor, recipient site 
#Store the distributions at each site/time as separate CSV file


#Visuals
  #overlapping distribution: origin dist, recipient dist, experiment dist

#Plots
  # skewness vs time, mean vs time, variance vs delta mean (change in mean from beginning to end)


##################################################



library(moments)

#Load biomass data
load("C:/Users/Brian/Dropbox/transplant/USE THIS DATA/traits.Rdata")
load("C:/Users/Brian/Dropbox/transplant/USE THIS DATA/Community.Rdata")
source("trait_distributions/r_scripts/trait_distribution_fx.R")
source("trait_distributions/r_scripts/trait_selecting_fx.R")


#script to make bootstrapped trait distributions for each turf at each time point


n_replicates<-100 #number of replicated draws of traits per turf, per year
multiplier<-1 #number of draws from the trait pool per percent cover.  1 is normal, 10 samples 10xpercent cover, etc.
for(i in 1: length(unique(cbind(cover_thin$turfID,cover_thin$year)))){

print(paste(  round(i/  length(unique(cbind(cover_thin$turfID,cover_thin$year)))*100,digits = 2  ),"percent finished" )) 
    
turfID<-unique(cbind(cover_thin$turfID,cover_thin$year))[i,][1]
year<-unique(cbind(cover_thin$turfID,cover_thin$year))[i,][2]
origin_site<-as.character(unique(cover_thin$originSiteID[which(cover_thin$turfID==turfID)]  ))
destination_site<-as.character(unique(cover_thin$destSiteID[which(cover_thin$turfID==turfID)]  ))  
pct_cover<-cover_thin[which(cover_thin$turfID==turfID & cover_thin$year==year),c('speciesName','cover')]
pct_cover$cover<-pct_cover$cover*multiplier


#Select the traits for use for each species
fixed_traits<-select_traits(species = pct_cover$speciesName,site = origin_site,traits_dataframe = traits)  
plastic_traits<-select_traits(species = pct_cover$speciesName,site = destination_site,traits_dataframe = traits)    

#toss out data according to flags


#Calculate distributions

# Reformat trait data to match expected input for trait_dist function
fixed_traits<-fixed_traits[,c("assigned_species","Wet_Mass_g", "Dry_Mass_g", "Leaf_Thickness_Ave_mm", "Leaf_Area_cm2", "SLA_cm2_g",
                "LDMC", "C_percent", "N_percent","CN_ratio", "dN15_percent", "dC13_percent", "P_AVG" )]

plastic_traits<-plastic_traits[,c("assigned_species","Wet_Mass_g", "Dry_Mass_g", "Leaf_Thickness_Ave_mm", "Leaf_Area_cm2", "SLA_cm2_g",
                              "LDMC", "C_percent", "N_percent","CN_ratio", "dN15_percent", "dC13_percent", "P_AVG" )]


fixed_trait_distributions<-trait_distributions(number_replicates = n_replicates,abundance_data = pct_cover,trait_data = fixed_traits)
plastic_trait_distributions<-trait_distributions(number_replicates = n_replicates,abundance_data = pct_cover,trait_data = plastic_traits)


#Save outputs

#Fixed traits
for(f in length(fixed_trait_distributions)){

trait_f<-names(fixed_trait_distributions)[[f]]    
dist_f<-fixed_trait_distributions[[f]]  

write.csv(x = dist_f,file = paste("trait_distributions/using_native_site/",turfID,"_",year,"_",trait_f,"_","Fixed",".csv",sep = "")  )
  
}


#Plastic traits
for(f in length(plastic_trait_distributions)){
  
  trait_f<-names(fixed_trait_distributions)[[f]]    
  dist_f<-fixed_trait_distributions[[f]]  
  
  paste("trait_distributions/using_recipient site/",turfID,"_",year,"_",trait_f,"_","Plastic",".csv",sep = "")  
  
  write.csv(x = dist_f,file = paste("trait_distributions/using_recipient site/",turfID,"_",year,"_",trait_f,"_","Plastic",".csv",sep = "")  )
  
}





}#for i loop
