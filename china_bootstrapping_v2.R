#Draft code for inferring trait distributions using nonparametric bootstrapping
#8/18/2017-?
#Jon Henn, Brian Maitner
########################################

#We'll need community distributions for each experimental plot 
    #with traits drawn from both the "home" location of the plot  
    #and the "current" location of the plot. 

#A distribution for each year would be good and the traits include: 
#-SLA
#-LDMC
#-nutrients (whatever is available)
#-leaf thickness
#-leaf area
#-dry weight


#Components needed:
  #I'm thinking modular here, so something like

#1) Core function
    #Input:
      #A) Dataframe with paired species names and abundances
      #B) 

#######################

#Bootstrapped data based on biomass dataset

library(moments)

#Load biomass data
load("C:/Users/Brian/Dropbox/transplant/USE THIS DATA/biomass_cleaned.Rdata")
load("C:/Users/Brian/Dropbox/transplant/USE THIS DATA/traits.Rdata")

cover_meta

unique(biomass$site)
bad_spp<-unique(biomass$speciesName[which(!biomass$speciesName %in% traits$Taxon)])
un_spp<-unique(biomass$speciesName[grep(pattern = "Unk",x = biomass$speciesName)])

for(i in 1:4){
site<-c("H","A","M","L")[i]  
data_i<-biomass[which(biomass$site==site),]
good_data_i<-sum(data_i$biomass[which(!data_i$speciesName%in%bad_spp)],na.rm = T  )
#bad_data_i<-sum(data_i$biomass[which(data_i$speciesName%in%bad_spp)],na.rm = T  )  
bad_data_i<-sum(data_i$biomass[which(data_i$speciesName%in%un_spp)],na.rm = T  )  

print(paste("site ",site, bad_data_i/(good_data_i+bad_data_i), " percent bad data"))

  
}
#Looks like we've got a lot of biomass in "Genus_spp" format, so have to add in a genus level sampling scheme
#use any Project that isn't OTC or 1:4


genus<-lapply(X = traits$Taxon,FUN = function(x){ strsplit(x,split=" ")[[1]][1]  })
genus<-unlist(genus)

traits<-cbind(traits,genus)


moments_output_plastic<-data.frame()
for(n in 1:200){
  for(i in 1:length(unique(biomass$site))){
    site_i<-unique(biomass$site)[i]
    #cover_i<-cover_thin[which(cover_thin$turfID==cover_meta$turfID[i] & cover_thin$year==cover_meta$year[i]),]
    cover_i<-biomass[which(biomass$site==site_i),]
    output_i<-NULL
    traits_i<-data.frame()
    print(c(n,i))
    for(j in 1:nrow(cover_i)){
      
      species_j<-cover_i$speciesName[j]
      genus_j<-strsplit(x = cover_i$speciesName[j],split = " ")[[1]][1]
      
      #first species control at site
      species_j_data<-traits[which(traits$Taxon==species_j & traits$Site==site_i & (traits$Project %in% c("LOCAL","SEAN","0","C","6") | is.na(traits$Project) )  ),]    
      
      #next, genus control at site
      if(nrow(species_j_data)==0){
      
        species_j_data<-traits[which(traits$genus==genus_j & traits$Site==site_i & (traits$Project %in% c("LOCAL","SEAN","0","C","6") | is.na(traits$Project) )  ),]    
        spp_in_set<-unique(species_j_data$Taxon)
        n_samples<-min(unlist(lapply(X = spp_in_set,FUN = function(x){nrow(species_j_data[which(species_j_data$Taxon==x),])[[1]][1]})))
        for(purge in 1: length(spp_in_set)){
          if(length(which(species_j_data$Taxon==spp_in_set[purge]))>n_samples){
            species_j_data<-species_j_data[-sample(x = which(species_j_data$Taxon==spp_in_set[purge]),size=length(which(species_j_data$Taxon==spp_in_set[purge]))-n_samples,replace = F),] 
          }}
        
        
        
        }
      
      #next, species control anywhere
      if(nrow(species_j_data)==0){
        
        species_j_data<-traits[which(traits$Taxon==species_j &  (traits$Project %in% c("LOCAL","SEAN","0","C","6") | is.na(traits$Project) )  ),]    
      }
      
      #next, genus control anywhere
      if(nrow(species_j_data)==0){
        
        species_j_data<-traits[which(traits$genus==genus_j & (traits$Project %in% c("LOCAL","SEAN","0","C","6") | is.na(traits$Project) )  ),]    
        
        #Need to ensure data is equally likely between species
        spp_in_set<-unique(species_j_data$Taxon)
        n_samples<-min(unlist(lapply(X = spp_in_set,FUN = function(x){nrow(species_j_data[which(species_j_data$Taxon==x),])[[1]][1]})))
        for(purge in 1: length(spp_in_set)){
        if(length(which(species_j_data$Taxon==spp_in_set[purge]))>n_samples){
        species_j_data<-species_j_data[-sample(x = which(species_j_data$Taxon==spp_in_set[purge]),size=length(which(species_j_data$Taxon==spp_in_set[purge]))-n_samples,replace = F),] 
        }}
      }  
      
      species_j_cover<-cover_i$cover[j]
      if(is.na(species_j_cover)){species_j_cover<-0}
      
      if(nrow(species_j_data)!=0){
        wet_mass_sample<-sample(x = species_j_data$Wet_Mass_g,size = species_j_cover,replace = T)
        dry_mass_sample<-sample(x = species_j_data$Dry_Mass_g,size = species_j_cover,replace = T)
        area_sample<-sample(x = species_j_data$Leaf_Area_cm2,size = species_j_cover,replace = T)
        thickness_sample<-sample(x = species_j_data$Leaf_Thickness_Ave_mm,size = species_j_cover,replace = T)
        sla_sample<-sample(x = species_j_data$SLA_cm2_g,size = species_j_cover,replace = T)
        ldmc_sample<-sample(x = species_j_data$LDMC,size = species_j_cover,replace = T)
        
        species_j_sample<-cbind(wet_mass_sample,dry_mass_sample,area_sample,thickness_sample,sla_sample,ldmc_sample)
        traits_i<-rbind(traits_i,species_j_sample)
      }else{
        #print(c(species_j," no data for species"))  
      }
      
    }#j
    
    sla_mean<-mean(traits_i$sla_sample,na.rm = T)
    sla_var<-var(traits_i$sla_sample,na.rm = T)
    sla_skew<-skewness(traits_i$sla_sample,na.rm = T)
    sla_kurt<-kurtosis(traits_i$sla_sample,na.rm = T)
    
    ldmc_mean<-mean(traits_i$ldmc_sample,na.rm=T)
    ldmc_var<-var(traits_i$ldmc_sample,na.rm=T)
    ldmc_skew<-skewness(traits_i$ldmc_sample,na.rm=T)
    ldmc_kurt<-kurtosis(traits_i$ldmc_sample,na.rm=T)
    
    area_mean<-mean(traits_i$area_sample,na.rm=T)
    area_var<-var(traits_i$area_sample,na.rm=T)
    area_skew<-skewness(traits_i$area_sample,na.rm=T)
    area_kurt<-kurtosis(traits_i$area_sample,na.rm=T)
    
    thickness_mean<-mean(traits_i$thickness_sample,na.rm=T)
    thickness_var<-var(traits_i$thickness_sample,na.rm=T)
    thickness_skew<-skewness(traits_i$thickness_sample,na.rm=T)
    thickness_kurt<-kurtosis(traits_i$thickness_sample,na.rm=T)
    
    
    
    output_i<-cbind(unique(cover_i[1:9]),sla_mean,sla_var,sla_skew,sla_kurt,ldmc_mean,ldmc_var,ldmc_skew,
                    ldmc_kurt,area_mean,area_var,area_skew,area_kurt,thickness_mean,thickness_var,thickness_skew,thickness_kurt)
    moments_output_plastic<-rbind(moments_output_plastic,output_i)
    
    
  }#i
  
}#n  

