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

##################
#Changes to make

#Add a batch of code that uses percent cover (*10?)  May need to multiply since many species occur in less than one percent of plot

####################################################################

#Moments output: from biomass sampling

#For each plot (not the site containing them):
  #Make output file 
  #For each replicate:  
    # Make an empty output dataframe (ncols=ntraits )
      # For each species:
        # Pull out the species cover data from that plot
        # Identify the best available trait data for that species
        # Sample from trait data proportional to cover
        # Add sample to output dataframe
      # Calculate mean,median,variance,skewness for each trait
      # Bind: site, plot, replicate, mean, meadian, variance, skewness together, add to output dataframe


#Bootstrapped data based on biomass dataset

library(moments)

#Load biomass data
load("C:/Users/Brian/Dropbox/transplant/USE THIS DATA/biomass_cleaned.Rdata")
load("C:/Users/Brian/Dropbox/transplant/USE THIS DATA/traits.Rdata")

unique(biomass$site)
bad_spp<-unique(biomass$speciesName[which(!biomass$speciesName %in% traits$Taxon)])
un_spp<-unique(biomass$speciesName[grep(pattern = "Unk",x = biomass$speciesName)])





#How good is trait sampling?

for(i in 1:4){
  site<-c("H","A","M","L")[i]  
  data_i<-biomass[which(biomass$site==site),]
  good_data_i<-sum(data_i$biomass[which(!data_i$speciesName%in%bad_spp)],na.rm = T  )
  bad_data_i<-sum(data_i$biomass[which(data_i$speciesName%in%bad_spp)],na.rm = T  )  
  #bad_data_i<-sum(data_i$biomass[which(data_i$speciesName%in%un_spp)],na.rm = T  )  
  
  print(paste("site ",site, bad_data_i/(good_data_i+bad_data_i)*100, " percent bad data"))

}
#Looks like we've got a lot of biomass in "Genus_spp" format, so have to add in a genus level sampling scheme
#use any Project that isn't OTC or 1:4

# How good is trait sampling?  How many species from each site (community data) have species data?  Genus data?
trait_coverage_table<-as.data.frame(matrix(nrow = 4,ncol = 4))
rownames(trait_coverage_table)<-c("H","A","M","L")
colnames(trait_coverage_table)<-c(" Species w/ data", "Genera w/ data","Biomass spp w/ data","Biomass genera w/ data")

for(i in 1:4){
  site<-c("H","A","M","L")[i]  
  data_i<-biomass[which(biomass$site==site),]
  all_spp <- unique(data_i$speciesName)
  good_spp <- all_spp[which(all_spp%in%traits$Taxon)]
  all_genera <- unlist(lapply(X = all_spp, FUN = function(x){strsplit(x,split = " ")[[1]][[1]]}))
  trait_genera <- unlist(lapply(X = unique(traits$Taxon), FUN = function(x){strsplit(x,split = " ")[[1]][[1]]}))
  good_genera <- all_genera[which(all_genera%in%trait_genera)]
  
  good_sp_data<- data_i[which(data_i$speciesName%in%good_spp),]
  good_genera_data<-data_i[which(unlist(lapply(X = data_i$speciesName, FUN = function(x){strsplit(x,split = " ")[[1]][[1]]})) %in% good_genera),]
  
  
  
  #good_data_i<-sum(data_i$biomass[which(!data_i$speciesName%in%bad_spp)],na.rm = T  )
  #bad_data_i<-sum(data_i$biomass[which(data_i$speciesName%in%bad_spp)],na.rm = T  )  
  #bad_data_i<-sum(data_i$biomass[which(data_i$speciesName%in%un_spp)],na.rm = T  )  
  #print(paste("site ",site, bad_data_i/(good_data_i+bad_data_i)*100, " percent bad data"))
  
  print(paste("site ",site, length(good_spp)/(length(all_spp))*100, " percent species with trait data"))
  print(paste("site ",site, length(good_genera)/(length(all_genera))*100, " percent genera with trait data"))
  print(paste("site ",site, sum(good_sp_data$biomass,na.rm = T)/(sum(data_i$biomass,na.rm = T))*100, " percent biomass from spp with trait data"))
  print(paste("site ",site, sum(good_genera_data$biomass,na.rm = T)/(sum(data_i$biomass,na.rm = T))*100, " percent biomass from genera with trait data"))

  trait_coverage_table[i,1]<-round(length(good_spp)/(length(all_spp))*100,digits = 2)
  trait_coverage_table[i,2]<-round(length(good_genera)/(length(all_genera))*100,digits = 2)  
  trait_coverage_table[i,3]<-round(sum(good_sp_data$biomass,na.rm = T)/(sum(data_i$biomass,na.rm = T))*100,digits = 2)
  trait_coverage_table[i,4]<-round(sum(good_genera_data$biomass,na.rm = T)/(sum(data_i$biomass,na.rm = T))*100,digits = 2)
}

rm(good_genera_data,good_sp_data,all_genera,all_spp,good_genera,good_spp,trait_genera)




#add genus name to traits table for convenience
genus<-lapply(X = traits$Taxon,FUN = function(x){ strsplit(x,split=" ")[[1]][1]  })
genus<-unlist(genus)
traits<-cbind(traits,genus)
rm(genus)

#add N:P to traits table
traits$NP_ratio<-traits$N_percent/traits$P_AVG


#remove observations with SLA values over 500 and under 5, along with LDMC values over 1.
traits$SLA[which(traits$SLA_cm2_g > 500 | traits$SLA_cm2_g < 5)]<-NA
traits$LDMC[which(traits$LDMC>1)]<-NA

moments_output_plastic<-data.frame()
rm(bad_data_i,bad_spp,good_data_i,i,site,un_spp,data_i)


#For each plot (not the site containing them):
#Make output file 
#For each replicate:  
# Make an empty output dataframe (ncols=ntraits )
# For each species:
# Pull out the species cover data from that plot
# Identify the best available trait data for that species
# Sample from trait data proportional to cover
# Add sample to output dataframe
# Calculate mean,median,variance,skewness for each trait
# Bind: site, plot, replicate, mean, meadian, variance, skewness together, add to output dataframe

n_reps=1000
for(n in 1: n_reps ){
  for( i in 1: nrow(unique(biomass[c('site','plot')]))){
  site_i<-unique(biomass[c('site','plot')])[i,]
  #cover_i<-cover_thin[which(cover_thin$turfID==cover_meta$turfID[i] & cover_thin$year==cover_meta$year[i]),]
  cover_i<-biomass[which(biomass$site==site_i$site & biomass$plot==site_i$plot),]
  output_i<-NULL
  traits_i<-data.frame()
  print(c(n,i))

  
  for(j in 1:nrow(cover_i)){
    
    species_j<-cover_i$speciesName[j]
    genus_j<-strsplit(x = cover_i$speciesName[j],split = " ")[[1]][1]
    
    #first species control at site
    species_j_data<-traits[which(traits$Taxon==species_j & traits$Site==site_i$site & (traits$Project %in% c("LOCAL","SEAN","0","C","6") | is.na(traits$Project) )  & !is.na(traits$C_percent) & !is.na(traits$P_AVG)  ),]    
    
    #next, genus control at site
    if(nrow(species_j_data)==0){
      
      species_j_data<-traits[which(traits$genus==genus_j & traits$Site==site_i$site & (traits$Project %in% c("LOCAL","SEAN","0","C","6") | is.na(traits$Project) )  & !is.na(traits$C_percent)  & !is.na(traits$P_AVG) ),]    
      
      #Exclude traits values here
      
      
      #This next bit rarifies trait data across species within the genus to be used as a proxy, so that genus mean isn't biased by more abundant species
      if(nrow(species_j_data)!=0){
        spp_in_set<-unique(species_j_data$Taxon)
        n_samples<-min(unlist(lapply(X = spp_in_set,FUN = function(x){nrow(species_j_data[which(species_j_data$Taxon==x),])[[1]][1]})))
        for(purge in 1: length(spp_in_set)){
          if(length(which(species_j_data$Taxon==spp_in_set[purge]))>n_samples){
            species_j_data<-species_j_data[-sample(x = which(species_j_data$Taxon==spp_in_set[purge]),size=length(which(species_j_data$Taxon==spp_in_set[purge]))-n_samples,replace = F),] 
          }}
        
      }
      
    }
    
    #next, species control anywhere
    if(nrow(species_j_data)==0){
      
      species_j_data<-traits[which(traits$Taxon==species_j &  (traits$Project %in% c("LOCAL","SEAN","0","C","6") | is.na(traits$Project) )  & !is.na(traits$C_percent)  & !is.na(traits$P_percent) & !is.na(traits$P_AVG) ),]    
    }
    
    #next, genus control anywhere
    if(nrow(species_j_data)==0){
      
      species_j_data<-traits[which(traits$genus==genus_j & (traits$Project %in% c("LOCAL","SEAN","0","C","6") | is.na(traits$Project) ) & !is.na(traits$C_percent) & !is.na(traits$P_AVG)),]    
      
      
      if(nrow(species_j_data)!=0){
        #Need to ensure data is equally likely between species
        spp_in_set<-unique(species_j_data$Taxon)
        n_samples<-min(unlist(lapply(X = spp_in_set,FUN = function(x){nrow(species_j_data[which(species_j_data$Taxon==x),])[[1]][1]})))
        for(purge in 1: length(spp_in_set)){
          if(length(which(species_j_data$Taxon==spp_in_set[purge]))>n_samples){
            species_j_data<-species_j_data[-sample(x = which(species_j_data$Taxon==spp_in_set[purge]),size=length(which(species_j_data$Taxon==spp_in_set[purge]))-n_samples,replace = F),] 
          }}
      }  
    }  
    
    species_j_cover<-cover_i$cover[j]
    if(is.na(species_j_cover)){species_j_cover<-0}
    
    
    
    
    if(nrow(species_j_data)!=0){
      
      wet_mass_sample<-sample(x = na.omit(species_j_data$Wet_Mass_g),size = species_j_cover,replace = T)
      dry_mass_sample<-sample(x = na.omit(species_j_data$Dry_Mass_g),size = species_j_cover,replace = T)
      area_sample<-sample(x = na.omit(species_j_data$Leaf_Area_cm2),size = species_j_cover,replace = T)
      thickness_sample<-sample(x = na.omit(species_j_data$Leaf_Thickness_Ave_mm),size = species_j_cover,replace = T)
      sla_sample<-sample(x = na.omit(species_j_data$SLA_cm2_g),size = species_j_cover,replace = T)
      ldmc_sample<-sample(x = na.omit(species_j_data$LDMC),size = species_j_cover,replace = T)
      c_percent_sample<-sample(x = na.omit(species_j_data$C_percent),size = species_j_cover,replace = T)
      n_percent_sample<-sample(x = na.omit(species_j_data$N_percent),size = species_j_cover,replace = T)
      p_percent_sample<-sample(x = na.omit(species_j_data$P_AVG),size = species_j_cover,replace = T)
      dc13_percent_sample<-sample(x = na.omit(species_j_data$dC13_percent),size = species_j_cover,replace = T)
      dn15_percent_sample<-sample(x = na.omit(species_j_data$dN15_percent),size = species_j_cover,replace = T)
      cn_ratio_sample<-sample(x = na.omit(species_j_data$CN_ratio),size = species_j_cover,replace = T)
      
      
      species_j_sample<-cbind(wet_mass_sample,dry_mass_sample,area_sample,thickness_sample,sla_sample,ldmc_sample,
                              c_percent_sample,n_percent_sample,p_percent_sample,cn_ratio_sample,dc13_percent_sample,dn15_percent_sample)
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
  
  n_pct_mean<-mean(traits_i$n_percent_sample,na.rm=T)
  n_pct_var<-var(traits_i$n_percent_sample,na.rm=T)
  n_pct_skew<-skewness(traits_i$n_percent_sample,na.rm=T)
  n_pct_kurt<-kurtosis(traits_i$n_percent_sample,na.rm=T)
  
  c_pct_mean<-mean(traits_i$c_percent_sample,na.rm=T)
  c_pct_var<-var(traits_i$c_percent_sample,na.rm=T)
  c_pct_skew<-skewness(traits_i$c_percent_sample,na.rm=T)
  c_pct_kurt<-kurtosis(traits_i$c_percent_sample,na.rm=T)
  
  p_pct_mean<-mean(traits_i$p_percent_sample,na.rm=T)
  p_pct_var<-var(traits_i$p_percent_sample,na.rm=T)
  p_pct_skew<-skewness(traits_i$p_percent_sample,na.rm=T)
  p_pct_kurt<-kurtosis(traits_i$p_percent_sample,na.rm=T)
  
  dn15_pct_mean<-mean(traits_i$dn15_percent_sample,na.rm=T)
  dn15_pct_var<-var(traits_i$dn15_percent_sample,na.rm=T)
  dn15_pct_skew<-skewness(traits_i$dn15_percent_sample,na.rm=T)
  dn15_pct_kurt<-kurtosis(traits_i$dn15_percent_sample,na.rm=T)
  
  dc13_pct_mean<-mean(traits_i$dc13_percent_sample,na.rm=T)
  dc13_pct_var<-var(traits_i$dc13_percent_sample,na.rm=T)
  dc13_pct_skew<-skewness(traits_i$dc13_percent_sample,na.rm=T)
  dc13_pct_kurt<-kurtosis(traits_i$dc13_percent_sample,na.rm=T)
  
  cn_ratio_mean<-mean(traits_i$cn_ratio_sample,na.rm=T)
  cn_ratio_var<-var(traits_i$cn_ratio_sample,na.rm=T)
  cn_ratio_skew<-skewness(traits_i$cn_ratio_sample,na.rm=T)
  cn_ratio_kurt<-kurtosis(traits_i$cn_ratio_sample,na.rm=T)
  
  
  
  output_i<-cbind(n,as.character(site_i$site),site_i$plot,sla_mean,sla_var,sla_skew,sla_kurt,ldmc_mean,ldmc_var,ldmc_skew,
                  ldmc_kurt,area_mean,area_var,area_skew,area_kurt,thickness_mean,thickness_var,thickness_skew,thickness_kurt,
                  c_pct_mean,c_pct_var,c_pct_skew,c_pct_kurt,n_pct_mean,n_pct_var,n_pct_skew,n_pct_kurt,p_pct_mean,p_pct_var,p_pct_skew,p_pct_kurt,
                  dc13_pct_mean,dc13_pct_var,dc13_pct_skew,dc13_pct_kurt,dn15_pct_mean,dn15_pct_var,dn15_pct_skew,dn15_pct_kurt,
                  cn_ratio_mean,cn_ratio_var,cn_ratio_skew,cn_ratio_kurt)
  
  
  
  moments_output_plastic<-rbind(moments_output_plastic,output_i)
  

  
    
    
    
}#for each plot loop  (i)
  
}#nreps loop (n)




names(moments_output_plastic)[1]<-"replicate"
names(moments_output_plastic)[2]<-"site"
names(moments_output_plastic)[3]<-"plot"

rm(output_i,site_i,species_j_data,species_j_sample,cover_i,traits_i,area_kurt,area_mean,area_sample,area_skew,area_var)
rm(dry_mass_sample,genus_j,i,j,ldmc_kurt, ldmc_mean, ldmc_sample,ldmc_skew,ldmc_var,n,n_reps,n_samples,purge)
rm(sla_kurt,sla_mean,sla_sample,sla_skew,sla_var,species_j,species_j_cover,spp_in_set,thickness_kurt,thickness_mean,thickness_sample,thickness_skew,thickness_var,wet_mass_sample)
rm(c_pct_kurt,c_pct_mean,c_pct_skew,c_pct_var,n_pct_kurt,n_pct_mean,n_pct_skew,n_pct_var,cn_ratio_kurt,cn_ratio_mean,cn_ratio_skew,cn_ratio_var)
rm(c_percent_sample,cn_ratio_sample,dc13_percent_sample,dn15_percent_sample,n_percent_sample,p_pct_kurt,p_pct_mean,p_pct_skew,p_pct_var)
rm(dc13_pct_kurt,dc13_pct_mean,dc13_pct_skew,dc13_pct_var,dn15_pct_kurt,dn15_pct_mean,dn15_pct_skew,dn15_pct_var,p_percent_sample)

#Now, need to summarize the data usefully.

# For each trait x moment x site
  # min;mean;max
moment_plastic_summary_site_level<-NULL
moment_plastic_summary_plot_level<-NULL

#Site Level
for(i in 1: length(unique(moments_output_plastic$site))){
  
data_i<-moments_output_plastic[which(moments_output_plastic$site==unique(moments_output_plastic$site)[i]),]  
site<-as.character(unique(moments_output_plastic$site)[i])

for(j in 4:ncol(moments_output_plastic)){

variable<-colnames(data_i)[j]
sort_j<-sort(as.numeric(as.character(data_i[,j])))  
mean_val<-mean(sort_j)

to_remove<-length(sort_j)*.025 #to get 95% ci, need to remove top and bottom 2.5 percent of records
sort_j<-sort_j[-1:-to_remove]#remove bottom 2.5%
#should be removing 65 entries

sort_j<-sort_j[-(length(sort_j)-to_remove+1):-length(sort_j)]
lower_95_ci<- min(sort_j) 
upper_95_ci<-max(sort_j)  


output<-cbind(site,variable,lower_95_ci,mean_val,upper_95_ci)
print("   ")
print(c(site,variable))
print(output)


moment_plastic_summary_site_level<-rbind(moment_plastic_summary_site_level,output)


}}


moment_plastic_summary_site_level<-as.data.frame(moment_plastic_summary_site_level)


#Plot level

for(i in 1: nrow(unique(moments_output_plastic[c('site','plot')]))){
  
  data_i<-NULL
  site<-NULL
  plot<-NULL
  site<-as.character(unique(moments_output_plastic[c('site','plot')])[i,'site'])
  plot<-as.character(unique(moments_output_plastic[c('site','plot')])[i,'plot'])
  data_i<-moments_output_plastic[which(as.character(moments_output_plastic$site)==site & as.character(moments_output_plastic$plot)==plot),]
  
  for(j in 4:ncol(moments_output_plastic)){
    
    variable<-colnames(data_i)[j]
    sort_j<-sort(as.numeric(as.character(data_i[,j])))  
    mean_val<-mean(sort_j)
    
    to_remove<-length(sort_j)*.025 #to get 95% ci, need to remove top and bottom 2.5 percent of records
    sort_j<-sort_j[-1:-to_remove]#remove bottom 2.5%
    #should be removing 65 entries
    
    sort_j<-sort_j[-(length(sort_j)-to_remove+1):-length(sort_j)]
    lower_95_ci<- min(sort_j) 
    upper_95_ci<-max(sort_j)  
    
    
    output<-cbind(site,plot,variable,lower_95_ci,mean_val,upper_95_ci)
    #print("   ")
    print(c(site,plot,variable))
    #print(output)
    
    
    moment_plastic_summary_plot_level<-rbind(moment_plastic_summary_plot_level,output)
    
    
  }}


moment_plastic_summary_plot_level<-as.data.frame(moment_plastic_summary_plot_level)
rm(i,j,lower_95_ci,mean_val,plot,site,sort_j,to_remove,upper_95_ci,variable,output,data_i)

write.csv(x = moment_plastic_summary_site_level,file = "C:/Users/Brian/Desktop/current_projects/transplant/site_bootstrapped_moments_2_26_2018.csv")
write.csv(x = moment_plastic_summary_site_level,file = "C:/Users/Brian/Desktop/current_projects/transplant/plot_bootstrapped_moments_2_26_2018.csv")

rm(moment_plastic_summary_plot_level,moment_plastic_summary_site_level,moments_output_plastic)

####################################################################

#Biomass based estimates

#Set adjustable parameters
biomass_multiplier = 10
n_reps=1000

#Make output files
moments_output_biomass<-data.frame()

#Run stuff
for(n in 1: n_reps ){
  for( i in 1: nrow(unique(biomass[c('site','plot')]))){
    site_i<-unique(biomass[c('site','plot')])[i,]
    #cover_i<-cover_thin[which(cover_thin$turfID==cover_meta$turfID[i] & cover_thin$year==cover_meta$year[i]),]
    cover_i<-biomass[which(biomass$site==site_i$site & biomass$plot==site_i$plot),]
    output_i<-NULL
    traits_i<-data.frame()
    print(c(n,i))
    
    
    for(j in 1:nrow(cover_i)){
      
      species_j<-cover_i$speciesName[j]
      genus_j<-strsplit(x = cover_i$speciesName[j],split = " ")[[1]][1]
      
      #first species control at site
      species_j_data<-traits[which(traits$Taxon==species_j & traits$Site==site_i$site & (traits$Project %in% c("LOCAL","SEAN","0","C","6") | is.na(traits$Project) )  & !is.na(traits$C_percent) & !is.na(traits$P_AVG)  ),]    
      
      #next, genus control at site
      if(nrow(species_j_data)==0){
        
        species_j_data<-traits[which(traits$genus==genus_j & traits$Site==site_i$site & (traits$Project %in% c("LOCAL","SEAN","0","C","6") | is.na(traits$Project) )  & !is.na(traits$C_percent)  & !is.na(traits$P_AVG) ),]    
        
        #Exclude traits values here
        
        
        #This next bit rarifies trait data across species within the genus to be used as a proxy, so that genus mean isn't biased by more abundant species
        if(nrow(species_j_data)!=0){
          spp_in_set<-unique(species_j_data$Taxon)
          n_samples<-min(unlist(lapply(X = spp_in_set,FUN = function(x){nrow(species_j_data[which(species_j_data$Taxon==x),])[[1]][1]})))
          for(purge in 1: length(spp_in_set)){
            if(length(which(species_j_data$Taxon==spp_in_set[purge]))>n_samples){
              species_j_data<-species_j_data[-sample(x = which(species_j_data$Taxon==spp_in_set[purge]),size=length(which(species_j_data$Taxon==spp_in_set[purge]))-n_samples,replace = F),] 
            }}
          
        }
        
      }
      
      #next, species control anywhere
      if(nrow(species_j_data)==0){
        
        species_j_data<-traits[which(traits$Taxon==species_j &  (traits$Project %in% c("LOCAL","SEAN","0","C","6") | is.na(traits$Project) )  & !is.na(traits$C_percent)  & !is.na(traits$P_percent) & !is.na(traits$P_AVG) ),]    
      }
      
      #next, genus control anywhere
      if(nrow(species_j_data)==0){
        
        species_j_data<-traits[which(traits$genus==genus_j & (traits$Project %in% c("LOCAL","SEAN","0","C","6") | is.na(traits$Project) ) & !is.na(traits$C_percent) & !is.na(traits$P_AVG)),]    
        
        
        if(nrow(species_j_data)!=0){
          #Need to ensure data is equally likely between species
          spp_in_set<-unique(species_j_data$Taxon)
          n_samples<-min(unlist(lapply(X = spp_in_set,FUN = function(x){nrow(species_j_data[which(species_j_data$Taxon==x),])[[1]][1]})))
          for(purge in 1: length(spp_in_set)){
            if(length(which(species_j_data$Taxon==spp_in_set[purge]))>n_samples){
              species_j_data<-species_j_data[-sample(x = which(species_j_data$Taxon==spp_in_set[purge]),size=length(which(species_j_data$Taxon==spp_in_set[purge]))-n_samples,replace = F),] 
            }}
        }  
      }  
      
      #species_j_cover<-cover_i$cover[j]
      species_j_biomass<-cover_i$biomass[j]*biomass_multiplier
      
      if(is.na(species_j_biomass)){species_j_biomass<-0}
      
      
      
      
      if(nrow(species_j_data)!=0){
        
        wet_mass_sample<-sample(x = na.omit(species_j_data$Wet_Mass_g),size = species_j_biomass,replace = T)
        dry_mass_sample<-sample(x = na.omit(species_j_data$Dry_Mass_g),size = species_j_biomass,replace = T)
        area_sample<-sample(x = na.omit(species_j_data$Leaf_Area_cm2),size = species_j_biomass,replace = T)
        thickness_sample<-sample(x = na.omit(species_j_data$Leaf_Thickness_Ave_mm),size = species_j_biomass,replace = T)
        sla_sample<-sample(x = na.omit(species_j_data$SLA_cm2_g),size = species_j_biomass,replace = T)
        ldmc_sample<-sample(x = na.omit(species_j_data$LDMC),size = species_j_biomass,replace = T)
        c_percent_sample<-sample(x = na.omit(species_j_data$C_percent),size = species_j_biomass,replace = T)
        n_percent_sample<-sample(x = na.omit(species_j_data$N_percent),size = species_j_biomass,replace = T)
        p_percent_sample<-sample(x = na.omit(species_j_data$P_AVG),size = species_j_biomass,replace = T)
        dc13_percent_sample<-sample(x = na.omit(species_j_data$dC13_percent),size = species_j_biomass,replace = T)
        dn15_percent_sample<-sample(x = na.omit(species_j_data$dN15_percent),size = species_j_biomass,replace = T)
        cn_ratio_sample<-sample(x = na.omit(species_j_data$CN_ratio),size = species_j_biomass,replace = T)
        np_ratio_sample<-sample(x = na.omit(species_j_data$NP_ratio),size = species_j_biomass,replace = T)
        
        species_j_sample<-cbind(wet_mass_sample,dry_mass_sample,area_sample,thickness_sample,sla_sample,ldmc_sample,
                                c_percent_sample,n_percent_sample,p_percent_sample,cn_ratio_sample,dc13_percent_sample,dn15_percent_sample,np_ratio_sample)
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
    
    n_pct_mean<-mean(traits_i$n_percent_sample,na.rm=T)
    n_pct_var<-var(traits_i$n_percent_sample,na.rm=T)
    n_pct_skew<-skewness(traits_i$n_percent_sample,na.rm=T)
    n_pct_kurt<-kurtosis(traits_i$n_percent_sample,na.rm=T)
    
    c_pct_mean<-mean(traits_i$c_percent_sample,na.rm=T)
    c_pct_var<-var(traits_i$c_percent_sample,na.rm=T)
    c_pct_skew<-skewness(traits_i$c_percent_sample,na.rm=T)
    c_pct_kurt<-kurtosis(traits_i$c_percent_sample,na.rm=T)
    
    p_pct_mean<-mean(traits_i$p_percent_sample,na.rm=T)
    p_pct_var<-var(traits_i$p_percent_sample,na.rm=T)
    p_pct_skew<-skewness(traits_i$p_percent_sample,na.rm=T)
    p_pct_kurt<-kurtosis(traits_i$p_percent_sample,na.rm=T)
    
    dn15_pct_mean<-mean(traits_i$dn15_percent_sample,na.rm=T)
    dn15_pct_var<-var(traits_i$dn15_percent_sample,na.rm=T)
    dn15_pct_skew<-skewness(traits_i$dn15_percent_sample,na.rm=T)
    dn15_pct_kurt<-kurtosis(traits_i$dn15_percent_sample,na.rm=T)
    
    dc13_pct_mean<-mean(traits_i$dc13_percent_sample,na.rm=T)
    dc13_pct_var<-var(traits_i$dc13_percent_sample,na.rm=T)
    dc13_pct_skew<-skewness(traits_i$dc13_percent_sample,na.rm=T)
    dc13_pct_kurt<-kurtosis(traits_i$dc13_percent_sample,na.rm=T)
    
    cn_ratio_mean<-mean(traits_i$cn_ratio_sample,na.rm=T)
    cn_ratio_var<-var(traits_i$cn_ratio_sample,na.rm=T)
    cn_ratio_skew<-skewness(traits_i$cn_ratio_sample,na.rm=T)
    cn_ratio_kurt<-kurtosis(traits_i$cn_ratio_sample,na.rm=T)
    
    np_ratio_mean<-mean(traits_i$np_ratio_sample,na.rm=T)
    np_ratio_var<-var(traits_i$np_ratio_sample,na.rm=T)
    np_ratio_skew<-skewness(traits_i$np_ratio_sample,na.rm=T)
    np_ratio_kurt<-kurtosis(traits_i$np_ratio_sample,na.rm=T)
    
    
    output_i<-cbind(n,as.character(site_i$site),site_i$plot,sla_mean,sla_var,sla_skew,sla_kurt,ldmc_mean,ldmc_var,ldmc_skew,
                    ldmc_kurt,area_mean,area_var,area_skew,area_kurt,thickness_mean,thickness_var,thickness_skew,thickness_kurt,
                    c_pct_mean,c_pct_var,c_pct_skew,c_pct_kurt,n_pct_mean,n_pct_var,n_pct_skew,n_pct_kurt,p_pct_mean,p_pct_var,p_pct_skew,p_pct_kurt,
                    dc13_pct_mean,dc13_pct_var,dc13_pct_skew,dc13_pct_kurt,dn15_pct_mean,dn15_pct_var,dn15_pct_skew,dn15_pct_kurt,
                    cn_ratio_mean,cn_ratio_var,cn_ratio_skew,cn_ratio_kurt,np_ratio_mean,np_ratio_var,np_ratio_skew,np_ratio_kurt)
    
    
    
    moments_output_biomass<-rbind(moments_output_biomass,output_i)
    
    
    
    
    
    
  }#for each plot loop  (i)
  
}#nreps loop (n)




names(moments_output_biomass)[1]<-"replicate"
names(moments_output_biomass)[2]<-"site"
names(moments_output_biomass)[3]<-"plot"

rm(output_i,site_i,species_j_data,species_j_sample,cover_i,traits_i,area_kurt,area_mean,area_sample,area_skew,area_var)
rm(dry_mass_sample,genus_j,i,j,ldmc_kurt, ldmc_mean, ldmc_sample,ldmc_skew,ldmc_var,n,n_reps,n_samples,purge)
rm(sla_kurt,sla_mean,sla_sample,sla_skew,sla_var,species_j,species_j_biomass,spp_in_set,thickness_kurt,thickness_mean,thickness_sample,thickness_skew,thickness_var,wet_mass_sample)
rm(c_pct_kurt,c_pct_mean,c_pct_skew,c_pct_var,n_pct_kurt,n_pct_mean,n_pct_skew,n_pct_var,cn_ratio_kurt,cn_ratio_mean,cn_ratio_skew,cn_ratio_var)
rm(c_percent_sample,cn_ratio_sample,dc13_percent_sample,dn15_percent_sample,n_percent_sample,p_pct_kurt,p_pct_mean,p_pct_skew,p_pct_var)
rm(dc13_pct_kurt,dc13_pct_mean,dc13_pct_skew,dc13_pct_var,dn15_pct_kurt,dn15_pct_mean,dn15_pct_skew,dn15_pct_var,p_percent_sample)
rm(np_ratio_mean,np_ratio_skew,np_ratio_var,np_ratio_kurt)
#Now, need to summarize the data usefully.

# For each trait x moment x site
# min;mean;max
moment_plastic_summary_site_level<-NULL
moment_plastic_summary_plot_level<-NULL

#Site Level
for(i in 1: length(unique(moments_output_biomass$site))){
  
  data_i<-moments_output_biomass[which(moments_output_biomass$site==unique(moments_output_biomass$site)[i]),]  
  site<-as.character(unique(moments_output_biomass$site)[i])
  
  for(j in 4:ncol(moments_output_biomass)){
    
    variable<-colnames(data_i)[j]
    sort_j<-sort(as.numeric(as.character(data_i[,j])))  
    mean_val<-mean(sort_j)
    
    to_remove<-length(sort_j)*.025 #to get 95% ci, need to remove top and bottom 2.5 percent of records
    sort_j<-sort_j[-1:-to_remove]#remove bottom 2.5%
    #should be removing 65 entries
    
    sort_j<-sort_j[-(length(sort_j)-to_remove+1):-length(sort_j)]
    lower_95_ci<- min(sort_j) 
    upper_95_ci<-max(sort_j)  
    
    
    output<-cbind(site,variable,lower_95_ci,mean_val,upper_95_ci)
    print("   ")
    print(c(site,variable))
    print(output)
    
    
    moment_plastic_summary_site_level<-rbind(moment_plastic_summary_site_level,output)
    
    
  }}


moment_plastic_summary_site_level<-as.data.frame(moment_plastic_summary_site_level)


#Plot level

for(i in 1: nrow(unique(moments_output_biomass[c('site','plot')]))){
  
  data_i<-NULL
  site<-NULL
  plot<-NULL
  site<-as.character(unique(moments_output_biomass[c('site','plot')])[i,'site'])
  plot<-as.character(unique(moments_output_biomass[c('site','plot')])[i,'plot'])
  data_i<-moments_output_biomass[which(as.character(moments_output_biomass$site)==site & as.character(moments_output_biomass$plot)==plot),]
  
  for(j in 4:ncol(moments_output_biomass)){
    
    variable<-colnames(data_i)[j]
    sort_j<-sort(as.numeric(as.character(data_i[,j])))  
    mean_val<-mean(sort_j)
    
    to_remove<-length(sort_j)*.025 #to get 95% ci, need to remove top and bottom 2.5 percent of records
    sort_j<-sort_j[-1:-to_remove]#remove bottom 2.5%
    #should be removing 65 entries
    
    sort_j<-sort_j[-(length(sort_j)-to_remove+1):-length(sort_j)]
    lower_95_ci<- min(sort_j) 
    upper_95_ci<-max(sort_j)  
    
    
    output<-cbind(site,plot,variable,lower_95_ci,mean_val,upper_95_ci)
    #print("   ")
    print(c(site,plot,variable))
    #print(output)
    
    
    moment_plastic_summary_plot_level<-rbind(moment_plastic_summary_plot_level,output)
    
    
  }}


moment_plastic_summary_plot_level<-as.data.frame(moment_plastic_summary_plot_level)
rm(i,j,lower_95_ci,mean_val,plot,site,sort_j,to_remove,upper_95_ci,variable,output)




####################################################################
#Generate plots of mean, var, skew, kurt for each variable (with error bars)

out_folder<-"Henn_Maitner_trait_distributions/trait_distrubution_plots_2_8_18/"



for(i in 1:length(unique(moment_plastic_summary_site_level$variable))){
  
variable <- as.character(unique(moment_plastic_summary_site_level$variable)[i])
  
  
  
  
}





######################

