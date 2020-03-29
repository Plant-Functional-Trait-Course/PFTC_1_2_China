#China trait distribution bootstrapping

traits<-read.csv("traits/traits.csv")
traits$Taxon<-as.character(traits$Taxon)


source("setup.R")



cover_thin$species<-gsub(pattern = taxon$speciesName,replacement = taxon$species,x = cover_thin$species)


#run "start_here"

cover_meta

library(moments)
taxon<-dbGetQuery(conn = con,statement = "SELECT * FROM taxon;")

#  remove "gramineae" "sedge"  
species_to_remove<-taxon$speciesName[taxon$functionalGroup%in%c("gramineae","sedge")]
cover_thin_2<-cover_thin[which(!cover_thin$speciesName%in%species_to_remove),]
cover_thin<-cover_thin_2


#fix treatments names on traits
old_traits_names<-c("LOCAL","SEAN","3","0","$C","OTC","1","2","Unknown","6","4")
new_traits_names<-c("LOCAL","SEAN","warm3","local","control","OTC","warm1","cool1","Unknown","6","cool3")
name_translator<-cbind(old_traits_names,new_traits_names)

for(i in 1:nrow(name_translator)){
traits$Project<-gsub(traits$Project,pattern = name_translator[,1][i],replacement = name_translator[,2][i])    
}

rm(old_traits_names,new_traits_names,name_translator)


moments_output_plastic<-data.frame()
for(n in 1:200){
for(i in 1:nrow(cover_meta)){
cover_i<-cover_thin[which(cover_thin$turfID==cover_meta$turfID[i] & cover_thin$year==cover_meta$year[i]),]
output_i<-NULL
traits_i<-data.frame()
print(c(n,i))
for(j in 1:nrow(cover_i)){

species_j<-cover_i$speciesName[j]

#first treatment x recipient site
species_j_data<-traits[which(traits$Taxon==species_j & traits$Site==cover_i$destSiteID[j] & cover_i$TTtreat[j]==traits$Project),]    
 
#second controls + local at recipient site
if(nrow(species_j_data)==0){
  species_j_data<-traits[which(traits$Taxon==species_j & 
                    (traits$Project=="local"|traits$Project=="control"|traits$Project=="LOCAL") & traits$Site==cover_i$destSiteID[j] ),]      
}

#third global controls + local
if(nrow(species_j_data)==0){
  species_j_data<-traits[which(traits$Taxon==species_j & (traits$Project=="local"|traits$Project=="control"|traits$Project=="LOCAL")),]      
}

#fourth whereever
if(nrow(species_j_data)==0){
  species_j_data<-traits[which(traits$Taxon==species_j ),]      
}

if(nrow(species_j_data)==0){
species_j_data<-traits[which(traits$Taxon==species_j),]  }
species_j_cover<-cover_i$cover[j]

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
  
######

write.csv(x = moments_output_plastic,file = "china_bootstrap_moments_plastic_4_6_2016.csv",row.names = F)



######

#compare 2016 control data at elevation

data_2016<-moments_output_plastic[moments_output_plastic$year==2016&moments_output_plastic$TTtreat=="control",]

boxplot(data_2016$sla_mean~data_2016$destSiteID)
boxplot(data_2016$sla_var~data_2016$destSiteID)
boxplot(data_2016$sla_skew~data_2016$destSiteID)
boxplot(data_2016$sla_kurt~data_2016$destSiteID)

boxplot(data_2016$ldmc_mean~data_2016$destSiteID)
boxplot(data_2016$ldmc_var~data_2016$destSiteID)
boxplot(data_2016$ldmc_skew~data_2016$destSiteID)
boxplot(data_2016$ldmc_kurt~data_2016$destSiteID)

#####

warm_all<-moments_output_plastic[moments_output_plastic$TTtreat=="warm1",]

boxplot(warm_all$sla_mean~warm_all$year+warm_all$originSiteID)
boxplot(warm_all$sla_var~warm_all$year+warm_all$originSiteID)
boxplot(warm_all$sla_skew~warm_all$year+warm_all$originSiteID)
boxplot(warm_all$sla_kurt~warm_all$year+warm_all$originSiteID)

boxplot(warm_all$ldmc_mean~warm_all$year+warm_all$originSiteID)
boxplot(warm_all$ldmc_var~warm_all$year+warm_all$originSiteID)
boxplot(warm_all$ldmc_skew~warm_all$year+warm_all$originSiteID)
boxplot(warm_all$ldmc_kurt~warm_all$year+warm_all$originSiteID)

###

warm3_all<-moments_output_plastic[moments_output_plastic$TTtreat=="warm3",]

boxplot(warm3_all$sla_mean~warm3_all$year+warm3_all$originSiteID)
boxplot(warm3_all$sla_var~warm3_all$year+warm3_all$originSiteID)
boxplot(warm3_all$sla_skew~warm3_all$year+warm3_all$originSiteID)
boxplot(warm3_all$sla_kurt~warm3_all$year+warm3_all$originSiteID)

boxplot(warm3_all$ldmc_mean~warm3_all$year+warm3_all$originSiteID)
boxplot(warm3_all$ldmc_var~warm3_all$year+warm3_all$originSiteID)
boxplot(warm3_all$ldmc_skew~warm3_all$year+warm3_all$originSiteID)
boxplot(warm3_all$ldmc_kurt~warm3_all$year+warm3_all$originSiteID)

###


otc_all<-moments_output_plastic[moments_output_plastic$TTtreat=="OTC",]

boxplot(otc_all$sla_mean~otc_all$year+otc_all$originSiteID)
boxplot(otc_all$sla_var~otc_all$year+otc_all$originSiteID)
boxplot(otc_all$sla_skew~otc_all$year+otc_all$originSiteID)
boxplot(otc_all$sla_kurt~otc_all$year+otc_all$originSiteID)

boxplot(otc_all$ldmc_mean~otc_all$year+otc_all$originSiteID)
boxplot(otc_all$ldmc_var~otc_all$year+otc_all$originSiteID)
boxplot(otc_all$ldmc_skew~otc_all$year+otc_all$originSiteID)
boxplot(otc_all$ldmc_kurt~otc_all$year+otc_all$originSiteID)






#####################

####################
#A: Fixed
#first treatment x home site
#second controls + local at home site
#third global controls + local
#fourth whereever









#B: Plasticity
#first treatment x recipient site
#second controls + local at recipient site
#third global controls + local
#fourth whereever


