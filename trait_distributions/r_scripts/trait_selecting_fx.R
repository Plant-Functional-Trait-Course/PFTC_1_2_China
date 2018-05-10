#Function to generate sets of traits for trait_distribution function to use
#This function is particular to the china data format
#Prioritizes  

#Inputs:
  #species = species vector
  #site = character; the site (H,A,M,L)
  #treatment = "2"     NA      "LOCAL" "SEAN"  "3"     "0"     "C"     "OTC"   "1"     "6"     "4"
#Output: dataframe with
  #row = individuals
  #columns = species, traits

#C control plot
#0 local transplant
#1 transplanted one ‘step’ warmer
#2 transplanted one ‘step’ cooler
#3 transplanted three ‘step’ extreme warmer 
#4 transplanted three ‘step’ extreme cooler
#6 typo: unclear what these are.  Ignore them
#OTC open top chamber warming
#SEAN leaf temperature trait, local
#LOCAL leaf collected from the area surrounding the plots
#NA unknown

#Note: remove any observation with "#zap" note



#For plastic, set site to destination site, if fixed, set site to origin

select_traits<-function(species,site,traits_dataframe){

traits_output<-NULL    

  #Initial stab: if plastic = TRUE, prioritize destination sites
  #1) species in destination site control
  #2) species in any control
  #3) species anywhere
  #4) genus in destination site control
  #5) genus in any site control
  #6) genus anywhere
  

for(i in 1:length(unique(species))){
  
species_i<-unique(species)[i]  
genus_i<-strsplit(x = species_i,split = " ")[[1]][1]

#1) species in destination site control
out_i<-traits_dataframe[which(traits_dataframe$Taxon==species_i & 
                         traits_dataframe$Site == site &
                         traits_dataframe$Project %in% c("SEAN","LOCAL","0","C",NA)),]
  
#2) species in any control
if(nrow(out_i)==0){
  out_i<-traits_dataframe[which(traits_dataframe$Taxon==species_i & 
                                  traits_dataframe$Project %in% c("SEAN","LOCAL","0","C",NA)),]
  
}

#3) species anywhere
if(nrow(out_i)==0){
  out_i<-traits_dataframe[which(traits_dataframe$Taxon==species_i ),]
  
}



#4) genus in destination site control

if(nrow(out_i)==0){

out_i<-traits_dataframe[which( unlist(lapply(X = traits_dataframe$Taxon,FUN = function(x){strsplit(x,split = " ")[[1]][1]} )  )==genus_i & 
                                traits_dataframe$Site == site &
                                traits_dataframe$Project %in% c("SEAN","LOCAL","0","C",NA)),]

}



#5) genus in any site control
if(nrow(out_i)==0){
  
  out_i<-traits_dataframe[which( unlist(lapply(X = traits_dataframe$Taxon,FUN = function(x){strsplit(x,split = " ")[[1]][1]} )  )==genus_i & 
                                   traits_dataframe$Project %in% c("SEAN","LOCAL","0","C",NA)),]
  
}






#6) genus anywhere

if(nrow(out_i)==0){
  
  out_i<-traits_dataframe[which( unlist(lapply(X = traits_dataframe$Taxon,FUN = function(x){strsplit(x,split = " ")[[1]][1]} )  )==genus_i),]
  
}



if(nrow(out_i)!=0){
out_i$assigned_species<-species_i
traits_output<-rbind(traits_output,out_i)
}
  
} #for i species loop 
  
  
return(traits_output)  

  
}
