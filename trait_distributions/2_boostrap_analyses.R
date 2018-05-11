

#Visuals
#overlapping distribution: origin dist, recipient dist, experiment dist

#Plots
# skewness vs time, mean vs time, variance vs delta mean (change in mean from beginning to end)

source("trait_distributions/r_scripts/summarize_moments.R")

file_directory_native<-"trait_distributions/using_native_site/"
file_directory_recipient<-"trait_distributions/using_recipient site//"

moments_fixed<-extract_moments(file_directory = file_directory_native)
moments_plastic<-extract_moments(file_directory = file_directory_recipient)


warmed_1<-subset(x = moments_fixed,subset = moments_fixed$treatment==1)
warmed_3<-subset(x = moments_fixed,subset = moments_fixed$treatment==3)


###############################

#Generate plots of skew vs time


for( i in 1:length(unique(moments_fixed$trait))){
  trait<-as.character(unique(moments_fixed$trait)[i])
    

for( t in 1:length(unique(moments_fixed$treatment))){
  
  treatment <- as.character(unique(moments_fixed$treatment)[t])  
  
  data_it<-moments_fixed[which(moments_fixed$trait==trait & moments_fixed$treatment==treatment),]
    
  plot(as.numeric(as.character(data_it$skew))~data_it$year,xlab="year",ylab="skewness",main=paste(trait,"Treatment ",treatment))
    
  
  
  }#t loop  

}#i loop
