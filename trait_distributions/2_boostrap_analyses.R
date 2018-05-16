

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

windows()
for( i in 1:length(unique(moments_fixed$trait))){
  trait<-as.character(unique(moments_fixed$trait)[i])
    

for( t in 1:length(unique(moments_fixed$treatment))){
  
  treatment <- as.character(unique(moments_fixed$treatment)[t])  
  
  data_it<-moments_fixed[which(moments_fixed$trait==trait & moments_fixed$treatment==treatment),]
  
  #windows()
  plot(as.numeric(as.character(data_it$skew))~data_it$year,xlab="year",ylab="skewness",main=paste(trait,"Treatment ",treatment))
    
  savePlot(filename = paste("C:/Users/Brian/Google Drive/China_PFTC12_distribution_output/skewness_v_time/","assume_fixed_traits_",trait,"treatment_",treatment,sep = ""),type = "jpeg")
  
  
  }#t loop  

}#i loop


#Generate plots of mean vs time

windows()
for( i in 1:length(unique(moments_fixed$trait))){
  trait<-as.character(unique(moments_fixed$trait)[i])
  
  
  for( t in 1:length(unique(moments_fixed$treatment))){
    
    treatment <- as.character(unique(moments_fixed$treatment)[t])  
    
    data_it<-moments_fixed[which(moments_fixed$trait==trait & moments_fixed$treatment==treatment),]
    
    #windows()
    plot(as.numeric(as.character(data_it$mean))~data_it$year,xlab="year",ylab="Mean",main=paste(trait,"Treatment ",treatment))
    
    savePlot(filename = paste("C:/Users/Brian/Google Drive/China_PFTC12_distribution_output/mean_v_time/","assume_fixed_traits_",trait,"_treatment_",treatment,sep = ""),type = "jpeg")
    
    
  }#t loop  
  
}#i loop



#Generate plots of var vs time

windows()
for( i in 1:length(unique(moments_fixed$trait))){
  trait<-as.character(unique(moments_fixed$trait)[i])
  
  
  for( t in 1:length(unique(moments_fixed$treatment))){
    
    treatment <- as.character(unique(moments_fixed$treatment)[t])  
    
    data_it<-moments_fixed[which(moments_fixed$trait==trait & moments_fixed$treatment==treatment),]
    
    #windows()
    plot(as.numeric(as.character(data_it$var))~data_it$year,xlab="year",ylab="Variance",main=paste(trait,"Treatment ",treatment))
    
    savePlot(filename = paste("C:/Users/Brian/Google Drive/China_PFTC12_distribution_output/var_v_time/","assume_fixed_traits_",trait,"_treatment_",treatment,sep = ""),type = "jpeg")
    
    
  }#t loop  
  
}#i loop


#Generate plots of kurtosis vs time

windows()
for( i in 1:length(unique(moments_fixed$trait))){
  trait<-as.character(unique(moments_fixed$trait)[i])
  
  
  for( t in 1:length(unique(moments_fixed$treatment))){
    
    treatment <- as.character(unique(moments_fixed$treatment)[t])  
    
    data_it<-moments_fixed[which(moments_fixed$trait==trait & moments_fixed$treatment==treatment),]
    
    #windows()
    plot(as.numeric(as.character(data_it$kurt))~data_it$year,xlab="year",ylab="Kurtosis",main=paste(trait,"Treatment ",treatment))
    
    savePlot(filename = paste("C:/Users/Brian/Google Drive/China_PFTC12_distribution_output/kurtosis_v_time/","assume_fixed_traits_",trait,"_treatment_",treatment,sep = ""),type = "jpeg")
    
    
  }#t loop  
  
}#i loop

###########################


#For each turf x trait X moment: calculate regression slope

turf_regression_output<-NULL
for(i in 1:length(unique(moments_fixed$turf))){
  
  turf <- as.character(unique(moments_fixed$turf)[i])
  data_i<-moments_fixed[which(moments_fixed$turf==turf),]
  
  treatment <-as.character(unique(data_i$treatment))
  
  traits<-unique(data_i$trait)    

for(t in 1:length(traits)){

trait<-as.character(traits[t])    
trait_data_t<-data_i[which(data_i$trait==trait),]  
lm.mean<-lm(formula = as.numeric(as.character(trait_data_t$mean)) ~ as.numeric(as.character(trait_data_t$year)))
lm.var<-lm(formula = as.numeric(as.character(trait_data_t$var)) ~ as.numeric(as.character(trait_data_t$year)))
lm.skew<-lm(formula = as.numeric(as.character(trait_data_t$skew)) ~ as.numeric(as.character(trait_data_t$year)))
lm.kurt<-lm(formula = as.numeric(as.character(trait_data_t$kurt)) ~ as.numeric(as.character(trait_data_t$year)))  

mean_int<-lm.mean$coefficients[1]
mean_slope<-lm.mean$coefficients[2]
mean_sum<-summary(lm.mean)
mean_r2<-mean_sum$r.squared
mean_slope_p<-mean_sum$coefficients[,4][2]

var_int<-lm.var$coefficients[1]
var_slope<-lm.var$coefficients[2]
var_sum<-summary(lm.var)
var_r2<-var_sum$r.squared
var_slope_p<-var_sum$coefficients[,4][2]

skew_int<-lm.skew$coefficients[1]
skew_slope<-lm.skew$coefficients[2]
skew_sum<-summary(lm.skew)
skew_r2<-skew_sum$r.squared
skew_slope_p<-skew_sum$coefficients[,4][2]

kurt_int<-lm.kurt$coefficients[1]
kurt_slope<-lm.kurt$coefficients[2]
kurt_sum<-summary(lm.kurt)
kurt_r2<-kurt_sum$r.squared
kurt_slope_p<-kurt_sum$coefficients[,4][2]




output_t<-cbind(turf,treatment,trait,
                mean_int,mean_slope,mean_r2,mean_slope_p,
                var_int,var_slope,var_r2,var_slope_p,
                skew_int,skew_slope,skew_r2,skew_slope_p,
                kurt_int,kurt_slope,kurt_r2,kurt_slope_p)



turf_regression_output<-rbind(turf_regression_output,output_t)
  
  
} #trait loop 
  
    
  
  
} #turf loop

turf_regression_output<-as.data.frame(turf_regression_output)

####
library(ggplot2)

treatment_trait<-unique(cbind(as.character(turf_regression_output$trait),as.character(turf_regression_output$treatment)))
for(i in 1:nrow(treatment_trait)){
print(i)
trait_i<-treatment_trait[i,1]  
treatment_i<-treatment_trait[i,2]    
data_i<-turf_regression_output[which(turf_regression_output$trait==trait_i &turf_regression_output$treatment==treatment_i),]

data_i$mean_slope<-as.numeric(as.character(data_i$mean_slope))
data_i$mean_slope_p<-as.numeric(as.character(data_i$mean_slope_p))


ggplot(data_i,aes(x=mean_slope)) + 
  geom_histogram(data=data_i,fill = "red", alpha = 0.2,binwidth = 0.1) + 
  geom_histogram(data=subset(data_i,subset = mean_slope_p<0.05),fill = "blue", alpha = 0.2,binwidth = 0.1)+
  xlim(-1,1)
ggsave(filename = paste("C:/Users/Brian/Google Drive/China_PFTC12_distribution_output/distributions_of_slopes_of_mean_v_time/","assume_fixed_traits_",trait_i,"_treatment_",treatment_i,".jpeg",sep = ""),device = "jpeg")  

#savePlot(filename = paste("C:/Users/Brian/Google Drive/China_PFTC12_distribution_output/distributions_of_slopes_of_mean_v_time/","assume_fixed_traits_",trait_i,"_treatment_",treatment_i,sep = ""),type = "jpeg")

}





