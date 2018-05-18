

#Visuals
#overlapping distribution: origin dist, recipient dist, experiment dist

#Plots
# skewness vs time, mean vs time, variance vs delta mean (change in mean from beginning to end)

source("trait_distributions/r_scripts/summarize_moments.R")

file_directory_native<-"trait_distributions/using_native_site/"
file_directory_recipient<-"trait_distributions/using_recipient site//"

moments_fixed<-extract_moments(file_directory = file_directory_native)
moments_plastic<-extract_moments(file_directory = file_directory_recipient)

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
#Generate histograms of mean slope for each treatment x trait

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


####

#Generate plots of treatment x trait x moment over time, with plots seperated

for(i in 1:nrow(unique(cbind(as.character(moments_fixed$site),as.character(moments_fixed$treatment),as.character(moments_fixed$trait))))){
print(i/nrow(unique(cbind(as.character(moments_fixed$site),as.character(moments_fixed$treatment),as.character(moments_fixed$trait))))*100)
  
site_i<-unique(cbind(as.character(moments_fixed$site),as.character(moments_fixed$treatment),as.character(moments_fixed$trait)))[i,1]
treatment_i<-unique(cbind(as.character(moments_fixed$site),as.character(moments_fixed$treatment),as.character(moments_fixed$trait)))[i,2]
trait_i<-unique(cbind(as.character(moments_fixed$site),as.character(moments_fixed$treatment),as.character(moments_fixed$trait)))[i,3]

data_i<-moments_fixed[which(moments_fixed$site==site_i & moments_fixed$treatment==treatment_i & moments_fixed$trait==trait_i),]

data_i$year<-as.numeric(as.character(data_i$year))
data_i$mean<-as.numeric(as.character(data_i$mean))
data_i$mean_lower<-as.numeric(as.character(data_i$mean_lower))
data_i$mean_upper<-as.numeric(as.character(data_i$mean_upper))

data_i$var<-as.numeric(as.character(data_i$var))
data_i$var_lower<-as.numeric(as.character(data_i$var_lower))
data_i$var_upper<-as.numeric(as.character(data_i$var_upper))

data_i$skew<-as.numeric(as.character(data_i$skew))
data_i$skew_lower<-as.numeric(as.character(data_i$skew_lower))
data_i$skew_upper<-as.numeric(as.character(data_i$skew_upper))

data_i$kurt<-as.numeric(as.character(data_i$kurt))
data_i$kurt_lower<-as.numeric(as.character(data_i$kurt_lower))
data_i$kurt_upper<-as.numeric(as.character(data_i$kurt_upper))





mean_i<-ggplot(data=data_i,
       aes(x=year, y=mean, colour=turf)) +
  geom_line(size=1.5) + ylab("Mean")+
  geom_ribbon(aes(ymin=data_i$mean_lower,ymax=data_i$mean_upper,fill=turf),alpha=0.3)+ggtitle(paste("Trait",trait_i,", Site ",site_i,", Treatment ",treatment_i))
  

var_i<-ggplot(data=data_i,
       aes(x=year, y=var, colour=turf)) +
  geom_line(size=1.5) +ylab("Variance")+
  geom_ribbon(aes(ymin=data_i$var_lower,ymax=data_i$var_upper,fill=turf),alpha=0.3)+ggtitle(paste("Trait",trait_i,", Site ",site_i,", Treatment ",treatment_i))

skew_i<-ggplot(data=data_i,
       aes(x=year, y=skew, colour=turf)) +
  geom_line(size=1.5) +ylab("Skewness")+
  geom_ribbon(aes(ymin=data_i$skew_lower,ymax=data_i$skew_upper,fill=turf),alpha=0.3)+ggtitle(paste("Trait",trait_i,", Site ",site_i,", Treatment ",treatment_i))

kurt_i<-ggplot(data=data_i,
       aes(x=year, y=kurt, colour=turf)) +
  geom_line(size=1.5) +ylab("Kurtosis")+
  geom_ribbon(aes(ymin=data_i$kurt_lower,ymax=data_i$kurt_upper,fill=turf),alpha=0.3)+ggtitle(paste("Trait",trait_i,", Site ",site_i,", Treatment ",treatment_i))


ggsave(filename = paste("C:/Users/Brian/Google Drive/China_PFTC12_distribution_output/plot_level_mean_v_time/","assume_fixed_traits_mean_",trait_i,"_treatment_",treatment_i,"_site_",site_i,".jpeg",sep = ""),plot = mean_i)
ggsave(filename = paste("C:/Users/Brian/Google Drive/China_PFTC12_distribution_output/plot_level_variance_v_time/","assume_fixed_traits_var_",trait_i,"_treatment_",treatment_i,"_site_",site_i,".jpeg",sep = ""),plot = var_i)
ggsave(filename = paste("C:/Users/Brian/Google Drive/China_PFTC12_distribution_output/plot_level_skewness_v_time/","assume_fixed_traits_skew_",trait_i,"_treatment_",treatment_i,"_site_",site_i,".jpeg",sep = ""),plot = skew_i)
ggsave(filename = paste("C:/Users/Brian/Google Drive/China_PFTC12_distribution_output/plot_level_kurtosis_v_time/","assume_fixed_traits_kurt_",trait_i,"_treatment_",treatment_i,"_site_",site_i,".jpeg",sep = ""),plot = kurt_i)

rm(data_i,mean_i,skew_i,var_i,kurt_i,site_i,trait_i,treatment_i)

}#i unique treatment x traits



####################


#Model fitting

library(lme4)
moments_fixed$mean<-as.numeric(as.character(moments_fixed$mean))
moments_fixed$year<-as.numeric(as.character(moments_fixed$year))
moments_fixed$mean<-as.numeric(as.character(moments_fixed$mean))
moments_fixed$year<-as.numeric(as.character(moments_fixed$year))


glm_out<-glm(mean ~ year + treatment,data = moments_fixed[which(moments_fixed$trait=="C_percent"),])
glm_out2<-glm(mean ~ year + treatment+site,data = moments_fixed[which(moments_fixed$trait=="C_percent"),])

glm_out_C<-glm(mean ~ treatment + site + year*treatment ,data = moments_fixed[which(moments_fixed$trait=="C_percent"),])
glm_out_N<-glm(mean ~ treatment + site + year*treatment ,data = moments_fixed[which(moments_fixed$trait=="N_percent"),])
glm_out_P<-glm(mean ~ treatment + site + year*treatment ,data = moments_fixed[which(moments_fixed$trait=="P_AVG"),])
glm_out_NP<-glm(mean ~ treatment + site + year*treatment ,data = moments_fixed[which(moments_fixed$trait=="NP_ratio"),])
glm_out_CN<-glm(mean ~ treatment + site + year*treatment ,data = moments_fixed[which(moments_fixed$trait=="CN_ratio"),])
glm_out_dC<-glm(mean ~ treatment + site + year*treatment ,data = moments_fixed[which(moments_fixed$trait=="dC13_percent"),])
glm_out_dN<-glm(mean ~ treatment + site + year*treatment ,data = moments_fixed[which(moments_fixed$trait=="dN15_percent"),])
glm_out_SLA<-glm(mean ~ treatment + site + year*treatment ,data = moments_fixed[which(moments_fixed$trait=="SLA_cm2_g"),])
glm_out_drymass<-glm(mean ~ treatment + site + year*treatment ,data = moments_fixed[which(moments_fixed$trait=="Dry_Mass_g"),])
glm_out_wetmass<-glm(mean ~ treatment + site + year*treatment ,data = moments_fixed[which(moments_fixed$trait=="Wet_Mass_g"),])
glm_out_ldmc<-glm(mean ~ treatment + site + year*treatment ,data = moments_fixed[which(moments_fixed$trait=="LDMC"),])
glm_out_leafarea<-glm(mean ~ treatment + site + year*treatment ,data = moments_fixed[which(moments_fixed$trait=="Leaf_Area_cm2"),])
glm_out_leafthickness<-glm(mean ~ treatment + site + year*treatment ,data = moments_fixed[which(moments_fixed$trait=="Leaf_Thickness_Ave_mm"),])
  
library(bbmle)
AICtab(glm_out,glm_out_C,glm_out2)#looks like we're justified in adding the interaction

moments_fixed$treatment

summary(glm_out_C)
summary(glm_out_N)
summary(glm_out_P)
summary(glm_out_NP)
summary(glm_out_CN)
summary(glm_out_dC)
summary(glm_out_dN)
summary(glm_out_SLA)
summary(glm_out_drymass)
summary(glm_out_wetmass)
summary(glm_out_ldmc)
summary(glm_out_leafarea)
summary(glm_out_leafthickness)

sg<-stargazer(mpd_rs,nnd_rs,pd_rs,type="html",out="native_range_metrics_rescaled_output.htm")

library(stargazer)

?stargazer

stargazer(glm_out_C,glm_out_N,glm_out_P,glm_out_NP,glm_out_CN,glm_out_dC,glm_out_dN,glm_out_SLA,glm_out_drymass,glm_out_wetmass,glm_out_ldmc,glm_out_leafarea,glm_out_leafthickness,
          column.labels = c("C","N","P","NP ratio","CN ratio","dC13","dN15","SLA","dry mass","wet mass","LDMC","area","thickness"),
          model.numbers = F,
          #type = "text",
          dep.var.caption = "",out = "C:/Users/Brian/Desktop/sg_out_china_boots.htm")





plot(sg)

#+ (1|main_thing:nested_component)

unique(moments_fixed$trait)

moments_plastic$mean<-as.numeric(as.character(moments_plastic$mean))
moments_plastic$year<-as.numeric(as.character(moments_plastic$year))
moments_plastic$mean<-as.numeric(as.character(moments_plastic$mean))
moments_plastic$year<-as.numeric(as.character(moments_plastic$year))



glm_out_plastic_C<-glm(mean ~ treatment + site + year*treatment ,data = moments_plastic[which(moments_plastic$trait=="C_percent"),])
glm_out_plastic_N<-glm(mean ~ treatment + site + year*treatment ,data = moments_plastic[which(moments_plastic$trait=="N_percent"),])
glm_out_plastic_P<-glm(mean ~ treatment + site + year*treatment ,data = moments_plastic[which(moments_plastic$trait=="P_AVG"),])
glm_out_plastic_NP<-glm(mean ~ treatment + site + year*treatment ,data = moments_plastic[which(moments_plastic$trait=="NP_ratio"),])
glm_out_plastic_CN<-glm(mean ~ treatment + site + year*treatment ,data = moments_plastic[which(moments_plastic$trait=="CN_ratio"),])
glm_out_plastic_dC<-glm(mean ~ treatment + site + year*treatment ,data = moments_plastic[which(moments_plastic$trait=="dC13_percent"),])
glm_out_plastic_dN<-glm(mean ~ treatment + site + year*treatment ,data = moments_plastic[which(moments_plastic$trait=="dN15_percent"),])
glm_out_plastic_SLA<-glm(mean ~ treatment + site + year*treatment ,data = moments_plastic[which(moments_plastic$trait=="SLA_cm2_g"),])
glm_out_plastic_drymass<-glm(mean ~ treatment + site + year*treatment ,data = moments_plastic[which(moments_plastic$trait=="Dry_Mass_g"),])
glm_out_plastic_wetmass<-glm(mean ~ treatment + site + year*treatment ,data = moments_plastic[which(moments_plastic$trait=="Wet_Mass_g"),])
glm_out_plastic_ldmc<-glm(mean ~ treatment + site + year*treatment ,data = moments_plastic[which(moments_plastic$trait=="LDMC"),])
glm_out_plastic_leafarea<-glm(mean ~ treatment + site + year*treatment ,data = moments_plastic[which(moments_plastic$trait=="Leaf_Area_cm2"),])
glm_out_plastic_leafthickness<-glm(mean ~ treatment + site + year*treatment ,data = moments_plastic[which(moments_plastic$trait=="Leaf_Thickness_Ave_mm"),])

stargazer(glm_out_plastic_C,glm_out_plastic_N,glm_out_plastic_P,glm_out_plastic_NP,glm_out_plastic_CN,glm_out_plastic_dC,glm_out_plastic_dN,glm_out_plastic_SLA,glm_out_plastic_drymass,glm_out_plastic_wetmass,glm_out_plastic_ldmc,glm_out_plastic_leafarea,glm_out_plastic_leafthickness,
          column.labels = c("C","N","P","NP ratio","CN ratio","dC13","dN15","SLA","dry mass","wet mass","LDMC","area","thickness"),
          model.numbers = F,
          #type = "text",
          dep.var.caption = "",out = "C:/Users/Brian/Desktop/sg_out_china_boots_plastic.htm")
