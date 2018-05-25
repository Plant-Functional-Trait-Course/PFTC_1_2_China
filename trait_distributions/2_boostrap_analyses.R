

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

glm_out_C<-glm(mean ~ treatment + site + year + year*treatment ,data = moments_fixed[which(moments_fixed$trait=="C_percent"),])
glm_out_N<-glm(mean ~ treatment + site + year + year*treatment ,data = moments_fixed[which(moments_fixed$trait=="N_percent"),])
glm_out_P<-glm(mean ~ treatment + site + year + year*treatment ,data = moments_fixed[which(moments_fixed$trait=="P_AVG"),])
glm_out_NP<-glm(mean ~ treatment + site + year + year*treatment ,data = moments_fixed[which(moments_fixed$trait=="NP_ratio"),])
glm_out_CN<-glm(mean ~ treatment + site + year + year*treatment ,data = moments_fixed[which(moments_fixed$trait=="CN_ratio"),])
glm_out_dC<-glm(mean ~ treatment + site + year + year*treatment ,data = moments_fixed[which(moments_fixed$trait=="dC13_percent"),])
glm_out_dN<-glm(mean ~ treatment + site + year + year*treatment ,data = moments_fixed[which(moments_fixed$trait=="dN15_percent"),])
glm_out_SLA<-glm(mean ~ treatment + site + year + year*treatment ,data = moments_fixed[which(moments_fixed$trait=="SLA_cm2_g"),])
glm_out_drymass<-glm(mean ~ treatment + site + year + year*treatment ,data = moments_fixed[which(moments_fixed$trait=="Dry_Mass_g"),])
glm_out_wetmass<-glm(mean ~ treatment + site + year + year*treatment ,data = moments_fixed[which(moments_fixed$trait=="Wet_Mass_g"),])
glm_out_ldmc<-glm(mean ~ treatment + site + year + year*treatment ,data = moments_fixed[which(moments_fixed$trait=="LDMC"),])
glm_out_leafarea<-glm(mean ~ treatment + site + year + year*treatment ,data = moments_fixed[which(moments_fixed$trait=="Leaf_Area_cm2"),])
glm_out_leafthickness<-glm(mean ~ treatment + site + year + year*treatment ,data = moments_fixed[which(moments_fixed$trait=="Leaf_Thickness_Ave_mm"),])
  
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


library(stargazer)

?stargazer

stargazer(glm_out_C,glm_out_N,glm_out_P,glm_out_NP,glm_out_CN,glm_out_dC,glm_out_dN,glm_out_SLA,glm_out_drymass,glm_out_wetmass,glm_out_ldmc,glm_out_leafarea,glm_out_leafthickness,
          column.labels = c("C","N","P","NP ratio","CN ratio","dC13","dN15","SLA","dry mass","wet mass","LDMC","area","thickness"),
          model.numbers = F,
          #type = "text",
          dep.var.caption = "",out = "C:/Users/Brian/Desktop/sg_out_china_boots.htm")





unique(moments_fixed$trait)

moments_plastic$mean<-as.numeric(as.character(moments_plastic$mean))
moments_plastic$year<-as.numeric(as.character(moments_plastic$year))
moments_plastic$mean<-as.numeric(as.character(moments_plastic$mean))
moments_plastic$year<-as.numeric(as.character(moments_plastic$year))



glm_out_plastic_C<-glm(mean ~ treatment + site + year + year*treatment ,data = moments_plastic[which(moments_plastic$trait=="C_percent"),])
glm_out_plastic_N<-glm(mean ~ treatment + site + year + year*treatment ,data = moments_plastic[which(moments_plastic$trait=="N_percent"),])
glm_out_plastic_P<-glm(mean ~ treatment + site + year + year*treatment ,data = moments_plastic[which(moments_plastic$trait=="P_AVG"),])
glm_out_plastic_NP<-glm(mean ~ treatment + site + year + year*treatment ,data = moments_plastic[which(moments_plastic$trait=="NP_ratio"),])
glm_out_plastic_CN<-glm(mean ~ treatment + site + year + year*treatment ,data = moments_plastic[which(moments_plastic$trait=="CN_ratio"),])
glm_out_plastic_dC<-glm(mean ~ treatment + site + year + year*treatment ,data = moments_plastic[which(moments_plastic$trait=="dC13_percent"),])
glm_out_plastic_dN<-glm(mean ~ treatment + site + year + year*treatment ,data = moments_plastic[which(moments_plastic$trait=="dN15_percent"),])
glm_out_plastic_SLA<-glm(mean ~ treatment + site + year + year*treatment ,data = moments_plastic[which(moments_plastic$trait=="SLA_cm2_g"),])
glm_out_plastic_drymass<-glm(mean ~ treatment + site + year + year*treatment ,data = moments_plastic[which(moments_plastic$trait=="Dry_Mass_g"),])
glm_out_plastic_wetmass<-glm(mean ~ treatment + site + year + year*treatment ,data = moments_plastic[which(moments_plastic$trait=="Wet_Mass_g"),])
glm_out_plastic_ldmc<-glm(mean ~ treatment + site + year + year*treatment ,data = moments_plastic[which(moments_plastic$trait=="LDMC"),])
glm_out_plastic_leafarea<-glm(mean ~ treatment + year + site + year*treatment ,data = moments_plastic[which(moments_plastic$trait=="Leaf_Area_cm2"),])
glm_out_plastic_leafthickness<-glm(mean ~ treatment + site + year + year*treatment ,data = moments_plastic[which(moments_plastic$trait=="Leaf_Thickness_Ave_mm"),])

stargazer(glm_out_plastic_C,glm_out_plastic_N,glm_out_plastic_P,glm_out_plastic_NP,glm_out_plastic_CN,glm_out_plastic_dC,glm_out_plastic_dN,glm_out_plastic_SLA,glm_out_plastic_drymass,glm_out_plastic_wetmass,glm_out_plastic_ldmc,glm_out_plastic_leafarea,glm_out_plastic_leafthickness,
          column.labels = c("C","N","P","NP ratio","CN ratio","dC13","dN15","SLA","dry mass","wet mass","LDMC","area","thickness"),
          model.numbers = F,
          #type = "text",
          dep.var.caption = "",out = "C:/Users/Brian/Desktop/sg_out_china_boots_plastic.htm")
summary(glm_out_plastic_C)

#R2s
library(rsq)
#fixed
rsq(glm_out_C)
rsq(glm_out_N)
rsq(glm_out_P)
rsq(glm_out_NP)
rsq(glm_out_CN)
rsq(glm_out_dC)
rsq(glm_out_dN)
rsq(glm_out_SLA)
rsq(glm_out_drymass)
rsq(glm_out_wetmass)
rsq(glm_out_ldmc)
rsq(glm_out_leafarea)
rsq(glm_out_leafthickness)

#adj r2
rsq(glm_out_C,adj = T)
rsq(glm_out_N,adj = T)
rsq(glm_out_P,adj = T)
rsq(glm_out_NP,adj = T)
rsq(glm_out_CN,adj = T)
rsq(glm_out_dC,adj = T)
rsq(glm_out_dN,adj = T)
rsq(glm_out_SLA,adj = T)
rsq(glm_out_drymass,adj = T)
rsq(glm_out_wetmass,adj = T)
rsq(glm_out_ldmc,adj = T)
rsq(glm_out_leafarea,adj = T)
rsq(glm_out_leafthickness,adj = T)

#Plastic r2s
rsq(glm_out_plastic_C)
rsq(glm_out_plastic_N)
rsq(glm_out_plastic_P)
rsq(glm_out_plastic_NP)
rsq(glm_out_plastic_CN)
rsq(glm_out_plastic_dC)
rsq(glm_out_plastic_dN)
rsq(glm_out_plastic_SLA)
rsq(glm_out_plastic_drymass)
rsq(glm_out_plastic_wetmass)
rsq(glm_out_plastic_ldmc)
rsq(glm_out_plastic_leafarea)
rsq(glm_out_plastic_leafthickness)

#adj r2
rsq(glm_out_plastic_C,adj = T)
rsq(glm_out_plastic_N,adj = T)
rsq(glm_out_plastic_P,adj = T)
rsq(glm_out_plastic_NP,adj = T)
rsq(glm_out_plastic_CN,adj = T)
rsq(glm_out_plastic_dC,adj = T)
rsq(glm_out_plastic_dN,adj = T)
rsq(glm_out_plastic_SLA,adj = T)
rsq(glm_out_plastic_drymass,adj = T)
rsq(glm_out_plastic_wetmass,adj = T)
rsq(glm_out_plastic_ldmc,adj = T)
rsq(glm_out_plastic_leafarea,adj = T)
rsq(glm_out_plastic_leafthickness,adj = T)

############################################


summary(glm_out_plastic_C)

library(visreg)
visreg(fit = glm_out_C,xvar = "year",by="site")
visreg(fit = glm_out_C)



############################################


#What model best explains 

#glmulti for model selection
#examples in Brian's 3rd code set from GEB paper

library(glmulti)
?glmulti

glmulti_out<-glmulti(mean ~ treatment + site + year + year*treatment ,data = moments_plastic[which(moments_plastic$trait=="Leaf_Thickness_Ave_mm"),],crit=aicc)
summary(glmulti_out)
weightable(glmulti_out)

glm_out_plastic_leafthickness<-glm(mean ~ treatment + site + year + year*treatment ,data = moments_plastic[which(moments_plastic$trait=="Leaf_Thickness_Ave_mm"),])

library(MuMIn) #also in code set 3
library(rsq)
#fixed assumption
mumin_summary<-NULL
for(i in 1:length(unique(moments_fixed$trait))){
trait <-  as.character(unique(moments_fixed$trait))[i]
  
data_i<-moments_fixed[which(moments_fixed$trait==trait),]

Model_TSYI<-glm(formula = mean ~ treatment + site + year + year*treatment ,data = data_i)  
Model_TSY<-glm(formula = mean ~ treatment + site + year ,data = data_i)    
Model_TS<-glm(formula = mean ~ treatment + site ,data = data_i)      
Model_T<-glm(formula = mean ~ treatment ,data = data_i)      
Model_SY<-glm(formula = mean ~ site + year ,data = data_i)    
Model_S<-glm(formula = mean ~ site ,data = data_i)    
Model_Y<-glm(formula = mean ~ year ,data = data_i)    
Model_TY<-glm(formula = mean ~ treatment + year ,data = data_i)    

mumin_out<-MuMIn::model.sel(object = list(Model_TSYI,Model_TSY,Model_TS,Model_T,Model_SY,Model_S,Model_Y,Model_TY))

mumin_out<-mumin_out[order(row.names(mumin_out)),]
mumin_out$trait<-trait

mumin_out$rsq<-NA
mumin_out$rsq.adj<-NA
mumin_out$rsq[1]<-rsq(fitObj = Model_TSYI)
mumin_out$rsq[2]<-rsq(fitObj = Model_TSY)
mumin_out$rsq[3]<-rsq(fitObj = Model_TS)
mumin_out$rsq[4]<-rsq(fitObj = Model_T)
mumin_out$rsq[5]<-rsq(fitObj = Model_SY)
mumin_out$rsq[6]<-rsq(fitObj = Model_S)
mumin_out$rsq[7]<-rsq(fitObj = Model_Y)
mumin_out$rsq[8]<-rsq(fitObj = Model_TY)

mumin_out$rsq.adj[1]<-rsq(fitObj = Model_TSYI,adj = T)
mumin_out$rsq.adj[2]<-rsq(fitObj = Model_TSY,adj = T)
mumin_out$rsq.adj[3]<-rsq(fitObj = Model_TS,adj = T)
mumin_out$rsq.adj[4]<-rsq(fitObj = Model_T,adj = T)
mumin_out$rsq.adj[5]<-rsq(fitObj = Model_SY,adj = T)
mumin_out$rsq.adj[6]<-rsq(fitObj = Model_S,adj = T)
mumin_out$rsq.adj[7]<-rsq(fitObj = Model_Y,adj = T)
mumin_out$rsq.adj[8]<-rsq(fitObj = Model_TY,adj = T)

mumin_out$model<-NA
mumin_out$model[1]<-"TSYI"
mumin_out$model[2]<-"TSY"
mumin_out$model[3]<-"TS"
mumin_out$model[4]<-"T"
mumin_out$model[5]<-"SY"
mumin_out$model[6]<-"S"
mumin_out$model[7]<-"Y"
mumin_out$model[8]<-"TY"


mumin_out<-as.data.frame(mumin_out)

mumin_summary<-rbind(mumin_summary,mumin_out)

mumin_summary


  
}


write.csv(file = "C:/Users/Brian/Google Drive/China_PFTC12_distribution_output/stats/model_selection_mean.csv",row.names = F,x = mumin_summary)




#make output table ranking models for each variable. -weight table

#calculate r2 for each model component

# re-do for graminoid and non-graminoid

# 

##############################

#Turf skewness vs kurtosis

#It would be good to see all of them together in a giant skewness vs. kurtosis plot 
#but maybe color coded by site and then by treatment and maybe year

plot(as.numeric(as.character(moments_fixed$skew))~as.numeric(as.character(moments_fixed$kurt)),xlab="Kurtosis",ylab = "Skewness")
plot(as.numeric(as.character(moments_fixed$mean))~as.numeric(as.character(moments_plastic$mean)))

pvf<-lm(as.numeric(as.character(moments_fixed$mean))~as.numeric(as.character(moments_plastic$mean)))
summary(pvf)



moments_fixed$skew<-as.numeric(as.character(moments_fixed$skew))
moments_fixed$kurt<-as.numeric(as.character(moments_fixed$kurt))

library(ggplot2)
skew_kurt_color_by_trait <- ggplot(moments_fixed, aes(x = moments_fixed$skew, y = moments_fixed$kurt, colour = moments_fixed$trait)) + geom_point() 
skew_kurt_color_by_trait

skew_kurt_color_by_site <- ggplot(moments_fixed, aes(x = moments_fixed$skew, y = moments_fixed$kurt, colour = moments_fixed$site)) + geom_point() 
skew_kurt_color_by_site

skew_kurt_color_by_treatment <- ggplot(moments_fixed, aes(x = moments_fixed$skew, y = moments_fixed$kurt, colour = moments_fixed$treatment)) + geom_point() 
skew_kurt_color_by_treatment

skew_kurt_color_by_year <- ggplot(moments_fixed, aes(x = moments_fixed$skew, y = moments_fixed$kurt, colour = moments_fixed$year)) + geom_point() 
skew_kurt_color_by_year

###################################################

#Relative mean vs time

#For each trait X site X treatment
#moments_fixed$
tst<-unique(moments_fixed[,c(2,3,5)])


dev_from_mean_v_time<-NULL

for(i in 1:nrow(tst)){
  
trait<-as.character(tst$trait[i])  
treatment<-as.character(tst$treatment[i])  
site<-as.character(tst$site[i])  

#Specify origin and destination plots

if(treatment %in% c("C","O","OTC")){
origin_site<-site
destination_site<-site
}

if(treatment == 3){
origin_site<-"H"
destination_site<-"L"

}

if(treatment == 5){
  origin_site<-"L"
  destination_site<-"H"
}


if(treatment==1){
site_numeric<-which(c("H","A","M","L")==site)  

origin_site<-site
destination_site  <- c("H","A","M","L")[site_numeric+1]

}

if(treatment==2){
  site_numeric<-which(c("H","A","M","L")==site)  
  
  origin_site<-site
  destination_site  <- c("H","A","M","L")[site_numeric-1]
  
}

#####

#Extract destination site mean at year 1 for trait x site

initial_value<-mean(moments_fixed$mean[which(moments_fixed$trait==trait & moments_fixed$site==destination_site & moments_fixed$year=="2012" & moments_fixed$treatment%in%c("C","O"))])

data_i<-moments_fixed[which(moments_fixed$treatment==treatment & moments_fixed$site==site & moments_fixed$trait==trait),]

data_i$mean <- (data_i$mean - initial_value)/initial_value

#plot(data_i$mean ~ data_i$year)
plot_mean_diff <- ggplot(data = data_i, aes(x = year, y = mean,  colour = turf)) + geom_point(aes(size=abs(data_i$mean)))+geom_hline(yintercept = 0)+ggtitle(paste("Trait",trait,", Site ",site,", Treatment ",treatment)) 
plot_mean_diff


ggsave(filename = paste("C:/Users/Brian/Google Drive/China_PFTC12_distribution_output/convergence_on_destination_site/","assume_fixed_traits_mean_",trait,"_treatment_",treatment,"_site_",site,".jpeg",sep = ""),plot = plot_mean_diff)


lm_data_i<-lm(data_i$mean~data_i$year)

plot_mean_diff+geom_abline(slope = lm_data_i$coefficients[2],intercept = lm_data_i$coefficients[1])

y_min<-min(data_i$mean)
y_max<-max(data_i$mean)



intercept<-lm_data_i$coefficients[1]
slope<-lm_data_i$coefficients[2]
summary_lm<-summary(lm_data_i)
p_val_int<-summary_lm$coefficients[2,4]

summary_lm
out_i<-cbind(treatment,origin_site,destination_site,trait,slope,intercept,y_min,y_max,p_val_int)


dev_from_mean_v_time<-rbind(dev_from_mean_v_time,out_i)



  
}#end i loop

dev_from_mean_v_time <- as.data.frame(dev_from_mean_v_time)
dev_from_mean_v_time$slope <- as.numeric(as.character(dev_from_mean_v_time$slope))
dev_from_mean_v_time$intercept <- as.numeric(as.character(dev_from_mean_v_time$intercept))
dev_from_mean_v_time$y_min <- as.numeric(as.character(dev_from_mean_v_time$y_min))
dev_from_mean_v_time$y_max <- as.numeric(as.character(dev_from_mean_v_time$y_max))
dev_from_mean_v_time$p_val_int <- as.numeric(as.character(dev_from_mean_v_time$p_val_int))


#plot multiple lines


data_3<-dev_from_mean_v_time[which(dev_from_mean_v_time$treatment=="3" & dev_from_mean_v_time$p_val_int<= 0.05),]
data_4<-dev_from_mean_v_time[which(dev_from_mean_v_time$treatment=="4"& dev_from_mean_v_time$p_val_int<= 0.05),]


ggplot(data = data_3)+xlim(c(2012,2016))+ylim(c(min(data_3$y_min),max(data_3$y_max)))+geom_abline(aes(slope = data_3$slope,intercept = data_3$intercept,colour=data_3$trait))+ggtitle("Treatment 3")
ggplot(data = data_4)+xlim(c(2012,2016))+ylim(c(min(data_4$y_min),max(data_4$y_max)))+geom_abline(aes(slope = data_4$slope,intercept = data_4$intercept,colour=data_4$trait))+ggtitle("Treatment 4")



scatterplot <- qplot(x=Wind, y=Temp, data=airquality)
scatterplot + geom_abline(aes(intercept=intercept, slope=slope,
                              colour=quantile), data=quantile.regressions)


#######################


##Plot of variance vs rate of change


#Variance = initial variance of plot

#Rate of change = abs(slope of mean vs time relationship)

#Color by treatment, site or else separate plots?

###

#Fixed traits

var_v_change_data_fixed<-NULL
for(i in 1:length(unique(moments_fixed$turf))){
turf<-  as.character(unique(moments_fixed$turf)[i])
data_i <- moments_fixed[which(moments_fixed$turf==turf),]  
  
for(t in 1:length(unique(data_i$trait))){
  
trait<-as.character(unique(data_i$trait)[t])
data_t <- data_i[which(data_i$trait==trait),]


  
lm_t<-lm(formula = data_t$mean~data_t$year)
slope<-abs(lm_t$coefficients[2])
initial_var <- as.numeric(as.character(data_t$var))[which.min(data_t$year)]
treatment<-as.character(unique(data_t$treatment))
site<-as.character(unique(data_t$site))

out_t<-cbind(trait,site,treatment,turf,slope,initial_var)
var_v_change_data_fixed<-rbind(var_v_change_data_fixed,out_t)

    
  
} #for t 
  
}#for i

var_v_change_data_fixed<-as.data.frame(var_v_change_data_fixed)
var_v_change_data_fixed$slope<-as.numeric(as.character(var_v_change_data_fixed$slope))
var_v_change_data_fixed$initial_var<-as.numeric(as.character(var_v_change_data_fixed$initial_var))
plot(abs(var_v_change_data_fixed$slope)~log(var_v_change_data_fixed$initial_var))



plot(abs(var_v_change_data_fixed$slope[which(var_v_change_data_fixed$treatment=="1")])~log(var_v_change_data_fixed$initial_var[which(var_v_change_data_fixed$treatment=="1")]),main="treatment 1")


tt<-unique(moments_fixed[c('trait','treatment')])

output_slope_v_var<-NULL
for(i in 1:nrow(tt)){
  
  
treatment <- as.character(tt$treatment[i]  )
trait <- as.character(tt$trait[i])  

data_i<-var_v_change_data_fixed[which(var_v_change_data_fixed$trait==trait &
                                        var_v_change_data_fixed$treatment==treatment),]
lm_plot_slope<-lm(formula = slope~initial_var,data = data_i)
summary_lm<-summary(lm_plot_slope)
p_val<-round(summary_lm$coefficients[2,4],2)
r2<-round(summary_lm$r.squared,2)
adjr2<-round(summary_lm$adj.r.squared,2)

plot_slope_v_var <- ggplot(data = data_i, aes(x = initial_var, y = slope)) + geom_point(aes(colour=site))+ geom_hline(yintercept = 0)+
  geom_abline(data = data_i, slope = lm_plot_slope$coefficients[2] , intercept = lm_plot_slope$coefficients[1], colour = "red")+ 
  ggtitle(paste("Trait",trait,", Treatment ",treatment, ", p-value", p_val, ", r2",r2,", adj.r2",adjr2)) +
  xlab("Initial variance")+ ylab("Slope (mean vs time)")


plot_slope_v_var


ggsave(filename = paste("C:/Users/Brian/Google Drive/China_PFTC12_distribution_output/rate_of_change_vs_initial_var/","assume_fixed_traits_mean_",trait,"_treatment_",treatment,".jpeg",sep = ""),plot = plot_slope_v_var)

out_i<-cbind(trait,treatment,p_val,r2,adjr2)
output_slope_v_var<-rbind(output_slope_v_var,out_i)
  
  
}

output_slope_v_var<-as.data.frame(output_slope_v_var)
output_slope_v_var$p_val<-as.numeric(as.character(output_slope_v_var$p_val))
output_slope_v_var$r2<-as.numeric(as.character(output_slope_v_var$r2))
output_slope_v_var$adjr2<-as.numeric(as.character(output_slope_v_var$adjr2))
hist(output_slope_v_var$p_val)
hist(output_slope_v_var$r2)
hist(output_slope_v_var$adjr2)

ggplot(data = output_slope_v_var,aes(x=output_slope_v_var$trait,y=output_slope_v_var$p_val))+geom_boxplot(aes(fill=trait))
#I really don't think there are major patterns here
ggplot(data = output_slope_v_var,aes(x=output_slope_v_var$trait,y=output_slope_v_var$adjr2))+geom_boxplot(aes(fill=trait))


