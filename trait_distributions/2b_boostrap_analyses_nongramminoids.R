

#Visuals
#overlapping distribution: origin dist, recipient dist, experiment dist

#Plots
# skewness vs time, mean vs time, variance vs delta mean (change in mean from beginning to end)

source("trait_distributions/r_scripts/summarize_moments.R")
library(moments)
library(ggpubr)

file_directory_native<-"trait_distributions/non_gramminoids/native_site"
file_directory_recipient<-"trait_distributions/non_gramminoids/recipient_site/"

moments_fixed<-extract_moments(file_directory = file_directory_native)
moments_plastic<-extract_moments(file_directory = file_directory_recipient)
moments_fixed$mean<-as.numeric(as.character(moments_fixed$mean))

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
    
  savePlot(filename = paste("C:/Users/Brian/Google Drive/China_PFTC12_distribution_output/non_gramminoids/skewness_v_time/","assume_fixed_traits_",trait,"treatment_",treatment,sep = ""),type = "jpeg")
  
  
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
    
    savePlot(filename = paste("C:/Users/Brian/Google Drive/China_PFTC12_distribution_output/non_gramminoids/mean_v_time/","assume_fixed_traits_",trait,"_treatment_",treatment,sep = ""),type = "jpeg")
    
    
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
    
    savePlot(filename = paste("C:/Users/Brian/Google Drive/China_PFTC12_distribution_output/non_gramminoids/var_v_time/","assume_fixed_traits_",trait,"_treatment_",treatment,sep = ""),type = "jpeg")
    
    
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
    
    savePlot(filename = paste("C:/Users/Brian/Google Drive/China_PFTC12_distribution_output/non_gramminoids/kurtosis_v_time/","assume_fixed_traits_",trait,"_treatment_",treatment,sep = ""),type = "jpeg")
    
    
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
ggsave(filename = paste("C:/Users/Brian/Google Drive/China_PFTC12_distribution_output/non_gramminoids/distributions_of_slopes_of_mean_v_time/","assume_fixed_traits_",trait_i,"_treatment_",treatment_i,".jpeg",sep = ""),device = "jpeg")  

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


ggsave(filename = paste("C:/Users/Brian/Google Drive/China_PFTC12_distribution_output/non_gramminoids/plot_level_mean_v_time/","assume_fixed_traits_mean_",trait_i,"_treatment_",treatment_i,"_site_",site_i,".jpeg",sep = ""),plot = mean_i)
ggsave(filename = paste("C:/Users/Brian/Google Drive/China_PFTC12_distribution_output/non_gramminoids/plot_level_variance_v_time/","assume_fixed_traits_var_",trait_i,"_treatment_",treatment_i,"_site_",site_i,".jpeg",sep = ""),plot = var_i)
ggsave(filename = paste("C:/Users/Brian/Google Drive/China_PFTC12_distribution_output/non_gramminoids/plot_level_skewness_v_time/","assume_fixed_traits_skew_",trait_i,"_treatment_",treatment_i,"_site_",site_i,".jpeg",sep = ""),plot = skew_i)
ggsave(filename = paste("C:/Users/Brian/Google Drive/China_PFTC12_distribution_output/non_gramminoids/plot_level_kurtosis_v_time/","assume_fixed_traits_kurt_",trait_i,"_treatment_",treatment_i,"_site_",site_i,".jpeg",sep = ""),plot = kurt_i)

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
          dep.var.caption = "",out = "C:/Users/Brian/Google Drive/China_PFTC12_distribution_output/non_gramminoids/sg_out_china_boots.htm")





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
          dep.var.caption = "",out = "C:/Users/Brian/Google Drive/China_PFTC12_distribution_output/non_gramminoids/sg_out_china_boots_plastic.htm")
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


write.csv(file = "C:/Users/Brian/Google Drive/China_PFTC12_distribution_output/non_gramminoids/stats/model_selection_mean.csv",row.names = F,x = mumin_summary)




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


ggsave(filename = paste("C:/Users/Brian/Google Drive/China_PFTC12_distribution_output/non_gramminoids/rate_of_change_vs_initial_var/","assume_fixed_traits_mean_",trait,"_treatment_",treatment,".jpeg",sep = ""),plot = plot_slope_v_var)

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

#######################################
#Calculate effect size vs origin and vs destination

library(qdap)
library(ggplot2)
library(moments)
library(ggpubr)

tst<-unique(moments_fixed[,c(2,3,5)])

convergence_v_time<-NULL
divergence_v_time<-NULL

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
  
  if(treatment == 4){
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
  
  #Extract origin site meansfor trait x site x year
  
  
  initial_value_origin<-mean(moments_fixed$mean[which(moments_fixed$trait==trait & moments_fixed$site==origin_site & moments_fixed$year=="2012" & moments_fixed$treatment%in%c("C","O"))])
  initial_value_destination<-mean(moments_fixed$mean[which(moments_fixed$trait==trait & moments_fixed$site==destination_site & moments_fixed$year=="2012" & moments_fixed$treatment%in%c("C","O"))])
  data_origin_i<-moments_fixed[which(moments_fixed$treatment==treatment & moments_fixed$site==site & moments_fixed$trait==trait),]
  data_destination_i<-moments_fixed[which(moments_fixed$treatment==treatment & moments_fixed$site==site & moments_fixed$trait==trait),]
  
  #data relative to origin
  data_origin_i$mean <- (data_origin_i$mean - initial_value_origin)
  data_origin_i$year<-as.numeric(as.character(data_origin_i$year))
  data_origin_i$year<-data_origin_i$year-2012
  
  #data relative to destination
  data_destination_i$mean <- (data_destination_i$mean - initial_value_destination)
  data_destination_i$year<-as.numeric(as.character(data_destination_i$year))
  data_destination_i$year<-data_destination_i$year-2012
  
  #Plots for fun and to spot obvious errors
  plot_effect_size_origin <- ggplot(data = data_origin_i, aes(x = year, y = mean,  colour = turf)) + geom_point(aes(size=abs(data_origin_i$mean)))+geom_hline(yintercept = 0)+ggtitle(paste("Trait",trait,", Site ",site,", Treatment ",treatment))+ylab("Effect (vs. origin)") 
  #plot_effect_size_origin
  
  plot_effect_size_destination <- ggplot(data = data_destination_i, aes(x = year, y = mean,  colour = turf)) + geom_point(aes(size=abs(data_destination_i$mean)))+geom_hline(yintercept = 0)+ggtitle(paste("Trait",trait,", Site ",site,", Treatment ",treatment))+ylab("Effect (vs. destination)") 
  #plot_effect_size_destination
  
  lm_data_origin_i<-lm(data_origin_i$mean~data_origin_i$year)
  lm_data_destination_i<-lm(data_destination_i$mean~data_origin_i$year)
  #summary(lm_data_i)
  
  plot_effect_size_origin+geom_abline(slope = lm_data_origin_i$coefficients[2],intercept = lm_data_origin_i$coefficients[1])
  plot_effect_size_destination+geom_abline(slope = lm_data_destination_i$coefficients[2],intercept = lm_data_destination_i$coefficients[1])
  
  y_min_origin<-min(data_origin_i$mean)
  y_max_origin<-max(data_origin_i$mean)
  
  y_min_destination<-min(data_destination_i$mean)
  y_max_destination<-max(data_destination_i$mean)
  
  
  intercept_origin<-lm_data_origin_i$coefficients[1]
  slope_origin<-lm_data_origin_i$coefficients[2]
  summary_lm_origin<-summary(lm_data_origin_i)
  p_val_slope_origin<-summary_lm_origin$coefficients[2,4]
  p_val_int_origin<-summary_lm_origin$coefficients[1,4]
  r2_origin<-summary_lm_origin$r.squared
  r2_adj_origin<-summary_lm_origin$adj.r.squared
  
  intercept_destination<-lm_data_destination_i$coefficients[1]
  slope_destination<-lm_data_destination_i$coefficients[2]
  summary_lm_destination<-summary(lm_data_destination_i)
  p_val_slope_destination<-summary_lm_destination$coefficients[2,4]
  p_val_int_destination<-summary_lm_destination$coefficients[1,4]
  r2_destination<-summary_lm_destination$r.squared
  r2_adj_destination<-summary_lm_destination$adj.r.squared
  
  
  #summary_lm
  out_i_destination<-cbind(treatment,origin_site,destination_site,trait,slope_destination,intercept_destination,y_min_destination,
                           y_max_destination,p_val_int_destination,p_val_slope_destination,r2_destination,r2_adj_destination)
  
  convergence_v_time<-rbind(convergence_v_time,out_i_destination)
  
  out_i_origin<-cbind(treatment,origin_site,destination_site,trait,slope_origin,intercept_origin,y_min_origin,
                      y_max_origin,p_val_int_origin,p_val_slope_origin,r2_origin,r2_adj_origin)
  
  divergence_v_time<-rbind(divergence_v_time,out_i_origin)
  
  rm(out_i_destination,out_i_origin,lm_data_destination_i,lm_data_origin_i,plot_effect_size_destination,plot_effect_size_origin)
  rm(data_origin_i,data_destination_i,summary_lm_destination,summary_lm_origin,p_val_int_destination,p_val_int_origin,p_val_slope_destination,p_val_slope_origin )
  rm(slope_destination,slope_origin,intercept_destination,intercept_origin,y_max_destination,y_max_origin,y_min_destination,y_min_origin)
  rm(trait,site,treatment,initial_value_destination,initial_value_origin,r2_origin,r2_adj_origin,r2_destination,r2_adj_destination )  
}#end i loop

convergence_v_time<-as.data.frame(convergence_v_time)
colnames(convergence_v_time)<-gsub(x = colnames(convergence_v_time),pattern = "_destination",replacement = "")
rownames(convergence_v_time)<-1:nrow(convergence_v_time)
convergence_v_time$slope<-as.numeric(as.character(convergence_v_time$slope))
convergence_v_time$intercept<-as.numeric(as.character(convergence_v_time$intercept))
convergence_v_time$y_min<-as.numeric(as.character(convergence_v_time$y_min))
convergence_v_time$y_max<-as.numeric(as.character(convergence_v_time$y_max))
convergence_v_time$p_val_int<-as.numeric(as.character(convergence_v_time$p_val_int))
convergence_v_time$p_val_slope<-as.numeric(as.character(convergence_v_time$p_val_slope))
convergence_v_time$r2<-as.numeric(as.character(convergence_v_time$r2))
convergence_v_time$r2_adj<-as.numeric(as.character(convergence_v_time$r2_adj))

convergence_v_time$signif<-NA
convergence_v_time$signif[which(convergence_v_time$p_val_slope <0.05)] <- "significant"
convergence_v_time$signif[which(convergence_v_time$p_val_slope>0.05 & convergence_v_time$p_val_slope<0.1)] <- "marginal"
convergence_v_time$signif[which(convergence_v_time$p_val_slope>0.1)] <- "nonsignificant"

convergence_v_time$linetype<-NA
convergence_v_time$linetype[which(convergence_v_time$p_val_slope<0.05)] <- 1
convergence_v_time$linetype[which(convergence_v_time$p_val_slope>0.05 & convergence_v_time$p_val_slope<0.1)] <- 2
convergence_v_time$linetype[which(convergence_v_time$p_val_slope>0.1)] <- 3
convergence_v_time$linetype<-as.numeric(convergence_v_time$linetype)

convergence_v_time$int_signif<-NA
convergence_v_time$int_signif[which(convergence_v_time$p_val_int<0.05)] <- "significant"
convergence_v_time$int_signif[which(convergence_v_time$p_val_int>0.05 & convergence_v_time$p_val_int<0.1)] <- "marginal"
convergence_v_time$int_signif[which(convergence_v_time$p_val_int>0.1)] <- "nonsignificant"

convergence_v_time$signif <- ordered(convergence_v_time$signif, levels = c("significant", "marginal", "nonsignificant"))
convergence_v_time$int_signif <- ordered(convergence_v_time$int_signif, levels = c("significant", "marginal", "nonsignificant"))
unique(convergence_v_time$trait)
convergence_v_time_og<-convergence_v_time
convergence_v_time<-convergence_v_time_og

convergence_v_time$trait<-multigsub(convergence_v_time$trait,pattern = c("C_percent","CN_ratio","dC13_percent","dN15_percent", "Dry_Mass_g", "LDMC", "Leaf_Area_cm2",        
                                                                         "Leaf_Thickness_Ave_mm", "N_percent", "NP_ratio", "P_AVG", "SLA_cm2_g", "Wet_Mass_g"  ),
                                    replacement = c("C %","C:N ratio","dC13 %","dN15 %", "Dry_Mass_g", "LDMC", "Leaf Area",        
                                                    "Thickness", "N_percent", "N:P ratio", "P %", "SLA", "Wet_Mass_g"  )
)
convergence_v_time
#

divergence_v_time<-as.data.frame(divergence_v_time)
colnames(divergence_v_time)<-gsub(x = colnames(divergence_v_time),pattern = "_origin",replacement = "")
rownames(divergence_v_time)<-1:nrow(divergence_v_time)
divergence_v_time$slope<-as.numeric(as.character(divergence_v_time$slope))
divergence_v_time$intercept<-as.numeric(as.character(divergence_v_time$intercept))
divergence_v_time$y_min<-as.numeric(as.character(divergence_v_time$y_min))
divergence_v_time$y_max<-as.numeric(as.character(divergence_v_time$y_max))
divergence_v_time$p_val_int<-as.numeric(as.character(divergence_v_time$p_val_int))
divergence_v_time$p_val_slope<-as.numeric(as.character(divergence_v_time$p_val_slope))
divergence_v_time$r2<-as.numeric(as.character(divergence_v_time$r2))
divergence_v_time$r2_adj<-as.numeric(as.character(divergence_v_time$r2_adj))

divergence_v_time$signif<-NA
divergence_v_time$signif[which(divergence_v_time$p_val_slope <0.05)] <- "significant"
divergence_v_time$signif[which(divergence_v_time$p_val_slope>0.05 & divergence_v_time$p_val_slope  <0.1)] <- "marginal"
divergence_v_time$signif[which(divergence_v_time$p_val_slope>0.1)] <- "nonsignificant"

divergence_v_time$linetype<-NA
divergence_v_time$linetype[which(divergence_v_time$p_val_slope<0.05)] <- 1
divergence_v_time$linetype[which(divergence_v_time$p_val_slope>0.05 & divergence_v_time$p_val_slope<0.1)] <- 2
divergence_v_time$linetype[which(divergence_v_time$p_val_slope>0.1)] <- 3
divergence_v_time$linetype<-as.numeric(divergence_v_time$linetype)

divergence_v_time$int_signif<-NA
divergence_v_time$int_signif[which(divergence_v_time$p_val_int<0.05)] <- "significant"
divergence_v_time$int_signif[which(divergence_v_time$p_val_int>0.05 & divergence_v_time$p_val_int<0.1)] <- "marginal"
divergence_v_time$int_signif[which(divergence_v_time$p_val_int>0.1)] <- "nonsignificant"

divergence_v_time$signif <- ordered(divergence_v_time$signif, levels = c("significant", "marginal", "nonsignificant"))
divergence_v_time$int_signif <- ordered(divergence_v_time$int_signif, levels = c("significant", "marginal", "nonsignificant"))
unique(divergence_v_time$trait)
divergence_v_time_og<-divergence_v_time
divergence_v_time<-divergence_v_time_og

divergence_v_time$trait<-multigsub(divergence_v_time$trait,pattern = c("C_percent","CN_ratio","dC13_percent","dN15_percent", "Dry_Mass_g", "LDMC", "Leaf_Area_cm2",        
                                                                       "Leaf_Thickness_Ave_mm", "N_percent", "NP_ratio", "P_AVG", "SLA_cm2_g", "Wet_Mass_g"  ),
                                   replacement = c("C %","C:N ratio","dC13 %","dN15 %", "Dry_Mass_g", "LDMC", "Leaf Area",        
                                                   "Thickness", "N_percent", "N:P ratio", "P %", "SLA", "Wet_Mass_g"  )
)
divergence_v_time




#######################################

#Multi-panel effect size plots

# make for treatment 3 and 4

#We can drop leaf mass and perhaps N and P (just looking at N:P and we can put the N and P plots in the sup doc?)

# each figure (one per treatment) has 10 panels
#each panel shows the lm (or lowess) fits of effect size v time.
# solid lines = significant (p < 0.05), dashed lines = marginal (0.05<0.10), dotted lines = non-significant trend (p > 0.10)

library(ggplot2)
library(gridExtra)
library(ggpubr)

treatment3estd<-convergence_v_time[which(convergence_v_time$treatment==3),]
treatment3estd<-treatment3estd[which(!treatment3estd$trait%in%c("Dry_Mass_g","Wet_Mass_g","N_percent","P_percent")),]

treatment4estd<-convergence_v_time[which(convergence_v_time$treatment==4),]
treatment4estd<-treatment4estd[which(!treatment4estd$trait%in%c("Dry_Mass_g","Wet_Mass_g","N_percent","P_percent")),]

treatment3estd$signif<-factor(treatment3estd$signif)
t3d<-ggplot()+geom_hline(yintercept = rep(0,10))+ 
  geom_abline(data = treatment3estd,mapping=aes(slope = treatment3estd$slope,intercept = treatment3estd$intercept,linetype=signif,color=int_signif),show.legend = F)+
  ylim(c(-1.5,2) )+
  xlim(c(0,4))+facet_wrap(~treatment3estd$trait,nrow = 2,ncol = 5)+ggtitle("+ 5.5 degrees C")+ylab("Effect size (vs. destination)")+
  scale_colour_manual(name="Intercept",values = c("significant"="red","marginal"="green3","nonsignificant"="blue"))+
  scale_linetype_manual(name="Slope",values = c("significant"="solid","marginal"="dashed","nonsignificant"="dotted"))

t3d


t4d<-ggplot()+geom_hline(yintercept = rep(0,10))+ 
  geom_abline(data = treatment4estd,mapping=aes(slope = treatment4estd$slope,intercept = treatment4estd$intercept,linetype=signif,color=int_signif),show.legend = F)+
  ylim(c(-1.5,2) )+
  xlim(c(0,4))+facet_wrap(~treatment4estd$trait,nrow = 2,ncol = 5)+ggtitle("- 5.5 degrees C")+ylab("Effect size (vs. destination)")+
  scale_colour_manual(name="Intercept",values = c("significant"="red","marginal"="green3","nonsignificant"="blue"))+
  scale_linetype_manual(name="Slope",values = c("significant"="solid","marginal"="dashed","nonsignificant"="dotted"))

t4d


t3d_v_t4d_effect_over_time<-grid.arrange(t3d,t4d,ncol=1)
plot(t3d_v_t4d_effect_over_time)

#ggsave(plot = t3d_v_t4d_effect_over_time, width = 6, height = 6, dpi = 300, filename = "C:/Users/Brian/Desktop/t3d_v_t4d_convergence_over_time.pdf")
#ggsave(plot = t3d_v_t4d_effect_over_time, width = 6, height = 6, dpi = 300, filename = "C:/Users/Brian/Desktop/t3d_v_t4d_convergence_over_time.jpg")

ggsave(plot = t3d_v_t4d_effect_over_time, width = 6, height = 6, dpi = 300, filename = "C:/Users/Brian/Google Drive/China_PFTC12_distribution_output/non_gramminoids/figures/t3d_v_t4d_convergence_over_time.pdf")
ggsave(plot = t3d_v_t4d_effect_over_time, width = 6, height = 6, dpi = 300, filename = "C:/Users/Brian/Google Drive/China_PFTC12_distribution_output/non_gramminoids/figures/t3d_v_t4d_convergence_over_time.jpg")


treatmentotcestd<-convergence_v_time[which(convergence_v_time$treatment=="OTC"),]
treatmentotcestd<-treatmentotcestd[which(!treatmentotcestd$trait%in%c("Dry_Mass_g","Wet_Mass_g","N_percent","P_percent")),]


treatmentotcestd$signif<-factor(treatmentotcestd$signif)
totcd<-ggplot()+geom_hline(yintercept = rep(0,40))+ 
  geom_abline(data = treatmentotcestd,mapping=aes(slope = slope,intercept = intercept,linetype=signif,color=int_signif),show.legend = F)+
  ylim(c(-1.5,2) )+
  xlim(c(0,4))+facet_wrap(~treatmentotcestd$trait,nrow = 2,ncol = 5)+ggtitle("Open-top chamber")+ylab("Effect size (vs. destination)")+
  scale_colour_manual(name="Intercept",values = c("significant"="red","marginal"="green3","nonsignificant"="blue"))+
  scale_linetype_manual(name="Slope",values = c("significant"="solid","marginal"="dashed","nonsignificant"="dotted"))

totcd
#ggsave(plot = totcd, width = 6, height = 3, dpi = 300, filename = "C:/Users/Brian/Desktop/totcd_convergence_over_time.jpg")
ggsave(plot = totcd, width = 6, height = 3, dpi = 300, filename = "C:/Users/Brian/Google Drive/China_PFTC12_distribution_output/non_gramminoids/figures/totcd_convergence_over_time.jpg")


t3d_v_t4d__totcd_convergence_over_time<-grid.arrange(totcd,t3d,t4d,ncol=1)
#ggsave(plot = t3d_v_t4d__totcd_convergence_over_time, width = 6, height = 9, dpi = 300, filename = "C:/Users/Brian/Desktop/t3d_t4d_totcd_convergence_over_time.jpg")
ggsave(plot = t3d_v_t4d__totcd_convergence_over_time, width = 6, height = 9, dpi = 300, filename = "C:/Users/Brian/Google Drive/China_PFTC12_distribution_output/non_gramminoids/figures/t3d_t4d_totcd_convergence_over_time.jpg")



#diverge (vs origin)


treatment3esto<-divergence_v_time[which(divergence_v_time$treatment==3),]
treatment3esto<-treatment3esto[which(!treatment3esto$trait%in%c("Dry_Mass_g","Wet_Mass_g","N_percent","P_percent")),]

treatment4esto<-divergence_v_time[which(divergence_v_time$treatment==4),]
treatment4esto<-treatment4esto[which(!treatment4esto$trait%in%c("Dry_Mass_g","Wet_Mass_g","N_percent","P_percent")),]

treatment3esto$signif<-factor(treatment3esto$signif)
t3o<-ggplot()+geom_hline(yintercept = rep(0,10))+ 
  geom_abline(data = treatment3esto,mapping=aes(slope = treatment3esto$slope,intercept = treatment3esto$intercept,linetype=signif,color=int_signif),show.legend = F)+
  ylim(c(-1.5,2) )+
  xlim(c(0,4))+facet_wrap(~treatment3esto$trait,nrow = 2,ncol = 5)+ggtitle("+ 5.5 degrees C")+ylab("Effect size (vs. origin)")+
  scale_colour_manual(name="Intercept",values = c("significant"="red","marginal"="green3","nonsignificant"="blue"))+
  scale_linetype_manual(name="Slope",values = c("significant"="solid","marginal"="dashed","nonsignificant"="dotted"))

t3o


t4o<-ggplot()+geom_hline(yintercept = rep(0,10))+ 
  geom_abline(data = treatment4esto,mapping=aes(slope = treatment4esto$slope,intercept = treatment4esto$intercept,linetype=signif,color=int_signif),show.legend = F)+
  ylim(c(-1.5,2) )+
  xlim(c(0,4))+facet_wrap(~treatment4esto$trait,nrow = 2,ncol = 5)+ggtitle("- 5.5 degrees C")+ylab("Effect size (vs. origin)")+
  scale_colour_manual(name="Intercept",values = c("significant"="red","marginal"="green3","nonsignificant"="blue"))+
  scale_linetype_manual(name="Slope",values = c("significant"="solid","marginal"="dashed","nonsignificant"="dotted"))

t4o


t3o_v_t4o_effect_over_time<-grid.arrange(t3o,t4o,ncol=1)
plot(t3o_v_t4o_effect_over_time)

ggsave(plot = t3o_v_t4o_effect_over_time, width = 6, height = 6, dpi = 300, filename = "C:/Users/Brian/Google Drive/China_PFTC12_distribution_output/non_gramminoids/figures/t3o_v_t4o_divergence_over_time.pdf")
ggsave(plot = t3o_v_t4o_effect_over_time, width = 6, height = 6, dpi = 300, filename = "C:/Users/Brian/Google Drive/China_PFTC12_distribution_output/non_gramminoids/figures/t3o_v_t4o_divergence_over_time.jpg")




treatmentotcesto<-divergence_v_time[which(divergence_v_time$treatment=="OTC"),]
treatmentotcesto<-treatmentotcesto[which(!treatmentotcesto$trait%in%c("Dry_Mass_g","Wet_Mass_g","N_percent","P_percent")),]

treatmentotcesto$signif<-factor(treatmentotcesto$signif)
totco<-ggplot()+geom_hline(yintercept = rep(0,40))+ 
  geom_abline(data = treatmentotcesto,mapping=aes(slope = slope,intercept = intercept,linetype=signif,color=int_signif),show.legend = F)+
  ylim(c(-1.5,2) )+
  xlim(c(0,4))+facet_wrap(~treatmentotcesto$trait,nrow = 2,ncol = 5)+ggtitle("Open-top chamber")+ylab("Effect size (vs. origin)")+
  scale_colour_manual(name="Intercept",values = c("significant"="red","marginal"="green3","nonsignificant"="blue"))+
  scale_linetype_manual(name="Slope",values = c("significant"="solid","marginal"="dashed","nonsignificant"="dotted"))

totco

ggsave(plot = totco, width = 6, height = 3, dpi = 300, filename = "C:/Users/Brian/Google Drive/China_PFTC12_distribution_output/non_gramminoids/figures/totco_divergence_over_time.jpg")

t3o_v_t4o__totco_divergence_over_time<-grid.arrange(totco,t3o,t4o,ncol=1)
ggsave(plot = t3o_v_t4o__totco_divergence_over_time, width = 6, height = 9, dpi = 300, filename = "C:/Users/Brian/Google Drive/China_PFTC12_distribution_output/non_gramminoids/figures/t3o_t4o_totco_divergence_over_time.jpg")


#combined

diverge_vs_converge<-ggarrange(t4o,t4d,t3o,t3d,totco, ncol=2, nrow=3, common.legend = TRUE, legend="right")

ggsave(plot = diverge_vs_converge, width = 12, height = 9, dpi = 300, filename = "C:/Users/Brian/Google Drive/China_PFTC12_distribution_output/non_gramminoids/figures/diverge_vs_converge.jpg")

##########################################################################################
##########################################################################################

# (I) we will need a table summarizing the statistics for the new panel plots

div_pruned<-divergence_v_time[c( "treatment","origin_site","destination_site","trait","slope","intercept","p_val_int","p_val_slope","r2","r2_adj")]
con_pruned<-convergence_v_time[c( "treatment","origin_site","destination_site","trait","slope","intercept","p_val_int","p_val_slope","r2","r2_adj")]
div_pruned$slope<-round(div_pruned$slope,digits = 4)
con_pruned$slope<-round(con_pruned$slope,digits = 4)
div_pruned$p_val_slope<-round(div_pruned$p_val_slope,digits = 4)
con_pruned$p_val_slope<-round(con_pruned$p_val_slope,digits = 4)
div_pruned$r2<-round(div_pruned$r2,digits = 4)
con_pruned$r2<-round(con_pruned$r2,digits = 4)
div_pruned$r2_adj<-round(div_pruned$r2_adj,digits = 4)
con_pruned$r2_adj<-round(con_pruned$r2_adj,digits = 4)

merged_div_con<-merge(x = div_pruned,y = con_pruned,by=c("treatment","origin_site","destination_site","trait","slope","p_val_slope","r2","r2_adj"))

colnames(merged_div_con) <- multigsub(pattern = c("intercept.x","p_val_int.x","intercept.y","p_val_int.y"),
          replacement = c("intercept(origin)","p_val_int(origin)","intercept(destination)","p_val_int(destination)"),
          colnames(merged_div_con)
            )

write.csv(x = merged_div_con,file = "C:/Users/Brian/Google Drive/China_PFTC12_distribution_output/non_gramminoids/stats/effect_size_over_time.csv",row.names = F)
rm(div_pruned,con_pruned)

merged_div_con_pruned<-merged_div_con[which(merged_div_con$treatment%in%c(3,4,"OTC")),]
merged_div_con_pruned<-merged_div_con_pruned[which(!merged_div_con_pruned$trait%in%c("Dry_Mass_g","Wet_Mass_g","N_percent","P_percent")),]
write.csv(x = merged_div_con_pruned,file = "C:/Users/Brian/Google Drive/China_PFTC12_distribution_output/non_gramminoids/stats/effect_size_over_time_pruned.csv",row.names = F)
##########################################################################################
#iV) for traits along the gradient multi-panel plot showing control CWM vs elevation, w CIs

#so just a plot of cwm vs elevation at time =2016 (=4) ?

mean_v_elevation<-NULL
tty<-unique(moments_fixed[c("trait","treatment","year")])

for(i in 1:nrow(tty)){
  
trait<-as.character(tty$trait[i])
treatment<-as.character(tty$treatment[i])  
year<-as.numeric(as.character(tty$year[i]))

data_i<-moments_fixed[which(moments_fixed$trait==trait & moments_fixed$treatment== treatment & moments_fixed$year==year ),]
data_i$elevation<-NA 
data_i$elevation<-as.numeric(multigsub(pattern = c("L","M","A","H"),replacement = c(3000,3500,3850,4130),data_i$site))
data_i$elevation<-(data_i$elevation-3000)/1130



lm_i<-lm(formula = data_i$mean~data_i$elevation)
summary_lm_i<-summary(lm_i)

slope<-lm_i$coefficients[2]
intercept<-lm_i$coefficients[1]
y_min<-min(data_i$mean)
y_max<-max(data_i$mean)
p_val_int<-summary_lm_i$coefficients[7]
p_val_slope<-summary_lm_i$coefficients[8]
r2<-summary_lm_i$r.squared
r2_adj<-summary_lm_i$adj.r.squared

out_i<-cbind(treatment,trait,year,slope,intercept,y_min,
                    y_max,p_val_int,p_val_slope,r2,r2_adj)


mean_v_elevation<-rbind(mean_v_elevation,out_i)
rm(out_i,slope,intercept,y_min,y_max,p_val_int,p_val_slope,r2,r2_adj,lm_i,summary_lm_i,trait,treatment,year,data_i)
}


mean_v_elevation<-as.data.frame(mean_v_elevation)
rownames(mean_v_elevation)<-1:nrow(mean_v_elevation)
mean_v_elevation$slope<-as.numeric(as.character(mean_v_elevation$slope))
mean_v_elevation$intercept<-as.numeric(as.character(mean_v_elevation$intercept))
mean_v_elevation$y_min<-as.numeric(as.character(mean_v_elevation$y_min))
mean_v_elevation$y_max<-as.numeric(as.character(mean_v_elevation$y_max))
mean_v_elevation$p_val_int<-as.numeric(as.character(mean_v_elevation$p_val_int))
mean_v_elevation$p_val_slope<-as.numeric(as.character(mean_v_elevation$p_val_slope))
mean_v_elevation$r2<-as.numeric(as.character(mean_v_elevation$r2))
mean_v_elevation$r2_adj<-as.numeric(as.character(mean_v_elevation$r2_adj))
mean_v_elevation$trait<-multigsub(mean_v_elevation$trait,pattern = c("C_percent","CN_ratio","dC13_percent","dN15_percent", "Dry_Mass_g", "LDMC", "Leaf_Area_cm2",        
                                                                         "Leaf_Thickness_Ave_mm", "N_percent", "NP_ratio", "P_AVG", "SLA_cm2_g", "Wet_Mass_g"  ),
                                    replacement = c("C %","C:N ratio","dC13 %","dN15 %", "Dry_Mass_g", "LDMC", "Leaf Area",        
                                                    "Thickness", "N_percent", "N:P ratio", "P %", "SLA", "Wet_Mass_g"  ))

mean_v_elevation$int_signif<-NA
mean_v_elevation$int_signif[which(mean_v_elevation$p_val_int<0.05)] <- "significant"
mean_v_elevation$int_signif[which(mean_v_elevation$p_val_int>0.05 & mean_v_elevation$p_val_int<0.1)] <- "marginal"
mean_v_elevation$int_signif[which(mean_v_elevation$p_val_int>0.1)] <- "nonsignificant"

mean_v_elevation$slope_signif<-NA
mean_v_elevation$slope_signif[which(mean_v_elevation$p_val_slope<0.05)] <- "significant"
mean_v_elevation$slope_signif[which(mean_v_elevation$p_val_slope>0.05 & mean_v_elevation$p_val_slope<0.1)] <- "marginal"
mean_v_elevation$slope_signif[which(mean_v_elevation$p_val_slope>0.1)] <- "nonsignificant"
mean_v_elevation$int_signif <- ordered(mean_v_elevation$int_signif, levels = c("significant", "marginal", "nonsignificant"))
mean_v_elevation$slope_signif <- ordered(mean_v_elevation$slope_signif, levels = c("significant", "marginal", "nonsignificant"))

##so just a plot of cwm vs elevation at time =2016 (=4) ?
mean_v_elevation_2016_co<-mean_v_elevation[which(mean_v_elevation$treatment%in%c("C","O") & mean_v_elevation$year==2016),]
mean_v_elevation_2016_co<-mean_v_elevation_2016_co[which(!mean_v_elevation_2016_co$trait%in%c("Dry_Mass_g","Wet_Mass_g","N_percent","P_percent")),]


mve_2016<-ggplot()+geom_hline(yintercept = rep(0,20))+
  geom_abline(data = mean_v_elevation_2016_co,mapping=aes(slope = mean_v_elevation_2016_co$slope,
                                                          intercept = mean_v_elevation_2016_co$intercept,
                                                          linetype=slope_signif,color=int_signif),show.legend = T)+
  #xlim(c(0,1) )+
  scale_x_continuous(breaks = c(0,1),limits = c(0,1),minor_breaks = c(0.5))+
  ylim(c(-2,2))+facet_wrap(~mean_v_elevation_2016_co$trait,nrow = 2,ncol = 5)+ggtitle("2016 Control Plots")+ylab("Trait Mean")+xlab("Relative elevation")+
  scale_colour_manual(name="Intercept",values = c("significant"="red","marginal"="green3","nonsignificant"="blue"))+
  scale_linetype_manual(name="Slope",values = c("significant"="solid","marginal"="dashed","nonsignificant"="dotted"))

mve_2016
ggsave(plot = mve_2016, width = 7, height = 3, dpi = 300, filename = "C:/Users/Brian/Google Drive/China_PFTC12_distribution_output/non_gramminoids/figures/mean_v_elevation_2016.jpg")
##########################################################################################
#iV) for traits along the gradient multi-panel plot showing control CWM vs elevation, w CIs


source("trait_distributions/r_scripts/extract_moments_site.R")
native_site_moments<-extract_moments_site(file_directory = file_directory_native)
recipient_site_moments<-extract_moments_site(file_directory = file_directory_recipient)

native_site_moments_2016_c<-native_site_moments[which(native_site_moments$year==2016 & native_site_moments$treatment=="C"),]
native_site_moments_2016_c<-native_site_moments_2016_c[which(!native_site_moments_2016_c$trait%in%c("Dry_Mass_g","Wet_Mass_g","N_percent","P_percent")),]
native_site_moments_2016_c$trait<-multigsub(native_site_moments_2016_c$trait,pattern = c("C_percent","CN_ratio","dC13_percent","dN15_percent", "Dry_Mass_g", "LDMC", "Leaf_Area_cm2",        
                                                                     "Leaf_Thickness_Ave_mm", "N_percent", "NP_ratio", "P_AVG", "SLA_cm2_g", "Wet_Mass_g"  ),
                                  replacement = c("C %","C:N ratio","dC13 %","dN15 %", "Dry_Mass_g", "LDMC", "Leaf Area",        
                                                  "Thickness", "N_percent", "N:P ratio", "P %", "SLA", "Wet_Mass_g"  ))
native_site_moments_2016_c$elevation<-NA 
native_site_moments_2016_c$elevation<-as.numeric(multigsub(pattern = c("L","M","A","H"),replacement = c(3000,3500,3850,4130),native_site_moments_2016_c$site))
#native_site_moments_2016_c$elevation<-(native_site_moments_2016_c$elevation-3000)/1130

recipient_site_moments_2016_c<-recipient_site_moments[which(recipient_site_moments$year==2016 & recipient_site_moments$treatment=="C"),]
recipient_site_moments_2016_c<-recipient_site_moments_2016_c[which(!recipient_site_moments_2016_c$trait%in%c("Dry_Mass_g","Wet_Mass_g","N_percent","P_percent")),]
recipient_site_moments_2016_c$trait<-multigsub(recipient_site_moments_2016_c$trait,pattern = c("C_percent","CN_ratio","dC13_percent","dN15_percent", "Dry_Mass_g", "LDMC", "Leaf_Area_cm2",        
                                                                                         "Leaf_Thickness_Ave_mm", "N_percent", "NP_ratio", "P_AVG", "SLA_cm2_g", "Wet_Mass_g"  ),
                                            replacement = c("C %","C:N ratio","dC13 %","dN15 %", "Dry_Mass_g", "LDMC", "Leaf Area",        
                                                            "Thickness", "N_percent", "N:P ratio", "P %", "SLA", "Wet_Mass_g"  ))
recipient_site_moments_2016_c$elevation<-NA 
recipient_site_moments_2016_c$elevation<-as.numeric(multigsub(pattern = c("L","M","A","H"),replacement = c(3000,3500,3850,4130),recipient_site_moments_2016_c$site))




ggplot()+geom_hline(yintercept = rep(0,20))+
  geom_abline(data = mean_v_elevation_2016_co,mapping=aes(slope = mean_v_elevation_2016_co$slope,
                                                          intercept = mean_v_elevation_2016_co$intercept,
                                                          linetype=slope_signif,color=int_signif),show.legend = T)+
  #xlim(c(0,1) )+
  scale_x_continuous(breaks = c(0,1),limits = c(0,1),minor_breaks = c(0.5))+
  ylim(c(-2,2))+
  facet_wrap(~mean_v_elevation_2016_co$trait,nrow = 2,ncol = 5)+
  ggtitle("2016 Control Plots")+ylab("Trait Mean")+xlab("Relative elevation")+
  scale_colour_manual(name="Intercept",values = c("significant"="red","marginal"="green3","nonsignificant"="blue"))+
  scale_linetype_manual(name="Slope",values = c("significant"="solid","marginal"="dashed","nonsignificant"="dotted"))


ggplot(data = native_site_moments_2016_c , aes(x = elevation,y = mean)) +
  geom_point()+
  geom_smooth(method = "lm",se = F,col="grey")+
  geom_errorbar(aes(ymin=ci_min, ymax=ci_max))+
  theme(axis.text.x = element_text(angle = -90, hjust = 0,vjust=.5))+
  scale_x_continuous(breaks = c(3000,3500,3850,4130),name = "Elevation (m)")+
  facet_wrap( ~ trait,nrow = 2,ncol=5)




ggplot(data = recipient_site_moments_2016_c , aes(x = elevation,y = mean)) +
  geom_point()+
  geom_smooth(method = "lm",se = F,col="grey")+
  geom_errorbar(aes(ymin=ci_min, ymax=ci_max))+
  theme(axis.text.x = element_text(angle = -90, hjust = 0,vjust=.5))+
  scale_x_continuous(breaks = c(3000,3500,3850,4130),name = "Elevation (m)")+
  facet_wrap( ~ trait,nrow = 2,ncol=5)

  
##########################################################################################
#iii) a cool figure showing some examples of shifts in community trait distributions through time 
    #and perhaps showing differences in trait distributions across the 4 sites? 

source("trait_distributions/r_scripts/plot_histograms_treat_trait_year.R") #to look at trait dist across the 4 sites


#########################################################################################

#summary table of trait v elevation
library(gridExtra)
source("trait_distributions/r_scripts/summarize_elev_gradient_pvals.R")
O_trait_vs_elev_sig<-summarize_elevation_gradient_pvals(full_file_directory = file_directory_native,treatment = "O")
grid.table(O_trait_vs_elev_sig)

grid.table(trait_x_elev_sig)

##########################################################################################

# ii.b)  add pvals, r2s for significant lines
#iii) a cool figure showing some examples of shifts in community trait distributions through time 
      #and perhaps showing differences in trait distributions across the 4 sites? 
#iV) for traits along the gradient multi-panel plot showing control CWM vs elevation, w CIs






#####################################
#Histogram along gradient
  #in hist csvs, rows=replicates, columns=pct cover

#file_directory_native
source("trait_distributions/r_scripts/plot_histograms_treat_trait_year.R")
library(ggplot2)
full_file_directory_native<-"C:/Users/Brian/Desktop/current_projects/transplant/trait_distributions/non_gramminoids/native_site/"

plot_histograms_treatment_trait_year(full_file_directory = full_file_directory_native,trait = "SLA_cm2_g",treatment = "OTC",year = 2016,bw = 0.05)
plot_histograms_treatment_trait_year(full_file_directory = full_file_directory_native,trait = "SLA_cm2_g",treatment = "O",year = 2016,bw = 0.05)
plot_histograms_treatment_trait_year(full_file_directory = full_file_directory_native,trait = "LDMC",treatment = "O",year = 2016,bw = 0.05,xlim = c(-.25,.25))
plot_histograms_treatment_trait_year(full_file_directory = full_file_directory_native,trait = "N_percent",treatment = "C",year = 2016,bw = 0.05,xlim = c(-1,1))
plot_histograms_treatment_trait_year(full_file_directory = full_file_directory_native,trait = "dN15_percent",treatment = "C",year = 2016,bw = 0.05,xlim = c(-1,1))
plot_histograms_treatment_trait_year(full_file_directory = full_file_directory_native,trait = "dN15_percent",treatment = "C",year = 2016,bw = 0.1,xlim = c(-1,1))
plot_histograms_treatment_trait_year(full_file_directory = full_file_directory_native,trait = "dN15_percent",treatment = "C",year = 2016,bw = 0.2,xlim = c(-1,1))

plot_histograms_treatment_trait_year(full_file_directory = full_file_directory_native,trait = "NP_ratio",treatment = "C",year = 2016,bw = 0.05,xlim = c(-1,1))

source("trait_distributions/r_scripts/plot_histograms_treatment_trait_site.R")
#leaf area, N:P, d15N  relative to the control plots

plot_histograms_treatment_trait_site(full_file_directory = full_file_directory_native,trait = "NP_ratio",treatment = 3,
                                     site = "H",control = 2012,xlim = c(-1,1),bw = 0.1)

plot_histograms_treatment_trait_site(full_file_directory = full_file_directory_native,trait = "dN15_percent",treatment = 3,
                                     site = "H",control = 2012,xlim = c(-1,1),bw = 0.1)




#Making a fuckton of plots for treatment x trait x site

tts<-unique(moments_fixed[c("trait","treatment","site")])


for(i in 1:nrow(tts)){

print(paste(round(i/nrow(tts)*100,digits = 2),"% done"))  
plot_i<-plot_histograms_treatment_trait_site(full_file_directory = full_file_directory_native,trait = tts$trait[i],treatment = tts$treatment[i],
                                       site = tts$site[i],control = 2012,xlim = c(-1,1),bw = 0.2)
  
ggsave(plot = plot_i,
       filename = paste("C:/Users/Brian/Google Drive/China_PFTC12_distribution_output/non_gramminoids/distribution_plots/distributions_vs_time/",
                        "assume_fixed_traits_",tts$trait[i],
                        "_treatment_",tts$treatment[i],
                        "_site",tts$site[i],
                        ".jpeg",sep = ""),
       device = "jpeg")  

  
  
}




#Making a fuckton of plots for treatment x year x trait

tty<-unique(moments_fixed[c("trait","treatment","year")])


for(i in 1:nrow(tty)){
  
  print(paste(round(i/nrow(tty)*100,digits = 2),"% done"))  
  
  plot_i<-plot_histograms_treatment_trait_year(full_file_directory = full_file_directory_native,trait = tty$trait[i],treatment = tty$treatment[i],
                                                year=tty$year[i],xlim = c(-1,1),bw = 0.2)
  
  
  ggsave(plot = plot_i,
         filename = paste("C:/Users/Brian/Google Drive/China_PFTC12_distribution_output/non_gramminoids/distribution_plots/distributions_vs_elevation/",
                          "assume_fixed_traits_",tty$trait[i],
                          "_treatment_",tty$treatment[i],
                          "_year",tty$year[i],
                          ".jpeg",sep = ""),
         device = "jpeg")  
  
  
  
}

######################################

#Send CWM values to Richard

saveRDS(object = moments_fixed[c("turf","treatment","site","year","trait","mean" )],file = "C:/Users/Brian/Desktop/China_pftc_cwm_nongramminoids.rds")



#####################################

#joyplots : different elevations along y axis, different years on x




