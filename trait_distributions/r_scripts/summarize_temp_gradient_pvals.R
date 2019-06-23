#summary table of trait v elevation

#Input: file directory, temp file

#Output:  table of p-val of trait v elevation with dimensions year x trait

summarize_temp_gradient_pvals<-function(full_file_directory,treatment="C",china_env_data = "trait_distributions/ScrubbedCHINAdata.csv"){
  
  china_env_data<-read.csv(china_env_data)
  temps<-ddply(china_env_data,"PlotID",summarise,mean_air_temp=mean(AirTemp,na.rm = T),mean_soil_temp=mean(SoilTemp,na.rm = T))
  temps$PlotID<-toupper(x = temps$PlotID)
  
  
  files<-list.files(full_file_directory,full.names = T)  
  file_index<-NULL
  
  for(i in 1:length(files)){
    file_i<-files[i]    
    file_name_i <- strsplit(x = file_i,split = "/")[[1]][length(strsplit(x = file_i,split = "/")[[1]]  )]  
    file_name_i<-strsplit(file_name_i,split = ".csv")[[1]][1]
    turf<-unlist(strsplit(file_name_i,".",fixed = T))[1]
    treatment_i<-strsplit(turf,"-")[[1]][2]
    site<-strsplit(x = turf,split = "")[[1]][1]
    year_i<-unlist(strsplit(file_name_i,".",fixed=T))[2]
    trait_i<-unlist(strsplit(file_name_i,".",fixed = T))[3]
    assumption<-unlist(strsplit(file_name_i,".",fixed = T))[4]  
    file_index<-rbind(file_index,cbind(files[i],turf,treatment_i,site,year_i,trait_i,assumption))
  }# file loop end
  rm(file_i,file_name_i,turf,treatment_i,site,year_i,trait_i,assumption,i)
  
  file_index <- as.data.frame(file_index)
  file_index <- file_index[which(file_index$treatment_i==treatment),]
  file_index$elevation<-NA
  file_index$elevation<-as.numeric(multigsub(pattern = c("L","M","A","H"),replacement = c(3000,3500,3850,4130),file_index$site))
  file_index$turf<-as.character(file_index$turf)
  colnames(temps)[1]<-"turf"
  
  file_index<- join(x = file_index,
       y = temps,by="turf"
       )
  
  
  trait_x_temp_sig<-matrix(nrow = length(unique(as.character(file_index$trait_i))), ncol = length(unique(as.numeric(as.character(file_index$year_i)))))
  trait_x_temp_sig<-as.data.frame(trait_x_temp_sig)
  colnames(trait_x_temp_sig)<-unique(as.numeric(as.character(file_index$year_i)))
  rownames(trait_x_temp_sig)<-unique(as.character(file_index$trait_i))
  
  for(t in 1:length(unique(as.character(file_index$trait_i)))){
    trait<-unique(as.character(file_index$trait_i))[t]
    for(y in 1:length(unique(as.numeric(as.character(file_index$year_i))))){
      year<-unique(as.numeric(as.character(file_index$year_i)))[y]  
      
      files_yt<-file_index[which(file_index$trait_i==trait & file_index$year_i==year),]
      data_yt<-NULL
      
      
      for(i in 1:nrow(files_yt)){
        mean<-mean(unlist(read.csv(as.character(files_yt$V1[i]))) ) 
        suppressWarnings(data_i<-cbind(files_yt[i,],mean))
        data_yt<-rbind(data_yt,data_i)
      }
      
      
      summary_lm_yt<-summary(lm(formula = data_yt$mean~data_yt$mean_air_temp))
      
      if(summary_lm_yt$coefficients[8]<0.05){sig<-"*"}
      if(summary_lm_yt$coefficients[8]>0.05 & summary_lm_yt$coefficients[8]<0.1){sig<-"."}
      if(summary_lm_yt$coefficients[8]>0.1){sig<-""}
      
      
      trait_x_temp_sig[t,y]<-paste(round(summary_lm_yt$r.squared,digits = 2),sig)
      
    }#year loop
    
  }#trait loop  

  output<-list()
  output[[1]]<-trait_x_temp_sig
  names(output)<-"air_temp"
  #names(output)
  #output$air_temp
  
  
  
  
  
  trait_x_temp_sig<-matrix(nrow = length(unique(as.character(file_index$trait_i))), ncol = length(unique(as.numeric(as.character(file_index$year_i)))))
  trait_x_temp_sig<-as.data.frame(trait_x_temp_sig)
  colnames(trait_x_temp_sig)<-unique(as.numeric(as.character(file_index$year_i)))
  rownames(trait_x_temp_sig)<-unique(as.character(file_index$trait_i))
  
  for(t in 1:length(unique(as.character(file_index$trait_i)))){
    trait<-unique(as.character(file_index$trait_i))[t]
    for(y in 1:length(unique(as.numeric(as.character(file_index$year_i))))){
      year<-unique(as.numeric(as.character(file_index$year_i)))[y]  
      
      files_yt<-file_index[which(file_index$trait_i==trait & file_index$year_i==year),]
      data_yt<-NULL
      
      
      for(i in 1:nrow(files_yt)){
        mean<-mean(unlist(read.csv(as.character(files_yt$V1[i]))) ) 
        suppressWarnings(data_i<-cbind(files_yt[i,],mean))
        data_yt<-rbind(data_yt,data_i)
      }
      
      
      summary_lm_yt<-summary(lm(formula = data_yt$mean~data_yt$mean_soil_temp))
      
      if(summary_lm_yt$coefficients[8]<0.05){sig<-"*"}
      if(summary_lm_yt$coefficients[8]>0.05 & summary_lm_yt$coefficients[8]<0.1){sig<-"."}
      if(summary_lm_yt$coefficients[8]>0.1){sig<-""}
      
      
      trait_x_temp_sig[t,y]<-paste(round(summary_lm_yt$r.squared,digits = 2),sig)
      
    }#year loop
    
  }#trait loop  
  
  output[[2]]<-trait_x_temp_sig
  names(output)<-c("air_temp","soil_temp")
  
  output$air_temp
  output$soil_temp
  
  return(output)
  
  
  
}

