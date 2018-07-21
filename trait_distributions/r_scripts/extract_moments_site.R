#site x treatment x year means and CIs

extract_moments_site<-function(file_directory){
  out_summary<-NULL
  
  files<-list.files(file_directory,full.names = T)  
  
  file_index<-NULL
  
  for(i in 1:length(files)){
    file_i<-files[i]    
    file_name_i <- strsplit(x = file_i,split = "/")[[1]][length(strsplit(x = file_i,split = "/")[[1]]  )]  
    file_name_i<-strsplit(file_name_i,split = ".csv")[[1]][1]
    turf<-unlist(strsplit(file_name_i,".",fixed = T))[1]
    treatment<-strsplit(turf,"-")[[1]][2]
    site<-strsplit(x = turf,split = "")[[1]][1]
    year<-unlist(strsplit(file_name_i,".",fixed=T))[2]
    trait<-unlist(strsplit(file_name_i,".",fixed = T))[3]
    assumption<-unlist(strsplit(file_name_i,".",fixed = T))[4]  
    file_index<-rbind(file_index,cbind(files[i],turf,treatment,site,year,trait,assumption))
    rm(file_i,file_name_i,turf,treatment,site,year,trait,assumption,i)
  }

  
  file_index<-as.data.frame(file_index)
  sytta<-unique(file_index[c("site","year","trait","treatment","assumption")])
  sytta$ci_min<-NA
  sytta$mean<-NA
  sytta$ci_max<-NA
  
  for(i in 1:nrow(sytta)){
  
  files_i<-as.character(file_index$V1[which(file_index$site==sytta$site[i] &
          file_index$year==sytta$year[i] &
          file_index$trait==sytta$trait[i] &
          file_index$treatment==sytta$treatment[i] &
          file_index$assumption==sytta$assumption[i])])  
    
  data_i<-unlist(lapply(X = files_i,FUN = function(x){ unlist(read.csv(x)) }))#end lapply  
    
  ci_i<-calc_ci(x = data_i,ci = 0.95)
  
  sytta$ci_min[i]<-ci_i$ci_min  
  sytta$mean[i]<-ci_i$mean
  sytta$ci_max[i]<-ci_i$ci_max
  rm(files_i,data_i,ci_i)  
  
  }  

  
  return(sytta)
  
    
  
    
  
}#end fx