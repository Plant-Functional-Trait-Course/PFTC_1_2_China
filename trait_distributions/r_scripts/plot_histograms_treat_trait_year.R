#code to plot a specified trait x treatment x year across the gradient


plot_histograms_treatment_trait_year<-function(full_file_directory,trait,treatment,year,bw=0.005,xlim=c(-1,1)){
  
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
  
  }
  file_index<-as.data.frame(file_index)
  
  focal_files<-file_index[which(as.character(file_index$treatment_i)==treatment &
                                  as.numeric(as.character(file_index$year_i))==year &
                                  as.character(file_index$trait_i)==trait),]
  
  focal_data<-NULL
  for(i in 1:nrow(focal_files)){
    data_i<-unlist(read.csv(as.character(focal_files$V1[i])))  
    suppressWarnings(data_i<-cbind(focal_files[i,],data_i))
    focal_data<-rbind(focal_data,data_i)
  }
  
  focal_data$site<- ordered(focal_data$site, levels = c("H","A","M","L"))

  output_plot<-ggplot(focal_data, aes(x = data_i, fill = site, colour = site))+
    geom_density(bw=bw,alpha = 0.1)+xlim(xlim)+ggtitle(label = paste(trait,year,treatment))

  return(output_plot)
    
  
  
}#function  
