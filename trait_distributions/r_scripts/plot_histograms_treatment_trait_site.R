#(iii) temporal change in turfs (experiment 3 and also  experiment 4)
#plus add the distribution of the control plot (a visual way of  seeing how different the trait distribution   is in the transplant from the destination  community but then also show the temporal dynamics of the shift toward the destination community. It would be cool to  show  shifting temporal distributions for leaf area, N:P, d15N  relative to the control plots

#control should be null, for no control, or a year, to specify a year to show as a control
plot_histograms_treatment_trait_site<-function(full_file_directory,trait,treatment,site,control=NULL,bw=0.005,xlim=c(-1,1)){
  
  files<-list.files(full_file_directory,full.names = T)  
  
  file_index<-NULL
  
  for(i in 1:length(files)){
    file_i<-files[i]    
    file_name_i <- strsplit(x = file_i,split = "/")[[1]][length(strsplit(x = file_i,split = "/")[[1]]  )]  
    file_name_i<-strsplit(file_name_i,split = ".csv")[[1]][1]
    turf<-unlist(strsplit(file_name_i,".",fixed = T))[1]
    treatment_i<-strsplit(turf,"-")[[1]][2]
    site_i<-strsplit(x = turf,split = "")[[1]][1]
    year_i<-unlist(strsplit(file_name_i,".",fixed=T))[2]
    trait_i<-unlist(strsplit(file_name_i,".",fixed = T))[3]
    assumption<-unlist(strsplit(file_name_i,".",fixed = T))[4]  
    file_index<-rbind(file_index,cbind(files[i],turf,treatment_i,site_i,year_i,trait_i,assumption))
    
  }
  file_index<-as.data.frame(file_index)
  
  focal_files<-file_index[which(as.character(file_index$treatment_i)==treatment &
                                  as.character(file_index$site_i)==site &
                                  as.character(file_index$trait_i)==trait),]
  
  focal_data<-NULL
  for(i in 1:nrow(focal_files)){
    data_i<-unlist(read.csv(as.character(focal_files$V1[i])))  
    suppressWarnings(data_i<-cbind(focal_files[i,],data_i))
    focal_data<-rbind(focal_data,data_i)
  }
  
  
  if(!is.null(control)){
    
    focal_control<-file_index[which(as.character(file_index$treatment_i)=="C" &
                                    as.character(file_index$site_i)==site &
                                    as.character(file_index$trait_i)==trait &
                                      file_index$year_i==control ),]
    focal_control$year_i<-paste(control,"control")
    
    for(i in 1:nrow(focal_control)){
      data_i<-unlist(read.csv(as.character(focal_control$V1[i])))  
      suppressWarnings(data_i<-cbind(focal_control[i,],data_i))
      focal_data<-rbind(focal_data,data_i)
    }
  
    focal_data$year_i<- ordered(focal_data$year_i, levels = c(paste(control,"control"),"2012","2013","2014","2015","2016"))  
      
  }else{
    focal_data$year_i<- ordered(focal_data$year_i, levels = c("2012","2013","2014","2015","2016"))  
  }
  
  output_plot<-ggplot(focal_data, aes(x = data_i, fill = year_i, colour = year_i))+
    geom_density(bw=bw,alpha = 0.1)+xlim(xlim)+ggtitle(label = paste(trait,site,treatment))
  
  return(output_plot)
  
  
  
}#function  
