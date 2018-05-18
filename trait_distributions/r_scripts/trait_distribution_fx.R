#Fx to create trait distribution
#Brian Maitner 4/17/2018

#Inputs:
  #Number of replicated outputs
  # Species abundance dataframe (2 columns, first column: species name, second column: abundance)
  # Trait data frame (2 or more columns: first column: species name, columns 2+ : traits)

#Output
  #Matrix with nrows = number of replicates, ncols = total abundance

trait_distributions<-function(number_replicates, abundance_data, trait_data){
  
  output<-list()
  
  for(t in 2:ncol(trait_data)){
    trait_t<-colnames(trait_data)[t]  
    
    out_t<-NULL
    for(n in 1:number_replicates){
      rep_n<-NULL  
      
      for( i in 1:nrow( abundance_data)){
        
        species_i<-as.character(abundance_data[i,1])    
        abund_i<-abundance_data[i,2]
        traits_i<-trait_data[which(trait_data[,1]==species_i),]
        trait_ti<-na.omit(traits_i[,trait_t])
        trait_ti<-as.data.frame(trait_ti)
        
        #Dont do anything if there's no trait data
        if(nrow(trait_ti)!=0){rep_n<-c(rep_n,sample(x = trait_ti[,1],size = abund_i,replace = T))}
        
      }# i abundance loop
      
      out_t<-rbind(out_t,rep_n)
      
    }#n replicates loop
    
    output[[t-1]]<-out_t
    
  }#t traits loop  
  names(output)<-colnames(trait_data)[2:ncol(trait_data)]
  
  return(output)  
  
}# trait_distribution function

#################################################
