#helper function designed to calculate the 

calc_ci<-function(x,ci=0.95){
  mean<-mean(x)
  amt_to_trim <- (1-ci)/2    
  n_to_trim<-round(length(x)*amt_to_trim)  
  x<-sort(x)
  x<-x[-1:-n_to_trim]
  x<-x[-(1+length(x)-n_to_trim  ):-length(x)]
  
  ci_min<-min(x)
  ci_max<-max(x)
  
  output<-as.data.frame(cbind(ci_min,mean,ci_max))
  
  return(output)
  
}
