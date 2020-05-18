  library(wakefield)
  library(ggplot2) 
  library(plotly) 
  df<-0
  num<-0
  thresh<-4
  t=0
  n<-100
  x<-0
  m<-100 # Initial percentages to consider
  data <- as.data.frame(matrix(0, ncol = 1, nrow = 100))
  df<-matrix(nrow=n+2, ncol=n+2)
  
  
  ex<-100  # Number of experiments
  
  for (t in 1:ex)
  {
  
    results<-matrix(0L,ncol=5)
  
  
  
  x<-2
  while (x <=m )
  {
  for (i in 1:(n+1)){
  for (j in 1:(n+1)){
  df[i,j]<-r_sample_logical(1, prob=c(1-(x/200),(x/200)  ))
  }
  }
  
  df[1,]<-df[n+1,]
  df[,1]<-df[,n+1]
  
  df[n+2,]<-df[2,]
  df[,n+2]<-df[,2]
  
  initial<-table(unlist(df[-c(1,n+2),-c(1,n+2)]))  
  
  for (t in 1:1000){
    before<-table(unlist(df[-c(1,n+2),-c(1,n+2)]))  
  for (i in 2:(n+1)){
    for (j in 2:(n+1)){
     
      if (df[i,j]==FALSE){}
      
      if (i!=1 && j!=1 && i!=n && j!=n){
      num<-df[i,j+1]+df[i,j-1]+df[i+1,j]+df[i-1,j]+df[i+1,j+1]+df[i-1,j-1]+df[i-1,j+1]+df[i+1,j-1]
     
      
      
      }
      
      else {
        
        
      }
      
      if (num >=thresh){
        df[i,j]=TRUE
        
      }
      
      
      }
  }
    after<-table(unlist(df[-c(1,n+2),-c(1,n+2)]))  
    
  if (after[1]==before[1])
  {
    break  
  }  
  }
  results<- rbind(results, c(0,0,0,0,0))
  results[x,1]<-results[x,1]+initial[2] # initial % of black, final % of black, time steps, thresh, n
  results[x,2]<-results[x,2]+after[2] # initial % of black, final % of black, time steps, thresh, n
  results[x,3]<-results[x,3]+t # initial % of black, final % of black, time steps, thresh, n
  results[x,4]<-results[x,4]+thresh # initial % of black, final % of black, time steps, thresh, n
  results[x,5]<-results[x,5]+n # initial % of black, final % of black, time steps, thresh, n
  
  x<-x+1
  }
  
  
  }
  
  
  
  
  plot(results[,1]/((n^2)),results[,2]/((n^2)), xlim=c(0,.1), ylim=c(0,1)  )
lines(results[,1]/((n^2)),results[,2]/((n^2)), xlim=c(0,.1), ylim=c(0,1)  )
  
  #for (i in 1:8){
  #points(data[,1+5*i]/n^2,data[,2+5*i]/n^2)
  #}