generuj_ID<-function(Data){
  #generuje id potrzebne w dalszych funkcjach
  
  total<-nrow(Data)
  
  for(i in 1:total){
    Data$id[i]<-i
  }
  
  #print(Data$id)
  return(Data)
}