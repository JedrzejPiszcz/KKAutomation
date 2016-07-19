generuj_lokalizacje<-function(Data){
  library(openxlsx)
  
  Lokalizacje<- read.xlsx("C:/JP/TEB Akademia/Automatyzacja KK/kody_pocztowe_lat_lon_ang.xlsx", sheet = 1)
  
  new_cols <- c("Gmina","Powiat","Powiat_ANG","Województwo","Województwo_ANG","LAT", "LON")
  Data<-Data[ , !(names(Data) %in% new_cols)]
  
  #sorted_Data<-Data[with(Data, order(Kod.korespondencji)), ]         #sortuje po kodzie korespondencji
  start.time<-Sys.time()
  sorted_Lok<-Lokalizacje[with(Lokalizacje, order(PNA)), ] 
  
  #occurences_Data<-as.data.frame(table(sorted_Data$Kod.korespondencji))   #wyznacza liczbê wyst¹pieñ danego kodu
  
  occurences_Lok<-as.data.frame(table(sorted_Lok$PNA))
  
  
  
  l<-0
  total<-nrow(Data)
  pb <- txtProgressBar(min = 0, max = total, style = 3) 
  for( i in 1:total){
    
    first_occ<-min(which(Data$Kod.korespondencji[i]==sorted_Lok$PNA))
    
    occ_number<-occurences_Lok$Freq[which(Data$Kod.korespondencji[i]==as.character(occurences_Lok$Var1))]
    
    #print(i)
    
    if(isTRUE(first_occ%%1==0)){
        for (j in first_occ:(first_occ+occ_number)){
          l<-l+1
        }
    }
    setTxtProgressBar(pb, i)
  }
  
  print(Sys.time()-start.time)
  
  return(Data) 
}
