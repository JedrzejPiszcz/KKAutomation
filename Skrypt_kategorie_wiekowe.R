generuj_kategorie_wiekowe<-function(Data, rokBazy){
  
  library(lubridate)
  
  przypisz_kategorie<-function(wiek, kategorie){
    
    kat<-as.numeric(cut(wiek,kategorie))
    poziomy<-kategorie[kat]
    kolumna<-character(length(wiek))
    
    pb <- txtProgressBar(min = 0, max = length(wiek), style = 3)
    for (i in 1:length(wiek)){
      
      if(is.na(poziomy[i])==T){
        kolumna[i]<-paste("bd")
      }else{
      
         floor_v<-kategorie[max(which(kategorie<wiek[i]))]
         ceil_v<-kategorie[min(which(kategorie>=wiek[i]))]
      
         if((abs(floor_v-ceil_v)==1) || floor_v==ceil_v){
        
           kolumna[i]<-paste(ceil_v)
        
          }else if(floor_v==kategorie[1]){
        
           kolumna[i]<-paste("poni¿ej", ceil_v)
        
          }else if(ceil_v==kategorie[length(kategorie)]){
        
           kolumna[i]<-paste("powy¿ej", floor_v)
           
          }else{
        
            kolumna[i]<-paste("od", floor_v+1 , "do", ceil_v)
        
         }
      }
     
                               # poziomy[i]<-paste("0")   #trick ¿eby oszukaæ warunek w kolejnym if() 
                                                          #funkcja max() wymaga wyniku T lub F - NA jest niedopuszczalne, wiêc pole nie mo¿e byæ puste
                                                         
    #  if(poziomy[i]==max(poziomy, na.rm = T)){
     #                           kolumna[i]<-paste("powy¿ej", kategorie[length(kategorie)-1]) 
    #                                         }
      
      setTxtProgressBar(pb, i)    
    }
    
    return(kolumna)
  }
  
  kat_lic_sum<-c(1,18,19,20,21,22,23,24,25,30,40,99) #przedzia³y wiekowe dla lic/sum - konieczne s¹ pocz¹tkowe 1 i koñcowe 99 (lub inne wysokie)
  kat_sp_mba <-c(1,21,25,30,35,40,45,99)             #przedzia³y wiekowe dla lic/sum - konieczne s¹ pocz¹tkowe 1 i koñcowe 99 (lub inne wysokie)
  
  #rok bazy podany w argumencie funckji/mo¿e bazowaæ na roku akademickim?
  Data$rok.bazy<-rokBazy
  
  #do aktualnej daty - tutaj do roku
  #year(strptime(format(Sys.time(), format = "%d.%m.%Y"), format = "%d.%m.%Y"))-year(Data$Data.urodzenia[1]) 
  
  total<-length(Data$id)
  
  Data$Data.urodzenia<-as.Date(Data$Data.urodzenia)
  
  for(i in 1:total){
    #obliczany do roku bazy - yyyy-01-01
    if(is.Date(Data$Data.urodzenia[i])){
    Data$wiek[i]<-year(as.Date(paste(Data$rok.bazy[i], "-01-01", sep="")))-year(Data$Data.urodzenia[i])
    }else{
      if(sprawdz_PESEL(Data$PESEL[i])){
        
        r_bazy<-as.numeric(substr(Data$rok.bazy[i], 3, nchar(Data$rok.bazy)))
        r_uro <-as.numeric(substr(Data$PESEL[i], 1, (nchar(as.character(Data$PESEL[i]))-9)))
        
        Data$wiek[i]<-abs((r_bazy+100)-r_uro)%%100
      }
    }
  }
  
  #sprawdziæ dodatkowo po PESELu
  
  
  print("Przypisywanie kategori LIC SUM")
  Data$kat_lic_sum<-przypisz_kategorie(Data$wiek, kat_lic_sum)
  
  print("Przypisywanie kategori SP MBA")
  Data$kat_sp_mba <-przypisz_kategorie(Data$wiek, kat_sp_mba)
  
  return(Data)
}