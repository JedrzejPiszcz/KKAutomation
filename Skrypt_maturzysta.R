generuj_maturzysta<-function(Data){
  
  sprawdz_warunki<-function(Warunki, tekst){
    
    lista_warunkow<-list(length(names(Warunki)))
    
    for(i in 1:length(names(Warunki))){
      lista_warunkow[i]<-list(Warunki[,i])
    }
  
    
    
    #Warunki$LIC,Warunki$ROK,Warunki$CUDZ
    
    kolumna<-character(nrow(Warunki))
 
    kolumna[Reduce(intersect, lista_warunkow)]<-tekst
    
    return(kolumna)
  }
  
  Warunki<-data.frame(LIC=integer(length(Data$id)),
                      ROK=integer(length(Data$id)),
                      CUDZ=integer(length(Data$id))
                      )
  
  Warunki$LIC[which(Data$Produkt=="LIC")]                   <-which(Data$Produkt=="LIC")
  Warunki$ROK[which(Data$Rok.ukoñczenia==Data$rok.bazy[1])] <-which(Data$Rok.ukoñczenia==Data$rok.bazy[1])
  Warunki$CUDZ[which(Data$CudzoziemiecR=="Polak")]          <-which(Data$CudzoziemiecR=="Polak")
  
  
  maturzysta1<-sprawdz_warunki(Warunki, "maturzysta")
  
  #print(length(maturzysta1))
  
  Warunki$LIC[which(Data$Produkt=="LIC")]                                   <-which(Data$Produkt=="LIC")
  Warunki$ROK[which(Data$kat_lic_sum=="do 18" | Data$kat_lic_sum=="do 19")] <-which(Data$kat_lic_sum=="do 18" | Data$kat_lic_sum=="do 19")
  Warunki$CUDZ[which(Data$CudzoziemiecR=="Polak")]                          <-which(Data$CudzoziemiecR=="Polak")
  
  maturzysta2<-sprawdz_warunki(Warunki, "maturzysta")
  
  Data$maturzysta<-maturzysta1
  
  Data$maturzysta[which(maturzysta1=="")]<-maturzysta2[which(maturzysta1=="")]
  
  Warunki$LIC[which(Data$Produkt!="")]                                 <-which(Data$Produk!="")
  Warunki$ROK[which(Data$Rok.ukoñczenia!=as.numeric(Data$rok.bazy[1]))]<-which(Data$Rok.ukoñczenia!=as.numeric(Data$rok.bazy[1]))
  Warunki$CUDZ[which(Data$CudzoziemiecR=="Polak")]                     <-which(Data$CudzoziemiecR=="Polak")
  
  starszy1<-sprawdz_warunki(Warunki, "starszy maturzysta")
  
  Data$maturzysta[which(Data$maturzysta=="")]<-starszy1[which(Data$maturzysta=="")]
  
  Data$maturzysta[which(Data$maturzysta=="")]<-"bd"
  
  #dzia³a dobrze dla powy¿szych zestawów warunków - mo¿na to jeszcze sparametryzowaæ
  
  return(Data)
}