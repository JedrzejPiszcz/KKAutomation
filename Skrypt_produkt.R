generuj_produkt<-function(Data){
  
  cant_find<-"XXXXXXXXXX" #czyms trzeba zapelnic pozostale wolne pola w df
  
  total<-length(Data$id)
  
  szukaj_produktu<-function(lista, lista_unikaty){
    produkt<-NULL
    for(i in lista){
      produkt<-c(produkt, as.character(lista_unikaty[grep(i, lista_unikaty, ignore.case = T)]))
    }
    return(produkt)
  }
  
  szukaj_w_kolumnie<-function(lista_produkt, lista_poczatkowa, listy, kolumna){
    
    Produkt<-character(length(kolumna))
    
    for( i in lista_produkt){
      
      for(j in 1:length(szukaj_produktu(listy[[i]], lista_poczatkowa))){
        
        Produkt[which(kolumna==szukaj_produktu(listy[[i]], lista_poczatkowa)[j])]<-as.character(paste(i))
        
      }
      lista_poczatkowa<-setdiff(lista_poczatkowa, szukaj_produktu(listy[[i]], lista_poczatkowa))
      
    }
    
    return(Produkt)
  }
  
  Data$Produkt<-""
  
  listy<-data.frame(LIC=character(7),
                    SUM=character(7),
                    MBA=character(7),
                    SP =character(7))
  
  listy$LIC <-c("^i st", "^in¿", "^lic", "^wsb" , "wy¿sze", "jednolite", "jmgr" )
  listy$SUM <-c("^ii st", "mgr", "mag", "sum", "usm", cant_find, cant_find)
  listy$MBA <-c("MBA", cant_find, cant_find, cant_find, cant_find, cant_find, cant_find)
  listy$SP  <-c("SP", "podyplomowe", cant_find, cant_find, cant_find, cant_find, cant_find)
 
  lista_produkt<-names(listy)
  
  #Najpierw przeszukujê polê Filia, potem Rodzaj.studiów - 
  #uzupe³niam braki z Filia danymi z pola Rodzaj.studiów, zak³adaj¹c ¿e "studia wy¿sze" to LIC
  
  lista_poczatkowa<-unique(Data$Filia)
  Produkt1<-szukaj_w_kolumnie(lista_produkt, lista_poczatkowa, listy, Data$Filia)
  
  lista_poczatkowa<-unique(Data$Rodzaj.studiów)
  Produkt2<-szukaj_w_kolumnie(lista_produkt, lista_poczatkowa, listy, Data$Rodzaj.studiów)
  
  Data$Produkt<-Produkt1
  
  Data$Produkt[which(Produkt1=="")]<-Produkt2[which(Produkt1=="")]
  
  return(Data)
}
