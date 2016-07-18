generuj_status_rekrutacji<-function(Data){
  
  new_cols <- c("rekrutacjaStatus")
  Data<-Data[ , !(names(Data) %in% new_cols)]
  
  unikaty<-as.character(unique(Data$Status.Rek))
  #unikaty<-unikaty[-c(which(!nzchar(unikaty)))]
  
  stworz_liste<-function(hasla, kolumna){

    lista<-character()
    for(i in hasla){lista<-c(lista, as.character(unique(kolumna)[grep(i, unique(kolumna) ,ignore.case = T)]))}
    
    return(lista)
  }
  
  przypisz_liste<-function(lista, kolumna){
    
    nazwa<-deparse(substitute(lista))        
    status<-character(length(kolumna))       
    
    for(i in lista){
      status[which(kolumna==i)]<-substr(nazwa, 7, nchar(nazwa))  
    }
    #wycina "lista_" i w nazwie statusu pozostawia resztê nazwy listy np "rezygnacja"
    return(status)
  }
   
  lista_rezygnacja<-stworz_liste("rezygnacja", Data$Status.Rek)
  unikaty<-setdiff(unikaty, lista_rezygnacja)
  
  lista_ok<-stworz_liste(c("^ok", "przyjêty"), Data$Status.Rek)
  unikaty<-setdiff(unikaty, lista_ok)
  
  lista_potwierdzony<-stworz_liste(c("^potwierdz"), Data$Status.Rek)
  unikaty<-setdiff(unikaty, lista_potwierdzony)
  
  
  lista_dok_dostarczone<-stworz_liste(c("dostarcz"), Data$Status.Rek)
  unikaty<-setdiff(unikaty, lista_dok_dostarczone)
  
  lista_pozostale<-setdiff(unikaty, lista_dok_dostarczone)
  
  #print(Data$Status[which(Data$Status=="")]<-przypisz_liste(lista_pozostale, Data$Status.Rek)[which(Data$Status=="")])
  Data$rekrutacjaStatus<-""
  
  Data$rekrutacjaStatus[which(Data$rekrutacjaStatus=="")]<-przypisz_liste(lista_rezygnacja,       Data$Status.Rek)[which(Data$rekrutacjaStatus=="")]
  Data$rekrutacjaStatus[which(Data$rekrutacjaStatus=="")]<-przypisz_liste(lista_ok,               Data$Status.Rek)[which(Data$rekrutacjaStatus=="")]
  Data$rekrutacjaStatus[which(Data$rekrutacjaStatus=="")]<-przypisz_liste(lista_potwierdzony,     Data$Status.Rek)[which(Data$rekrutacjaStatus=="")]
  Data$rekrutacjaStatus[which(Data$rekrutacjaStatus=="")]<-przypisz_liste(lista_dok_dostarczone,  Data$Status.Rek)[which(Data$rekrutacjaStatus=="")]
  Data$rekrutacjaStatus[which(Data$rekrutacjaStatus=="")]<-przypisz_liste(lista_pozostale,        Data$Status.Rek)[which(Data$rekrutacjaStatus=="")]
  
  return(Data)
}