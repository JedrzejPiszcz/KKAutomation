usun_puste<-function(Data){
  
  Data<-sapply(Data, as.character) #konwersja NA na empty string aby zachowaæ zgodnoœæ miêdzy read.csv i read.xlsx
  
  Data[is.na(Data)]<-""
  
  Data<-as.data.frame(Data)
  
  drops<-which(!nzchar(as.character(Data$Filia))) #wybiera pola z kolumny Filia o d³ugoœci==0
    
  if(length(drops)>0){Data<-Data[-c(drops), ]}    #zostawia tylko pola inne ni¿ "drops"
  
  return(Data)
}
