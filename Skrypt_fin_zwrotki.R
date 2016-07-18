generuj_fin_zwrotki<-function(Data){
  
  col_index<-grep("^zwrotki\\d_2016_R", names(Data), ignore.case = T)
  
  col_names<-character(length(col_index))
  
  for(i in 1:length(col_index)){
    col_names[i]<-names(Data)[col_index[i]]
  }
  
  Data$Zwrotki_FIN<-"NIEZWROTKOWICZ"
  
  for(i in col_names){
    l<-lappend(which(Data[[i]]=="ZWROTKOWICZ"))
  }
  
  concat_col_names<-""
  
  for(i in col_names){
    concat_col_names<-paste0(concat_col_names, "which(Data$", i, "==ZWROTKOWICZ),")
  }
  
  concat_col_names<-substr(concat_col_names, 1, (nchar(concat_col_names)-1))
  
  
  
  Data$Zwrotki_FIN[union(which(Data$zwrotki2_2016_R=="ZWROTKOWICZ"), 
                         which(Data$zwrotki1_2016_R=="ZWROTKOWICZ"))]<-"ZWROTKOWICZ"

    
  
  return(Data)
}