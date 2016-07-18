generuj_lokalizacje<-function(Data){
  library(openxlsx)
  
  Lokalizacje<- read.xlsx("../KK/kody_pocztowe_lat_lon_ang.xlsx", sheet = 1)
  
  Unikaty<-intersect(Data$Kod.korespondencji, Lokalizacje$PNA)
  
  KOD            <-NULL
  MIEJSCOWOŒÆ    <-NULL
  GMINA          <-NULL
  POWIAT         <-NULL
  POWIAT.ANG     <-NULL
  WOJEWÓDZTWO    <-NULL
  WOJEWÓDZTWO.ANG<-NULL
  LAT            <-NULL
  LON            <-NULL
  
  new_cols <- c("Gmina","Powiat","Powiat_ANG","Województwo","Województwo_ANG","LAT", "LON")
  Data<-Data[ , !(names(Data) %in% new_cols)]
  
  Data$Gmina<-""
  Data$Powiat<-""
  Data$Powiat_ANG<-""
  Data$Województwo<-""
  Data$Województwo_ANG<-""
  Data$LAT<-""
  Data$LON<-""
  
  total<-length(Unikaty)
  pb <- txtProgressBar(min = 0, max = total, style = 3) 
  for(i in 1:total){
   wektor<-which(Unikaty[i]==Lokalizacje$PNA)
      for(j in wektor){
        KOD            <-append(KOD,             Lokalizacje$PNA[j])
        MIEJSCOWOŒÆ    <-append(MIEJSCOWOŒÆ,     Lokalizacje$MIEJSCOWOŒÆ[j])
        GMINA          <-append(GMINA,           Lokalizacje$GMINA[j])
        POWIAT         <-append(POWIAT,          Lokalizacje$POWIAT[j])
        POWIAT.ANG     <-append(POWIAT.ANG,      Lokalizacje$POWIAT.ANG[j])
        WOJEWÓDZTWO    <-append(WOJEWÓDZTWO,     Lokalizacje$WOJEWÓDZTWO[j])
        WOJEWÓDZTWO.ANG<-append(WOJEWÓDZTWO.ANG, Lokalizacje$Woj.ang[j])
        LAT            <-append(LAT,             Lokalizacje$lat[j])
        LON            <-append(LON,             Lokalizacje$lon[j])
    }

    setTxtProgressBar(pb, i)
  }
  df_list<-list(KOD, GMINA, MIEJSCOWOŒÆ, POWIAT, POWIAT.ANG, WOJEWÓDZTWO, WOJEWÓDZTWO.ANG, LAT, LON)
  found<-as.data.frame(df_list, col.names = c("KOD", "GMINA", "MIEJSCOWOŒÆ", "POWIAT", "POWIAT.ANG", "WOJEWÓDZTWO", "WOJEWÓDZTWO.ANG", "LAT", "LON"))
 
  drukuj_czas(start.time, "Zakoñczono przygotowywanie danych dla lokalizacji.")
  
  
  #max_instances<-length(which(found$KOD==names(which.max(table(found$KOD))))) #maskymalna liczba wyst¹pieñ danej wartoœci w wektorze; je¿eli list jest posortowana to ogranicza obszar do poszukiwania
  
  total<-length(Data$id)
  pb <- txtProgressBar(min = 0, max = total, style = 3) 
  
  for(i in 1:total){ 
    
    correct_KOD<-which(as.character(Data$Kod.korespondencji[i])==found$KOD)

    for (j in correct_KOD)   #sprawdza kod pocztowy z miastem zamieszkania lub urodzenia - najwieksza poprawnosc
    if(nzchar(as.character(Data$Miejscowoœæ[i]))){  
      if(length(agrep(Data$Miejscowoœæ[i],       found$MIEJSCOWOŒÆ[j], ignore.case=T, max.distance=1.0 ))>0){
        Data$Gmina[i]          <-as.character(found$GMINA[j])
        Data$Powiat[i]         <-as.character(found$POWIAT[j])
        Data$Powiat_ANG[i]     <-as.character(found$POWIAT.ANG[j])
        Data$Województwo[i]    <-as.character(found$WOJEWÓDZTWO[j])
        Data$Województwo_ANG[i]<-as.character(found$WOJEWÓDZTWO.ANG[j])
        Data$LAT[i]            <-as.character(found$LAT[j])
        Data$LON[i]            <-as.character(found$LON[j])
      }
    }else if(nzchar(as.character(Data$Miejsce.urodzenia[i]))){
      if(length(agrep(Data$Miejsce.urodzenia[i], found$MIEJSCOWOŒÆ[j], ignore.case=T, max.distance=1.0 ))>0){
        Data$Gmina[i]          <-as.character(found$GMINA[j])
        Data$Powiat[i]         <-as.character(found$POWIAT[j])
        Data$Powiat_ANG[i]     <-as.character(found$POWIAT.ANG[j])
        Data$Województwo[i]    <-as.character(found$WOJEWÓDZTWO[j])
        Data$Województwo_ANG[i]<-as.character(found$WOJEWÓDZTWO.ANG[j])
        Data$LAT[i]            <-as.character(found$LAT[j])
        Data$LON[i]            <-as.character(found$LON[j])
      }
    }
    
    if(!nzchar(as.character(Data[[new_cols[1]]][i]))){ #sprawdza tylko kod pocztowy dla pozosta³ych rekordów
      Data$Gmina[i]          <-as.character(found$GMINA[correct_KOD[1]])
      Data$Powiat[i]         <-as.character(found$POWIAT[correct_KOD[1]])
      Data$Powiat_ANG[i]     <-as.character(found$POWIAT.ANG[correct_KOD[1]])
      Data$Województwo[i]    <-as.character(found$WOJEWÓDZTWO[correct_KOD[1]])
      Data$Województwo_ANG[i]<-as.character(found$WOJEWÓDZTWO.ANG[correct_KOD[1]])
      Data$LAT[i]            <-as.character(found$LAT[correct_KOD[1]])
      Data$LON[i]            <-as.character(found$LON[correct_KOD[1]])
      }

    setTxtProgressBar(pb, i)
  }
  
    not_found<-which(is.na(Data[[new_cols[1]]]))
    
    for (i in not_found){   #sprawdznie po miescie rekordów, których nie znaleziono w bazie
      if(nzchar(as.character(Data$Miejscowoœæ[i]))){
          values<-agrep(Data$Miejscowoœæ[i], found$MIEJSCOWOŒÆ, ignore.case=T, max.distance=1.0 )
        if(length(values)>0){
          
          #match(names(sort(table(found$GMINA[values]),decreasing=TRUE)[1]), as.character(found$GMINA))
          Data$Gmina[i]          <-found$GMINA[values[1]]
          Data$Powiat[i]         <-found$POWIAT[values[1]]
          Data$Powiat_ANG[i]     <-found$POWIAT.ANG[values[1]]
          Data$Województwo[i]    <-found$WOJEWÓDZTWO[values[1]]
          Data$Województwo_ANG[i]<-found$WOJEWÓDZTWO.ANG[values[1]]
          Data$LAT[i]            <-found$LAT[values[1]]
          Data$LON[i]            <-found$LON[values[1]]

        }
      }else{
        Data$Gmina[i]          <-"bd"
        Data$Powiat[i]         <-"bd"
        Data$Powiat_ANG[i]     <-"bd"
        Data$Województwo[i]    <-"bd"
        Data$Województwo_ANG[i]<-"bd"
        Data$LAT[i]            <-"bd"
        Data$LON[i]            <-"bd"
      }
    }
  
  return(Data)
}