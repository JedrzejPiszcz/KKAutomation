generuj_obywatelstwo<-function(Data){
  
  unikaty<-unique(Data$Obywatelstwo)
  
  lista_1<-as.character(unikaty[agrep("^pol", unikaty, ignore.case = T, max.distance = 1.0)])
  unikaty<-setdiff(unikaty, lista_1)
  
  lista_2<-as.character(unikaty[agrep("^pl", unikaty, ignore.case = T, max.distance = 1.0)])
  unikaty<-setdiff(unikaty, lista_2)
  
  lista_3<-as.character(unikaty[agrep("polskie", unikaty, ignore.case=T, max.distance = 2.0)])
  unikaty<-setdiff(unikaty, lista_3)
  
  lista<-c(lista_1,lista_2, lista_3)
  
  cudzoziemiec1<-rep("bd",length(Data$id))
  
  for( i in lista){
    
      cudzoziemiec1[which(Data$Obywatelstwo==i)]<-"Polak"
  }

      cudzoziemiec1[which(nzchar(as.character(Data$Cudzoziemiec)))]<-"Cudzoziemiec"
      
      
      
  total<-length(Data$id)
      
  for( i in 1:total){            #cos tu jest nie tak - dodatkowy warunek na pole obywatelstwo, sprawdzic
     
     if(cudzoziemiec1[i]=="bd" && (nzchar(as.character(Data$Obywatelstwo[i])))){
        cudzoziemiec1[i]<-"Cudzoziemiec"
     }
    
     if(cudzoziemiec1[i]=="bd"){  
      # print(i)
          
         if(sprawdz_PESEL(Data$PESEL[i])){
           
            cudzoziemiec1[i]<-"Polak"
           }
     }  
  }  
      
      
      Data$CudzoziemiecR<-cudzoziemiec1
  
  return(Data)
}

#trzeba dodatkowo sprawdziæ jeszcze jakieœ pole - pozostaje du¿o bd ze wzglêdu nie niuzupe³nione pola cudzioziemiec i obywatelstwo