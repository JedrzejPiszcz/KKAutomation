drukuj_czas<- function(czas_pocz.time, tekst){
  print(tekst)
  end.time<-Sys.time()
  print(paste("Czas od pocz�tku:", end.time - czas_pocz.time))
}