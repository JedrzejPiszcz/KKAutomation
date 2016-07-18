generuj_zwrotki<-function(Data, Zwrotki, lista_Data, lista_Zwrotki){
  library(openxlsx) #chyba najlepsza biblioteka do duzych plikow
  wd<-"C:/Users/studen001/Desktop/JPiszcz/Jedrzej/R_test" #ustawiæ working directory
  if(getwd()!=wd){setwd(wd)}
  env_ZIPCMD<-"C:/Rtools/bin/zip"                           #nalezy ustawic sciezke do pliku zip w pakiecie rtools
  if(Sys.getenv("R_ZIPCMD")!=env_ZIPCMD){Sys.setenv(R_ZIPCMD = env_ZIPCMD)}        
  #z jakiegoœ powodu resetuje siê po ponownym uruchomieniu PC
  #Konieczne do ZAPISYWANIA plików .xlsx
  
  #trzeba doczytywaæ drugi plik ze zwrotkami


#deklaracja nazw pol w "Data", "Zwrotki"

#lista_Data<-c("Imiê", "Nazwisko", "Kod.zamieszkania", "Kod.korespondencji", "Email","Telefon.kom.", "id") 

#lista_Zwrotki<-c("kod1", "e-mail", "telefon")

#START

#Wczytywanie danych

#if(exists("Data")==F){
#  Data <- read.xlsx("../KK/Baza KK 20.06.2016/baza_maj_20.06.2016.xlsx", sheet = 1)
#}

drukuj_czas(start.time, "Wczytano dane.")

#if(exists("Zwrotki")==F){
#  Zwrotki <- read.xlsx("../KK/Bazy zwrotek/Zwrotki 2011-2015 - okrojone.xlsx", sheet = 1)
#}

#drukuj_czas("Wczytano zwrotki.")

#Deklaracja wektorów i przygotowanie danych - porownywane elementy w intersekcji to lista_Zmiennych[1] vs lista_Zmienne_Zwrotki[1]

lista_Zmiennych<-c(paste(lista_Data[5]),
                   paste(lista_Data[6]),
                   "kod_1",
                   "kod_2",
                   "kod_3",
                   "kod_4")

lista_Zmienne_Zwrotki<-c(lista_Zwrotki[2],
                         lista_Zwrotki[3],
                         lista_Zwrotki[1],
                         lista_Zwrotki[1],
                         lista_Zwrotki[1],
                         lista_Zwrotki[1])

rozmiar_Data <- length(Data[[lista_Data[7]]])

#obiekt danych ze znalezionymi wartoœciami wspólnymi

found<- data.frame(a=character(rozmiar_Data),b=character(rozmiar_Data),
                   c=character(rozmiar_Data),d=character(rozmiar_Data),
                   e=character(rozmiar_Data),f=character(rozmiar_Data), stringsAsFactors = F)

colnames(found)<- c(lista_Zmiennych[1],
                    lista_Zmiennych[2],
                    lista_Zmiennych[3],
                    lista_Zmiennych[4],
                    lista_Zmiennych[5], 
                    lista_Zmiennych[6])


for (j in lista_Zmiennych){
  found[[j]][1:length(Data[[lista_Data[7]]])]<-NA
}

#usiniecie wartosci NA z wektorów i zamiana na puste pole

Data[[lista_Data[1]]][is.na(Data[[lista_Data[1]]])]<- ""
Data[[lista_Data[2]]][is.na(Data[[lista_Data[2]]])]<- ""
Data[[lista_Data[3]]][is.na(Data[[lista_Data[3]]])]<- ""
Data[[lista_Data[4]]][is.na(Data[[lista_Data[4]]])]<- ""

drukuj_czas(start.time, "Przygotowano dane.")

#Generowanie kodow imie, nazwisko, kod pocztowy

Data[[lista_Zmiennych[1]]]<-tolower(Data[[lista_Data[5]]])
Data[[lista_Zmiennych[2]]]<-tolower(Data[[lista_Data[6]]])
Data[[lista_Zmiennych[3]]]<-tolower(paste(Data[[lista_Data[1]]], Data[[lista_Data[2]]], Data[[lista_Data[3]]], sep=""))
Data[[lista_Zmiennych[4]]]<-tolower(paste(Data[[lista_Data[2]]], Data[[lista_Data[1]]], Data[[lista_Data[3]]], sep=""))
Data[[lista_Zmiennych[5]]]<-tolower(paste(Data[[lista_Data[1]]], Data[[lista_Data[2]]], Data[[lista_Data[4]]], sep=""))
Data[[lista_Zmiennych[6]]]<-tolower(paste(Data[[lista_Data[2]]], Data[[lista_Data[1]]], Data[[lista_Data[4]]], sep=""))


for(j in lista_Zmiennych){
  
  #intersekcja wektorów (obliczanie czêœci wspólnej)
  common_values<-intersect(Data[[j]], tolower(Zwrotki[[lista_Zmienne_Zwrotki[which (j == lista_Zmiennych)]]]))
  
  total <- length(common_values)
  if(total==0){total<-1}
  pb <- txtProgressBar(min = 0, max = total, style = 3) 
  
  #przypisanie znalezionych wartosci do wektora found
  for (i in 1:total){
    
    found[[j]][which(common_values[i]==Data[[j]])]<-Data[[j]][which(common_values[i]==Data[[j]])][1]
    
    setTxtProgressBar(pb, i)
  }
  drukuj_czas(start.time, paste("Ukonczono ", j, sep=""))
  
}

#przypisanie pol zwrotkowicz/niezwrotkowicz na podstawie wektorów w df found

total<-length(Data[[7]])
pb <- txtProgressBar(min = 0, max = total, style = 3) 

for (i in 1:total){
  if(is.na(found[[lista_Zmiennych[1]]][i])  ==T &&
     is.na(found[[lista_Zmiennych[2]]][i])   ==T &&
     is.na(found[[lista_Zmiennych[3]]][i]) ==T &&
     is.na(found[[lista_Zmiennych[4]]][i]) ==T &&
     is.na(found[[lista_Zmiennych[5]]][i]) ==T &&
     is.na(found[[lista_Zmiennych[6]]][i]) ==T   ){
                                   Data[[paste0(deparse(substitute(Zwrotki)), "_2016_R")]][i]<-"NIEZWROTKOWICZ"
  }else{
                                   Data[[paste0(deparse(substitute(Zwrotki)), "_2016_R")]][i]<-"ZWROTKOWICZ"
                                  }
  
}

#drukuj_czas(start.time, "Ukonczono przypisywanie danych na podstawie kodow.")
#write.xlsx(dane, "R_testowy_2016.xlsx")
#drukuj_czas("Ukonczono przypisywanie zwrotek.")

return(Data)
}