#rm(list=ls())     #czyœci workspace

library(openxlsx) #biblioteka odpowiednia dla duzych plikow .xls/.xlsx

wd<-"C:/GIT/Automatyzacja_KK/KKAutomation" #ustawiæ working directory
if(getwd()!=wd){setwd(wd)}
env_ZIPCMD<-"C:/Rtools/bin/zip"                           #nalezy ustawic sciezke do pliku zip w pakiecie rtools
if(Sys.getenv("R_ZIPCMD")!=env_ZIPCMD){Sys.setenv(R_ZIPCMD = env_ZIPCMD)}               

#source - MUSI byæ wywo³ywany po ustawieniu WD

source("drukuj_czas.R")              #Wypisuje czas od pocz¹tku wykonywania skryptu + komentarz
source("sprawdz_PESEL.R")            #sprawdza sumê kontroln¹ dla numeru PESEL, je¿eli jest poprawna zwraca T, w ka¿dy innym wypadku F
source("Skrypt_usun_puste.R")        #usuwa puste rzêdy bazuj¹c na pustych polach kolumny "Filia"
source("Skrypt_ID.R")                #ID
source("Skrypt_produkt.R")           #Produkt
source("Skrypt_zwrotki_v2.R")        #Zwortki
source("Skrypt_kategorie_wiekowe.R") #Wiek, Kategorie wiekowe[Lic, sum], Kategorie wiekowe[sp, mba]
source("Skrypt_obywatelstwo.R")      #Cudzoziemiec
source("Skrypt_maturzysta.R")        #Maturzysta
source("Skrypt_status_rekrutacji.R") #Status rekrutacji
source("Skrypt_lokalizacje.R")       #Gmina, powiat, województwo,dystanse


#zmienne dla skrytpu

rokBazy<-"2016"

#START obliczeñ

start.time <- Sys.time()

#Wczytywanie danych

if(exists("dane")==F){
  dane<-read.csv2("C:/JP/TEB Akademia/Automatyzacja KK/Baza KK 18.07.2016/wynik.csv", header = T, sep=";", encoding="UTF-8")
  #dane<- read.xlsx("../KK/wynik_dodatek.xlsx", sheet = 1)
  }
drukuj_czas(start.time, "Za³adowano dane.")

if(exists("zwrotki1")==F){zwrotki1 <- read.xlsx("C:/JP/TEB Akademia/Automatyzacja KK/Bazy zwrotek/Zwrotki 2011-2015 - okrojone.xlsx", sheet = 1)}
drukuj_czas(start.time, "Za³adowano zwrotki 1.")

if(exists("zwrotki2")==F){zwrotki2 <- read.xlsx("C:/JP/TEB Akademia/Automatyzacja KK/Bazy zwrotek/zwrotki 2016.06.20.xlsx", sheet = 3)}
drukuj_czas(start.time, "Za³adowano zwrotki 2.")

lista_pola_dane_zwrotki<-c("Imiê", "Nazwisko", "Kod.zamieszkania", "Kod.korespondencji", "Email","Telefon.kom.", "id") 

lista_pola_zwrotki1<-c("kod1", "e-mail", "telefon")
lista_pola_zwrotki2<-c("KOD", "e-mail", "telefon_dobry")

dane<-generuj_ID(dane)
drukuj_czas(start.time, "Wygenerowano ID.")

dane<-usun_puste(dane)
drukuj_czas(start.time, "Usunieto puste pola.")

dane<-generuj_produkt(dane)
drukuj_czas(start.time, "Wygenerowano produkt.")

dane<-generuj_zwrotki(dane, zwrotki1, lista_pola_dane_zwrotki, lista_pola_zwrotki1)
drukuj_czas(start.time, "Wygenerowano zwrotki 1.")

dane<-generuj_zwrotki(dane, zwrotki2, lista_pola_dane_zwrotki, lista_pola_zwrotki2)
drukuj_czas(start.time, "Wygenerowano zwrotki 2.")

dane<-generuj_kategorie_wiekowe(dane, rokBazy)
drukuj_czas(start.time, "Wygenerowano kategorie wiekowe.")

dane<-generuj_obywatelstwo(dane)
drukuj_czas(start.time, "Wygenerowano obywatelstwa.")

dane<-generuj_maturzysta(dane)
drukuj_czas(start.time, "Wygenerowano maturzystów.")

dane<-generuj_status_rekrutacji(dane)
drukuj_czas(start.time, "Wygenerowano status rekrutacji.")

#dane<-generuj_lokalizacje(dane)
#drukuj_czas(start.time, "Wygenerowano lokalizacje.")

#zapis pliku .xlsx

write.xlsx(dane, paste0("wynik_R_", as.character(Sys.Date()), "_dodatek.xlsx"))

drukuj_czas(start.time, "Ukoñczono obliczenia.")

#ujednoliciæ nazwy pól generowanych przez skrypt w df
