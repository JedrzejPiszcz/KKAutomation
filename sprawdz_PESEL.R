sprawdz_PESEL<-function(PESEL){
  PESEL<-as.character(PESEL)
  if(missing(PESEL)){
    poprawnosc<-FALSE
  }else{
    if(is.na(as.numeric(PESEL) %% 1==0)){
       poprawnosc<-FALSE
    }else{
      if(nchar(as.character(PESEL))==11){
          checksum<-integer(10)
          checksum[1] <-(as.integer(substr(PESEL, 1, 1))   * 1) %% 10
          checksum[2] <-(as.integer(substr(PESEL, 2, 2))   * 3) %% 10
          checksum[3] <-(as.integer(substr(PESEL, 3, 3))   * 7) %% 10
          checksum[4] <-(as.integer(substr(PESEL, 4, 4))   * 9) %% 10
          checksum[5] <-(as.integer(substr(PESEL, 5, 5))   * 1) %% 10
          checksum[6] <-(as.integer(substr(PESEL, 6, 6))   * 3) %% 10
          checksum[7] <-(as.integer(substr(PESEL, 7, 7))   * 7) %% 10
          checksum[8] <-(as.integer(substr(PESEL, 8, 8))   * 9) %% 10
          checksum[9] <-(as.integer(substr(PESEL, 9, 9))   * 1) %% 10
          checksum[10]<-(as.integer(substr(PESEL, 10, 10)) * 3) %% 10
    
          if((10-(sum(checksum)%%10))==(as.integer(substr(PESEL, 11, 11)))){
             poprawnosc<-TRUE
          }else{
             poprawnosc<-FALSE
          }
      }else{
          poprawnosc<-FALSE
      }
    }
  }
  return(poprawnosc)
}