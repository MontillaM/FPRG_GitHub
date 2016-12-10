# MICHAELA MONTILLA
# Vysoke uceni technicke v Brne 
# FPRG - programovani v bioinformatice
# /////////////-/////////////-/////////////-/////////////-/////////////

# Priklad vstupu: VraceniMinci(198,c(100,50,20,10,5,2,1))

VraceniMinci<-function(M,c){
 if (is.numeric(M)& is.numeric(c)){
  d<-length(c)
  zbytek<-floor(M)
  mince<-c()
  for (i in 1:d){
    mince[i]<-c(floor(zbytek/c[i]))
    zbytek<-zbytek-mince[i]*c[i]
  }
  suma<-sum (mince*c)
  if (suma==floor(M)){
    cat("Vratime nasledujuci pocet kusov ", mince, " tychto minci ", c)
} else (print('chyba'))
 }else (print ('neni zadne cislo'))
}