# MICHAELA MONTILLA
# Vysoke uceni technicke v Brne 
# FPRG - programovani v bioinformatice
# /////////////-/////////////-/////////////-/////////////-/////////////

# Priklad vstupu: HanojskaVez(5,2,3)

HanojskaVez<-function(n,zKoliku,naKolik){
  if (n==1){
    cat ('presun disk z koliku',zKoliku,'na kolik',naKolik,'\n')
  } else {
    free<-6-zKoliku-naKolik
    HanojskaVez (n-1,zKoliku,free)
    cat ('presun disk z koliku',zKoliku,'na kolik',naKolik,'\n')
    HanojskaVez (n-1,free,naKolik)
    
  }
}