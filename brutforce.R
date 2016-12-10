# MICHAELA MONTILLA
# Vysoke uceni technicke v Brne 
# FPRG - programovani v bioinformatice
# /////////////-/////////////-/////////////-/////////////-/////////////

# Priklad vstupu: brutforce(c(2,4),c(1,5),c(1,1,4))

brutforce <-function(f1,f2,r) {
  m1<-c(0,cumsum(f1))
  m2<-c(0,cumsum(f2))
  
s<-unique(sort(c(m1,m2)))

d<-c(0)
for (i in 1:(length(s)-1)){
  d<-c(d,c(s[i+1]-s[i]))
}
d<-sort(d[-1])

cat('Mapa pozicii Xa:',m1,'\n')
cat('Mapa pozicii Xb:',m2,'\n')
cat('Zlucene pozicie:',s,'\n')
cat('Diferencie:',d,'\n')

if (identical(d,r)){
  cat('Varianta usporiadania vektorov fragmentov je spravnym riesenim s reverznym riesenim: ','\n')
  pom1 <- m1[2:(length(m1)-1)]
  pom2 <-tail(m1,1)-pom1[length(pom1):1]
  cat(pom1,pom2,'\n')
  pom3 <- m2[2:(length(m2)-1)]
  pom4 <- tail(m2,1)-pom2[length(pom2):1]
  cat(pom3,pom4,'\n')
  } else {
  print ('Varianta usporiadania vektorov fragmentov nie je spravnym riesenim')
}


}