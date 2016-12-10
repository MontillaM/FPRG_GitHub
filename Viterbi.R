# MICHAELA MONTILLA
# Vysoke uceni technicke v Brne 
# FPRG - programovani v bioinformatice
# /////////////-/////////////-/////////////-/////////////-/////////////

# Priklad vstupu: Viterbi ( c('H','L'), c('A','C','G','T'), matrix(c(0.5, 0.5, 0.4, 0.6), nrow=2, ncol=2, byrow= TRUE), matrix(c(0.2, 0.3, 0.3, 0.2, 0.3, 0.2, 0.2, 0.3), nrow=2, ncol=4, byrow= TRUE), c(0.5, 0.5),c('G','G','C','A','C','T','G','A','A'))
# Priklad vstupu:Viterbi ( c('H','L'), c('A','C','G','T'), matrix(c(-1, -1, -1.322, -0.737), nrow=2, ncol=2, byrow= TRUE), matrix(c(-2.322,-1.737,-1.737,-2.322,-1.737,-2.322,-2.322,-1.737), nrow=2, ncol=4, byrow= TRUE), c(-1, -1), c('G','G','C','A','C','T','G','A','A'))

Viterbi <- function (N,M,A,B,P,sek) {
  N<- c('H','L')
  M<- c('A','C','G','T')
  A<- matrix(c(0.5, 0.5, 0.4, 0.6), nrow=2, ncol=2, byrow= TRUE)
  B<- matrix(c(0.2, 0.3, 0.3, 0.2, 0.3, 0.2, 0.2, 0.3), nrow=2, ncol=4, byrow= TRUE)
  P<- c(0.5, 0.5)
  sek<- c('G','G','C','A','C','T','G','A','A')
  
  if (sum(P)==1){
    f<-1
    }else {
      f<-2
  }
  
  K<- matrix(0,nrow=length(N),ncol=length(sek))
  S<- matrix(0,nrow=length(N),ncol=length(sek))
  
  # BEZ LOGARITMU
  if (f==1){
  
        for (i in 1:length(N)){
          sl<- which (M==sek[1])
          p <- B[i,sl]*P[i]
          K[i,1]<- p
        }  
        
        help<-0
        for (j in 2:length(sek)){
          sl<- which (M==sek[j])
          for (k in 1:length(N)){
              for (m in 1:length(N)){
              help[m]<-K[m,j-1]*A[m,k]
              }
            p <- B[k,sl]*max(help)
            S[k,j] <- which.max(help)
            K[k,j]<- p
          }
        }
  }
  
  # LOGARITMICKY
  
  if (f==2){
    
    for (i in 1:length(N)){
      sl<- which (M==sek[1])
      p <- B[i,sl]+P[i]
      K[i,1]<- p
    }  
    
    help<-0
    for (j in 2:length(sek)){
      sl<- which (M==sek[j])
      for (k in 1:length(N)){
        for (m in 1:length(N)){
          help[m]<-K[m,j-1]+A[m,k]
        }
        p <- B[k,sl]+max(help)
        S[k,j] <- which.max(help)
        K[k,j]<- p
      }
    }
  }
  
  
  # SPATNA CESTA
  
    start<- max(K[,length(sek)])
    index<- which.max(K[,length(sek)])
    Out<-matrix(0,ncol=length(sek), nrow=1)
    Out[length(sek)]<-N[index]
    for (n in (length(sek)):2){
      index<-S[index,n]
      Out[n-1]<-N[index]
    }
    
    Out
  }  