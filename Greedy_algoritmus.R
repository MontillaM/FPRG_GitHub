# MICHAELA MONTILLA
# Vysoke uceni technicke v Brne 
# FPRG - programovani v bioinformatice
# /////////////-/////////////-/////////////-/////////////-/////////////

# Priklad vstupu: Gree(DNAStringSet(c('CATGC','CTAAGT','GCTA','TTCA','ATGCATC')))

Gree<- function(R) {
for (t in 1:(length(R)-1)){

if (length(R)==2){
              j<-1
              k<-2
              pom1<-R[[j]]
              pom2<-R[[k]]
              m0<-min(length(pom1), length(pom2))
              # z prava 
              m1<- length(pom1)
              n1<-1
              D1<-0
              for (i in 1:m0){
                if (pom1[m1:length(pom1)]==pom2[1:n1]) {
                  D1<-pom1[m1:length(pom1)]}
                m1<-m1-1
                n1<-n1+1
              }
              D1
              
              # zlava
              m2<-length(pom2)
              n2<-1
              D2<-0
              for (i in 1:m0){
                if (pom2[m2:length(pom2)]==pom1[1:n2]) {
                  D2<-pom2[m2:length(pom2)]}
                m2<-m2-1
                n2<-n2+1
              }
              D2
              
              if (identical(D1,0)){
                D<- D2
              } else if (identical(D2,0)){
                D<- D1
              } else if (length(D1)>length(D2)){
                D<- D1
              } else {
                D<- D2
              }
              
              if (identical(D,0)){
                max<- max
                max_ind<-max_ind
              } else {
                max<- D
                max_ind<-c(j,k)
              }         
              
              if (length(max)==1){
                u<- length(R[[2]])
                v<- length(R[[1]])
                if (identical(R[[1]][1],tail(R[[2]],1))){
                  kd<-c(DNAString(R[[2]][1:(u-1)]),DNAString(R[[1]]))
                } else {
                  kd<-c(DNAString(R[[1]]),DNAString(R[[2]][2:u]))
                }
              }else {
                kd1<-c(DNAString(R[[max_ind[1]]]),DNAString(R[[max_ind[2]]]))
                q1<-matchPattern(max,kd1)
                if (((end(q1)[1])+1)==start(q1)[2]){
                  kd<- kd1[-(start(q1)[1]:(end(q1)[1]))]
                } else {
                  kd1<-c(DNAString(R[[max_ind[2]]]),DNAString(R[[max_ind[1]]]))
                  q1<-matchPattern(max,kd1)
                  kd<- kd1[-(start(q1)[1]:(end(q1)[1]))]}
              }
              
              R<-kd
  
} else {
              
              l<-length(R)-1
              max<-0
              max_ind<-c(0,0)
              for (j in 1:l){
                for (k in (j+1):(l+1)) {
                  pom1<-R[[j]]
                  pom2<-R[[k]]
                        m0<-min(length(pom1), length(pom2))
                              # z prava 
                              m1<- length(pom1)
                              n1<-1
                              D1<-0
                              for (i in 1:m0){
                                if (pom1[m1:length(pom1)]==pom2[1:n1]) {
                                  D1<-pom1[m1:length(pom1)]}
                                m1<-m1-1
                                n1<-n1+1
                              }
                              D1
                              
                              # zlava
                              m2<-length(pom2)
                              n2<-1
                              D2<-0
                              for (i in 1:m0){
                                if (pom2[m2:length(pom2)]==pom1[1:n2]) {
                                D2<-pom2[m2:length(pom2)]}
                                m2<-m2-1
                                n2<-n2+1
                              }
                              D2
                        
                        if (identical(D1,0)){
                          D<- D2
                        } else if (identical(D2,0)){
                          D<- D1
                        } else if (length(D1)>length(D2)){
                          D<- D1
                        } else {
                          D<- D2
                        }
                              
                              if (identical(D,0)){
                                max<- max
                                max_ind<-max_ind
                              } else if (length(D)>length(max)){
                                max<- D
                                max_ind<-c(j,k)
                              } else {
                                max<- max
                                max_ind<-max_ind
                              }         
                }
              }
              kd1<-c(DNAString(R[[max_ind[1]]]),DNAString(R[[max_ind[2]]]))
              q1<-matchPattern(max,kd1)
              if (((end(q1)[1])+1)==start(q1)[2]){
                kd<- kd1[-(start(q1)[1]:(end(q1)[1]))]
              }else {
                kd1<-c(DNAString(R[[max_ind[2]]]),DNAString(R[[max_ind[1]]]))
                q1<-matchPattern(max,kd1)
                kd<- kd1[-(start(q1)[1]:(end(q1)[1]))]}
              
              
              R[[max_ind[1]]]<- kd
              R<-R[-max_ind[2]]
              

  }
}
  R
}


