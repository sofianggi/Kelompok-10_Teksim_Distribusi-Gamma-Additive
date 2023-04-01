#Kelompok 10
#Nama Anggota : 
##Sofi Anggi Astuti       (B2A020047)
##Fellya Naza Nurcahyani  (B2A020054)
##Novina Indah Fitriyah   (B2A020056)


#Distribusi Gamma Additive
Additive_RNG<-function(a,z0,c,m,n) {
  xi<-matrix(NA,n,4)
  colnames(xi)<-c("aZ(i-1)+c","Xi","Ui","Distribusi")
  for (i in 1:n)
  {
    xi[i,1]<-(a*z0+c)
    xi[i,2]<-xi[i,1]%%m
    J<-xi[i,3]<-xi[i,2]/m
    z0<-xi[i,2]
    lambda<-3
    alpha<-4
    U<-log (runif(J*alpha))
    Um<-matrix(U,n)
    Y<-apply(Um,1,sum)
    xi[,4]<--Y/lambda
  }
  View(xi)
}
Additive_RNG(45,21139,437,417,150)