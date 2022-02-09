#for Laplace(0,1) distribution:
T=c()
M=c()
V=c()
mu=0
n=5000
k=n-1
var=2/(n)
for(i in 1:10000){
  x=rlaplace(n)
  m=sum(x)/n
  z=((x-m)^2)/(n-1)
  s=sqrt(sum(z))
  v=sum(z)
  t=(sqrt(n)*(m-mu))/s
  T=c(T,t)
  M=c(M,m)
  V=c(V,v)
}
est_density<-function(x){
  (1/(sqrt(2*pi*var)))*exp(((-((x-mu)^2))/(2*var)))
}
est_density1<-function(x){
  (1/(sqrt(2*pi)))*exp(-(x^2)/2)
}

hist(M,col='yellow',probability = TRUE,main = "Histogram of Sample Mean")
plot(est_density,-0.1,0.1,col='darkblue',add=TRUE,lwd=3)
hist(T,col='yellow',probability = TRUE,main = "Histogram of Sample T")
plot(est_density1,-3,3,col='darkblue',add=TRUE,lwd=3)
hist(V,col='yellow',probability = TRUE,main = "Histogram of Sample Variance")
par(new=TRUE)
plot(density(V),axes=FALSE,frame.plot=FALSE,lwd=2,main = "")
library(DescTools)
z=Freq2D(M,V,n=20)
dim(z)
D=matrix(0,dim(z)[1],dim(z)[2])
for (i in 1:dim(z)[1]) {
  for (j in 1:dim(z)[2]) {
    D[i,j]=(z[i,j]/length(M))-(sum(z[i,]))*(sum(z[,j]))/(length(M)^2)
  }
}
max(abs(D))