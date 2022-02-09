T=c()
M=c()
V=c()
mu=-digamma(1)
n=10000
k=n-1
var=(pi^2)/(6*n)
for(i in 1:10000){
  x=rGumbel(n)
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

par(mfrow=c(1,3))
hist(M,col='orange',probability = TRUE,main = "Histogram of Sample Mean")
plot(est_density,0.4,0.65,col='darkblue',add=TRUE,lwd=3)
hist(T,col='orange',probability = TRUE,main = "Histogram of Sample T")
plot(est_density1,-4,4,col='darkblue',add=TRUE,lwd=3)
hist(V,col='orange',probability = TRUE,main = "Histogram of Sample Variance")
par(new=TRUE)
plot(density(V),axes=FALSE,frame.plot=FALSE,lwd=2,main = "")

z=Freq2D(M,V,n=20)
dim(z)
D=matrix(0,dim(z)[1],dim(z)[2])
for (i in 1:dim(z)[1]) {
  for (j in 1:dim(z)[2]) {
    D[i,j]=(z[i,j]/length(M))-(sum(z[i,]))*(sum(z[,j]))/(length(M)^2)
  }
}
max(abs(D))