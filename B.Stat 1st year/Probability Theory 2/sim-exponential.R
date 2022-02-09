#for exponential(1) distribution:
T1=c()
M1=c()
V1=c()
n=20000
k=n-1
mu1=1
var1=1/n
for(i in 1:10000){
  x1=rexp(n)
  m1=sum(x1)/n
  z1=((x1-m1)^2)/k
  s1=sqrt(sum(z1))
  v1=sum(z1)
  t1=(sqrt(n)*(m1-1))/s1
  T1=c(T1,t1)
  M1=c(M1,m1)
  V1=c(V1,v1)
}
est_density1<-function(x){
  (1/(sqrt(2*pi)))*exp(-(x^2)/2)
}
est_density3<-function(x){
  (1/(sqrt(2*pi*var1)))*exp(((-((x-mu1)^2))/(2*var1)))
}

par(mfrow=c(1,3))
hist(M1,breaks=25,col='gray',probability = TRUE,main = "Histogram of Sample Mean")
plot(est_density3,0.97,1.03,col='darkblue',add=TRUE,lwd=3)
hist(T1,breaks=15,col='gray',probability = TRUE,main = "Histogram of Sample T")
plot(est_density1,-3,3,col='darkblue',add=TRUE,lwd=3)
hist(V1,breaks=15,col='gray',probability = TRUE,main = "Histogram of Sample Variance")
par(new=TRUE)
plot(density(V),axes=FALSE,frame.plot=FALSE,lwd=2,main=" ")

z1=Freq2D(M1,V1,n=20)
dim(z1)
D1=matrix(0,dim(z1)[1],dim(z1)[2])
for (i in 1:dim(z1)[1]) {
  for (j in 1:dim(z1)[2]) {
    D1[i,j]=(z1[i,j]/length(M1))-(sum(z1[i,]))*(sum(z1[,j]))/(length(M1)^2)
  }
}
max(abs(D1))
