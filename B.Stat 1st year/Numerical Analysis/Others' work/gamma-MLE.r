#Problem 3: Newton-Raphson method
#
#Computing the MLE for a Gamma distribution
#
X<-scan("D:/All Downloads/gamma.txt")
#
#(a) MLE using Newton-Raphson in '(l,a)'
#
ln<-function(p,X) {
  n<-length(X)
  l<-p[1,1]
  a<-p[2,1]
  ln<-n*(a*log(l)-lgamma(a))+(a-1)*sum(log(X))-l*sum(X)
  attr(ln,"gradient")<-n*matrix(c(a/l-mean(X),log(l)-digamma(a)+mean(log(X))),2,1)
  attr(ln,"hessian")<-n*matrix(c(-a/l^2,1/l,1/l,-trigamma(a)),2,2)
  ln
}
#Newton-Raphson method (as in lecture)
newmle<-function(p,ln,...) {
  l<-ln(p,...)
  pnew<-p-solve(attr(l,"hessian"))%*%attr(l,"gradient")
  # allow only steps of length eps=0.25
  eps=0.25
  if (sqrt(sum((pnew-p)^2))>eps)
    pnew<-p+eps*(pnew-p)/sqrt(sum((pnew-p)^2)) 
  pnew
}
#Apply Newton-Raphson iteration
#10 iterations
N<-10
r<-rep(0,2*N)
dim(r)<-c(N,2)
p<-c(l1,a1)
dim(p)<-c(2,1)
r[1,]<-t(p)
for (i in (2:N)) {
  p<-newmle(p,ln,X=X)
  r[i,]<-t(p)
}
r
#
#(b) MLE using Newton-Raphson only in variable 'a'
#
#Log-likelihood, 1st & 2nd derivative
ln<-function(a,X) {
  n<-length(X) 
  ln<-1 # likelihood is not needed
  attr(ln,"gradient")<-n*log(a)-n*log(mean(X))+sum(log(X))-n*digamma(a)
  attr(ln,"hessian")<-n/a-n*trigamma(a)
  ln
}
#Newton-Raphson method (as in lecture)
newmle<-function(p,ln,...) {
  l<-ln(p,...)
  pnew<-p-attr(l,"gradient")/attr(l,"hessian")
  pnew
}
#Apply Newton-Raphson iteration
#10 iterations
N=10
r<-rep(0,N)
a<-a1
r[1]<-a
for (i in (2:N)) {
  a<-newmle(a,ln,X=X)
  r[i]<-a
}
l<-a/mean(X)
r
#variance estimates:
n<-length(X)
vl<-(1/n)/(a*trigamma(a)-1)*l^2*trigamma(a)
va<-(1/n)/(a*trigamma(a)-1)*a
c(vl,va)
#