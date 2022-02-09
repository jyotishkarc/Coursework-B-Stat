library("plot3D")
library("animation")

gravitywell.2 = function(h,n) #step-length and no. of iterations 
{
   x=rep(0,n)
   y=rep(0,n)
   u=rep(0,n)
   z=rep(0,n)
   f=rep(0,n)
   x1=rep(0,n)
   y1=rep(0,n)
   u1=rep(0,n)
   f1=rep(0,n)
   f2=rep(0,n)
   Rtil=rep(0,n)
   #initial values are as follows:
   t=10 #independent parameter t
   x[1]=t 
   y[1]=0
   x1[1]=0
   y1[1]=5
   for (i in 2:n)
   {
      u[i-1]=sqrt(x[i-1]^2+y[i-1]^2) #u
      f[i-1]=sqrt(u[i-1]-1) #f(u)
      z[i-1]=f[i-1] #z-coordinate
      u1[i-1]=(x[i-1]*x1[i-1]+y[i-1]*y1[i-1])/sqrt(x[i-1]^2+y[i-1]^2) #u'
      f1[i-1]=1/(2*sqrt(u[i-1]-1)) #f'(u)
      f2[i-1]=-1/(4*(u[i-1]-1)^1.5) #f"(u)
      Rtil[i-1]=((f1[i-1]*(x1[i-1]^2+y1[i-1]^2-u1[i-1]^2)/u[i-1])+u1[i-1]^2*f2[i-1]+9.8)/(u[i-1]*(f1[i-1]+(1/f1[i-1]))) #R_tilda
      x[i]=x[i-1]+h*x1[i-1]+((h^2)/2)*(-x[i-1]*Rtil[i-1]) #x-coordinate using Taylor's Method
      y[i]=y[i-1]+h*y1[i-1]+((h^2)/2)*(-y[i-1]*Rtil[i-1]) #y-coordinate using Taylor's Method
      x1[i]=x1[i-1]+h*(-x[i-1]*Rtil[i-1]) #x-component of velocity using Euler's Method
      y1[i]=y1[i-1]+h*(-y[i-1]*Rtil[i-1]) #y-component of velocity using Euler's Method
   }
   
   u[n]=sqrt(x[n]^2+y[n]^2)
   f[n]=sqrt(u[n]-1)
   z[n]=f[n]
   
   lines3D(x,y,z,type="l")

}
