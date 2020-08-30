!     Last change:  A    28 May 2014    1:47 am
PROGRAM Runge_kutta__2nd_ODE
IMPLICIT NONE
INTEGER::i,n
REAL::a,b,h,x(100),y(100),y0,yp0,u(100),k1,k2,m1,m2,f
OPEN(1,"inr.dat")
OPEN(2,"or22.dat")
READ(1,*)a,b,h,y0,yp0
x(0)=a
y(0)=y0
u(0)=yp0
n=(b-a)/h
DO i=0,n
m1=u(i)
k1=f(x(i),y(i),u(i))
m2=u(i)+h*k1
k2=f(x(i)+h,y(i)+h*m1,u(i)+h*k1)
y(i+1)=y(i)+(h/2)*(m1+m2)
u(i+1)=u(i)+(h/2)*(k1+k2)
x(i+1)=x(i)+h
END DO
WRITE(2,*)" 2nd Order Initial value Problem By Runge-Kutta 2nd Order Technique  "
WRITE(2,*)""
WRITE(2,*)"   X(i)           Y(i)                  U(i)                                 "
WRITE(2,*)"======================================================================"
DO i=0,n+1
WRITE(2,3)x(i),y(i),u(i)
END DO
3 FORMAT(1x,F5.2,5X,F15.8,5x,F15.8)
END PROGRAM
function f(x,y,z)
IMPLICIT NONE
REAL::x,y,z,f
f=EXP(2*x)*SIN(x)+2*z-2*y
return
END
