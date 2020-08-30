!     Last change:  A    28 May 2014    1:42 am
PROGRAM Runge_kutta__2nd_ODE
IMPLICIT NONE
INTEGER::i,n
REAL::a,b,h,x(100),y(100),y0,yp0,u(100),k1,k2,k3,k4,m1,m2,m3,m4,f
OPEN(1,"inr.dat")
OPEN(2,"or.dat")
READ(1,*)a,b,h,y0,yp0
x(0)=a
y(0)=y0
u(0)=yp0
n=(b-a)/h
DO i=0,n
m1=u(i)
k1=f(x(i),y(i),u(i))
m2=u(i)+.5*h*k1
k2=f(x(i)+h/2,y(i)+.5*h*m1,u(i)+.5*h*k1)
m3=u(i)+.5*h*k2
k3=f(x(i)+.5*h,y(i)+.5*h*m2,u(i)+.5*h*k2)
m4=u(i)+h*k3
k4=f(x(i)+h,y(i)+h*m3,u(i)+h*k3)
y(i+1)=y(i)+(h/6)*(m1+2*m2+2*m3+m4)
u(i+1)=u(i)+(h/6)*(k1+2*k2+2*k3+k4)
x(i+1)=x(i)+h
END DO
WRITE(2,*)" 2nd Order Initial value Problem By Runge-Kutta 4 th Order Technique  "
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
end
