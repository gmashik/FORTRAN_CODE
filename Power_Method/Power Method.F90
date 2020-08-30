
Algoritm: Burden Page-578
__________________________________________________________________________________________________________________________
__________________________________________________________________________________________________________________________
PROGRAM Power_method
IMPLICIT NONE
INTEGER::i,j,k,n
REAL::a(100,100),x(100),y(100),de,tol,e(100),maxi
OPEN(1,"inq5.dat")
OPEN(2,"outq5.dat")
READ(1,*)n,k,tol
READ(1,*)((a(i,j),j=1,n),i=1,n)
READ(1,*)(x(i),i=1,n)
DO i=1,k
y=x
call matm(a,y,x,n)
de=maxi(x,n)
x=x/de
DO j=1,n
e(j)=ABS(x(j)-y(j))
END DO
IF(maxi(e,n).lt.tol) GO TO 10
END DO
WRITE (2,*)' Maximun Number OF iteration exceeded procedure was unsuccessful'
GO TO 11
10 WRITE(2,*)'The dominent Eigenvalue is :',de
WRITE(2,*)'The Dominent eigenvecors are :'
DO i=1,n
WRITE(2,3)x(i)
3 FORMAT(1X,f7.3)
END DO
11 END PROGRAM
FUNCTION maxi(x,n)
IMPLICIT NONE
INTEGER::i,n
REAL::x(100),maxi
maxi=x(1)
DO i=2,n
IF(x(i).gt.maxi) maxi=x(i)
END DO
END
SUBROUTINE matm(a,y,x,n)
IMPLICIT NONE
INTEGER::i,j,n
REAL::a(100,100),y(100),x(100)
DO i=1,n
x(i)=0.0
DO j=1,n
x(i)=x(i)+a(i,j)*y(j)
END DO
END DO
END SUBROUTINE


__________________________________________________________________________________________________________________________
INPUT:
__________________________________________________________________________________________________________________________
3 1000 .000001
1 2 0
-2 1 2
1 3 1
1 1 1 
________________________________________________________________________________________________________________________
OUTPUT:
_______________________________________________________________________________________________________________________
The dominent Eigenvalue is :    3.00000    
The Dominent eigenvecors are :
   0.500
   0.500
   1.000
________________________________________________________________________________________________________________________