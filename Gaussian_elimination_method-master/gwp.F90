!     Last change:  A     2 May 2013    0:45 am
PROGRAM Gaussian_elemination
IMPLICIT NONE
REal::a(100,100),temp(100,100),lar,p,x(100),sum1
INTEGER::i,j,k,l,h,n
OPEN(1,"inq.dat")
OPEN(2,"outq.dat")
READ(1,*)n
READ(1,*)((a(i,j),j=1,n+1),i=1,n)
WRITE(2,*)'The Augmented matrix is :'
Do i=1,n
WRITE(2,*)(a(i,j),j=1,n+1)
END DO
WRITE(2,*)'The Elenentry operatons are :'
DO k=1,n-1
h=0
l=0
DO i=k,n
IF(a(i,k).eq.0) l=l+1
h=h+1
END DO
IF (h==l) THEN
Go TO 10
Else
GOTO 9
END IF
!Pivitaization Start
9 lar=a(k,k)
l=0
DO j=k,n
IF(ABS(a(j,k))>lar) Then
lar=a(j,k)
l=j
END IF
END DO
IF (l==0) THEN
GO TO 15
ELSE
    GOTO 16
END IF
16 DO j=k,n+1
temp(k,j)=a(k,j)
END DO
DO j=k,n+1
a(k,j)=a(l,j)
a(l,j)=temp(k,j)
END DO
15 DO j=k+1,n
p=a(j,k)/a(k,k)
DO i=k,n+1
a(j,i)=a(j,i)-(a(k,i)*p)
END DO
END DO
WRITE(2,*)''
Do i=1,n
WRITE(2,*)(a(i,j),j=1,n+1)
END DO
WRITE(2,*)''
END do
IF (a(n,n).eq.0.0) GO TO 10
x(n)=a(n,n+1)/a(n,n)
DO i=n-1,1,-1
sum1=0.0
DO j=i+1,n
sum1=sum1+a(i,j)*x(j)
END DO
x(i)=(a(i,n+1)-sum1)/a(i,i)
END DO
WRITE(2,*)'The Solution of the System '
DO i=1,n
WRITE(2,7)i,x(i)
7 FORMAT(1X,'x(',I2,')= ',F8.3)
END DO
GO TO 11


10 WRITE(2,*)'No unique Solution exists'
11 END PROGRAM
