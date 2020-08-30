!     Last change:  A    30 Apr 2013    8:20 pm
PROGRAM Gaussian_elemination
IMPLICIT NONE
REal::a(100,100),temp(100,100),p,x(100),sum1
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
9 IF (a(k,k).eq.0.0) THEN !Pivitaization Start
l=0
DO j=k,n
IF(a(j,k).ne.0.0) Then
 l=j
 GO TO 100
 END IF
END DO
100 DO j=k,n+1
temp(k,j)=a(k,j)
END DO
DO j=k,n+1
a(k,j)=a(l,j)
a(l,j)=temp(k,j)
END DO
ELSE
    GO to 15
END if
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
WRITE(2,*)'The Solution of the System'
DO i=1,n
WRITE(2,7)i,x(i)
7 FORMAT(1X,'x(',I2,')= ',F8.3)
END DO
GO TO 11
10 WRITE(2,*)'No unique Solution exists'
11 END PROGRAM
