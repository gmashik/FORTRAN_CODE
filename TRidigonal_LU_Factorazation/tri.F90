!     Last change:  A     6 May 2013    0:05 am
PROGRAM TRidigonal_LU_Fact
IMPLICIT NONE
INTEGER::i,j,n
REAL::a(100,100),l(100,100),u(100,100),x(100),z(100)
OPEN(1,"inq.dat")
OPEN(2,"outq.dat")
READ(1,*)n
READ(1,*)((a(i,j),j=1,n+1),i=1,n)
l(1,1)=a(1,1)
u(1,2)=a(1,2)/l(1,1)
z(1)=a(1,n+1)/l(1,1)
DO i=1,n
DO j=1,n
IF (i.ne.j) CYCLE
u(i,j)=1.0
END DO
END DO
DO i=2,n-1
l(i,i-1)=a(i,i-1)
l(i,i)=a(i,i)-(l(i,i-1)*u(i-1,i))
u(i,i+1)=a(i,i+1)/l(i,i)
z(i)=(a(i,n+1)-(l(i,i-1)*z(i-1)))/l(i,i)
END DO
l(n,n-1)=a(n,n-1)
l(n,n)=a(n,n)-(l(n,n-1)*u(n-1,n))
z(n)=(a(n,n+1)-l(n,n-1)*z(n-1))/l(n,n)
x(n)=z(n)
DO i=n-1,1,-1
x(i)=z(i)-(u(i,i+1)*x(i+1))
END DO
WRITE(2,*)'The Upper Triangular Matrix is :'
DO i=1,n
WRITE(2,*)(u(i,j),j=1,n)
END DO
WRITE(2,*)'The Lower Triangular Matrix is :'
DO i=1,n
WRITE(2,*)(l(i,j),j=1,n)
END DO
WRITE(2,*)' Hence the solution of the system : '
DO i=1,n
WRITE(2,7)i,x(i)
END DO
7 FORMAT(1X,'x(',I2,')= ',F7.3)
END PROGRAM
