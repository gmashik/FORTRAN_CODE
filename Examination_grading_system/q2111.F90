!     Last change:  A    16 Jul 2012    0:33 am
PROGRAM q211
IMPLICIT NONE
INTEGER::id(100),atc(100),n,i,j,k
REAL::totalcl,incourse(100),final(100),totalm(100),totalinc(100),a,b,d,e
CHARACTER(LEN=6):: p
OPEN(2,"inq211.dat")
OPEN(3,"outq211.dat")
OPEN(8,"inq2112.dat")
READ(2,*)n,totalcl
Do i=1,n
READ(8,*)id(i),atc(i),incourse(i),final(i)
END do
WRITE(3,*)' ID INCOURSE(25%+5%=30%) FINAL  TOTAL(100%)  GP  GL  CLASSES CLASSES% '
Do i=1,n
a=totalcl
j=atc(i)
CALL atm(a,j,b,k)
totalinc(i)=incourse(i)+k
totalm(i)=final(i)+totalinc(i)
d=totalm(i)
call gpgl(d,e,p)
WRITE(3,4)id(i),totalinc(i),final(i),totalm(i),e,p,atc(i),b
4 FORMAT(I4,F10.2,6X,F10.2,5x,F7.2,F7.2,1X,A3,I5,3x,F6.2)
END do
END PROGRAM
SUBROUTINE atm(a,j,b,k)
IMPLICIT NONE
INTEGER::j,k
REAL::a,b,temp
temp=(j/a)*100
b=CEILING(temp)
IF(b.le.59) k=0
IF(b.ge.60 .AND. b.le.74) k=1
IF(b.ge.75 .AND. b.le.79) k=2
IF(b.ge.80 .AND. b.le.84) k=3
IF(b.ge.85 .AND. b.le.89) k=4
IF(b.ge.90 .AND. b.le.100) k=5
END SUBROUTINE
SUBROUTINE gpgl(d,e,p)
IMPLICIT NONE
REAL::d,e
CHARACTER(LEN=6)::p

IF(d.le.39) Then
e=0.0
p='F'
ElseIF(d.ge.40 .AND. d.le.44)Then
e=2.0
p='D'
ElseIF(d.ge.45 .AND. d.le.49)Then
e=2.25
p='C'
ELSEIF(d.ge.50 .AND. d.le.54)Then
e=2.5
p='C+'
ElseIF(d.ge.55 .AND. d.le.59)Then
e=2.75
p='B-'
ElseIF(d.ge.60 .AND. d.le.64)Then
e=3.00
p='B'
ElseIF(d.ge.65 .AND. d.le.69)Then
e=3.25
p='B+'
ElseIF(d.ge.70 .AND. d.le.74)Then
e=3.5
p='A-'
ElseIF(d.ge.75 .AND. d.le.79)Then
e=3.75
p='A'
ElseIF(d.ge.80 .AND. d.le.100)Then
e=4.00
p='A+'
End IF
END SUBROUTINE

