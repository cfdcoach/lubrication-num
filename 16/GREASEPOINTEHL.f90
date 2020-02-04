	PROGRAM GREASEPOINTEHL
	DIMENSION THETA(15),EALFA(15),EBETA(15)
	COMMON /COM1/Z,ENDA,AKC,HM0,HMC,EK,EAL,EBE,AD,AD1,KK1,KK2,KK3,KK4,FN,FN1,FF
	COMMON /COM2/W0,E1,RX,B,PH,US,U1,U2,EDA0
	COMMON /COM3/A1,A2,A3,LMIN
	DATA PAI,Z,AKC,AD,AD1/3.14159265,0.68,1.0,0.0,0.0/
    DATA T0,EDA0,AK,AK1,AK2,CV,CV1,CV2,RO0,RO1,RO2,S0,D0/303.,0.058,0.14,46.,46.,2000.,470.,470.,890.,7850.,7850.,-1.1,-0.00065/
	DATA N,NZ,RX,RY,X0,XE,E1,US,CT,W0/65,5,0.05,0.05,-2.5,1.5,2.21E11,1.5,0.31,39.24/
	DATA THETA/10.,20.,30.,35.,40.,45.,50.,55.,60.,65.,70.,75.,80.,85.,90./
	DATA EALFA/6.612,3.778,2.731,2.397,2.136,1.926,1.754,1.611,1.486,1.378,1.284,1.202,1.128,1.061,1.0/
	DATA EBETA/0.319,0.408,0.493,0.53,0.567,0.604,0.641,0.678,0.717,0.759,0.802,0.846,0.893,0.944,1.0/
	DATA KK1,KK2,KK3,KK4/0,0,0,0/
	WRITE(*,*)'n<=1 INPUT n=?'
	READ(*,*)FN
	FN1=1.0/FN
	FF=1.0/FN-1.0
	WRITE(*,*)"FF=",FF
	EK=RX/RY
	AA=0.5*(1./RX+1./RY)
	BB=0.5*ABS(1./RX-1./RY)
	CC=ACOS(BB/AA)*180.0/PAI
	EAL=1.0
	EBE=1.0
	DO I=1,15
	IF(CC.LT.THETA(I))THEN
	WRITE(*,*)I
	EAL=EALFA(I-1)+(CC-THETA(I))*(EALFA(I)-EALFA(I-1))/(THETA(I)-THETA(I-1))
	EBE=EBETA(I-1)+(CC-THETA(I))*(EBETA(I)-EBETA(I-1))/(THETA(I)-THETA(I-1))	
	GOTO 10
	ENDIF
	ENDDO
10	EA=EAL*(1.5*W0/AA/E1)**(1./3.0)
	EB=EBE*(1.5*W0/AA/E1)**(1./3.0)
	PH=1.5*W0/(EA*EB*PAI)
	OPEN(8,FILE='FILM.DAT',STATUS='UNKNOWN')
	OPEN(9,FILE='PRESS.DAT',STATUS='UNKNOWN')
	OPEN(10,FILE='OUT.DAT',STATUS='UNKNOWN')
	WRITE(*,*)"N,X0,XE,PH,E1,EDA0,RX,US"
	WRITE(*,*)N,X0,XE,PH,E1,EDA0,RX,US
	WRITE(16,*)"N,X0,XE,PH,E1,EDA0,RX,US"
	WRITE(16,*)N,X0,XE,PH,E1,EDA0,RX,US
	H00=0.0
	MM=N-1
	LMIN=ALOG(N-1.)/ALOG(2.)-1.99
	U=EDA0*(US/2.)**FN/(E1*RX**FN)
	WRITE(*,*)"U=",U
	U1=0.5*(2.+AKC)*U
	U2=0.5*(2.-AKC)*U
	A1=ALOG(EDA0)+9.67
	A2=5.1E-9*PH
	A3=0.59/(PH*1.E-9)
	B=PAI*PH*RX/E1
	W=2.*PAI*PH/(3.*E1)*(B/RX)**2
	ALFA=Z*5.1E-9*A1
	G=ALFA*E1
	AHM=1.0-EXP(-0.68*1.03)
	AHC=1.0-0.61*EXP(-0.73*1.03)
	HM0=3.63*(RX/B)**2*G**0.49*U**0.68*W**(-0.073)*AHM
	HMC=2.69*(RX/B)**2*G**0.53*U**0.67*W**(-0.067)*AHC
	ENDA=2.*U*(3.+FF)*2.0**(1.0+FF)*(E1/PH)**(1.0+FF)*(RX/B)**(3.0+FF)
	WRITE(*,*)"ENDA=",ENDA
	UTL=EDA0*US*RX/(B*B*2.E7)
	W0=2.0*PAI*EA*EB*PH/3.0
	T1=PH*B/RX
	T2=EDA0*US*RX/(B*B)
	WRITE(*,*)'               Wait please'
	CALL SUBAK(MM)
	CALL MULTI(N,NZ,X0,XE,H00)
	STOP
	END	
	SUBROUTINE MULTI(N,NZ,X0,XE,H00)
	DIMENSION X(65),Y(65),H(4500),RO(4500),EPS(4500),EDA(4500),P(4500),POLD(4500),T(65,65,5)
	COMMON /COM1/Z,ENDA,AKC,HM0,HMC,EK,EAL,EBE,AD,AD1,KK1,KK2,KK3,KK4,FN,FN1,FF
	DATA MK,KTK,G00/200,1,2.0943951/
	G0=G00*EAL*EBE
	NX=N
	NY=N
	NN=(N+1)/2
	CALL INITI(N,DX,X0,XE,X,Y,P,POLD)
	CALL HREE(N,DX,H00,G0,X,Y,H,RO,EPS,EDA,P)
	M=0
14	KK=15
	CALL ITER(N,KK,DX,H00,G0,X,Y,H,RO,EPS,EDA,P)
	CALL ERP(N,ER,P,POLD)
	ER=ER/KK
	WRITE(*,*)'ER=',ER
    M=M+1
	IF(M.LT.MK.AND.ER.GT.1.E-4)GOTO 14
	CALL OUPT(N,DX,X,Y,H,P,EDA,TMAX)	
	RETURN
	END
	SUBROUTINE INITI(N,DX,X0,XE,X,Y,P,POLD)
	DIMENSION X(N),Y(N),P(N,N),POLD(N,N)
	NN=(N+1)/2
	DX=(XE-X0)/(N-1.)
	Y0=-0.5*(XE-X0)
	DO 5 I=1,N
	X(I)=X0+(I-1)*DX
	Y(I)=Y0+(I-1)*DX
5	CONTINUE
	DO 10 I=1,N
	D=1.-X(I)*X(I)
	DO 10 J=1,NN
	C=D-Y(J)*Y(J)
	IF(C.LE.0.0)P(I,J)=0.0
10	IF(C.GT.0.0)P(I,J)=SQRT(C)
	DO 20 I=1,N
	DO 20 J=NN+1,N
	JJ=N-J+1
20	P(I,J)=P(I,JJ)
	DO I=1,N
	DO J=1,N
	POLD(I,J)=P(I,J)
	ENDDO
	ENDDO
	RETURN
	END
	SUBROUTINE HREE(N,DX,H00,G0,X,Y,H,RO,EPS,EDA,P)
	DIMENSION X(N),Y(N),P(N,N),H(N,N),RO(N,N),EPS(N,N),EDA(N,N)
	DIMENSION W(150,150),P0(150,150),ROU(65,65)
	COMMON /COM1/Z,ENDA,AKC,HM0,HMC,EK,EAL,EBE,AD,AD1,KK1,KK2,KK3,KK4,FN,FN1,FF
	COMMON /COM2/W0,E1,RX,B,PH,US,U1,U2,EDA0
	COMMON /COM3/A1,A2,A3,LMIN	
	DATA KR,NW,PAI,PAI1,DELTA/0,150,3.14159265,0.2026423,0.0/
	NN=(N+1)/2
	CALL VI(NW,N,DX,P,W)
	HMIN=1.E3
	DO 30 I=1,N
	DO 30 J=1,NN
	RAD=X(I)*X(I)+EK*Y(J)*Y(J)
	W1=0.5*RAD+DELTA
	ZZ=0.5*AD1*AD1+X(I)*ATAN(AD*PAI/180.0)
	IF(W1.LE.ZZ)W1=ZZ
	H0=W1+W(I,J)
	IF(H0.LT.HMIN)HMIN=H0
30	H(I,J)=H0
	IF(KK.EQ.0)THEN
	KG1=0
	H01=-HMIN+HM0
	DH=0.005*HM0
	H02=-HMIN
	H00=0.5*(H01+H02)
	ENDIF
	W1=0.0
	DO 32 I=1,N
	DO 32 J=1,N
32	W1=W1+P(I,J)
	W1=DX*DX*W1/G0
	DW=1.-W1
	IF(KK.EQ.0)THEN
	KK=1
	GOTO 50
	ENDIF
	IF(DW.LT.0.0)THEN
	KG1=1
	H00=AMIN1(H01,H00+DH)
	ENDIF
	IF(DW.GT.0.0)THEN
	KG2=2
	H00=AMAX1(H02,H00-DH)
	ENDIF
50	DO 60 I=1,N
	DO 60 J=1,NN
	H(I,J)=H00+H(I,J)
	IF(P(I,J).LT.0.0)P(I,J)=0.0
	EDA1=EXP(A1*(-1.+(1.+A2*P(I,J))**Z))
	EDA(I,J)=EDA1
	RO(I,J)=1.
	EPS(I,J)=ENDA*RO(I,J)*H(I,J)**(2.+FN1)/(EDA(I,J)**FN1)
60	CONTINUE  
	DO 70 J=NN+1,N
	JJ=N-J+1
	DO 70 I=1,N
	H(I,J)=H(I,JJ)
	RO(I,J)=RO(I,JJ)
	EDA(I,J)=EDA(I,JJ)
70	EPS(I,J)=EPS(I,JJ)
	RETURN
	END
    SUBROUTINE ITER(N,KK,DX,H00,G0,X,Y,H,RO,EPS,EDA,P)
	DIMENSION X(N),Y(N),P(N,N),H(N,N),RO(N,N),EPS(N,N),EDA(N,N)
	DIMENSION D(70),A(350),B(210),ID(70)
	COMMON /COM1/Z,ENDA,AKC,HM0,HMC,EK,EAL,EBE,AD,AD1,KK1,KK2,KK3,KK4,FN,FN1,FF
	COMMON /COMAK/AK(0:65,0:65)
	DATA KG1,PAI1,C1,C2/0,0.2026423,0.27,0.27/
	IF(KG1.NE.0)GOTO 2
	KG1=1
	AK00=AK(0,0)
	AK10=AK(1,0)
	AK20=AK(2,0)
	BK00=AK00-AK10
	BK10=AK10-0.25*(AK00+2.*AK(1,1)+AK(2,0))
	BK20=AK20-0.25*(AK10+2.*AK(2,1)+AK(3,0))
2	NN=(N+1)/2
	MM=N-1
	DX1=1./DX
	DX2=DX*DX
	DX3=1./DX2
	DO 100 K=1,KK
	PMAX=0.0
	DO 70 J=2,NN
	J0=J-1
	J1=J+1
	IA=1
8	MM=N-IA
	IF(P(MM,J0).GT.1.E-6)GOTO 20
	IF(P(MM,J).GT.1.E-6)GOTO 20
	IF(P(MM,J1).GT.1.E-6)GOTO 20
	IA=IA+1
	IF(IA.LT.N)GOTO 8
	GOTO 70
20	IF(MM.LT.N-1)MM=MM+1 
	DPDX1=ABS((P(2,J)-P(1,J))*DX1)**(FF)
	D2=0.5*(EPS(1,J)+EPS(2,J))*DPDX1
	DO 50 I=2,MM
	I0=I-1
	I1=I+1
	II=5*I0
	DPDX2=ABS((P(I1,J)-P(I,J))*DX1)**(FF)
	DPDY1=ABS((P(I,J)-P(I,J0))*DX1)**(FF)
	DPDY2=ABS((P(I,J1)-P(I,J))*DX1)**(FF)
	D1=D2
	D2=0.5*(EPS(I1,J)+EPS(I,J))*DPDX2
	D4=0.5*(EPS(I,J0)+EPS(I,J))*DPDY1
	D5=0.5*(EPS(I,J1)+EPS(I,J))*DPDY2
	P1=P(I0,J)
	P2=P(I1,J)
	P3=P(I,J)
	P4=P(I,J0)
	P5=P(I,J1)
	D3=D1+D2+D4+D5
	IF(H(I,J).LE.0.0)THEN
	ID(I)=0
	A(II+1)=0.0
	A(II+2)=0.0
	A(II+3)=1.0
	A(II+4)=0.0
	A(II+5)=1.0
	A(II-4)=0.0
	GOTO 50
	ENDIF
	ID(I)=1
	IF(J.EQ.NN)P5=P4
	A(II+1)=PAI1*(RO(I0,J)*AK10-RO(I,J)*AK20)
	A(II+2)=DX3*D1+PAI1*(RO(I0,J)*AK00-RO(I,J)*AK10)
	A(II+3)=-DX3*D3+PAI1*(RO(I0,J)*AK10-RO(I,J)*AK00)
	A(II+4)=DX3*D2+PAI1*(RO(I0,J)*AK20-RO(I,J)*AK10)
	A(II+5)=-DX3*(D1*P1+D2*P2+D4*P4+D5*P5-D3*P3)+DX1*(RO(I,J)*H(I,J)-RO(I0,J)*H(I0,J))
50	CONTINUE
	CALL TRA4(MM,D,A,B)
	DO 60 I=2,MM
	IF(ID(I).EQ.1)P(I,J)=P(I,J)+C1*D(I)
	IF(P(I,J).LT.0.0)P(I,J)=0.0
	IF(PMAX.LT.P(I,J))PMAX=P(I,J)
60	CONTINUE
70	CONTINUE
	DO 80 J=1,NN
	JJ=N+1-J
	DO 80 I=1,N
80	P(I,JJ)=P(I,J)
	CALL HREE(N,DX,H00,G0,X,Y,H,RO,EPS,EDA,P)
100	CONTINUE
	RETURN
	END
	SUBROUTINE TRA4(N,D,A,B)
	DIMENSION D(N),A(5,N),B(3,N)
	C=1./A(3,N)
	B(1,N)=-A(1,N)*C
	B(2,N)=-A(2,N)*C
	B(3,N)=A(5,N)*C
	DO 10 I=1,N-2
	IN=N-I
	IN1=IN+1
	C=1./(A(3,IN)+A(4,IN)*B(2,IN1))
	B(1,IN)=-A(1,IN)*C
	B(2,IN)=-(A(2,IN)+A(4,IN)*B(1,IN1))*C
10	B(3,IN)=(A(5,IN)-A(4,IN)*B(3,IN1))*C
	D(1)=0.0
	D(2)=B(3,2)
	DO 20 I=3,N
20	D(I)=B(1,I)*D(I-2)+B(2,I)*D(I-1)+B(3,I)
	RETURN
	END
	SUBROUTINE VI(NW,N,DX,P,V)
	DIMENSION P(N,N),V(NW,NW)
	COMMON /COMAK/AK(0:65,0:65)
	PAI1=0.2026423
	DO 40 I=1,N
	DO 40 J=1,N
	H0=0.0
	DO 30 K=1,N
	IK=IABS(I-K)
	DO 30 L=1,N
	JL=IABS(J-L)
30	H0=H0+AK(IK,JL)*P(K,L)
40	V(I,J)=H0*DX*PAI1
	RETURN
	END
	SUBROUTINE SUBAK(MM)
	COMMON /COMAK/AK(0:65,0:65)
	S(X,Y)=X+SQRT(X**2+Y**2)
	DO 10 I=0,MM
	XP=I+0.5
	XM=I-0.5
	DO 10 J=0,I
	YP=J+0.5
	YM=J-0.5
	A1=S(YP,XP)/S(YM,XP)
	A2=S(XM,YM)/S(XP,YM)
	A3=S(YM,XM)/S(YP,XM)
	A4=S(XP,YP)/S(XM,YP)
	AK(I,J)=XP*ALOG(A1)+YM*ALOG(A2)+XM*ALOG(A3)+YP*ALOG(A4)
10	AK(J,I)=AK(I,J)
	RETURN
	END
	SUBROUTINE ERP(N,ER,P,POLD)
	DIMENSION P(N,N),POLD(N,N)
	ER=0.0
	SUM=0.0
	NN=(N+1)/2
	DO 10 I=1,N
	DO 10 J=1,NN
	ER=ER+ABS(P(I,J)-POLD(I,J))
	SUM=SUM+P(I,J)
10	CONTINUE
	ER=ER/SUM
	DO I=1,N
	DO J=1,N
	POLD(I,J)=P(I,J)
	ENDDO
	ENDDO
	RETURN
	END
	SUBROUTINE OUPT(N,DX,X,Y,H,P,EDA,TMAX)
	DIMENSION X(N),Y(N),H(N,N),P(N,N),EDA(N,N)
	COMMON /COM1/Z,ENDA,AKC,HM0,HMC,EK,EAL,EBE,AD,AD1,KK1,KK2,KK3,KK4,FN,FN1,FF
	COMMON /COM2/W0,E1,RX,B,PH,US,U1,U2,EDA0
	A=0.0
	WRITE(8,40)A,(Y(I),I=1,N)
	DO I=1,N
	WRITE(8,40)X(I),(H(I,J),J=1,N)
	ENDDO
	WRITE(9,40)A,(Y(I),I=1,N)
	DO I=1,N
	WRITE(9,40)X(I),(P(I,J),J=1,N)
	ENDDO
40	FORMAT(66(E12.6,1X))   
	HMIN=1.E3
	PMAX=0.0
	DO J=1,N
	DO I=2,N
	IF(H(I,J).LT.HMIN)HMIN=H(I,J)
	IF(P(I,J).GT.PMAX)PMAX=P(I,J)
	ENDDO
	ENDDO
	HMIN=HMIN*B*B/RX
	PMAX=PMAX*PH
	WRITE(10,*)'HMIN,PMAX,TMAX',HMIN,PMAX,TMAX
	RETURN
	END