	PROGRAM POINTEHLT
	DIMENSION THETA(15),EALFA(15),EBETA(15)
	COMMON /COM1/ENDA,A1,A2,A3,Z,HM0/COM3/E1,PH1,B1,U1,U2,RE,CT/COMK/LMIN,AKC
	COMMON /COM2/T0,AK0,AK1,AK2,CV,CV1,CV2,RO0,RO1,RO2,S0,D0
	COMMON /COMW/W0,T1,T2,RX,B,PH/COMC/KT,NF/COMD/AD,AD1,KK1,KK2,KK3,KK4/COME/US,EDA0/COMH/HMC
	COMMON /COMEK/EK,EAL,EBE
	DATA PAI,Z/3.14159265,0.68/,N,W0,E1,EDA0,RX,RY,US,X0,XE/65,39.24,2.21E11,0.03,0.01,0.03,1.5,-2.5,1.5/
	DATA NZ,CT,AKC/5,0.31,1.0/
	DATA THETA/10.,20.,30.,35.,40.,45.,50.,55.,60.,65.,70.,75.,80.,85.,90./
	DATA EALFA/6.612,3.778,2.731,2.397,2.136,1.926,1.754,1.611,1.486,1.378,1.284,1.202,1.128,1.061,1.0/
	DATA EBETA/0.319,0.408,0.493,0.53,0.567,0.604,0.641,0.678,0.717,0.759,0.802,0.846,0.893,0.944,1.0/
	DATA KK1,KK2,KK3,KK4,NF,AD,AD1,EAL,EBE/0,0,0,0,0,0.0,0.0,1.0,1.0/
	EK=RX/RY
	WRITE(*,*)'KT='
	READ(*,*)KT
	AA=0.5*(1./RX+1./RY)
	BB=0.5*ABS(1./RX-1./RY)
	CC=ACOS(BB/AA)*180.0/PAI
	DO I=1,15
	IF(CC.LT.THETA(I))THEN
	WRITE(*,*)I
	EAL=EALFA(I-1)+(CC-THETA(I))*(EALFA(I)-EALFA(I-1))/(THETA(I)-THETA(I-1))
	EBE=EBETA(I-1)+(CC-THETA(I))*(EBETA(I)-EBETA(I-1))/(THETA(I)-THETA(I-1))	
	GOTO 1
	ENDIF
	ENDDO
1	EA=EAL*(1.5*W0/AA/E1)**(1./3.0)
	EB=EBE*(1.5*W0/AA/E1)**(1./3.0)
	PH=1.5*W0/(EA*EB*PAI)
	OPEN(8,FILE='FILM.DAT',STATUS='UNKNOWN')
	OPEN(9,FILE='PRESS.DAT',STATUS='UNKNOWN')
	OPEN(10,FILE='OUT.DAT',STATUS='UNKNOWN')
	WRITE(*,*)N,X0,XE,PH,E1,EDA0,RX,US
	H00=0.0
	MM=N-1
	LMIN=ALOG(N-1.)/ALOG(2.)-1.99
	U=EDA0*US/(2.*E1*RX)
	U1=0.5*(2.+AKC)*U
	U2=0.5*(2.-AKC)*U
	A1=ALOG(EDA0)+9.67
	A2=5.1E-9*PH
	A3=0.59/(PH*1.E-9)
	B=PAI*PH*RX/E1
	PH1=PH
	B1=B
	RE=RX
	W=2.*PAI*PH/(3.*E1)*(B/RX)**2
	ALFA=Z*5.1E-9*A1
	G=ALFA*E1
	AHM=1.0-EXP(-0.68*1.03)
	AHC=1.0-0.61*EXP(-0.73*1.03)
	HM0=3.63*(RX/B)**2*G**0.49*U**0.68*W**(-0.073)*AHM
	HMC=2.69*(RX/B)**2*G**0.53*U**0.67*W**(-0.067)*AHC
	ENDA=12.*U*(E1/PH)*(RX/B)**3
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
	COMMON /COMT/T1(65,65)/COMC/KT,NF
	COMMON /COMEK/EK,EAL,EBE
	DATA MK,KTK,G00/200,1,2.0943951/
	G0=G00*EAL*EBE
	NX=N
	NY=N
	NN=(N+1)/2
	DO I=1,N
	DO J=1,N
	T1(I,J)=1.0
	DO K=1,5
	T(I,J,K)=1.0
	ENDDO
	ENDDO
	ENDDO
	CALL INITI(N,DX,X0,XE,X,Y,P,POLD)
	CALL HREE(N,DX,H00,G0,X,Y,H,RO,EPS,EDA,P)
	M=0
	KTK=0
14	KK=15
15	CALL ITER(N,KK,DX,H00,G0,X,Y,H,RO,EPS,EDA,P)
	M=M+1
	CALL ERP(N,ER,P,POLD)
	ER=ER/KK
	WRITE(*,*)'ER=',ER
	IF(KT.NE.0)GOTO 17
	IF(M.LT.MK.AND.ER.GT.1.E-7)GOTO 14
	GOTO 120
17	KT1=0
18	CALL THERM(NX,NY,NZ,DX,P,H,T)
	CALL ERROM(NX,NY,NZ,T,ERM)
	IF(ER.LT.1.0E-5)GOTO 120
	IF(KT1.LT.1)THEN
	KT1=KT1+1
	GOTO 18
	ENDIF
	IF(KTK.LT.MK)THEN
	KTK=KTK+1
	GOTO 14
	ENDIF
120	CONTINUE
	OPEN(11,FILE='TEM.DAT',STATUS='UNKNOWN')
	WRITE(11,110)X0,(Y(I),I=1,N)
	TMAX=0.0
	DO I=1,N
	WRITE(11,110)X(I),(273.0*(T1(I,JJ)-1.),JJ=1,N)
	DO J=1,N
	IF(TMAX.LT.273.0*(T1(I,J)-1.))TMAX=273.*(T1(I,J)-1.)
	ENDDO
	ENDDO
110	FORMAT(66(E12.6,1X))
130	CALL OUPT(N,DX,X,Y,H,P,EDA,TMAX)	
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
	COMMON /COM1/ENDA,A1,A2,A3,Z,HM0/COMAK/AK(0:65,0:65)
	COMMON /COM2/T0,EAK,EAK1,EAK2,CV,CV1,CV2,RO0,RO1,RO2,S0,D0
	COMMON /COMT/T1(65,65)/COMK/LMIN,AKC/COMD/AD,AD1,KK,KK2,KK3,KK4/COMC/KT,NF
	COMMON /COMEK/EK,EAL,EBE
	DATA KR,NW,pai,PAI1,delta/0,150,3.14159265,0.2026423,0.0/
	NN=(N+1)/2
	CALL VI(NW,N,DX,P,W)
	HMIN=1.E3
	IF(KR.EQ.0)THEN
	OPEN(12,FILE='ROUGH2.DAT',STATUS='UNKNOWN')
	DO I=1,N
	DO J=NN+1,N
	ROU(I,J)=ROU(I,N+1-J)
	ENDDO
100	FORMAT(33(1X,F10.6))
	ENDDO
	CLOSE(12)
	KR=1
	ENDIF
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
	CT1=((T1(I,J)-0.455445545)/0.544554455)**S0
	CT2=D0*T0*(T1(I,J)-1.)
	IF(P(I,J).LT.0.0)P(I,J)=0.0
	EDA1=EXP(A1*(-1.+(1.+A2*P(I,J))**Z*CT1))
	EDA(I,J)=EDA1
	IF(NF.EQ.0)GOTO 55
	IF(I.NE.1.AND.J.NE.1)THEN
	DPDX=(P(I,J)-P(I-1,J))/DX
	DPDY=(P(I,J)-P(I,J-1))/DX
	EDA(I,J)=EQEDA(DPDX,DPDY,P(I,J),H(I,J),EDA1)
	ENDIF
	EDA1=EDA(I,J)
55	RO(I,J)=(A3+1.34*P(I,J))/(A3+P(I,J))+CT2
60	EPS(I,J)=RO(I,J)*H(I,J)**3/(ENDA*EDA1)
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
	COMMON /COM1/ENDA,A1,A2,A3,Z,C3/COMAK/AK(0:65,0:65)
	DATA KG1,PAI1,C1,C2/0,0.2026423,0.31,0.31/
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
	DX4=0.3*DX2
	DO 100 K=1,KK
	PMAX=0.0
	DO 70 J=2,NN
	J0=J-1
	J1=J+1
	JJ=N-J+1
	IA=1
8	MM=N-IA
	IF(P(MM,J0).GT.1.E-6)GOTO 20
	IF(P(MM,J).GT.1.E-6)GOTO 20
	IF(P(MM,J1).GT.1.E-6)GOTO 20
	IA=IA+1
	IF(IA.LT.N)GOTO 8
	GOTO 70
20	IF(MM.LT.N-1)MM=MM+1 
	D2=0.5*(EPS(1,J)+EPS(2,J))
	DO 50 I=2,MM
	I0=I-1
	I1=I+1
	II=5*I0
	D1=D2
	D2=0.5*(EPS(I1,J)+EPS(I,J))
	D4=0.5*(EPS(I,J0)+EPS(I,J))
	D5=0.5*(EPS(I,J1)+EPS(I,J))
	P1=P(I0,JJ)
	P2=P(I1,JJ)
	P3=P(I,JJ)
	P4=P(I,JJ+1)
	P5=P(I,JJ-1)
	D3=D1+D2+D4+D5
	IF(J.EQ.NN.AND.ID(I).EQ.1)P(I,J)=P(I,J)-0.5*C2*D(I)
	IF(H(I,J).LE.0.0)THEN
	ID(I)=2
	A(II+1)=0.0
	A(II+2)=0.0
	A(II+3)=1.0
	A(II+4)=0.0
	A(II+5)=1.0
	A(II-4)=0.0
	GOTO 50
	ENDIF
	IF(D1.GE.DX4)GOTO 30
	IF(D2.GE.DX4)GOTO 30
	IF(D4.GE.DX4)GOTO 30
	IF(D5.GE.DX4)GOTO 30
	ID(I)=1
	IF(J.EQ.NN)P5=P4
	A(II+1)=PAI1*(RO(I0,J)*BK10-RO(I,J)*BK20)
	A(II+2)=DX3*(D1+0.25*D3)+PAI1*(RO(I0,J)*BK00-RO(I,J)*BK10)
	A(II+3)=-1.25*DX3*D3+PAI1*(RO(I0,J)*BK10-RO(I,J)*BK00)
	A(II+4)=DX3*(D2+0.25*D3)+PAI1*(RO(I0,J)*BK20-RO(I,J)*BK10)
	A(II+5)=-DX3*(D1*P1+D2*P2+D4*P4+D5*P5-D3*P3)+DX1*(RO(I,J)*H(I,J)-RO(I0,J)*H(I0,J))
	GOTO 50
30	ID(I)=0
	P4=P(I,J0)
	IF(J.EQ.NN)P5=P4
	A(II+1)=PAI1*(RO(I0,J)*AK10-RO(I,J)*AK20)
	A(II+2)=DX3*D1+PAI1*(RO(I0,J)*AK00-RO(I,J)*AK10)
	A(II+3)=-DX3*D3+PAI1*(RO(I0,J)*AK10-RO(I,J)*AK00)
	A(II+4)=DX3*D2+PAI1*(RO(I0,J)*AK20-RO(I,J)*AK10)
	A(II+5)=-DX3*(D1*P1+D2*P2+D4*P4+D5*P5-D3*P3)+DX1*(RO(I,J)*H(I,J)-RO(I0,J)*H(I0,J))
50	CONTINUE
	CALL TRA4(MM,D,A,B)
	DO 60 I=2,MM
	IF(ID(I).EQ.2)GOTO 60
	IF(ID(I).EQ.0)GOTO 52
	DD=D(I+1)
	IF(I.EQ.MM)DD=0
	P(I,J)=P(I,J)+C2*(D(I)-0.25*(D(I-1)+DD))
	IF(J0.NE.1)P(I,J0)=P(I,J0)-0.25*C2*D(I)
	IF(P(I,J0).LT.0.)P(I,J0)=0.0
	IF(J1.GE.NN)GOTO 54
	P(I,J1)=P(I,J1)-0.25*C2*D(I)
	GOTO 54
52	P(I,J)=P(I,J)+C1*D(I)
54	IF(P(I,J).LT.0.0)P(I,J)=0.0
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
	SUBROUTINE THERM(NX,NY,NZ,DX,P,H,T)
	DIMENSION T(NX,NY,NZ),T1(21),TI(21),U(21),DU(21),UU(21),V(21),DV(21),VV(21),W(21),EDA(21),RO(21),EDA1(21),EDA2(21),ROR(21),P(NX,NX),H(NX,NX),TFX(21),TFY(21)
	COMMON /COMD/AD,AD1,KK1,KK,KK3,KK4
	IF(KK.NE.0)GOTO 4
	DO 2 K=1,NZ
	DO 1 J=1,NY
1   T(1,J,K)=1.0
	DO 2 I=1,NX
2   T(I,1,K)=1.0
4   DO 30 I=2,NX
	DO 30 J=2,NY
	KG=0
	DO 6 K=1,NZ
	TFX(K)=T(I-1,J,K)
	TFY(K)=T(I,J-1,K)
	IF(KK.NE.0)GOTO 5
	T1(K)=T(I-1,J,K)
	GOTO 6
5   T1(K)=T(I,J,K)
6   TI(K)=T1(K)
	P1=P(I,J)
	H1=H(I,J)
	DPX=(P(I,J)-P(I-1,J))/DX
	DPY=(P(I,J)-P(I,J-1))/DX
	CALL TBOUD(NX,NY,NZ,I,J,CC1,CC2,T)
10	CALL EROEQ(NZ,T1,P1,H1,DPX,DPY,EDA,RO,EDA1,EDA2,KG)
	CALL UCAL(NZ,DX,H1,EDA,RO,ROR,EDA1,EDA2,U,UU,DU,V,VV,DV,W,DPX,DPY)
	CALL TCAL(NZ,DX,CC1,CC2,T1,TFX,TFY,U,V,W,DU,DV,H1,DPX,DPY,EDA,RO)
	CALL ERRO(NZ,TI,T1,ETS)
	KG=KG+3
	IF(ETS.GT.1.E-4.AND.KG.LE.50)GOTO 10
	DO 20 K=1,NZ
	ROR(K)=RO(K)
	UU(K)=U(K)
	VV(K)=V(K)
20  T(I,J,K)=T1(K)
30	CONTINUE
	KK=1
	RETURN
	END
	SUBROUTINE TBOUD(NX,NY,NZ,I,J,CC1,CC2,T)
	DIMENSION T(NX,NY,NZ)
	CC1=0.
	CC2=0.
	DO 10 L=1,I-1
	DS=1./SQRT(FLOAT(I-L))
	IF(L.EQ.I-1)DS=1.1666667
	CC1=CC1+DS*(T(L,J,2)-T(L,J,1))
10    CC2=CC2+DS*(T(L,J,NZ)-T(L,J,NZ-1))
	RETURN
	END
	SUBROUTINE ERRO(NZ,T0,T,ETS)
	DIMENSION T0(NZ),T(NZ)
	ETS=0.0
	DO 10 K=1,NZ
	IF(T(K).LT.1.E-5)ETS0=1.
	IF(T(K).GE.1.E-5)ETS0=ABS((T(K)-T0(K))/T(K))
	IF(ETS0.GT.ETS)ETS=ETS0
10    T0(K)=T(K)
	RETURN
	END
	SUBROUTINE EROEQ(NZ,T,P,H,DPX,DPY,EDA,RO,EDA1,EDA2,KG)
	DIMENSION T(NZ),EDA(NZ),RO(NZ),EDA1(NZ),EDA2(NZ)
	COMMON /COM1/ENDA,A1,A2,A3,Z,C3/COM2/T0,AK0,AK1,AK2,CV,CV1,CV2,RO0,RO1,RO2,S0,D0/COM3/E1,PH,B,U1,U2,R,CC/COMC/KT,NF
	DATA A4,A5/0.455445545,0.544554455/
	IF(KG.NE.0)GOTO 20
	B1=(1.+A2*P)**Z
	B2=(A3+1.34*P)/(A3+P)
20    DO 30 K=1,NZ
	EDA3=EXP(A1*(-1.+B1*((T(K)-A4)/A5)**S0))
	EDA(K)=EDA3
	IF(NF.NE.0)EDA(K)=EQEDA(DPX,DPY,P,H,EDA3)
30	RO(K)=B2+D0*T0*(T(K)-1.)
	CC1=0.5/(NZ-1.)
	CC2=1./(NZ-1.)
	C1=0.
	C2=0.
	DO 40 K=1,NZ
	IF(K.EQ.1)GOTO 32
	C1=C1+0.5/EDA(K)+0.5/EDA(K-1)
	C2=C2+CC1*((K-1.)/EDA(K)+(K-2.)/EDA(K-1))
32    EDA1(K)=C1*CC2
40    EDA2(K)=C2*CC2
	RETURN
	END
	SUBROUTINE UCAL(NZ,DX,H,EDA,RO,ROR,EDA1,EDA2,U,UU,DU,V,VV,DV,W,DPX,DPY)
	DIMENSION U(NZ),UU(NZ),DU(NZ),V(NZ),VV(NZ),DV(NZ),W(NZ),ROR(NZ),EDA(NZ),RO(NZ),EDA1(NZ),EDA2(NZ)
	COMMON /COM2/T0,AK0,AK1,AK2,CV,CV1,CV2,RO0,RO1,RO2,S0,D0/COM3/E1,PH,B,U1,U2,R,CC
	COMMON /COMD/AD,AD1,KK1,KK2,KK,KK4
	IF(KK.NE.0)GOTO 20
	A1=U1
	A2=PH*(B/R)**3/E1
	A3=U2-U1
20    CUA=A2*DPX*H
	CUB=CUA*H
	CVA=A2*DPY*H
	CVB=CVA*H
	CC3=A3/H
	CC4=1./EDA1(NZ)
	DO 30 K=1,NZ
	U(K)=A1+CUB*(EDA2(K)-CC4*EDA2(NZ)*EDA1(K))+A3*CC4*EDA1(K)
	V(K)=CVB*(EDA2(K)-CC4*EDA2(NZ)*EDA1(K))
	DU(K)=CUA/EDA(K)*((K-1.)/(NZ-1.)-CC4*EDA2(NZ))+CC3*CC4/EDA(K)
30    DV(K)=CVA/EDA(K)*((K-1.)/(NZ-1.)-CC4*EDA2(NZ))
	A4=B/((NZ-1)*R*DX)
	C1=A4*H
	IF(KK.EQ.0)GOTO 50
	DO 40 K=2,NZ-1
	W(K)=(RO(K-1)*W(K-1)+C1*(RO(K)*(U(K)+V(K))-ROR(K)*(UU(K)+VV(K))))/RO(K)
40	CONTINUE
50      KK=1
	RETURN
	END
	SUBROUTINE TCAL(NZ,DX,CC1,CC2,T,TFX,TFY,U,V,W,DU,DV,H,DPX,DPY,EDA,RO)
	DIMENSION T(NZ),U(NZ),DU(NZ),V(NZ),DV(NZ),W(NZ),EDA(NZ),RO(NZ),A(4,21),D(21),AA(2,21),TFX(NZ),TFY(NZ)
	COMMON /COM2/T0,AK0,AK1,AK2,CV,CV1,CV2,RO0,RO1,RO2,S0,D0/COM3/E1,PH,B,U1,U2,R,CC
	COMMON /COMD/AD,AD1,KK1,KK2,KK3,KK/COME/US,EDA0
	DATA CC5,PAI/0.6666667,3.14159265/
	IF(KK.NE.0)GOTO 5
	KK=1
	A2=-CV*RO0*E1*B**3/(EDA0*AK0*R)
	A3=-E1*PH*B**3*D0/(AK0*EDA0*T0*R)
	A4=-(E1*R)**2/(AK0*EDA0*T0)
	A5=0.5*R/B*A2
	A6=AK0*SQRT(EDA0*R/(PAI*RO1*CV1*U1*E1*AK1*B**3))
	A7=AK0*SQRT(EDA0*R/(PAI*RO2*CV2*U2*E1*AK2*B**3))
5     CC3=A6*SQRT(DX)
	CC4=A7*SQRT(DX)
	DZ=H/(NZ-1.)
	DZ1=1./DZ
	DZ2=DZ1*DZ1
	CC6=A3*DPX
	CC7=A3*DPY
	DO 10 K=2,NZ-1
	A(1,K)=DZ2+DZ1*A5*RO(K)*W(K)
	A(2,K)=-2.*DZ2+A2*RO(K)*(U(K)+V(K))/DX+(CC6*U(K)+CC7*V(K))/RO(K)
	A(3,K)=DZ2-DZ1*A5*RO(K)*W(K)
10	A(4,K)=A4*EDA(K)*(DU(K)**2+DV(K)**2)+A2*RO(K)*(U(K)*TFX(K)+V(K)*TFY(K))/DX
	A(1,1)=0.
	A(2,1)=1.+2.*DZ1*CC3*CC5
	A(3,1)=-2.*DZ1*CC3*CC5
	A(1,NZ)=-2.*DZ1*CC4*CC5
	A(2,NZ)=1.+2.*DZ1*CC4*CC5
	A(3,NZ)=0.
	A(4,1)=1.+CC1*CC3*DZ1
	A(4,NZ)=1.-CC2*CC4*DZ1
	CALL TRA3(NZ,D,A,AA)
	DO 20 K=1,NZ
	T(K)=(1.-CC)*T(K)+CC*D(K)
20    IF(T(K).LT.1.)T(K)=1.
30	CONTINUE
	RETURN
	END
	SUBROUTINE TRA3(N,D,A,B)
	DIMENSION D(N),A(4,N),B(2,N)
	C=1./A(2,N)
	B(1,N)=-A(1,N)*C
	B(2,N)=A(4,N)*C
	DO 10 I=1,N-1
	IN=N-I
	IN1=IN+1
	C=1./(A(2,IN)+A(3,IN)*B(1,IN1))
	B(1,IN)=-A(1,IN)*C
10  B(2,IN)=(A(4,IN)-A(3,IN)*B(2,IN1))*C
	D(1)=B(2,1)
	DO 20 I=2,N
20  D(I)=B(1,I)*D(I-1)+B(2,I)
	RETURN
	END
	SUBROUTINE ERROM(NX,NY,NZ,T,ERM)
	DIMENSION T(NX,NY,NZ)
	COMMON /COMT/T1(65,65)
	ERM=0.
	C1=1./FLOAT(NZ)
	DO 20 I=2,NX
	DO 20 J=2,NY
	TT=0.
	DO 10 K=1,NZ
10  TT=TT+T(I,J,K)
	TT=C1*TT
	ER=ABS((TT-T1(I,J))/TT)
	IF(ER.GT.ERM)ERM=ER
20	T1(I,J)=TT
	RETURN
	END
	SUBROUTINE OUPT(N,DX,X,Y,H,P,EDA,TMAX)
	DIMENSION X(N),Y(N),H(N,N),P(N,N),EDA(N,N)
	COMMON /COM1/ENDA,A1,A2,A3,Z,HM0/COMH/HMC
	COMMON /COMW/W0,T1,T2,RX,B,PH/COMK/LMIN,AKC/COMD/AD,AD1,KK1,KK2,KK3,KK4
	NN=(N+1)/2
	A=0.0
	WRITE(8,110)A,(Y(I),I=1,N)
	DO I=1,N
	WRITE(8,110)X(I),(H(I,J),J=1,N)
	ENDDO
	WRITE(9,110)A,(Y(I),I=1,N)
	DO I=1,N
	WRITE(9,110)X(I),(P(I,J),J=1,N)
	ENDDO
110 FORMAT(66(E12.6,1X))
    F=0.0
	HMIN=H(1,1)
	PMAX=0.0
	HM=0.0
    NCOUN=0
    NPA=0
    DO I=2,N
    DO J=1,N
    IF(X(I).LE.0.0.AND.X(I+1).GE.0.0)THEN
    IF(Y(J).LE.0.0.AND.Y(I+1).GE.0.0)HC=H(I,J)
    ENDIF
    DPDX=(P(I,J)-P(I-1,J))/DX
    TAU=T1*DPDX*H(I,J)+0.5*AKC*T2*EDA(I,J)/H(I,J)
    F=F+TAU
    IF(H(I,J).LT.HMIN)HMIN=H(I,J)
    IF(P(I,J).GT.PMAX)PMAX=P(I,J)
    RAD=SQRT(X(I)*X(I)+Y(J)*Y(J))
    IF(RAD.LE.0.5)THEN
    NCOUN=NCOUN+1
    HM=HM+H(I,J)
    ENDIF
    IF(P(I,J).GT.1.E-6)THEN
    PA=PA+P(I,J)
    NPA=NPA+1
    ENDIF
    ENDDO
    ENDDO
	PA=PA/FLOAT(NPA)
	HM=HM/FLOAT(NCOUN)
    F=B*B*F*DX*DX/W0
    HMIN=HMIN*B*B/RX
    HM=HM*B*B/RX
    HC=HC*B*B/RX
    PMAX=PMAX*PH
    HDM=HM0*B*B/RX
    HDC=HMC*B*B/RX
    WRITE(10,*)'W0,F,HMIN,HC,HDM,HDC,PMAX,HM,TMAX,PA'
    WRITE(10,120)W0,F,HMIN,HC,HDM,HDC,PMAX,HM,TMAX,PA
120	FORMAT(10(1X,E12.6))
    RETURN
    END
	FUNCTION EQEDA(DPDX,DPDY,P,H,EDA)
	COMMON /COME/U0,EDA0/COMW/W0,T1,T2,R,B,PH/COMK/LMIN,AKC
	DATA TAU0/2.E7/
	DPDX1=DPDX*PH/B
	DPDY1=DPDY*PH/B
	P1=P*PH
	H1=H*B*B/R
	EDA1=EDA*EDA0
	TAUL=TAU0+0.036*P1
	C1=-0.5*EDA1*AKC*U0/H1-0.5*H1*DPDX1
	TAU1=DPDX1*H1+C1
	TAU2=C1
	TAUY=0.5*DPDY1*H1
	TAUX=AMAX1(ABS(TAU1),ABS(TAU2))
	TAU=SQRT(TAUX**2+TAUY**2)
	X=TAUL/TAU
	EQEDA=EDA
	IF(X.LT.1)THEN
	EQEDA=EDA*X
	ENDIF
	IF(EQEDA.LT.1)EQEDA=1.
	RETURN
	END
	BLOCK DATA
	COMMON /COM2/T0,AK0,AK1,AK2,CV,CV1,CV2,RO0,RO1,RO2,S0,D0
	DATA T0,AK0,AK1,AK2,CV,CV1,CV2,RO0,RO1,RO2,S0,D0/303.,0.14,46.,46.,2000.,470.,470.,890.,7850.,7850.,-1.1,-0.00065/
	END
