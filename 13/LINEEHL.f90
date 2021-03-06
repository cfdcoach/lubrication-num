		PROGRAM LINEEHL
		CHARACTER*1 S,S1,S2
		COMMON /COM1/ENDA,A1,A2,A3,Z,C1,C3,CW/COM2/EDA0/COM4/X0,XE/COM3/E1,PH,B,U1,U2,R
		DATA PAI,Z,P0/3.14159265,0.68,1.96E8/,S1,S2/1HY,1Hy/
		DATA N,X0,XE,W,E1,EDA0,R,Us,CU,C1/129,-4.0,1.4,1.E5,2.21E11,0.028,0.012183,0.87,0.67,0.5/
		OPEN(8,FILE='result.DAT',STATUS='UNKNOWN')
		WRITE(*,*)'Show the example or not (Y or N)?'
		READ(*,'(A)')S
		IF(S.EQ.S1.OR.S.EQ.S2)THEN
		GOTO 10
		ENDIF
		WRITE(*,*)'N,PH,US='
		READ(*,*)N,PH,US
		W=2.*PAI*R*PH*(PH/E1)
		WRITE(*,*)'W=',W
10		W1=W/(E1*R)
		PH=E1*SQRT(0.5*W1/PAI)
		A1=(ALOG(EDA0)+9.67)
		A2=PH/P0
		A3=0.59/(PH*1.E-9)
		B=4.*R*PH/E1
		ALFA=Z*A1/P0
		G=ALFA*E1
		U=EDA0*US/(2.*E1*R)
		CC1=SQRT(2.*U)
		AM=2.*PAI*(PH/E1)**2/CC1
		AL=G*SQRT(CC1)
		CW=(PH/E1)*(B/R)
		C3=1.6*(R/B)**2*G**0.6*U**0.7*W1**(-0.13)
		ENDA=3.*(PAI/AM)**2/8.
		U1=0.5*(2.+CU)*U
		U2=0.5*(2.-CU)*U
		CW=-1.13*C3
		WRITE(*,*)N,X0,XE,W,E1,EDA0,R,US
		WRITE(8,*)N,X0,XE,W,E1,EDA0,R,US,B,PH
		WRITE(*,40)
40      FORMAT(2X,'                     Wait     Please',//)
		CALL SUBAK(N)
		CALL EHL(N)
		STOP
		END
		SUBROUTINE EHL(N)
		DIMENSION X(1100),P(1100),H(1100),RO(1100),POLD(1100),EPS(1100),EDA(1100),R(1100)
		COMMON /COM1/ENDA,A1,A2,A3,Z,C1,C3,CW/COM4/X0,XE
		COMMON /COM3/E1,PH,B,U1,U2,RR
		DATA MK,G0/1,1.570796325/
		NX=N
		DX=(XE-X0)/(N-1.0)
		DO 10 I=1,N
		X(I)=X0+(I-1)*DX
		IF(ABS(X(I)).GE.1.0)P(I)=0.0
		IF(ABS(X(I)).LT.1.0)P(I)=SQRT(1.-X(I)*X(I))
10      CONTINUE
		CALL HREE(N,DX,H00,G0,X,P,H,RO,EPS,EDA)
		CALL FZ(N,P,POLD)
14		KK=19
		CALL ITER(N,KK,DX,H00,G0,X,P,H,RO,EPS,EDA,R)
		MK=MK+1
		CALL ERROP(N,P,POLD,ERP)
		WRITE(*,*)ERP
		IF(MK.EQ.2)THEN
		ENDIF
		IF(ERP.GT.1.E-4.AND.MK.LE.200)THEN
		GOTO 14
		ENDIF
105		IF(MK.GE.200)THEN
		WRITE(*,*)'Pressures are not convergent !!!'
		READ(*,*)
		ENDIF
	 	H2=1.E3
		P2=0.0
		DO 106 I=1,N
		IF(H(I).LT.H2)H2=H(I)
		IF(P(I).GT.P2)P2=P(I)
106     CONTINUE
		H3=H2*B*B/RR
		P3=P2*PH
110		FORMAT(6(1X,E12.6))
120		CONTINUE
		WRITE(8,*)'P2,H2,P3,H3=',P2,H2,P3,H3
		CALL OUTHP(N,X,P,H,R)
		RETURN
		END
		SUBROUTINE OUTHP(N,X,P,H,R)
		DIMENSION X(N),P(N),H(N),R(N)
		DX=X(2)-X(1)
		DO 10 I=1,N
		WRITE(8,20)X(I),P(I),H(I),R(I)
10      CONTINUE
20      FORMAT(1X,6(F12.6,1X))
		RETURN
		END
		SUBROUTINE HREE(N,DX,H00,G0,X,P,H,RO,EPS,EDA)
		DIMENSION X(N),P(N),H(N),RO(N),EPS(N),EDA(N)
		DIMENSION W(2200)
		COMMON /COM1/ENDA,A1,A2,A3,Z,C1,C3,CW,K/COM2/EDA0/COMAK/AK(0:1100)
		DATA KK,NW,PAI1/0,2200,0.318309886/
		IF(KK.NE.0)GOTO 3
		HM0=C3
		H00=0.0
3		W1=0.0
		DO 4 I=1,N
4       W1=W1+P(I)
		C3=(DX*W1)/G0
		DW=1.-C3
		CALL VI(N,DX,P,W)
		HMIN=1.E3
		DO 30 I=1,N
		H0=0.5*X(I)*X(I)+W(I)
		IF(H0.LT.HMIN)HMIN=H0
		H(I)=H0
30		CONTINUE
		IF(KK.NE.0)GOTO 32
		KK=1
		H00=-HMIN+HM0
32		H0=H00+HMIN
		IF(H0.LE.0.0)GOTO 48
		IF(H0+0.3*CW*DW.GT.0.0)HM0=H0+0.3*CW*DW
		IF(H0+0.3*CW*DW.LE.0.0)HM0=HM0*C3
48		H00=HM0-HMIN
50		DO 60 I=1,N
60		H(I)=H00+H(I)
		DO 100 I=1,N
		EDA(I)=EXP(A1*(-1.+(1.+A2*P(I))**Z))
		RO(I)=(A3+1.34*P(I))/(A3+P(I))
		EPS(I)=RO(I)*H(I)**3/(ENDA*EDA(I))
100		CONTINUE
		RETURN
		END
		SUBROUTINE ITER(N,KK,DX,H00,G0,X,P,H,RO,EPS,EDA,R)
		DIMENSION X(N),P(N),H(N),RO(N),EPS(N),EDA(N),R(N)
		COMMON /COM1/ENDA,A1,A2,A3,Z,C1,C3,CW
		COMMON /COMAK/AK(0:1100)
		DATA KG1,PAI/0,3.14159265/
		IF(KG1.NE.0)GOTO 5
		KG1=1
		DX1=1./DX
		DX2=DX*DX
		DX3=1./DX2
		DX4=DX1/PAI
		DXL=DX*ALOG(DX)
		AK0=DX*AK(0)+DXL
		AK1=DX*AK(1)+DXL
5		DO 100 K=1,KK
		D2=0.5*(EPS(1)+EPS(2))
		D3=0.5*(EPS(2)+EPS(3))
		D5=DX1*(RO(2)*H(2)-RO(1)*H(1))
		D7=DX4*(RO(2)*AK0-RO(1)*AK1)
		PP=0.
		DO 70 I=2,N-1
		D1=D2
		D2=D3
		D4=D5
		D6=D7
		IF(I+2.LE.N)D3=0.5*(EPS(I+1)+EPS(I+2))
		D5=DX1*(RO(I+1)*H(I+1)-RO(I)*H(I))
		D7=DX4*(RO(I+1)*AK0-RO(I)*AK1)
		DD=(D1+D2)*DX3
		IF(0.05*DD.LT.ABS(D6))GOTO 10
		RI=-DX3*(D1*P(I-1)-(D1+D2)*P(I)+D2*P(I+1))+D4
		R(I)=RI
		DLDP=-DX3*(D1+D2)+D6
		RI=RI/DLDP
		RI=RI/C1
		GOTO 20
10		RI=-DX3*(D1*PP-(D1+D2)*P(I)+D2*P(I+1))+D4
		R(I)=RI
		DLDP=-DX3*(2.*D1+D2)+2.*D6
		RI=RI/DLDP
		RI=0.5*RI
		IF(I.GT.2.AND.P(I-1)-C1*RI.GT.0.0)P(I-1)=P(I-1)-C1*RI
20      PP=P(I)
		P(I)=P(I)+C1*RI
		IF(P(I).LT.0.0)P(I)=0.0
		IF(P(I).LE.0.0)R(I)=0.0
70		CONTINUE
		CALL HREE(N,DX,H00,G0,X,P,H,RO,EPS,EDA)
100     CONTINUE
		RETURN
		END
		SUBROUTINE VI(N,DX,P,V)
		DIMENSION P(N),V(N)
		COMMON /COMAK/AK(0:1100)
		PAI1=0.318309886
		C=ALOG(DX)
		DO 10 I=1,N
		V(I)=0.0
		DO 10 J=1,N
		IJ=IABS(I-J)
10		V(I)=V(I)+(AK(IJ)+C)*DX*P(J)
		DO I=1,N
		V(I)=-PAI1*V(I)
		ENDDO
		RETURN
		END
		SUBROUTINE SUBAK(MM)
		COMMON /COMAK/AK(0:1100)
		DO 10 I=0,MM
10      AK(I)=(I+0.5)*(ALOG(ABS(I+0.5))-1.)-(I-0.5)*(ALOG(ABS(I-0.5))-1.)
		RETURN
		END
		SUBROUTINE FZ(N,P,POLD)
		DIMENSION P(N),POLD(N)
		DO 10 I=1,N
10      POLD(I)=P(I)
		RETURN
		END
		SUBROUTINE ERROP(N,P,POLD,ERP)
		DIMENSION P(N),POLD(N)
		SD=0.0
		SUM=0.0
		DO 10 I=1,N
		SD=SD+ABS(P(I)-POLD(I))
		POLD(I)=P(I)
10      SUM=SUM+P(I)
		ERP=SD/SUM
		RETURN
		END

