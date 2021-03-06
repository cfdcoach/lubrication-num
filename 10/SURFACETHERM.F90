	PROGRAM SURFACETHERM
	DIMENSION X(200),Y(200),P(20000),H(20000),T(20000)
	DATA U,ALX,ALY,EDA0,RO,C,AJ,H1,H2/1.0,0.01,0.01,0.05,890.0,1870.0,4.184,1.1E-6,1.E-6/
	OPEN(7,FILE='FILM.DAT',STATUS='UNKNOWN')
	OPEN(8,FILE='PRESSURE.DAT',STATUS='UNKNOWN')
	OPEN(9,FILE='TEM.DAT',STATUS='UNKNOWN')
	N=129
	M=65
	A=U*ALX*EDA0/2.0/AJ/RO/C/H2**2
	T0=303.0/A
	DX=1./(N-1.0)
	DY=1./(M-1.0)
	HH=H1/H2
	DH=HH-1.0
	ALFA1=ALX/ALY
	CALL INIT(N,M,DX,DY,HH,DH,T0,X,Y,H,P,T)
	CALL THERM(N,M,A,ALFA1,DX,DY,T0,X,Y,P,H,T)
	CALL OUTPUT(N,M,A,T0,X,Y,H,P,T)
	STOP
	END
	SUBROUTINE INIT(N,M,DX,DY,HH,DH,T0,X,Y,H,P,T)
	DIMENSION X(N),Y(M),H(N,M),P(N,M),T(N,M)
	DO I=1,N
	X(I)=(I-1)*DX
	ENDDO
	DO J=1,M
	Y(J)=-0.5+(J-1)*DY
	ENDDO
	DO I=1,N
	DO J=1,M
	H(I,J)=HH-DH*X(I)
	P(I,J)=-(-1.0/(H(I,J))+HH/(HH+1.0)/H(I,J)**2+1.0/(HH+1.0))/DH*(1.0-4.0*Y(J)*Y(J))
	T(I,J)=T0
	ENDDO
	ENDDO
	DO I=1,N
	P(I,1)=0.0
	P(I,M)=0.0
	ENDDO
	DO J=1,M
	P(1,J)=0.0
	P(N,J)=0.0
	ENDDO
	RETURN
	END
	SUBROUTINE THERM(N,M,A,ALFA1,DX,DY,T0,X,Y,P,H,T)
	DIMENSION X(N),Y(M),H(N,M),P(N,M),T(N,M)
10	ERT=0.0
	DO I=2,N
	DO J=M/2+1,1,-1
	TOLD=T(I,J)
	EDA=EXP(-0.03*A*(T(I,J)-T0))
	DPDX=(P(I,J)-P(I-1,J))/DX
	IF(J.EQ.M/2+1)THEN
	DPDY=0.0
	DTDY=0.0
	ELSE
	DPDY=(P(I,J+1)-P(I,J))/DY
	DTDY=(T(I,J+1)-T(I,J))/DY
	ENDIF
	QX=0.5*H(I,J)-0.5*H(I,J)**3*DPDX
	QY=-0.5*H(I,J)**3*DPDY
	AA=-0.5*ALFA1*QY*DTDY
	AB=2.0*EDA/H(I,J)
	AC=6.0*H(I,J)/EDA*(DPDX**2+ALFA1**2*DPDY**2)
	BA=QX/DX-ALFA1*QY/DY
	BB=QX/DX*T(I-1,J)-ALFA1*QY/DY*T(I,J+1)
	T(I,J)=(BB+AB+AC)/BA
	T(I,J)=0.7*TOLD+0.3*T(I,J)
	ERT=ERT+ABS(T(I,J)-TOLD)
	ENDDO
	ENDDO
	ERT=A*ERT/(303.0*(N-1)*(M-1))
	WRITE(*,*)ERT
	IF(ERT.GT.1.E-8)GOTO 10
	DO I=2,N
	DO J=1,M/2
	T(I,M-J+1)=T(I,J)
	ENDDO
	ENDDO
	RETURN
	END
	SUBROUTINE OUTPUT(N,M,A,T0,X,Y,H,P,T)
	DIMENSION X(N),Y(M),H(N,M),P(N,M),T(N,M)
	DO I=1,N
	DO J=1,M
	T(I,J)=A*(T(I,J)-T0)
	END DO
	ENDDO
	WRITE(7,30)X(1),(Y(J),J=1,M)
	WRITE(8,30)X(1),(Y(J),J=1,M)
	WRITE(9,30)X(1),(Y(J),J=1,M)
	DO I=1,N
	WRITE(7,30)X(I),(H(I,J),J=1,M)
	WRITE(8,30)X(I),(P(I,J),J=1,M)
	WRITE(9,30)X(I),(T(I,J),J=1,M)
	ENDDO
30  FORMAT(130(1X,E12.6))
	RETURN
	END
