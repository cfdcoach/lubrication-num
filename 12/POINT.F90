	DIMENSION P(4500),H(4500),V(4500),X(65),Y(65)
	OPEN (8,FILE='PRESS.DAT',STATUS='UNKNOWN')
	OPEN (10,FILE='FILM.DAT',STATUS='UNKNOWN')
	N=33
	CALL SUBAK(N)
	CALL PCAL(N,X,Y,P,H,V)
	STOP
	END
	SUBROUTINE PCAL(N,X,Y,P,H,V)
	DIMENSION P(N,N),H(N,N),X(N),Y(N),V(N,N)
	COMMON /COMAK/AK(0:65,0:65)
	KL=ALOG(N-1.)/ALOG(2.)-1.99
	DX=2.4/(N-1.0)
	DO I=1,N
	X(I)=-1.2+DX*(I-1)
	A=X(I)*X(I)
	DO J=1,N
	Y(J)=-1.2+DX*(J-1)
	P(I,J)=0.0
	H(I,J)=0.5*A+0.5*Y(J)*Y(J)
	ENDDO
	ENDDO
	M=0
	DO I=1,N
	DO J=1,N
	A=1.0-X(I)*X(I)-Y(J)*Y(J)
	IF(A.GE.0.0) P(I,J)=SQRT(A)
	ENDDO
	ENDDO
	CALL VI(N,DX,P,V)
	DO 10 I=1,N
	DO 10 J=1,N
	H(I,J)=H(I,J)+V(I,J)
10	CONTINUE
	XP=1.0
	WRITE(8,20)XP,(Y(I),I=1,N)
	WRITE(10,20)XP,(Y(I),I=1,N)
	DO I=1,N
	WRITE(8,20)X(I),(P(I,J),J=1,N)
	WRITE(10,20)X(I),(H(I,J),J=1,N)
	ENDDO
20	FORMAT(1X,34(F6.3,1X))
	STOP
	END
       SUBROUTINE VI(N,DX,P,V)
       DIMENSION P(N,N),V(N,N)
       COMMON /COMAK/AK(0:65,0:65)
	   PAI1=0.2026423
       DO 40 I=1,N
       DO 40 J=1,N
       H0=0.0
       DO 30 K=1,N
       IK=IABS(I-K)
       DO 30 L=1,N
       JL=IABS(J-L)
30     H0=H0+AK(IK,JL)*P(K,L)
40     V(I,J)=H0*DX*PAI1
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
10     AK(J,I)=AK(I,J)
       RETURN
       END