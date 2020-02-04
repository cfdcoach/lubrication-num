        PROGRAM JOURNALSQUEEZE
		DIMENSION X(121),Y(121),H(121,121),P(121,121),PD(121,121),HH(121)
        DIMENSION HXY(121,121),HXF(121,121),HXB(121,121),HYF(121,121),HYB(121,121)
		DIMENSION H1(51),T(51),V1(51),W(51),EPS(51)
		DATA N,N1,N2,N3/121,120,51,50/
		DATA B,D,DD,E,EDA,PI,V/0.024,0.030,0.02992,0.328999E-04,0.02,3.14159265,0.342583E-03/
		OPEN(8,FILE='PRESSURE.DAT',STATUS='UNKNOWN')
		OPEN(9,FILE='FILM.DAT',STATUS='UNKNOWN')
		OPEN(13,FILE='RESULT.DAT',STATUS='UNKNOWN')
		OPEN(14,FILE='LOAD.DAT',STATUS='UNKNOWN')
 		PI2=2.0*PI
		R=D/2
		C=(D-DD)/2
		DX=PI2/N1 	     
		DY=1.0/N1
		RATIO=(PI2*R)/B 
	    ALFA=RATIO**2	
		DXDY=PI2/(N1*N1)
		V=V/C
	    EPSON=E/C
		S=D*B
		DO I=1,N
		X(I)=(I-1)*DX
		Y(I)=-0.5+(I-1)*DY	
		DO J=1,N
        P(I,J)=0.0
		ENDDO
		ENDDO
		WRITE(*,*)'LD=0 挤压过程计算；LD=1 挤压瞬时压力分布与载荷计算?'
		READ(*,*)LD
		IF(LD.EQ.1)THEN
		SUM0=0.0
		GO TO 70
        ELSE
		SUM0=1000.0
		E=0.00001
		GO TO 50
        ENDIF
50		U=PI2/50.0
        DO I=1,N2
        W(I)=SUM0*(1.0+SIN(U*(I-1)))
        ENDDO
        EPS(1)=EPSON
		H1(1)=1-EPS(1)
        DO K=1,N3        
        V=1.0
        V=V/C
		DT=0.001
		WRITE(*,*)K,V,EPSON
        CALL SUBH
		CALL SUBP 
		CALL SUBM    
        ALENDA=(12.0*EDA*R**2)/C**2
        SUM1=ALENDA*S*SUM1
	    WRITE(*,*)SUM1
        V1(K)=W(K)*V/SUM1
	    WRITE(*,*)V1(K)
        EPS(K+1)=EPS(K)+V1(K)*DT 
	    H1(K+1)=1-EPS(K+1)
	    EPSON=EPS(K+1)
	    WRITE(*,*)EPSON
	    ENDDO
	    EPSON=EPS(N2)
	 	V=1.0
        V=V/C
		CALL SUBH	
		CALL SUBP  
		CALL SUBM  
        ALENDA=(12.0*EDA*R**2)/C**2
        SUM1=ALENDA*S*SUM1
        V1(N2)=W(N2)*V/SUM1
		WRITE(13,"('时间T 载荷W 偏心距E 轴心移动速度de/dt 挤压速度dh/dt')")
        DO I=1,N2
        WRITE(13,40)DT*I,W(I),EPS(I),V1(I)*C,-V1(I)*C
        ENDDO
	    STOP
70		CALL SUBH
		CALL SUBP
		CALL SUBM
		CALL SUBMAX 
		ALENDA=(12.0*EDA*R**2)/C**2
		SUM1=ALENDA*S*SUM1
		WRITE(8,40)Y(1),(Y(I),I=1,N)
		DO I=1,N
		WRITE(8,40)X(I)*180.0/PI,(P(I,J),J=1,N)
		ENDDO
		WRITE(9,40)Y(1),(Y(I),I=1,N)
		DO I=1,N
		WRITE(9,40)X(I)*180.0/PI,(H(I,J),J=1,N)
		ENDDO
		WRITE(14,*)'承载力W'
		WRITE(14,*)SUM1
		WRITE(*,*)AIMAX,J_MAX,I_MAX
40		FORMAT(122(E12.6,1X))
		CONTAINS
		SUBROUTINE SUBH
		DO I=1,N
		DO J=1,N
		H(I,J)=1.0+EPSON*COS((I-1)*DX)
		ENDDO	
		ENDDO
		DO I=1,N
		I1=I-1
		I2=I+1
		IF(I.EQ.1)I1=N1
		IF(I.EQ.N)I2=2
		DO J=2,N1
		HXF(I,J)=(0.5*(H(I2,J)+H(I,J)))**3
		HXB(I,J)=(0.5*(H(I1,J)+H(I,J)))**3
		HYF(I,J)=ALFA*(0.5*(H(I,J+1)+H(I,J)))**3
		HYB(I,J)=ALFA*(0.5*(H(I,J-1)+H(I,J)))**3
		HXY(I,J)=1.0/(HXF(I,J)+HXB(I,J)+HYF(I,J)+HYB(I,J))
		ENDDO
		ENDDO        
		RETURN
		END SUBROUTINE SUBH
		SUBROUTINE SUBP
		DO I=1,N
		DO J=1,N
		PD(I,J)=P(I,J)
		ENDDO
		ENDDO
		IK=0
        TEMP0=DX**2*V
10		C1=0.0
		DO I=1,N1
		I1=I-1
		I2=I+1
		IF(I1.EQ.0)I1=N1
		IF(I2.EQ.N)I2=1
		DO J=2,N1
		P(I,J)=(HXF(I,J)*P(I2,J)+HXB(I,J)*P(I1,J)+HYF(I,J)*P(I,J+1)+HYB(I,J)*P(I,J-1)-TEMP0*COS((I-1)*DX))*HXY(I,J)
        IF(P(I,J).LE.0.0)P(I,J)=0.0
        C1=C1+ABS(P(I,J)-PD(I,J))
		PD(I,J)=P(I,J)
		ENDDO
		ENDDO
		DO J=2,N1
		P(N,J)=P(1,J)
		PD(N,J)=PD(1,J)
		ENDDO
		IK=IK+1	
		IF(C1.GT.1.E-20.AND.IK.LE.20000)GOTO 10
 		RETURN
		END SUBROUTINE SUBP
		SUBROUTINE SUBM
		PX=0.0
		PY=0.0
		TEMP=PI/60.0
		DO I=1,N1
		AI=(I-1)*TEMP
		DO J=1,N	
		PX=PX-P(I,J)*COS(AI)*DXDY
		PY=PY+P(I,J)*SIN(AI)*DXDY
		ENDDO
		ENDDO
		SUM1=SQRT(PX*PX+PY*PY)
        RETURN
		END SUBROUTINE SUBM
		SUBROUTINE SUBMAX
		TEMP0=PI2/N1
		PMAX=P(2,2)
		DO I=1,N
        DO J=1,N
        IF(P(I,J).GE.PMAX)THEN
        PMAX=P(I,J)
        I_MAX=I
        J_MAX=J
        ENDIF
        ENDDO
        ENDDO
        AIMAX=(I_MAX-1)*TEMP0*180/PI
	    RETURN
		END  SUBROUTINE SUBMAX
		END
