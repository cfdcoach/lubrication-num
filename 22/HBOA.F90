		PROGRAM HBOA
		USE DFLOGM
		INCLUDE 'RESOURCE.FD'
		CALL DoDialog()	
		STOP
		END PROGRAM

		SUBROUTINE DoDialog() 
		USE DFLOGM  
		INCLUDE 'RESOURCE.FD'
		INTEGER retint  
		LOGICAL retlog  
		TYPE (dialog) dlg  
		EXTERNAL DISPLAY  
		EXTERNAL CLEAR
		EXTERNAL CHOOSE
		EXTERNAL DISPLAY1
		IF ( .not. DlgInit( IDD_DIALOG1, dlg ) ) THEN
		WRITE (*,*) 'Error: dialog not found' 
		ELSE
         retlog=dlgset(dlg,idc_EDIT_DD,"2.992")
         retlog=dlgsetsub(dlg,idc_EDIT_DD,CLEAR,dlg_change)
         retlog=dlgset(dlg,idc_EDIT_D,"3.0")
         retlog=dlgsetsub(dlg,idc_EDIT_D,CLEAR,dlg_change)		 
         retlog=dlgset(dlg,idc_EDIT_AL,"2.4")
		 retlog=dlgsetsub(dlg,idc_EDIT_AL,CLEAR,dlg_change)
		 retlog=dlgset(dlg,idc_EDIT_AN,"5000")
		 retlog=dlgsetsub(dlg,idc_EDIT_AN,CLEAR,dlg_change)
         retlog=dlgset(dlg,idc_EDIT_M,"10")
		 retlog=dlgsetsub(dlg,idc_EDIT_M,CLEAR,dlg_change)
 		 retlog=dlgset(dlg,idc_EDIT_MAXALF,"60")
		 retlog=dlgsetsub(dlg,idc_EDIT_MAXALF,CLEAR,dlg_change)
		 retlog=dlgset(dlg,idc_EDIT_MINALF,"20")
		 retlog=dlgsetsub(dlg,idc_EDIT_MINALF,CLEAR,dlg_change)
		 retlog=dlgset(dlg,idc_EDIT_MAXRLGL,"1.3")
		 retlog=dlgsetsub(dlg,idc_EDIT_MAXRLGL,CLEAR,dlg_change)
		 retlog=dlgset(dlg,idc_EDIT_MINRLGL,"0.7")
		 retlog=dlgsetsub(dlg,idc_EDIT_MINRLGL,CLEAR,dlg_change)
		 retlog=dlgset(dlg,idc_EDIT_MAXCG,"0.01")	
		 retlog=dlgsetsub(dlg,idc_EDIT_MAXCG,CLEAR,dlg_change)
		 retlog=dlgset(dlg,idc_EDIT_MINCG,"0.003")	
		 retlog=dlgsetsub(dlg,idc_EDIT_MINCG,CLEAR,dlg_change)
		 retlog=dlgset(dlg,idc_EDIT_DEDA,"24.5")
		 retlog=dlgsetsub(dlg,idc_EDIT_DEDA,CLEAR,dlg_change)	
		 retlog=dlgset(dlg,idc_EDIT_RO,"0.945")	
		 retlog=dlgsetsub(dlg,idc_EDIT_RO,CLEAR,dlg_change)
		 retlog=dlgset(dlg,idc_EDIT_AS,"80")
		 retlog=dlgsetsub(dlg,idc_EDIT_AS,CLEAR,dlg_change)	
		 retlog=dlgset(dlg,idc_EDIT_CO,"1800")
		 retlog=dlgsetsub(dlg,idc_EDIT_CO,CLEAR,dlg_change)	
		 retlog=dlgsetsub(dlg,idc_CALCULATE,DISPLAY)
		 retlog=dlgsetsub(dlg,idc_continue,DISPLAY1)
         retlog=dlgset(dlg,idc_CONTINUE,.false.,dlg_enable )
		 retlog=dlgsetsub(dlg,idc_CLEAR,CLEAR)		 	
		 retlog=dlgset(dlg,idc_EDIT_EPSON,.TRUE.)
		 retlog=dlgset(dlg,idc_EDIT_SUM,.FALSE.)
		 retlog=dlgset(dlg,idc_RADIO_LOAD,.TRUE.)
		 retlog=dlgset(dlg,idc_RADIO_FLUX,.FALSE.)
	     retlog1=dlgset(dlg,idc_RADIO_SUM,.FALSE.,dlg_enable)
		 retlog=dlgsetsub(dlg,IDC_RADIO_LOAD,CHOOSE)
		 retlog=dlgsetsub(dlg,IDC_RADIO_FLUX,CHOOSE)
		 retlog=dlgsetsub(dlg,IDC_RADIO_SUM,CHOOSE)
		 retlog=dlgsetsub(dlg,IDC_RADIO_EPSON,CHOOSE)
	     retlog=dlgset(dlg,IDC_RADIO_DYNAMIC,.FALSE.,DLG_STATE)
		 retlog=dlgset(dlg,IDC_RADIO_STATIC,.TRUE.,DLG_STATE)
		 retint=DLGMODAL(dlg)
		 CALL DLGUNINIT(dlg)
		END IF
		RETURN
		END SUBROUTINE DoDialog
		
		SUBROUTINE CHOOSE(dlg,control_name,calltype)
		USE DFLOGM
		INCLUDE 'resource.fd'
		TYPE (dialog) dlg
		INTEGER retint,LF
		LOGICAL retlog,retlog1,PUSHED_state
		retlog=dlgget(dlg,idc_RADIO_LOAD,PUSHED_state)
		IF(PUSHED_state) THEN
		LF=1
		retlog=dlgset(dlg,idc_RADIO_SUM,.false.,dlg_enable)
	    retlog=dlgset(dlg,idc_EDIT_SUM,.false.,dlg_enable)
        retlog=dlgset(dlg,idc_EDIT_EPSON,.TRUE.,dlg_enable)
		ELSE
		LF=0
		retlog=dlgset(dlg,idc_RADIO_SUM,.TRUE.,dlg_enable)
		ENDIF
		retlog=dlgget(dlg,idc_RADIO_EPSON,PUSHED_state)
		IF(PUSHED_STATE.AND.LF.EQ.0) THEN
	    retlog1=dlgset(dlg,idc_EDIT_SUM,.FALSE.,dlg_enable)
        retlog1=dlgset(dlg,idc_EDIT_EPSON,.TRUE.,dlg_enable)		
		ELSE
		IF(LF.EQ.0) THEN
	    retlog=dlgset(dlg,idc_EDIT_EPSON,.FALSE.,dlg_enable)
		retlog=dlgset(dlg,idc_EDIT_SUM,.TRUE.,dlg_enable)
		ELSE
		retlog=dlgset(dlg,idc_RADIO_SUM,.false.,dlg_enable)
	    retlog=dlgset(dlg,idc_EDIT_SUM,.false.,dlg_enable)
        retlog=dlgset(dlg,idc_EDIT_EPSON,.TRUE.,dlg_enable)
		ENDIF
		ENDIF
		END SUBROUTINE CHOOSE
				
		SUBROUTINE CLEAR(dlg,control_name,calltype)
        USE DFLOGM
		INCLUDE 'resource.fd'
		TYPE (dialog) dlg
		LOGICAL retlog
		INTEGER retint,control_name,calltype 
		retlog=dlgset(dlg,idc_EDIT_SUM2,"   ")
		retlog=dlgset(dlg,idc_EDIT_EPSON2,"   ")
        retlog=dlgset(dlg,idc_EDIT_E,"   ")
		retlog=dlgset(dlg,idc_EDIT_ALF2,"   ")
		retlog=dlgset(dlg,idc_EDIT_Q,"   ")
		retlog=dlgset(dlg,idc_EDIT_RLGL2,"    ")
		retlog=dlgset(dlg,idc_EDIT_CG2,"    ")
		RETURN 
		END SUBROUTINE CLEAR
		
		SUBROUTINE DISPLAY1(dlg,control_name,calltype)
		USE dflogm
		INCLUDE 'resource.fd'
		TYPE (dialog) dlg
		LOGICAL retlog, pushed_state
		INTEGER retint,control_name,calltype
		INTEGER LCON
		retlog=dlgset(dlg,idc_EDIT_SUM2,"   ")
		retlog=dlgset(dlg,idc_EDIT_EPSON2,"   ")
        retlog=dlgset(dlg,idc_EDIT_E,"   ")
		retlog=dlgset(dlg,idc_EDIT_ALF2,"   ")
		retlog=dlgset(dlg,idc_EDIT_Q,"   ")
		retlog=dlgset(dlg,idc_EDIT_RLGL2,"    ")
		retlog=dlgset(dlg,idc_EDIT_CG2,"    ")
		retlog=dlgset(dlg,idc_RADIO_continue,.true.)        
        CALL DISPLAY(dlg,control_name,calltype)
        END SUBROUTINE DISPLAY1
	


        SUBROUTINE DISPLAY(dlg,control_name,calltype)
		USE dflogm
		INCLUDE 'resource.fd'
		TYPE (dialog) dlg
		LOGICAL retlog, pushed_state
		INTEGER retint,control_name,calltype,LLL
		INTEGER LCON
		CHARACTER(256) text 
		DATA PI/3.14159265/				
		OPEN(8,FILE='pressure.DAT',STATUS='UNKNOWN')
		OPEN(9,FILE='height.DAT',STATUS='UNKNOWN')
		OPEN(14,FILE='result.DAT',STATUS='UNKNOWN')
		OPEN(13,FILE='ORESULT.DAT',STATUS='UNKNOWN')
		LLL=0
        retlog=dlgget(dlg,idc_radio_continue,pushed_state)
		IF(pushed_state) GO TO 100
		retlog=dlgget(dlg,idc_edit_MAXALF,text)
		read(text,*) ALFMAX
		retlog=dlgget(dlg,idc_edit_MINALF,text)
		read(text,*) ALFMIN
		retlog=dlgget(dlg,idc_edit_MAXRLGL,text)
		read(text,*) RLGLMAX
		retlog=dlgget(dlg,idc_edit_MINRLGL,text)
		read(text,*) RLGLMIN
		retlog=dlgget(dlg,idc_edit_MAXCG,text)
		read(text,*) CGMAX
	    retlog=dlgget(dlg,idc_edit_MINCG,text)
		read(text,*) CGMIN
		retlog=dlgget(dlg,idc_edit_DD,text)
		read(text,*) DD
		retlog=dlgget(dlg,idc_edit_D,text)
		read(text,*) D
		retlog=dlgget(dlg,idc_edit_AL,text)
		read(text,*) AL
		retlog=dlgget(dlg,idc_edit_AN,text)
		read(text,*) AN
	    retlog=dlgget(dlg,idc_edit_M,text)
		read(text,*) M
		retlog=dlgget(dlg,idc_EDIT_DEDA,text)				
		read(text,*) DEDA
    	retlog=dlgget(dlg,idc_EDIT_RO,text)
		read(text,*) RO 	
		retlog=dlgget(dlg,idc_EDIT_AS,text)
		read(text,*) AS	
		retlog=dlgget(dlg,idc_EDIT_CO,text)	
        read(text,*) CO
		retlog = DLGGET (dlg, IDC_RADIO_FLUX, pushed_state)
        IF (PUSHED_STATE) THEN
        LF=0
		ELSE
		LF=1
		ENDIF 
        retlog = DLGGET (dlg, IDC_RADIO_EPSON, pushed_state)
        IF (PUSHED_STATE) THEN
		retlog=dlgget(dlg,idc_edit_EPSON,text)
		read(text,*) EPSON
		W0=0.0
		ME=1
		ELSE
		retlog=dlgget(dlg,idc_edit_SUM,text)
		read(text,*) W0
		EPSON=0.2
		ME=0
		ENDIF
        retlog = DLGGET (dlg, IDC_RADIO_STATIC, pushed_state)
		IF(PUSHED_STATE) THEN
		LD=1
		ELSE
		LD=-1
		END IF 
!		WRITE(*,*)'LF,ME,LD=',LF,ME,LD

        Q1=0.0
		SUM=0.0
        DD=DD*1.0E-3
		D=D*1.0E-3
		AL=AL*1.0E-3
		RO=RO*1.0E3
		DEDA=DEDA*1.0E-6

		X1=ALFMAX*PI/180
		X0=ALFMIN*PI/180
		Y1=RLGLMAX
		Y0=RLGLMIN
        Z1=CGMAX*1.0E-3
		Z0=CGMIN*1.0E-3
10		DX=(X1-X0)/10.0
		DY=(Y1-Y0)/10.0
		DZ=(Z1-Z0)/10.0
		CALL RANDOM(RVAL)
		I=IFIX(RVAL*10)+1
		XMIN=X0+I*(X1-X0)/10.0
		CALL RANDOM(RVAL)
		J=IFIX(RVAL*10)+1
		YMIN=Y0+J*(Y1-Y0)/10.0
		CALL RANDOM(RVAL)
		K=IFIX(RVAL*10)+1
		ZMIN=Z0+K*(Z1-Z0)/10.0
		WRITE(*,*)'XMIN=',XMIN,'YMIN=',YMIN,'ZMIN=',ZMIN
		FF=F(XMIN,YMIN,ZMIN,DD,D,AL,AN,RO,W0,DEDA,AS,CO,LF,ME,LD,M,EPSON,Q1,SUM)
		FMIN=FF
		WRITE(*,*) 'FMIN=', FMIN
		DO MM=1,11
		Z=Z0+DZ*(MM-1)
		FF=F(XMIN,YMIN,Z,DD,D,AL,AN,RO,W0,DEDA,AS,CO,LF,ME,LD,M,EPSON,Q1,SUM)
		WRITE(*,*)'XMIN,YMIN,Z,FF=',XMIN,YMIN,Z,FF
		IF(FF.LT.FMIN)THEN
		K=MM
		ZMIN=Z
		FMIN=FF
		ENDIF
		ENDDO
		WRITE(*,*)'XMIN=',XMIN,'YMIN=',YMIN,'ZMIN=',ZMIN,'FMIN=',FMIN
		DO MM=1,11
		Y=Y0+DY*(MM-1)
		FF=F(XMIN,Y,ZMIN,DD,D,AL,AN,RO,W0,DEDA,AS,CO,LF,ME,LD,M,EPSON,Q1,SUM)
		WRITE(*,*)'XMIN,Y,ZMIN,FF=',XMIN,Y,ZMIN,FF
		IF(FF.LT.FMIN)THEN
		J=MM
		YMIN=Y
		FMIN=FF
		ENDIF
		ENDDO
		WRITE(*,*)'XMIN=',XMIN,'YMIN=',YMIN,'ZMIN=',ZMIN,'FMIN=',FMIN
		DO MM=1,11
		X=X0+DX*(MM-1)
		FF=F(X,YMIN,ZMIN,DD,D,AL,AN,RO,W0,DEDA,AS,CO,LF,ME,LD,M,EPSON,Q1,SUM)
		WRITE(*,*)'X,YMIN,ZMIN,FF=',X,YMIN,ZMIN,FF
		IF(FF.LT.FMIN)THEN
		I=MM
		XMIN=X
		FMIN=FF
		ENDIF
		ENDDO

		FMIN=F(XMIN,YMIN,ZMIN,DD,D,AL,AN,RO,W0,DEDA,AS,CO,LF,ME,LD,M,EPSON,Q1,SUM)
		WRITE(*,*)'XMIN=',XMIN,'YMIN=',YMIN,'ZMIN=',ZMIN,'FMIN=',FMIN

        retlog=dlgset(dlg,idc_continue,.true.,dlg_enable)
        GO TO 200

100     retlog=dlgset(dlg,idc_RADIO_continue,.false.)
		IF(I.EQ.1)X1=X0+2.*DX
		IF(I.EQ.11)X0=X1-2.*DX
		IF(I.NE.1.AND.I.NE.11)THEN
		X0=XMIN-DX
		X1=XMIN+DX
		ENDIF
		IF(J.EQ.1)Y1=Y0+2.*DY
		IF(J.EQ.11)Y0=Y1-2.*DY
		IF(J.NE.1.AND.J.NE.11)THEN
		Y0=YMIN-DY
		Y1=YMIN+DY
		ENDIF
		IF(K.EQ.1)Z1=Z0+2.*DZ
		IF(K.EQ.11)Z0=Z1-2.*DZ
		IF(K.NE.1.AND.K.NE.11)THEN
		Z0=ZMIN-DZ
		Z1=ZMIN+DZ
		ENDIF
		WRITE(*,*)'X,Y,Z=',X0,X1,Y0,Y1,Z0,Z1
		GOTO 10
200     WRITE(TEXT,'(G12.4)') XMIN*180/PI
		retlog=dlgset(dlg,idc_edit_ALF2,trim(adjustl(TEXT)))
		WRITE(TEXT,'(G12.4)') YMIN
		retlog=dlgset(dlg,idc_edit_RLGL2,trim(adjustl(TEXT)))
		WRITE(TEXT,'(G12.4)') ZMIN*1.0E3
		retlog=dlgset(dlg,idc_edit_CG2,trim(adjustl(TEXT)))	
		WRITE(TEXT,'(G12.4)') EPSON
		retlog=dlgset(dlg,idc_edit_EPSON2,trim(adjustl(TEXT)))
        IF (LF.EQ.1) THEN
		SUM=-FMIN
		WRITE(TEXT,'(G12.4)') -FMIN
		retlog=dlgset(dlg,idc_edit_SUM2,trim(adjustl(TEXT)))
		WRITE(TEXT,'(G12.4)') Q1
		retlog=dlgset(dlg,idc_edit_Q,trim(adjustl(TEXT)))
		ELSE
		WRITE(TEXT,'(G12.4)') FMIN
		retlog=dlgset(dlg,idc_edit_Q,trim(adjustl(TEXT)))
		WRITE(TEXT,'(G12.4)') SUM
		retlog=dlgset(dlg,idc_edit_SUM2,trim(adjustl(TEXT)))
		ENDIF
		IF(LD.EQ.1) THEN
		TEXT='STATIC'
		ELSE
		TEXT='DYNAMIC'
		ENDIF
		WRITE(13,*)'¦Á ¦Ã Cg ¦Å W0 LOAD LEAKAGE LOADING'
		WRITE(13,4) XMIN*180/PI,YMIN,ZMIN,EPSON,W0,SUM,Q1,TEXT
4       FORMAT(20(G12.4))
        END SUBROUTINE DISPLAY

	

		FUNCTION F(ALF,RLGL,CG,DD,D,AL,AN,RO,W0,DEDA,AS,CO,LF,ME,LD,M,EPSON,Q1,SUM) RESULT(FF)
		REAL*4 X(121),Y(121),H(121,121),P(121,121),PD(121,121),HH(121)
		REAL*4 HXY(121,121),HXF(121,121),HXB(121,121),HYF(121,121),HYB(121,121)
		REAL*4,INTENT(OUT):: Q1,SUM,EPSON
		CHARACTER(10) TEXT
		DATA N,N1,N2/121,120,61/
		DATA PI/3.14159265/

!		WRITE(*,*) 'ALF,RLGL,CG=',ALF*180/PI,RLGL,CG
!       WRITE(*,*) 'DD,D,AL,EPSON=',DD,D,AL,EPSON
!		WRITE(*,*) 'LDF=',LD
		PI2=2.0*PI
		EDA=DEDA*RO
		R=D/2.0
		ALD=AL/D
		C=(D-DD)/2.0
		PESAI=(D-DD)/D
		K2=120/M
		DX=1.0/N1
		ALX=PI2*R
		ALY=AL
		RATIO=ALX/ALY
		ALFA=RATIO**2		
		OMIGA=AN*PI2/60.0
		U=OMIGA*R
		AKEXI=1.0
		IF(ALD.GT.1.0)AKEXI=(D/ALY)**1.5
		ALENDA=6.0*U*EDA*ALX/C**2	    	
		HMIN=C*(1.0-EPSON)
		TEMP=1./N1		
		DO I=1,N
		X(I)=(I-1)*TEMP
		Y(I)=-0.5+X(I)	
		ENDDO
 		Y(N2)=0.0
		K1=(RLGL/(1+RLGL))*K2
		IF(ABS(K1-(RLGL/(1+RLGL))*K2).GT.0.5) K1=K1+1
		CCG=CG/C
		DO I=1,N
		DO J=1,N
		P(I,J)=0.0
		ENDDO
		ENDDO

        IF (ME.EQ.1) THEN		
		CALL SUBH(N,N1,EPSON,X,Y,H,HH,PI2,K1,K2,CCG,RATIO,ALF,ALFA,N2,HXF,HXB,HYF,HYB,HXY)
		CALL SUBP(N,N1,PD,P,H,DX,HXF,HXB,HYF,HYB,HXY,LD)
		CALL SUBM(N,N1,PI,P,PX,PY,SUM1)
		SUM0=ALENDA*ALX*ALY*SUM1/(N*N)
		GO TO 30
		ELSE
		EPSON=0.5
		EPSON1=0.0
		EPSON2=1.0
 		GO TO 6
		ENDIF

6       CALL SUBH(N,N1,EPSON,X,Y,H,HH,PI2,K1,K2,CCG,RATIO,ALF,ALFA,N2,HXF,HXB,HYF,HYB,HXY)
		CALL SUBP(N,N1,PD,P,H,DX,HXF,HXB,HYF,HYB,HXY,LD)
		CALL SUBM(N,N1,PI,P,PX,PY,SUM1)
		SUM0=ALENDA*ALX*ALY*SUM1/(N*N)
!		WRITE(*,*) ME,W0,SUM0,EPSON
		IF(ABS((SUM0-W0)/W0).LE.0.005) GOTO 30
		IF(W0.LT.SUM0)THEN
		EPSON2=EPSON
		EPSON=0.5*(EPSON1+EPSON)
		ELSE
		EPSON1=EPSON
		EPSON=0.5*(EPSON+EPSON2)
		ENDIF

        GOTO 6

30	    E=EPSON*C		    	
		HMIN=C*(1.0-EPSON)
	    Q1=0.0					
        DO I=1,N1
		Q=H(I,1)**3*(4.0*P(I,2)-P(I,3))+H(I,N)**3*(4.0*P(I,N-1)-P(I,N-2))
		IF(Q.LT.0.0)Q=0.0
        Q1=Q1+Q
        ENDDO
        Q1=Q1*0.25*U*ALY*C
        Q=0.5*U*ALY*(C*(1+EPSON)+CG*K1/K2)
		DDT=0.0
        AI=180.0*ATAN(PY/PX)/PI	
		PA=SUM0/(D*ALY)
		FS=PI*EDA*OMIGA/PESAI/PA+0.55*PESAI*AKEXI
		DT=(FS/PESAI)*PA/(CO*RO*Q/(PESAI*U*ALY*D)+PI*AS/PESAI/U)
		IF(LD.EQ.1) THEN
		TEXT='STATIC'
		ELSE
		TEXT='DYNAMIC'
		ENDIF
		WRITE(14,*)'¦Á ¦Ã Cg ¦Å e W0 LOAD FLUX LEAKAGE ¡÷T LOADING'
		WRITE(14,4) ALF*180/PI,RLGL,CG,EPSON,E,W0,SUM0,Q,Q1,DT,TEXT
40		FORMAT(122(E12.6,1X))
4       FORMAT(20(G12.4))
        SUM=SUM0
!		WRITE(*,*) 'Q1,SUM=',Q1,SUM
        IF(LF.EQ.1) THEN
        FF=-SUM0
		ELSE
        FF=Q1
		ENDIF
!		WRITE(*,*)'FF=',FF
		RETURN
		END        

 		SUBROUTINE SUBH(N,N1,EPSON,X,Y,H,HH,PI2,K1,K2,CCG,RATIO,ALF,ALFA,N2,HXF,HXB,HYF,HYB,HXY)
		REAL*4 X(N),Y(N),H(N,N),HH(N),HXF(N,N),HXB(N,N),HYF(N,N),HYB(N,N),HXY(N,N)
		DO I=1,N
		DO J=1,N
		H(I,J)=1.0+EPSON*COS(PI2*X(I))
		ENDDO	
		ENDDO
		IF(K1.EQ.0)GOTO 20
		DO I=1,N
		HH(I)=0.0
		ENDDO		
		DO I=1,N-K2,K2    
		DO L=1,K1
		HH(I+L-1)=CCG
		ENDDO
		ENDDO	
		HH(N)=HH(1)
		TEMP=1./RATIO
		DO J=1,N
		DO I=1,N1
		I1=I+ABS(J-N2)*(1./TAN(ALF/2))*TEMP
		IF(I1.GT.N)THEN
		I1=I1-N1
		ENDIF
		H(I,J)=H(I,J)+HH(I1)
		ENDDO
		H(N,J)=H(1,J)	
		ENDDO
20		DO I=1,N
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

		SUBROUTINE SUBP(N,N1,PD,P,H,DX,HXF,HXB,HYF,HYB,HXY,LD)
		REAL*4 X(N),Y(N),PD(N,N),P(N,N),H(N,N),HXF(N,N),HXB(N,N),HYF(N,N),HYB(N,N),HXY(N,N)
		DO I=1,N
		DO J=1,N
		PD(I,J)=P(I,J)
		ENDDO
		ENDDO
		IK=0
		TEMP=0.5*DX		
10		C1=0.0
		DO I=1,N1
		I1=I-1
		I2=I+1
		IF(I1.EQ.0)I1=N1
		IF(I2.EQ.N)I2=1
		DO J=2,N1
		P(I,J)=(HXF(I,J)*P(I2,J)+HXB(I,J)*P(I1,J)+HYF(I,J)*P(I,J+1)+HYB(I,J)*P(I,J-1)-LD*TEMP*(H(I2,J)-H(I1,J)))*HXY(I,J)
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
!		WRITE(*,*) 'IK,ER=',IK,C1
		RETURN
		END SUBROUTINE SUBP
 
        SUBROUTINE SUBM(N,N1,PI,P,PX,PY,SUM1)
		DIMENSION P(N,N)
		PX=0.0
		PY=0.0
		TEMP=PI/60.0
		DO I=1,N1
		AI=(I-1)*TEMP
		DO J=1,N	
		PX=PX-P(I,J)*COS(AI)
		PY=PY+P(I,J)*SIN(AI)
		ENDDO
		ENDDO
		SUM1=SQRT(PX*PX+PY*PY)
        RETURN 
		END	SUBROUTINE SUBM



		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
