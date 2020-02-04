		PROGRAM HBFA
		USE DFLOGM
		INCLUDE 'RESOURCE.FD'
		CALL DoDialog()		
		STOP
		END PROGRAM

		SUBROUTINE DoDialog() 
		USE DFLOGM  
		INCLUDE 'RESOURCE.FD'
		INTEGER retint,LL
		LOGICAL retlog  
		TYPE (dialog) dlg  
		EXTERNAL DISPLAY  
		EXTERNAL CLEAR
		EXTERNAL CHOOSE
		EXTERNAL CHOOSE1,CHOOSE2
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
 		 retlog=dlgset(dlg,idc_EDIT_ALF,"30")
		 retlog=dlgsetsub(dlg,idc_EDIT_ALF,CLEAR,dlg_change)
		 retlog=dlgset(dlg,idc_EDIT_RLGL,"1.0")
		 retlog=dlgsetsub(dlg,idc_EDIT_RLGL,CLEAR,dlg_change)
		 retlog=dlgset(dlg,idc_EDIT_CG,"0.004")	
		 retlog=dlgsetsub(dlg,idc_EDIT_CG,CLEAR,dlg_change)
		 retlog=dlgset(dlg,idc_EDIT_DEDA,"24.5")
		 retlog=dlgsetsub(dlg,idc_EDIT_DEDA,CLEAR,dlg_change)	
		 retlog=dlgset(dlg,idc_EDIT_RO,"0.945")	
		 retlog=dlgsetsub(dlg,idc_EDIT_RO,CLEAR,dlg_change)
		 retlog=dlgset(dlg,idc_EDIT_AS,"80")
		 retlog=dlgsetsub(dlg,idc_EDIT_AS,CLEAR,dlg_change)	
		 retlog=dlgset(dlg,idc_EDIT_CO,"1800")
		 retlog=dlgsetsub(dlg,idc_EDIT_CO,CLEAR,dlg_change)
		 retlog=dlgsetsub(dlg,idc_EDIT_SUM,CLEAR,dlg_change)
		 retlog=dlgsetsub(dlg,idc_EDIT_EPSON,CLEAR,dlg_change)	
		 retlog=dlgsetsub(dlg,idc_CALCULATE,DISPLAY)
		 retlog=dlgsetsub(dlg,idc_CLEAR,CLEAR)	
		 retlog=dlgset(dlg,idc_EDIT_EPSON,.TRUE.)
		 retlog=dlgset(dlg,idc_EDIT_SUM,.FALSE.)
		 retlog=dlgsetsub(dlg,IDC_RADIO_EPSON,CHOOSE)
		 retlog=dlgsetsub(dlg,IDC_RADIO_SUM,CHOOSE)	
		 retlog=dlgset(dlg,idc_CHECK_STATIC,.TRUE.,DLG_STATE)
		 retlog=dlgset(dlg,idc_CHECK_DYNAMIC,.FALSE.,DLG_STATE)
		 retlog=dlgsetsub(dlg,IDC_CHECK_STATIC,CHOOSE1)
		 retlog=dlgsetsub(dlg,IDC_CHECK_DYNAMIC,CHOOSE2)
		 retint=DLGMODAL(dlg)
		 CALL DLGUNINIT(dlg)
		END IF
		LL=0
		RETURN
		END SUBROUTINE DoDialog
		
		SUBROUTINE CHOOSE(dlg,control_name,calltype)
		USE DFLOGM
		INCLUDE 'resource.fd'
		TYPE (dialog) dlg
		LOGICAL retlog,PUSHED_state
		retlog = DLGGET (dlg, IDC_RADIO_EPSON, pushed_state)
        IF (PUSHED_STATE) THEN
	    retlog=dlgset(dlg,idc_EDIT_SUM,.false.,dlg_enable)
        retlog=dlgset(dlg,idc_EDIT_EPSON,.TRUE.,dlg_enable)
		ELSE
	    retlog=dlgset(dlg,idc_EDIT_EPSON,.false.,dlg_enable)
		retlog=dlgset(dlg,idc_EDIT_SUM,.TRUE.,dlg_enable)
        ENDIF
		END SUBROUTINE CHOOSE

		SUBROUTINE CHOOSE1(dlg,control_name,calltype)
		USE DFLOGM
		INCLUDE 'resource.fd'
		TYPE (dialog) dlg
		LOGICAL retlog,PUSHED_state
		retlog=dlgset(dlg,idc_CHECK_STATIC,.TRUE.,DLG_STATE)
        retlog=dlgset(dlg,idc_CHECK_DYNAMIC,.FALSE.,DLG_STATE)
		retlog=dlgset(dlg,idc_EDIT_SUM2,"   ")
		retlog=dlgset(dlg,idc_EDIT_EPSON2,"   ")
        retlog=dlgset(dlg,idc_EDIT_E,"   ")
		retlog=dlgset(dlg,idc_EDIT_Q1,"   ")
		retlog=dlgset(dlg,idc_EDIT_Q,"   ")
		retlog=dlgset(dlg,idc_EDIT_DT,"    ")
		retlog=dlgset(dlg,idc_EDIT_AI,"    ")
		END SUBROUTINE CHOOSE1

		SUBROUTINE CHOOSE2(dlg,control_name,calltype)
		USE DFLOGM
		INCLUDE 'resource.fd'
		TYPE (dialog) dlg
		LOGICAL retlog,PUSHED_state
		retlog=dlgset(dlg,idc_CHECK_DYNAMIC,.TRUE.,DLG_STATE)
		retlog=dlgset(dlg,idc_CHECK_STATIC,.FALSE.,DLG_STATE)
		retlog=dlgset(dlg,idc_EDIT_SUM2,"   ")
		retlog=dlgset(dlg,idc_EDIT_EPSON2,"   ")
        retlog=dlgset(dlg,idc_EDIT_E,"   ")
		retlog=dlgset(dlg,idc_EDIT_Q1,"   ")
		retlog=dlgset(dlg,idc_EDIT_Q,"   ")
		retlog=dlgset(dlg,idc_EDIT_DT,"    ")
		retlog=dlgset(dlg,idc_EDIT_AI,"    ")
		END SUBROUTINE CHOOSE2
		
		SUBROUTINE CLEAR(dlg,control_name,calltype)
        USE DFLOGM
		INCLUDE 'resource.fd'
		TYPE (dialog) dlg
		LOGICAL retlog
		INTEGER retint,control_name,calltype 
		retlog=dlgset(dlg,idc_EDIT_SUM2,"   ")
		retlog=dlgset(dlg,idc_EDIT_EPSON2,"   ")
        retlog=dlgset(dlg,idc_EDIT_E,"   ")
		retlog=dlgset(dlg,idc_EDIT_Q1,"   ")
		retlog=dlgset(dlg,idc_EDIT_Q,"   ")
		retlog=dlgset(dlg,idc_EDIT_DT,"    ")
		retlog=dlgset(dlg,idc_EDIT_AI,"    ")
		RETURN 
		END SUBROUTINE CLEAR

        SUBROUTINE DISPLAY(dlg,control_name,calltype)
		USE dflogm
		INCLUDE 'resource.fd'
		TYPE (dialog) dlg
		LOGICAL retlog
		INTEGER retint,control_name,calltype 					
		INTEGER N,N1,N2,M,K1,K2,I,J,K,I1,IK,L	  
		REAL*4 X(121),Y(121),H(121,121),P(121,121),PD(121,121),HH(121)
		REAL*4 HXY(121,121),HXF(121,121),HXB(121,121),HYF(121,121),HYB(121,121)
		REAL*4 PI,EPSON,U,EDA,ALD,AL,AN,DD,D,R,ALF,RLGL,CG,CCG,PI2,C,E,DX
		REAL*4 ALX,ALY,RATIO,ALFA,ALENDA,PESAI
		REAL*4 CO,RO,AS,DEDA
		REAL*4 OMIGA,TEMP,TEMP1,HMIN,SUM1,Q,Q1
		REAL*4 A,B,SUM
		REAL*4 PX,PY,AI,KEXI,PA,F,DT,C1
		LOGICAL PUSHED_STATE
		DATA PI/3.14159265/
		CHARACTER(256) text,DG 
		OPEN(8,FILE='pressure.DAT',STATUS='UNKNOWN')
		OPEN(9,FILE='height.DAT',STATUS='UNKNOWN')
		OPEN(13,FILE='result.DAT',STATUS='UNKNOWN')
2       FORMAT(20X,A12,I2.2,':',I2.2,':',I2.2,'.',I3.3)
					
		retlog=dlgget(dlg,idc_edit_DD,text)
		READ(text,*) DD
		retlog=dlgget(dlg,idc_edit_D,text)
		read(text,*) D
		retlog=dlgget(dlg,idc_edit_AL,text)
		read(text,*) AL
		retlog=dlgget(dlg,idc_edit_AN,text)
		read(text,*) AN
	    retlog=dlgget(dlg,idc_edit_M,text)
		read(text,*) M
		retlog=dlgget(dlg,idc_edit_ALF,text)
		read(text,*) ALF
		retlog=dlgget(dlg,idc_edit_RLGL,text)
		read(text,*) RLGL
	    retlog=dlgget(dlg,idc_edit_CG,text)
		read(text,*) CG
		retlog=dlgget(dlg,idc_EDIT_DEDA,text)
		read(text,*) DEDA
		retlog=dlgget(dlg,idc_EDIT_RO,text)
		read(text,*) RO 	
		retlog=dlgget(dlg,idc_EDIT_AS,text)
		read(text,*) AS	
		retlog=dlgget(dlg,idc_EDIT_CO,text)	
        read(text,*) CO
        retlog = DLGGET (dlg, IDC_CHECK_STATIC, pushed_state)
        IF (PUSHED_STATE) THEN
		LD=1
		ELSE 
		LD=-1
		ENDIF
!		WRITE(*,*)'LD=',LD
!		WRITE(*,*)'DD,D,AL,AN='		
!		WRITE(*,*) DD,D,AL,AN
!		WRITE(*,*)'EPSON,RLGL,ALF,CG='		
!		WRITE(*,*) EPSON,RLGL,ALF,CG
!		WRITE(*,*)'DEDA,RO,CO,AS='
!		WRITE(*,*) DEDA,RO,CO,AS	

		EDA=DEDA*RO*1.0E-3
		RO=RO*1.0E3
		DD=DD*1.0E-3
		D=D*1.0E-3
		AL=AL*1.0E-3
		CG=CG*1.0E-3
		N=121
		N1=N-1
		N2=N1/2+1		
		PI2=2.0*PI
		R=D/2
		ALD=AL/D
		C=(D-DD)/2
		PESAI=(D-DD)/D
		ALF=ALF*PI/180.0
		K2=120/M
		K1=(RLGL/(1+RLGL))*K2
		IF(ABS(K1-(RLGL/(1+RLGL))*K2).GT.0.5) K1=K1+1
		CCG=CG/C
		DX=1.0/N1
		ALX=PI2*R
		ALY=AL
		RATIO=ALX/ALY
		ALFA=RATIO**2		
		OMIGA=AN*PI2/60.0
		U=OMIGA*R
        TEMP=1./N1		
		DO I=1,N
		X(I)=(I-1)*TEMP
		Y(I)=-0.5+X(I)	
		ENDDO
		Y(N2)=0.0			
		DO I=1,N
		DO J=1,N
		P(I,J)=0.0
		ENDDO
		ENDDO

        retlog = DLGGET (dlg, IDC_RADIO_EPSON, pushed_state)
        IF (PUSHED_STATE) THEN
		retlog=dlgget(dlg,idc_edit_EPSON,text)
		read(text,*) EPSON
		WRITE(*,*) 'EPSON=',EPSON
		CALL SUBH
		CALL SUBP
		CALL SUBM
		GO TO 70
		ELSE 
		GO TO 50
		ENDIF

50     	retlog=dlgget(dlg,idc_EDIT_SUM,text)	
        read(text,*) SUM
		SUM0=SUM*(N*N)*C**2/(6.0*U*EDA*ALX*ALX*ALY)
		EPSON=0.5
		EPSON1=0.0
		EPSON2=1.0
		WRITE(*,*)'LOAD=',SUM
60		CALL SUBH
		CALL SUBP
		CALL SUBM
		IF(ABS((SUM1-SUM0)/SUM0).LE.0.005)GOTO 70
		IF(SUM1.GT.SUM0)THEN
		EPSON2=EPSON
		EPSON=0.5*(EPSON1+EPSON)
		ELSE
		EPSON1=EPSON
		EPSON=0.5*(EPSON+EPSON2)
		ENDIF
		GOTO 60

70		E=EPSON*C*1.0E3		    	
		HMIN=C*(1.0-EPSON)
        AI=180.0*ATAN(PY/PX)/PI	
		
		Q1=0.0					
		Q=0.0
        DO I=1,N1
		Q=H(I,1)**3*(4.0*P(I,2)-P(I,3))+H(I,N)**3*(4.0*P(I,N-1)-P(I,N-2))
		IF(Q.LT.0.0)Q=0.0
        Q1=Q1+Q
        ENDDO
        Q1=Q1*0.25*U*ALY*C
        Q=0.5*U*ALY*(C*(1+EPSON)+CG*K1/K2)
		DDT=0.0

		ALENDA=6.0*U*EDA*ALX/C**2
		SUM1=ALENDA*ALX*ALY*SUM1/(N*N)
		KEXI=1.0
		IF(ALD.GT.1.0)KEXI=(D/ALY)**1.5
		PA=SUM1/(D*ALY)
		F=PI*EDA*OMIGA/PESAI/PA+0.55*PESAI*KEXI
		DT=(F/PESAI)*PA/(CO*RO*Q/(PESAI*U*ALY*D)+PI*AS/PESAI/U)

!		WRITE(*,*)DT,F,PA,CO,RO,AS
		WRITE(8,40)Y(1),(Y(I),I=1,N)
		DO I=1,N
		WRITE(8,40)X(I)*360,(P(I,J)*ALENDA,J=1,N)
		ENDDO
		WRITE(9,40)Y(1),(Y(I),I=1,N)
	    DO I=1,N
		WRITE(9,40)X(I)*360,(H(I,J)*C,J=1,N)
		ENDDO
40		FORMAT(122(E12.6,1X))
4        FORMAT(10(G12.4))
!       WRITE(*,*)'EPSON,E=',EPSON,E
!		WRITE(*,*)'M,CG,AL=',M,CG,AL
!		WRITE(*,*)'SUM1,AI,Q,Q1=',SUM1,AI,Q,Q1
		IF(LL.EQ.0)	WRITE(13,*)'¦Å e W ¦× Flux Leakage ¡÷T Loading'
		LL=LL+1
		IF(LD.EQ.1) THEN
		TEXT='STATIC'
		ELSE
		TEXT='DYNAMIC'
		ENDIF
		WRITE(13,4)EPSON,E,SUM1,AI,Q,Q1,DT,TEXT
!       WRITE(*,*)'DD,D,C,PESAI=',DD,D,C,PESAI
        
		WRITE(TEXT,'(G12.4)') EPSON
		retlog=dlgset(dlg,idc_edit_EPSON2,trim(adjustl(TEXT)))		
		WRITE(TEXT,'(G12.4)') SUM1
		retlog=dlgset(dlg,idc_edit_SUM2,trim(adjustl(TEXT)))
		write(TEXT,'(G12.4)') AI
        retlog=dlgset(dlg,idc_edit_AI,trim(adjustl(TEXT)))
		write(TEXT,'(G12.4)') Q
		retlog=dlgset(dlg,idc_EDIT_Q,trim(adjustl(text))) 
		WRITE(TEXT,'(G12.4)') DT
		retlog=dlgset(dlg,idc_EDIT_DT,trim(adjustl(text)))
		WRITE(TEXT,'(G12.4)') Q1
		retlog=dlgset(dlg,idc_EDIT_Q1,trim(adjustl(text)))
		WRITE(TEXT,'(G12.4)') E
		retlog=dlgset(dlg,idc_EDIT_E,trim(adjustl(text)))

        CONTAINS
		SUBROUTINE SUBH
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
		DO I=1,N
		ENDDO
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

		SUBROUTINE SUBP
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
 !   	WRITE(*,*) 'ER=',IK,C1
		RETURN
		END SUBROUTINE SUBP
 
        SUBROUTINE SUBM
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
		END SUBROUTINE SUBM
		END SUBROUTINE DISPLAY

