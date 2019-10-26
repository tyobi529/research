	Module sub_connection
	
	use globals
	use subprogs
	
	implicit none
	contains
	
!	-------------------------
!	 directly connected case
!	-------------------------	

	subroutine connection
	
     real(8) dv, hd, HM, DL, AG, B0, CDW, CDO, L1, L2
     INTEGER i, j, K, ME, n, id, mh, ist
     INTEGER IC, JC
    L1=0.5D0
    L2=0.5D0

  	
    DL=2.0D0*(L1+L2)           ! CONGRUENT SIZE
    AG=L1*L2              ! CONTRUENT SIZE
	B0=MIN(L1,L2)
	CDW=0.48D0
	CDO=0.57D0


    DO ME=1, MESH

    IF(MARK(ME)==1) THEN
    IC=CN_I(ME) ; JC=CN_J(ME)
    
			IF(BS_SW(IC, JC)+dd2_sw(ic)>=BASEO(ME)) THEN
			   DV=0.0D0
!	  -------------------------------
!            pipe -> surface
!	  -------------------------------
			ELSEIF(H_SW(IC, JC)+BS_SW(IC, JC)>=H(ME)+BASEO(ME)) THEN
				
				HD=H_SW(IC, JC)+BS_SW(IC, JC)-BASEO(ME)
				HM=H(ME)
				
					IF((HD-HM)<B0/2.0D0)THEN
						DV=-CDW*0.6666667D0*DL*SQRT(2.0D0*GG)*((HD-HM)**1.5D0)
					ELSE
						DV=-CDO*AG*SQRT(2.0D0*GG*(HD-HM))
					ENDIF
			   	    DV=MAX(DV/NM_METOSW(IC,JC), -a_sw(ic,jc)*DX_SW(IC)/DT2)
!     -------------------------------
!             surface -> pipe
!     -------------------------------
			ELSEIF(H_SW(IC, JC)+BS_SW(IC, JC)>BASEO(ME))THEN
				
				HD=H_SW(IC, JC)+BS_SW(IC, JC)-BASEO(ME)
				HM=H(ME)
				
					IF((HM-HD)<B0/2.0D0)THEN
						DV=CDW*0.66667D0*DL*SQRT(2.0D0*GG)*((HM-HD)**1.5D0)
					ELSE
						DV=CDO*AG*SQRT(2.0D0*GG*(HM-HD))
					ENDIF
			        DV=MIN(DV,H(ME)*SMESH(ME)/DT2)
			ELSE
				
				HD=H_SW(IC, JC)+BS_SW(IC, JC)-BASEO(ME)
				HM=H(ME)
				
					IF(HM<B0/2.0D0)THEN
						DV=CDW*0.6666667D0*DL*SQRT(2.0D0*GG)*(HM**1.5D0)
					ELSE
						DV=CDO*AG*SQRT(2.0D0*GG*HM)
					ENDIF
					CHECK(IC, JC)=4
					DV=MIN(DV,HM*SMESH(ME)/DT2)
		    ENDIF

		QL_SW(IC, JC)=QL_SW(IC, JC)+DV/DX_SW(IC)
		QLME(ME)=QLME(ME)-DV/SMESH(ME)
		
	ENDIF
		    IF(DV<0.0D0)THEN
	        AC_QM=AC_QM+DV
	    ELSE
	        AC_QP=AC_QP+DV
	    ENDIF
		AC_CNQ = AC_CNQ+dv
ENDDO

1000 FORMAT (F7.2, 3X, I7, 3X, F12.9, 3X, F12.9, 3X, I5)
1010 FORMAT (F20.10, 3X,F20.10)
1020 FORMAT (I3, 3X, I7, 3X, I7, 3X, I7, 3X, 5(F20.6, 3X))
1030 FORMAT ('TIME(S)=', F8.2)
1040 FORMAT ('MARK(ME)  ME   CN_I(ME)   CN_J(ME)  DV   H_SW(CN_I(ME) CN_J(ME))  HD    HM')

END SUBROUTINE connection
	
end module sub_connection	