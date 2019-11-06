	module sub_connect_2

	use globals
	use subprogs

	implicit none
	contains

!     ====================================
!           CONNECTION (EXCHANGE DISCHARGE)
!     ====================================
     SUBROUTINE CONNECT_2 ! PIPE<-> STORM DRAIN BOX
     real(8) dv, hd, HM, DL, AG, B0, CDW, CDO, L1, L2, CNCD, ASDB
     INTEGER i, j, K, ME, n, id, mh, ist
     INTEGER IC, JC
		! 取付管直径
    L1=0.2D0
    L2=0.2D0
    CNCD=1.0D0

		! 取付管の円周
    DL=PI*L1*CNCD            ! CONGRUENT SIZE
		! 取付管の断面積
    AG=(PI*L1**2.0D0)/4.0D0*CNCD               ! CONTRUENT SIZE
		! 取付管の半径？
	B0=L1/2.0D0
	CDW=0.48D0
	CDO=0.57D0
		! 雨水ますの断面積
    ASDB=SDB_X*SDB_W

! 　　道路メッシュの数だけループ
    DO I=1, SDBR
		! 道路メッシュから最も近い下水道管の番号
    IC=CN_I(CN_ME(I)) ; JC=CN_J(CN_ME(I))

			IF(BS_SW(IC, JC)+ DD2_SW(IC)>=BS_SDB(I)) THEN
			   DV=0.0D0
!	  -------------------------------
!           PIPE -> STORM DRAIN BOX
!	  -------------------------------
			ELSEIF(H_SW(IC, JC)+BS_SW(IC, JC)>=H_SDB(I)+BS_SDB(I)) THEN

				HD=H_SW(IC, JC)+BS_SW(IC, JC)-BS_SDB(I)
				HM=H_SDB(I)

					IF((HD-HM)<B0/2.0D0)THEN
						DV=-CDW*0.6666667D0*DL*SQRT(2.0D0*GG)*((HD-HM)**1.5D0)
					ELSE
						DV=-CDO*AG*SQRT(2.0D0*GG*(HD-HM))
					ENDIF
			   	    DV=MAX(DV/NM_METOSW(IC,JC), -BSL(IC)*( H_SW(IC,JC)-DD2_SW(IC) )*DX_SW(IC)/DT2)
!			   	    DV=MAX(DV/NM_METOSW(IC,JC), -A_SW(IC,JC)*DX_SW(IC)/DT2)
!     -------------------------------
!            DRAIN BOX -> PIPE
!     -------------------------------
			ELSEIF(H_SW(IC, JC)+BS_SW(IC, JC)>BS_SDB(I))THEN

				HD=H_SW(IC, JC)+BS_SW(IC, JC)-BS_SDB(I)
				HM=H_SDB(I)

					IF((HM-HD)<B0/2.0D0)THEN
						DV=CDW*0.66667D0*DL*SQRT(2.0D0*GG)*((HM-HD)**1.5D0)
					ELSE
						DV=CDO*AG*SQRT(2.0D0*GG*(HM-HD))
					ENDIF
			        DV=MIN(DV,HM*ASDB/DT2)
			ELSE
				HD=H_SW(IC, JC)+BS_SW(IC, JC)-BS_SDB(I)
				HM=H_SDB(I)

					IF(HM<B0/2.0D0)THEN
						DV=CDW*0.6666667D0*DL*SQRT(2.0D0*GG)*(HM**1.5D0)
					ELSE
						DV=CDO*AG*SQRT(2.0D0*GG*HM)
					ENDIF

					DV=MIN(DV,HM*ASDB/DT2)
		    ENDIF

		! 単位長さあたりの流量？
		QL_SW(IC, JC)=QL_SW(IC, JC)+DV/DX_SW(IC)
		! 流速
		QL_SDB(I)=QL_SDB(I)-DV/SDB_X

ENDDO

END SUBROUTINE CONNECT_2

end module sub_connect_2
