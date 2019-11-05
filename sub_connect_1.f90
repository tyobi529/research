	module sub_connect_1

	use globals
	use subprogs

	implicit none
	contains

!     ====================================
!           CONNECTION (EXCHANGE DISCHARGE)
!     ====================================
     SUBROUTINE CONNECT_1 ! GROUND <-> STORM DRAIN BOX
     real(8) dv, hd, HM, DL, AG, B0, CDW, CDO, L1, L2, CNCD, ASDB, CDME
     INTEGER i, j, K, ME, n, id, mh, ist
     INTEGER IC, JC
    L1=0.5D0
    L2=0.5D0
    CNCD=1.0D0

		! グレーチングの周囲の長さ
    DL=2.0D0*(L1+L2)*CNCD            ! CONGRUENT SIZE
		! グレーチングの断面積
    AG=L1*L2*CNCD               ! CONTRUENT SIZE
		! 最小グレーチング幅
	B0=MIN(L1,L2)
	CDW=0.48D0
	CDO=0.57D0
		! 雨水ますの断面積
    ASDB=SDB_X*SDB_W

		! 道路メッシュの数だけループ
    DO I=1, SDBR

    IC=CN_ME(I)
		! 道路メッシュの面積/10
    CDME=SMESH(CN_ME(I))/10.0D0 ! NUMBER STORM DRAINS PER UNIT AREA (170M^2) , ?? ???E170) ??? ??E
!    WRITE(*,*) I, IC

			IF(BS_SDB(I)>=BASEO(IC)) THEN
			   DV=0.0D0
!	  -------------------------------
!            STORM DRAIN BOX -> GROUND
!	  -------------------------------
			ELSEIF(H_SDB(I)+BS_SDB(I)>=H(IC)+BASEO(IC)) THEN

				HD=H_SDB(I)+BS_SDB(I)-BASEO(IC)
				HM=H(IC)

					IF((HD-HM)<B0/2.0D0)THEN
						DV=-CDW*0.6666667D0*DL*SQRT(2.0D0*GG)*((HD-HM)**1.5D0)*CDME
					ELSE
						DV=-CDO*AG*SQRT(2.0D0*GG*(HD-HM))*CDME
					ENDIF
			   	        DV=MAX(DV, -H_SDB(I)*ASDB/DT2)
!     -------------------------------
!            GROUND -> STORM DRAIN BOX
!     -------------------------------
			ELSEIF(H_SDB(I)+BS_SDB(I)>BASEO(IC))THEN

				HD=H_SDB(I)+BS_SDB(I)-BASEO(IC)
				HM=H(IC)

					IF((HM-HD)<B0/2.0D0)THEN
						DV=CDW*0.66667D0*DL*SQRT(2.0D0*GG)*((HM-HD)**1.5D0)*CDME
					ELSE
						DV=CDO*AG*SQRT(2.0D0*GG*(HM-HD))*CDME
					ENDIF
			        DV=MIN(DV,HM*SMESH(IC)/DT2)
			ELSE
				HD=H_SDB(I)+BS_SDB(I)-BASEO(IC)
				HM=H(IC)
				
					IF(HM<B0/2.0D0)THEN
						DV=CDW*0.6666667D0*DL*SQRT(2.0D0*GG)*(HM**1.5D0)*CDME
					ELSE
						DV=CDO*AG*SQRT(2.0D0*GG*HM)*CDME
					ENDIF
					DV=MIN(DV,HM*SMESH(IC)/DT2)
		    ENDIF

		QL_SDB(I)=QL_SDB(I)+DV/SDB_X
		! 雨水ます→地表への流速？
		QLME(IC)=QLME(IC)-DV/SMESH(IC)

			! 交換流量（地表→雨水枡を正）
	    AC_CNQ=AC_CNQ+DV	! ACCUMULATED EXCHANGE DISCHARGE

	    IF(DV<0.0D0)THEN
					! 雨水ますの流量？
	        AC_QM=AC_QM+DV
	    ELSE
	        AC_QP=AC_QP+DV
	    ENDIF

ENDDO

END SUBROUTINE CONNECT_1

end module sub_connect_1
