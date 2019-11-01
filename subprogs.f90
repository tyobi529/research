!     #######################################################
!     ##                                                   ##
!     ##                   sub programs                    ##
!     ##                                                   ##
!     #######################################################
!
module subprogs
  use globals
  implicit none
contains
!

!     =======================================================
!          STORM DRAIN BOX GEO DATA GENERATION
!     =======================================================
    SUBROUTINE STORM_BOX

    INTEGER ME, CI, CJ, NM
    REAL(8) DIST, SDB_V, SDB_H

    ! DISTANCE CALCULATION

!     ===========================
!           DRAIN BOX
!     ===========================

        SDB_X = 5.D0 ! LENGTH OF DRAIN BOX 20m
        SDB_W = 0.5D0 ! WIDTH OF DRAIN BOX 0.5m

    NM=0
      DO ME=1, MESH
    	  IF(MARK(ME)==1)THEN
		    NM=NM+1
		    CI=CN_I(ME) ; CJ=CN_J(ME)

        ! メッシュ中心と最寄りの下水道管の距離？
		    DIST = SQRT((XMESH(ME)-X_M(CI,CJ))**2.0D0+(YMESH(ME)-Y_M(CI,CJ))**2.0D0)
        ! 取付管の容積？
		    SDB_V = DIST*(PI*0.2D0**2.0D0)/4.0D0
        ! 雨水ます内の水深？
		    SDB_H = SDB_V/(SDB_X*SDB_W)
		    
!		    BS_SDB(NM)=BASEO(ME)-SDB_H   !SDB_H 0.3m
		    BS_SDB(NM)=BASEO(ME)-0.3d0

		    CN_ME(NM)=INT(ME)

	      ENDIF

	  ENDDO
	        SDBR=NM
1000 FORMAT(F12.3, 3X, F12.3, 5X, 5(F10.5, 3X) )

END SUBROUTINE STORM_BOX



!     =======================================================
!                   MANHOLE MESH GENERATION
!     =======================================================
    SUBROUTINE MH_MESH

    INTEGER I, J, K
    REAL(8) R_MH

        MH_NO=0

        DO I=1, MNHL
            IF (MARK_MH_CHECK(I)==1)THEN
            R_MH=SQRT(AREA_MH(I)/PI)

            DO J=1,4
                XX_MH(I,J) = X_MH(I)+R_MH*COS(2*PI*0.25D0*INT(J))
                YY_MH(I,J) = Y_MH(I)+R_MH*SIN(2*PI*0.25D0*INT(J))
                MH_NO=MH_NO+1
            ENDDO
            ENDIF
       ENDDO

       MH_EL= MH_NO/4

END SUBROUTINE MH_MESH

!     =======================================================
!                PUMPING STATION MESH GENERATION
!     =======================================================
    SUBROUTINE PM_MESH

    INTEGER I, J, K
    REAL(8) R_PM

        PM_NO=0

        DO I=1, IPUMP
            R_PM=SQRT(AREA_MH(MH_PM(I))/PI)

            DO J=1,4
                XX_PM(I,J) = X_MH(MH_PM(I))+R_PM*10*COS(2*PI*0.25D0*INT(J))
                YY_PM(I,J) = Y_MH(MH_PM(I))+R_PM*10*SIN(2*PI*0.25D0*INT(J))
                PM_NO=PM_NO+1
            ENDDO
       ENDDO

       PM_EL= PM_NO/4

END SUBROUTINE PM_MESH




!   =======================================================
!         REDUCTION OF CONVECTIVE ACCELERATION TERM
!   =======================================================
    SUBROUTINE RCAT
    INTEGER I, J
    REAL(8) VV(ISW,JSW), FR(ISW,JSW), FR_A(ISW,JSW), FR_R(ISW,JSW), FR_L(ISW,JSW)
    REAL(8) F0, F1, BETA

    F0=0.9D0
    F1=1.1D0
    BETA=2.0D0

    DO I=1, ISWR
        DO J=1, JSWR(I)

            IF(H_SW(I,J)>TH)THEN
                VV(I,J) = Q_SW(I,J) / A_SW(I,J)
                FR(I,J) = VV(I,J) / SQRT(GG*H_SW(I,J))

                 IF(J==1) THEN
                    FR_R(I,J)=(FR(I,J)+FR(I,J+1)) /2.0D0
                    FR_L(I,J)=FR(I,J)
                 ELSEIF(J==JSWR(I))THEN
                    FR_R(I,J)=FR(I,J)
                    FR_L(I,J)=(FR(I,J-1)+FR(I,J)) / 2.0D0
                 ELSE
                    FR_R(I,J)=(FR(I,J)+FR(I,J+1)) / 2.0D0
                    FR_L(I,J)=(FR(I,J-1)+FR(I,J)) / 2.0D0
                 ENDIF

                   FR_A(I,J) = (FR_R(I,J)+FR_L(I,J)) / 2.0D0

                IF(FR_A(I,J)>F0 .AND. FR_A(I,J)<F1)THEN
                    ALPHA(I,J)=(F1-FR_A(I,J)**BETA) / (F1-F0**BETA)
                ELSEIF(FR_A(I,J)<F0)THEN
                    ALPHA(I,J) = 1.0D0
                ELSE
                    ALPHA(I,J) = 0.0D0
                ENDIF
            ENDIF
         ENDDO
     ENDDO

     END SUBROUTINE RCAT


!
!     ==========================================================
!        CONTINUITY EQUATION FOR STORM DRAIN BOX
!     ==========================================================
        SUBROUTINE SUISIN_BOX

        INTEGER I

        DO I=1, SDBR

        H_SDB(I)=HO_SDB(I)+DT2*QL_SDB(I)/(SDB_W)

        H_SDB(I)=MAX(0.0D0, H_SDB(I))

        ENDDO


        END SUBROUTINE SUISIN_BOX

!
!
!     ******************************************************************
!     ====================================
!             流水断面積→水深
!     ====================================
      subroutine atoh(i, j)
      real(8) a0, seki, hh
      integer i, j, k
!
        a0 = pi*dd1_sw(i)**2/4.0d0
        if(a_sw(i, j)/a0 < 1.0d-6) then
          h_sw(i, j) = 0.0d0
          return
        endif
      do k = 1, 1000
        seki = (a_sw(i, j)/a0 - a_spc(k))*(a_sw(i, j)/a0 - a_spc(k + 1))
        if(seki < 0.0d0) goto 1000
      enddo
      pause
!
 1000 hh = h_spc(k) + (a_sw(i, j)/a0 - a_spc(k))/(a_spc(k + 1) - a_spc(k))*(h_spc(k + 1) - h_spc(k))
      h_sw(i, j) = hh*dd1_sw(i)
!
      end subroutine atoh

!     ====================================
!                  H TO A
!     ====================================
        SUBROUTINE HTOA(I,J)
        INTEGER I, J, K
        REAL(8) SEL, AS, A0

        A0 = PI * DD1_SW(I) ** 2.0D0 /4.0D0
        IF(H_BOX(I,J) < TH) THEN
            A_BOX(I,J) = 0.0D0
            RETURN
        ENDIF


        DO K=1, 1000
            SEL = (H_BOX(I,J)/DD2_SW(I) - H_SPC(K)) * (H_BOX(I,J) / DD2_SW(I) - H_SPC(K+1))
			IF(SEL <= 0.0d0 ) GOTO 1000
			if(k==1000) write(*,*) time, i, sel
        ENDDO

        PAUSE

1000 AS = A_SPC(K) + (A_SPC(K+1)-A_SPC(K))* ((H_BOX(I,J) / DD2_SW(I)) - H_SPC(K))/(H_SPC(K+1)-H_SPC(K))
     A_BOX(I,J) = A0 * AS

        END SUBROUTINE HTOA


end module subprogs
