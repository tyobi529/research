	module sub_rdt
	
	use globals
	use subprogs
	
	implicit none
	contains
	
      subroutine rdat
      real(8) dd, dseg, x0, y0, X_SD, Y_SD, XS, YS, XE, YE
      integer i, j, k, n, idd1, idd2, nm
      integer j1, j2, me_in, SW_I, SW_J, NM_P, NM_SD
      integer no, me, li, START, MH
      integer it
!     =======================================================
!                     mesh data
!     =======================================================
        read(17, 1701) mesh 
        write(*,*)'mesh=', mesh
      do me = 1, mesh
        read(17, 1702) ko(me), (menode(me, k), k = 1, ko(me))
        read(17, 1703) (melink(me, k), k = 1, ko(me))
        read(17, 1704) smesh(me), xmesh(me), ymesh(me)
        read(17, 1705) (rtuv(me, k), k = 1, ko(me))
!        xmesh(me) = xmesh(me) -  43995.00
!        ymesh(me) = ymesh(me) - 154999.00
        read(500,4000) ii(me), jj(me)
      enddo
 1701 format(6x, i6)
 1702 format(8x, i5, 5x, 3i8)
 1703 format(13x, 3i8)
 1704 format(8x, f13.2, 2f15.2)
 1705 format(13x, 3f10.5)
 4000 format(7x, i3, 7x, i3)
      close(17)
      close(500)

!     =======================================================
!                     link data
!     =======================================================
        read(18, 1801) link
      write(*,*)'link=', link
      do li = 1, link
        read(18, 1802) limesh(li, 1), limesh(li, 2), linode(li, 1), linode(li, 2)
        read(18, 1803) scv(li), rthl(li, 1), rthl(li, 2)
        read(18, 1804) ux(li), uy(li)
      enddo
 1801 format(6x, i6)
 1802 format(8x, 2i8, 2x, 2i8)
 1803 format(8x, f10.2, 2f10.5)
 1804 format(18x, 2f10.5)
      close(18)
!     =======================================================
!                        inf data read
!     =======================================================
      do me = 1, mesh
        inf(me) = 1
      enddo       
!     =======================================================
!                      bs data
!     =======================================================
      do me = 1, mesh
        read(15, 1501) baseo(me)
      enddo
 1501 format(f10.2)
      close(15)
!     =======================================================
!                   rain data
!     =======================================================
     
!      do i = 1, irain
!        read(16, 1602) rain(i)
!      enddo
        
 !      read(16, 1601) irain, dtrain
!       do i = 1, mesh
!        read(16, 1602) (rain(i,j), j=1, irain)
!        write(*,*) i, j, irain, rain(i, irain)
!      enddo
!WRITE(*,*) IRAIN, DTRAIN
! 1601 format(i5, f10.1)
! 1602 format(10x, 540f10.3)
!      close(16)
irain=1260
dtrain=60.0

!     =======================================================
!                     manhole data
!     =======================================================
       read(11, 1111) mnhl
      do n = 1, mnhl
      ! read(11, 1112) id_mh(n), inf_mh(n), ngrp(n), x_mh(n), y_mh(n), bs_mh(n), area_mh(n), num(n), (isw_mh(n, nm), nm = 1, num(n))
      read(11,1112) id_mh(n), grp_mh(n), no_mh(n), bs_mh(n), kubun_mh(n), shp_mh(n), x_mh(n), y_mh(n), area_mh(n), num(n), (isw_mh(n,j), j=1,num(n))
      inf_mh(n)=1
      ngrp(n)=1
      enddo

 1111 format(i6)
 ! 1112 format(8x, i9, 2i5, 2f9.1, 15x, f8.3, 8x, f8.1, 16x, i8, 10i8)
 1112 format(i3, 2i4, f6.2, i4, i4, 2x, f12.5, 1x, f12.4, 2x, f6.3, i4, 4i5)
      close(11)
      
      
!     ========================================================
!           PUMPING STATION DATA READ
!     ========================================================
!        read(32, *) ipump
!      do i = 1, ipump
!        read(32, 1321) mh_pm(i), q_pm(i)
!      enddo
! 1321 format(i8, f10.2)
!      close(32)
       ipump = 0
!     =======================================================
!                     conduit data
!     =======================================================
        read(12, 1121) iswr

      do i = 1, iswr
!        read(12, 1122) id_sw(i), inf_sw(i), igrp(i), idup_mh(i), iddw_mh(i), dst_sw(i), idshp_sw(i), idd1, idd2, rn_sw(i), bsdw_sw(i), slp_sw(i), mhup_sw(i), mhdw_sw(i) &
!                       , ipt_sw(i), (x_sw(i, j), y_sw(i, j), j = 1, ipt_sw(i))
         read(12,1122) id_sw(i), idshp_sw(i), idd1, idd2, slp_sw(i), dst_sw(i), bsup_sw(i), bsdw_sw(i),mhup_sw(i), mhdw_sw(i), ipt_sw(i), (x_sw(i,j), y_sw(i,j), j=1,ipt_sw(i))
!
        dd1_sw(i) = dble(idd1)*1.0d-3                                    ! ??
        dd2_sw(i) = dble(idd2)*1.0d-3                                    ! ??
        dst_sw(i) = 0.0d0
        rn_sw(i)=0.013d0
        slp_sw(i)=slp_sw(i)*1.0d-3
        inf_sw(i)=1
        igrp(i)=1
      enddo
      ! 1122 format(8x, i8, 2i5, i9, i10, f8.3, 14x, i3, 2i8, f8.3, 7x, f8.3, f9.6, 8x, 2i8, i9, 20(f8.1, f9.1))
1122 format(2i4, 2i5, 2f5.1, 2f8.3, 2i5, i3, 3x, 4(f12.5, 2x, f12.4, 2x)) 
!   ----------------------PIPE OUTLET SORING ---------------------
!DO I=1, IPUMP
!    START=MH_PM(I)
!
!        DO J=1, ISWR
!        MARK_MH(I,J)=0
!
!            IF(MHDW_SW(J)==START.OR.MHUP_SW(J)==START)THEN
!                MARK_SW(I,J)=1
!            ELSE
!                MARK_SW(I,J)=0
!            ENDIF
!        
!        ENDDO
!
!CALL SORPIPE(I)        
!
!ENDDO      
!
! DO I=1, ISWR
!    MARK_SW_CHECK(I)=0  
! ENDDO
! 
! DO I=1, MNHL
!    MARK_MH_CHECK(I)=0  
! ENDDO
!
!    DO I=1, IPUMP
!        DO J=1, ISWR
!            IF(MARK_SW(I,J)==1) MARK_SW_CHECK(J)=1 ! PIPES CONNECTED WITH PUMPING STATION 
!        enddo
!    ENDDO 
!
!     DO I=1, ISWR  ! MANHOLE CONNECTED WITH UPPER PIPES
!        IF(MARK_SW_CHECK(I)==1)THEN
!        MARK_MH_CHECK(MHDW_SW(I))=1
!        MARK_MH_CHECK(MHUP_SW(I))=1
!        ENDIF       
!     ENDDO    
!    
do I=1, ISWR
 MARK_SW_CHECK(i)=1
enddo
do i=1,mnhl
 MARK_MH_CHECK(i)=1
enddo
!---pipe---
do i=1,6
mark_sw_check(i)=0
enddo
do i=31,49
mark_sw_check(i)=0
enddo
do i=124,127
mark_sw_check(i)=0
enddo
!---mh---
mark_mh_check(1)=0
do i=3,4
mark_mh_check(i)=0
enddo
do i=32,37
mark_mh_check(i)=0
enddo
do i=39,42
mark_mh_check(i)=0
enddo
do i=44,45
mark_mh_check(i)=0
enddo
do i=47,49
mark_mh_check(i)=0
enddo
mark_mh_check(125)=0
mark_mh_check(127)=0




!   ------------------connection data-------------------------------
    read(503,*)
    read(503,*)
    do i=1,10
        read(503,1130) con_mh(i), con_mesh(i)
        bs_mh(con_mh(i))=baseo(con_mesh(i))
    enddo
    1130 format(i3,i7)

!   ------------------PIPE GRID DIVISION----------------------------      
      do i=1, iswr
      IF(MARK_SW_CHECK(I)==1)THEN
        do j = 1, ipt_sw(i) - 1
          dst_sw(i) = dst_sw(i) + sqrt((x_sw(i, j + 1) - x_sw(i, j))**2 + (y_sw(i, j + 1) - y_sw(i, j))**2)
        enddo

        jswr(i) = max(int(dst_sw(i)/20.0d0), 1)    ! 20M INTERVAL

        dx_sw(i) = dst_sw(i)/dble(jswr(i))                               ! ?ｷ・｣・樓ｶｻ・ｴx
        

          j1 = 1
          j2 = j1 + 1
          dd = dx_sw(i)/2.0d0  ! ?・ｶｻstorm drain・場ｶｳ・?EE
          dseg = sqrt((x_sw(i, j2) - x_sw(i, j1))**2 + (y_sw(i, j2) - y_sw(i, j1))**2)  ! ?耶弊・??ｩ・ｻ?EE
          
          x0 = x_sw(i, j1)
          y0 = y_sw(i, j1)

        do j = 1, jswr(i)
       
          if(j /= 1) dd = dx_sw(i)
  100     continue
          if(dd > dseg) then
            j1 = j2
            j2 = j1 + 1

            dd = dd - dseg
            dseg = sqrt((x_sw(i, j2) - x_sw(i, j1))**2 + (y_sw(i, j2) - y_sw(i, j1))**2)  ! ?耶弊・??ｩ・ｻ?EE
            x0 = x_sw(i, j1)
            y0 = y_sw(i, j1)
            goto 100
          endif

          X_M(I,J) = x0 + (x_sw(i, j2) - x0)*dd/dseg ! CENTER OF X-COORDINATION OF PIPE GRID
          Y_M(I,J) = y0 + (y_sw(i, j2) - y0)*dd/dseg ! CENTER OF Y-COORDINATION OF PIPE GRID

          dseg = dseg - dd

          x0 = X_M(I,J)
          y0 = Y_M(I,J)

          bs_sw(i, j) = bsdw_sw(i) + slp_sw(i)*(jswr(i) + 0.5d0 - j)*dx_sw(i)  ! ?耶傑?ｼ・・､ｵ・ｬ?・?E,2,3,・?jswr(i)・?
       
        enddo
     ENDIF
!write(*,*) jswr(i)

      enddo
 1120 FORMAT(F10.1)     
 1121 format(i6)

      close(12)
write(*,*) 


!   

      
!       -----------------------------------------
!                   SEWER PIPE MESH GENERATION
!       -----------------------------------------
        NM_P=0
       DO I = 1, ISWR
 IF(MARK_SW_CHECK(I)==1)THEN
          J1 = 1
          J2 = J1 + 1
          
          DSEG = SQRT((X_SW(I, J2) - X_SW(I, J1))**2 + (Y_SW(I, J2) - Y_SW(I, J1))**2)
                                                                                                           
          XS = X_SW(I, J1)
          YS = Y_SW(I, J1)

        DO J = 1, JSWR(I)

            DD = DX_SW(I)

  200     CONTINUE
          IF(DD > DSEG) THEN
            J1 = J2
            J2 = J1 + 1
            DD = DD - DSEG
            DSEG = SQRT((X_SW(I, J2) - X_SW(I, J1))**2 + (Y_SW(I, J2) - Y_SW(I, J1))**2)
  
            GOTO 200                                                                                       
          ENDIF                                                                                            
                                                                                                           
          XE = XS + (X_SW(I, J2) - XS)*DD/DSEG                                                       
          YE = YS + (Y_SW(I, J2) - YS)*DD/DSEG 
          
          DSEG = DSEG - DD  

        IF(J==JSWR(I))THEN
            XE=X_SW(I,IPT_SW(I))
            YE=Y_SW(I,IPT_SW(I))
        ENDIF

CALL STLINE(I, XS, YS, XE, YE, NM_P)                                    

                                                                                                         
          XS = XE
          YS = YE

        ENDDO
ENDIF        
      ENDDO
      
!   ========================================================
!                 manhole elevation adjustment
!   ========================================================
        do i=1, iswr
       IF(MARK_SW_CHECK(I)==1)THEN
            MH = MHUP_SW(I)
            BS_BOX(I,1) = BS_SW(I,1) + SLP_SW(I)*(DX_SW(I)/2.0D0 + SQRT(AREA_MH(MH)/PI))
          !  IF(BS_BOX(I,1) < BS_MH(MH)) BS_MH(MH) = BS_BOX(I,1)
            
            mh = mhdw_sw(i)
            BS_BOX(I,2) = BS_SW(I,JSWR(I)) - SLP_SW(I)*(DX_SW(I)/2.0D0 + SQRT(AREA_MH(MH)/PI))
          !  IF(BS_BOX(I,2) < BS_MH(MH)) BS_MH(MH) = BS_BOX(I,2)
        endif
!        WRITE(*,*) I, BS_BOX(I,1), BS_BOX(I,2)
        enddo
!        do n=1,182
!        write(*,*) n, bs_mh(n)   
!        enddo 
10000 format(i5, 3x, 5(f10.5, 3x))         
!     =======================================================
!                     node data
!     =======================================================
       read(13, *)
       read(13, *)
       read(13, 1301) node
      write(*,*)'node=', node
       read(13, *)
      do no = 1, node
        read(13, *) i, dnox(no), dnoy(no)
!        dnox(no) = dnox(no) -  43995.00
!        dnoy(no) = dnoy(no) - 154999.00
      enddo      
      
      READ(13,*)
      READ(13,1301) MESH
      
        DO ME=1, MESH
            READ(13,*) N, I, J, K, MARK(ME)
        ENDDO
      
      DO ME=1, MESH
	  IF(MARK(ME)==1.OR.MARK(ME)==2)THEN
		
		NM_SD = NM_SD+1
        
        X_SD=XMESH(ME)
		Y_SD=YMESH(ME)

      CALL SD(X_SD, Y_SD, SW_I, SW_J,ME,judge)

        CN_I(ME)=SW_I
        CN_J(ME)=SW_J
    WRITE(77,1200 ) ME, CN_I(ME), CN_J(ME),judge        
        ENDIF
    ENDDO  
    1200 FORMAT(I6, 5X, I5, 5X, I3, f8.3)

    !       -----------------------------------------
!            CONNECTION GRID BETWEEN STORM DRAIN AND SEWER PIPE
!       -----------------------------------------          
DO ME=1, MESH
    IF(MARK(ME)==1.OR.MARK(ME)==2)THEN
          WRITE(78,*) 'TITLE="CONNECTION GRID BETWEEN STORM DRAIN AND SEWER PIPE"'
          WRITE(78,*) 'VARIABLES= "X", "Y"'
          WRITE(78,1305)  2
  !      WRITE(78,1320) XMESH(ME), X_M(CN_I(ME), CN_J(ME))
  !      WRITE(78,1320) YMESH(ME), Y_M(CN_I(ME), CN_J(ME))
     ENDIF
ENDDO


!       -----------------------------------------
!             storm drain POSITION PRINTING
!       -----------------------------------------          
          WRITE(110,*) 'TITLE="NAKAHAMA STROM DRAINS"'
          WRITE(110,*) 'VARIABLES= "X", "Y"'
          WRITE(110,1310)  NM_SD

    DO ME=1, MESH
      IF(MARK(ME)==1)THEN
          WRITE(110,1320) XMESH(ME)
      ENDIF
    ENDDO
    
    DO ME=1, MESH
      IF(MARK(ME)==1)THEN
          WRITE(110,1320) YMESH(ME)
      ENDIF
    ENDDO
    
 1301 format(i6)
 1302 FORMAT(I6, 3X, I6, 3X, I6)
 1305 FORMAT('ZONE DATAPACKING=BLOCK, I=', I10)
 1310 FORMAT('ZONE T="STORM DRAINS", DATAPACKING=BLOCK, I=', I10)
 1320 FORMAT(2F15.3)
    !       -----------------------------------------
!   CONUNTING NUMBER OF MATCHING GRIDS(GROUND AND SEWER PIPE)
!       -----------------------------------------

	DO I=1, ISWR
		DO J=1, JSWR(I)
		NM_METOSW(I,J)=0
			DO ME=1, MESH
				IF(I==CN_I(ME).AND.J==CN_J(ME))THEN
					NM_METOSW(I,J)=NM_METOSW(I,J)+1
				ENDIF
			ENDDO
		ENDDO
	ENDDO
	
      close(13)      
!     =======================================================
!                         ・｣・梦ｲｫ・・・ｩ・
!     =======================================================
      read(14, *)
      do k = 1, 1001
        read(14, 1141) h_spc(k), a_spc(k), r_spc(k)
      enddo
 1141 format(f9.3, 11x, 2f10.6)
      close(14)
!     =======================================================
!            ??・?
!     =======================================================
       do me = 1, mesh
           lambda(me) = 0.0d0
       enddo
!     =======================================================
!            ・・寸・?
!     =======================================================
       do li = 1, link
         rbeta(li) = 1.0d0
       enddo
!    pumpに接続されているところの水位データ

!     
!      do it=1,1560
!        read(501,'(f5.2)') hh(it)
!      enddo
!      close(501)
!      
!!
     do it=1,1560
        read(502,'(f5.2)') qq(it)
      enddo
      close(502)
      end subroutine rdat
!
!     *****************************************************
    !***************************************
    !  PIPE SORTING START
    !***************************************

SUBROUTINE SORPIPE(I)

INTEGER I, J, K, L, NUM, NUMO

NUM=0
NUMO=0
100 CONTINUE

!WRITE(*,*) NUM, NUMO

DO J=1, ISWR
    IF(MARK_SW(I,J)==1)THEN
        MARK_MH(I,MHDW_SW(J))=1 ; MARK_MH(I,MHUP_SW(J))=1
    ELSE
    ENDIF
ENDDO

DO J=1, ISWR
    IF(MARK_MH(I,MHDW_SW(J))==1.OR.MARK_MH(I,MHUP_SW(J))==1) MARK_SW(I,J)=1
ENDDO


DO J=1, ISWR
   IF(MARK_SW(I,J)==1) NUM=NUM+1
ENDDO



IF(NUM/=NUMO) THEN
 NUMO=NUM
 NUM=0
 GOTO 100
ENDIF



END SUBROUTINE SORPIPE
    


!     =======================================================
!    MATCHING BETWEEN STORM DRAIN ON GROUND AND SEWER PIPE GRIND
!     =======================================================
    SUBROUTINE SD(X_SD, Y_SD, SW_I, SW_J, ME,judge)

    INTEGER I, J, ME, NUM, SW_I,  SW_J
    INTEGER I1, I2
    REAL(8) XH, YH, X_SD, Y_SD    
    REAL(8) DIST(100000), JUDGE

    NUM=1
    

    DO I=1, ISWR
  !  write (*,*) MARK_SW_CHECK(I)
    IF(MARK_SW_CHECK(I)/=1) GOTO 110
	    DO J=1, JSWR(I) 
	    IF(BS_SW(I,J)+DD2_SW(I) >= BASEO(ME)) GOTO 100 ! FREEBOARD FOR STORM DRAIN BOX = 1m
            DIST(NUM) = SQRT((X_SD-X_M(I,J))**2 + (Y_SD-Y_M(I,J))**2) ! DISTANCE BETWEEN A GROUND MESH COORDINATION AND A PIPE GRID CENTER COORDINATION
            NUM=NUM+1                                                 ! X_SD , Y_SD :  STORM DARIN COORDINATION

100     ENDDO
110  ENDDO

    JUDGE=100000

    DO I=1, NUM-2
        I1=I ; I2=I+1
	    JUDGE=MIN(DIST(I1), DIST(I2), JUDGE)
    ENDDO
 
    DO I=1, ISWR
    IF(MARK_SW_CHECK(I)/=1) GOTO 210
	    DO J=1, JSWR(I)
	    IF(BS_SW(I,J)+ DD2_SW(I)  >= BASEO(ME)) GOTO 200	 ! FREEBORD FOR STORM DRAIN BOX = 1m   
            IF(JUDGE== SQRT( (X_SD-X_M(I,J))**2 + (Y_SD-Y_M(I,J))**2)) THEN

            SW_I=INT(I)
            SW_J=INT(J)

         ENDIF

200      ENDDO
210   ENDDO

END SUBROUTINE SD

!     =======================================================
!           SEWER PIPE GRID MESH GENERATION FOR TECPLOT
!     =======================================================
    SUBROUTINE STLINE(I, X1, Y1, X2, Y2, NM)  
    
    INTEGER I, J1, J2, NM
    REAL(8) X1, Y1, X2, Y2
    REAL(8) A, D
       
     D=DD1_SW(I)
    
    A=(Y1-Y2)/(X1-X2)
    
    IF(X1-X2==0) GOTO 100
    IF(Y1-Y2==0) GOTO 200
    

! *****************************************    
    !(2-4) ??????????E
NM=NM+1   
NUM_PG(NM)=NM
    
    XX(NM)=X1-(D/2)*SQRT(A**2+1)/(A+1/A)
    YY(NM)=A*XX(NM)+Y1-A*X1+(D/2)*SQRT(A**2+1)

    !(2-5) ??????????E
NM=NM+1    
NUM_PG(NM)=NM
    
    XX(NM)=(A*X1+X2/A+Y2-Y1-(D/2)*SQRT(A**2+1))/(A+1/A)
    YY(NM)=A*XX(NM)+Y1-A*X1+(D/2)*SQRT(A**2+1)
   
    !(3-5) ??????????E
NM=NM+1    
NUM_PG(NM)=NM
    
    XX(NM)=(A*X1+X2/A-Y1+Y2+(D/2)*SQRT(A**2+1))/(A+1/A)
    YY(NM)=A*XX(NM)+Y1-A*X1-(D/2)*SQRT(A**2+1)
    
NM=NM+1
NUM_PG(NM)=NM
    !(3-4) ??????????E
    
    XX(NM)=X1+(D/2)*SQRT(A**2+1)/(A+1/A)
    YY(NM)=A*XX(NM)+Y1-A*X1-(D/2)*SQRT(A**2+1)

    
GOTO 300

100 CONTINUE

NM=NM+1
NUM_PG(NM)=NM

XX(NM)=X1-D/2 ; YY(NM)=Y1

NM=NM+1
NUM_PG(NM)=NM

XX(NM)=X1+D/2 ; YY(NM)=Y1

NM=NM+1
NUM_PG(NM)=NM

XX(NM)=X2+D/2 ; YY(NM)=Y2


NM=NM+1
NUM_PG(NM)=NM

XX(NM)=X2-D/2 ; YY(NM)=Y2

GOTO 300

200 CONTINUE

NM=NM+1
NUM_PG(NM)=NM

XX(NM)=X1; YY(NM)=Y1+D/2

NM=NM+1
NUM_PG(NM)=NM

XX(NM)=X2; YY(NM)=Y2+D/2

NM=NM+1
NUM_PG(NM)=NM

XX(NM)=X2; YY(NM)=Y2-D/2

NM=NM+1
NUM_PG(NM)=NM

XX(NM)=X1 ; YY(NM)=Y1-D/2

GOTO 300

300 CONTINUE
    NUM_PMAX=NUM_PG(NM) ;NUM_EM= INT(NUM_PMAX/4)

END SUBROUTINE STLINE  
end module sub_rdt	