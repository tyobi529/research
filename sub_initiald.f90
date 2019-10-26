	Module sub_initiald
		use globals
		use subprogs
		implicit none
		contains
	
!     =======================================================
!                           룊딖??E
!     =======================================================
      subroutine initiald
      integer me, li, i, j, n
!     ===========================
!                럖둢뭤
!     ===========================
      do me = 1, mesh
        if(inf(me) == 1) mn(me) = 0.067d0
        if(inf(me) == 2) mn(me) = 0.043d0
        if(inf(me) == 3) mn(me) = 0.020d0
        if(inf(me) == 4) mn(me) = 0.025d0
        h(me) = 0.0d0 ; ho(me) = h(me) ; hmax(me) = h(me)
        qlme(me) = 0.0d0
      enddo
!     ====================================
      do li = 1, link
        um(li) = 0.0d0 ; umo(li) = um(li)
        vn(li) = 0.0d0 ; vno(li) = vn(li)
!     ---------------------------------- 뺚듩릣?
        if(limesh(li, 2) /= 0) then
          hl(li) = h(limesh(li, 1))*rthl(li, 1) + h(limesh(li, 2))*rthl(li, 2)
        else
          hl(li) = h(limesh(li, 1))
        endif
        lhan(li) = 0 ; lhano(li) = lhan(li)
      enddo
!     ===========================
!             뒼멄돷릣벞
!     ===========================
      do i = 1, iswr
      do j = 1, jswr(i)
        a_sw(i, j) = 0.0d0  ; ao_sw(i, j) = a_sw(i, j)
        h_sw(i, j) = 0.0d0
        q_sw(i, j) = 0.0d0  ; qo_sw(i, j) = 0.0d0 ; uu_sw(i, j) = 0.0d0
        ai_sw(i, j) = 0.0d0 ; r_sw(i, j) = 0.0d0
       
      enddo
        q_sw(i, jswr(i)+1) = 0.0d0  ; qo_sw(i, jswr(i)+1) = 0.0d0
      enddo
!     ===========================
!             ?깛긼??E
!     ===========================
      do n = 1, mnhl
        h_mh(n) = 0.0d0
        ho_mh(n) = 0.0d0
		A_MH(N) = 0.0D0
      enddo

!     ===========================
!   ACCUMULATED EXCHANGE DISCHARGE
!     ===========================      
      AC_CNQ=0.0D0
WRITE(310,*) 'TIME          AC_QP          AC_QM          AC_CNQ'

!     ===========================
!   INLET DISCHARGE TO PUMPING STATION
!     ===========================
DO I=1, ISWR
    
    DO J=1, IPUMP
     IF(MHUP_SW(I) == MH_PM(J))THEN        ! IN CASE OF UPSTREAM PIPE ADJOING
      IP(J)=I
      JP(J)=1
     ELSEIF (MHDW_SW(I)==MH_PM(J)) THEN    ! IN CASE OF DOWNSTREAM PIPE ADJOING
      IP(J)=I
      JP(J)=JSWR(I)+1
     ENDIF
ENDDO
ENDDO

WRITE(320,1000) (MH_PM(I), I=1, IPUMP)
1000 FORMAT ('TIME',5X,  10(I10,5X))

!     ===========================
!               ABOX
!     ===========================
        DO I=1, ISWR
            DO J=1, 2
                IF(MARK_SW_CHECK(I)==1)THEN
                    A_BOX(I,J) = 0.0D0
                    H_BOX(I,J) = 0.0D0
                    R_BOX(I,J) = 0.0D0
                ENDIF
            
            ENDDO
        ENDDO

!     ===========================
!             STORM DRAIN BOX
!     ===========================      
      DO I=1, SDBR
       H_SDB(I)=0.0D0
       HO_SDB(I)=0.0D0
      ENDDO

      end subroutine initiald

End Module sub_initiald	  