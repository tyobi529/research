	module sub_forward
	
		use globals
		use subprogs
		
		implicit none
		contains
		
!     ******************************************************************
      subroutine forward
      integer li, me, i, j
!
      do li = 1, link
        umo(li) = um(li) ; vno(li) = vn(li)
        lhano(li) = lhan(li)
      enddo
!
      do me = 1, mesh
        ho(me) = h(me)
        qlme(me) = 0.0d0
      enddo
!

     do i = 1, iswr
IF(MARK_SW_CHECK(I)/=1)GOTO 100      
      do j = 1, jswr(i)
        qo_sw(i, j) = q_sw(i, j)
        ao_sw(i, j) = a_sw(i, j)
        ql_sw(i, j) = 0.0d0
      enddo
        qo_sw(i, jswr(i) + 1) = q_sw(i, jswr(i) + 1)
100   enddo
!
      do i = 1, mnhl
        ho_mh(i) = h_mh(i)
        qsum_mh(i) = 0.0d0
        hsum_mh(i) = 0.0d0
      enddo
!
    DO I=1, SDBR
        HO_SDB(I)=H_SDB(I)
        QL_SDB(I)=0.0D0
    ENDDO
      end subroutine forward

end module sub_forward	  