	module sub_mdvel
	
	use globals
	use subprogs
	
	implicit none
	contains
	
!     ====================================
!               流速などの補完
!     ====================================
      subroutine mdvel
      real(8) phi
      integer li, me, k, i, j, n, np
!     =================================
!              hl calculation
!     =================================
      do li = 1, link
        if(limesh(li, 2) == 0) then
          hl(li) = h(limesh(li, 1))
        else
          hl(li) = h(limesh(li, 1))*rthl(li, 1) + h(limesh(li, 2))*rthl(li, 2)
        endif
      enddo
!     =================================
!            uu, vv calculation
!     =================================
      do li = 1, link
        if(hl(li) > th) then
          uu(li) = um(li)/hl(li)
          vv(li) = vn(li)/hl(li)
        else
          uu(li) = 0.0d0
          vv(li) = 0.0d0
        endif
      enddo
!     =================================
!           umm, vnm calculation
!     =================================
      do me = 1, mesh
        umm(me) = 0.0d0
        vnm(me) = 0.0d0
      do k = 1, ko(me)
        umm(me) = umm(me) + um(melink(me, k))*rtuv(me, k)
        vnm(me) = vnm(me) + vn(melink(me, k))*rtuv(me, k)
      enddo
      enddo
      do me = 1, mesh
        if(h(me) < th) then
          uum(me) = 0.0d0
          vvm(me) = 0.0d0
        else
          uum(me) = umm(me)/h(me)
          vvm(me) = vnm(me)/h(me)
        endif
      enddo
!     =================================
!           uusw, aisw calculation
!     =================================
      do i = 1, iswr
      IF(MARK_SW_CHECK(I)/=1)GOTO 100
      do j = 1, jswr(i)
!       --------------------- 補完面積
        if(j /= 1) ai_sw(i, j) = (a_sw(i, j - 1) + a_sw(i, j))/2.0d0
!       --------------------- 流速
        if(h_sw(i, j) > th) then
          uu_sw(i, j) = (q_sw(i, j) + q_sw(i, j + 1))/2.0d0/a_sw(i, j)
        else
          uu_sw(i, j) = 0.0d0
        endif
!       --------------------- 径深
        if(idshp_sw(i) == 1) then
          r_sw(i, j) = a_sw(i, j)/(dd1_sw(i) + 2.0d0*h_sw(i, j))
        elseif(idshp_sw(i) == 2) then
          phi = 2.0d0 * acos(1.0d0 - 2.0d0*min(h_sw(i, j),dd1_sw(i))/dd1_sw(i))
          r_sw(i, j) = (dd1_sw(i)/4.0d0)*(1.0d0 - sin(phi)/phi)
        endif
      enddo
100   enddo
!
        do i=1, iswr
            IF(MARK_SW_CHECK(I)/=1)GOTO 200
                if(idshp_sw(i) == 1) then
                    r_box(i, 1) = a_box(i, 1)/(dd1_sw(i) + 2.0d0*h_box(i, 1))
                    r_box(i, 2) = a_box(i, 2)/(dd1_sw(i) + 2.0d0*h_box(i, 2))
                elseif(idshp_sw(i) == 2) then
                    phi = 2.0d0 * acos(1.0d0 - 2.0d0*min(h_box(i, 1),dd1_sw(i))/dd1_sw(i))
                    r_box(i, 1) = (dd1_sw(i)/4.0d0)*(1.0d0 - sin(phi)/phi)
                    phi = 2.0d0 * acos(1.0d0 - 2.0d0*min(h_box(i, 2),dd1_sw(i))/dd1_sw(i))
                    r_box(i, 2) = (dd1_sw(i)/4.0d0)*(1.0d0 - sin(phi)/phi)
                endif
200     enddo
              
              
1000 FORMAT (F7.2, 3X, I6, 3X, I6, 10(3X, F15.5))
      end subroutine mdvel


end module sub_mdvel	  