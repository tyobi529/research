	module sub_sentan
	
	use globals

	implicit none
	contains
	
!     ====================================
!            CALCULATION Cutting edge part
!     ====================================
      subroutine sentan(h_up, h_dw, bs_up, bs_dw, unitq, ist)
      real(8) h_up, h_dw, bs_up, bs_dw, unitq
      real(8) hhe, hhw, hhep, hhwp, sgn, hh3
      integer ist, MODE, I, J, MH
!
        ist = 0
        if(h_up <= th .and. h_dw <= th) then
          unitq = 0.0d0
          ist = 1
          return
        endif
          hhe = h_dw + bs_dw    
          hhw = h_up + bs_up
          hhep = h_dw - th
          hhwp = h_up - th
!       --------------------------- 段落ち式
        if(hhe < bs_up) then
          if(h_up > th) then
            unitq = 0.544d0*h_up*sqrt(gg*h_up)
          else
            unitq = 0.0d0
          endif
           ist = 1
        elseif(hhw < bs_dw) then
          if(h_dw > th) then
            unitq = -0.544d0*h_dw*sqrt(gg*h_dw)
          else
            unitq = 0.0d0
          endif
          ist = 1
!       --------------------------- 完全越流の式
        elseif(hhep*hhwp < 0.0d0) then
          if(h_up > th .or. h_dw > th) then
            sgn = -(h_dw - h_up)/abs(h_dw - h_up)
            hh3 = max(h_dw + bs_dw, h_up + bs_up) - max(bs_dw, bs_up)
            unitq = sgn*0.35d0*hh3*sqrt(2.0d0*gg*hh3)
          else
            unitq = 0.0d0
          endif
          ist = 1
        endif
        
!
      end subroutine sentan

end module sub_sentan	  