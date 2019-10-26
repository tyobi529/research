	module sub_upq_ori_modi
	
	use globals
	use subprogs
	use sub_sentan

	implicit none
	contains
	
!     ===============================
!              Cal upq
!     ===============================
		subroutine upq_ori_modi(i)
		
		integer mh, ist, i, j, mode
		real(8) h_up, bs_up, h_dw, bs_dw, unitq
		real(8) dx_mn, hm, hs, as, vv, hv, avv
!-----------------------------------------------------------		
        mh = mhup_sw(i)
		
        h_up  = h_mh(mh)  ; h_dw  = h_sw(i, 1)
        bs_up = bs_mh(mh) ; bs_dw = bs_sw(i, 1)
        call sentan(h_up, h_dw, bs_up, bs_dw, unitq, ist)
        if(ist == 1) then
          q_sw(i, 1) = unitq * dd1_sw(i)
          goto 311
        endif
		
		if(h_sw(i,1) > th)then
			vv = ( qo_sw(i,1) + qo_sw(i,2) ) / (2.0d0*a_sw(i,1)) ! velocity
		else
			vv = 0.0d0
		endif
		
		hv = vv**2.0d0 / (2.0d0*gg)  ! velocity head		
		
		hm = h_up + bs_up - bs_dw
		hs = h_dw + bs_dw + hv - bs_up 
		dx_mn = sqrt(area_mh(mh)/pi)
		avv = (a_sw(i,1) + a_box(i,1)) / 2.0d0
		
		if(idshp_sw(i)==1) then
		    as = dd1_sw(i) * dd2_sw(i)
		elseif(idshp_sw(i) ==2) then
		    as = pi*dd1_sw(i)**2.0d0 / 4.0d0
		endif

		if(hm >= hs)then
				q_sw(i,1) = 0.6d0 * min(as, avv) *sqrt(2.0d0*(hm-hs))
				q_sw(i,1) = min(q_sw(i,1), a_box(i,1)*dx_mn/dt2)
		else	
				q_sw(i,1) = -0.6d0 * min(as, avv) * sqrt(2.0d0*gg*(hs-hm))
				q_sw(i,1) = max(q_sw(i,1), - a_sw(i,1)*dx_sw(i)/dt2)
		endif
		
  311   qsum_mh(mh) = qsum_mh(mh) - q_sw(i, 1)
        hsum_mh(mh) = hsum_mh(mh) + h_sw(i, 1)	

		end subroutine upq_ori_modi
		
end module sub_upq_ori_modi		
		
		