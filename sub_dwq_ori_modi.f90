	module sub_dwq_ori_modi
	
	use globals
	use subprogs
	use sub_sentan

	implicit none
	contains
	
!     ===============================
!              Cal upq
!     ===============================
		subroutine dwq_ori_modi(i)
		
		integer mh, ist, i, j, mode
		real(8) h_up, bs_up, h_dw, bs_dw, unitq
		real(8) dx_mn, hm, hs, as, vv, hv, avv
!-----------------------------------------------------------		
        mh = mhdw_sw(i)
		
        h_dw  = h_mh(mh)  ; h_up  = h_sw(i, jswr(i))
        bs_dw = bs_mh(mh) ; bs_up = bs_sw(i, jswr(i))
        call sentan(h_up, h_dw, bs_up, bs_dw, unitq, ist)
        if(ist == 1) then
          q_sw(i, jswr(i)+1) = unitq * dd1_sw(i)
          goto 312
        endif
		
		if(h_sw(i,jswr(i)) > th)then
			vv = ( qo_sw(i,jswr(i)) + qo_sw(i,jswr(i)+1) ) / (2.0d0*a_sw(i,jswr(i))) ! velocity
		else
			vv = 0.0d0
		endif

		hv = vv**2.0d0 / (2.0d0*gg)  ! velocity head	
		
		hm = h_dw + bs_dw - bs_up
		hs = h_up + bs_up + hv - bs_dw
		dx_mn = sqrt(area_mh(mh)/pi)
		avv = (a_sw(i,jswr(i)) + a_box(i,2)) / 2.0d0
		
		if(idshp_sw(i)==1) then
		    as = dd1_sw(i) * dd2_sw(i)
		elseif(idshp_sw(i) ==2) then
		    as = pi*dd1_sw(i)**2.0d0 / 4.0d0
		endif		
		
		if(hm >= hs)then
				q_sw(i,jswr(i)+1) = - 0.6d0 * min(as, avv) *sqrt(2.0d0*gg*(hm-hs))
				q_sw(i,jswr(i)+1) = max(q_sw(i,jswr(i)+1), -a_box(i,2)*dx_mn/dt2)
		else	
				q_sw(i,jswr(i)+1) = 0.6d0 * min(as, avv) * sqrt(2.0d0*gg*(hs-hm))
				q_sw(i,jswr(i)+1) = min(q_sw(i,jswr(i)+1), a_sw(i,jswr(i))*dx_sw(i)/dt2)
		endif
		
  312   qsum_mh(mh) = qsum_mh(mh) + q_sw(i, jswr(i)+1)
        hsum_mh(mh) = hsum_mh(mh) + h_sw(i, jswr(i))	

		end subroutine dwq_ori_modi
		
end module sub_dwq_ori_modi
		
		