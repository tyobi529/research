	module sub_dwq_ori
	
	use globals
	use subprogs
	use sub_sentan

	implicit none
	contains
	
!     ===============================
!              Cal upq
!     ===============================
		subroutine dwq_ori(i)
		
		integer mh, ist, i, j, mode
		real(8) h_up, bs_up, h_dw, bs_dw, unitq
		real(8) dx_mn, hm, hs, as
!-----------------------------------------------------------		
      

        mh = mhdw_sw(i)
		
        h_dw  = h_mh(mh)  ; h_up  = h_sw(i, jswr(i))
        bs_dw = bs_mh(mh) ; bs_up = bs_sw(i, jswr(i))
        call sentan(h_up, h_dw, bs_up, bs_dw, unitq, ist)
!        if(time>66710.0d0 .and. i==179)then
!		write(*,*) ist, unitq
!		endif
		
        if(ist == 1) then
          q_sw(i, jswr(i)+1) = unitq * dd1_sw(i)
        goto 312
        endif

		hm = h_dw + bs_dw - bs_up
!		hs = h_up + bs_up - bs_dw
		hs = h_up + uu_sw(i,jswr(i))**2/2.0d0/gg
		dx_mn = sqrt(area_mh(mh)/pi)
		
		if(idshp_sw(i)==1) then
		    as = dd1_sw(i) * dd2_sw(i)
		elseif(idshp_sw(i) ==2) then
		    as = pi*dd1_sw(i)**2.0d0 / 4.0d0
		endif		
		
		if(hm >= hs)then
			if(hs/hm < 0.6666667d0)then
				q_sw(i,jswr(i)+1) = - 0.35d0 * min(as, a_box(i,2)) *sqrt(2.0d0*gg*hm)
			else
				q_sw(i,jswr(i)+1) = - 0.91d0 * min(as, a_sw(i,jswr(i))) *sqrt(2.0d0*gg*(hm-hs))
			endif
			
					
				q_sw(i,jswr(i)+1) = max(q_sw(i,jswr(i)+1), -a_box(i,2)*dx_mn/dt2)

		else	
			if(hm/hs < 0.6666667d0)then
				q_sw(i,jswr(i)+1) = 0.35d0 * min(as, a_sw(i,jswr(i))) * sqrt(2.0d0*gg*hs)
			else
				q_sw(i,jswr(i)+1) = 0.91d0 * min(as, a_box(i,2)) * sqrt(2.0d0*gg*(hs-hm))
			endif
									
				q_sw(i,jswr(i)+1) = min(q_sw(i,jswr(i)+1), a_sw(i,jswr(i))*dx_sw(i)/dt2)

		endif
		
  312   qsum_mh(mh) = qsum_mh(mh) + q_sw(i, jswr(i)+1)
        hsum_mh(mh) = hsum_mh(mh) + h_sw(i, jswr(i))	
        
        
        q_con(mh) = q_sw(i, jswr(i)+1)
       
       
       if(isnan(qsum_mh(mh)) .eqv. .TRUE.) then
         write(*,*) mh, q_sw(i,jswr(i)+1)
         pause "9 error"
         endif
       
         if(isnan(q_con(mh)) .eqv. .TRUE.) then
         write(*,*) mh, q_sw(i,jswr(i)+1), hm, hs
         pause "3 error"
         endif
      
        
		end subroutine dwq_ori
		
end module sub_dwq_ori		
		
		