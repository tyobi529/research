	module sub_upq_ori

	use globals
	use subprogs
	use sub_sentan

	implicit none
	contains

!     ===============================
!              Cal upq
!     ===============================
		subroutine upq_ori(i)

		integer mh, ist, i, j, mode
		real(8) h_up, bs_up, h_dw, bs_dw, unitq
		real(8) dx_mn, hm, hs, as
!-----------------------------------------------------------
				! mhにマンホール番号代入
			  mh = mhup_sw(i)

        h_up  = h_mh(mh)  ; h_dw  = h_sw(i, 1)
        bs_up = bs_mh(mh) ; bs_dw = bs_sw(i, 1)
        call sentan(h_up, h_dw, bs_up, bs_dw, unitq, ist)
        if(ist == 1) then
          q_sw(i, 1) = unitq * dd1_sw(i)
          goto 311
        endif

		hm = h_up + bs_up - bs_dw
	!	hs = h_dw + bs_dw - bs_up
	    hs = h_dw + uu_sw(i,1)**2/2.0d0/gg

		dx_mn = sqrt(area_mh(mh)/pi)

		if(idshp_sw(i)==1) then
		    as = dd1_sw(i) * dd2_sw(i)
		elseif(idshp_sw(i) ==2) then
		    as = pi*dd1_sw(i)**2.0d0 / 4.0d0
		endif


		if(hm >= hs)then
			if(hs/hm < 0.6666667d0)then
				q_sw(i,1) = 0.35d0 * min(as, a_box(i,1)) *sqrt(2.0d0*gg*hm)
			else
				q_sw(i,1) = 0.91d0 * min(as, a_sw(i,1)) *sqrt(2.0d0*gg*(hm-hs))
			endif
				q_sw(i,1) = min(q_sw(i,1), a_box(i,1)*dx_mn/dt2)

!			if(i==118) then
!			write(*,*) 'hm>hs up i=118', q_sw(i,jswr(i)+1)
!			endif
		else
			if(hm/hs < 0.6666667d0)then
				q_sw(i,1) = -0.35d0 * min(as, a_sw(i,1)) * sqrt(2.0d0*gg*hs)
			else
				q_sw(i,1) = -0.91d0 * min(as, a_box(i,1)) * sqrt(2.0d0*gg*(hs-hm))
			endif



				q_sw(i,1) = max(q_sw(i,1), - a_sw(i,1)*dx_sw(i)/dt2)
!		if(i==118) then
!			write(*,*) 'hm<hs up i=118', q_sw(i,jswr(i)+1)
!			endif
			endif

  311   qsum_mh(mh) = qsum_mh(mh) - q_sw(i, 1)
        hsum_mh(mh) = hsum_mh(mh) + h_sw(i, 1)

!if(i==108)then
!write(*,*) 'upq', q_sw(i,1), h_up, h_dw
!endif


       if(isnan(qsum_mh(mh)) .eqv. .TRUE.) then
         write(*,*) mh, q_sw(i,1), h_up, h_dw , unitq, ist
         pause "8 error"
         endif

         if(isnan(q_sw(i, 1)) .eqv. .TRUE.) then
         write(*,*) mh, h_up, h_dw
         pause "4 error"
         endif


		end subroutine upq_ori

end module sub_upq_ori
