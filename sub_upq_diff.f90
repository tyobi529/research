	module sub_upq_diff
	
	use globals
	use subprogs
	use sub_sentan

	implicit none
	contains
	
!     ===============================
!              Cal upq
!     ===============================
		subroutine upq_diff(i)
		
		integer mh, ist, i, j, mode
		real(8) h_up, bs_up, h_dw, bs_dw, unitq
		real(8) dx_mn, ri, u2, u3
!-----------------------------------------------------------		
        mh = mhup_sw(i)
!       ------------------------
!             ??E???
!       ------------------------

        h_up  = h_box(i,1)    ; h_dw  = h_sw(i, 1)
        bs_up = bs_box(i,1)   ; bs_dw = bs_sw(i, 1)
		
        call sentan(h_up, h_dw, bs_up, bs_dw, unitq, ist)

                mode = 0

        if(ist == 1) then
            q_sw(i, 1) = unitq * dd1_sw(i)
            mode = 1
            goto 311
        ENDIF
	
!     --------------------------------
!       UPSTREAM (DIFFUSIVE EQUATION)
!     --------------------------------mhup_sw(i)
		DX_MN = SQRT(AREA_MH(MH)/PI)
		ri = (r_box(i, 1) + r_sw(i, 1))/2.0d0
		U2 = -GG * (A_BOX(I,1) + A_SW(I,1))/2.0D0 *(H_SW(I,1)+BS_SW(I,1)-H_BOX(I,1)-BS_BOX(I,1))/(DX_SW(I)/2.0D0 + DX_MN )
		U3 = -GG * RN_SW(I)**2 * ABS(QO_SW(I,1)) / 2.0D0 / ((A_BOX(I,1) + A_SW(I,1))/2.0D0) / ri **(4.0D0/3.0D0)

!		ri =  r_sw(i, 1)
!		U2 = -GG * A_SW(I,1) *(H_SW(I,1)+BS_SW(I,1)-H_BOX(I,1)-BS_BOX(I,1))/(DX_SW(I)/2.0D0 + DX_MN )
!		U3 = -GG * RN_SW(I)**2 * ABS(QO_SW(I,1)) / 2.0D0 / A_SW(I,1) / ri **(4.0D0/3.0D0)
		q_sw(i, 1) = (u2 * dt2 + (1.0d0 + dt2 * u3) * qo_sw(i, 1)) / (1.0d0 - dt2 * u3)
		
	
  311   qsum_mh(mh) = qsum_mh(mh) -Q_SW(I,1) 
        hsum_mh(mh) = hsum_mh(mh) + h_sw(i, 1)
if(i==3200) write(2000,2000) mode, time, i, j, r_box(i,1),r_sw(i,1),  (r_box(i, 1) + r_sw(i, 1))/2.0d0, ri, u2, u3, q_sw(i,1), h_box(i,1)+bs_box(i,1), h_sw(i,1)+bs_sw(i,1)

2000 format (i5, 3x, F10.3, 3X, i5, 3x, i5, 3x, 20(f10.5, 3x))
	
	end subroutine upq_diff

end module sub_upq_diff	



 