	module sub_dwq_diff
	
	use globals
	use subprogs
	use sub_sentan

	implicit none
	contains
	
!     ===============================
!              Cal dwp
!     ===============================

	subroutine dwq_diff(i)
		
		integer mh, ist, i, j, mode
		real(8) h_up, bs_up, h_dw, bs_dw, unitq
		real(8) dx_mn, ri, u2, u3
!------------------------------------------------------		
         mh = mhdw_sw(i)                                                                                        
!       ------------------------                                                                               
!             ?»ç•‡?E???                                                                                        
!       ------------------------                                                                               

        H_DW  = H_box(i,2)  ; H_UP  = H_SW(I, JSWR(I))
        BS_DW = BS_box(i,2) ; BS_UP = BS_SW(I, JSWR(I))
		                                                                                                           
		call sentan(h_up, h_dw, bs_up, bs_dw, unitq, ist)    
		        mode = 0
                                                     
        if(ist == 1) then                                                                                      
          q_sw(i, jswr(i)+1) = unitq * dd1_sw(i)   
          mode =2                                                            
          goto 312                                                                                             
		ENDIF                                                                                                      
		                                                                                                           
!     --------------------------------
!       DOWNSTREAM (DIFFUSIVE EQUATION)
!     --------------------------------mhdw_sw(i)
! PARAMETER DEFINATION

		DX_MN = SQRT(AREA_MH(MH)/PI)
        ri = (r_box(i, 2) + r_sw(i, jswr(i)))/2.0d0
		U2 = -GG * (A_BOX(I,2) + A_SW(I,JSWR(I)))/2.0D0 *(H_BOX(I,2)+BS_BOX(I,2)-H_SW(I,JSWR(I))-BS_SW(I,JSWR(I)))/(DX_SW(I)/2.0D0 + DX_MN )
		U3 = -GG * RN_SW(I)**2 * ABS(QO_SW(I,JSWR(I))) / 2.0D0 /( (A_BOX(I,2) + A_SW(I,JSWR(I)))/2.0D0 )/ ri **(4.0D0/3.0D0)

!        ri = r_sw(i, jswr(i))
!		U2 = -GG * A_SW(I,JSWR(I)) *(H_BOX(I,2)+BS_BOX(I,2)-H_SW(I,JSWR(I))-BS_SW(I,JSWR(I)))/(DX_SW(I)/2.0D0 + DX_MN )
!		U3 = -GG * RN_SW(I)**2 * ABS(QO_SW(I,JSWR(I))) / 2.0D0 /A_SW(I,JSWR(I))/ ri **(4.0D0/3.0D0)
		
		Q_SW(I,JSWR(I)+1) = (u2 * dt2 + (1.0d0 + dt2 * u3) * qo_sw(i, JSWR(I)+1)) / (1.0d0 - dt2 * u3)       
		
  312   qsum_mh(mh) = qsum_mh(mh) + Q_SW(I,JSWR(I)+1)                                                          
        hsum_mh(mh) = hsum_mh(mh) + h_sw(i, jswr(i) + 1)
if(i==3200) write(2000,2000)mode, time, i, j, r_box(i,2), r_sw(i,jswr(i)),  (r_box(i, 2) + r_sw(i, jswr(i)))/2.0d0, ri, u2, u3, q_sw(i,jswr(i)+1), h_box(i,2)+bs_box(i,2), h_sw(i,jswr(i))+bs_sw(i,jswr(i))
1001 FORMAT ('1', 3X, F7.2, 3X, I5, 3X, 20(F8.4,3X))     
1002 FORMAT ('2', 3X, F7.2, 3X, I5, 3X, 20(F8.4,3X))     
2000 format (i5, 3x, F10.3, 3X, i5, 3x, i5, 3x, 20(f10.5, 3x))
!     ===============================

  
  end subroutine dwq_diff
  
 end module sub_dwq_diff