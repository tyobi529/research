	module sub_fluxsw

	use globals
	use subprogs
	use sub_upq_diff
	use sub_dwq_diff
	use sub_upq_ori
	use sub_dwq_ori
	use sub_sentan

	implicit none
	contains
!     ******************************************************************
!                         Sewer Pipe calculation
!     ******************************************************************
!     ====================================
!            1D Momentum Equation
!     ====================================
      subroutine fluxsw
      real(8) hhe, hhw, hhep, hhwp, sgn, hh3
      real(8) u1, u1r, u1l, u2, u3, ri, rni, aa, dx_mn
      real(8) h_up, h_dw, bs_up, bs_dw, unitq, DV
      integer i, j, n, id, mh, ist, me
      INTEGER MODE
      REAL(8) A0
      REAL(8) ASB, LSB, B0, QQ, HHH, CD
!     !�p�C�v�̐��[�ȊO���v�Z


      do i = 1, iswr
        if(inf_sw(i) == 0) goto 100
        if(igrp(i) > 10) goto 100
        IF(MARK_SW_CHECK(I)/=1)GOTO 100
      do j = 2, jswr(i)
!     ===============
!      ���ʂ̕s�A��
!     ===============
				! h_sw=初期値０
        h_up  = h_sw(i, j - 1)  ; h_dw  = h_sw(i, j)
				! 隣り合うグリッドのそれぞれの標高
        bs_up = bs_sw(i, j - 1) ; bs_dw = bs_sw(i, j)



        call sentan(h_up, h_dw, bs_up, bs_dw, unitq, ist)
        if(ist == 1) then
					! 流量（単位幅流量×幅）
          q_sw(i, j) = unitq * dd1_sw(i)
          goto 303
        endif
!     ===============
!         �ڗ���
!     ===============
        u1r = uu_sw(i, j)*(qo_sw(i, j) + qo_sw(i, j + 1))/2.d0 + abs(uu_sw(i, j))*(qo_sw(i, j) - qo_sw(i, j + 1))/2.d0
        u1l = uu_sw(i, j - 1)*(qo_sw(i, j - 1) + qo_sw(i, j))/2.d0 + abs(uu_sw(i, j - 1))*(qo_sw(i, j - 1) - qo_sw(i, j))/2.d0
        u1 = ALPHA(I,J)*(u1r - u1l)/dx_sw(i)
!     ===============
!        �d�͍�
!     ===============
        u2 = -gg*(a_sw(i, j - 1) + a_sw(i, j))/2.0d0*(h_sw(i, j) + bs_sw(i, j) - h_sw(i, j - 1) - bs_sw(i, j - 1))/dx_sw(i)
!     ===============
!        �����f��
!     ===============
        ri = (r_sw(i, j - 1) + r_sw(i, j))/2.0d0
        u3 = -gg*rn_sw(i)**2*abs(qo_sw(i, j))/2.0d0/ai_sw(i, j)/ri**(4.0d0/3.0d0)
!     ===============================
!           qsw calculation
!     ===============================
        q_sw(i, j) = ((-u1 + u2)*dt2 + (1.0d0 + dt2*u3)*qo_sw(i, j))/(1.0d0 - dt2*u3)

         if(isnan(q_sw(i, j)) .eqv. .TRUE.) then
             write(*,*) i, h_up, h_dw
             pause "5 error"
         endif
        goto 303

!
  301   q_sw(i, j) = 0.0d0
  303 enddo
    ! �p�C�v�̐��[���v�Z
		call upq_ori(i)

		call dwq_ori(i)

    100 enddo

    do j=1,10
    ac_cnq = ac_cnq - q_con(con_mh(j))
    q_con_total(con_mh(j)) = q_con_total(con_mh(j)) + q_con(con_mh(j))
    enddo

      return

!
!     ******************************************************************
!     ====================================
!           1D Continuity Equation
!     ====================================
      entry suisinsw
!
      do i = 1, iswr
        if(inf_sw(i) == 0) goto 200
        if(igrp(i) > 10) goto 200
        IF(MARK_SW_CHECK(I)/=1)GOTO 200
        aa = 10.0d0
        if(idshp_sw(i) == 1) BSL(I) = gg * (dd1_sw(i)*dd2_sw(i))/aa**2     ! 長方形の場合
        if(idshp_sw(i) == 2) BSL(I) = gg * (pi*dd1_sw(i)**2/4.0d0)/aa**2     ! 円形の場合
      do j = 1, jswr(i)



!       ------------------------
!         �t���b�N�X�ɂ��闬��
!       ------------------------
        a_sw(i, j) = ao_sw(i, j) + dt2*(-(q_sw(i, j + 1) - q_sw(i, j))/dx_sw(i) + ql_sw(i, j))

        if(a_sw(i, j) < 0.0d0) a_sw(i, j) = 0.0d0

!       ---------------------
!              ���[�̌v�Z
!       ------------------------
!             ---------------------- ! REC�@���`�f��
        if(idshp_sw(i) == 1) then
          if(a_sw(i, j) <= (dd1_sw(i)*dd2_sw(i))) then
            h_sw(i, j) = a_sw(i, j)/dd1_sw(i)
          else
            h_sw(i, j) = dd2_sw(i) + (a_sw(i, j) - dd1_sw(i)*dd2_sw(i))/BSL(I)
          endif
!             ---------------------- ! CIR�@�~�`�f��
        elseif(idshp_sw(i) == 2) then
          IF(A_SW(I,J) <1.0D-6)THEN
            H_SW(I,J) = 0.0D0
          ELSEIF(a_sw(i, j) <= (pi*dd1_sw(i)**2/4.0d0)) then
            call atoh(i, j)
          else
            h_sw(i, j) = dd1_sw(i) + (a_sw(i, j) - pi*dd1_sw(i)**2/4.0d0)/BSL(I)

         if(h_sw(i,j)>30)then
         write(*,*)'sw', i, j, h_sw(i,j)
         endif

         if(isnan(h_sw(i, j)) .eqv. .TRUE.) then
         write(*,*) i, j, bsl(i)
         pause "6 error"
         endif

          endif
        endif
      enddo
  200 enddo


!     ===============================
!        calculation of manhole depth
!     ===============================
      do n = 1, mnhl
        if(inf_mh(n) == 0) goto 210
        if(ngrp(n) > 10) goto 210
        IF(MARK_MH_CHECK(N)==0) GOTO 210

!
        if(area_mh(n) > 0.0d0) then
          h_mh(n) = ho_mh(n) + dt2 * qsum_mh(n) / area_mh(n)
        else
          h_mh(n) = hsum_mh(n)/dble(num(n))
        endif
        h_mh(n) = max(h_mh(n), 0.0d0)

        do i=1,10
        h_mh(con_mh(i)) = h(con_mesh(i))
        h_mh(con_mh(i)) = max(h_mh(con_mh(i)), 0.0d0)
      enddo

        if(h_mh(n)>30) then
        write(*,*) 'mh', n,  h_mh(n)
        endif


        if(isnan(qsum_mh(n)) .eqv. .TRUE.) then
         write(*,*) n, ho_mh(n)
         pause "7 error"
         endif
  210 enddo


!        write(*,*) h_mh(108), h_sw(107,1), h_mh(7)



!     ===============================
!             Pumping Station
!     ===============================
      do i = 1, ipump
        n = mh_pm(i)
        dv = min(q_pm(i)*dt2, h_mh(n)*area_mh(n))
        h_mh(n) = h_mh(n) - dv/area_mh(n)

      enddo

!     ===============================
!              ABOX CALCULATION
!     ===============================

        DO I=1, ISWR
        IF(MARK_SW_CHECK(I)/=1)GOTO 300
            DO J=1, 2

            IF(J==1) THEN
                MH = MHUP_SW(I)
            ELSEIF(J==2) THEN
                MH = MHDW_SW(I)
            ENDIF

            H_BOX(I,J) = H_MH(MH) + BS_MH(MH) - BS_BOX(I,J)
            H_BOX(I,J) = MAX(H_BOX(I,J), 0.0D0)

            IF(IDSHP_SW(I)==1)THEN
                A0 = DD1_SW(I) * DD2_SW(I)
                IF(H_BOX(I,J) < TH)THEN
                    A_BOX(I,J) = 0.0D0
                ELSEIF(H_BOX(I,J)< DD2_SW(I))THEN
                    A_BOX(I,J) = H_BOX(I,J) * DD1_SW(I)
                ELSE
                    A_BOX(I,J) = A0 + (H_BOX(I,J) - DD2_SW(I)) * BSL(I)
                ENDIF
             ELSEIF(IDSHP_SW(I)==2)THEN
                A0 = PI * DD1_SW(I)**2.0D0 / 4.0D0
                IF(H_BOX(I,J) < TH)THEN
                    A_BOX(I,J) = 0.0D0
                ELSEIF(H_BOX(I,J) < DD1_SW(I))THEN
                    CALL HTOA(I,J)
                ELSE
                    A_BOX(I,J) = A0 + (H_BOX(I,J)-DD1_SW(I)) * BSL(I)
                ENDIF
             ELSE
                PAUSE
             ENDIF
             ENDDO
300     ENDDO
!
      end subroutine fluxsw
!     ******************************************************************

!

end module sub_fluxsw
