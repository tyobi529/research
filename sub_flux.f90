	Module sub_flux
		use globals
		use subprogs
		implicit none
		contains
!     ******************************************************************
!                           Runoff flow calculation
!     ******************************************************************
!     ====================================
!              Momentum Equation
!     ====================================
      subroutine flux
      real(8) hhe, hhw, hhep, hhwp, hhd, hhan, sgn, hh1, hh2
      real(8) u11, v11, u13, v13, dx, dy, dl, sqx, rnx, ram
      real(8) sumf, rr, rnof, h1, dv, vzan
      integer li, k, k2, me1, me2
      integer me, it, ln1, ln2

      integer i, j
      real(8) ame(1000,1000)
      character(len=50) inputname

      do li = 1, link
      if(limesh(li, 2) == 0) goto 300
      if(inf(limesh(li, 2)) == 0) goto 300
      if(inf(limesh(li, 1)) == 0) goto 300
      if(rbeta(li) == 0.0d0) goto 300
      if(h(limesh(li, 1)) <= th .and. h(limesh(li, 2)) <= th) goto 300
         hhe = h(limesh(li, 2)) + baseo(limesh(li, 2))
         hhw = h(limesh(li, 1)) + baseo(limesh(li, 1))
         hhep = h(limesh(li, 2)) - th
         hhwp = h(limesh(li, 1)) - th
!     ++++++++++++++++++++++++++++++++
!         水面が不連続
!     ++++++++++++++++++++++++++++++++
!     段落ち式
      if(hhe < baseo(limesh(li, 1))) then
       if(h(limesh(li, 1)) > th) then
        um(li) = 0.544d0*h(limesh(li, 1))*sqrt(gg*h(limesh(li, 1)))*ux(li)
        vn(li) = 0.544d0*h(limesh(li, 1))*sqrt(gg*h(limesh(li, 1)))*uy(li)
       else
        um(li) = 0.0d0
        vn(li) = 0.0d0
       endif
        lhan(li) = 1
        goto 301
      elseif (hhw < baseo(limesh(li, 2))) then
       if(h(limesh(li, 2)) > th) then
        um(li) = -0.544d0*h(limesh(li, 2))*sqrt(gg*h(limesh(li, 2)))*ux(li)
        vn(li) = -0.544d0*h(limesh(li, 2))*sqrt(gg*h(limesh(li, 2)))*uy(li)
       else
        um(li) = 0.0d0
        vn(li) = 0.0d0
       endif
        lhan(li) = 1
        goto 301
!     越流公式
      elseif(hhep*hhwp < 0.0d0) then
        if(h(limesh(li, 2)) > 0.0d0 .or. h(limesh(li, 1)) > 0.0d0) then
          hhan = hhep - hhwp
          sgn = hhan/abs(hhan)
          hh1 =   max(h(limesh(li, 2))+baseo(limesh(li, 2)), h(limesh(li, 1))+baseo(limesh(li, 1))) &
                - max(baseo(limesh(li, 2)), baseo(limesh(li, 1)))
            um(li) = -sgn*0.35d0*hh1*sqrt(2.0d0*gg*hh1)*ux(li)
            vn(li) = -sgn*0.35d0*hh1*sqrt(2.0d0*gg*hh1)*uy(li)
        else
            um(li) = 0.0d0
            vn(li) = 0.0d0
        endif
        lhan(li) = 1
        goto 301

         if(isnan(um(li)) .eqv. .TRUE.) then
         pause "2 error"
         endif

      endif
!     ++++++++++++++++++++++++++++++++
!         　水面が連続
!     ++++++++++++++++++++++++++++++++
!     ========================
!         convective terms
!     ========================
        u11 = 0.0d0
        v11 = 0.0d0
				! メッシュの辺の数だけループ
        do k = 1, ko(limesh(li, 1))
          if(lhano(melink(limesh(li, 1), k)) == 1) goto 321
          if(melink(limesh(li, 1), k) == li) goto 321
					! 同一メッシュで他のノードの番号
          k2 = mod(k, ko(limesh(li, 1))) + 1
					! ノード間の距離の計算
          dx = dnox(menode(limesh(li, 1), k2)) - dnox(menode(limesh(li, 1), k))
          dy = dnoy(menode(limesh(li, 1), k2)) - dnoy(menode(limesh(li, 1), k))
	    if(uu(melink(limesh(li, 1), k))*dy > 0.0d0) then
                 me1 = limesh(li, 1)
	    else
            if(limesh(li, 1) == limesh(melink(limesh(li, 1), k), 1)) me1 = limesh(melink(limesh(li, 1), k), 2)
            if(limesh(li, 1) == limesh(melink(limesh(li, 1), k), 2)) me1 = limesh(melink(limesh(li, 1), k), 1)
	    endif
!
	    if(vv(melink(limesh(li, 1), k))*dx <0.0d0) then
                 me2 = limesh(li, 1)
	    else
            if(limesh(li, 1) == limesh(melink(limesh(li, 1), k), 1)) me2 = limesh(melink(limesh(li, 1), k), 2)
            if(limesh(li, 1) == limesh(melink(limesh(li, 1), k), 2)) me2 = limesh(melink(limesh(li, 1), k), 1)
	    endif
					! 各辺におけるフラックス（リンク中点の流速×各辺の長さ※xy方向に分解）×図心の流速
          u11 = u11 + uu(melink(limesh(li, 1), k))*umm(me1)*dy - vv(melink(limesh(li, 1), k))*umm(me2)*dx
          v11 = v11 + uu(melink(limesh(li, 1), k))*vnm(me1)*dy - vv(melink(limesh(li, 1), k))*vnm(me2)*dx
  321   enddo
!
        do k = 1, ko(limesh(li, 2))
          if(lhano(melink(limesh(li, 2), k)) == 1) goto 322
          if(melink(limesh(li, 2), k) == li) goto 322
          k2 = mod(k, ko(limesh(li, 2))) + 1
          dx = dnox(menode(limesh(li, 2), k2)) - dnox(menode(limesh(li, 2), k))
          dy = dnoy(menode(limesh(li, 2), k2)) - dnoy(menode(limesh(li, 2), k))
	    if(uu(melink(limesh(li, 2), k))*dy > 0.0d0) then
                 me1 = limesh(li, 2)
	    else
            if(limesh(li, 2) == limesh(melink(limesh(li, 2), k), 1)) me1 = limesh(melink(limesh(li, 2), k), 2)
            if(limesh(li, 2) == limesh(melink(limesh(li, 2), k), 2)) me1 = limesh(melink(limesh(li, 2), k), 1)
	    endif
!
	    if(vv(melink(limesh(li, 2), k))*dx < 0.0d0) then
                 me2 = limesh(li, 2)
	    else
            if(limesh(li, 2) == limesh(melink(limesh(li, 2), k), 1)) me2 = limesh(melink(limesh(li, 2), k), 2)
            if(limesh(li, 2) == limesh(melink(limesh(li, 2), k), 2)) me2 = limesh(melink(limesh(li, 2), k), 1)
	    endif
			! リンク中点の流速×重心のフラックス×ノード間の距離
          u11 = u11 + uu(melink(limesh(li, 2), k))*umm(me1)*dy - vv(melink(limesh(li, 2), k))*umm(me2)*dx
          v11 = v11 + uu(melink(limesh(li, 2), k))*vnm(me1)*dy - vv(melink(limesh(li, 2), k))*vnm(me2)*dx
  322   enddo
        u11 = u11*dt2/scv(li)
        v11 = v11*dt2/scv(li)
!     ===============
!         重力項
!     ===============
				! リンクが接する2つのメッシュの距離
        dx = xmesh(limesh(li, 2)) - xmesh(limesh(li, 1))
        dy = ymesh(limesh(li, 2)) - ymesh(limesh(li, 1))
        dl = sqrt(dx**2 + dy**2)
        if(dl /= 0.0d0) then
          u13 = gg*hl(li)*dt2/dl*ux(li)*(h(limesh(li, 2)) + baseo(limesh(li, 2)) - h(limesh(li, 1)) - baseo(limesh(li, 1)))
          v13 = gg*hl(li)*dt2/dl*uy(li)*(h(limesh(li, 2)) + baseo(limesh(li, 2)) - h(limesh(li, 1)) - baseo(limesh(li, 1)))
        else
          u13 = 0.0d0
          v13 = 0.0d0
        endif
!     ===============
!        せん断項
!     ===============
        sqx = sqrt(uu(li)**2 + vv(li)**2)
        rnx = 0.5d0*(mn(limesh(li, 2)) + mn(limesh(li, 1)))
        ram = gg*rnx**2*sqx/hl(li)**1.333333
        if(hl(li) <= th) ram = 0.0d0
!     ===============================
!           um, vn calculation
!     ===============================
!      u11=0.0d0
!      v11=0.0d0
      um(li) = ((1.0d0 - dt2*ram*(1.0d0 - fita))*umo(li) - u11 - u13)/(1.0d0 + dt2*ram*fita)
      vn(li) = ((1.0d0 - dt2*ram*(1.0d0 - fita))*vno(li) - v11 - v13)/(1.0d0 + dt2*ram*fita)
      lhan(li) = 0
      goto 301
!
  300 um(li) = 0.0d0
      vn(li) = 0.0d0
      lhan(li) = 1
  301 enddo
      return
!
!     ******************************************************************
!     ====================================
!             Continuity Equation
!     ====================================
      entry suisin(rnof)

      it = int(time/dtrain) + 1
!小栗栖用
        if(it>=1261) goto 756
      write(inputname,4010) it
        4010format('inputdata/rain/OGURISU-',i4.4,'rain.dat')
        open(510,file=inputname,action='read')
            do j=646,654
            do i=461,471
                read(510,4020) ame(i,j)
            enddo
            enddo
        close(510)
        4020format(f10.3)
      756 continue
      do me = 1, mesh
        if(inf(me) == 0) goto 402
!       ------------------------
!         フラックスによる流入
!       ------------------------
           sumf = 0.0d0
        do k = 1, ko(me)
           k2 = mod(k, ko(me)) + 1
           dx = dnox(menode(me, k2)) - dnox(menode(me, k))
           dy = dnoy(menode(me, k2)) - dnoy(menode(me, k))
           umbeta(melink(me, k)) = um(melink(me, k))*rbeta(melink(me, k))
           vnbeta(melink(me, k)) = vn(melink(me, k))*rbeta(melink(me, k))
           sumf = sumf + umbeta(melink(me, k))*dy - vnbeta(melink(me, k))*dx
        enddo
!       ------------------------
!             雨量の計算
!       ------------------------
        !rr = rain(me,it)*rnof*0.75d0
        ! *(1.0d0 - lambda(me))  ! 됄돫궔귞궻봱릣똭뱷귩빶뱑뛩궑?E????E

!小栗栖用
        if(time<33000.0d0) rr = ame(ii(me), jj(me))*rnof*0.22d0
        if(time>=33000.0d0 .and. time<43800.0d0) rr = ame(ii(me), jj(me))*rnof*0.40d0
        if(time>=43800.0d0 .and. time<75600.0d0) rr = ame(ii(me), jj(me))*rnof*0.74d0
        if(time>=75600.0d0) rr=0.0d0


!       ------------------------
!              h　計算
!       ------------------------


      do j=1,10
        if(me==con_mesh(j)) then  !connection (畑川)
        h(me)=ho(me) - dt2*(sumf/smesh(me)/(1.0d0 - lambda(me)))    &
                       + rr/1000.0d0/dtrain*dt2/(1.0d0 - lambda(me))  &
                       + qlme(me)*dt2/(1.0d0 - lambda(me))  &
                       +q_con(con_mh(j))*dt2/smesh(me)/(1.0d0 - lambda(me))
        h(me) = max(h(me), 0.0d0)
        goto 402
        endif
      enddo

!      if(me==31315 .or. me==31443) then  ! qq(it)
!       h(me) = ho(me) - dt2*(sumf/smesh(me)/(1.0d0 - lambda(me)))    &
!                       + rr/1000.0d0/dtrain*dt2/(1.0d0 - lambda(me))  &
!                       + qlme(me)*dt2/(1.0d0 - lambda(me))  &
!                       -qq(it)/2.0d0/smesh(me)*dt2/(1.0d0 - lambda(me))
!
!      else
        h(me) = ho(me) - dt2*(sumf/smesh(me)/(1.0d0 - lambda(me)))    &
                       + rr/1000.0d0/dtrain*dt2/(1.0d0 - lambda(me))  &
                       + qlme(me)*dt2/(1.0d0 - lambda(me))
        h(me) = max(h(me), 0.0d0)

!      endif

  402 enddo
 !      ------------------------ hh(it)
!       it = int(time/dtrain) + 1
!       h(31315) = hh(it)-baseo(31315)
!       h(31443) = hh(it)-baseo(31443)
!       h(31315) = max(h(31315),0.0d0)
!       h(31443) = max(h(31443),0.0d0)

       h(31315) = h(31315)-qq(it)/2.0d0/smesh(31315)*dt2/(1.0d0 - lambda(me))
       h(31443) = h(31443)-qq(it)/2.0d0/smesh(31443)*dt2/(1.0d0 - lambda(me))
       h(31315) = max(h(31315),0.0d0)
       h(31443) = max(h(31443),0.0d0)

      end subroutine flux

end Module sub_flux
