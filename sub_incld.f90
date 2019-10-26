	module sub_incld
	
	use globals
	use subprogs
	
	implicit none
	contains
	
!     *****************************************************
      subroutine incld(xh, yh, me_in)
      real(8) xh, yh, dist, a, b, sgn
      integer me, k, me_in, kflag, nr, nf
!
      do me = 1, mesh
        dist = sqrt((xh - xmesh(me))**2 + (yh - ymesh(me))**2)
        if(dist > 300.0d0) goto 100
!
        kflag = 1
        do k = 1, 3
            nr = menode(me, k)                                          ! rear node
            nf = menode(me, mod(k, 3) + 1)                                 ! fore node
!
          if(dnox(nf) == dnox(nr)) then
            if(dnoy(nr) < dnoy(nf) .and. dnox(nr) < xh) kflag = 0
            if(dnoy(nr) > dnoy(nf) .and. dnox(nr) > xh) kflag = 0
            if((dnox(nr) == xh) .and. ((dnoy(nf) - yh)*(dnoy(nr) - yh) < 0.0d0)) kflag = 2
          else
            a = (dnoy(nf) - dnoy(nr)) / (dnox(nf) - dnox(nr))
            b = -a*dnox(nr) + dnoy(nr)                                         ! equation of this side (y=ax+b)
            sgn = -(dnox(nf) - dnox(nr)) / abs(dnox(nf) - dnox(nr))            ! sign of outward normal vector
            if((a*xh + b - yh)*sgn < 0.0d0)  kflag = 0                         ! kflag=0 : this point exists on the right hand side
            if(((a*xh + b - yh)*sgn == 0.0d0) .and. ((dnox(nf) - xh)*(dnox(nr) - xh) < 0.0d0)) kflag = 2          ! kflag=2 : this point exists on this side
          endif
          if(kflag == 0) goto 100
        enddo
          me_in = me
          return
  100 enddo
      me_in = 0
!
      end subroutine incld
      
!     =======================================================
!                     SEARCHING ROAD MESH
!     =======================================================
      subroutine incld_ROAD(xh, yh, me_in)      
      real(8) xh, yh, a, b, sgn
      integer me, k, me_in, nr, nf, NUM, I, J
      INTEGER I1, I2, kflag(100000) 
      REAL(8) DIST(100000), JUDGE
!
    NUM=0
    DO ME=1, MESH
        IF(MARK(ME)==1) THEN
         NUM=NUM+1
         KFLAG(NUM)=ME
        ENDIF
    ENDDO
        
   DO I=1, NUM
         dist(I) = sqrt((xh - xmesh(KFLAG(I)))**2 + (yh - ymesh(KFLAG(I)))**2)
   ENDDO

    JUDGE=100000
    DO I=2, NUM
        I1=I-1 ; I2=I
        JUDGE=MIN(DIST(I1), DIST(I2), JUDGE)
    ENDDO
        

    DO I=1, NUM
        IF(JUDGE==DIST(I))  THEN
        ME_in = KFLAG(I)
        XH=XMESH(KFLAG(I))
        YH=YMESH(KFLAG(I))
        ENDIF
    ENDDO

      end subroutine incld_ROAD  

end module sub_incld	  