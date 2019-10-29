!
!
!


!     #######################################################
!     ##                                                   ##
!     ##                   main program                    ##
!     ##                                                   ##
!     #######################################################
!
!
!
program main
  ! 変数の定義
  use globals
  ! 関数STORM_BOX,MH_MESH,PM_MESH,RCAT,SUISIN_BOX
  use subprogs
  use sub_rdt
  use sub_initiald
  use sub_flux
  use sub_fluxsw
  use sub_hyd_graph
  use sub_connect_1
  use sub_connect_2
  use sub_write
  use sub_write_tec
  use sub_mdvel
  use sub_forward
  use sub_connection
  use sub_upq_diff
  use sub_dwq_diff
  use sub_confirm

   implicit none
   ! tday:計算日数 thour:時間 tmin:分 tsec:秒
      real(8) tday, thour, tmin, tsec, dkout, dpout
      real(8) time0, timmax
      real(8) qout, vrain, vzan, dx, dy, rnof
      integer me, li, k, k2, i, j
      integer kkk, lpout, lkout
      INTEGER RFSN
      character*50  fnd, fcndt, fvr, fhar, fbs, frn, fmsh, flnk, fpm, fh, fuu, fvv, fswr
      character*50  fn(2)

 integer t1, t2, t_rate, t_max, diff

 ! 基準地点からの時間を取得（a,b,c）a:時間 b:1秒当たりのカウント c:最大のカウント
  call system_clock(t1)   ! �J�n�����L�^

!     =====================================================
!              CHOOSING OF RAINFALL GAUGE STATION
!     =====================================================

! NAKAHAMA_BENTEN.DATのファイルがない？
      data fn/ 'READFILE/ogurisu.dat', 'READFILE/NAKAHAMA_BENTEN.DAT'/
      ! (*,は画面への出力*)は形式基本的に*でよい
	  WRITE(*,*) 'CHOOSE A RAINFALL GAUGE STATION DATA (1. OSAKA, 2. BENTEN) : '
    ! キーボードから入力されたデータ整数2桁をRFSNに割り当て
      READ(*,2000) RFSN
2000 FORMAT(I2)
!     =====================================================
!                      �p�����[�^����
!     =====================================================
      do kkk = 1, RFSN

        ! 読み込み専用でfn(kkk)を開き10番を割り当て
      open(10, file = fn(kkk), action = 'read')
      read(10, 1001) tday, thour, tmin, tsec
      read(10, 1002) dt
      read(10, 1002) dkout
      read(10, 1002) dpout
      read(10, 1002) rnof
      read(10, *)
!     =====================================================
!                        �t�@�C����
!     =====================================================
! それぞれのファイルを格納
      read(10, 1009) fnd
      read(10, 1009) fcndt
      read(10, 1009) fvr
      read(10, 1009) fhar
      read(10, 1009) fbs
      read(10, 1009) frn
      read(10, 1009) fmsh
      read(10, 1009) flnk
      read(10, 1009) fpm
      read(10, 1009) fh
      read(10, 1009) fuu
      read(10, 1009) fvv
      read(10, 1009) fswr
 1001 format(7x, 4f6.1)
 1002 format(7x, f11.3)
 1009 format(a50)
 WRITE(*,*) FRN
      close(10)
!     ===============================================
      time0 = 0.0d0
      timmax = 3600.0d0*24.0d0*tday + 3600.d0*thour + 60.d0*tmin + tsec
      dt2 = 2.0d0*dt
      th = 1.0d-3
      gg = 9.8d0
      fita = 0.5d0
      pi = 4.0d0*atan(1.0d0)
!     =====================================================
!                        �������ݎ���
!     =====================================================
      lpout = int(dpout/dt)
      lkout = int(dkout/dt)
!     =====================================================
!                     �f�[�^�t�@�C�����J��
!     =====================================================
!     ------------------------------- ���̓t�@�C��
      open(11, file = fnd, action = 'read')
      open(12, file = fcndt, action = 'read')
      open(13, file = fvr, action = 'read')
      open(14, file = fhar, action = 'read')
      open(15, file = fbs, action = 'read')
 !     open(16, file = frn, action = 'read')
      open(17, file = fmsh, action = 'read')
      open(18, file = flnk, action = 'read')
!      open(32, file = fpm, action = 'read')
!!     ------------------------------- �o�̓t�@�C��
      open(91, file = fh, action = 'write')
      open(96, file = fuu, action = 'write')
      open(97, file = fvv, action = 'write')
      open(98, file = fswr, action = 'write')
      open(99, file ='out/ogurisu_haisui.dat', action = 'write')
      open(92, file ='out/ogurisu_q_con.dat', action = 'write')
      open(93, file ='out/ogurisu_q_con_total.dat', action = 'write')


      open (500,file='inputdata/rain_mesh.dat',action='read')
!      open (501,file='inputdata/h_t_yamashina.dat',action='read')
      open (502,file='inputdata/q_t_drain.dat',action='read')
      open (503,file='inputdata/connection.dat',action='read')
 IF(KKK==1)THEN  ! OSAKA RAINFALL STATION
      open(77, file = 'SEWER_OUT/ME_TO_SD.DAT', action = 'write')
      OPEN(78, FILE = 'SEWER_OUT/ME_TO_SD(LINE).DAT', ACTION='WRITE')
      OPEN(80, FILE = 'OUT/HYDROGRAPH(OSAKA).DAT', ACTION='WRITE')
      OPEN(81, FILE = 'OUT/ANI_HYDROGRAPH(OSAKA).DAT', ACTION='WRITE')
      open(100, file='OUT/ogurisu_inundation.dat', action = 'write')
      OPEN(101, FILE='OUT/inundation_area.dat', ACTION='WRITE')
      OPEN(102, FILE='OUT/inundation_volume.dat', ACTION='WRITE')
      OPEN(110, FILE='OUT/STORM DRARINS POSITION-SDM-2.DAT',ACTION='WRITE')
      OPEN(120, FILE='SEWER_OUT/P_B_PUMP.DAT', ACTION='WRITE')
      OPEN(191, FILE='SEWER_OUT/PIPE GRID.dat', ACTION='WRITE')
      OPEN(192, FILE='SEWER_OUT/ALL PIPES(ELE).DAT', ACTION='WRITE')
      OPEN(193, FILE='SEWER_OUT/ALL PIPES(DEPTH).DAT', ACTION='WRITE')
      OPEN(200, FILE='SEWER_OUT/MANHOLE_GRID.dat', ACTION='WRITE')
      OPEN(300, FILE='SEWER_OUT/PUPMING STATION_GRID.DAT', ACTION='WRITE')
      OPEN(310, FILE='SEWER_OUT/EXCHANGE DISCHARGE.DAT', ACTION='WRITE')
      OPEN(320, FILE='SEWER_OUT/INLET Q TO PUMP.DAT', ACTION='WRITE')
      OPEN(400, FILE='SEWER_OUT/STORM DRAIN BOX(H).DAT', ACTION='WRITE')

      OPEN(1000, FILE='SEWER_OUT/PIPE TO PUMPING STATION INLET).DAT', ACTION='WRITE')
      OPEN(2000, FILE='SEWER_OUT/TEST.DAT', ACTION='WRITE')

 ELSEIF(KKK==2)THEN  ! BENTEN RAINFALL STATION
      open(77, file = 'SEWER_OUT_BENTEN/ME_TO_SD.DAT', action = 'write')
      OPEN(78, FILE = 'SEWER_OUT_BENTEN/ME_TO_SD(LINE).DAT', ACTION='WRITE')
      OPEN(80, FILE = 'OUT_BENTEN/HYDROGRAPH(BENTEN).DAT', ACTION='WRITE')
      OPEN(81, FILE = 'OUT_BENTEN/ANI_HYDROGRAPH(BENTEN).DAT', ACTION='WRITE')
      open(100, file='OUT_BENTEN/INUNDATION(N_RN_NHM-SDM-2).DAT', action = 'write')
      OPEN(101, FILE='OUT_BENTEN/INUNDATION AREAS.DAT', ACTION='WRITE')
      OPEN(110, FILE='OUT_BENTEN/STORM DRARINS POSITION-SDM-2.DAT',ACTION='WRITE')
      OPEN(120, FILE='SEWER_OUT_BENTEN/P_B_PUMP.DAT', ACTION='WRITE')
      OPEN(191, FILE='SEWER_OUT_BENTEN/PIPE GRID.DAT', ACTION='WRITE')
      OPEN(192, FILE='SEWER_OUT_BENTEN/ALL PIPES(ELE).DAT', ACTION='WRITE')
      OPEN(193, FILE='SEWER_OUT_BENTEN/ALL PIPES(DEPTH).DAT', ACTION='WRITE')
      OPEN(200, FILE='SEWER_OUT_BENTEN/MANHOLE_GRID.DAT', ACTION='WRITE')
      OPEN(300, FILE='SEWER_OUT_BENTEN/PUPMING STATION_GRID.DAT', ACTION='WRITE')
      OPEN(310, FILE='SEWER_OUT_BENTEN/EXCHANGE DISCHARGE.DAT', ACTION='WRITE')
      OPEN(320, FILE='SEWER_OUT_BENTEN/INLET Q TO PUMP.DAT', ACTION='WRITE')
      OPEN(400, FILE='SEWER_OUT_BENTEN/STORM DRAIN BOX(H).DAT', ACTION='WRITE')

      OPEN(1000, FILE='SEWER_OUT_BENTEN/PIPE TO PUMPING STATION INLET).DAT', ACTION='WRITE')
      OPEN(2000, FILE='SEWER_OUT_BENTEN/TEST.DAT', ACTION='WRITE')

ENDIF
 !     OPEN(1000, FILE='SEWER_OUT_BENTEN/STORM BOX TEST.DAT', ACTION='WRITE')




!     =====================================================
!                       �f�[�^�ǂݍ���
!     =====================================================
      CALL DISKP_INITIAL
      call rdat
      CALL MH_MESH    ! MANHOLE MESH GENERATION
      CALL PM_MESH ! PUMPING STATION MESH GENERATION
      CALL TEC_GROUND
      CALL TEC_PIPEGRID
      CALL TEC_MH
!      CALL TEC_PM
      CALL STORM_BOX
!      CALL HYD_GRPH(DKOUT)
      CALL CONFIRMATION


!     =====================================================
!                         ��������
!     =====================================================
      call initiald
!     =====================================================
      time = time0
      mstep = 0
      vrain = 0.0d0
      qout = 0.0d0
!     =====================================================

      call diskwrite
      call dispwrite(vrain, qout)

!     +++++++++++++++++++++++++++++++++++++++++++++++++++++
!                       loop start��
!     +++++++++++++++++++++++++++++++++++++++++++++++++++++
!     $$$$$$$$$$$$$$$$$$$$
!          �^��������
!     $$$$$$$$$$$$$$$$$$$$

1	  call flux
!1  continue
!write(*,*)'------------------------------'
!      call flux
      call fluxsw
      CALL CONNECT_1
      CALL CONNECT_2

!     =====================
!        先端の処理
!     =====================
      do me = 1, mesh
      if(h(me) >= th) goto 200
      do k = 1, ko(me)
        k2 = mod(k, ko(me)) + 1
        li = melink(me, k)
        dx = dnox(menode(me, k2)) - dnox(menode(me, k))
        dy = dnoy(menode(me, k2)) - dnoy(menode(me, k))
        if((um(li)*dy - vn(li)*dx) > 0.0d0) then
          um(li) = 0.0d0
          vn(li) = 0.0d0
        endif
      enddo
  200 enddo

      do i = 1, iswr
      do j = 1, jswr(i)
        if(h_sw(i, j) >= th) goto 210
        if(q_sw(i, j)   < 0.0d0) q_sw(i, j)   = 0.0d0
        if(q_sw(i, j+1) > 0.0d0) q_sw(i, j+1) = 0.0d0
  210 enddo
      enddo
!     ====================
!         time step
!     ====================
      time = time + dt
      mstep = mstep + 1
!     $$$$$$$$$$$$$$$$$$$$
!            �A����
!     $$$$$$$$$$$$$$$$$$$$
      call suisin(rnof)
      call suisinsw
      CALL SUISIN_BOX

!     =================================================================
!              �����̎Z�o
!     =================================================================
	  call mdvel
      CALL RCAT                   ! REDUCTION OF CONVECTIVE ACCELERATION TERM
      call forward
      call sumrain(vrain)
!     ====================
!         time step
!     ====================
      time = time + dt
      mstep = mstep + 1
!     =================================================================
!              �o��
!     =================================================================
      if(mod(mstep, lkout) == 0) call diskwrite
      if(mod(mstep, lpout) == 0) then
      call dispwrite(vrain, qout)
      if(mod(mstep, LKOUT) == 0) CALL WRUNDER
!     =================================================================
!              time judging
!     =================================================================
        call system_clock(t2, t_rate, t_max)
            if ( t2 < t1 ) then
            diff = t_max - t1 + t2
        else
            diff = t2 - t1
        endif
        print "(A, F10.3)", "time it took was:", diff/dble(t_rate)
        endif
      if(time + dt <= timmax) goto 1
!     +++++++++++++++++++++++++++++++++++++++++++++++++++++
!                          loop end
!     +++++++++++++++++++++++++++++++++++++++++++++++++++++
      call diskwrite
      write(*, 1999) time, timmax
 1999 format('      - normal end -  time=',f8.0,'  timmax=',f8.0)

  enddo
!

 call system_clock(t2, t_rate, t_max)   ! �I�������L�^
  if ( t2 < t1 ) then
    diff = t_max - t1 + t2
  else
    diff = t2 - t1
  endif
  print "(A, F10.3)", "time it took was:", diff/dble(t_rate)

  pause '�v�Z�I��'

end program main
