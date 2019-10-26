	
!     ##########################################################
!     ##                                                      ##
!     ##    뿬덃긾긢?E붌뿏됶먏긵??깋?E묈띲럖뭷뷿뭤?)       ##
!     ##                                                      ##
!     ##       2013.05                   by k.kawaike         ##
!     ##       2014.01          MODIFIED BY SEUNGSOO LEE      ##
!     ##########################################################
!
module globals
      integer, parameter :: nog = 300000, meg = 300000, lig = 300000, kg = 3
      integer, parameter :: imh = 5000, isw = 5000, jsw = 1000
      real(8) gg, dt, dt2, time, dtrain, th, fita, pi
      real(8) x_mh(imh), y_mh(imh), bs_mh(imh), area_mh(imh), A_MH(IMH)
      real(8) dst_sw(isw), rn_sw(isw), bsdw_sw(isw), slp_sw(isw), x_sw(isw, 100), y_sw(isw, 100), dx_sw(isw), bs_sw(isw, jsw), dd1_sw(isw), dd2_sw(isw)
      real(8) a_sw(isw, jsw), ao_sw(isw, jsw), h_sw(isw, jsw), ai_sw(isw, jsw), r_sw(isw, jsw), ql_sw(isw, jsw)
      real(8) q_sw(isw, jsw), qo_sw(isw, jsw), uu_sw(isw, jsw)
      real(8) h_mh(imh), ho_mh(imh), qsum_mh(imh), hsum_mh(imh)
      real(8) h_spc(2000), a_spc(2000), r_spc(2000)
      real(8) h(meg), ho(meg), hl(lig), hmax(meg)
      real(8) um(lig), umo(lig), umm(meg), uu(lig), vn(lig), vno(lig), vnm(meg), vv(lig)
      real(8) qlme(meg)
      real(8) baseo(meg)
      real(8) dnox(nog), dnoy(nog)
      real(8) smesh(meg), scv(lig), rthl(lig, 2), ux(lig), uy(lig)
      real(8) mn(meg), lambda(meg), rbeta(lig), umbeta(lig), vnbeta(lig)
      real(8) xmesh(meg), ymesh(meg), rtuv(meg, kg)
      real(8) rain(150000,540), q_pm(10)
      real(8) uum(meg), vvm(meg)
      integer mstep
      integer mnhl, iswr, jswr(isw)
      integer id_mh(imh), inf_sw(isw), igrp(isw), id_sw(isw), idup_mh(isw), iddw_mh(isw), idshp_sw(isw), ipt_sw(isw), mhup_sw(isw), mhdw_sw(isw)
      integer num(imh), isw_mh(imh, 10), inf_mh(imh), ngrp(imh)
      integer limesh(lig, 2), linode(lig, 2)
      integer inf(meg), lhan(lig), lhano(lig)
      integer ko(meg), menode(meg, kg), melink(meg, kg)
      integer NM_METOSW(ISW, JSW), mh_pm(10)
      integer mesh, link, node, ipump
      INTEGER MARK(MEG)
      INTEGER CN_I(MEG), CN_J(MEG)
      REAL(8) X_M(isw, jsw), Y_M(isw, jsw)
      REAL(8) XX(NOG), YY(NOG)
      INTEGER NUM_PG(NOG), NUM_PMAX, NUM_EM, CHECK(ISW,JSW), CHECK_MH(IMH), CHECK_QL(ISW,JSW)
      REAL(8) BSL(ISW), ALPHA(ISW,JSW)
      REAL(8) XX_MH(ISW,JSW), YY_MH(ISW,JSW), XX_PM(ISW,JSW), YY_PM(ISW,JSW), AC_CNQ, AC_QP, AC_QM
      INTEGER MH_NO, MH_EL, PM_NO, PM_EL       
      INTEGER IP(ISW), JP(ISW)
      REAL(8) SDB_X, SDB_W, BS_SDB(MEG), H_SDB(MEG), QL_SDB(MEG), HO_SDB(MEG)
      REAL(8) HMM(ISW,2), A_BOX(IMH,2), BS_BOX(IMH,2), H_BOX(IMH,2), r_box(imh, 2)
      INTEGER SDBR, CN_ME(MEG)      
      INTEGER MARK_MH(IMH,IMH), MARK_SW(JSW,ISW), MARK_SW_CHECK(ISW), MARK_MH_CHECK(IMH)
      INTEGER  irain
      integer ii(meg), jj(meg)
      real(8) hh(1560), qq(1560)
      real(8) volume
      integer grp_mh(jsw), no_mh(jsw), kubun_mh(jsw), shp_mh(jsw), shp_sw(jsw)
      real(8) bsup_sw(jsw)
      real(8) judge
      integer con_mh(10), con_mesh(10)
      real(8) q_con(isw),q_con_total(isw)
      real(8) hhh_mh, hhh_sw, hhh_vir, hhh_riv
    
 
      
      
end module globals

