	module sub_write
	
	use globals
	use subprogs
	
	implicit none
	contains
	
!   =======================================================
!        INITIALIZATION OF DISK PRINT FILES
!   =======================================================             
     
SUBROUTINE DISKP_INITIAL

WRITE(101,1000) 

1000 FORMAT( 5X, 'TIME', 19X, '1cm > h',7X, '10cm > h', 7X, '20cm > h', 7X, '30cm > h', 7X, '40cm > h', 7X, '50cm > h')

WRITE(77,1100) 

1100 FORMAT(3X, 'ME',3X, 'CN_I(ME)',3X, 'CN_J(ME)')



END SUBROUTINE DISKP_INITIAL   	
	
!     ******************************************************************
      subroutine sumqa(sv, sa, saj, ssw)
      real(8) sv, sa, saj, ssw
      real(8) vrain, rr
      integer me, it, i, j, np, n
      
      integer ix, jy
      real(8) ame(1000,1000)
      character(len=50) inputname
      real(8) rnof
!     ==============================
!               氾濫水量
!     ==============================
      sv = 0.0d0
      sa = 0.0d0
      saj = 0.0d0
      ssw = 0.0d0
      do me = 1, mesh	
        if(inf(me) == 0) goto 100
        sv = sv + h(me)*smesh(me)*(1.0d0 - lambda(me))
        if(h(me) > 1.0d-3) sa  = sa  + smesh(me)
        if(h(me) > 1.0d-3) saj = saj + smesh(me)*(1.0d0 - lambda(me))  ! ??뿦귩뛩궑궫궴궖궻?렲붌뿏?E?
  100 enddo
      return
!
!     ==============================
!              累積降雨量
!     ==============================
    entry sumrain(vrain)
        it = int(time/dtrain) + 1
         if(it>=1261) goto 757   
             write(inputname,4010) it
        4010format('inputdata/rain/OGURISU-',i4.4,'rain.dat')
        open(510,file=inputname,action='read')
            do jy=646,654
            do ix=461,471
                read(510,4020) ame(ix,jy)
            enddo
            enddo
        close(510)
        4020format(f10.3)
        757 continue  
      do me = 1, mesh
        if(inf(me) == 0) goto 220
      !  rr = rain(me,it)
        if(time<75600.0d0) rr = ame(ii(me), jj(me))
        if(time>=75600.0d0) rr=0.0d0
        
        vrain = vrain + rr/1000.d0/dtrain*dt2*smesh(me)*(1.0d0 - lambda(me))
  220 enddo
!
      end subroutine sumqa
!
!     ******************************************************************
      subroutine wrfile
      real(8) vrain, qout, sv, sa, saj, ssw, vzan
      REAL(8) IN_AREA1, IN_AREA10, IN_AREA20, IN_AREA30,IN_AREA40,IN_AREA50
      INTEGER H_P,  C_P
      integer me, i, j
!     =============================================
!                ディスプレイ表示
!     =============================================
      entry dispwrite(vrain, qout)
      call sumqa(sv, sa, saj, ssw)
      write(*, 1000) time, vrain, sv, AC_CNQ !qout
 1000 format('   time=', f8.0, '(s)', 5f15.2)
      return
!
!     =============================================
!             データファイルへの書き込み
!     =============================================
      entry diskwrite
      write(91, 1010) time
      write(91, 1011) (h(me), me = 1, mesh)
!
      write(96, 1010) time
      write(96, 1011) (uum(me), me = 1, mesh)
!
      write(97, 1010) time
      write(97, 1011) (vvm(me), me = 1, mesh)
             
      write(99, 1012) time, h(31315), h(31443)
 
      write(92, 1013) time, (q_con(con_mh(j)), j=1,10)
      
      write(93, 1013) time, (q_con_total(con_mh(j)), j=1,10)
      
      do j=1,10
        q_con_total(con_mh(j))=0.0d0
      enddo
      
 1010 format(' time=', f8.0, '(s)')
 1011 format(10f7.3)    
 1012 format(' time=', f8.0, '(s)', 2f7.3)  
 1013 format(' time=', f8.0, '(s)', 10f7.3)
 
 
!-----------------INUNDATED AREA----------------------
    IN_AREA1=0.0D0
    IN_AREA10=0.0D0
    IN_AREA20=0.0D0
    IN_AREA30=0.0D0
    IN_AREA40=0.0D0
    IN_AREA50=0.0D0

    write(100,*) 'VARIABLES = "X" , "Y", "Z", "U", "V", "DEPTH(h)",  "ELEVATION(H)"'
    WRITE(100,1230) TIME, NODE, MESH
    WRITE(100,*) 'VARSHARELIST=([1-3]=1), CONNECTIVITYSHAREZONE=1'
        
            DO I=1, MESH
                WRITE(100, 1208) UM(i)
            ENDDO
        
            DO I=1, MESH
                WRITE(100, 1208) VN(i)
            ENDDO
        
            DO I=1, MESH
                WRITE(100, 1205) H(I)
            ENDDO
        
            DO I=1, MESH
                WRITE(100, 1205) BASEO(I)+H(I)
            ENDDO
!-----------------INUNDATION DEPTH DATA START-----------------------------
        DO I=1, mesh
            if(mark(i) == 4) cycle
            IF(H(I)>=0.01D0)IN_AREA1=IN_AREA1+SMESH(I)
            IF(H(I)>=0.10D0)IN_AREA10=IN_AREA10+SMESH(I)
            IF(H(I)>=0.20D0)IN_AREA20=IN_AREA20+SMESH(I)
            IF(H(I)>=0.30D0)IN_AREA30=IN_AREA30+SMESH(I)
            IF(H(I)>=0.40D0)IN_AREA40=IN_AREA40+SMESH(I)
            IF(H(I)>=0.50D0)IN_AREA50=IN_AREA50+SMESH(I)
                       
        ENDDO
       
        WRITE(101, 1220) TIME, IN_AREA1, IN_AREA10, IN_AREA20, IN_AREA30, IN_AREA40, IN_AREA50

!-----------------INUNDATION DEPTH DATA END-----------------------------
    
1205 format(5f15.2) 
1206 FORMAT(3I8)
1207 FORMAT(5F15.2)
1208 FORMAT(F7.4)
1220 FORMAT( F10.2, '(S)', 5X,  7F15.1)
1230 FORMAT( 'ZONE T=', '"', F10.2, '"', ',', 3X, 'DATAPACKING=BLOCK',',', 3X, 'VARLOCATION=([3-7]=CELLCENTERED),',3X, 'NODES=',I6,',',3X, 'ELEMENTS=',I6, ',',  3X,'ZONETYPE=FETRIANGLE' )


!-----------------INUNDATION VOLUME-------------------------------------
volume=0.0d0
do i=1, mesh
volume = volume+(h(i)*smesh(i))
enddo

write (102, 1220)time, volume  
!--------------UPSTREAM PIPE OF EACH PUMPING STATION-----------------
    WRITE(120, 1990) TIME
    DO I=1, ISWR
    IF(MARK_SW_CHECK(I)==1)THEN
        IF(MHDW_SW(I)==345.OR.MHUP_SW(I)==345)THEN
         C_P=345
                    WRITE(120, 1991) C_P, (H_SW(I,J), J=1, JSWR(I))
                    WRITE(120, 1992) C_P, (H_SW(I,J)+BS_SW(I,J), J=1, JSWR(I))

        ELSEIF(MHDW_SW(I)==773.OR.MHUP_SW(I)==773)THEN
         C_P=773
                    WRITE(120, 1991) C_P, (H_SW(I,J), J=1, JSWR(I))
                    WRITE(120, 1992) C_P, (H_SW(I,J)+BS_SW(I,J), J=1, JSWR(I))

        ELSEIF(MHDW_SW(I)==1711.OR.MHUP_SW(I)==1711)THEN
         C_P=1711
                    WRITE(120, 1991) C_P, (H_SW(I,J), J=1, JSWR(I))
                    WRITE(120, 1992) C_P, (H_SW(I,J)+BS_SW(I,J), J=1, JSWR(I))

        ELSEIF(MHDW_SW(I)==4282.OR.MHUP_SW(I)==4282)THEN
         C_P=4282
                    WRITE(120, 1991) C_P, (H_SW(I,J), J=1, JSWR(I))
                    WRITE(120, 1992) C_P, (H_SW(I,J)+BS_SW(I,J), J=1, JSWR(I))

        ELSEIF(MHDW_SW(I)==4284.OR.MHUP_SW(I)==4284)THEN
         C_P=4284
                    WRITE(120, 1991) C_P, (H_SW(I,J), J=1, JSWR(I))
                    WRITE(120, 1992) C_P, (H_SW(I,J)+BS_SW(I,J), J=1, JSWR(I))

        ELSEIF(MHDW_SW(I)==4300.OR.MHUP_SW(I)==4300)THEN
         C_P=4300
                    WRITE(120, 1991) C_P, (H_SW(I,J), J=1, JSWR(I))
                    WRITE(120, 1992) C_P, (H_SW(I,J)+BS_SW(I,J), J=1, JSWR(I))

        ELSEIF(MHDW_SW(I)==4303.OR.MHUP_SW(I)==4303)THEN
         C_P=4303
                    WRITE(120, 1991) C_P, (H_SW(I,J), J=1, JSWR(I))
                    WRITE(120, 1992) C_P, (H_SW(I,J)+BS_SW(I,J), J=1, JSWR(I))

        ELSEIF(MHDW_SW(I)==4314.OR.MHUP_SW(I)==4314)THEN
         C_P=4314
                    WRITE(120, 1991) C_P, (H_SW(I,J), J=1, JSWR(I))
                    WRITE(120, 1992) C_P, (H_SW(I,J)+BS_SW(I,J), J=1, JSWR(I))
        ENDIF
   ENDIF
    ENDDO
    
1990 FORMAT('T=','"',F8.1,'"')
1991 FORMAT('ELE', 7X, I6, 3X, 20F6.2)
1992 FORMAT('DEPTH', 5X, I6, 3X, 20F6.2)


      return
!
      end subroutine wrfile
!     =============================================
!                UNDERGROUND PART WRITING
!     =============================================
SUBROUTINE WRUNDER
INTEGER I, J, ME
!--------PIPE GRID OUTPUT-----------------      
    WRITE(191,2100) TIME, NUM_PMAX, NUM_EM
    WRITE(191,*) 'VARSHARELIST=([1-4]=1), CONNECTIVITYSHAREZONE=1'

        DO I=1, ISWR
IF(MARK_SW_CHECK(I)==1)THEN        
            DO J=1, JSWR(I)
                WRITE(191,2020) H_SW(I,J)
            ENDDO
ENDIF            
        ENDDO
    
        DO I=1, ISWR
IF(MARK_SW_CHECK(I)==1)THEN                
            DO J=1, JSWR(I)
                WRITE(191,2020) BS_SW(I,J)+H_SW(I,J)
            ENDDO
ENDIF                        
        ENDDO
        
!--------------MANHOLE GRID OUTPUT-------------
    WRITE(200,1000)TIME, MH_NO, MH_EL
    WRITE(200,*)  'VARSHARELIST=([1-4]=1), CONNECTIVITYSHAREZONE=1'       
      
        
        DO I=1, MNHL
IF(MARK_MH_CHECK(I)==1)THEN        
         WRITE(200,1010) H_MH(I)
ENDIF         
        ENDDO
        
        DO I=1, MNHL
IF(MARK_MH_CHECK(I)==1)THEN        
         WRITE(200,1010) H_MH(I)+BS_MH(I)
ENDIF         
        ENDDO

!--------------PUMPING STATION GRID OUTPUT-------------
    WRITE(300,1000)TIME,  PM_NO, PM_EL
    WRITE(300,*)  'VARSHARELIST=([1-2]=1), CONNECTIVITYSHAREZONE=1'       
      
        
        DO I=1, IPUMP
         WRITE(300,1010) H_MH(MH_PM(I))
        ENDDO
        
        DO I=1, IPUMP
         WRITE(300,1010) H_MH(MH_PM(I))+BS_MH(MH_PM(I))
        ENDDO        

!--------------ALL PIPES WRITING---------------    
    WRITE(192,2040) TIME
    WRITE(193,2040) TIME
    DO I=1, ISWR
IF(MARK_SW_CHECK(I)==1)THEN        
            WRITE(192,2050) I, DST_SW(I), (BS_SW(I,J)+H_SW(I,J), J=1, JSWR(I))
            WRITE(193,2050) I, DST_SW(I), (H_SW(I,J), J=1, JSWR(I))
ENDIF            
    ENDDO

!----------ALL MANHOLES WRITING--------------

      WRITE(98, 2040)TIME
      do i = 1, mnhl
        write(98, 1982) i, h_mh(i), H_MH(I)+BS_MH(I)
      enddo
!----------EXCHANGE DISCHARGE--------------      
    WRITE(310,2200) TIME, AC_QP, AC_QM, AC_CNQ
    
!----------INLET DISCHARGE TO PUMPING STATION--------------      


 WRITE(320,2220) TIME, (Q_SW(IP(I),JP(I)), I=1, IPUMP)
!write(*,*) (ip(i), jp(i), i=1, ipump)        

!----------STORM DRAIN BOX--------------      

        WRITE(400,3200) TIME
        WRITE(400,3210) 
            DO I=1, SDBR
                WRITE(400,3220) I, CN_ME(I), CN_I(CN_ME(I)), CN_J(CN_ME(I)), H_SDB(I), BS_SDB(I)+H_SDB(I)-BASEO(CN_ME(I))+H(CN_ME(I))
            ENDDO


!----------------------------------------------
1000 FORMAT('ZONE T=','"', F8.2,'"', ',', 3X,  'NODES=', I7,',', 'ELEMENTS=', I7,',', 'DATAPACKING=BLOCK,', 1X, 'VARLOCATION=([3-6]=CELLCENTERED),', 'ZONETYPE=FEQUADRILATERAL') 
1010 FORMAT(10F15.3)
1020 FORMAT(4(I5,1X))        
1982 FORMAT(I10, F10.2, 5X, F10.2)
2010 FORMAT (F12.3, 3X, F13.3, 3X, I10)
2020 FORMAT (F12.4)
2030 FORMAT (4I8)
2040 FORMAT ('TIME(S)=', F10.3, 5X, '/DEPTH)/',10X, '/ELEVATION/')
2050 FORMAT (I5, F12.5, 120F10.3)
2100 FORMAT ( 'ZONE T=','"', F8.2,'"', ',' , 3X,   'NODES=', I10, ', ', 'ELEMENTS=', I10 ',' , 'DATAPACKING=BLOCK, VARLOCATION=([3-6]=CELLCENTERED), ZONETYPE=FEQUADRILATERAL')
2200 FORMAT (F10.3, 5X, 5F20.3)       
2220 FORMAT (10(F15.3,5X))
3200 FORMAT('TIME=', F10.2, '(S)')
3210 FORMAT('I', 10X, 'CN_ME(I)', 5X, 'CN_I(CN_ME(I))', 5X, 'CN_J(CN_ME(I))', 5X, 'H_SDB(I)', 5X, 'GRD-SDB(H)')
3220 FORMAT(4I10, 10F10.5)

END SUBROUTINE WRUNDER 

end module sub_write	