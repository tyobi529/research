	Module sub_write_tec
	 use globals
	 use subprogs
	 implicit none
	 contains
	 
!     =======================================================
!         TECPLOT BASIC DATA PRINT FOR GROUND
!     =======================================================
   SUBROUTINE TEC_GROUND
   INTEGER I,J, ME

    write(100,*) 'Title = "NAKAHAMA INUNDATION MODEL" '
    write(100,*) 'VARIABLES = "X" , "Y", "Z", "U", "V", "DEPTH(h)",  "ELEVATION(H)"'
    WRITE(100,1000) TIME, NODE, MESH
    
            DO I=1, NODE
                WRITE(100, 1010) DNOX(i)
            ENDDO
        
            DO I=1, NODE
                WRITE(100, 1010) DNOY(i)
            ENDDO
        
            DO I=1, MESH
                WRITE(100, 1010) BASEO(i)
            ENDDO
        
            DO I=1, MESH
                WRITE(100, 1020) UM(i)
            ENDDO
        
            DO I=1, MESH
                WRITE(100, 1020) VN(i)
            ENDDO
        
            DO I=1, MESH
                WRITE(100, 1010) H(I)
            ENDDO
        
            DO I=1, MESH
                WRITE(100, 1010) BASEO(I)+H(I)
            ENDDO
    
        WRITE(100,*)

        DO I=1, MESH
            WRITE(100,1030)  (MENODE(I, J), J=1, KO(I))
        ENDDO
        
1000 FORMAT ( 'ZONE T=', '"', F10.2, '"', ',', 3X, 'DATAPACKING=BLOCK',',', 3X, 'VARLOCATION=([3-7]=CELLCENTERED),',3X, 'NODES=',I6,',',3X, 'ELEMENTS=',I6, ',',  3X,'ZONETYPE=FETRIANGLE' ) 
1010 FORMAT(5F15.2) 
1020 FORMAT(F7.4)
1030 FORMAT(3I8)      

END SUBROUTINE TEC_GROUND  

!     =======================================================
!         TECPLOT BASIC DATA PRINT FOR PIPES
!     =======================================================
   SUBROUTINE TEC_PIPEGRID
   INTEGER I, J
   
   WRITE(191,*)'VARIABLES= "X", "Y", "Z",  "I", "DEPTH(h)", "ELE(H)"  '    


        WRITE(191,2000) TIME, NUM_PMAX, NUM_EM

        DO I=1, NUM_PMAX
            WRITE(191,2010) XX(I)
        ENDDO
        
         DO I=1, NUM_PMAX
            WRITE(191,2010) YY(I)
        ENDDO
        
       
        DO I=1, ISWR
        IF(MARK_SW_CHECK(I)==1)THEN
            DO J=1, JSWR(I)
                WRITE(191,2020) BS_SW(I,J)
            ENDDO
        ENDIF
        ENDDO
        
        DO I=1, ISWR
        IF(MARK_SW_CHECK(I)==1)THEN        
            DO J=1, JSWR(I)
                WRITE(191,2030) I
            ENDDO
        ENDIF
        ENDDO           
    
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
        
   
     
        
        DO I=1, NUM_PMAX, 4
            WRITE(191,2030) NUM_PG(I), NUM_PG(I+1), NUM_PG(I+2),NUM_PG(I+3)
        ENDDO
2000 FORMAT ( 'ZONE T=','"', F8.2,'"', ',', 3X,  'NODES=', I10, ', ', 'ELEMENTS=', I10 ',' , 'DATAPACKING=BLOCK, VARLOCATION=([3-6]=CELLCENTERED), ZONETYPE=FEQUADRILATERAL')
2010 FORMAT (F12.3, 3X, F13.3, 3X, I10)
2020 FORMAT (F12.4)
2030 FORMAT (4I8)

END SUBROUTINE TEC_PIPEGRID    

!     =======================================================
!         TECPLOT BASIC DATA PRINT FOR MANHOLE
!     =======================================================
     SUBROUTINE TEC_MH
     INTEGER I,J, K
    
    WRITE(200,*) 'VARIABLES="X", "Y", "Z",  "I", "DEPTH", "ELE" '
    WRITE(200,1000)TIME, MH_NO, MH_EL
        
        DO I=1, MNHL
        IF(MARK_MH_CHECK(I)==1)THEN        
            DO J=1, 4
            WRITE(200,1010) XX_MH(I,J)
            ENDDO
        ENDIF            
        ENDDO
        
        DO I=1, MNHL
        IF(MARK_MH_CHECK(I)==1)THEN        
            DO J=1, 4
            WRITE(200,1010) YY_MH(I,J)
            ENDDO
        ENDIF            
        ENDDO
        
         DO I=1, MNHL
        IF(MARK_MH_CHECK(I)==1)THEN        
         WRITE(200,1010) BS_MH(I)
        ENDIF     
        ENDDO 
                
         DO I=1, MNHL
        IF(MARK_MH_CHECK(I)==1)THEN        
         WRITE(200,1020) I
        ENDIF     
        ENDDO        
        
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
        

        
K=0
DO I=1, MNHL
IF(MARK_MH_CHECK(I)==1)K=K+1        
ENDDO
        DO I=1, K*4, 4
            WRITE(200,1020) INT(I), INT(I+1), INT(I+2), INT(I+3)
        ENDDO
        
1000 FORMAT('ZONE T=','"', F8.2,'"', ',', 3X,  'NODES=', I7,',', 'ELEMENTS=', I7,',', 'DATAPACKING=BLOCK,', 1X, 'VARLOCATION=([3-6]=CELLCENTERED),', 'ZONETYPE=FEQUADRILATERAL') 
1010 FORMAT(10F15.3)
1020 FORMAT(4(I5,1X))


END SUBROUTINE TEC_MH

!     =======================================================
!         TECPLOT BASIC DATA PRINT FOR PUMPING STATION
!     =======================================================
     SUBROUTINE TEC_PM
     INTEGER I,J
    
    WRITE(300,*) 'VARIABLES="X", "Y" , "DEPTH", "ELE"'
    WRITE(300,1000) TIME, PM_NO, PM_EL
        
        DO I=1, IPUMP
            DO J=1, 4
            WRITE(300,1010) XX_PM(I,J)
            ENDDO
        ENDDO
        
        DO I=1, IPUMP
            DO J=1, 4
            WRITE(300,1010) YY_PM(I,J)
            ENDDO
        ENDDO
        
        DO I=1, IPUMP
         WRITE(300,1010) H_MH(MH_PM(I))
        ENDDO
        
        DO I=1, IPUMP
         WRITE(300,1010) H_MH(MH_PM(I))+BS_MH(MH_PM(I))
        ENDDO
        
        DO I=1, IPUMP*4, 4
            WRITE(300,1020) INT(I), INT(I+1), INT(I+2), INT(I+3)
        ENDDO
        
1000 FORMAT('ZONE T=','"', F8.2,'"', ',', 3X,  'NODES=', I7,',', 'ELEMENTS=', I7,',', 'DATAPACKING=BLOCK,', 1X, 'VARLOCATION=([3-4]=CELLCENTERED),', 'ZONETYPE=FEQUADRILATERAL') 
1010 FORMAT(10F15.3)
1020 FORMAT(4(I5,1X))

END SUBROUTINE TEC_PM

end module sub_write_tec	 