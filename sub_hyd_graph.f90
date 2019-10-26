	module sub_hyd_graph
	
	use globals
	use subprogs
	
	implicit none
	contains
	
!   =======================================================
!        GENERATION OF HYDRO-GRAPH
!   =======================================================
    SUBROUTINE HYD_GRPH(DTR)

    INTEGER I, J, K, NM, CONN, me
    INTEGER R_NODE, R_MESH
    REAL(8) LDPK, DVR, DTR, R_TIME
    REAL(8) NOX(1000,100), NOY(1000,100), CELLRAIN(1000)

    NM=0
    LDPK=DTR

    DVR= DTRAIN/LDPK ! 600/60 = 10

    WRITE(80,*)'"TITLE= HYDRO GRAPH OF STUDY AREA"'
    WRITE(80,*)' VARIABLES = "X", "Y", "RAIN"'

    DO I=1, IRAIN
	    DO J=1, DVR+1
	    NOX(I,J)= DTRAIN*(I-1+float(J-1)/DVR)
	  !  NOY(I,J)= RAIN(me,I)  !me‚ð‚Æ‚è‚ ‚¦‚¸‚¢‚ê‚Ä‚Ý‚½
	    NM=NM+1
	    ENDDO
    ENDDO

    R_NODE=NM*2
    R_MESH=INT(IRAIN)*INT(DVR)

WRITE(*,*) IRAIN, R_NODE, R_MESH

    DO I=1, R_MESH
	    DO J=2, IRAIN+1
		    IF(I*LDPK >=(J-1)*DTRAIN.AND. I*LDPK<J*DTRAIN) CELLRAIN(I)=RAIN(me,J-1)  !
	    ENDDO
    ENDDO

    WRITE(80,1000) R_NODE, R_MESH

    DO I=1, IRAIN
	    DO J=1, DVR+1
		    WRITE(80, 1100) NOX(I,J)
	    	WRITE(80, 1100) NOX(I,J)
	    ENDDO
    ENDDO

    DO I=1, IRAIN
	    DO J=1, DVR+1
		    WRITE(80, 1100) 0
		    WRITE(80, 1100) NOY(I,J)
	    ENDDO
    ENDDO

    DO I=1, R_MESH
        WRITE(80,1200) CELLRAIN(I)
    ENDDO


	DO I=1, R_NODE-2, 2
		WRITE(80, 1300) I, I+1, I+3, I+2
	ENDDO 

1000 FORMAT('ZONE NODES=', I6, ',', 3X, 'ELEMENTS=', I6,',', 3X, 'DATAPACKING=BLOCK, VARLOCATION=([3]=CELLCENTERED), ZONETYPE=FEQUADRILATERAL')
1100 FORMAT(F10.3, 3X, F10.3)
1200 FORMAT (F7.2)
1300 FORMAT (4(I10))

!-----------------------ANI HYDROGRAPH-------------------------------------------------


    DO K=1, IRAIN*DVR ! 610 * 10
 
    R_TIME = LDPK*K ! 60 * K 
    R_NODE = K*2+2  
    R_MESH = K

WRITE(81,*)'"TITLE= ANIMATION HYDRO GRAPH OF STUDY AREA"'
WRITE(81,*)' VARIABLES = "X", "Y", "RAIN"'
WRITE(81,2000) R_TIME, R_NODE, R_MESH

    NM=0

    DO I=1, IRAIN
	    DO J=1, DVR+1
            NM=NM+1
		 !   IF(NM<=K+1)THEN
		!	    WRITE(81, 2100) NOX(I,J), 0,        RAIN(me,I) !‚Æ‚è‚ ‚¦‚¸Á‚µ‚½
		!	    WRITE(81, 2100) NOX(I,J), NOY(I,J), RAIN(me,I) !
         !   ENDIF

	    ENDDO
    ENDDO

    CONN=0

	    DO I=1, R_NODE-2, 2
	    CONN=CONN+1
		    IF( CONN <= R_MESH)THEN
			    WRITE(81, 2300) I, I+1, I+3, I+2
		    ENDIF
	    ENDDO 

    NM=0

    ENDDO

2000 FORMAT('ZONE T=', '"',F10.2,'"', 3X, ',', 'NODES=', I6, ',', 3X, 'ELEMENTS=', I6,',', 3X, 'DATAPACKING=POINT, ZONETYPE=FEQUADRILATERAL')
2100 FORMAT(3(F10.3, 3X))
2300 FORMAT (4(I10))
     
    END SUBROUTINE HYD_GRPH   

end module sub_hyd_graph	