!     #######################################################
!     ##                                                   ##
!     ##                  PRE-CONFIMATION                  ##
!     ##                                                   ##
!     #######################################################
!
module sub_confirm
  use globals
  implicit none
contains

!	===================================================
!			ELEVATION CHECK OF PIPE AND SURFACE
!	===================================================

	SUBROUTINE CONFIRMATION
	
	INTEGER I, J, K, ME
	
	OPEN(10, FILE = 'CHECK/ELE_CHECK(PIPE AND SUR).DAT', ACTION = 'WRITE')
	WRITE(10,*) 'BS_SW(I,J) / BS_SW(I,J)+DD2_SW(I) /  BASEO(ME) / DIFFERENCE'
	DO ME=1, MESH
	!	IF(MARK(ME)/=0)THEN
	    if(mark(me) ==1 .or. mark(me)==2) then
			I=CN_I(ME) ; J=CN_J(ME)
			WRITE(10, 1000) I, J, BS_SW(I,J), BS_SW(I,J)+DD2_SW(I), BASEO(ME), BASEO(ME) - (BS_SW(I,J)+DD2_SW(I))
		ENDIF
	ENDDO

1000 FORMAT(I5, 5X, I5, 10(F10.3, 5X))

	END SUBROUTINE CONFIRMATION
	
END MODULE sub_confirm


