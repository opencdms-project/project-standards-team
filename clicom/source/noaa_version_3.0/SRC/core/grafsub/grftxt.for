$STORAGE:2
***********************************************************************
      SUBROUTINE GRFTXT(ILEN,PRTTXT,IFCOL,IBCOL)
C
C   ROUTINE TO PRINT PRTTXT TO THE SCREEN IN COLOR ICOLOR (IN GRAPHICS)
C
      INTEGER*2 ILEN,IFCOL,IBCOL
      CHARACTER*(*) PRTTXT
      CHARACTER*80 HLDTXT
C
      HLDTXT = '!'//PRTTXT(1:ILEN)//'!'
      CALL SETTCL(IFCOL,IBCOL)      
      CALL TEXT(HLDTXT)
      CALL DELTCU
      RETURN
      END
