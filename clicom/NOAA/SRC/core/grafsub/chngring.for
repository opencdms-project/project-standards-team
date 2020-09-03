$STORAGE:2
      SUBROUTINE CHNGRING(LBLRNG)
C------------------------------------------------------------------------------
C     CHANGE THE PLACEMENT OF LABELS ON OCCURRANCE RINGS FOR THE WINDROSE
C
C     INPUT AND OUTPUT ARGUMENT:
C
C     LBLRNG  INT2   POSITION FOR THE LABELS ON RINGS. 0 = NO DISPLAY OF LABELS
C------------------------------------------------------------------------------
      INTEGER      MENUNBR, LBLRNG, LPICK, HELPLVL
      CHARACTER*2  INCHAR   
C
      DATA MENUNBR/45/,HELPLVL/45/
C      
      XWIN = 0.1
      YWIN = 0.8
      LPICK  = LBLRNG+1
      WRITE (UNIT=INCHAR,FMT='(I1,1X)') LPICK
      CALL GRAFMNU(5,MENUNBR,XWIN,YWIN,HELPLVL,INCHAR)
      IF     (INCHAR .GE. '1 ' .AND. INCHAR .LE. '5 ') THEN
             READ (UNIT=INCHAR,FMT='(I1,1X)') LPICK
             LBLRNG = LPICK - 1
      ENDIF
      RETURN
      END      
