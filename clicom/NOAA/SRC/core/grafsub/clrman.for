$STORAGE:2
      SUBROUTINE CLRMAN(PALETTE,PALDEF)
C------------------------------------------------------------------------------
C     CHANGE THE CURRENT PALETTE TO 1 OF 12 AVAILABLE PALETTES.
C
C     INPUT AND OUTPUT ARGUMENTS:
C
C     PALETTE  INT2       CURRENT PALETTE NUMBER
C     PALDEF   INT2 ARRAY CURRENT DEFINITION OF ALL 12 POSSIBLE PALETTES
C------------------------------------------------------------------------------
      INTEGER*2   PALETTE,PALDEF(16,12)
      INTEGER*2   HELPLVL      
      CHARACTER*2 INCHAR   
C
      HELPLVL=10      
      XWIN=.1
      YWIN=.8
  100 CALL GRAFMNU(1,10,XWIN,YWIN,HELPLVL,INCHAR)
      IF     (INCHAR .EQ. '1 ') THEN
             CALL PICKCOL(INDX,.8,.8,0,16,PALETTE,PALDEF,1)
      ELSEIF (INCHAR .EQ. '2 ') THEN
             CALL CUSTCOL(.8,.8,PALETTE,PALDEF)
      ELSEIF (INCHAR .EQ. '4F' .OR. INCHAR .EQ. 'ES') THEN
             RETURN
      ENDIF
      GO TO 100
      END      
