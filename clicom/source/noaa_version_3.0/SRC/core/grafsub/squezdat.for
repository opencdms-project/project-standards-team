$STORAGE:2
      SUBROUTINE SQUEZDAT(STR1,STR2)
C
C       ** OBJECTIVE: GIVEN TWO DATE STRINGS, FIND THE NAME OF THE MONTH IN
C                     THE STRINGS AND REWRITE THE STRINGS USING ONLY THE
C                     FIRST THREE CHARACTERS OF THE MONTH NAME.  CONCATENATE
C                     THE TWO STRINGS AND PUT THE RESULTS INTO STRING 2.
C
C       ** INPUT:
C             STR1....INPUT DATE STRING 1
C             STR2....INPUT DATE STRING 2
C       ** OUTPUT:
C             STR1....CONTAINS THE RESULTS OF CONCATENATING THE TWO INPUT
C                     STRINGS WITH THE ABBREVIATED MONTH NAME     
C
      CHARACTER*(*) STR1
      CHARACTER*(*) STR2
      CHARACTER*12  MONNAME(13),MONTH
      CHARACTER*40  SAVSTR
C
      CALL GETMON(MONNAME,12)
C
      NCSTR1 = LNG(STR1)
      DO 30 IMON=1,12
         MONTH = MONNAME(IMON)
         NCMON = LNG(MONTH)
         IE = NCMON-1
         DO 20 IB=1,NCSTR1
            IE = IE+1
            IF (IE .GT. NCSTR1) GO TO 30
            IF (STR1(IB:IE) .EQ. MONTH(1:NCMON)) GO TO 35
   20    CONTINUE         
   30 CONTINUE            
      IB=0
   35 CONTINUE            
      IF (IB.GT.0) THEN
         SAVSTR = STR1
         STR1(IB+3:) = SAVSTR(IE+1:)
      ENDIF
C      
      NCSTR2 = LNG(STR2)
      DO 50 IMON=1,12
         MONTH = MONNAME(IMON)
         NCMON = LNG(MONTH)
         IE = NCMON-1
         DO 40 IB=1,NCSTR2
            IE = IE + 1
            IF (IE .GT. NCSTR2) GO TO 50
            IF (STR2(IB:IE) .EQ. MONTH(1:NCMON)) GO TO 55
   40    CONTINUE         
   50 CONTINUE            
      IB=0
   55 CONTINUE            
      IF (IB.GT.0) THEN
         SAVSTR = STR2
         SAVSTR(IB+3:) = STR2(IE+1:)
      ENDIF
C
      NCSTR1 = LNG(STR1)
      NCSTR2 = LNG(SAVSTR)
      STR1(NCSTR1+1:) = '-'//SAVSTR
C
      RETURN
      END
            
            

