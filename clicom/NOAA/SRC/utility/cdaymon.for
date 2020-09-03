$STORAGE:2

      SUBROUTINE CDAYMON(TIMID,NDAYMON)
C          ENTRY IDAYMON(INTYR,INTMON,NDAYMON)    
C
      CHARACTER*10 TIMID
      INTEGER*2    INTYR,INTMON,NDAYMON
C
      INTEGER*2 IYR,IMON,NDAYS(12)
      DATA NDAYS/31,28,31,30,31,30,31,31,30,31,30,31/      
C      
C----------------------------------------------------------------------
C
C-----SUBROUTINE CDAYMON(TIMID,NDAYMON)
C
C       ** INPUT:
C             TIMID......DATE/TIME ID OF THE FORM YYYYMMDDHH
C       ** OUTPUT:
C             NDAYMON....NUMBER OF DAYS IN THE CURRENT MONTH
C
      READ(TIMID,500) IYR,IMON
      GO TO 10
C      
C----------------------------------------------------------------------
C
C**** ENTRY ****
C
      ENTRY IDAYMON(INTYR,INTMON,NDAYMON)    
C      
C       ** INPUT:
C             INTYR......CURRENT YEAR -- INTEGER FORMAT
C             INTMON.....CURRENT MONTH -- INTEGER FORMAT
C       ** OUTPUT:
C             NDAYMON....NUMBER OF DAYS IN THE CURRENT MONTH
C
      IYR  = INTYR
      IMON = INTMON
C
C---------------------------------------------------------------------- 
C
   10 CONTINUE   
      IF (IMON.EQ.2) THEN
         IF (MOD(IYR,4).EQ.0) THEN
            IF (MOD(IYR,100).EQ.0 .AND. MOD(IYR,400).EQ.0) THEN
               NDAYMON=28
            ELSE
               NDAYMON=29
            ENDIF
         ELSE
            NDAYMON=28
         ENDIF
      ELSE
         NDAYMON=NDAYS(IMON)
      ENDIF                  
      RETURN
C
C     ** FORMAT STMTS
C
  500 FORMAT(I4,I2)
C
      END        
