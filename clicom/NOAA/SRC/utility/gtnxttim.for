$STORAGE:2

      SUBROUTINE GTNXTTIM(ITYPE,WRTTIM)
C
C       ** OBJECTIVE:  GIVEN THE INDEX TO THE OBSERVATION TYPE AND THE TIME ID
C                      OF THE CURRENT RECORD, ROUTINE CALCULATES THE NEXT 
C                      SEQUENTIAL TIME ID
C       ** INPUT:
C             ITYPE.....INDEX TO THE OBSERVATION TYPE
C                       1=MLY  2=10D  3=DLY  4=SYN  5=HLY  6=15M
C             WRTTIM....TIME ID OF CURRENT RECORD IN FORMAT OF YYYYMMDD
C       ** OUTPUT:
C             WRTTIM....NEXT SEQUENTIAL TIME ID IN FORMAT OF YYYYMMDD
C
      INTEGER*2    ITYPE
      CHARACTER*8 WRTTIM
C
      LOGICAL DAYFLG,MONFLG,YRFLG
      INTEGER*2 IDAY,IMON,IYR
C
      YRFLG  = .FALSE.
      MONFLG = .FALSE.
      DAYFLG = .FALSE.
      IF (ITYPE.LE.2) THEN
C          .. 1=MLY, 2=10D      
         YRFLG = .TRUE.
      ELSE IF(ITYPE.EQ.3) THEN
C          .. 3=DLY      
         MONFLG = .TRUE.
      ELSE IF(ITYPE.LT.7) THEN
C          .. 4=SYN, 5=HLY, 6=15M      
         DAYFLG = .TRUE.
      ELSE   
C          .. 7=U-A  -- NOT ALLOWED       
         CALL WRTMSG(3,245,12,1,1,' ',0)
         STOP ' '
      ENDIF      
C 
      READ(WRTTIM,500) IYR,IMON,IDAY
C
      IF (DAYFLG) THEN
         CALL CDAYMON(WRTTIM,MAXDAY)
         IDAY = IDAY + 1
         IF (IDAY.GT.MAXDAY) THEN
            IDAY = 1
            MONFLG = .TRUE.
         ENDIF
      ENDIF
      IF (MONFLG) THEN
         IMON = IMON + 1
         IF (IMON.GT.12) THEN
            IMON = 1
            YRFLG = .TRUE.
         ENDIF
      ENDIF
      IF (YRFLG) THEN
         IYR = IYR + 1
      ENDIF               
C      
      WRITE(WRTTIM,510) IYR,IMON,IDAY
C
      RETURN
C
C       ** FORMAT STMTS
  500 FORMAT(I4,2I2)      
  510 FORMAT(I4.4,2I2.2)      
      END
