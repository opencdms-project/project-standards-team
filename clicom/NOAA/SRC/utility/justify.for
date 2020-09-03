$STORAGE:2

      SUBROUTINE JUSTIFY(INVAL)
C 
C   THIS ROUTINE TAKES THE NUMERIC PORTION OF INVAL AND RIGHT JUSTIFY'S
C      IT.  
C
      CHARACTER*1 INVAL(6),OUTVAL(6)
C
      DO 50 I = 1,5
         OUTVAL(I) = ' '
   50 CONTINUE
      OUTVAL(6) = INVAL(6)
      J = 6
      DO 100 I = 5,1,-1
         IF (INVAL(I).NE.' ') THEN
            J = J - 1
            OUTVAL(J) = INVAL(I)
         END IF
  100 CONTINUE
      DO 150 I = 1,5
         INVAL(I) = OUTVAL(I)
  150 CONTINUE
      RETURN
      END
