$STORAGE:2
C
      SUBROUTINE GETMON(MONNAM,NAMLEN)
C
C   THIS ROUTINE READS THE NAMES OF THE 12 MONTHS FROM THE MESSAGE FILE
C
      CHARACTER*1 MONNAM(NAMLEN,13), MSGLIN(78,2)
C
C   INITIALIZE
C
      DO 20 I = 1,13
         MONNAM(1,I)(1:NAMLEN) = ' '
   20 CONTINUE
C
C   READ THE MONTH NAMES FROM THE MESSAGE FILE
C
      CALL GETMSG(293,MSGLIN(1,1))
      CALL GETMSG(294,MSGLIN(1,2))
      CALL GETMSG(999,MSGLIN(1,1))
C
C   PARSE THE MESSAGE LINES READ INTO THE MONTH NAMES - THEY SHOULD
C   BE SEPARATED BY COMMAS (,).
C
      IMON = 0
      IBEGIN = 1
      I = 1
      IMSG = 1
  100 CONTINUE
         IF (MSGLIN(I,IMSG).EQ.','.OR.I.EQ.78) THEN
            IEND = I - 1
            IMON = IMON + 1
            IF (IEND-IBEGIN.GT.NAMLEN-1) THEN
               IEND = IBEGIN + NAMLEN - 1
            END IF
            DO 120 J = IBEGIN,IEND
               MONNAM(J-IBEGIN+1,IMON) = MSGLIN(J,IMSG)
  120       CONTINUE 
            IBEGIN = I + 1
         END IF
         I = I + 1
         IF (IMON.LT.13) THEN
            IF (I.LE.78) THEN
               GO TO 100
            ELSE IF (IMSG.LT.2) THEN
               IMSG = 2
               I = 1
               IBEGIN = 1
               GO TO 100
            END IF
         END IF
C            
      RETURN
      END
      