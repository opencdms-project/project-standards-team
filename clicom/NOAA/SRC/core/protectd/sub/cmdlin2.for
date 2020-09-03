$STORAGE:2
      SUBROUTINE CMDLIN2(COMMND,CMDLEN,RESULT)
C
C       ** OBJECTIVE:  ROUTINE TO TAKE THE CONTENTS OF DOS COMMAND LINE 
C                      PARAMETERS - BEGINNING AT ADDRESS COMMND AND PASS 
C                      THEM BACK AS RESULT
C       ** NOTE:  THIS IS A SPECIAL VERSION OF CMDLIN USED FOR INSTALLATION
C                 PURPOSES.  IT ASSUMES THAT THE FILE MESSAGES.FTN IS NOT
C                 AVAILABLE.
C           
C
      CHARACTER*64 COMMND,RESULT
      CHARACTER*45 MSGLIN
      CHARACTER*2  INCHAR
      CHARACTER*1  CMDLEN,CHAR
C
      ILEN = ICHAR(CMDLEN)
      IF (ILEN.EQ.0) THEN
         CALL POSLIN(IR,IC)
         MSGLIN = 'Error in command line - No parameter(s) given'
         IR = IR + 1
         CALL SCRNMSGI(MSGLIN,IR,12) 
         MSGLIN = 'Press any key to continue'
         IR = IR + 1
         CALL SCRNMSGI(MSGLIN,IR,15) 
         IR = IR + 1
         MSGLIN = ' '
         CALL SCRNMSGI(MSGLIN,IR,15) 
         CALL GETCHAR(0,INCHAR)
         RESULT = ' '
      ELSE   
         DO 50 I = 1,ILEN
            CHAR = COMMND(I:I)
            IF (CHAR.NE.' '.AND.CHAR.NE.',') THEN
               GO TO 60
            END IF
            ISTRT = I
   50    CONTINUE
   60    CONTINUE
         RESULT = '  '
         RESULT(1:ILEN) = COMMND(ISTRT+1:ILEN)
      END IF
      RETURN
      END

