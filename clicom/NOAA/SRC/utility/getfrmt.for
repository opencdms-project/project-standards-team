$STORAGE:2
      SUBROUTINE GETFRMT(LINE,OUTFRM)
C
C   ROUTINE TO FIND THE END OF STRING "LINE" AND BUILD AN 'A' FORMAT FOR
C   THE LENGTH FOUND.
C  
      CHARACTER*(*) LINE
      CHARACTER*6 OUTFRM
C
      LGTH = LEN(LINE)
      DO 100 I = LGTH,1,-1
         IF (LINE(I:I).NE.' ') THEN
            ILEN = I
            GO TO 110
         END IF
100   CONTINUE
      ILEN = 1
110   CONTINUE
C     
      IF (ILEN.LT.10) THEN
         WRITE(OUTFRM,'(A2,I1,A1)') '(A',ILEN,')'            
      ELSE
         WRITE(OUTFRM,'(A2,I2.2,A1)') '(A',ILEN,')'            
      END IF
      RETURN
      END
