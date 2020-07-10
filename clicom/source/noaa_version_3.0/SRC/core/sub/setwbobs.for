$STORAGE:2
      SUBROUTINE SETWBOBS
C
$INCLUDE:'VAL1.INC'
$INCLUDE:'GENMOIST.INC'      
C
      DO 10 I=1,NUMELEM
         IF (TBLELEM(I).EQ.102) GO TO 15
   10 CONTINUE
      RETURN
   15 CONTINUE   
      IELEM = I
C               
      IF (SPECOBS(IELEM).EQ.1) THEN
C          .. ASPIRATED WET BULB MEASUREMENT      
         ASPTWB = 1
      ELSE IF (SPECOBS(IELEM).EQ.2) THEN
C          .. NON-ASPIRATED WET BULB MEASUREMENT      
         ASPTWB = 0
      ELSE
C          .. INCORRECT VALUE OR NO VALUE SET 
C             DEFAULT TO ASPIRATED WET BULB MEASUREMENT      
         ASPTWB = 1
      ENDIF      
C
      RETURN
      END              
      
            
                  