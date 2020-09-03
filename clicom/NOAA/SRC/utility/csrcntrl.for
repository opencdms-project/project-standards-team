$STORAGE:2

      SUBROUTINE CSRCNTRL(FLDTYPE,CNTRLCHAR,FIELD,FLDLEN,FLDPOS,
     +        VALPOS,ICOL,INSERT,RTNCODE)
C
C   ROUTINE TO MOVE THE CURSOR WITHIN A FIELD, DELETE A CHARACTER, AND
C       OTHERWISE DEAL WITH THE CURSOR CONTROL AND FUNCTION KEYS 
C       RELEVANT WITHIN A FIELD
C
C     THE RTNCODE ... 0 - NO PROBLEMS
C                     1 - POSITION NOW LEFT OF INPUT FIELD
C                     2 - POSITION NOW RIGHT OF INPUT FIELD
C                     3 - NON-FIELD CONTROL CHARACTER ENTERED
C         FLDTYPE = 0,1 NEW VALUE TREATED AS ALPHA
C                 =   2 STANDARD VALUE (LAST POSTION ALPHA)
C                 = > 2  NUMBER 
C
      CHARACTER*1 FIELD(6),RTNCODE
      CHARACTER*2 CNTRLCHAR
      INTEGER*2 FLDTYPE,FLDLEN,FLDPOS,VALPOS,ICOL 
      LOGICAL INSERT
C
      RTNCODE = '0'  
C 
C   --- LEFT ARROW ENTERED ---
C 
         IF (.NOT.INSERT.AND.CNTRLCHAR.EQ.'BS') THEN
            CNTRLCHAR = 'LA'
         END IF
         IF (CNTRLCHAR.EQ.'LA') THEN
            IF (FLDPOS.GT.1) THEN
               IF (VALPOS.GT.0) THEN 
                  FLDPOS = FLDPOS - 1
                  VALPOS = VALPOS - 1
                  ICOL = ICOL - 1
               ELSE
                  RTNCODE = '1'
               END IF
            ELSE
               RTNCODE = '1'
            END IF
C
C    --- RIGHT ARROW ---
C 
         ELSE IF (CNTRLCHAR.EQ.'RA') THEN
            IF (FLDPOS.GE.FLDLEN) THEN
               RTNCODE = '2'
            ELSE
               FLDPOS = FLDPOS + 1
               VALPOS = VALPOS + 1
               ICOL = ICOL + 1
            END IF
C
C     --- F6 - FIELD CLEAR ---
C
         ELSE IF (CNTRLCHAR.EQ.'6F') THEN
            ICOL = ICOL - FLDPOS + 1
            FLDPOS = 1
            VALPOS = 1 
            IF (FLDTYPE.EQ.2) THEN
               FLDTYPE = 0
            END IF
            DO 200 I = 1,FLDLEN
               FIELD(I) = ' '
  200       CONTINUE

C
C     --- DELETE CHARACTER ---
C
         ELSE IF (CNTRLCHAR.EQ.'DE'.OR.CNTRLCHAR.EQ.'BS') THEN
            IF (CNTRLCHAR.EQ.'BS') THEN
               IF (FLDPOS.GT.1) THEN
                  IF (VALPOS.GT.0) THEN 
                     FLDPOS = FLDPOS - 1
                     VALPOS = VALPOS - 1
                     ICOL = ICOL - 1
                  ELSE
                     RTNCODE = '1'
                     RETURN
                  END IF
               ELSE
                  RTNCODE = '1'
                  RETURN
               END IF
            END IF 
            IF (FLDTYPE.EQ.2) THEN
               IF (FLDPOS.GE.FLDLEN) THEN
                  FIELD(FLDPOS) = ' '
               ELSE
                  DO 240 I = FLDPOS,2,-1
                     FIELD(I) = FIELD(I-1)
  240             CONTINUE
C **DEBUG 11-18-91
                  FIELD(1) = ' '  
                  IF (FIELD(FLDPOS).EQ.' ') THEN
                     FLDPOS = FLDPOS + 1
                     ICOL = ICOL + 1
                  END IF 
               END IF
            ELSE
               IF (FLDPOS.LT.FLDLEN) THEN
                  DO 250 I = FLDPOS,FLDLEN-1
                     FIELD(I) = FIELD(I+1)
  250             CONTINUE
               END IF
               FIELD(FLDLEN) = ' '
            END IF
C
C     --- INSERT KEY - TURN INSERT MODE ON/OFF ---
C
         ELSE IF (CNTRLCHAR.EQ.'IN') THEN
            CALL INSSWTCH(INSERT)
C
C     --- NON-FIELD CONTROL CHARACTER ENTERED ---
C
         ELSE
            RTNCODE = '3'
         END IF
      RETURN
      END    