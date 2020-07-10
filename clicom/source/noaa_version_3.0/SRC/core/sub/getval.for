$STORAGE:2

      SUBROUTINE GETVAL(INVAL,DATAFLAG,RTNFLAG)
C
C   ROUTINE TO READ A DATA VALUE - IT INCLUDES CODE TO CHECK THE CHARS 
C       ENTERED TO MAKE SURE THEY ARE APPROPRIATE
C
      CHARACTER*1 INVAL(6), INCHAR(2),ACHAR(2),DATAFLAG,RTNCODE
      CHARACTER*2 CNTRLCHAR,RTNFLAG
      INTEGER*2 FLDPOS,VALPOS,STRTCOL,FLDLEN,FLDTYPE
      LOGICAL ALLSPACES,ALPHA,INSERT,FRSTCL
      EQUIVALENCE (CNTRLCHAR,INCHAR)
      DATA FLDLEN/6/, FRSTCL /.TRUE./

      ACHAR(2) = CHAR(0)
      FLDPOS = 1
      VALPOS = 1
      CALL POSLIN(IROW,ICOL)
      STRTCOL = ICOL
      INSERT = .FALSE.
C
C   ON FIRST CALL TO THIS ROUTINE SET TEXT COLORS FOR COLOR OR B&W
C
      IF (FRSTCL) THEN
         FRSTCL = .FALSE.
         CALL STATUS(IMODE,ICLTYP,IPAGE)
      END IF
      IF (IMODE.EQ.3) THEN
         IBG = 1
         IFG = 15
      ELSE
         IBG = 0
         IFG = 1
      END IF
C
C  IF THE FIELD ALREADY CONTAINS A NUMBER - MOVE THE CURSOR TO THE
C     FIRST POSITION OF THE NUMBER
C
      ALLSPACES = .TRUE.
      DO 40 I=1,FLDLEN
         IF (INVAL(I).NE.' ') THEN
            ALLSPACES = .FALSE.
         END IF
   40 CONTINUE
      IF (.NOT.ALLSPACES) THEN
         FLDTYPE = 2
         DO 50 I = 1,FLDLEN
            IF (INVAL(I).NE.' ') THEN
               FLDPOS = I
               GO TO 55
            END IF
   50    CONTINUE
   55    CONTINUE
         ICOL = ICOL + FLDPOS - 1
      ELSE
         FLDTYPE = 0
      END IF
C
C    START LOOP TO RETRIEVE AND STORE THE INPUT CHARACTERS
C
  100 CALL LOCATE(IROW,ICOL,IERR)
      CALL GETCHAR(0,INCHAR)        
      CALL CLRMSG(2)
      CALL LOCATE(IROW,ICOL,IERR)
C
C   TREAT A "+" CHARACTER AS A TAB
C
      IF (CNTRLCHAR.EQ.'+ ')THEN
         CNTRLCHAR = 'TB'
      END IF

C 
C ---- A NORMAL KEYBOARD CHARACTER HAS BEEN ENTERED
C 
      IF (INCHAR(2).EQ.' ') THEN
         RTNFLAG = '  '
         ALPHA = .FALSE. 
C
C      --- A NUMBER HAS BEEN ENTERED - CHECK FOR VALID PLACEMENT ---
C
          IF (INCHAR(1).GE.'0'.AND.INCHAR(1).LE.'9') THEN
             IF (FLDPOS.GE.FLDLEN) THEN
                CALL WRTMSG(2,41,12,1,0,' ',0)
                GO TO 100
             END IF
C
C      --- A MINUS SIGN HAS BEEN ENTERED - DITTO
C         
          ELSE IF (INCHAR(1).EQ.'-') THEN
             IF (VALPOS.GT.1) THEN
                CALL WRTMSG(2,67,12,1,0,' ',0)
                GO TO 100
             END IF
C
C      --- A LETTER HAS BEEN ENTERED - DITTO
C
          ELSE IF ((INCHAR(1).GE.'A'.AND.INCHAR(1).LE.'Z').OR.
     +              INCHAR(1).EQ.' ') THEN  
             IF (FLDTYPE.EQ.0) THEN
                IF (INVAL(FLDPOS).NE.' ') THEN
                   CALL WRTMSG(2,67,12,1,0,' ',0)
                   GO TO 100
                END IF
             ELSE 
                IF (FLDPOS.LT.FLDLEN) THEN
                   CALL WRTMSG(2,67,12,1,0,' ',0)
                   GO TO 100
                END IF
             END IF
             ALPHA = .TRUE.
C
C      --- AN INVALID CHARACTER HAS BEEN ENTERED ---
C
          ELSE
             CALL WRTMSG(2,57,12,1,0,' ',0)
             GO TO 100
          END IF
            
C
C      THE CHARACTER HAS BEEN ACCEPTED
C
C
C            STORE NORMALLY - IF INSERT MODE IS OFF
C
          IF (ALPHA.OR..NOT.INSERT) THEN
             ACHAR(1) = INCHAR(1)
             CALL CLTEXT(IBG,0,IERR)
             CALL CWRITE(ACHAR(1),IFG,IERR)
             IF (ALPHA) THEN
                INVAL(FLDLEN) = INCHAR(1)
                GO TO 500
             ELSE
                INVAL(FLDPOS) = INCHAR(1)
             END IF
             FLDPOS = FLDPOS + 1
             VALPOS = VALPOS + 1
             ICOL = ICOL + 1 
             IF (FLDPOS.GT.FLDLEN) THEN
                GO TO 500
             ELSE
                GO TO 100
             END IF
          ELSE
C           
C       INSERT MODE IS ON
C
             CALL INSCHR(FLDTYPE,INVAL,FLDLEN-1,FLDPOS,VALPOS,
     +           INCHAR(1),ICOL)
             CALL WRTVAL(INVAL,DATAFLAG,IROW,STRTCOL)
             GO TO 100 
         END IF
C
C  --- OTHERWISE A CONTROL CHARACTER HAS BEEN ENTERED
C
      ELSE
         RTNFLAG = CNTRLCHAR
         CALL CSRCNTRL(FLDTYPE,CNTRLCHAR,INVAL,6,FLDPOS,VALPOS,ICOL,
     +         INSERT,RTNCODE)
         IF (RTNCODE.EQ.'0') THEN
            IF (CNTRLCHAR.EQ.'6F'.OR.CNTRLCHAR.EQ.'DE') THEN
               DATAFLAG = ' '
               CALL WRTVAL(INVAL,DATAFLAG,IROW,STRTCOL)
            END IF       
C **DEBUG
            GO TO 100            
C            ALLSPACES = .TRUE.
C            DO 450 I=1,FLDLEN
C               IF (INVAL(I).NE.' ') THEN
C                  ALLSPACES = .FALSE.
C               END IF
C  450       CONTINUE
C            IF (CNTRLCHAR.EQ.'DE' .AND. .NOT.ALLSPACES) GO TO 100
         END IF
      END IF
  500 CONTINUE

      IF (INSERT) THEN
         CALL INSSWTCH(INSERT)
      END IF
      IF (FLDTYPE.EQ.0) THEN
         CALL JUSTIFY(INVAL)
         CALL WRTVAL(INVAL,DATAFLAG,IROW,STRTCOL)
      END IF
      RETURN
      END
 