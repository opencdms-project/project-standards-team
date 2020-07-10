$STORAGE:2
      SUBROUTINE SETUSERS
C
C   PROGRAM TO SET USER NAMES AND THEIR AUTHORITY LEVELS
C
      CHARACTER*72 MESSAG
      CHARACTER*12 FIELD(2),HLDUSR(99)
      CHARACTER*3 LEVEL(3),INLVL,DEVER
      CHARACTER*2 RTNFLAG
      CHARACTER*1 REPLY
      INTEGER*2 HLDLVL(99),HLDDEL(99)
      DATA LEVEL /'LO','MED','HI'/
C
C   VERFIY THAT THE USER CAN ACCESS THIS ROUTINE
C
      CALL CHKPAS(INLVL)
      IF (INLVL.NE.'HI ') THEN
         CALL WRTMSG(12,145,12,1,1,' ',0)
         CALL LOCATE(24,0,IERR)
         RETURN
      END IF   
C
      CALL CLS
      OPEN (4,FILE='P:\DATA\USERS.DAT',STATUS='OLD',FORM='UNFORMATTED')
C
C   READ ALL CURRENT USERS AND AUTHORITIES INTO MEMORY
C
      DO 100 I = 1,99
         CALL RDUSER(HLDUSR(I),HLDLVL(I),IEOF)
         IF (IEOF.NE.0) THEN 
            GO TO 120
         END IF
         NUMREC = I
         HLDDEL(I) = 0
  100 CONTINUE
  120 CONTINUE
      CLOSE(4)
      I = 0
C
C   WRITE THE FUNCTION KEY DEFINITIONS
C
      CALL LOCATE(24,0,IERR)
      CALL WRTSTR(' F2',3,14,0)
      CALL WRTSTR('STORE',5,15,4)
      CALL WRTSTR(' F3',3,14,0)
      CALL WRTSTR('FIND NEXT',9,15,4)
      CALL WRTSTR(' sF3',4,14,0)
      CALL WRTSTR('FIND PREVIOUS',13,15,4)
      CALL GETDEASE(DEVER)
      IF (DEVER.EQ.'4.0') THEN
          CALL WRTSTR(' ESC',4,14,0)
      ELSE
          CALL WRTSTR(' F4',3,14,0)
      END IF
      CALL WRTSTR('EXIT',4,15,4)
      CALL WRTSTR(' F6',3,14,0)
      CALL WRTSTR('CLEAR FIELD',11,15,4)
      CALL WRTSTR(' F7',3,14,0)
      CALL WRTSTR('DELETE',6,15,4)
      CALL WRTSTR(' F8',3,14,0)
      CALL WRTSTR('MODIFY',6,15,4)
C
C   SOLICIT THE CHANGES TO USERS AND/OR ATHORITIES
C
  150 CONTINUE
         IF (I.EQ.0) THEN
            FIELD(1) = ' '
            FIELD(2) = ' '
         ELSE
            FIELD(1) = HLDUSR(I)
            FIELD(2) = LEVEL(HLDLVL(I))
         END IF
  160    CONTINUE
         RTNFLAG = ' '
         IF (HLDDEL(I).EQ.1) THEN
            WRITE(MESSAG,170) I
  170       FORMAT('Record number ',I2,' - it is deleted')
         ELSE IF (I.GT.0) THEN
            WRITE(MESSAG,180) I
  180       FORMAT('Record number ',I2)
         ELSE
            MESSAG = ' '  
         END IF
         CALL LOCATE(15,0,IERR)
         CALL WRTSTR(MESSAG,72,11,0)
         CALL DRWBOX(18,3,50,9,2,11,0)
         CALL LOCATE(5,20,IERR)
         CALL WRTSTR('User-name ',10,14,0)
         CALL WRTSTR(FIELD(1),12,15,1)
         CALL LOCATE(7,20,IERR)
         CALL WRTSTR('Authority (LO, MED, HI) ',24,14,0)
         CALL WRTSTR(FIELD(2),4,15,1)
         CALL LOCATE(5,30,IERR)
         CALL GETSTR(0,FIELD(1),12,15,1,RTNFLAG)
         IF (RTNFLAG(2:2).NE.'F'.AND.RTNFLAG(2:2).NE.'S') THEN
            CALL LOCATE(7,44,IERR)
            CALL GETSTR(0,FIELD(2),4,15,1,RTNFLAG)
         END IF
         MESSAG = ' '
         CALL LOCATE(17,0,IERR)
         CALL WRTSTR(MESSAG,72,12,0)
         IF (RTNFLAG.EQ.'4F') THEN
            GO TO 300
         ELSE IF(RTNFLAG.EQ.'3F') THEN
            IF (I.LT.NUMREC) THEN
               I = I + 1
            ELSE
               I = 0
               CALL LOCATE(17,0,IERR)
               CALL WRTSTR('No more records',15,11,0)
               CALL BEEP
            END IF
         ELSE IF (RTNFLAG.EQ.'3S') THEN
            IF (I.GT.0) THEN
               I = I - 1
            ELSE
               CALL LOCATE(17,0,IERR)
               CALL WRTSTR('No more records',15,11,0)
               CALL BEEP
            END IF
         ELSE IF (RTNFLAG.EQ.'7F') THEN
            IF (I.GE.1.AND.I.LE.NUMREC) THEN
               CALL LOCATE(17,0,IERR)
               CALL WRTSTR('Delete this user record ',24,12,0)
               CALL OKREPLY(REPLY,RTNFLAG)
               IF (REPLY.EQ.'Y'.AND.RTNFLAG.NE.'4F') THEN
                  DO 185 J = 1,NUMREC
                     IF (J.NE.I.AND.HLDDEL(J).EQ.0.AND
     +                     .HLDLVL(J).EQ.3) THEN
                        GO TO 188
                     END IF
  185             CONTINUE
                  CALL WRTMSG(8,140,12,1,0,' ',0)
                  GO TO 160
  188             CONTINUE   
                  HLDDEL(I) = 1
                  WRITE(MESSAG,190) I
  190             FORMAT('Record ',I2,' is deleted')
                  CALL LOCATE(17,0,IERR)
                  CALL WRTSTR(MESSAG,72,11,0)
               END IF
            END IF               
         ELSE
            IF (FIELD(2).EQ.'LO') THEN
               NLEVEL = 1
            ELSE IF (FIELD(2).EQ.'MED') THEN
               NLEVEL = 2
            ELSE IF (FIELD(2).EQ.'HI ') THEN
               NLEVEL = 3
            ELSE
               CALL WRTMSG(8,141,12,1,0,' ',0)
               GO TO 160
            END IF
            IF (FIELD(1).EQ.' ') THEN
               CALL WRTMSG(8,142,12,1,0,' ',0)
               GO TO 160
            END IF
            IF (RTNFLAG.EQ.'8F') THEN
               IF (I.GE.1.AND.I.LE.NUMREC) THEN
                  IF (HLDDEL(I).EQ.0) THEN
                     HLDUSR(I) = FIELD(1)
                     HLDLVL(I) = NLEVEL
                     WRITE(MESSAG,200) I
  200                FORMAT('Record ',I2,' is modified')
                  ELSE
                     HLDDEL(I) = 0
                     HLDUSR(I) = FIELD(1)
                     HLDLVL(I) = NLEVEL
                     WRITE(MESSAG,210) I
  210                FORMAT('Record ',I2,' is undeleted and modified')
                  END IF
                  CALL LOCATE(17,0,IERR)
                  CALL WRTSTR(MESSAG,72,11,0)
               ELSE
                  CALL WRTMSG(8,143,12,1,0,' ',0)
               END IF
            ELSE IF (RTNFLAG.EQ.'2F') THEN
               NUMREC = NUMREC + 1
               I = NUMREC
               HLDUSR(I) = FIELD(1)
               HLDLVL(I) = NLEVEL
               HLDDEL(I) = 0
               WRITE(MESSAG,220) I
  220          FORMAT('Record ',I2,' is stored')
               CALL LOCATE(17,0,IERR)
               CALL WRTSTR(MESSAG,72,11,0)
            ELSE
               CALL WRTMSG(8,144,12,1,0,' ',0)
               GO TO 160
            END IF
         END IF
         GO TO 150
  300 CONTINUE
C
C   CHANGES COMPLETE - COPY THE USERS TO THE NEW FILE
C
      OPEN (4,FILE='P:\DATA\USERS.DAT',STATUS='UNKNOWN'
     +      ,FORM='UNFORMATTED')
      DO 350 I = 1,NUMREC
         IF (HLDDEL(I).EQ.0) THEN
            CALL WRTUSER(HLDUSR(I),HLDLVL(I))  
         END IF
  350 CONTINUE
      CLOSE(4)
      RETURN
      END
