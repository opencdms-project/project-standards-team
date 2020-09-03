$STORAGE:2
C
       SUBROUTINE CHKKEY(RTNFLAG,IELEM,ILINE,RECTYPE,LINQCD,POSFLD,
     +                   STRTELEM,STRTLINE,NUMLN2,HOURLBL,RTNCODE)
C
C  THIS ROUTINE IDENTIFIES THE CONTROL CHARACTER PASSED TO IT AND
C  INITIATES THE PROPER RESPONSE
C
$INCLUDE: 'VAL1.INC'
$INCLUDE: 'INDEX.INC'
C
      CHARACTER*1 RTNCODE,REPLY,LINQCD(MAXLINE)
      CHARACTER*2 RTNFLAG,HOURLBL(24)
      CHARACTER*3 RECTYPE
      CHARACTER*64 FILENAM
C
      INTEGER*2 STRTELEM,STRTLINE,POSFLD
      INTEGER*2 BEEPON,FGCOLOR, IELEM,ILINE,HLDLINE
C
      CHARACTER*1 TWO
      CHARACTER*2 TWOF
      CHARACTER*20 BLANK
      DATA TWO,TWOF /'2','2F'/
      DATA BEEPON /1/
      DATA BLANK /'             '/
C
      RTNCODE = '0'
      HLDLINE = ILINE
C
C  CHECK FOR CURSOR CONTROL CHARACTERS AND TAKE INDICATED
C  ACTION WHEN FOUND.
C
C  THE FOLLOWING THIRTEEN (13) CONTROL CHARACTERS APPLY TO BOTH
C  DATA ENTRY AND VALIDATION
C
C  'BACK TAB' OR 'LEFT ARROW' = GO TO PREVIOUS ELEMENT
C
   20 CONTINUE
      IF (RTNFLAG.EQ.'BT'.OR.RTNFLAG.EQ.'LA') THEN
         IELEM = IELEM - 1
         IF (IELEM.LT.1)THEN
            IELEM = NUMELEM
            ILINE = ILINE - 1
            IF (ILINE.LT.1)THEN
               IELEM = 1
               ILINE = 1
               RETURN
            ELSE
               RTNCODE = TWO
               RETURN
            END IF
         ELSE
            RETURN
         END IF
C
C  'TAB' OR 'RIGHT ARROW' = GO TO NEXT ELEMENT
C  'SPACE' = ENTER BLANK IN DATA FLAG AND GO TO NEXT ELEMENT
C
      ELSE IF (RTNFLAG.EQ.'TB'.OR.RTNFLAG.EQ.'RA'.OR.
     +         RTNFLAG.EQ.BLANK)THEN
         IELEM = IELEM + 1
         IF (IELEM.GT.NUMELEM)THEN
            IELEM = 1
            ILINE = ILINE + 1
            RTNCODE = TWO
            IF (ILINE.LE.NUMLN2)THEN
               RETURN
            ELSE
               IELEM = NUMELEM
               ILINE = NUMLN2
               RETURN
            END IF
         END IF
         RETURN
C
C  'UP ARROW' = GO TO PREVIOUS LINE - SAME ELEMENT
C
      ELSE IF (RTNFLAG.EQ.'UA') THEN
            ILINE = ILINE - 1
            IF (ILINE.LT.1)THEN
               ILINE = 1
               RETURN
            ELSE
               RTNCODE  = TWO
               RETURN
            END IF
C
C  'DOWN ARROW' = GO TO NEXT LINE - SAME ELEMENT
C
      ELSE IF (RTNFLAG.EQ.'DA') THEN
            ILINE = ILINE + 1
            RTNCODE = TWO
            IF (ILINE.GT.NUMLN2)THEN
               IELEM = NUMELEM
               ILINE = NUMLN2
            END  IF
            RETURN
C
C  'RETURN' = GO TO NEXT LINE - FIRST ELEMENT
C
      ELSE IF (RTNFLAG.EQ.'RE')THEN
           IELEM = 1
           ILINE = ILINE + 1
           IF (ILINE.GT.NUMLN2)THEN
              IELEM = NUMELEM
              ILINE = NUMLN2
           END IF
           RTNCODE = TWO
           RETURN
C
C  'HOME' = GO TO LINE 1 - ELEMENT 1
C
      ELSE IF (RTNFLAG.EQ.'HO') THEN
            IELEM = 1
            ILINE = 1
            RTNCODE = TWO
            RETURN
C
C  'END' = GO TO LAST ELEMENT IN LAST LINE WITH DATA
C
      ELSE IF (RTNFLAG.EQ.'EN') THEN
            IELEM = NUMELEM
            DO 50 LNCNT = NUMLN2,1,-1
               DO 50 ICOL = 1,NUMELEM
                  IF (VALARRAY(ICOL,LNCNT).NE.'      ')THEN
                     GO TO 60
                  END IF
   50       CONTINUE
            LNCNT = 0
   60       CONTINUE
            IF (LNCNT.LT.NUMLN2) THEN
               LNCNT = LNCNT + 1
            END IF
            ILINE = LNCNT
            RTNCODE = TWO
            RETURN
C
C  'F1' = DISPLAY THE HELP SCREEN
C
      ELSE IF (RTNFLAG.EQ.'1F')THEN
            IF (PASSWD.EQ.'DE')THEN
               FILENAM = 'P:\HELP\ENTRY.HLP'
            ELSE
               FILENAM = 'P:\HELP\DATQC.HLP'
            END IF
            CALL DSPWIN(FILENAM)
            RTNCODE = '0'
            RETURN
C
C  'F2' = WRITE THIS FRAME OF DATA TO DISK WORK FILES
C
      ELSE IF (RTNFLAG.EQ.TWOF)  THEN
            RTNCODE = '9'
            RETURN
C
C  'F4' = ABANDON THIS FRAME OF DATA WITHOUT WRITING IT TO DISK
C
      ELSE IF (RTNFLAG .EQ.'4F') THEN
         FGCOLOR = 12
         CALL WRTMSG(3,200,FGCOLOR,BEEPON,0,' ',0)
         CALL LOCATE(23,1,IERR)
         CALL CLRMSG(2)     
         CALL OKREPLY(REPLY,RTNCODE)
         IF (REPLY.EQ.'Y'.AND.RTNCODE.EQ.'0') THEN
            RTNCODE = '3'
            CALL CLRMSG(4)
            CALL CLRMSG(3)
            CALL CLRMSG(2)
            RETURN
         ELSE
            RTNCODE = '0'
            CALL CLRMSG(4)
            CALL CLRMSG(3)
            CALL CLRMSG(2)
            RETURN
         END IF
C
C  'F5' = CLEAR THIS FRAME OF DATA FROM THE SCREEN AND FROM MEMORY
C
      ELSE IF (RTNFLAG.EQ.'5F') THEN
            FGCOLOR = 12
            CALL WRTMSG(3,201,FGCOLOR,BEEPON,0,' ',0)
            CALL LOCATE(23,1,IERR)
            CALL OKREPLY(REPLY,RTNCODE)
            IF (REPLY.EQ.'Y'.AND.RTNCODE.EQ.'0') THEN
                CALL CLRMSG(4)
                CALL CLRMSG(3)
                CALL CLRMSG(2)
                GO TO 125
            ELSE
               CALL CLRMSG(4)
               CALL CLRMSG(3)
               CALL CLRMSG(2)
               RETURN
            END IF
C ** REVISION 8-30-93 ADD RTNCODE=8 IF UPPER AIR FORM IS CLEARED
  125    CONTINUE
         IF (RECTYPE.EQ.'U-A') RTNCODE='8'
         DO 130 J= 1,NUMLN2
               LINQCD(J) = 'N'
            DO 130 I=1,NUMELEM
               IF (PASSWD.EQ.'QC'.OR.FLAGARRAY(I,J,1).LT.'a')THEN
                  VALARRAY(I,J) = BLANK
                  FLAGARRAY(I,J,1) = BLANK
                  FLAGARRAY(I,J,2) = BLANK
               END IF
  130    CONTINUE 
         IELEM = 1
         ILINE = 1
         CALL WRTPAGE(RECTYPE,HOURLBL,STRTELEM,STRTLINE)
         RETURN
C
C  'F7' = CLEAR THIS LINE OF DATA
C
      ELSE IF (RTNFLAG.EQ.'7F') THEN
         DO 140 I1 = 1,NUMELEM
            IF (PASSWD.EQ.'QC'.OR.FLAGARRAY(I1,ILINE,1).LT.'a')THEN
               VALARRAY(I1,ILINE) = BLANK
               FLAGARRAY(I1,ILINE,1) = BLANK
               FLAGARRAY(I1,ILINE,2) = BLANK
            END IF
  140    CONTINUE
         LINQCD(ILINE) = 'N'
         IELEM = 1
         CALL WRTLINE(RECTYPE,HOURLBL,STRTELEM,ILINE,ILINE-STRTLINE+5)
         RETURN
C
C  SHIFTED 'F7' = DELETE THE CURRENT LINE AND SCROLL
C                 ALL SUBSEQUENT LINES UP 1 LINE
C
      ELSE IF (RTNFLAG.EQ.'7S')THEN
         RTNCODE = '7'
         IELEM = 1
         IF (ILINE.LT.NUMLN2)THEN
            DO 160 I1 = ILINE,NUMLN2-1
               I2 = I1 + 1
               DO 160 J1 = 1,NUMELEM
                  VALARRAY(J1,I1) = VALARRAY(J1,I2)
                  FLAGARRAY(J1,I1,1) = FLAGARRAY(J1,I2,1)
                  FLAGARRAY(J1,I1,2) = FLAGARRAY(J1,I2,2)
  160       CONTINUE
         END IF
         DO 165 J1 = 1,NUMELEM
            VALARRAY(J1,NUMLN2) = BLANK
            FLAGARRAY(J1,NUMLN2,1) = BLANK
            FLAGARRAY(J1,NUMLN2,2) = BLANK
  165    CONTINUE
            RETURN
C
C  'F8' = INSERT A BLANK LINE AT THE CURRENT CURSOR POSITION AND
C         SCROLL OTHER LINES DOWN - OFF THE SCREEN IF NECESSARY
C
      ELSE IF (RTNFLAG.EQ.'8F')THEN
         RTNCODE = '7'
         IELEM = 1
         IF (ILINE.LT.NUMLN2)THEN
            DO 170 I1 = NUMLN2-1,ILINE,-1
               I2 = I1 + 1
               DO 170 J1 = 1,NUMELEM
                  VALARRAY(J1,I2) = VALARRAY(J1,I1)
                  FLAGARRAY(J1,I2,1) = FLAGARRAY(J1,I1,1)
                  FLAGARRAY(J1,I2,2) = FLAGARRAY(J1,I1,2)
  170       CONTINUE
         END IF
         DO 175 J1 = 1,NUMELEM
            VALARRAY(J1,ILINE) = BLANK
            FLAGARRAY(J1,ILINE,1) = BLANK
            FLAGARRAY(J1,ILINE,2) = BLANK
  175    CONTINUE
         RETURN
      END IF
C
C  THE FOLLOWING CONTROL KEYS APPLY ONLY TO USERS WITH 'QC' AUTHORITY
C
      IF (PASSWD.EQ.'DE')THEN
          CALL BEEP
C
C  SHIFTED 'F1' = IF UPPER AIR THEN DISPLAY UPPER AIR PLOT
C          ELSE DISPLAY QC LIMITS FOR THIS ELEMENT
C
      ELSE IF(RTNFLAG.EQ.'1S')THEN
         IF (RECTYPE.EQ.'U-A')THEN
            RTNCODE = '5'
         ELSE
            RTNCODE = '6'
         END IF
C
C  SHIFTED 'F6' = SUPPRESS GENERATION OF THIS FIELD
C
      ELSE IF (RTNFLAG.EQ.'6S')THEN
         VALARRAY(IELEM,ILINE) = BLANK
         FLAGARRAY(IELEM,ILINE,1) = 'e'
         FLAGARRAY(IELEM,ILINE,2) = 'b'
         CALL POSLIN(IROW,ICOL)
         CALL WRTLINE(RECTYPE,HOURLBL,STRTELEM,ILINE,IROW)
         RTNFLAG = 'RA'
         GO TO 20
C
C  'F9' = GO TO NEXT 'RED' FLAGGED ELEMENT
C
      ELSE IF (RTNFLAG.EQ.'9F')THEN
         DO 110 I = 1,NUMELEM*NUMLN2
            IELEM = IELEM + 1
            IF (IELEM.GT.NUMELEM)THEN
               IELEM = 1
               ILINE = ILINE + 1
               IF (ILINE.GT.NUMLN2)THEN
                  IELEM = NUMELEM
                  ILINE = NUMLN2
                  RETURN
               END IF
            END IF
            IF (FLAGARRAY(IELEM,ILINE,1).EQ.'C'.OR.
     +           FLAGARRAY(IELEM,ILINE,1).EQ.'c') THEN
               POSFLD = 1
               RETURN
            END IF
  110    CONTINUE
C
C  SHIFTED 'F9' = GO TO PREVIOUS 'RED' FLAGGED ELEMENT
C
      ELSE IF (RTNFLAG.EQ.'9S')THEN
        DO 120 I = 1,NUMELEM*NUMLN2
           IELEM = IELEM - 1
           IF (IELEM.LT.1)THEN
              IELEM = NUMELEM
              ILINE = ILINE - 1
              IF (ILINE.LT.1)THEN
                 IELEM = 1
                 ILINE = 1
                 RETURN
              END IF
           END IF
            IF (FLAGARRAY(IELEM,ILINE,1).EQ.'C'.OR.
     +           FLAGARRAY(IELEM,ILINE,1).EQ.'c') THEN
              POSFLD = 1
              RETURN
           END IF
  120   CONTINUE
C
C  'F10' = RE-EDIT THIS ENTIRE FRAME OF DATA.
C
      ELSE IF (RTNFLAG.EQ.'0F')THEN
         RTNCODE = '4'
      END IF
C
      RETURN
      END
