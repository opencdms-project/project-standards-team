$STORAGE:2

      SUBROUTINE EDPAGE(TEXT,MAXROW,FILENAME)
C
C   ROUTINE TO EDIT A PAGE OF TEXT - IT USES MANY OF THE SAME COMMANDS AS
C   THE NORTON EDITOR BUT IS MUCH SIMPLER.  IT READS THE TEXT FROM FILE
C   FILENAME (MAX OF MAXROW LINES) AND EDITS THE TEXT IN MEMORY.
C 
C     TEXT = THE BUFFER TO BE EDITED. IT MUST BE CHARACTER*80 DIMENSIONED
C            MAXROW IN THE CALLING PROGRAM
C     MAXROW = DIMENSION OF TEXT (SETS MAXIMUM NUMBER OF LINES ALLOWED)
C     FILENAME  = FULL NAME OF THE FILE INCLUDING PATH - MUST BE 
C                 DECLARED AS CHARACTER*64 IN THE CALLING PROGRAM
C
      INTEGER*2 MAXROW, NUMROW
      CHARACTER*80 TEXT(MAXROW),LINE,MESSAGE(4)
      CHARACTER*64 FILENAME,HELPFILE
      CHARACTER*8 INSTEXT
      CHARACTER*3 OUTVAL
      CHARACTER*2 INCHAR
      CHARACTER*1 RTNCODE,REPLY
C
      INTEGER*2 FGCOLOR,BGCOLOR,ENDROW,STRTROW,LINEMARK,HLDMRK
      LOGICAL INSERT,FRSTCL,NEWPAGE
      DATA FGCOLOR,BGCOLOR /7,1/
      DATA FRSTCL /.TRUE./, HELPFILE /'P:\HELP\EDPAGE.HLP'/
C
C   ON FIRST CALL TO THIS ROUTINE SET TEXT COLORS FOR COLOR OR B&W
C
      IF (FRSTCL) THEN
         FRSTCL = .FALSE.
         CALL STATUS(IMODE,ICLTYP,IPAGE)
         CALL GETMSG(343,MESSAGE(1))
         CALL GETMSG(344,MESSAGE(2))
         CALL GETMSG(345,MESSAGE(3))
         CALL GETMSG(346,MESSAGE(4))
         CALL GETMSG(999,MESSAGE)
      END IF
      IF (IMODE.EQ.3) THEN
         IBG = BGCOLOR
         IFG = FGCOLOR
      ELSE
         IBG = 0
         IFG = 1
      END IF
      CALL CLS
C
C   READ THE FILE INTO MEMORY - IF FILE DOESN'T EXIST SET TEXT TO BLANKS 
C
      DO 10 I = 1,MAXROW
         TEXT(I) = ' '
10    CONTINUE
      NUMROW = 1
      OPEN(61,FILE=FILENAME,STATUS='OLD',FORM='FORMATTED',IOSTAT=IERR)
      IF (IERR.EQ.0) THEN
         DO 20 I = 1,MAXROW
            READ(61,'(A80)',END=25) TEXT(I)
 20      CONTINUE
 25      CONTINUE
         NUMROW = I - 1
      END IF
      CLOSE(61)
C
C  INITIALIZE
C
      STRTROW = 1
      ENDROW = MAXROW
      IF (ENDROW.GT.24) THEN 
         ENDROW = 24
      END IF
      INSERT = .FALSE.
      LINEMARK = 0
      INSTEXT = 'Replace '
      MESSAGE(1)(73:80) = INSTEXT
      CALL LOCATE(24,0,IERR)
      CALL WRTSTR(MESSAGE(1),80,1,7)
40    CONTINUE
      DO 50 IROW = 1,ENDROW 
         CALL LOCATE(IROW-STRTROW,0,IERR)
         IF (IROW.GT.MAXROW) THEN
            CALL CHRWRT(' ',0,0,80)
         ELSE IF (IROW.GT.NUMROW) THEN
            CALL CHRWRT(' ',IBG,IFG,80)
         ELSE
            CALL WRTSTR(TEXT(IROW),80,IFG,IBG)
         END IF
50    CONTINUE
60    CONTINUE
      ICOL = 1
      IROW = 1
C
C  ----------- MAJOR INTERACTIVE LOOP BEGINS HERE  --------------
C
100   CONTINUE
      NEWPAGE = .FALSE.
      ISCROLL = 0
      WRITE(OUTVAL,'(I3)') IROW
      CALL LOCATE(24,5,IERR)
      CALL WRTSTR(OUTVAL,3,1,7)
      WRITE(OUTVAL,'(I2)') ICOL
      CALL LOCATE(24,13,IERR)
      CALL WRTSTR(OUTVAL,2,1,7)
      CALL LOCATE(IROW-STRTROW,ICOL-1,IERR)
      CALL GETCHAR(1,INCHAR)        
110   CONTINUE
C
      IF (INCHAR(2:2).EQ.' ') THEN
C 
C ---- A NORMAL KEYBOARD CHARACTER HAS BEEN ENTERED 
C       
C      STORE NORMALLY - IF INSERT MODE IS OFF
C
          IF (.NOT.INSERT) THEN
             TEXT(IROW)(ICOL:ICOL) = INCHAR(1:1)
             CALL CHRWRT(INCHAR(1:1),IBG,IFG,1)
             ICOL = ICOL + 1 
          ELSE
C           
C      INSERT MODE IS ON
C
             DO 120 I = 79,ICOL,-1
                TEXT(IROW)(I+1:I+1) = TEXT(IROW)(I:I)
120          CONTINUE
             TEXT(IROW)(ICOL:ICOL) = INCHAR(1:1)
             ICOL = ICOL + 1
             CALL LOCATE(IROW-STRTROW,0,IERR)
             IF (IROW.EQ.LINEMARK) THEN
                CALL WRTSTR(TEXT(IROW),80,15,IBG)
             ELSE
                CALL WRTSTR(TEXT(IROW),80,IFG,IBG)
             END IF
          END IF
C
C  --- OTHERWISE A CONTROL CHARACTER HAS BEEN ENTERED
C
      ELSE
C
C   CURSOR CONTROL CHARACTERS
C
         IF (.NOT.INSERT.AND.INCHAR.EQ.'BS') THEN
            INCHAR = 'LA'
         END IF
         IF (INCHAR.EQ.'1F') THEN
            CALL DSPWIN(HELPFILE)
         ELSE IF (INCHAR.EQ.'LA') THEN
            ICOL = ICOL - 1
         ELSE IF (INCHAR.EQ.'RA') THEN
            ICOL = ICOL + 1
         ELSE IF (INCHAR.EQ.'DA') THEN
            IROW = IROW + 1
         ELSE IF (INCHAR.EQ.'UA') THEN
            IROW = IROW - 1
         ELSE IF (INCHAR.EQ.'HO') THEN
            ICOL = 1
         ELSE IF (INCHAR.EQ.'EN') THEN
            ICOL = 80
         ELSE IF (INCHAR.EQ.'DP') THEN
            IROW = IROW + ENDROW
            NEWPAGE = .TRUE.
         ELSE IF (INCHAR.EQ.'UP') THEN
            IROW = IROW - ENDROW
            NEWPAGE = .TRUE.
C
C   DELETE CHARACTERS
C
         ELSE IF (INCHAR.EQ.'DE'.OR.INCHAR.EQ.'BS') THEN
            IF (INCHAR.EQ.'BS') THEN
               IF (ICOL.GT.1) THEN
                  ICOL = ICOL - 1
               ELSE
                  IF (IROW.GT.1) THEN
                     IROW = IROW - 1
                     ICOL = 80
                  END IF
                  IF (IROW.LE.STRTROW) THEN
                     GO TO 500
                  END IF
               END IF
            END IF
            IF (ICOL.LT.80) THEN
               DO 130 I = ICOL,80-1
                  TEXT(IROW)(I:I) = TEXT(IROW)(I+1:I+1)
130            CONTINUE
            END IF
            TEXT(IROW)(80:80) = ' '
            CALL LOCATE(IROW-STRTROW,0,IERR)
            IF (IROW.EQ.LINEMARK) THEN
               CALL WRTSTR(TEXT(IROW),80,15,IBG)
            ELSE
               CALL WRTSTR(TEXT(IROW),80,IFG,IBG)
            END IF
C
C     --- INSERT KEY - TURN INSERT MODE ON/OFF ---
C
         ELSE IF (INCHAR.EQ.'IN') THEN
            IF (INSERT) THEN
               INSERT = .FALSE.
               INSTEXT = 'Replace'
            ELSE
               INSERT = .TRUE.
               INSTEXT = 'Insert'
            END IF
            MESSAGE(1)(73:80) = INSTEXT
            CALL LOCATE(24,72,IERR)
            CALL WRTSTR(INSTEXT,8,1,7)
C
C   F3 - SAVE OR QUIT
C
         ELSE IF (INCHAR.EQ.'3F') THEN
            CALL LOCATE(24,0,IERR)
            CALL WRTSTR(MESSAGE(3),80,1,7)
            CALL LOCATE(IROW-STRTROW,ICOL-1,IERR)
            CALL GETCHAR(0,INCHAR)
140         CONTINUE
C
C         SAVE FILE - IF FILE CAN NOT BE CREATED ASK USER FOR A NEW NAME
C
            IF (INCHAR.EQ.'S '.OR.INCHAR.EQ.'E ') THEN
               OPEN(61,FILE=FILENAME,STATUS='UNKNOWN',FORM='FORMATTED'
     +             ,IOSTAT=IERR)
               IF (IERR.NE.0) THEN
                  CALL LOCATE(23,0,IERR)
                  CALL WRTSTR(MESSAGE(4),80,4,7)
                  CALL LOCATE(23,50,IERR)
                  CALL CHRWRT(' ',0,14,30)
                  CALL LOCATE(23,50,IERR)
                  READ(*,'(A30)') FILENAME
                  CALL LOCATE(23,0,IERR)
                  KROW = STRTROW+23
                  IF (KROW.GT.MAXROW) THEN
                     CALL CHRWRT(' ',0,0,80)
                  ELSE IF (KROW.EQ.LINEMARK) THEN
                     CALL WRTSTR(TEXT(KROW),80,15,IBG)
                  ELSE
                     CALL WRTSTR(TEXT(KROW),80,IFG,IBG)
                  END IF
                  CALL LOCATE(IROW-STRTROW,ICOL-1,IERR)
                  GO TO 140
               ELSE
                  DO 150 I = 1,NUMROW
                     WRITE(61,'(A80)') TEXT(I)
150               CONTINUE
                  CLOSE(61)
               END IF
               IF (INCHAR.EQ.'E ') THEN
                  RETURN
               ELSE
                  CALL LOCATE(24,0,IERR)
                  CALL WRTSTR(MESSAGE(1),80,1,7)
               END IF
C
C        USER HAS PRESSED F3-Q ABORT.  VERIFY INTENTION AND QUIT.
C
            ELSE IF (INCHAR.EQ.'Q ') THEN
               CALL WRTMSG(1,200,12,0,0,' ',0)
               CALL LOCATE(24,45,IERR)
               CALL OKREPLY(REPLY,RTNCODE)
               IF (REPLY.EQ.'Y') THEN
                  RETURN
               ELSE
                  CALL LOCATE(24,0,IERR)
                  CALL WRTSTR(MESSAGE(1),80,1,7)
               END IF
            ELSE IF (INCHAR.EQ.'4F') THEN
               CALL LOCATE(24,0,IERR)
               CALL WRTSTR(MESSAGE(1),80,1,7)
               GO TO 110
            ELSE
               CALL LOCATE(24,0,IERR)
               CALL WRTSTR(MESSAGE(1),80,1,7)
            END IF
C
C   F4 - MARK LINE THEN COPY OR MOVE
C
         ELSE IF (INCHAR.EQ.'4F') THEN
            CALL LOCATE(24,0,IERR)
            CALL WRTSTR(MESSAGE(2),80,1,7)
            CALL LOCATE(IROW-STRTROW,ICOL-1,IERR)
            CALL GETCHAR(0,INCHAR)
            CALL LOCATE(24,0,IERR)
            CALL WRTSTR(MESSAGE(1),80,1,7)
            IF (INCHAR.EQ.'3F') THEN
               GO TO 110
            END IF
            IF (INCHAR.EQ.'M '.AND.IROW.EQ.LINEMARK) THEN
               INCHAR = ' '
            END IF
C
C        MOVE LINE (DELETE AND FALL THROUGH TO COPY)
C
            IF (LINEMARK.GT.0) THEN
               IF (INCHAR.EQ.'M ') THEN
                  HLDMRK = LINEMARK
                  CALL DELLINE(TEXT,NUMROW,LINEMARK,ISCROLL
     +                ,LINEMARK,IFG,IBG)
                  IF (HLDMRK.LT.IROW) THEN
                     IROW = IROW - 1
                  END IF
                  IF (HLDMRK.GE.STRTROW.AND.HLDMRK-STRTROW
     +                  .LE.ENDROW-1) THEN
                     CALL CHKSCRL(TEXT,NUMROW,HLDMRK,ENDROW
     +                   ,STRTROW,ISCROLL,0,IFG,IBG,MAXROW)
                  END IF
               END IF
C
C           COPY LINE INTO NEW LOCATION
C               
               IF (INCHAR.EQ.'C '.OR.INCHAR.EQ.'M ') THEN
                  CALL INSLINE(TEXT,NUMROW,MAXROW,IROW,ISCROLL,LINEMARK)
                  TEXT(IROW) = LINE
                  CALL CHKSCRL(TEXT,NUMROW,IROW,ENDROW,STRTROW,ISCROLL
     +                 ,LINEMARK,IFG,IBG,MAXROW)
               ELSE IF (INCHAR.NE.'C '.AND.INCHAR.NE.'R '.AND.
     +                INCHAR.NE.'L ') THEN
                  GO TO 500
               END IF
               IF (INCHAR.NE.'C ') THEN
                  IF (LINEMARK.GE.STRTROW.AND.
     +                  LINEMARK.LE.STRTROW+ENDROW-1) THEN
                     CALL LOCATE(LINEMARK-STRTROW,0,IERR)
                     CALL WRTSTR(TEXT(LINEMARK),80,IFG,IBG)
                  END IF
                  LINEMARK = 0
               END IF
            END IF
            IF (INCHAR.EQ.'L ') THEN
               LINEMARK = IROW
               CALL LOCATE(IROW-STRTROW,0,IERR)
               CALL WRTSTR(TEXT(IROW),80,15,IBG)
            END IF
C
C   ALT-L CLEAR TO END OF LINE AND  ALT-K ERASE LINE
C
         ELSE IF (INCHAR.EQ.'AL') THEN
            TEXT(IROW)(ICOL:80) = '         '
            CALL CHRWRT(' ',IBG,IFG,81-ICOL)
         ELSE IF (INCHAR.EQ.'AK') THEN
            CALL DELLINE(TEXT,NUMROW,IROW,ISCROLL,LINEMARK,IFG,IBG)
C
C   F8 - INSERT LINE
C
         ELSE IF (INCHAR.EQ.'RE') THEN
            IF (INSERT.OR.IROW.EQ.NUMROW) THEN
               CALL INSLINE(TEXT,NUMROW,MAXROW,IROW,ISCROLL,LINEMARK)
               ICOL = 1
            ELSE
               ICOL = 1
               IROW = IROW + 1
            END IF
         END IF
      END IF
C
C   IF AT MARKED LINE SET COPY BUFFER (THUS BUFFER IS ALWAYS EQUAL TO
C   CURRENT CONTENTS OF MARKED LINE - EVEN IF LINE IS CHANGED AFTER
C   IT WAS MARKED
C
500   CONTINUE
      IF (IROW.EQ.LINEMARK) THEN
         LINE = TEXT(IROW)             
      END IF
C
C   CHECK CURSOR POSITION AND ADJUST CURSOR OR SCREEN IF NECESSARY
C
      IF (ICOL.GT.80) THEN
         IF (IROW.LT.NUMROW) THEN
            ICOL = 1
            IROW = IROW + 1
         ELSE
            ICOL = 80
         END IF
      ELSE IF (ICOL.LT.1) THEN
         IF (IROW.GT.1) THEN
            ICOL = 80
            IROW = IROW - 1
         ELSE 
            ICOL = 1
         END IF
      END IF
      IF (IROW.GT.NUMROW) THEN
         IROW = NUMROW
      ELSE IF (IROW.LT.1) THEN
         IROW = 1
      END IF
      IF (IROW-STRTROW.GT.ENDROW-1) THEN
         IF (IROW.GT.NUMROW) THEN
            IROW = NUMROW
         ELSE
            ISCROLL = 2
         END IF
      ELSE IF (IROW.LT.STRTROW) THEN
         IF (STRTROW.EQ.1) THEN
            IROW = STRTROW
         ELSE
            ISCROLL = 1
         END IF
      END IF
      IF (NEWPAGE) THEN
         NEWPAGE = .FALSE.
         IF (IROW.LT.STRTROW) THEN
            NEWPAGE = .TRUE.
            STRTROW = STRTROW - ENDROW
            IF (STRTROW.LT.1) THEN
               STRTROW = 1
            END IF
         ELSE IF (IROW.GT.STRTROW+ENDROW-1) THEN
            NEWPAGE = .TRUE.
            STRTROW = STRTROW + ENDROW
            IF (STRTROW.GT.NUMROW-(ENDROW/2)) THEN
               STRTROW = NUMROW - ENDROW/2
            END IF
         END IF
         IF (NEWPAGE) THEN
            NEWPAGE = .FALSE. 
            DO 600 JROW = STRTROW,STRTROW+ENDROW-1
               CALL LOCATE(JROW-STRTROW,0,IERR)
               IF (JROW.GT.MAXROW) THEN
                  CALL CHRWRT(' ',0,0,80)
               ELSE IF (IROW.GT.NUMROW) THEN
                  CALL CHRWRT(' ',IBG,IFG,80)
               ELSE
                  IF (JROW.EQ.LINEMARK) THEN
                     CALL WRTSTR(TEXT(JROW),80,15,IBG)
                  ELSE
                     CALL WRTSTR(TEXT(JROW),80,IFG,IBG)
                  END IF
               END IF
600         CONTINUE
         END IF
      ELSE
         CALL CHKSCRL(TEXT,NUMROW,IROW,ENDROW,STRTROW,ISCROLL
     +        ,LINEMARK,IFG,IBG,MAXROW)
      END IF
C
      GO TO 100

      END
************************************************************************
      SUBROUTINE CHKSCRL(TEXT,NUMROW,IROW,ENDROW,STRTROW,ISCROLL
     +        ,LINEMARK,IFG,IBG,MAXROW)
C
C   SCROLL SCREEN/DATA IF NECESSARY TO KEEP REQUESTED LINE ON THE SCREEN
C
      CHARACTER*80 TEXT(NUMROW)
      INTEGER*2 ENDROW,STRTROW,LINEMARK,ISCROLL,IFG,IBG,ENDSCRN

      JROW = -1
      ENDSCRN = ENDROW - 1
      LSTROW = STRTROW+ENDROW-1
      IF (LSTROW.GT.MAXROW) THEN
         ENDSCRN = NUMROW - STRTROW
      END IF
      IF (ISCROLL.EQ.1) THEN
         CALL SCROLL(0,1,0,0,ENDSCRN,79)
         STRTROW = STRTROW - 1
         IROW = STRTROW
         CALL LOCATE(0,0,IERR)
         JROW = IROW
      ELSE IF (ISCROLL.EQ.2) THEN
         CALL SCROLL(1,1,0,0,ENDSCRN,79)
         STRTROW = STRTROW + 1
         CALL LOCATE(ENDROW-1,0,IERR)
         JROW = IROW
      ELSE IF (ISCROLL.EQ.3) THEN
         CALL SCROLL(1,1,IROW-STRTROW,0,ENDSCRN,79)
         JROW = STRTROW+ENDSCRN
         CALL LOCATE(ENDSCRN,0,IERR)
      ELSE IF (ISCROLL.EQ.4) THEN
         CALL SCROLL(0,1,IROW-STRTROW,0,ENDSCRN,79)
         CALL LOCATE(IROW-STRTROW,0,IERR)
         JROW = IROW
      END IF
      IF (JROW.GT.0) THEN
         IF (JROW.GT.MAXROW) THEN
            CALL CHRWRT(' ',0,0,80)
         ELSE IF (JROW.EQ.LINEMARK) THEN
            CALL WRTSTR(TEXT(JROW),80,15,IBG)
         ELSE
            CALL WRTSTR(TEXT(JROW),80,IFG,IBG)
         END IF
      END IF
      ISCROLL = 0
      RETURN
      END
************************************************************************
      SUBROUTINE INSLINE(TEXT,NUMROW,MAXROW,IROW,ISCROLL,LINEMARK)
C
C   ROUTINE TO INSERT A LINE INTO THE FILE BUFFER AT IROW
C
      CHARACTER*80 TEXT(MAXROW)

      IF (NUMROW.LT.MAXROW) THEN
         NUMROW = NUMROW + 1
      END IF
      IF (NUMROW.EQ.IROW+1) THEN
         IROW = NUMROW
         TEXT(IROW) = '         '
      ELSE
         DO 100 I2 = NUMROW, IROW, -1
            TEXT(I2) = TEXT(I2-1)
100      CONTINUE
         TEXT(IROW) = '         '
      END IF
      ISCROLL = 4
      IF (LINEMARK.GT.0) THEN
         IF (LINEMARK.GE.IROW) THEN
            LINEMARK = LINEMARK + 1
         END IF
      END IF
      RETURN
      END
***********************************************************************
      SUBROUTINE DELLINE(TEXT,NUMROW,IROW,ISCROLL,LINEMARK,IFG,IBG)
C
C   ROUTINE TO DELETE A LINE FROM THE TEXT BUFFER
C
      CHARACTER*80 TEXT(NUMROW)

      DO 150 I2 = IROW,NUMROW-1
         TEXT(I2) = TEXT(I2+1)
150   CONTINUE
      TEXT(NUMROW) = '     '
      IF (IROW.EQ.NUMROW) THEN
         CALL POSLIN(JROW,ICOL)
         CALL LOCATE(JROW,0,IERR)
         CALL WRTSTR(TEXT(NUMROW),80,IFG,IBG)
         ISCROLL = 0
      ELSE
         ISCROLL = 3
      END IF
      NUMROW = NUMROW - 1
      IF (LINEMARK.GT.0) THEN
         IF (LINEMARK.EQ.IROW) THEN
            LINEMARK = 0
         ELSE IF (LINEMARK.GT.IROW) THEN
            LINEMARK = LINEMARK - 1
         END IF
      END IF
      RETURN
      END
