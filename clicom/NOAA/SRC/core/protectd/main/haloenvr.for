$STORAGE:2
C------------------------------------------------------------------------------
C     DEFINE THE INTERFACE TO THE C ROUTINE "SYSTEM"
C------------------------------------------------------------------------------
      INTERFACE TO INTEGER*2 FUNCTION SYSTEM [C]
     +        (STRING[REFERENCE])
      CHARACTER*1 STRING
      END
C
      PROGRAM HALOENVR
C------------------------------------------------------------------------------
C     PROGRAM TO SPECIFY THE EQUIPMENT FOR THE GRAPHICS ENVIRONMENT FILE AND
C     COPY THE NECESSARY HALO FILES INTO THE HALO SUBDIRECTORY.
C------------------------------------------------------------------------------
$INCLUDE:'HALOENV.INC'
      CHARACTER*78  MSGLIN
      CHARACTER*40  TXTLST(100)
      CHARACTER*24  DEVDRV, PRNDRV, NOECHO
      CHARACTER*64  HELPFILE,  HELPWDN, COPYSTRG
      CHARACTER*15  DEVFIL(100), PRNFIL(100)
      CHARACTER*12  MENUNAME
      CHARACTER*2   RTNFLG
      CHARACTER*1   HALOID, DELIM/'^'/, DUPSTRG(64), YESUP, YESLO, YN
      INTEGER       MODEV(100,3), NTRY, PIKDEV, PIKPRN, SYSTEM
      LOGICAL       HALOFILE, WARNMSG
      EQUIVALENCE   (COPYSTRG,DUPSTRG(1))
      DATA          HELPWDN /'P:\HELP\DSPWDN.HLP'/, WARNMSG/.FALSE./
C
C---  READ INTO MEMORY THE LISTS OF STANDARD HALO GRAPHIC DEVICES
C---  AND PRINTERS. ALSO READ IN CURRENT HALO  ENVIRONMENT VARIABLES
C
      COPYSTRG = 'COPY A:\CLICOM\HALO\'
      NOECHO = '>> C:\INSTOUT2.DAT' // CHAR(0)
      ENVFILE = 'P:\HALO\HALOGRF .ENV'
      CALL GETYN(1,1,YESUP,YESLO)
C
   10 CALL CLS
      IROW=3
      ICOL=3
      LANTYPE=0
      MENUNAME='HALO-PC-TYPE'
      HELPFILE='P:\HELP\HALONETW.HLP'
      CALL LOCATE (IROW,ICOL,IERR)
      CALL GETMNU(MENUNAME,HELPFILE,ITYPE)
      IF   (ITYPE .EQ. 0) THEN
C---  WHEN EXITING PGM, WARN USER IF THERE IS NO GRAPHICS ENVIRONMENT FILE
           OPEN (UNIT=12,FILE='P:\HALO\HALOGRFA.ENV',FORM='FORMATTED',
     +     STATUS='OLD',IOSTAT=IOHALO)
           IF (IOHALO .NE. 0) THEN
               IF (.NOT.WARNMSG) THEN
                  RTNFLG = '  '
                  CALL WRTMSG(7,460,12,0,0,' ',0)
                  CALL WRTMSG(6,461,12,1,0,' ',0)
                  CALL GETCHAR(0,RTNFLG)
                  IF (RTNFLG .NE. 'ES' .AND. RTNFLG .NE. '4F') THEN
                     GO TO 10
                  ENDIF
               ENDIF
           ENDIF
           GO TO 900
      ELSE IF(ITYPE .EQ. 1) THEN
           HALOID = 'A'
      ELSE IF(ITYPE .EQ. 2) THEN
           YN = ' '
           CALL WRTMSG(8,441,14,0,0,' ',0)
           CALL LOCATE(17,75,IERR)
           CALL GETSTR(0,YN,1,15,1,RTNFLG)
           IF (YN .EQ. YESUP .OR. YN .EQ. YESLO) THEN
              LANTYPE = 1
           ENDIF
           HALOID = 'A'
      ELSE IF(ITYPE .EQ. 3) THEN
           YN = ' '
           CALL WRTMSG(9,441,14,0,0,' ',0)
           CALL LOCATE(16,75,IERR)
           CALL GETSTR(0,YN,1,15,1,RTNFLG)
           IF (YN .EQ. YESUP .OR. YN .EQ. YESLO) THEN
              LANTYPE = 1
           ENDIF
           NTRY = 0
   25      CALL WRTMSG(8,467,14,0,0,' ',0)
           CALL LOCATE(17,75,IERR)
           HALOID=' '
           CALL GETSTR(0,HALOID,1,15,1,RTNFLG)
           IF (HALOID .LT. 'B' .OR. HALOID .GT. 'Z') THEN
              CALL WRTMSG(10,70,12,0,0,' ',0)
              NTRY = NTRY + 1
              IF (NTRY .GT. 3) THEN
                 GO TO 10
              ENDIF
              GO TO 25 
           ENDIF
      ENDIF
      ENVFILE(16:16) = HALOID
      IOCHK = 0
      OPEN (UNIT=10,FILE=ENVFILE,FORM='FORMATTED',STATUS='UNKNOWN')
      READ(10,*,IOSTAT=IOCHK) DEVICE,DEVMODE,AQCMODE,NETWORK,PRINTR,
     +                        PTRVAL,PTRASP,PRNTR2,PRVAL2,PRASP2,PLOTER
      IF (IOCHK .NE. 0) THEN
C
C------  THE HALO ENVIRONMENT FILE DOES NOT EXIST. SET DEFAULT VALUES
C
         DO 40 KL = 1,20
            PTRVAL(KL) = -1
            PRVAL2(KL) = -1
   40    CONTINUE
         NETWORK= 0
         PTRASP = 0.0
         PRASP2 = 0.0
         DEVICE = '^P:\HALO\HALOQQQQ.DEV^'
         PRINTR = '^P:\HALO\HALOQQQQ.PRN^'
         PRNTR2 = PRINTR
         PLOTER = '^P:\HALO\HALOQQQQ.EXE^'
      ENDIF     
C   
C---  DISPLAY HALO GRAPHICS HARDWARE MENU
C
  100 CALL CLS
      IROW=3
      ICOL=3
      CALL LOCATE (IROW,ICOL,IERR)
      MENUNAME='HALO-ENVIRON'
      HELPFILE='P:\HELP\HALOENVR.HLP'
      CALL GETMNU(MENUNAME,HELPFILE,ITYPE)
      IF (ITYPE .EQ. 0) THEN
         CALL LOCATE(22,0,IERR)
C
C-----   PUT REVISED VARIABLES IN HALO GRAPHICS ENVIRONMENT FILE
C 
         REWIND(10)
         IF (DEVICE(14:17) .EQ. 'QQQQ' ) THEN
            RTNFLG  = '  '
            WARNMSG = .TRUE.
            CALL WRTMSG(7,460,12,0,0,' ',0)
            CALL WRTMSG(6,461,12,1,0,' ',0)
            CALL GETCHAR(0,RTNFLG)
            IF (RTNFLG .EQ. 'ES' .OR. RTNFLG .EQ. '4F') THEN
               GO TO 10
            ENDIF
            GO TO 100
         ENDIF
         NETWORK = LANTYPE
         WRITE(10,135) DEVICE,DEVMODE,AQCMODE,NETWORK,PRINTR,PTRVAL,
     +                 PTRASP,PRNTR2,PRVAL2,PRASP2,PLOTER
  135    FORMAT('''',A24,'''',','I4,',',I4,',',I4/''''A24''''/2(I5,','),
     +   8(I3,','),2(I4,','),8(I3,','),F5.3/'''',A24,''''/2(I5,','),
     +   8(I3,','),2(I4,','),8(I3,','),F5.3/'''',A24,'''')
         CLOSE (10)
         IF (ENVFILE(16:16) .NE. 'A') THEN
            CALL GETMSG(458,MSGLIN)
            MSGLIN(52:52) = ENVFILE(16:16)
            CALL LOCATE(16,1,IERR)
            CALL WRTSTR(MSGLIN,60,14,0)
            CALL WRTMSG(8,459,14,1,1,' ',0)
            CALL CLRMSG(9)
            CALL LOCATE(18,1,IERR)
         ENDIF
C
C ---    COPY APPROPRIATE HALO FILES FROM INSTALLATION DISKETTE TO CLICOM.
C ---    IF THEY ARE NOT ALREADY IN THE HALO DIRECTORY   1-AUG-91
C
         MSGLIN =  'P:\HALO\' // DEVICE(10:21)
         INQUIRE (FILE=MSGLIN,EXIST=HALOFILE)
         IF (.NOT. HALOFILE) THEN         
            COPYSTRG(21:64) = DEVICE(10:21) // ' P:\HALO ' // NOECHO
            I = SYSTEM(DUPSTRG)
         ENDIF
         MSGLIN =  'P:\HALO\HALOVDIN.DEV'
         INQUIRE (FILE=MSGLIN,EXIST=HALOFILE)
         IF (.NOT. HALOFILE) THEN         
            COPYSTRG(21:32) = 'HALOVDIN.DEV'
            I = SYSTEM(DUPSTRG)
         ENDIF

         IF (PRINTR(14:17) .NE. 'QQQQ' ) THEN
            MSGLIN =  'P:\HALO\' // PRINTR(10:21)
            INQUIRE (FILE=MSGLIN,EXIST=HALOFILE)
            IF (.NOT. HALOFILE) THEN         
               COPYSTRG(21:64) = PRINTR(10:21) // ' P:\HALO ' // NOECHO
               I = SYSTEM(DUPSTRG)
            ENDIF
         ENDIF
         IF (PRNTR2(14:17) .NE. 'QQQQ' ) THEN
            MSGLIN =  'P:\HALO\' // PRNTR2(10:21)
            INQUIRE (FILE=MSGLIN,EXIST=HALOFILE)
            IF (.NOT. HALOFILE) THEN         
               COPYSTRG(21:64) = PRNTR2(10:21) // ' P:\HALO ' // NOECHO
               I = SYSTEM(DUPSTRG)
            ENDIF
         ENDIF
         GO TO 10
      ENDIF
C
C-----  UPDATE VIDEO DEVICE HALO INFO 
C
      IF (ITYPE .EQ. 1) THEN
         J=1
         OPEN (UNIT=8,FILE='P:\HALO\HALODEV.LST',FORM='FORMATTED')
  110    READ(8,111,END=115) TXTLST(J),DEVFIL(J),(MODEV(J,K),K=1,3)
  111    FORMAT(A40,A15,3I3)
         J=J+1
         GO TO 110
  115    NBRDEV=J-1
         CLOSE (8)
C
C-----   DISPLAY DEVICE NAMES IN WINDOW. PIKDEV IS USER'S CHOICE, 1-N
C
  120    CALL DSPWDN(TXTLST,NBRDEV,PIKDEV,HELPWDN)
         IF (PIKDEV .EQ. 0) THEN
            GO TO 100
         ENDIF
         DEVDRV = 'P:\HALO\'//DEVFIL(PIKDEV)
         DEVICE = DELIM // DEVDRV(1:21) // DELIM
C
C-----   DISPLAY MENU FOR VIDEO DEVICE MODE. 
C
         CALL LOCATE (IROW+3,ICOL+41,IERR)
         MENUNAME='HALO-VIDEO  '
         CALL GETMNU(MENUNAME,HELPFILE,MODNBR)
         IF (MODNBR .NE. 0) THEN
            IF (MODNBR .EQ. 3) THEN
               MODNBR=1
            ENDIF
            DEVMODE=MODEV(PIKDEV,MODNBR)
            IF (DEVMODE .EQ. -1) THEN
               DEVMODE= MODEV(PIKDEV,1)
C              INFO MSG HERE: VGA MODE NOT AVAILABLE, EGA MODE USED BY DEFAULT
               CALL WRTMSG(4,489,14,0,1,' ',0)
            ENDIF
            AQCMODE = MODEV(PIKDEV,3)
         ENDIF 
      ENDIF
C
C-----  UPDATE PRINTER HALO INFO 
C
      IF (ITYPE .EQ. 2 .OR. ITYPE .EQ. 3) THEN
         OPEN (UNIT=8,FILE='P:\HALO\HALOPRN.LST',FORM='FORMATTED')
         J=1
  210    READ(8,111,END=215) TXTLST(J),PRNFIL(J)
         J=J+1
         GO TO 210
  215    NBRPRN=J-1
         CLOSE (8)
C        CHECK TO SEE IF A READER ENVIRONMENT FILE BEING UPDATED...
         IF (PRINTR(14:17) .EQ. 'QQQQ' .AND. ITYPE .EQ. 3) THEN
            IF (ENVFILE(16:16) .EQ. 'A') THEN
               CALL WRTMSG(6,487,12,0,1,' ',0)
            ELSE
               CALL WRTMSG(7,487,12,0,0,' ',0)
               CALL WRTMSG(6,465,12,0,0,' ',0)
               CALL WRTMSG(5,466,12,0,1,' ',0)
               CALL CLRMSG(6)
               CALL CLRMSG(5)
            ENDIF
            GO TO 100
         ENDIF
C
C-----   DISPLAY PRINTER NAMES IN WINDOW. PIKPRN IS USER'S CHOICE, 1-N
C
  220    CALL DSPWDN(TXTLST,NBRPRN,PIKPRN,HELPWDN)
         IF (PIKPRN .EQ. 0) THEN
            GO TO 100
         ENDIF
         IF (ITYPE .EQ. 2) THEN
            PRNDRV = 'P:\HALO\'//PRNFIL(PIKPRN)
            PRINTR = DELIM // PRNDRV(1:21) // DELIM
            PTRASP = 0.0
            DO 240 KL = 1,20
               PTRVAL(KL) = -1
  240       CONTINUE
            IF (TXTLST(PIKPRN)(18:20) .EQ. 'IBM' .OR.
     +          TXTLST(PIKPRN)(1:5)   .EQ. 'IBM P') THEN
                PTRVAL(13) = 3
            ENDIF
C------  QUERY USER TO SPECIFY ENGLISH-METRIC UNITS. SET PRINTER UNIT VARIABLE
  250       CALL WRTMSG(9,488,14,0,0,' ',0)
            CALL LOCATE(16,75,IERR)
            HALOID=' '
            CALL GETSTR(0,HALOID,1,15,1,RTNFLG)
            IF (HALOID .EQ. '1') THEN
               PTRVAL(19) = 100
            ELSE
               IF (HALOID .EQ. '2') THEN
                  PTRVAL(19) = 254
               ELSE
                  CALL WRTMSG(7,69,12,1,0,' ',0)
                  GO TO 250
               ENDIF
            ENDIF
         ELSE
            PRNDRV = 'P:\HALO\'//PRNFIL(PIKPRN)
            PRNTR2 = DELIM // PRNDRV(1:21) // DELIM
            PRASP2 = 0.0
            DO 270 KL = 1,20
               PRVAL2(KL) = -1
  270       CONTINUE
            IF (TXTLST(PIKPRN)(18:20) .EQ. 'IBM' .OR.
     +          TXTLST(PIKPRN)(1:5)   .EQ. 'IBM P') THEN
                PRVAL2(13) = 3
            ENDIF
            IF (PRINTR .NE. PRNTR2) THEN
               PRVAL2(17) = 1
            ENDIF
C------  QUERY USER TO SPECIFY ENGLISH-METRIC UNITS. SET PRINTER UNIT VARIABLE
  290       CALL WRTMSG(9,488,14,0,0,' ',0)
            CALL LOCATE(16,75,IERR)
            HALOID=' '
            CALL GETSTR(0,HALOID,1,15,1,RTNFLG)
            IF (HALOID .EQ. '1') THEN
               PRVAL2(19) = 100
            ELSE
               IF (HALOID .EQ. '2') THEN
                  PRVAL2(19) = 254
               ELSE
                  CALL WRTMSG(7,69,12,1,0,' ',0)
                  GO TO 290
               ENDIF
            ENDIF
         ENDIF
      ENDIF
C
C-----  UPDATE PLOTTER HALO INFO 
C
      IF (ITYPE .EQ. 4) THEN
         J=1
         CALL WRTMSG(4,549,12,0,1,' ',0)
      ENDIF
      GO TO 100
 900  CALL LOCATE(1,0,IERR)
      CALL CLS
      STOP ' '
      END
C************************************************************************
      SUBROUTINE DSPWDN(TXTLINE,NBRDEV,NPICK,HLPFIL)
C------------------------------------------------------------------------------
C     PRINT A TEXT FILE IN A SCREEN WINDOW 1 PAGE (17 LINES) AT A TIME.
C     SCROLL UP 1 IF A PgUp IS HIT, GO TO THE NEXT PAGE IF A PgDn IS HIT, AND
C     GO TO THE 1ST PAGE IF Home KEY IS HIT.  CHARACTERS WITHIN THE MESSAGE 
C     FILE DELIMITTED BY ^'S ARE WRITTEN WITH A RED BACKGROUND (THESE ^ 
C     MESSAGES CAN NOT SPAN LINES.  EACH LINE MUST HAVE ITS OWN SET OF ^'S)
C
C     INPUT ARGUMENTS:
C
C     TXTLINE   CHAR ARRAY  LIST OF TEXT TO DISPLAY IN THE WINDOW
C     NBRDEV    INT2        NUMBER OF LINES IN THE FILE
C     HLPFIL    CHAR        NAME OF HELP FILE FOR THIS WINDOW
C
C     OUTPUT ARGUMENT:
C
C     NPICK     INT2        NUMBER OF THE TEXT LINE WHICH HAS THE ITEM 
C                           DESIRED.
C------------------------------------------------------------------------------
C     ---> MAXIMUM OF 40 CHARACTERS PER LINE AND 100 LINES PER FILE <---
C
      CHARACTER*64 HLPFIL
      CHARACTER*40 TXTLINE(100)
      CHARACTER*2 INCHAR
      INTEGER*2 STRTROW,STRTCOL,ENDROW,ENDCOL,BUFFER(1300,2),CHOICE
      COMMON /WINDOW/ BUFFER
      COMMON /WINFO/  KROW,KCOL,CHOICE,MAXROW,MINROW
C
      DATA IFG,IBG,IBG2 /0,3,4/
      DATA IWIN /1/
C
C   OPEN THE TEXT WINDOW ON THE RIGHT SIDE OF SCREEN. THE SIZE OF THE  
C   WINDOW IS 56 CHARACTERS TO ACCOMMODATE THE FUNCTION KEY LINE.
C
      STRTCOL = 22
      STRTROW =  3
      ENDCOL  = STRTCOL + 56
      ENDROW  = STRTROW + 19
      CALL OPENWIN(IWIN,BUFFER(1,IWIN),STRTROW,STRTCOL,ENDROW,ENDCOL)
      NUMLINE=NBRDEV
      NPICK  =0
C
C   PRINT THE TEXT FILE ONE PAGE AT A TIME AND CHECK INPUT FROM THE USER
C
      IPAGE = 1
      CALL WINRIT(IPAGE,NUMLINE,TXTLINE,IFG,IBG,IBG2,STRTROW,STRTCOL
     +            ,IWIN)
  100 CONTINUE
      CALL GETCHAR(0,INCHAR)
C  
C   MOVE HIGHLIGHT BAR UP-DOWN WITHIN WINDOW AS USER DIRECTS
C
      IF (INCHAR .EQ. 'UA') THEN
         KROW=KROW-1
         IF (KROW .LE. MINROW) THEN
            KROW=KROW+1
            CALL BEEP
         ELSE
            CALL DSPREC(KROW+1,KCOL,0,TXTLINE(CHOICE),40)
            CHOICE=CHOICE-1
            CALL DSPREC(KROW,KCOL,1,TXTLINE(CHOICE),40)
         ENDIF
      ENDIF
      IF (INCHAR .EQ. 'DA') THEN
         KROW=KROW+1
         IF (KROW .GT. MAXROW) THEN
            KROW=KROW-1
            CALL BEEP
         ELSE
            CALL DSPREC(KROW-1,KCOL,0,TXTLINE(CHOICE),40)
            CHOICE=CHOICE+1
            CALL DSPREC(KROW,KCOL,1,TXTLINE(CHOICE),40)
         ENDIF
      ENDIF
      IF (INCHAR.EQ.'DP') THEN
         CALL WINRIT(IPAGE,NUMLINE,TXTLINE,IFG,IBG,IBG2,STRTROW,STRTCOL
     +              ,IWIN)
      ENDIF 
      IF (INCHAR.EQ.'UP') THEN
         IF (IPAGE.GT.2) THEN
            IPAGE = IPAGE - 2
            CALL WINRIT(IPAGE,NUMLINE,TXTLINE,IFG,IBG,IBG2,STRTROW
     +          ,STRTCOL,IWIN)
         ELSE
            CALL BEEP
C            IPAGE = 1
C            CALL WINRIT(IPAGE,NUMLINE,TXTLINE,IFG,IBG,IBG2,STRTROW
C     +          ,STRTCOL,IWIN)
         ENDIF
      ENDIF
      IF (INCHAR.EQ.'HO') THEN
         IPAGE = 1
         CALL WINRIT(IPAGE,NUMLINE,TXTLINE,IFG,IBG,IBG2,STRTROW,STRTCOL
     +              ,IWIN)
      ENDIF
      IF (INCHAR .EQ. '1F') THEN
         CALL DSPWIN(HLPFIL)
      ENDIF
      IF (INCHAR .EQ. '4F' .OR. INCHAR .EQ. 'ES') THEN
         CALL LOCATE(24,0,IERR)
         CALL CLOSWIN(IWIN,BUFFER(1,IWIN))
         RETURN
      ENDIF
      IF (INCHAR.EQ.'RE') THEN
         CALL LOCATE(24,0,IERR)
         CALL CLOSWIN(IWIN,BUFFER(1,IWIN))
         NPICK=CHOICE
         RETURN
      ENDIF
      GO TO 100  
      END
C**********************************************************************
      SUBROUTINE WINRIT(IPAGE,NUMLINE,TXTLINE,IFG,IBG,IBG2,STRTROW
     +                 ,STRTCOL,IWIN)
C------------------------------------------------------------------------------
C     ROUTINE TO WRITE A WINDOW WORTH OF TXTLIN TO THE SCREEN
C     AND MOVE PAGE POINTER TO NEXT PAGE
C
C     INPUT ARGUMENTS:
C 
C     IPAGE    INT2        PAGE NUMBER OF TEXT FILE WRITTEN IN WINDOW
C     NUMLINE  INT2        NUMBER OF LINES IN THE WINDOW
C     TXTLINE  CHAR ARRAY  LIST OF TEXT TO DISPLAY IN THE WINDOW
C     IFG      INT2        FORGROUND COLOR
C     IBG      INT2        BACKGROUND COLOR
C     IBG2     INT2        BACKGROUND COLOR FOR ^ MESSAGES
C     STRTROW  INT2        BEGINING ROW NUMBER ON SCREEN FOR THIS WINDOW
C     STRTCOL  INT2        BEGINING COLUMN NUMBER ON SCREEN FOR THIS WINDOW
C     IWIN     INT2        NUMBER OF WINDOW THIS IS ( 1 OR 2)
C
C     OUTPUT ARGUMENT:  NONE
C------------------------------------------------------------------------------
      CHARACTER*40 TXTLINE(100),MORTXT,ENDTXT
      CHARACTER*81 MESSAGE
      CHARACTER*3  DEVERS
      INTEGER*2 STRTROW,STRTCOL,LENMSG,CHOICE
      LOGICAL FRSTCL
      COMMON /WINFO/  KROW,KCOL,CHOICE,MAXROW,MINROW
      DATA FRSTCL /.TRUE./ ,MESSAGE,MORTXT,ENDTXT /3*' '/
C
C   ON FIRST CALL READ ALL MESSAGE TEXT
C
      IF (FRSTCL) THEN
         FRSTCL = .FALSE.
         CALL GETMSG(310,MESSAGE)
         DO 50 J = 1,80
            IF (MESSAGE(J:J).EQ.',') THEN
               ISPLIT = J
               GO TO 55
            END IF
50       CONTINUE
55       CONTINUE
         MORTXT = '^ '
         MORTXT(2:ISPLIT-2) = MESSAGE(2:ISPLIT-2)
         MORTXT(ISPLIT-1:ISPLIT-1) = '^' 
         ENDTXT = '^  '
         DO 60 J = ISPLIT+2,80
            IF (MESSAGE(J:J).EQ.'''') THEN
               GO TO 65
            END IF
            J1 = J - ISPLIT  
            ENDTXT(J1:J1) = MESSAGE(J:J)
60       CONTINUE
65       CONTINUE
         J1 = J1 + 1
         ENDTXT (J1:J1) = '^'
         CALL GETDEASE(DEVERS)
C--- GET DATAEASE VERSION NUMBER FOR F4/ESC KEY
         IF (DEVERS .EQ. '4.0') THEN
            CALL GETMSG(607,MESSAGE)
         ELSE
            CALL GETMSG(606,MESSAGE)
         ENDIF
         CALL GETMSG(999,MESSAGE)
         DO 70 J = 80,1,-1
            IF (MESSAGE(J:J).NE.' ') THEN
               GO TO 75
            END IF
70       CONTINUE
75       CONTINUE
         LENMSG = J
      END IF
C
C  SET POINTER TO CURRENT LINE NUMBER IN THE TEXT ARRAY
C
      ISTRT = (IPAGE - 1) * 17 + 1
      IEND = ISTRT + 16
      IF (ISTRT.GT.NUMLINE) THEN
         CALL BEEP
         RETURN
      ELSE IF (IEND.GT.NUMLINE) THEN
         IEND = NUMLINE
      END IF
      IPAGE = IPAGE + 1
C
C  DISPLAY THE CURRENT PAGE 
C
      I1 = 0
      CALL CLRWIN(IWIN)
      DO 200 I = ISTRT,IEND
         I1 = I1 + 1
         IROW = STRTROW + I1 
         CALL DSPREC(IROW,STRTCOL+2,0,TXTLINE(I),40)
200   CONTINUE
      KROW=STRTROW+1
      KCOL=STRTCOL+2
      CHOICE=ISTRT
      CALL DSPREC(KROW,KCOL,1,TXTLINE(CHOICE),40)
C
C  TELL USER IF THERE IS MORE OR THIS IS THE END OF THE TEXT
C
      IF (IEND.EQ.NUMLINE) THEN
         CALL LOCATE(IROW+1,STRTCOL+2,IERR)
         CALL DSPSTR(ENDTXT,40,IFG,IBG,IBG2)
      ELSE
         CALL LOCATE(STRTROW+18,STRTCOL+2,IERR)
         CALL DSPSTR(MORTXT,40,IFG,IBG,IBG2)
      END IF
      CALL LOCATE(STRTROW+19,STRTCOL+2,IERR)
      CALL DSPSTR(MESSAGE,LENMSG,IFG,IBG,IBG2)
      MAXROW=IROW
      MINROW=STRTROW
      RETURN
      END 
