$STORAGE:2
      SUBROUTINE PRNTOP 
C------------------------------------------------------------------------------
C     REVISE THE OPTIONS FOR THE PRINTER (PRIMARY OR ALTERNATE WHICHEVER IS
C     CURRENTLY SELECTED) DEFINED BY THE ADMIN PGM.  THIS ROUTINE OPERATES 
C     OUTSIDE OF GRAPHICS MODE TO TAKE ADVANTAGE OF CLICOM'S FORM PGM.  THE
C     FUNCTIONS SUPPORTED BY HALO FOR THIS PRINTER (3 DIFFERENT FORMS EXIST) 
C     ARE DISPLAYED FOR THE USER TO FILL IN.  EACH FIELD IN THE FORM IS CHECKED
C     AND THE SCREEN IS REDRAWN WITH THE CURRENT GRAPH.
C------------------------------------------------------------------------------
$INCLUDE: 'HALOENV.INC'
      CHARACTER*1  YN, RTNCODE
      CHARACTER*2  YESUP, YESLO, INCHAR
      CHARACTER*8  FORMID
      CHARACTER*10 PRFLDS(12), PRIMRY, ALTRNT
      CHARACTER*64 HELPFILE
      CHARACTER*78 MSGLIN(6)
      INTEGER      PTRINQ(60), PTRSUP(64), WIDE, HLPLVL, WIDTH, HGT,
     +             FORMFD, DPI, COPIES, CENTER, XOFSET, YOFSET, RESOL,
     +             DITHER
      REAL         LOFSET, LTOPST
      LOGICAL      LASER, FIRSTCALL, ENGLUNIT
C
C       ** LOCAL COMMON TO SAVE SPACE IN DGROUP
      COMMON /PROPSV/ PTRINQ, PTRSUP, WIDE, HLPLVL, WIDTH, HGT, FORMFD,
     +                DPI, COPIES, CENTER, XOFSET, YOFSET, RESOL,DITHER,
     +                PRFLDS, PRIMRY, ALTRNT, FORMID, HELPFILE, MSGLIN,
     +                YESUP, YESLO, INCHAR
      DATA FIRSTCALL /.TRUE./
C
C---- SET TEMPORARY VARIABLES TO CURRENT VALUES FOR THE OPTIONS OF ACTIVE 
C---- PRINTER. ACTVPR IS THE PRINTER DESIGNATION, 0 = PRIMARY & 1 = ALTERNATE 
C
      IF (FIRSTCALL) THEN
         FIRSTCALL = .FALSE.
         CALL GETYN(1,2,YESUP,YESLO)
C ----   GET PRIMARY & ALTERNATE TITLES FROM MESSAGE FILE
         CALL GETMSG(468,HELPFILE)
         PRFLDS(1) = '          '
         PRFLDS(2) = '          '
         PRIMRY    = '          '
         ALTRNT    = '          '
         CALL PARSE1(HELPFILE,64,2,10,PRFLDS,ISTAT)         
         LEN = LNG(PRFLDS(1))
         PRIMRY(10-LEN+1:10)=PRFLDS(1)
         LEN = LNG(PRFLDS(2))
         ALTRNT(10-LEN+1:10) = PRFLDS(2)
         HELPFILE  = 'P:\HELP\PRNTOP.HLP'
      ENDIF
      IF (ACTVPTR .EQ. 0) THEN
         WIDTH  = PTRVAL(1)
         HGT    = PTRVAL(2)
         DITHER = PTRVAL(5)
         FORMFD = PTRVAL(6)
         DPI    = PTRVAL(8)
         COPIES = PTRVAL(9)
         CENTER = PTRVAL(10)
         XOFSET = PTRVAL(11)
         YOFSET = PTRVAL(12)
         RESOL  = PTRVAL(18)
         USCALE = PTRVAL(19) / 100.
         PRFLDS(1)= PRIMRY
         CALL SETPRN(PRINTR)
      ELSE
         WIDTH  = PRVAL2(1)
         HGT    = PRVAL2(2)
         DITHER = PRVAL2(5)
         FORMFD = PRVAL2(6)
         DPI    = PRVAL2(8)
         COPIES = PRVAL2(9)
         CENTER = PRVAL2(10)
         XOFSET = PRVAL2(11)
         YOFSET = PRVAL2(12)
         RESOL  = PRVAL2(18)
         USCALE = PRVAL2(19) / 100.
         PRFLDS(1)=ALTRNT
         CALL SETPRN(PRNTR2)
      ENDIF
      FORMID   = 'HALOPRN1'
      IF (USCALE .EQ. 1.00) THEN
         ENGLUNIT = .TRUE.
      ELSE
         ENGLUNIT = .FALSE.
      ENDIF            
C
C---- SELECT THE PROPER FORM TO DISPLAY DEPENDING UPON WHETHER THE
C---  PRINTER SUPPORTS DIFFERENT RESOLUTIONS & PAPER WIDTHS 
C
      CALL INQPRN(PTRINQ)
      CALL INQPAT(PTRSUP)
      HLPLVL = 1
      IF (DPI .EQ. -1) THEN
         MODE = PTRINQ(3)
      ELSE
         MODE = DPI
      ENDIF
      MM   = (MODE*5) + 4
      WIDE  = 0
      IF (PTRINQ(MM+3) .EQ. -1) THEN
         LASER = .FALSE.
      ELSE
         LASER = .TRUE.
      ENDIF
      IF (PTRSUP(8) .EQ. 1) THEN
         FORMID = 'HALOPRN2'
         IF (PTRINQ(8).EQ.PTRINQ(13) .AND. PTRINQ(9).EQ.PTRINQ(14)) THEN
            WIDE  = 1
            FORMID = 'HALOPRN3'
            REGSIZ = FLOAT(PTRINQ(6))  / FLOAT(PTRINQ(8)) * USCALE
            WIDSIZ = FLOAT(PTRINQ(11)) / FLOAT(PTRINQ(13)) * USCALE
         ENDIF
      ENDIF
      MAXX = PTRINQ(MM+2)
      MAXY = PTRINQ(MM+3)
      XDPI = PTRINQ(MM+4)
      YDPI = PTRINQ(MM+5)
C
C---  SET FIELDS IN THE PRINTER CONFIGURATION FORM TO THE CURRENT VALUES
C
      PAPRSZ = (FLOAT(MAXX) / XDPI) * USCALE
      IF (WIDE .EQ. 0) THEN
         IF (ENGLUNIT) THEN
            WRITE(UNIT=PRFLDS(2),FMT=20) PAPRSZ
         ELSE
            WRITE(UNIT=PRFLDS(2),FMT=21) PAPRSZ
         ENDIF
      ELSE
         IF (ENGLUNIT) THEN
            WRITE(UNIT=PRFLDS(2),FMT=20)  REGSIZ
            WRITE(UNIT=PRFLDS(12),FMT=20) WIDSIZ
         ELSE
            WRITE(UNIT=PRFLDS(2),FMT=21)  REGSIZ
            WRITE(UNIT=PRFLDS(12),FMT=21) WIDSIZ
         ENDIF
      ENDIF
      IF (COPIES .GT. 1) THEN
         WRITE(UNIT=PRFLDS(3),FMT=30) COPIES
      ELSE
         PRFLDS(3) = '1 '
      ENDIF
      IF (PTRSUP(6) .EQ. 1) THEN
         IF (FORMFD .LT. 1) THEN
            PRFLDS(4) = '1 '
         ELSE
            PRFLDS(4) = '2 '
         ENDIF
      ELSE
         PRFLDS(4) = '9 '
      ENDIF
      IF (PTRSUP(10) .EQ. 1) THEN
         IF (CENTER .LT. 1) THEN
            PRFLDS(5) = '1 '
         ELSE
            PRFLDS(5) = '2 '
         ENDIF
      ELSE
         PRFLDS(5) = '9 '
      ENDIF
      IF (PTRSUP(11) .EQ. 1) THEN
         IF (XOFSET .GE. 0) THEN
            LOFSET = (FLOAT(XOFSET) / XDPI) * USCALE
            WRITE(UNIT=PRFLDS(6),FMT=14) LOFSET
         ELSE
            PRFLDS(6) = '   '
         ENDIF
      ELSE
         PRFLDS(6) = '9.9'
      ENDIF
      IF (PTRSUP(12) .EQ. 1) THEN
         IF (YOFSET .GE. 0) THEN
            LTOPST = (FLOAT(YOFSET) / YDPI) * USCALE
            WRITE(UNIT=PRFLDS(7),FMT=14) LTOPST
         ELSE
            PRFLDS(7) = '   '
         ENDIF
      ELSE
         PRFLDS(7) = '9.9'
      ENDIF
      IF (PTRSUP(5) .EQ. 1) THEN
         IF (DITHER .LT. 1) THEN
            PRFLDS(8) = '1 '
         ELSE
            PRFLDS(8) = '2 '
         ENDIF
      ELSE
         PRFLDS(8) = '9 '
      ENDIF
      IF (PTRSUP(1) .EQ. 1) THEN
         IF (WIDTH .GE. 0) THEN
            CM = (FLOAT(WIDTH) / XDPI) * USCALE
            WRITE(UNIT=PRFLDS(9),FMT=15) CM
         ELSE
            PRFLDS(9) = '    '
         ENDIF
      ELSE
         WRITE(UNIT=PRFLDS(9),FMT=15) PAPRSZ
      ENDIF
      IF (RESOL .LE. 0) THEN
         PRFLDS(10) = '  '
      ELSE
         WRITE(UNIT=PRFLDS(10),FMT=30) RESOL
      ENDIF
      IF (ENGLUNIT) THEN
         IF (PAPRSZ .LT. 8.5) THEN
            PRFLDS(11) = '1 '
         ELSE
            PRFLDS(11) = '2 '
         ENDIF
      ELSE
         IF (PAPRSZ .LT. 21.0) THEN
            PRFLDS(11) = '1 '
         ELSE
            PRFLDS(11) = '2 '
         ENDIF
      ENDIF
   90 NUMER = 0
  100 INCHAR='  '
      ISZ   = PAPRSZ
      CALL CLS
      CALL LOCATE(1,1,IERR)
      CALL GETFRM(FORMID,HELPFILE,PRFLDS,10,INCHAR)
      CALL LOCATE(1,1,IERR)
C
C---  ASK USER TO ABANDON FORM IF STILL ERRORS ON IT. OTHERWISE, RESET FORM
C
      IF (INCHAR .EQ. '4F' .OR. INCHAR .EQ. 'ES') THEN
         IF (NUMER .GT. 0) THEN
            YN = ' '
            CALL WRTMSG(3,200,14,1,0,' ',0)
            CALL LOCATE(22,55,IERR)
            CALL OKREPLY(YN,RTNCODE)
            IF (YN .EQ. YESUP .OR. YN .EQ. YESLO) THEN
               RETURN
            ELSE
               GO TO 90
            ENDIF
         ELSE
            RETURN
         ENDIF
      ENDIF
      NUMER = 0
C
C-----  EXAMINE EACH OF THE 9 PRINTER OPTIONS ENTERED BY THE USER
C
C-----      NBR OF COPIES, 1=1 COPY(DEF), N>1= NBR COPIES
C
      IF (PTRSUP(9) .EQ. 1) THEN
         IVAL = ICHAR(PRFLDS(3)(1:1)) - 48
         COPIES = IVAL 
      ELSE
         IF (PRFLDS(3) .NE. '1 ') THEN
C           ERROR MSG--HALO DOES NOT SUPPORT THIS OPTION FOR THIS PRINTER
            NUMER = NUMER + 1
            IF (NUMER .LT. 4) THEN
               CALL GETMSG(471,MSGLIN(NUMER))
               PRFLDS(3) = '1 '
            ENDIF
         ENDIF
      ENDIF
C
C-----      FORM FEED -- 1=YES(DEF), 2=NO
C
      IF (PTRSUP(6) .EQ. 1) THEN
         IF (PRFLDS(4) .EQ. '2 ') THEN
            FORMFD = 1
         ELSE
            FORMFD = -1
         ENDIF
      ELSE
         IF (PRFLDS(4) .NE. '9 ') THEN
C           ERROR MSG--HALO DOES NOT SUPPORT THIS OPTION FOR THIS PRINTER
            NUMER = NUMER + 1
            IF (NUMER .LT. 4) THEN
               CALL GETMSG(472,MSGLIN(NUMER))
               PRFLDS(4) = '9 '
            ENDIF
         ENDIF
      ENDIF
C
C-----      CENTERING -- 1=YES(DEF), 2=NO
C
      IF (PTRSUP(10) .EQ. 1) THEN
         IF (PRFLDS(5) .EQ. '2 ') THEN
            CENTER = 1
         ELSE
            CENTER = -1
         ENDIF
      ELSE
         IF (PRFLDS(5) .NE. '9 ') THEN
C           ERROR MSG--HALO DOES NOT SUPPORT THIS OPTION FOR THIS PRINTER
            NUMER = NUMER + 1
            IF (NUMER .LT. 4) THEN
               CALL GETMSG(473,MSGLIN(NUMER))
               PRFLDS(5) = '9 '
            ENDIF
         ENDIF
      ENDIF
C
C-----      DITHERING -- 1=YES, 2=NO(DEF)
C
      IF (PTRSUP(5) .EQ. 1) THEN
         IF (PRFLDS(8) .EQ. '2 ') THEN
            DITHER = 1
         ELSE
            DITHER = -1
         ENDIF
      ELSE
         IF (PRFLDS(8) .NE. '9 ') THEN
C           ERROR MSG--HALO DOES NOT SUPPORT THIS OPTION FOR THIS PRINTER
            NUMER = NUMER + 1
            IF (NUMER .LT. 4) THEN
               CALL GETMSG(469,MSGLIN(NUMER))
               PRFLDS(8) = '9 '
            ENDIF
         ENDIF
      ENDIF
C
C---- DETERMINE THE NEW PRINTER RESOLUTION MODE NUMBER FROM USER INPUT OF
C---- 1=HIGH,2=MEDIUM,3=LOW, IF SUPPORTED BY HALO.  THIS MODE NUMBER IS
C---- USED IN ALL SUBSEQUENT OPERATIONS.  
C
      IF (FORMID(8:8) .NE. '1') THEN
         IVAL  = ICHAR(PRFLDS(10)(1:1)) - 48
         RESOL = IVAL
         IF (LASER) THEN
            DPI = PTRINQ(4) - IVAL
            NEWMODE   = DPI
         ELSE
            IF (WIDE .EQ. 1) THEN
               NRES = PTRINQ(4)/2
            ELSE
               NRES = PTRINQ(4)
            ENDIF
            IF (IVAL .LE. NRES) THEN
               DPI = PTRINQ(4) - (IVAL * (WIDE+1))
C
C           THE PANASONIC 24-PIN DOT MATRIX DRIVER (USED FOR MOST 24-PIN
C           PRINTERS) HAS 5 PRINTER RESOLUTIONS.  HIGH, MED, AND LOW SHOULD
C           BE MODES 2,3,4 NOT 3,4,5; THEREFORE IF NRES > 4 STATEMENT.
C           ALSO, SET DPI TO THE MODE NUMBER OF WIDE PAPER IF SELECTED
C
               IF (WIDE .EQ. 1 .AND. PRFLDS(11) .EQ. '2 ') THEN
                  DPI = DPI + 1
               ENDIF
               IF (NRES .GT. 4) THEN
                  IF (WIDE .EQ. 1) THEN
                     DPI = DPI - 2
                  ELSE
                     DPI = DPI - 1
                  ENDIF
               ENDIF
               NEWMODE = DPI
            ELSE
C              ERROR MSG--HALO DOES NOT SUPPORT THIS OPTION FOR THIS PRINTER
               NUMER = NUMER + 1
               IF (NUMER .LT. 4) THEN
                  IF (NRES .EQ. 1) THEN
                     CALL GETMSG(477,MSGLIN(NUMER))
                  ELSE
                     CALL GETMSG(480,MSGLIN(NUMER))
                  ENDIF
               ENDIF
               GO TO 400
            ENDIF
         ENDIF
      ELSE
         IF (DPI .EQ. -1) THEN
            NEWMODE   = PTRINQ(3)
            DPI = PTRINQ(3)
         ELSE
            NEWMODE = DPI
         ENDIF
      ENDIF
      MAXX = PTRINQ(4+(NEWMODE*5)+2)
      MAXY = PTRINQ(4+(NEWMODE*5)+3)
      XDPI = PTRINQ(4+(NEWMODE*5)+4)
      YDPI = PTRINQ(4+(NEWMODE*5)+5)
      ISZ  = MAXX / XDPI
C
C-----  ENTER NUMBER OF INCHES (OR CM) TO INDENT FROM LEFT OF PAGE
C
      IF (PTRSUP(11) .EQ. 1) THEN
         READ (UNIT=PRFLDS(6),FMT=141) LOFSET
         VAL = LOFSET / USCALE
         XOFSET = XDPI * VAL
      ELSE
         IF (PRFLDS(6) .NE. '9.9') THEN
C           ERROR MSG--HALO DOES NOT SUPPORT THIS OPTION FOR THIS PRINTER
            NUMER = NUMER + 1
            IF (NUMER .LT. 4) THEN
               CALL GETMSG(474,MSGLIN(NUMER))
               PRFLDS(6) = '9.9'
            ENDIF
         ENDIF
      ENDIF
C
C-----  ENTER NUMBER OF INCHES (OR CM) TO INDENT FROM TOP OF PAGE
C
      IF (PTRSUP(12) .EQ. 1) THEN
         READ (UNIT=PRFLDS(7),FMT=141) LTOPST
         VAL = LTOPST / USCALE
         YOFSET = YDPI * VAL
      ELSE
         IF (PRFLDS(7) .NE. '9.9') THEN
C           ERROR MSG--HALO DOES NOT SUPPORT THIS OPTION FOR THIS PRINTER
            NUMER = NUMER + 1
            IF (NUMER .LT. 4) THEN
               CALL GETMSG(475,MSGLIN(NUMER))
               PRFLDS(7) = '9.9'
            ENDIF
         ENDIF
      ENDIF
C
C-----  ENTER HORIZONTAL SIZE (WIDTH) OF THE PLOT IN INCHES (OR CM)
C
      IF (PTRSUP(1) .EQ. 1) THEN
         READ (UNIT=PRFLDS(9),FMT=151) VAL
         VAL = VAL / USCALE
         WIDTH = XDPI * VAL
         PLOTWID = VAL
C
C---  CALCULATE THE VERTICAL SIZE BASED ON THE HORIZONTAL SIZE AND ASPECT RATIO
C
         PASP = YDPI / XDPI
         HGT  = FLOAT(WIDTH) * PASP
      ELSE
         WIDTH = PTRINQ(4+(NEWMODE*5)+2)
         PASP  = YDPI / XDPI
         HGT   = FLOAT(WIDTH) * PASP
C        ERROR MSG--HALO DOES NOT SUPPORT THIS OPTION FOR THIS PRINTER
         NUMER = NUMER + 1
         IF (NUMER .LT. 4) THEN
            CALL GETMSG(476,MSGLIN(NUMER))
         ENDIF
      ENDIF
  400 CONTINUE
C
C---- VERIFY THAT WIDTH OF PLOT, OFFSETS, & PAGE SIZES ARE CONSISTENT
C---- WHEN CENTERING IS OFF.  NO OFFSETS ALLOWED WHEN CENTERING IS ON
C
      IF ((XOFSET+WIDTH) .GT. MAXX) THEN
         NUMER = NUMER + 1
         IF (NUMER .LT. 4) THEN
            CALL GETMSG(479,MSGLIN(NUMER))
         ENDIF
      ENDIF
      IF (CENTER .EQ. -1) THEN
         IF (XOFSET .GT. 0 .OR. YOFSET .GT. 0) THEN
            NUMER = NUMER + 1
            IF (NUMER .LT. 4) THEN
               CALL GETMSG(478,MSGLIN(NUMER))
            ENDIF
         ENDIF
      ENDIF            
C
C---- DISPLAY ANY ERROR MESSAGES. ASK USER TO REVISE SELECTIONS IF ERRORS
C
      IF (NUMER .GT. 0) THEN
         IF (NUMER .GT. 3) THEN
            NUMER = 4
            CALL GETMSG(481,MSGLIN(NUMER))
         ENDIF
         IROWUP = 24 - (NUMER + 1) 
         IF (FORMID .EQ. 'HALOPRN3') THEN
            IF (NUMER .GE. 3) THEN
               NUMER = 3
               IROWUP=21
            ENDIF
         ENDIF
         CALL LOCATE(IROWUP,1,IERR)
         DO 600 K = 1,NUMER
            CALL WRTSTR(MSGLIN(K),75,4,0)
            CALL LOCATE(IROWUP+K,1,IERR)
  600    CONTINUE
         CALL GETMSG(202,MSGLIN(6))
         CALL WRTSTR(MSGLIN(6),40,14,0)
         CALL GETCHAR(0,INCHAR)
      ELSE
C
C----- TRANSFER PRINTER OPTIONS FROM TEMP TO PERMANENT VARIABLES.  ADJUST
C----- WIDTH OF PLOT IF OFFSET + WIDTH EXCEEDS MAX X DUE TO CM-IN CONVERSION
C
         IF (INCHAR .EQ. '2F') THEN
            IF (ACTVPTR .EQ. 0) THEN
               PTRVAL(1) = WIDTH
               PTRVAL(2) = HGT
               PTRVAL(5) = DITHER
               PTRVAL(6) = FORMFD
               PTRVAL(8) = DPI
               PTRVAL(9) = COPIES
               PTRVAL(10)= CENTER
               PTRVAL(11)= XOFSET
               PTRVAL(12)= YOFSET
               PTRVAL(18)= RESOL
               PTRVAL(20)= PTRINQ(2)
               PTRASP    = PASP
           ELSE
               PRVAL2(1) = WIDTH
               PRVAL2(2) = HGT
               PRVAL2(5) = DITHER
               PRVAL2(6) = FORMFD
               PRVAL2(8) = DPI
               PRVAL2(9) = COPIES
               PRVAL2(10)= CENTER
               PRVAL2(11)= XOFSET
               PRVAL2(12)= YOFSET
               PRVAL2(18)= RESOL
               PRVAL2(20)= PTRINQ(2)
               PRASP2    = PASP
            ENDIF
C
C-----   MAKE PRINTER OPTIONS PERMANENT -- 1=YES, 2=NO(DEF)
C
            YN = ' '
            CALL LOCATE(21,5,IERR)
            CALL GETMSG(470,MSGLIN(6))
            CALL GETMSG(999,MSGLIN(6))
            CALL WRTSTR(MSGLIN(6),40,14,0)
            CALL OKREPLY(YN,RTNCODE)
            CALL LOCATE(22,1,IERR)
            IF (YN .EQ. YESUP .OR. YN .EQ. YESLO) THEN
               OPEN (UNIT=44,FILE=ENVFILE,FORM='FORMATTED')
               WRITE(44,700) DEVICE,DEVMODE,AQCMODE,NETWORK,PRINTR,
     +                       PTRVAL,PTRASP,PRNTR2,PRVAL2,PRASP2,PLOTER
  700          FORMAT('''',A24,'''',','I4,',',I4,',',I4/''''A24''''/
     +         2(I5,','),8(I3,','),2(I4,','),8(I3,','),F5.3/
     +         '''',A24,''''/2(I5,','),8(I3,','),2(I4,','),8(I3,','),
     +         F5.3/'''',A24,'''')
               CLOSE (44)
            ENDIF
            RETURN
         ENDIF
      ENDIF 
      GO TO 100
  14  FORMAT(F3.1)
  141 FORMAT(F3.0)
  15  FORMAT(F4.1)
  151 FORMAT(F4.0)
  20  FORMAT(F4.1,2HIN)
  21  FORMAT(F4.1,2HCM)
  30  FORMAT(I1,1H )
      END
