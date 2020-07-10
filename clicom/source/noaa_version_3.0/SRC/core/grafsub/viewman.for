$STORAGE:2
      SUBROUTINE VIEWMAN(VPNDLF,VPNDRT,VPNDBT,VPNDTP,GANWLF,GANWRT,
     +                   GANWBT,GANWTP,PALETTE,PALDEF,KLRBKGN)
C------------------------------------------------------------------------------
C     ALTER THE SIZE AND POSITION OF THE VIEWPORT OR THE PLOT AREA WITHIN 
C     THAT VIEWPORT.  THE BACKGROUND COLOR OF THE DISPLAY CAN ALSO BE CHANGED. 
C
C     INPUT ARGUMENTS:
C
C     VPND..   REAL       CURRENT VIEWPORT  (NORMALIZED DEVICE COORDINATES)
C     GANW..   REAL       CURRENT PLOT AREA (NORMALIZED WORLD COORDINATES)
C     PALETTE  INT2       CURRENT PALETTE NUMBER
C     PALDEF   INT2 ARRAY CURRENT DEFINITION OF ALL 12 POSSIBLE PALETTES
C     KLRBKGN  INT2       CURRENT COLOR INDEX NUMBER FOR THE BACKGROUND COLOR
C
C     OUTPUT ARGUMENTS:
C
C     VPND..   REAL       REVISED VIEWPORT  
C     GANW..   REAL       REVISED PLOT AREA 
C     KLRBKGN  INT2       REVISED COLOR INDEX NUMBER FOR THE BACKGROUND COLOR
C------------------------------------------------------------------------------
C
C       ** LOCAL COMMON TO SAVE SPACE IN D-GROUP
C

      INTEGER*2   PALETTE,PALDEF(16,12),KLRBKGN,BOXCLR
      REAL XOLD,YOLD,XPREV,YPREV,XPOS,YPOS,XDIF,YDIF,WLF,WTP,WRT,WBT
      REAL         XX, YX, XN, YN
      INTEGER*2   IXX,IYX,IXN,IYN
      CHARACTER*1  RTNCODE
      CHARACTER*2  INCHAR
      COMMON /VIEWSV/  XOLD,YOLD,XPREV,YPREV,XPOS,YPOS,XDIF,YDIF,
     +                 WLF,WTP,WRT,WBT,XX,YX,XN,YN,IXX,IYX,IXN,IYN,
     +                 INCHAR
      RTNCODE = '0'
C     
C   DETERMINE AND SAVE THE CURRENT VIEWPORT, COORDINATES,CROSS-HAIR
C   CURSOR LOCATION AND XOR MODE
C
      CALL INQVIE(XN1OLD,YN1OLD,XN2OLD,YN2OLD)
      CALL INQWOR(X1SAV,Y1SAV,X2SAV,Y2SAV)
      CALL INQHCU(XOLD,YOLD,IDUMMY)
      CALL INQXOR(IXOR)
C
      CALL SETXOR(0)
      IF (KLRBKGN .EQ. 0 .OR. KLRBKGN .EQ. 1) THEN
         BOXCLR = 3
      ELSE
         BOXCLR = 1
      ENDIF
      XN1 = VPNDLF
      XN2 = VPNDRT
      YN1 = VPNDTP
      YN2 = VPNDBT
C---  SET CURRENT VIEWPORT -----
      CALL SETVIE(XN1,YN1,XN2,YN2,-1,KLRBKGN)
      CALL SETWOR(0.,0.,1.,1.) 
      CALL INQWOR(XLOW,YLOW,XHIGH,YHIGH)
      CALL NW2W(GANWLF,GANWBT,WLF,WBT)
      CALL NW2W(GANWRT,GANWTP,WRT,WTP)
C
C   SET VERTICAL SENSITIVITY VALUES FOR CURSOR AND MOUSE
C   (REMEMBER CURSOR MOVEMENT IS ALWAYS 1/200TH OF THE WORLD).
C
      YSTEP    = (YHIGH - YLOW) / 200.
      XSTEP    = (XHIGH - XLOW) / 200.
      VIEWSTEP = 0.01
      XBHIGH = XHIGH - XSTEP
      YBHIGH = YHIGH - YSTEP
      IF (KLRBKGN .EQ. 0) THEN
         CALL SETCOL(1)
         CALL BOX(XLOW,YLOW,XBHIGH,YBHIGH)
      ENDIF
  10  CALL SETCOL(BOXCLR)
      CALL BOX(WLF,WTP,WRT,WBT)
      CALL DELHCU ()
C
C     MENU TO SELECT WHETHER TO MODIFY THE VIEWPORT OR THE PLOT AREA
C
      CALL GRAFMNU(1,4,.1,.8,4,INCHAR)
      IF (INCHAR .EQ. 'ES'.OR. INCHAR .EQ. '4F') THEN
         CALL SETCOL(0)
         CALL CLR
         CALL SETXOR(IXOR)
         CALL MOVHCA(XOLD,YOLD)
         CALL SETVIE(XN1OLD,YN1OLD,XN2OLD,YN2OLD,-1,-1)
         CALL SETWOR(X1SAV,Y1SAV,X2SAV,Y2SAV)
         RETURN
      ENDIF
      IF (INCHAR .EQ. '1 ') THEN 
C
C     ** THE ABILITY TO MODIFY THE VIEWPORT HAS BEEN REMOVED FOR CLICOM 3.0
C        PRINT A MESSAGE AND RETURN TO THE MENU (8-26-91)
C
         XWIN=.1
         YWIN=.95
         MSGN1=444
         MSGN2=202
         CALL GRAFNOTE(XWIN,YWIN,MSGN1,MSGN2,' ',0,INCHAR)
         GO TO 10
C
C     ** START SECTION OF CODE TO MODIFY VIEWPORT
C     MODIFY THE VIEWPORT
C
         YTSAV = YN1
         YBSAV = YN2
         XLSAV = XN1
         XRSAV = XN2
  100    CONTINUE
         CALL SETCOL(0)
         CALL CLR
         CALL SETVIE(XN1,YN1,XN2,YN2,-1,KLRBKGN)
         IF (KLRBKGN .EQ. 0) THEN
            CALL SETCOL(1)
            CALL BOX(XLOW,YLOW,XBHIGH,YBHIGH)
         ENDIF
         CALL SETCOL(BOXCLR)
         CALL SETWOR(XLOW,YLOW,XHIGH,YHIGH)
         CALL BOX(WLF,WTP,WRT,WBT)
         CALL GRAFMNU(1,9,.1,.8,9,INCHAR)
         IF (INCHAR.EQ.'ES' .OR. INCHAR.EQ.'4F') THEN
            CALL SETCOL(KLRBKGN)
            CALL CLR
            IF (KLRBKGN .EQ. 0) THEN
               CALL SETCOL(1)
               CALL BOX(XLOW,YLOW,XBHIGH,YBHIGH)
            ENDIF
            GO TO 10
         ENDIF
C
C     MODIFY THE VERTICAL SIZE OF THE VIEWPORT
C
         IF (INCHAR .EQ. '1 ') THEN
 110        CALL GETCHAR(0,INCHAR)
            IF (INCHAR .EQ. '4F' .OR. INCHAR .EQ. 'ES') THEN
C--------      RESET TO ORIGINAL VERTICAL VALUES           ----
               YN1 = VPNDTP
               YN2 = VPNDBT
               GO TO 100
            ELSE IF(INCHAR .EQ. 'RE') THEN
C--------        PUT REVISED VERTICAL VALUES IN COMMON BLOCK VARIABLES ----
                 VPNDTP = YN1
                 VPNDBT = YN2
                 GO TO 100
            ELSE IF(INCHAR .EQ. 'UA') THEN
C--------        INCREASE VERTICAL SIZE OF THE VIEWPORT    ----
                 YN2 = YN2 + VIEWSTEP
                 IF (YN2 .GT. 1.0) THEN
                    YN1 = YN1 - VIEWSTEP
                    YN2 = 1.0
                    IF (YN1 .LT. 0.0) THEN
                       YN1 = 0.0
                       CALL BEEP
                    ENDIF
                 ELSE
                    YN1 = YN1 - VIEWSTEP
                    IF (YN1 .LT. 0.0) THEN
                       YN2 = YN2 + VIEWSTEP
                       YN1 = 0.0
                       IF (YN2 .GT. 1.0) THEN
                          YN2 = 1.0
                          CALL BEEP
                       ENDIF
                    ENDIF
                 ENDIF
                 CALL CLR
                 CALL SETVIE(XN1,YN1,XN2,YN2,-1,KLRBKGN)
                 CALL SETWOR(XLOW,YLOW,XHIGH,YHIGH)
                 IF (KLRBKGN .EQ. 0) THEN
                    CALL SETCOL(1)
                    CALL BOX(XLOW,YLOW,XBHIGH,YBHIGH)
                 ENDIF
                 CALL SETCOL(BOXCLR)
                 CALL BOX(WLF,WTP,WRT,WBT)
            ELSE IF(INCHAR .EQ. 'DA') THEN
C--------        DECREASE VERTICAL SIZE OF THE VIEWPORT    ----
                 YN2 = YN2 - VIEWSTEP
                 YN1 = YN1 + VIEWSTEP
                 IF (YN2 .LE. YN1 .OR. YN1 .GE. YN2) THEN
                    CALL BEEP
                 ELSE
                    CALL SETCOL(0)
                    CALL CLR
                    CALL SETVIE(XN1,YN1,XN2,YN2,-1,KLRBKGN)
                    CALL SETWOR(XLOW,YLOW,XHIGH,YHIGH)
                    IF (KLRBKGN .EQ. 0) THEN
                       CALL SETCOL(1)
                       CALL BOX(XLOW,YLOW,XBHIGH,YBHIGH)
                    ENDIF
                    CALL SETCOL(BOXCLR)
                    CALL BOX(WLF,WTP,WRT,WBT)
                 ENDIF
            ELSE
                 CALL BEEP
            ENDIF
            GO TO 110
C
C     MODIFY THE HORIZONTAL SIZE OF THE VIEWPORT
C
         ELSE IF (INCHAR .EQ. '2 ') THEN
 150        CALL GETCHAR(0,INCHAR)
            IF (INCHAR .EQ. '4F' .OR. INCHAR .EQ. 'ES') THEN
C--------      RESET TO ORIGINAL HORIZONTAL VALUES           ----
               XN1 = VPNDLF
               XN2 = VPNDRT
               GO TO 100
            ELSE IF(INCHAR .EQ. 'RE') THEN
C--------        PUT REVISED HORIZONTAL VALUES IN COMMON BLOCK VARIABLES ----
                 VPNDLF = XN1
                 VPNDRT = XN2
                 GO TO 100
            ELSE IF(INCHAR .EQ. 'UA') THEN
C--------        INCREASE HORIZONTAL SIZE OF THE VIEWPORT    ----
                 XN2 = XN2 + VIEWSTEP
                 IF (XN2 .GT. 1.0) THEN
                    XN1 = XN1 - VIEWSTEP
                    XN2 = 1.0
                    IF (XN1 .LT. 0.0) THEN
                       XN1 = 0.0
                       CALL BEEP
                    ENDIF
                 ELSE
                    XN1 = XN1 - VIEWSTEP
                    IF (XN1 .LT. 0.0) THEN
                       XN2 = XN2 + VIEWSTEP
                       XN1 = 0.0
                       IF (XN2 .GT. 1.0) THEN
                          XN2 = 1.0
                          CALL BEEP
                       ENDIF
                    ENDIF
                 ENDIF
                 CALL CLR
                 CALL SETVIE(XN1,YN1,XN2,YN2,-1,KLRBKGN)
                 CALL SETWOR(XLOW,YLOW,XHIGH,YHIGH)
                 IF (KLRBKGN .EQ. 0) THEN
                    CALL SETCOL(1)
                    CALL BOX(XLOW,YLOW,XBHIGH,YBHIGH)
                 ENDIF
                 CALL SETCOL(BOXCLR)
                 CALL BOX(WLF,WTP,WRT,WBT)
            ELSE IF(INCHAR .EQ. 'DA') THEN
C--------        DECREASE HORIZONTAL SIZE OF THE VIEWPORT    ----
                 XN2 = XN2 - VIEWSTEP
                 XN1 = XN1 + VIEWSTEP
                 IF (XN2 .LE. XN1 .OR. XN1 .GE. XN2) THEN
                    CALL BEEP
                 ELSE
                    CALL SETCOL(0)
                    CALL CLR
                    CALL SETVIE(XN1,YN1,XN2,YN2,-1,KLRBKGN)
                    CALL SETWOR(XLOW,YLOW,XHIGH,YHIGH)
                    IF (KLRBKGN .EQ. 0) THEN
                       CALL SETCOL(1)
                       CALL BOX(XLOW,YLOW,XBHIGH,YBHIGH)
                    ENDIF
                    CALL SETCOL(BOXCLR)
                    CALL BOX(WLF,WTP,WRT,WBT)
                 ENDIF
            ELSE
                 CALL BEEP
            ENDIF
            GO TO 150
            CONTINUE
         ELSE
C
C   READ LOCATOR POSITION AND EVALUATE CHARACTERS RETURNED. MOVE THE VIEWPORT
C   AS REQUESTED. DISPLAY THE PLOT AREA AS A BOX ON A SOLID COLOR BACKGROUND
C
            CALL SETVIE(0.,0.,1.,1.,-1,-1)
            CALL SETWOR(0.,0.,1.,1.)
            CALL MAPNTW(XN1,YN1,XLF,YTP)
            CALL MAPNTW(XN2,YN2,XRT,YBT)
            XPREV = XLF
            YPREV = YBT
            XPOS  = XLF
            YPOS  = YBT
            CALL SETCOL(KLRBKGN)
            CALL BAR(XLF,YBT,XRT,YTP)
            IF (KLRBKGN .EQ. 0) THEN
               CALL SETCOL(1)
               CALL BOX(XLF,YBT,XRT,YTP)
            ENDIF
            XL = GANWLF * (XRT-XLF) + XLF
            XR = GANWRT * (XRT-XLF) + XLF
            YT = GANWTP * (YTP-YBT) + YBT
            YB = GANWBT * (YTP-YBT) + YBT
            CALL SETCOL(BOXCLR)
            CALL BOX(XL,YB,XR,YT)
            CALL ORGLOC(XPOS,YPOS)
 300        CONTINUE
            CALL RDLOC(XPOS,YPOS,INCHAR,RTNCODE)
C
C   IF F4 OR ESC HAS BEEN PRESSED, IGNORE THE MOVE AND RETURN
C
            IF (INCHAR .EQ. '4F' .OR. INCHAR .EQ. 'ES') THEN
               CALL SETWOR(XLOW,YLOW,XHIGH,YHIGH)
               GO TO 100
C
C   SAVE RESULTS OF THE NEWLY POSITIONED BOX IN COMMON BLOCK VARIABLES
C
            ELSE IF(INCHAR .EQ. 'RE') THEN
                 CALL MAPWTN(XPOS,YTP+YDIF,VPNDLF,VPNDTP)
                 CALL MAPWTN(XRT+XDIF,YPOS,VPNDRT,VPNDBT)
                 XN1 = VPNDLF
                 YN1 = VPNDTP
                 XN2 = VPNDRT
                 YN2 = VPNDBT
                 CALL SETWOR(XLOW,YLOW,XHIGH,YHIGH)
                 GO TO 100
            ELSE
C
C   POSITION HAS CHANGED - REDRAW VIEWPORT IN NEW LOCATION
C
               IF (YPOS.NE.YPREV .OR. XPOS.NE.XPREV) THEN
                  YDIF =  YPOS - YBT
                  XDIF =  XPOS - XLF
                  IF (XPOS .LT. 0.0) THEN
                     CALL BEEP
                  ELSE IF (YPOS .LT. 0.0) THEN
                     CALL BEEP
                  ELSE IF ((XRT+XDIF) .GE. 1.0) THEN
                     CALL BEEP
                  ELSE IF ((YTP+YDIF) .GE. 1.0) THEN
                     CALL BEEP
                  ELSE
                     CALL SETCOL(0)
                     CALL CLR
                     CALL SETCOL(KLRBKGN)
                     CALL BAR(XPOS,YPOS,XRT+XDIF,YTP+YDIF)
                     IF (KLRBKGN .EQ. 0) THEN
                        CALL SETCOL(1)
                        CALL BOX(XPOS,YPOS,XRT+XDIF,YTP+YDIF)
                     ENDIF
                     XL = GANWLF * (XRT+XDIF-XPOS) + XPOS
                     XR = GANWRT * (XRT+XDIF-XPOS) + XPOS
                     YT = GANWTP * (YTP+YDIF-YPOS) + YPOS
                     YB = GANWBT * (YTP+YDIF-YPOS) + YPOS
                     CALL SETCOL(BOXCLR)
                     CALL BOX(XL,YT,XR,YB)
                  ENDIF
                  XPREV = XPOS
                  YPREV = YPOS
               ENDIF
            ENDIF
            GO TO 300
         ENDIF
C
C     ** END SECTION OF CODE TO MODIFY VIEWPORT
C
      ELSEIF (INCHAR .EQ. '2 ') THEN
C
C   MODIFY THE PLOT AREA BOX. 
C
  500 CONTINUE
      CALL NW2W(GANWLF,GANWBT,WLF,WBT)
      CALL NW2W(GANWRT,GANWTP,WRT,WTP)
      XPREV = WLF
      YPREV = WTP 
      XPOS  = WLF
      YPOS  = WTP 
      CALL SETCOL(KLRBKGN)
      CALL CLR
      IF (KLRBKGN .EQ. 0) THEN
         CALL SETCOL(1)
         CALL BOX(XLOW,YLOW,XBHIGH,YBHIGH)
      ENDIF
      CALL SETCOL(BOXCLR)
      CALL BOX(WLF,WTP,WRT,WBT)
C
C     FIND THE ACTUAL WORLD COORDS FOR THE CURRENT VIEWPORT AS SET BY HALO
C     MAPPING ROUTINES. THE MAX VALUE RETURNED BY AN INQWOR IS THE VALUE USED
C     IN THE SETWORLD CALL. ROUND-OFF CAN LEAD TO A DIFFERENT VALUE  9-AP-91
C
      CALL MAPNTD(XN1,YN1,IXN,IYX)
      CALL MAPDTW(IXN,IYX,XN,YX)
      CALL MAPNTD(XN2,YN2,IXX,IYN)
      CALL MAPDTW(IXX,IYN,XX,YN)
      CALL DELHCU( )
      CALL GRAFMNU(1,9,.1,.8,9,INCHAR)
      IF (INCHAR.EQ.'ES' .OR. INCHAR.EQ.'4F') THEN
         CALL DELBOX
         CALL SETCOL(KLRBKGN)
         CALL CLR
         IF (KLRBKGN .EQ. 0) THEN
            CALL SETCOL(1)
            CALL BOX(XLOW,YLOW,XBHIGH,YBHIGH)
         ENDIF
         GO TO 10
      ENDIF
      CALL SETCOL(KLRBKGN)
      CALL CLR
      IF (KLRBKGN .EQ. 0) THEN
         CALL SETCOL(1)
         CALL BOX(XLOW,YLOW,XBHIGH,YBHIGH)
      ENDIF
      CALL SETCOL(BOXCLR)
      CALL RBOX(WLF,WTP,WRT,WBT)
      IF (INCHAR .EQ. '1 ') THEN
 510     CALL GETCHAR(0,INCHAR)
         IF (INCHAR .EQ. '4F' .OR. INCHAR .EQ. 'ES') THEN
C--------   RESET TO ORIGINAL VERTICAL VALUES           ----
            GO TO 500
         ELSE IF(INCHAR .EQ. 'RE') THEN
C--------     PUT REVISED VERTICAL VALUES IN COMMON BLOCK VARIABLES ----
              CALL W2NW(WLF,WBT,GANWLF,GANWBT)
              CALL W2NW(WRT,WTP,GANWRT,GANWTP)
              GO TO 500
         ELSE IF(INCHAR .EQ. 'UA') THEN
C--------     INCREASE VERTICAL SIZE OF THE PLOT AREA    ----
              WTP = WTP + YSTEP
              IF (WTP .GE. YHIGH) THEN
                 WTP = WTP - YSTEP
                 WBT = WBT - YSTEP
                 IF (WBT .LE. YLOW) THEN
                    WBT = WBT + YSTEP
                    CALL BEEP
                 ENDIF
              ELSE
                 WBT = WBT - YSTEP
                 IF (WBT .LE. YLOW) THEN
                    WBT = WBT + YSTEP
                    WTP = WTP + YSTEP
                    IF (WTP .GE. YHIGH) THEN
                       WTP = WTP - YSTEP
                       CALL BEEP
                    ENDIF
                 ENDIF
              ENDIF
              CALL RBOX(WLF,WTP,WRT,WBT)
         ELSE IF(INCHAR .EQ. 'DA') THEN
C--------     DECREASE VERTICAL SIZE OF THE PLOT AREA    ----
              WTP = WTP - YSTEP
              WBT = WBT + YSTEP
              IF (WTP .LE. WBT .OR. WBT .GE. WTP) THEN
                 CALL BEEP
              ELSE
                 CALL RBOX(WLF,WTP,WRT,WBT)
              ENDIF
         ELSE
              CALL BEEP
         ENDIF
         GO TO 510
      ELSE IF (INCHAR .EQ. '2 ') THEN
 550     CALL GETCHAR(0,INCHAR)
         IF (INCHAR .EQ. '4F' .OR. INCHAR .EQ. 'ES') THEN
C--------   RESET TO ORIGINAL HORIZONTAL VALUES           ----
            GO TO 500
         ELSE IF(INCHAR .EQ. 'RE') THEN
C--------     PUT REVISED HORIZONTAL VALUES IN COMMON BLOCK VARIABLES ----
              CALL W2NW(WLF,WBT,GANWLF,GANWBT)
              CALL W2NW(WRT,WTP,GANWRT,GANWTP)
              GO TO 500
         ELSE IF(INCHAR .EQ. 'UA') THEN
C--------     INCREASE HORIZONTAL SIZE OF THE PLOT AREA    ----
              WRT = WRT + XSTEP
              IF (WRT .GT. XHIGH) THEN
                 WLF = WLF - XSTEP
                 WRT = WRT - XSTEP
                 IF (WLF .LT. XLOW) THEN
                    WLF = WLF + XSTEP
                    CALL BEEP
                 ENDIF
              ELSE
                 WLF = WLF - XSTEP
                 IF (WLF .LT. XLOW) THEN
                    WRT = WRT + XSTEP
                    WLF = WLF + XSTEP
                    IF (WRT .GT. XHIGH) THEN
                       WRT = WRT - XSTEP
                       CALL BEEP
                    ENDIF
                 ENDIF
              ENDIF
              CALL RBOX(WLF,WTP,WRT,WBT)
         ELSE IF(INCHAR .EQ. 'DA') THEN
C--------     DECREASE HORIZONTAL SIZE OF THE PLOT AREA    ----
              WRT = WRT - XSTEP
              WLF = WLF + XSTEP
              IF (WRT .LE. WLF .OR. WLF .GE. WRT) THEN
                 CALL BEEP
              ELSE
                 CALL RBOX(WLF,WTP,WRT,WBT)
              ENDIF
         ELSE
              CALL BEEP
         ENDIF
         GO TO 550
         CONTINUE
      ELSE
C
C   READ LOCATOR POSITION AND EVALUATE CHARACTERS RETURNED.
C   MOVE BOX AS REQUESTED.
C
         CALL ORGLOC(XPOS,YPOS)
 600     CONTINUE
         CALL RDLOC(XPOS,YPOS,INCHAR,RTNCODE)
C
C   IF F4 OR ESC HAS BEEN PRESSED, IGNORE THE MOVE AND RETURN
C
         IF (INCHAR .EQ. '4F' .OR. INCHAR .EQ. 'ES') THEN
            GO TO 500
C
C   SAVE RESULTS OF THE NEWLY POSITIONED BOX IN COMMON BLOCK VARIABLES
C
         ELSE IF(INCHAR .EQ. 'RE') THEN
              IF (WLF .LE. XLOW) THEN
                  WLF = XLOW
              ELSE IF (WTP .GE. YHIGH) THEN
                  WTP = YHIGH
              ELSE IF (WRT .GE. XHIGH) THEN
                  WRT = XHIGH
              ELSE IF (WBT .LE. YLOW) THEN
                  WBT = YLOW
              ENDIF
              CALL W2NW(WLF,WBT,GANWLF,GANWBT)
              CALL W2NW(WRT,WTP,GANWRT,GANWTP)
              GO TO 500
         ELSE
C
C   POSITION HAS CHANGED - REDRAW BOX IN NEW LOCATION
C
            IF (YPOS.NE.YPREV .OR. XPOS.NE.XPREV) THEN
               WRT = WRT - (WLF - XPOS)
               WBT = WBT - (WTP - YPOS)
               WLF = XPOS
               WTP = YPOS
               IF (WLF.LE.XN .OR. WBT.LE.YN .OR. 
     +             WRT.GE.XX .OR. WTP.GE.YX) THEN
                   WRT = WRT + (XPREV - XPOS)
                   WBT = WBT + (YPREV - YPOS)
                   WLF = XPREV
                   WTP = YPREV
                   IF (RTNCODE .EQ. '1') THEN
                      CALL ORGLOC(WLF,WTP)
                   ELSE
                      XPOS = WLF
                      YPOS = WTP
                   ENDIF
                   CALL BEEP
               ELSE
                   CALL RBOX(WLF,WTP,WRT,WBT)
                   XPREV = XPOS
                   YPREV = YPOS
               ENDIF
            ENDIF
         ENDIF
         GO TO 600
      ENDIF
      ELSE
C
C---  MODIFY BACKGROUND COLOR
C
         INDX = KLRBKGN
         CALL PICKCOL(INDX,.8,.8,0,16,PALETTE,PALDEF,2)
         IF (INDX .GE. 0) THEN
            KLRBKGN = INDX
         ENDIF
         CALL SETCOL(KLRBKGN)
         CALL CLR
         IF (KLRBKGN .EQ. 0) THEN
            CALL SETCOL(1)
            CALL BOX(XLOW,YLOW,XBHIGH,YBHIGH)
         ENDIF
         IF (KLRBKGN .EQ. 0 .OR. KLRBKGN .EQ. 1) THEN
            BOXCLR = 3
         ELSE
            BOXCLR = 1
         ENDIF
         GO TO 10
      ENDIF
      RETURN
      END
