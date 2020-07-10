$STORAGE:2
      SUBROUTINE CHNGLBL(MENUNBR,TXT,TXCLR,TXFONT,TXSIZE,ASP,PATH,
     +                   TXLOC,PALETTE,PALDEF,LIMCHK)
C------------------------------------------------------------------------------
C     MANAGE THOSE ROUTINES THAT CHANGE THE ATTRIBUTES (COLOR, FONT, SIZE, 
C     AND LOCATION) OF A TEXT STRING (OR MAP MARKER) ON A GRAPH
C     
C     INPUT ARGUMENTS:
C
C     MENUNBR     INT2       NUMBER OF MENU TO DISPLAY FROM GRAPHICS MENU FILE
C     TXT         CHAR       TEXT STRING TO BE CHANGED
C     TXCLR       INT2       CURRENT TEXT COLOR
C     TXFONT      INT2       CURRENT TEXT FONT
C     TXSIZE      REAL       CURRENT TEXT HEIGHT (NWC)
C     ASP         REAL       CURRENT TEXT ASPECT RATIO
C     PATH        INT2       CURRENT TEXT PATH
C     TXLOC       REAL ARRAY POSITION OF TEXT STRING (BOTTOM-CENTER IN 
C                            NORMALIZED WORLD COORDINATES)
C     PALETTE     INT2       CURRENT PALETTE NUMBER
C     PALDEF      INT2 ARRAY CURRENT DEFINITION OF ALL 12 POSSIBLE PALETTES
C     LIMCHK      INT2       FLAG TO CONTROL TEXT POSITION: 
C                            0 = NO CHECKS
C                            1 = LEFT AND RIGHT SIDE EDGE CHECK
C                            2 = TOP AND BOTTOM EDGE CHECK
C                            -X= CHANGE THE TYPE OF STN MARKER ON MAP.
C                                X IS THE CURRENT STN MARKER VALUE (1-5)
C     OUTPUT ARGUMENTS:
C
C     TXCLR       INT2       REVISED TEXT COLOR
C     TXFONT      INT2       REVISED TEXT FONT
C     TXSIZE      REAL       REVISED TEXT HEIGHT (NWC)
C     ASP         REAL       REVISED TEXT ASPECT RATIO
C     TXLOC       REAL ARRAY REVISED POSITION OF TEXT STRING (NWC)
C     LIMCHK      INT2       REVISED TYPE OF STN MARKER ON MAP.  VALID ONLY
C                            WHEN LIMCHK IS -X ON INPUT
C------------------------------------------------------------------------------
$INCLUDE: 'SAVBUF.INC'
C
      INTEGER*2    MENUNBR, TXCLR, TXFONT, PATH, PALETTE, PALDEF(16,12),
     +             HELPLVL, LIMCHK
      INTEGER*4    MBYTS
      REAL         TXSIZE, TXLOC(2), XWIN, YWIN, ASP, HGT, WID, OFST
      REAL         XND, YND, XWR1, XWR2, YWR1, YWR2 
      CHARACTER*2  INCHAR   
      CHARACTER*28 TXT
      CHARACTER*78 MSG
C
      IF (MENUNBR .EQ. 29) THEN
         CALL GETMSG(541,MSG)
         CALL GETMSG(999,MSG)
         TXT = MSG
      ENDIF
      CALL DELIMSTR(TXT,MSG)
      CALL INQSTS(MSG,HGT,WID,OFST)
C
C     VERIFY THAT THE MENU DOES NOT INTERFERE WITH THE LABEL BEING REVISED
C
  100 IF (TXLOC(1) .GT. 0.3) THEN
         XWIN = 0.1
      ELSE 
         XWIN = 0.8
      ENDIF
      IF (TXLOC(2) .GT. 0.3) THEN
         YWIN = 0.8
      ELSE
         YWIN = 0.1
      ENDIF
C
      IF (MENUNBR .EQ. 29) THEN
         HELPLVL = 29
      ELSE   
         HELPLVL = 20
      ENDIF   
      CALL GRAFMNU(1,MENUNBR,XWIN,YWIN,HELPLVL,INCHAR)
      IF     (INCHAR .EQ. '1 ') THEN
             CALL TXTCOLR(MSG,TXLOC(1),TXLOC(2),TXSIZE,ASP,TXFONT,PATH,
     +       TXCLR,PALETTE,PALDEF,LIMCHK)
      ELSEIF (INCHAR .EQ. '2 ') THEN
             CALL TXTFONT(MSG,TXLOC(1),TXLOC(2),TXSIZE,ASP,TXFONT,PATH,
     +       TXCLR,LIMCHK) 
      ELSEIF (INCHAR .EQ. '3 ') THEN
             IF (MENUNBR .NE. 29) THEN
                CALL TXTSIZ(MSG,TXLOC(1),TXLOC(2),TXSIZE,ASP,TXFONT,
     +          PATH,TXCLR,LIMCHK,-1)
             ELSE
C----           MUST OPEN A WINDOW TO CHANGE SIZE OF A MAP STN MARKER, 
C----           NOT XOR THE ACTUAL TEXT AS FOR NON-AXIS LABELS
                XND=.2
                YND=.7
                CALL BUFSIZ(XND,YND-0.4,XND+0.4,YND,MBYTS)
                IF (MBYTS .GT. MAXBYT) THEN
                   MSGN1=554
                   MSGN2=202
                   CALL GRAFNOTE(XND,YND,MSGN1,MSGN2,' ',0,INCHAR)
                   GO TO 100
                ENDIF
                CALL MAPNTW(XND,YND-0.4,XWR1,YWR1)
                CALL MAPNTW(XND+0.4,YND,XWR2,YWR2)
                CALL MOVEFR(XWR1,YWR1,XWR2,YWR2,BUFFER)
C                IF (TXCLR .EQ. 0) THEN
C                   IBGCLR = 1
C                ELSE
C                   IBGCLR = 0
C                ENDIF
                IBGCLR = 0
                CALL SETCOL(IBGCLR)
                CALL BAR(XWR1,YWR1,XWR2,YWR2)
                CALL SETCOL(IBGCLR+1)
                CALL BOX(XWR1,YWR1,XWR2,YWR2)
                CALL SETCOL(IBGCLR)
                CALL TXTSIZ(MSG,XND+0.2,YND-0.38,TXSIZE,ASP,TXFONT,0,
     +                      TXCLR,0,IBGCLR)
                CALL MOVETO(XWR1,YWR1,BUFFER,1)
             ENDIF
      ELSEIF (INCHAR .EQ. '4 ') THEN
             IF (LIMCHK .GE. 0) THEN
                CALL TXTLOC(MSG,TXLOC(1),TXLOC(2),TXSIZE,ASP,TXFONT,
     +          PATH,TXCLR,LIMCHK)
             ELSE
C----           CHANGE THE TYPE OF MARKER USED FOR POINT VALUES ON A MAP
                LPICK = IABS(LIMCHK)
                WRITE(UNIT=INCHAR,FMT='(I1,1X)') LPICK
                HELPLVL=43
                CALL GRAFMNU(5,43,XWIN,YWIN,HELPLVL,INCHAR)
                IF (INCHAR .GE. '1 ' .AND. INCHAR .LE. '5 ') THEN
                   READ (UNIT=INCHAR,FMT='(I1,1X)') LPICK
                   LIMCHK = - LPICK
                ENDIF
             ENDIF
      ELSEIF (INCHAR .EQ. '4F' .OR. INCHAR .EQ. 'ES') THEN
             IF (LIMCHK .GT. 0) THEN
                CALL FLUSH(MSG,TXLOC(1),TXLOC(2),TXSIZE,ASP,TXFONT,
     +          PATH,LIMCHK)
             ELSE
                IF (LIMCHK .LT. 0) THEN
                   LIMCHK = LPICK - 1
                ENDIF
             ENDIF           
             RETURN
      ENDIF
      GO TO 100
      END      
