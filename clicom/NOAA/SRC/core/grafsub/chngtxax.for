$STORAGE:2
      SUBROUTINE CHNGTXAX(MENUNBR,TXCLR,TXFONT,TXSIZE,ASP,
     +                    DECLF,DECRT,CHRBT)
C------------------------------------------------------------------------------
C     MANAGE THOSE ROUTINES THAT CHANGE THE ATTRIBUTES (FONT, SIZE, AND
C     DECIMAL PLACES) OF THE TEXT LABELS ON THE X & Y AXIS FOR THE TIME 
C     SERIES OR MAP GRAPH.  DECIMAL PLACE OPTION IS VALID FOR TIME SERIES ONLY.
C
C     INPUT ARGUMENTS:
C
C     MENUNBR  INT2       NUMBER OF MENU TO DISPLAY FROM GRAPHICS MENU FILE
C     TXCLR    INT2       TEXT COLOR (NOT CHANGEABLE)
C     TXFONT   INT2       CURRENT TEXT FONT
C     TXSIZE   REAL       CURRENT TEXT HEIGHT (NORMALIZED WORLD COORDINATES)
C     ASP      REAL       CURRENT TEXT ASPECT RATIO
C     DECLF    INT2       CURRENT NBR OF DECIMAL PLACES FOR TEXT ON LEFT AXIS
C     DECRT    INT2       CURRENT NBR OF DECIMAL PLACES FOR TEXT ON RIGHT AXIS
C     CHRBT    INT2       CURRENT NBR OF TEXT CHARS FOR BOTTOM AXIS

C
C     OUTPUT ARGUMENTS:
C
C     TXFONT   INT2       REVISED TEXT FONT
C     TXSIZE   REAL       REVISED TEXT HEIGHT (NORMALIZED WORLD COORDINATES)
C     ASP      REAL       REVISED TEXT ASPECT RATIO
C     DECLF    INT2       REVISED NBR OF DECIMAL PLACES FOR TEXT ON LEFT AXIS
C     DECRT    INT2       REVISED NBR OF DECIMAL PLACES FOR TEXT ON RIGHT AXIS
C     CHRBT    INT2       REVISED NBR OF TEXT CHARS FOR BOTTOM AXIS
C------------------------------------------------------------------------------
      INTEGER      CHRBT
$INCLUDE: 'SAVBUF.INC'
C
C       ** LOCAL COMMON TO SAVE SPACE IN D-GROUP
C
      INTEGER      MENUNBR, TXCLR, TXFONT,  DECLF, DECRT, BGCOLR
      INTEGER      HELPLVL  
      INTEGER*4    MBYTS
      REAL         TXSIZE, ASP, THGT,
     +             XND, YND, XWR1, YWR1, XWR2, YWR2
      CHARACTER*2  INCHAR, NUMVAL, CURVAL   
      CHARACTER*78  MSG
      CHARACTER*20  TXT
      COMMON /CHAXSV/ HELPLVL,BGCOLR, MBYTS, NBCHR, THGT, XND, YND 
     +               ,XWR1,YWR1,XWR2,YWR2,INCHAR,NUMVAL,CURVAL,MSG,TXT
C
      XND   = 0.2
      YND   = 0.7
      HELPLVL = MENUNBR
      CALL GETMSG(541,MSG)
      CALL GETMSG(999,MSG)
      CALL DELIMSTR(MSG,TXT)
      CALL DEFHST(TXFONT,TXCLR,0.0,ASP,TXSIZE,THGT)
  100 CALL GRAFMNU(1,MENUNBR,XND,YND,HELPLVL,INCHAR)
      IF     (INCHAR .EQ. '1 ') THEN
             CALL TXTFONT(TXT,1.0,1.0,TXSIZE,ASP,TXFONT,0,TXCLR,0)
      ELSEIF (INCHAR .EQ. '2 ') THEN
             CALL BUFSIZ(XND,YND-0.4,XND+0.4,YND,MBYTS)
             IF (MBYTS .GT. MAXBYT) THEN
                XWIN=.1
                YWIN=.95
                MSGN1=554
                MSGN2=202
                CALL GRAFNOTE(XWIN,YWIN,MSGN1,MSGN2,' ',0,INCHAR)
                CALL FINHALO
                STOP 2
             ENDIF
             CALL MAPNTW(XND,YND-0.4,XWR1,YWR1)
             CALL MAPNTW(XND+0.4,YND,XWR2,YWR2)
             CALL MOVEFR(XWR1,YWR1,XWR2,YWR2,BUFFER)
C             IF (TXCLR .EQ. 0) THEN
C                BGCOLR = 1
C             ELSE
C                BGCOLR = 0
C             ENDIF
             BGCOLR = 0
             CALL SETCOL(BGCOLR)
             CALL BAR(XWR1,YWR1,XWR2,YWR2)
             CALL SETCOL(BGCOLR+1)
             CALL BOX(XWR1,YWR1,XWR2,YWR2)
             CALL SETCOL(BGCOLR)
             CALL TXTSIZ(TXT,XND+0.2,YND-0.38,TXSIZE,ASP,TXFONT,0,TXCLR,
     +                   0,BGCOLR)
             CALL MOVETO(XWR1,YWR1,BUFFER,1)
      ELSEIF (INCHAR .EQ. '3 ') THEN
 410         NUMVAL = ' '
             NBCHR  = 0
             WRITE(UNIT=CURVAL,FMT='(I2)') DECLF
             CALL GRAFMSG(.4,.9,538,539,CURVAL,2,3,1,NUMVAL,NBCHR)
             IF (NUMVAL .NE. 'ES') THEN
                READ(UNIT=NUMVAL,FMT='(I1)') BGCOLR
                IF (BGCOLR .GT. 4) THEN
                   CALL GRAFNOTE(.4,.9,553,202,' ',0,NUMVAL)
                   GO TO 410
                ENDIF
                DECLF = BGCOLR
             ENDIF
      ELSEIF (INCHAR .EQ. '4 ') THEN
 510         NUMVAL = '  '
             NBCHR  = 0
             WRITE(UNIT=CURVAL,FMT='(I2)') DECRT
             CALL GRAFMSG(.4,.9,538,539,CURVAL,2,3,1,NUMVAL,NBCHR)
             IF (NUMVAL .NE. 'ES') THEN
                READ(UNIT=NUMVAL,FMT='(I1)') BGCOLR
                IF (BGCOLR .GT. 4) THEN
                   CALL GRAFNOTE(.4,.9,553,202,' ',0,NUMVAL)
                   GO TO 510
                ENDIF
                DECRT = BGCOLR
             ENDIF
      ELSEIF (INCHAR .EQ. '5 ') THEN
 610         NUMVAL = '  '
             NBCHR  = 0
             WRITE(UNIT=CURVAL,FMT='(I2)') CHRBT
             CALL GRAFMSG(.4,.9,538,540,CURVAL,2,3,2,NUMVAL,NBCHR)
             IF (NBCHR .GT. 0) THEN
                IF (NBCHR .EQ. 2) THEN
                   READ(UNIT=NUMVAL,FMT='(I2)') BGCOLR
                ELSE
                   READ(UNIT=NUMVAL,FMT='(I1)') BGCOLR
                ENDIF
                IF (BGCOLR .LT. 0 .OR. BGCOLR .GT. 10) THEN
                   CALL GRAFNOTE(.4,.9,553,202,' ',0,NUMVAL)
                   GO TO 610
                ENDIF
                CHRBT = BGCOLR
             ENDIF
      ELSEIF (INCHAR .EQ. '4F' .OR. INCHAR .EQ. 'ES') THEN
             RETURN
      ENDIF
      GO TO 100
      END      
