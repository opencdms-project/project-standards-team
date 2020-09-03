$STORAGE:2
      SUBROUTINE DRWLGND (QCORIENT,BORDER,NUMENTRY,LINBAR,LBSTYL,
     +           LBWIDTH,KOLOR,DESCRP,LEGCLR,LEGFONT,LEGSIZE,LEGLOC,
     +           LEGASP,ALIGN)
C------------------------------------------------------------------------------
C     DRAW A LEGEND DESCRIBING THE ITEMS (LINES, BARS, POINTS, OR FILL AREA)
C     DISPLAYED ON A GRAPH.  EACH ENTRY IN THE LEGEND CONSISTS OF A SYMBOL,
C     LINE/BAR/CHAR FOLLOWED BY A TEXT DESCRIPTION.  A TEXT DESCRIPTION 
C     WITHOUT A SYMBOL IS A VALID ENTRY AS WELL. 
C 
C     INPUT ARGUMENTS:
C
C     QCORIENT INT2       ORIENTATION OF LEGEND:
C                         0 = NO LEGEND
C                         1 = HORIZONTAL
C                         2 = VERTICAL
C                         *** IF VALUE IS <0 THEN DRWLGND IS CALLED FROM KEY 
C                             ENTRY QUALITY CONTROL PROGRAMS AND PLOT BEHIND 
C                             LEGEND DOES NOT NEED TO BE SAVED
C     BORDER   CHAR       DRAW A BORDER AROUND THE LEGEND
C                         Y = YES
C     NUMENTRY INT2       NUMBER OF ENTRIES IN THE LEGEND.  EACH ENTRY
C                         HAS A VALUE DEFINED FOR REMAINING INPUT ARRAYS. 
C                         ELEMENT #1 IS FOR THE 1ST ITEM, ELEMENT #2 IS
C                         FOR THE 2ND ITEM, ETC FOR NUMENTRY ELEMENTS
C     LINBAR   CHAR ARRAY TYPE OF ENTRY FOR THIS ITEM:
C                         B = BAR  + TEXT
C                         L = LINE + TEXT
C                         T = TEXT ONLY, NO LINE OR BAR
C                         ANYTHING ELSE = POINT PLOT OF CHAR IN LINBAR + TEXT 
C     LBSTYL   INT2 ARRAY HATCH OR LINE STYLE (FROM HALO) 
C     LBWIDTH  REAL ARRAY WIDTH OF BAR OR LINE. USE 1.0 FOR TEXT OR POINT ENTRY
C     KOLOR    INT2 ARRAY COLOR OF BAR OR LINE.
C     DESCRP   CHAR ARRAY TEXT ASSOCIATED WITH THE BAR OR LINE
C     LEGCLR   INT2       COLOR OF TEXT WITHIN THE LEGEND
C     LEGFONT  INT2       FONT NUMBER FOR TEXT WITHIN THE LEGEND
C     LEGSIZE  REAL       SIZE OF TEXT WITHIN THE LEGEND (NORM WORLD COORDS)
C     LEGLOC   REAL ARRAY CENTER POINT OF LEGEND BOX (NWC)
C     ALIGN    INT2       ALIGN EACH COLUMN OF TEXT CHARACTERS. 
C                         0 = NO 
C                         1 = YES
C
C     OUTPUT ARGUMENTS:  NONE
C------------------------------------------------------------------------------
$INCLUDE: 'SAVBUF.INC'
      CHARACTER*1  BORDER,LINBAR(*),YES/'Y'/,RTNCODE
      CHARACTER*28 DESCRP(*)
      INTEGER      QCORIENT, ORIENT, NUMENTRY, LBSTYL(*), KOLOR(*),
     +             LEGCLR, LEGFONT, ALIGN, DEVCODE
      INTEGER*4    MBYTS,BUFBYT
      REAL         HGLINE(24), LBWIDTH(*), LF, LEGSIZE, LEGLOC(2),LEGASP
C
      ORIENT = IABS(QCORIENT)      
      IF (QCORIENT.LT.0) THEN
         BUFBYT = -1
      ELSE
         BUFBYT = MAXBYT
      ENDIF      
      IF (ORIENT .EQ. 0 .OR. ORIENT .GT. 2) THEN
         RETURN
      ENDIF
      CALL INQWOR (LF,BO,RG,TP)
      CALL INQDRA (MX,MY)
      XPIX=MX+1
      YPIX=MY+1
      HGPIX = (TP-BO)/YPIX
      CALL DEFHST(LEGFONT,LEGCLR,0.0,LEGASP,LEGSIZE,TXHG)
C
C---  DETERMINE SIZE OF THE LEGEND BOX
C
      CALL LGNDSIZE(ORIENT,NUMENTRY,LINBAR,LBWIDTH,DESCRP,LEGLOC,
     +    ALIGN,BUFBYT,ITEMLN,MAXLIN,HGLINE,HGTXT,WIDITM,WDTXT,XLBX,
     +    YBBX,XRBX,YTBX,RTNCODE)
      IF (RTNCODE .EQ. '1') THEN
         RETURN
      ENDIF
      XLF=XLBX + (0.05 * WIDITM)
      YTP=YTBX - (0.05 * HGTXT)
C
C----    SAVE PLOT BEHIND LEGEND BEFORE DRAWING LEGEND, EXCEPT FOR VRI DRIVER
C
      CALL INQDEV(DEVCODE)
      IF (DEVCODE.NE.68 .AND. QCORIENT.GE.0) THEN
         CALL MAPWTN(XLBX,YTBX,XND1,YND1)
         CALL MAPWTN(XRBX,YBBX,XND2,YND2)
         CALL BUFSIZ(XND1,YND1,XND2,YND2,MBYTS)
         CALL MOVEFR(XLBX,YTBX,XRBX,YBBX,BUFFER)
         OPEN (UNIT=62,ACCESS='DIRECT',FORM='BINARY',RECL=512,
     +         FILE='O:\DATA\SQUX')
         WRITE(62) BUFFER
         CLOSE(62)
      ENDIF
C
C---  CLEAR LEGEND AREA TO BACKGROUND COLOR 
C
      CALL INQBKN(KOLBAK)
      CALL SETCOL(KOLBAK)
      CALL SETHAT(1)
      CALL BAR   (XLBX,YBBX,XRBX,YTBX)
      YBO =YTP - (HGLINE(1) + (0.2 * HGTXT))
      YCTR=YTP - ((YTP-YBO)/2.0)
      XTX =XLF + (WIDITM * 0.1)
      YTX =YBO
C
C---  DRAW EACH ENTRY IN THE LEGEND
C
      J=1
      DO 500 N=1,NUMENTRY,ITEMLN
         XBLF=XLF
         N2=NUMENTRY-N
         IF (N2 .GE. ITEMLN) THEN
            N2=ITEMLN-1
         ENDIF
         DO 400 L=0,N2,1
            CALL SETCOL (KOLOR(N+L))
            IF (LINBAR(N+L) .EQ. 'B') THEN
C
C------------  DRAW A BAR ENTRY
C
               CALL SETHAT (LBSTYL(N+L))
               XBRG=XBLF+(0.3 * WDTXT)
               YTP=YCTR + (LBWIDTH(N+L)/2.0)
               YBO=YCTR - (LBWIDTH(N+L)/2.0)
               CALL BAR (XBLF,YBO,XBRG,YTP)
               XTX=XBRG + (WDTXT * 0.05)
               YTX=YCTR - (HGTXT/2.0)
               CALL SETHAT (1)
               CALL SETLNS (1)
               CALL SETLNW (1)
               IF (ALIGN .EQ. 0) THEN
                  CALL DRWTXT(DESCRP(N+L),XTX,YTX,0,0.0)
               ELSE
                  CALL INQSTS('^W^',HG,WD,OF)
                  NCHR = LNG(DESCRP(N+L))
                  DO 100 K = 1,NCHR
                     CALL DRWTXT(DESCRP(N+L)(K:K),XTX,YTX,0,0.0)
                     XTX = XTX + WD
  100             CONTINUE
               ENDIF
               XBLF=XBLF + WIDITM + (WDTXT * 0.05)
            ELSE
               IF (LINBAR(N+L) .EQ. 'L') THEN
C
C-----------   DRAW A LINE ENTRY
C
                  CALL SETLNS (LBSTYL(N+L))
                  LNWD=LBWIDTH(N+L)
                  CALL SETLNW (LNWD)
                  YBO=YCTR - ((LBWIDTH(N+L)*HGPIX)/2.0)
C--SHOULD IT BE?  YBO=YCTR - ((HGLINE(J))/2.0)
                  CALL MOVABS (XBLF,YBO)
                  XBRG=XBLF+(0.3 * WDTXT)
                  CALL LNABS  (XBRG,YBO)
                  XTX=XBRG + (WDTXT * 0.05)
                  YTX=YCTR - (HGTXT/2.0)
               ELSE
C
C-----------      DRAW A POINT OR TEXT ENTRY
C
                  XTX=XBLF
                  YTX=YBO
                  IF (LINBAR(N+L) .EQ. 'T') THEN
                     XBRG=XBLF+(0.3 * WDTXT)
                     GO TO 190
                  ENDIF
                  CALL SETSTC (KOLOR(N+L),KOLOR(N+L))
                  CALL DRWTXT(LINBAR(N+L),XTX,YTX,0,0.0)
                  CALL SETSTC (LEGCLR,LEGCLR)
                  XBRG=XBLF+(0.3 * WDTXT)
                  XTX =XBRG + (WDTXT * 0.05)
               ENDIF
  190          CALL SETLNS (1)
               CALL SETLNW (1)
               IF (ALIGN .EQ. 0) THEN
                  CALL DRWTXT(DESCRP(N+L),XTX,YTX,0,0.0)
               ELSE
                  CALL INQSTS('^W^',HG,WD,OF)
                  NCHR = LNG(DESCRP(N+L))
                  DO 200 K = 1,NCHR
                     CALL DRWTXT(DESCRP(N+L)(K:K),XTX,YTX,0,0.0)
                     XTX = XTX + WD
  200             CONTINUE
               ENDIF
               XBLF=XBLF + WIDITM + (WDTXT * 0.05)
            ENDIF
  400    CONTINUE
         YTP =YCTR - (HGLINE(J)/2.0 + (0.25 * HGLINE(J)))
         J   =J+1
         YBO =YTP  -  HGLINE(J) 
         YCTR=YTP  - ((YTP-YBO)/2.0)
  500 CONTINUE
      CALL DELTCU
      CALL SETLNS (1)
      CALL SETLNW (1)
      IF (BORDER .EQ. YES) THEN
         CALL SETCOL (LEGCLR)
         CALL BOX (XLBX,YBBX,XRBX,YTBX)
      ENDIF
      RETURN
      END
