$STORAGE:2
      SUBROUTINE COMTPOS(XWPOS,YWPOS,WIDTH,HEIGHT,PATH,XTPOS
     +                  ,YTPOS,X1BAR,Y1BAR,X2BAR,Y2BAR,LIMITS)
C------------------------------------------------------------------------------
C     COMPUTE THE TEXT POSITION REQUIRED BY HALO (BOTTOM-LEFT) FROM THE CLICOM
C     STANDARD (BOTTOM-CENTER) SO THAT THE TEXT IS DISPLAYED PROPERLY.  FOR
C     TEXT THAT IS ROTATED FROM THE NORMAL PATH, ADDITIONAL ADJUSTMENTS ARE
C     DONE.  COMPUTE THE 2 DIAGONAL CORNERS OF A BOX THAT SURROUNDS THE TEXT.
C
C     INPUT ARGUMENTS:
C
C     XWPOS,YWPOS   REAL  CURRENT POSITION OF TEXT STRING (BOTTOM-CENTER IN
C                         WORLD COORDS). MAY BE OUTSIDE PLOT AREA
C     WIDTH         REAL  CURRENT TEXT WIDTH  (WORLD COORDS)
C     HEIGHT        REAL  CURRENT TEXT HEIGHT (WORLD COORDS)
C     PATH          INT2  CURRENT TEXT PATH
C     LIMITS        INT2  FLAG TO CONTROL TEXT POSITION: 
C                         0 = NO CHECKS
C                         1 = LEFT AND RIGHT SIDE EDGE CHECK
C                         2 = TOP AND BOTTOM EDGE CHECK
C
C     OUTPUT ARGUMENTS:
C
C     XWPOS,YWPOS   REAL  CURRENT POSITION OF TEXT STRING (BOTTOM-CENTER IN
C                         WORLD COORDS). MAY BE OUTSIDE PLOT AREA.  VALUES
C                         IS ADJUSTED ONLY IF AN EDGE CHECK IS REQUESTED
C     XTPOS,YTPOS   REAL  TEXT COORDINATES COMPUTED FOR HALO STROKE TEXT 
C                         CALL WHICH IS THE LOWER LEFT CORNER OF THE TEXT
C     X1BAR-Y2BAR   REAL  CORNERS OF BOX THAT SURROUNDS THE TEXT (WORLD COORDS)
C------------------------------------------------------------------------------
$INCLUDE: 'GRFPARM.INC'
$INCLUDE: 'GRAFVAR.INC'
      INTEGER     PATH
      REAL        WIDTH,HEIGHT,XWIDTH
      CALL GETWASP(WASP)
      XWIDTH  = WIDTH * WASP
      XHEIGHT = HEIGHT / WASP
      CALL NW2W(GANWLF,GANWBT,WLF,WBT)
      CALL NW2W(GANWRT,GANWTP,WRT,WTP)
C
C     FOR AN EDGE CHECK (LIMITS=1,2) THE CORNERS OF THE BOX MUST NOT EXTEND
C     BEYOND THE PLOT AREA.  ADJUST TEXT POSITION WHICH IS CENTER-BOTTOM
C     AS NEEDED SO THAT THE DETERMINATION OF THE TEXT BOX IS IN THE PLOT AREA.
C
      IF (LIMITS .EQ. 1) THEN
         IF ((XWPOS - .5*WIDTH) .LT. WLF) THEN
            XWPOS = WLF + .5*WIDTH
         ENDIF
         IF ((XWPOS + .5*WIDTH) .GT. WRT) THEN
            XWPOS = WRT - .5*WIDTH
         ENDIF
      ENDIF
C
      IF (LIMITS .EQ. 2) THEN
         IF ((YWPOS - .5*XWIDTH) .LT. WBT) THEN
            YWPOS = WBT + .5*XWIDTH
         ENDIF
         IF ((YWPOS + .5*XWIDTH) .GT. WTP) THEN
            YWPOS = WTP - .5*XWIDTH
         ENDIF
      ENDIF

C----------------  NORMAL TEXT PATH --------------

      IF (PATH.EQ.0) THEN
         XTPOS = XWPOS-.5*WIDTH
         YTPOS = YWPOS 
         X1BAR = XTPOS
         X2BAR = XTPOS + WIDTH
         Y1BAR = YTPOS 
         Y2BAR = YWPOS + HEIGHT 

C---------------- LEFT SIDED TEXT PATH ----------

      ELSE IF (PATH.EQ.1) THEN
         XTPOS = XWPOS 
         YTPOS = YWPOS-.5*XWIDTH
C         X1BAR = XTPOS 
C         X2BAR = XTPOS - XHEIGHT
         X1BAR = XTPOS - XHEIGHT
         X2BAR = XTPOS 
         Y1BAR = YTPOS 
         Y2BAR = YTPOS + XWIDTH

C---------------- RIGHT SIDED TEXT PATH ---------

      ELSE IF (PATH.EQ.3) THEN
         XTPOS = XWPOS 
         YTPOS = YWPOS+.5*XWIDTH
         X1BAR = XTPOS 
         X2BAR = XTPOS + XHEIGHT
C         Y1BAR = YTPOS 
C         Y2BAR = YTPOS - XWIDTH
         Y1BAR = YTPOS - XWIDTH
         Y2BAR = YTPOS 
      ENDIF
      RETURN
C
C-------  DIAGRAMS FOR THE VARIOUS PATHS, 7-MAY-91  -------
C
C         0                       1                             3
C
C  ________________             _______                      _______
C  ³              ³             ³     ³                      ³  S  ³
C  ³              ³             ³  E  ³                      ³  A  ³
C  ³  SAMPLE TXT  ³             ³  L  ³                      ³  M  ³
C  ³      *       ³             ³  P *³                      ³* P  ³
C  ----------------             ³  M  ³                      ³  L  ³
C                               ³  A  ³                      ³  E  ³
C                               ³  S  ³                      ³     ³
C                               -------                      -------
C 
C
C  THE * IS THE LOCATION POINT FOR THE TEXT, CENTER BOTTOM.  X1BAR & Y1BAR
C  ARE CALCULATED TO ALWAYS BE THE LOWER LEFT CORNER OF THE BOX, WHILE 
C  X2BAR & Y2BAR ARE THE UPPER RIGHT CORNER OF THE BOX.  PATH 2 IS UPSIDE
C  DOWN TEXT AND NOT USED.
C 
      END
