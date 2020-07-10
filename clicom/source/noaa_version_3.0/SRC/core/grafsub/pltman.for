$STORAGE:2
      SUBROUTINE PLTMAN
C
C       ** OBJECTIVE:  MODIFY VARIABLES THAT CONTROL THE VISUAL REPRESENTATION
C                      OF THE  CURRENT PLOT.  MENU CHOICES ARE TAILORED TO 
C                      EACH PLOT TYPE.  MODIFICATIONS TO THE FOLLOWING 
C                      CATEGORIES ARE CONTROLLED BY THIS ROUTINE:   
C                         1. AXES -------------------- COLOR,WIDTH,TICSIZE
C                         2. XGRID(HORIZONTAL)-------- COLOR,WIDTH,STYLE
C                         3. YGRID(VERTICAL) --------- COLOR,WIDTH,STYLE
C                         4. MAPLINES ---------------- DETAIL,COLOR,WIDTH,STYLE
C                         5. DATA LINES -------------- COLOR,WIDTH,STYLE
C                         6. WINDROSE CONTROLS ------- NUMBER OF RINGS 
C                                              ------- PER CENT OUTER RING     
C                                              ------- WIDTH OF SMALLEST BAR
C
$INCLUDE:  'GRFPARM.INC'
$INCLUDE:  'GRAFVAR.INC'
$INCLUDE:  'CURRPLT.INC'
C
      PARAMETER (MAXCHAR=22,MAXCHOICE=9,MAXITEM=MAXCHOICE+1)
      INTEGER*2 WINSTAT,HELPLVL
      REAL *4   XWIN(4),YWIN(4)
      CHARACTER*2 INCHAR
      CHARACTER *(MAXCHAR) CHOICE(MAXITEM),MNUTTL
C
      DATA XWIN /.05,.40,.05,.40/,
     +     YWIN /.40,.40,.89,.89/
C
C       ** INITIAL WINDOW CONTROL VARIABLES;  WINSTAT IS SET TO DRAW MENU,
C          AND GET CHOICE; SCREEN IS NOT SAVED
C
      WINSTAT = 2
C
C       ** COMPOSE MENU TAILORED TO EACH GRAPH TYPE;  CORE ENTRIES ARE
C          READ FROM MENU FILE
C      
      IF (IGRAPH.LE.2) THEN
C
C          .. TIMESERIES AND MAP; ADD DATA COLUMN HEADERS TO ENTRIES IN FILE
         IF (IGRAPH.EQ.1) THEN
            MENUNUM = 12
         ELSE         
            MENUNUM = 13
         ENDIF
         CALL RDMENU(MENUNUM,MAXITEM,CHOICE,NUMCHOICE,RTNCODE)
         IDBEG = NUMCHOICE+1
C         
         IDARRAY = NUMCHOICE+1
         DO 20 NC=NCA,NCB
            IDARRAY = IDARRAY+1
            CHOICE(IDARRAY) = COLHDR(NC)
   20    CONTINUE
         NUMCHOICE = IDARRAY-1
      ELSE IF (IGRAPH.EQ.3) THEN
C
C          .. SKEWT      
         MENUNUM = 14
         CALL RDMENU(MENUNUM,MAXITEM,CHOICE,NUMCHOICE,RTNCODE)
      ELSE
C
C          .. WINDROSE; TEXT FOR MENU CHOICE IS IN FILE; TEXT IS COPIED FOR
C             EACH WIND SPEED GROUP AND GROUP NUMBER IS APPENDED TO END
C             OF TEXT STRING 
         MENUNUM = 15
         CALL RDMENU(MENUNUM,MAXITEM,CHOICE,NUMCHOICE,RTNCODE)
C          .. COPY TEXT STRING IF NUMBER OF GROUPS >1         
         NUMCHOICE = MIN0(NCHRBT+3,MAXCHOICE)
         DO 45 N=5,NUMCHOICE
            CHOICE(N+1)=CHOICE(5)
   45    CONTINUE   
C          .. ADD GROUP NUMBER TO TEXT   
         IDBEG = LNG(CHOICE(5)) + 2
         DO 50 N=1,NCHRBT
            WRITE(CHOICE(N+4)(IDBEG:IDBEG),510) N 
   50    CONTINUE   
      ENDIF
C
C       ** LOOP TO DISPLAY AND PROCESS MENU CHOICES
C      
      HELPLVL = MENUNUM
   70 CALL ARGMENU(WINSTAT,CHOICE,NUMCHOICE,XWIN(1),YWIN(1),HELPLVL,
     +             INCHAR)
      IF (INCHAR.EQ.'ES') THEN
C
C          .. ESC EXITS FROM THIS MENU 
         GO TO 100
      ELSE   
C
C          .. USE MENU CHOICE TO DETERMINE INDEX TO DATA      
         IF (IGRAPH.LE.2) THEN
C             .. TIMESERIES AND MAP         
            READ(INCHAR,500) IDCHOICE
            IF (IDCHOICE.GE.IDBEG) THEN
               MNUTTL = CHOICE(IDCHOICE+1)
               IDXDATA = NCA + (IDCHOICE-IDBEG)
               IF (IGRAPH.EQ.2) IDXDATA=IDXDATA-2
               IDCHOICE = 5
            ELSE
               IDXDATA = 0   
               IF (IGRAPH.EQ.2 .AND. IDCHOICE.GT.2) THEN
                  IDCHOICE=IDCHOICE+1
               ENDIF
            ENDIF
         ELSE IF (IGRAPH.EQ.3) THEN
C             .. SKEWT         
            READ(INCHAR,500) IDXDATA
            MNUTTL = CHOICE(IDXDATA+1)
            IDCHOICE = 5
         ELSE
C             .. WINDROSE
            READ(INCHAR,500) IDCHOICE
            IF (IDCHOICE.LE.3) THEN
C                .. NUMBER RINGS, PERCENT OUTER RING, MINIMUM BAR WIDTH        
               IDXDATA  = 0
               IWRFLG   = IDCHOICE
               IDCHOICE = 6
            ELSE   
C                .. WIND SPEED GROUPS
               IDXDATA  = IDCHOICE - 3  
               MNUTTL   = CHOICE(IDCHOICE+1)
               IDCHOICE = 5
            ENDIF   
         ENDIF   
C         
         IF (IDCHOICE.EQ.1) THEN
C
C             .. AXES
            CALL AXMAN(XWIN(2),YWIN(2),PALETTE,PALDEF,VPNDLF,VPNDRT,
     +                 AXSCLR,AXSTHK,TICSIZE)
         ELSE IF (IDCHOICE.EQ.2) THEN
C
C             .. X-GRID (TIMESERIES--HORIZONTAL, MAP--VERTICAL/HORIZONTAL)
            CALL GRDMAN(XWIN(2),YWIN(2),PALETTE,PALDEF,
     +                  XGRDCLR,XGRDTHK,XGRDTYP(IDPLT))         
         ELSE IF (IDCHOICE.EQ.3) THEN
C
C             .. Y-GRID (TIMESERIES--VERTICAL)         
            CALL GRDMAN(XWIN(2),YWIN(2),PALETTE,PALDEF,
     +                  YGRDCLR,YGRDTHK,YGRDTYP(IDPLT))         
         ELSE IF (IDCHOICE.EQ.4) THEN
C
C             .. MAPLINES         
            CALL MAPMAN(XWIN(2),YWIN(2),PALETTE,PALDEF,
     +                  MPCODE,YGRDTHK,YGRDTYP(1))         
         ELSE IF (IDCHOICE.EQ.5) THEN
C
C             .. DATA LINES
            CALL PLNMAN(XWIN(2),YWIN(2),IGRAPH,PALETTE,PALDEF,
     +               CONLEV,NDECRT(2),MNUTTL,COLTYPE(IDXDATA),
     +               COL1CLR(IDXDATA),COLTHK(IDXDATA),COL2CLR(IDXDATA),
     +               CONINCR(IDXDATA),NCONLEV)
         ELSE IF (IDCHOICE.EQ.6) THEN
C
C             .. WINDROSE CONTROLS
            CALL WNRMAN(IWRFLG,NDECLF(2),NDECLF(3),NDECLF(4))
         ENDIF   
      ENDIF
C
C       ** RETURN TO GET ANOTHER MENU CHOICE; MENU WILL NOT BE REDRAWN
C      
      WINSTAT = 3
      GO TO 70     
C
C       ** END OF THIS MENU; REMOVE MENU FROM SCREEN
C      
  100 CONTINUE
      WINSTAT = 0
      CALL ARGMENU(WINSTAT,CHOICE,NUMCHOICE,XWIN(1),YWIN(1),HELPLVL,
     +             INCHAR)
      RETURN
C
C       ** FORMAT STMTS
C
  500 FORMAT(I1,1X)
  510 FORMAT(I1)
      END