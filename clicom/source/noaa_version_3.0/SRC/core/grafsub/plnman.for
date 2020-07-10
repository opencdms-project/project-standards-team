$STORAGE:2
      SUBROUTINE PLNMAN(XWIN,YWIN,IGRAPH,PALETTE,PALDEF,CONLEV,
     +                  NTOTLEV,INTTL,COLTYPE,COL1CLR,COLTHK,COL2CLR,
     +                  CONINCR,NCONLEV)
C     
C       ** OBJECTIVE:  MODIFY VARIABLES THAT CONTROL THE VISUAL REPRESENTATION
C                      OF THE PLOT LINES.  MENU CHOICES ARE TAILORED TO 
C                      EACH PLOT TYPE.  MODIFICATIONS TO THE FOLLOWING 
C                      CATEGORIES ARE CONTROLLED BY THIS ROUTINE:   
C                         1. COLOR --------- LINE,POINT,FILL
C                         2. PATTERN ------- LINE,HATCH
C                         3. WIDTH --------- LINE
C                         4. CONTOUR ------- INTERVAL OPTIONS 
C                         5. COLOR --------- BORDER OF FILL
C                         6. PLOT OPTION --- LINE,POINT,BAR,FILL
C       ** INPUT:
C             XWIN......
C             YWIN......
C             IGRAPH....
C             PALETTE...
C             PALDEF....
C             CONLEV....
C             NTOTLEV...
C             INTTL.....
C       ** OUTPUT:      
C             COLTYPE...
C             COL1CLR...
C             COLTHK....
C             COL2CLR...
C             CONINCR...
C             NCONLEV...
C
      REAL*4         XWIN(*),YWIN(*),CONLEV(*)
      INTEGER*2      IGRAPH,PALETTE,PALDEF(16,*)
      INTEGER*2      COLTYPE,COL1CLR,COLTHK,COL2CLR
      CHARACTER *(*) INTTL
C
$INCLUDE:  'GRFPARM.INC'
C
      PARAMETER (MAXCHAR=22,MAXCHOICE=9,MAXITEM=MAXCHOICE+1)
      INTEGER*2 WINSTAT,HELPLVL
      CHARACTER*2 INCHAR
      CHARACTER *(MAXCHAR) CHOICE(MAXITEM),MNUTTL
C      
      NBRPICK=16
      IPAL=PALETTE
      IPICK=2
C
      IPLTYP = INT(COLTYPE/10)      
      IPLPAT = COLTYPE - IPLTYP*10
C
      IF (IGRAPH.EQ.1) THEN
         MENUNUM = 33
         CALL RDMENU(MENUNUM,MAXITEM,CHOICE,NUMCHOICE,RTNCODE)
         NCHR1 = LNG(INTTL)
         NCHR2 = LNG(CHOICE(IPLTYP+1))
         NCHR1 = MAX0(MAXCHAR-(NCHR2+2),1)
         MNUTTL = INTTL(1:NCHR1)//'--'//CHOICE(IPLTYP+1)
      ELSE   
         MNUTTL = INTTL
      ENDIF      
C
C       ** INITIAL WINDOW CONTROL VARIABLES;  WINSTAT IS SET TO DRAW MENU,
C          AND GET CHOICE; SCREEN IS NOT SAVED
C
      WINSTAT = 2
C
   10 CONTINUE
      IF (WINSTAT.EQ.2) THEN
         IF (IGRAPH.EQ.1) THEN
C
C          .. TIMESERIES
            IF (IPLTYP.EQ.1) THEN
C                .. LINE            
               MENUNUM = 34
               NXTRA = 0
               IPATT = 2
            ELSE IF (IPLTYP.EQ.2) THEN
C                .. POINT            
               MENUNUM = 34
               NXTRA = 2
               IPATT = 2
            ELSE IF (IPLTYP.EQ.3) THEN
C                .. BAR            
               MENUNUM = 35
               NXTRA = 0
               IPATT = 3
            ELSE 
C                .. FILL            
               MENUNUM = 35
               NXTRA = 1
               IPATT = 3
            ENDIF
         ELSE IF (IGRAPH.EQ.2) THEN   
C
C             .. MAP         
            MENUNUM = 36
            NXTRA = 0
            IPATT = 2
         ELSE IF (IGRAPH.EQ.3) THEN   
C
C             .. SKEWT         
            MENUNUM = 36
            NXTRA = 1
            IPATT = 2
         ELSE    
C
C             .. WINDROSE        
            MENUNUM = 37
            NXTRA = 0
            IPATT = 3
         ENDIF
         CALL RDMENU(MENUNUM,MAXITEM,CHOICE,NUMCHOICE,RTNCODE)
         CHOICE(1) = MNUTTL
         NUMCHOICE = NUMCHOICE - NXTRA
      ENDIF   
      HELPLVL = MENUNUM
C
C       **  DISPLAY AND PROCESS MENU CHOICES
C      
      CALL ARGMENU(WINSTAT,CHOICE,NUMCHOICE,XWIN(1),YWIN(1),HELPLVL,
     +             INCHAR)
      IF (INCHAR.EQ.'ES') THEN
C
C          .. ESC EXITS FROM THIS MENU 
         COLTYPE = IPLTYP*10 +IPLPAT
         GO TO 100
      ELSE   
C
         READ(INCHAR,500) INVAL
         IF (IGRAPH.EQ.1) THEN
C         
C             .. TIMESERIES
            IF (INVAL.EQ.1) THEN
C                .. PLOT OPTION            
               IDCHOICE = 6
            ELSE IF (INVAL.EQ.4 .AND. IPLTYP.EQ.3) THEN
C                .. BORDER COLOR FOR BAR PLOT            
               IDCHOICE = 5
            ELSE
               IDCHOICE = INVAL-1
            ENDIF
         ELSE
C
C             .. MAP,SKEWT,WINDROSE         
            IDCHOICE = INVAL            
         ENDIF   
C         
         WINSTAT = 3
         IF (IDCHOICE.EQ.1) THEN
C             .. LINE/FILL COLOR         
            ICNTRL=0
            ICLR=COL1CLR
            CALL PICKCOL(ICLR,XWIN(2),YWIN(2),ICNTRL,NBRPICK,IPAL,
     +                   PALDEF,IPICK)
            IF (ICLR.NE.-1) COL1CLR=ICLR
         ELSE IF (IDCHOICE.EQ.2) THEN
C             .. LINE/HATCH PATTERN
            CALL LNATRIB(IPATT,XWIN(2),YWIN(2),IPLPAT) 
         ELSE IF (IDCHOICE.EQ.3) THEN
C             .. LINE WIDTH
            ICNTRL=1
            CALL LNATRIB(ICNTRL,XWIN(2),YWIN(2),COLTHK) 
         ELSE IF (IDCHOICE.EQ.4) THEN
C             .. CONTOUR
            CALL CONMAN(XWIN(2),YWIN(2),CONLEV,NTOTLEV,
     +                  CONINCR,NCONLEV)
         ELSE IF (IDCHOICE.EQ.5) THEN
C             .. BORDER COLOR
            ICNTRL=0
            ICLR=COL2CLR
            CALL PICKCOL(ICLR,XWIN(2),YWIN(2),ICNTRL,NBRPICK,IPAL,
     +                   PALDEF,IPICK)
            IF (ICLR.NE.-1) COL2CLR=ICLR
         ELSE 
C             .. PLOT OPTION -- IDCHOICE=6
            MENUNUM = 33
            HELPLVL = 33
            CALL RDMENU(MENUNUM,MAXITEM,CHOICE,NUMCHOICE,RTNCODE)
            IWSTAT = 4
            WRITE(INCHAR,500) IPLTYP
            CALL ARGMENU(IWSTAT,CHOICE,NUMCHOICE,XWIN(2),YWIN(2),
     +                   HELPLVL,INCHAR)
            IF (INCHAR.NE.'ES') THEN
               READ(INCHAR,500) IPLTYP
               NCHR1 = LNG(INTTL)
               NCHR2 = LNG(CHOICE(IPLTYP+1))
               NCHR1 = MAX0(MAXCHAR-(NCHR2+2),1)
               MNUTTL = INTTL(1:NCHR1)//'--'//CHOICE(IPLTYP+1)
               WINSTAT = 2
            ENDIF
            IWSTAT=0
            CALL ARGMENU(IWSTAT,CHOICE,NUMCHOICE,XWIN(2),YWIN(2),
     +                   HELPLVL,INCHAR)
         ENDIF   
C
C          .. MAXIMUM PATTERN TYPE FOR A LINE PLOT IS 3
         IF (IPLTYP.EQ.1) IPLPAT=MIN0(IPLPAT,3)
C         
         IF (WINSTAT.EQ.2) THEN
C
C             .. REMOVE OLD MENU BEFORE REDRAWING MENU WITH NEW PLOT OPTION
            IWSTAT=0
            CALL ARGMENU(IWSTAT,CHOICE,NUMCHOICE,XWIN(1),YWIN(1),
     +                   HELPLVL,INCHAR)
         ENDIF
      ENDIF   
C
C       ** RETURN TO GET ANOTHER MENU CHOICE; REDRAW MENU IF PLOT OPTION CHOSEN
C      
      GO TO 10     
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