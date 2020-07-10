$STORAGE:2
      SUBROUTINE GRAFDFLT
C
C   ROUTINE TO SET THE DEFAULT GRAPH PARAMETER VALUES FOR A NEW GRAPH.
C   THE VALUES ARE STORED IN COMMON BLOCK GRAFVAR.
C      
C   --->  THIS ROUTINE IS ASSOCIATED WITH GRAFINIT <---    
C
$INCLUDE: 'GRFPARM.INC'
$INCLUDE: 'GRAFVAR.INC'
C
C       ** DETERMINE THE PLOT WIDTH AND
C          THE NUMBER OF PLOTS IN A STANDARD DATA FRAME.
C
      IF (IGRAPH.EQ.3) THEN
         PLTWID = NBRELEM
         NUMPLT = 1
      ELSE IF (IGRAPH.EQ.4) THEN
C          .. NOTE:  PLTWID FOR WINDROSE HAS ALREADY BEEN DEFINED IN PICKGARF  
         NUMPLT = 1
      ELSE   
         PLTWID = 1
         IF (IGRAPH.EQ.1) THEN
            NPLTCOL = NUMCOL
         ELSE   
            NPLTCOL = NUMCOL-2
         ENDIF   
         IF (PLTWID.LT.NPLTCOL) THEN
            NUMPLT = NPLTCOL / PLTWID
            IF (NUMPLT*PLTWID.LT.NPLTCOL) THEN
               NUMPLT = NUMPLT + 1
            END IF
         ELSE
            NUMPLT = 1
         END IF
      ENDIF      
C
C       ** DEFINE THE MAXIMUM NUMBER OF PLOT CONTROL VARIABLES THAT
C          COULD BE NEEDED FOR THE CURRENT DATA 
C
      IF (IGRAPH.EQ.2) THEN 
C          .. MAP      
         MXNVAL = MIN0(NUMCOL,MXELEM) - 2       
      ELSE IF (IGRAPH.EQ.4) THEN
C          .. WINDROSE      
         MXNVAL = MXWRCAT
      ELSE   
C          .. TIMESERIES, SKEWT      
         MXNVAL = MIN0(NUMCOL,MXELEM)        
      ENDIF
C
C       ** SET TITLES, DATA LIMITS, LEGEND FLAG, AXIS SCALES
C
      GRTITLE    = ' '
      GRSUBTITLE = ' '      
      LOWROW  = 1
      LOWCOL  = 1
      HIROW   = 999
      HICOL   = NUMCOL
      LEGEND  = 2
      NGRFSCR = 1
      NFRSET = 1
      ITYPSET = 0
      DO 50 I=1,MXNVAL
         LFTSCALE(1,I) = -99999.
         LFTSCALE(2,I) = -99999.
          RTSCALE(1,I) = -99999.
          RTSCALE(2,I) = -99999.
   50 CONTINUE      
      BTSCALE(1) = -99999.
      BTSCALE(2) = -99999.
C
C       .. EXCEPTIONS
      IF (IGRAPH.EQ.2) THEN
         LOWCOL        = 3
         ITYPSET       = 0
         LFTSCALE(1,1) = LOWLAT      
         LFTSCALE(2,1) =  HILAT      
          BTSCALE(1)   = LOWLON      
          BTSCALE(2)   =  HILON      
      ELSE IF (IGRAPH.EQ.3) THEN
         LFTSCALE(1,1) = -4.0      
         LFTSCALE(2,1) = 48.0
         LFTSCALE(2,2) = 400.0
          BTSCALE(1)   = -23.0
          BTSCALE(2)   =  32.1
      ELSE IF (IGRAPH.EQ.4) THEN
         LFTSCALE(1,1) = -36.0
         LFTSCALE(2,1) =  36.0
          BTSCALE(1)   = -36.0      
          BTSCALE(2)   =  36.0
      ENDIF    
C
C       ** SET TEXT VALUES
C
      DO 55 I=1,MXNVAL
         LFTTXT(I) = ' '
          RTTXT(I) = ' '
   55 CONTINUE             
      BOTTXT = ' '
      FTXT   = ' '
C
C       ** SET VIEWPORT --
C          IN ORDER TO OPEN THE VIEWPORT TO THE ENTIRE SCREEN, A MAX VALUE
C          EQUAL TO .999 MUST BE USED.  A VALUE OF 1.0 IS A SPECIAL SIGNAL FOR
C          HALO TO 'TURN OFF' THE VIEWPORT WHICH DOES NOT RESET ASPECT RATIOS
C
      VPNDLF = 0.
      VPNDRT = 0.999
      VPNDBT = 0.999
      VPNDTP = 0.
C
C       ** SET VIEWPORT AND AXIS WINDOW SIZES
C
      IF (IGRAPH.EQ.1) THEN
         GANWLF = .15
         GANWRT = .80
         GANWBT = .15
         GANWTP = .85
      ELSE IF (IGRAPH.EQ.2) THEN
         GANWLF = .05
         GANWRT = .85
         GANWBT = .05
         GANWTP = .85
      ELSE
         GANWLF = .01
         GANWRT = .99
         GANWBT = .01
         GANWTP = .99
      ENDIF   
C
C       ** SET TEXT PARAMETERS:  COLOR, FONT, SIZE, LOCATION
C
        TLCLR     = 1
        TLFONT    = 6      
        TLSIZE    = .05
        TLASP     = 1.07
        TLLOC(1)  = -99999.
        TLLOC(2)  = -99999.
C        
       STLCLR     = 1
       STLFONT    = 6      
       STLSIZE    = .04
       STLASP     = 1.07
       STLLOC(1)  = -99999.
       STLLOC(2)  = -99999.
C        
      LTXTCLR     = 1
      LTXTFONT    = 6      
      LTXTSIZE    = .04
      LTXTASP     = 1.07
      LTXTLOC(1)  = -99999.
      LTXTLOC(2)  = -99999.
C        
      RTXTCLR     = 1
      RTXTFONT    = 6      
      RTXTSIZE    = .04
      RTXTASP     = 1.07
      RTXTLOC(1)  = -99999.
      RTXTLOC(2)  = -99999.
C        
      BTXTCLR     = 1
      BTXTFONT    = 6      
      BTXTSIZE    = .04
      BTXTASP     = 1.07
      BTXTLOC(1)  = -99999.
      BTXTLOC(2)  = -99999.
C        
      FTXTCLR     = 1
      FTXTFONT    = 6      
      FTXTSIZE    = .04
      FTXTASP     = 1.07
      FTXTLOC(1)  = -99999.
      FTXTLOC(2)  = -99999.
C        
       LEGCLR     = 3
       LEGFONT    = 4      
       LEGSIZE    = .03
       LEGASP     = 1.07
       LEGLOC(1)  = -99999.
       LEGLOC(2)  = -99999.
C
       AXSCLR     = 3
       AXSFONT    = 6
       AXSTHK     = 1
      ATXTSIZE    = .03       
      ATXTASP     = 1.07
       TICSIZE    = .014
C
C       .. EXCEPTIONS
      IF (IGRAPH.EQ.2) THEN
C          .. MAP
C               RTXTSIZE....SIZE FOR CONTOUR LABELS      
         RTXTSIZE = .02
      ELSE IF (IGRAPH.EQ.3) THEN
           TLSIZE = .03
          STLSIZE = .03
      ELSE IF (IGRAPH.EQ.4) THEN
           TLSIZE = .04
          STLSIZE = .04
         LTXTSIZE = .03
         RTXTSIZE = .03
         BTXTSIZE = .03
      ENDIF   
C        
C       ** LABEL AND TIC CONTROL.
C          
C       .. FOR TIMESERIES
C              XMAJ/YMAJ   >=0  USE SPECIFIED VALUE FOR TIC INCREMENT
C                           <0  VALUE IS NUMBER OF DESIRED TIC DIVISIONS
C              NCHRBT      MAXIMUM NUMBER OF CHARACTERS IN AXIS LABEL THAT
C                          WILL BE PRINTED
      XMINBT = 0
      NCHRBT = 10
      IF (OBSTYPE.EQ.'MLY') THEN
         NCHRBT = 1
         XMAJBT = 1.
      ELSE IF (OBSTYPE.EQ.'10D') THEN
         NCHRBT = 1
         XMAJBT = 1.
      ELSE IF (OBSTYPE.EQ.'SYN') THEN
         XMAJBT = 1.
      ELSE IF (OBSTYPE.EQ.'HLY') THEN
         XMAJBT = 2.
      ELSE IF (OBSTYPE.EQ.'15M') THEN
         XMAJBT = 12.
      ELSE
         XMAJBT = 5.
      ENDIF
      DO 60 I=1,MXNVAL
          NDECLF(I)  = 1
          NDECRT(I)  = 1
         YMAJLFT(I)  = -4.
          YMAJRT(I)  = -4.
         YMINLFT(I)  = 1
          YMINRT(I)  = 1
   60 CONTINUE
C
C       .. EXCEPTIONS
      IF (IGRAPH.EQ.2) THEN
C
C          .. MAP DEFAULT -- USE 4 MAJOR TIC DIVISIONS
C               XMAJBT......WHOLE DEGREE INCREMENT FOR MAJOR LON DIVISIONS
C               YMAJLFT.....WHOLE DEGREE INCREMENT FOR MAJOR LAT DIVISIONS
C               NDECRT(1)...NUMBER OF DECIMAL PLACES FOR CONTOUR LABELS
C               NDECRT(3)...FONT FOR STATION MARKERS 
C               YMAJRT(1)...SIZE FOR STATION MARKERS
C               YMAJRT(2)...ASPECT RATIO FOR STATION MARKERS 
         XMAJBT     = AMAX1(AINT((HILON-LOWLON)/4.),1.)   
         YMAJLFT(1) = AMAX1(AINT((HILAT-LOWLAT)/4.),1.)   
         NDECRT(1)  = 1
         NDECRT(3) = AXSFONT
         YMAJRT(1) = .02
         YMAJRT(2) = 1.
      ELSE IF (IGRAPH.EQ.4) THEN
C
C          .. WINDROSE
C               NCHRBT......NUMBER OF WIND SPEED CATEGORIES
C               NDECLF(1)...NUMBER OF SPEED INTERVALS PER CATEGORY
C               NDECLF(2)...PERCENT VALUE FOR OUTER RING -- DEFAULT (-1) IS
C                           SET TO AUTO SCALE BY WINDROSE PROGRAM
C               NDECLF(3)...BEGINNING WIDTH OF A COMPASS SPIKE (DEGREES OF ARC)
C               NDECLF(4)...MAX NUMBER OF RINGS
C               NDECLF(5)...POSITION ON WINDROSE FOR RING LABELS
         NCHRBT    =  5
         NDECLF(1) = 10
         NDECLF(2) = -1
         NDECLF(3) =  3
         NDECLF(4) =  5
         NDECLF(5) =  1
      ENDIF
C
C       ** SET GRID CONTROLS
C
      XGRDCLR = 3
      XGRDTHK = 1
      YGRDCLR = 3
      YGRDTHK = 1
      IF (IGRAPH.EQ.2) THEN
C          .. MAP
C             XGRDTYP......TIC/GRID CONTROLS AND LINE PATTERN      
C             YGRDTYP(1)...MAP LINE PATTERN
C             YGRDTHK......MAP LINE WIDTH
         YGRDTHK =  3
         IXTYP   = -1
         IYTYP   =  3
      ELSE IF (OBSTYPE.EQ.'10D'.OR.OBSTYPE.EQ.'MLY') THEN
         IXTYP =  3
         IYTYP = -3
      ELSE
         IXTYP = 3
         IYTYP = 3
      ENDIF
      DO 65 I=1,MXNVAL
         XGRDTYP(I) = IXTYP
         YGRDTYP(I) = IYTYP
   65 CONTINUE
C
C       ** SET PALETTE AND BACKGROUND COLOR
C
      BKGNCLR = 0  
      PALETTE = 1
C
C       ** READ PALETTE DEFINITIONS
C      
      OPEN(51,FILE='O:\DATA\PALETTE.DEF',STATUS='OLD',FORM='FORMATTED'
     +       ,MODE='READ',IOSTAT=ICHK)
      IF (ICHK.NE.0) THEN
         CALL WRTMSG(3,44,12,1,1,' O:\DATA\PALETTE.DEF',20)
         CALL LOCATE(24,0,IERR)
         STOP 7
      ENDIF         
      READ(51,*) ((PALDEF(I,J),I=1,16),J=1,12)
      CLOSE(51)
C
C       **  SET GRAPH CONTROLS FOR EACH COLUMN:  TYPE,AXIS,THICKNESS,COLOR
C
      ICLR = 9
      DO 80 I = 1,MXNVAL      
         COLAXIS(I) = 0
         COLTHK(I) = 1
         ICLR = MAX0(MOD(ICLR+1,16),1)
         COL1CLR(I) = ICLR
         IF (IGRAPH.EQ.1) THEN
            COL2CLR(I) = ICLR
         ELSE
            COL2CLR(I) = MAX0(MOD(ICLR+1,16),1)
         ENDIF
         COLTYPE(I) = 1
   80 CONTINUE
C
C       .. EXCEPTIONS
      IF (IGRAPH.EQ.1) THEN   
C          .. COLTYPE FOR TIMESERIES:  PLOT TYPE/LINE PATTERN VALUES
C             PLOT TYPE -- 1=LINE  2=POINT  3=BAR  4=FILL  
         IPLPAT=1
         DO 82 I=1,MXNVAL  
            IPLTYP=1
            IF (OBSTYPE.EQ.'MLY') THEN
               IPLTYP = 3
            ELSE IF (OBSTYPE.EQ.'DLY' .AND. I.LE.NBRELEM) THEN
               IF ((GRAFELEM(I).GE.5.AND.GRAFELEM(I).LE.11).OR.
     +              GRAFELEM(I).EQ.18.OR.GRAFELEM(I).EQ.19 .OR.
     +             (GRAFELEM(I).GE.49.AND.GRAFELEM(I).LE.51)) THEN
                  IPLTYP = 3
               ELSE IF (GRAFELEM(I).GT.1000) THEN
                  IPLTYP = 2
               END IF
            END IF
            COLTYPE(I) = IPLTYP*10 + IPLPAT   
  82     CONTINUE   
      ELSE IF (IGRAPH.EQ.3) THEN
         PALETTE = 1
         COL1CLR(1) =  1
         COL1CLR(2) = 14
      ENDIF      
      IF (IGRAPH.EQ.2) THEN
C
C          ** SET CONTOUR AND MAPPING CONTROLS
C
C          .. MAP PROJECTION:  0=MILLER  1=SCREEN
         NDECRT(4) = 0
C
C          .. MPCODE VALUES:   -1=IGNORE OPTION  >-1=COLORS  
C             MPCODE POSITIONS:  1=COAST  2=RIVERS  3=BOUNDARIES
C                                4=LAKES  5=STATES
         MPCODE(1) =  14
         MPCODE(2) =  -1
         MPCODE(3) =  14
         MPCODE(4) =  -1
         MPCODE(5) =  14
C
C          .. CONTOUR LEVELS
C               CONINCR.....FLAG INDICATING METHOD OF CALCULATING
C                           CONTOUR LEVELS
C                            0=NUMBER AND VALUE FOR CONTOUR LEVELS SPECIFIED
C                           <0=NUMBER OF CONTOUR LEVELS SPECIFIED
C                           >0=VALUE FOR INCREMENT BETWEEN CONTOUR LEVELS
C               NCONLEV.....NUMBER OF USER SPECIFIED CONTOUR LEVELS PLOTTED
C               NDECRT(2)...NUMBER OF CONTOUR LEVEL VALUES ENTERED IN ARRAY
C               CONLEV......CONTOUR LEVEL VALUES
         DO 84 I=1,MXNVAL
            CONINCR(I) = -5.
   84    CONTINUE
         NCONLEV = 0
         NDECRT(2)=0.
         DO 85 I=1,MXCONLEV
            CONLEV(I)  = 0.            
   85    CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE GRAFMOD(NCOLOLD,NPLTOLD)
C
C   ROUTINE TO MODIFY THE DEFAULT GRAPH PARAMETER VALUES FOR AN OLD 
C   GRAPH DEFINITION -- TIMESERIES AND MAP ONLY
C   THE VALUES ARE STORED IN COMMON BLOCK GRAFVAR.
C      
C   --->  THIS ROUTINE IS ASSOCIATED WITH GRAFINIT <---    
C
$INCLUDE: 'GRFPARM.INC'
$INCLUDE: 'GRAFVAR.INC'
C
C       ** THE .GDF FILE WILL BE MODIFIED UNDER THE FOLLOWING CONDITIONS:
C             THE PLOT TYPE IS TIMESERIES OR MAP
C
C       ** NOTE:  THE NUMBER OF PLOT COLUMNS MAY INCREASE, DECREASE, OR
C                 STAY THE SAME
C
      IF (IGRAPH.GE.3) GO TO 100
C
C       ** DETERMINE THE PLOT WIDTH AND
C          THE NUMBER OF PLOTS IN A STANDARD DATA FRAME.
C
      IF (IGRAPH.EQ.1) THEN
         NPLTCOL = NUMCOL
      ELSE   
         NPLTCOL = NUMCOL-2
      ENDIF   
C      
      PLTWID = MIN0(PLTWID,NPLTCOL)
      IF (PLTWID.LT.NPLTCOL) THEN
         NUMPLT = NPLTCOL / PLTWID
         IF (NUMPLT*PLTWID.LT.NPLTCOL) THEN
            NUMPLT = NUMPLT + 1
         END IF
      ELSE
         NUMPLT = 1
      END IF
C
C       ** DEFINE THE MAXIMUM NUMBER OF PLOT CONTROL VARIABLES THAT
C          COULD BE NEEDED FOR THE CURRENT DATA AND THE NUMBER USED FOR
C          THE PREVIOUSLY DEFINED GRAPH DEFINITION
C
      IF (IGRAPH.EQ.2) THEN 
C          .. MAP      
         MXNVAL  = MIN0(NUMCOL,MXELEM) - 2       
         MXNVOLD = NCOLOLD - 2
      ELSE   
C          .. TIMESERIES      
         MXNVAL  = MIN0(NUMCOL,MXELEM)        
         MXNVOLD = NCOLOLD 
      ENDIF
C
C       ** SET TITLES, DATA LIMITS, LEGEND FLAG, AXIS SCALES
C
      IF (IGRAPH.EQ.1) THEN
         LOWCOL = 1
         HICOL  = NUMCOL
         DO 50 I=NPLTOLD+1,MXNVAL
            LFTSCALE(1,I) = -99999.
            LFTSCALE(2,I) = -99999.
             RTSCALE(1,I) = -99999.
             RTSCALE(2,I) = -99999.
   50    CONTINUE      
      ELSE
         LOWCOL = 3
         HICOL  = NUMCOL
C          .. RESET MAP SCALES IN CASE LAT/LON LIMITS HAVE CHANGED 
         LFTSCALE(1,1) = LOWLAT      
         LFTSCALE(2,1) =  HILAT      
          BTSCALE(1)   = LOWLON      
          BTSCALE(2)   =  HILON      
      ENDIF    
C
C       ** SET TEXT VALUES
C
      DO 55 I=NPLTOLD+1,MXNVAL
         LFTTXT(I) = ' '
          RTTXT(I) = ' '
   55 CONTINUE             
C        
C       ** LABEL AND TIC CONTROL.
C
      IF (IGRAPH.EQ.1) THEN
C
C          .. FOR TIMESERIES
C                 XMAJ/YMAJ   >=0  USE SPECIFIED VALUE FOR TIC INCREMENT
C                              <0  VALUE IS NUMBER OF DESIRED TIC DIVISIONS
         DO 60 I=NPLTOLD+1,MXNVAL
             NDECLF(I)  = 1
             NDECRT(I)  = 1
            YMAJLFT(I)  = -4.
             YMAJRT(I)  = -4.
            YMINLFT(I)  = 1
             YMINRT(I)  = 1
   60    CONTINUE
      ENDIF
C
C       ** SET GRID CONTROLS
C
      DO 65 I=NPLTOLD+1,MXNVAL
         XGRDTYP(I) = XGRDTYP(NPLTOLD)
         YGRDTYP(I) = YGRDTYP(NPLTOLD)
   65 CONTINUE
C
C       **  SET GRAPH CONTROLS FOR EACH COLUMN:  TYPE,AXIS,THICKNESS,COLOR
C
      ICLR = COL1CLR(MXNVOLD)
      DO 80 I = MXNVOLD+1,MXNVAL      
         COLAXIS(I) = 0
         COLTHK(I) = COLTHK(MXNVOLD)
         ICLR = MAX0(MOD(ICLR+1,16),1)
         COL1CLR(I) = ICLR
         IF (IGRAPH.EQ.1) THEN
            COL2CLR(I) = ICLR
         ELSE
            COL2CLR(I) = MAX0(MOD(ICLR+1,16),1)
         ENDIF
         COLTYPE(I) = 1
   80 CONTINUE
C
C       .. EXCEPTIONS
      IF (IGRAPH.EQ.1) THEN
C          .. COLTYPE FOR TIMESERIES:  PLOT TYPE/LINE PATTERN VALUES
C             PLOT TYPE -- 1=LINE  2=POINT  3=BAR  4=FILL  
         IPLPAT=1
         DO 82 I=MXNVOLD+1,MXNVAL  
            IPLTYP=1
            IF (OBSTYPE.EQ.'MLY') THEN
               IPLTYP = 3
            ELSE IF (OBSTYPE.EQ.'DLY' .AND. I.LE.NBRELEM) THEN
               IF ((GRAFELEM(I).GE.5.AND.GRAFELEM(I).LE.11).OR.
     +              GRAFELEM(I).EQ.18.OR.GRAFELEM(I).EQ.19 .OR.
     +             (GRAFELEM(I).GE.49.AND.GRAFELEM(I).LE.51)) THEN
                  IPLTYP = 3
               ELSE IF (GRAFELEM(I).GT.1000) THEN
                  IPLTYP = 2
               END IF
            END IF
            COLTYPE(I) = IPLTYP*10 + IPLPAT   
  82     CONTINUE   
      ENDIF
      IF (IGRAPH.EQ.2) THEN
C
C          ** SET CONTOUR CONTROLS
C
         DO 85 I=MXNVOLD+1,MXNVAL
            CONINCR(I) = -5.
   85    CONTINUE   
      ENDIF
C
  100 CONTINUE
C  
      RETURN
      END
