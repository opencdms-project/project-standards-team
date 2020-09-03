$STORAGE:2
      SUBROUTINE TIMESER(IOPT,TICLBX,YVAL,MXDATROW,NVAL,
     +                   I1NODAT,RTNCODE)
C
C       ** OBJECTIVE:  PLOT ANY DATA AS A LINE,BAR CHART,POINT, OR FILL. 
C
C       ** INPUT:
C             IOPT..............FLAG TO CONTROL GRAPH PROGRESSION
C                                  0=REDRAW CURRENT PLOT    
C                                  1=NEXT PLOT IN CURRENT FRAME  
C                                  2=NEW FRAME
C                                  4=REDRAW CURRENT SCREEN -- PRINT   
C             TICLBX............VALUES FOR X-AXIS TIC LABELS -- ONE SET/FRAME
C             YVAL..............Y-AXIS DATA VALUES FOR LINE PLOT/BAR CHART
C                               ONE SET FOR EACH ELEMENT PLOTTED/FRAME
C             MXDATROW..........NUMBER OF ROWS IN THE YVAL ARRAY
C             NVAL..............MAXIMUM NUMBER OF VALUES IN A SET
C       ** OUTPUT:
C             I1NODAT...........NO DATA FLAG FOR EACH COLUMN OF CURRENT PLOT
C                               0=DATA  1=NODATA      ARRAY IS PASSED TO SAVE
C                               SPACE; ARRAY VALUES ARE ONLY USED LOCALLY
C             RTNCODE...........ERROR FLAG    '0'=NO ERROR 
C                                             '1'=EXIT USING ESC OR F4
C                                             '2'=NO MORE PLOTS IN FRAME
C                                             '5'=NO DATA FOR THIS PLOT 
C
$INCLUDE:  'GRFPARM.INC'
$INCLUDE:  'GRAFVAR.INC'
C
$INCLUDE:  'CURRPLT.INC'
C
C       ** ARGUMENTS:
      REAL*4 YVAL(MXDATROW,*)
      INTEGER*2 IOPT
      INTEGER*1 I1NODAT(*)
      CHARACTER *(*) TICLBX(*)
      CHARACTER *1 RTNCODE
C
C       ** COMMON TO REMOVE LOCAL VARIABLES FROM D-GROUP
C
C       .. GRFPARM.INC REQUIRED FOR PARAMETERS MXELEM AND LENTXTD
      INTEGER*2 XORGFLG,YORGFLG 
      REAL*4 XYPEN(2,MXELEM),RSCLPLT(2),LSCLPLT(2),BSCLPLT(2),YSCLPLT(2)
     +      ,LMAJPLT,RMAJPLT,BMAJPLT
      LOGICAL NEWPLT,PLTLAX,PLTRAX,PLTBAX,PLTHGRD,PLTVGRD,BGNBOX
      CHARACTER *(LENTXTD) TTLTXT
      CHARACTER*1 YAXSID
C
      COMMON /TIMESV/ XORGFLG,YORGFLG,
     +                XYPEN,RSCLPLT,LSCLPLT,BSCLPLT,YSCLPLT,
     +                LMAJPLT,RMAJPLT,BMAJPLT,
     +                NEWPLT,PLTLAX,PLTRAX,PLTBAX,PLTHGRD,PLTVGRD,BGNBOX
     +                TTLTXT,YAXSID
C      
      INTEGER*1 I1ON,I1DUM,I1DXFIL(MXELEM)
      DATA I1ON/1/
C
C       ** OPTION 4 (PRINT) IS CURRENTLY TREATED LIKE OPTION 0 (REDRAW
C          CURRENT PLOT)
C
      IF (IOPT.EQ.4) THEN
         ITSOPT=0
      ELSE
         ITSOPT=IOPT
      ENDIF            
C                
C
C       ** INITIAL VARIABLES FOR CURRENT GRAPH OPTION
C
      RTNCODE = '0'
      FRAMWID = MIN0(HICOL,NUMCOL)
      IF (ITSOPT.EQ.2) THEN
C      
C          .. NEW FRAME      
         NCA = 0
         NCB = MAX0(LOWCOL-1,0)
         IDPLT = 0
      ENDIF
      IF (ITSOPT.GT.0) THEN
C      
C          .. NEW FRAME OR NEXT PLOT IN CURRENT FRAME    
         IF (NCB+1 .GT. MIN0(NCB+PLTWID,FRAMWID)) GO TO 905
         NCA = NCB + 1
         NCB = MIN0(NCB+PLTWID,FRAMWID)
         IDPLT = IDPLT + 1
      ENDIF   
C
C          .. DETERMINE NUMBER OF DATASETS ON BAR CHART         
      MAXBAR = 0
      DO 10 NC=NCA,NCB
         IPLTYP = INT(COLTYPE(NC)/10)
         IF (IPLTYP.EQ.3) MAXBAR=MAXBAR+1
   10 CONTINUE      
      IDBAR = 0
C
C       ** CALCULATE SCALING IF AUTOSCALE FLAG IS ON
C
      NELMPLT = NCB-NCA+1
C       .. LEFT AXIS      
      IDSCL = 0
      CALL AUTOSCL(YVAL(1,NCA),MXDATROW,NVAL,NELMPLT,COLAXIS(NCA),IDSCL,
     +             LFTSCALE(1,IDPLT),YMAJLFT(IDPLT),NDECLF(IDPLT),
     +             LSCLPLT,LMAJPLT,NODATL,I1NODAT(NCA))
C       .. RIGHT AXIS     
      IDSCL = 1
      CALL AUTOSCL(YVAL(1,NCA),MXDATROW,NVAL,NELMPLT,COLAXIS(NCA),IDSCL,
     +             RTSCALE(1,IDPLT),YMAJRT(IDPLT),NDECRT(IDPLT),
     +             RSCLPLT,RMAJPLT,NODATR,I1NODAT(NCA))
C       .. BOTTOM AXIS--SPECIAL CASE IF ANY BAR CHARTS ARE USED
      IDSCL = 2
      IF (MAXBAR.GT.0) IDSCL=3
      CALL AUTOSCL(YVAL(1,NCA),MXDATROW,NVAL,NELMPLT,COLAXIS(NCA),IDSCL,
     +             BTSCALE(1),XMAJBT,0,BSCLPLT,BMAJPLT,IDUM1,I1DUM)
      IF (NODATL.EQ.1 .AND. NODATR.EQ.1) GO TO 910
C
C       ** INITIAL FLAGS
C
      NEWPLT  = .TRUE.      
      PLTLAX  = .TRUE.      
      PLTRAX  = .TRUE.      
      PLTBAX  = .TRUE.      
      PLTHGRD = .TRUE.
      PLTVGRD = .TRUE.
      BGNBOX  = .TRUE.
      FILFLG  = 0
C
C       ** REPEAT FOR EACH ELEMENT IN CURRENT PLOT
C      
      DO 100 NCI=NCA,NCB
C
C           .. ELEMENTS ARE ALWAYS PLOTTED IN THE ORDER THEY APPEAR IN THE
C              GRAPHICS.API FILE EXCEPT WHEN A FILL PLOT IS REQUESTED.  FOR
C              FILL PLOTS, ELEMENTS ARE PLOTTED IN ORDER OF THEIR MAX VALUE
C              FROM LOWEST TO HIGHEST.  THIS IS NECESSARY BECAUSE THE PLOT
C              WILL NOT FILL PROPERLY IF IT IS OVERWRITING A HATCH PATTERN.
C              ON THE FIRST PASS (NCI=NCA) THE ELEMENT INDEX IS ALWAYS SET TO
C              THE FIRST ELEMENT IN THE GROUP.  THIS VALUE WILL BE RESET IF
C              THE PLOT IS A VALID FILL PLOT.
         IF (NCI.EQ.NCA .OR. FILFLG.EQ.0) THEN
            NC = NCI
         ELSE 
            NC = I1DXFIL(NCI) + NCA - 1
         ENDIF      
C
C          .. EXIT PLOT IF ECS OR F4 ENTERED     
         CALL ESCQUIT(*900)
         IF (COLAXIS(NC).EQ.0) THEN
C
C             .. Y-AXIS ON THE LEFT         
            YAXSID     = 'L'
            YORGFLG    = 1
            XORGFLG    = 1
            XORIGIN    = BSCLPLT(1)
            YORIGIN    = LSCLPLT(1)
            YSCLPLT(1) = LSCLPLT(1)
            YSCLPLT(2) = LSCLPLT(2)
            YMAJOR     = LMAJPLT
         ELSE 
C
C             .. Y-AXIS ON THE RIGHT -- NOT ALLOWED IN FILL PLOTS
            YAXSID     = 'R'
            YORGFLG    = 1
            XORGFLG    = 0
            XORIGIN    =  BSCLPLT(2)
            YORIGIN    =  RSCLPLT(1)
            YSCLPLT(1) = RSCLPLT(1)
            YSCLPLT(2) = RSCLPLT(2)
            YMAJOR     = -RMAJPLT
         ENDIF
         CALL BGNPLT(GANWLF,GANWRT,GANWBT,GANWTP,
     1               BSCLPLT,YSCLPLT,XORIGIN,YORIGIN)
C         
         IF (NEWPLT) THEN
            NEWPLT=.FALSE.
C
C             .. CHECK IF FILL_PLOT DATA FOLLOWS THE RULES (MISSING DATA, 
C                INTERSECTING POINTS, ETC.) -- GET SEED POINTS
            NFILELM = NCB-NCA+1
            CALL CHKFDAT(YVAL(1,NCA),MXDATROW,NVAL,COLTYPE(NCA),
     +             XYPEN(1,NCA),NFILELM,AXSTHK,FILFLG,I1DXFIL(NCA))
C
C             .. REDEFINE ELEMENT INDEX FOR FILL PLOT.  THIS WILL NOT AFFECT
C                RESULTS BECAUSE THE INDEX HAS ONLY BEEN USED TO SELECT LEFT/
C                RIGHT AXIS.  ALL FILL ELEMENTS MUST BE ON THE LEFT AXIS.   
            IF (FILFLG.GT.0) THEN
               NC = I1DXFIL(NCI) + NCA - 1
            ENDIF      
         ENDIF      
C         
C          .. PLOT AXES, LABELS, ETC. ONLY IF THERE IS DATA FOR THIS ELEMENT
         IF (I1NODAT(NC).EQ.I1ON) GO TO 100
C
C          .. DRAW A BORDER AROUND PLOT AREA            
         IF (BGNBOX) THEN
            BGNBOX = .FALSE.
            CALL PLTBOX(AXSCLR,AXSTHK)
         ENDIF
C
C          .. EXIT PLOT IF ECS OR F4 ENTERED     
         CALL ESCQUIT(*900)
C
C          ** X-AXIS **
C
         IF (FILFLG.EQ.0 .AND. PLTBAX) THEN
C
C             .. DRAW X-AXIS AND TIC MARKS; NO TICS IF YGRDTYP (VERTICAL 
C                GRID) EQUALS ZERO   
            PLTBAX = .FALSE.
            LABEL = 1
            IF (YGRDTYP(IDPLT).EQ.0) BMAJPLT=0
            CALL DRWXAX(NVAL,TICLBX,NCHRBT,BMAJPLT,XMINBT,XORGFLG,LABEL
     +                 ,TICSIZE,ATXTSIZE,AXSFONT,ATXTASP,AXSCLR,AXSTHK)
         ENDIF
C
C          ** Y-AXIS
C
C          .. NO TICS IF XGRDTYP (HORIZONTAL GRID) EQUALS ZERO
         IF (XGRDTYP(IDPLT).EQ.0)  YMAJOR=0
C        
         IF (FILFLG.EQ.0) THEN 
            IF (PLTLAX .AND. YAXSID.EQ.'L') THEN
               PLTLAX = .FALSE.
C
C                .. DRAW Y-AXIS AND TIC MARKS ON THE LEFT        
               LABEL = -1
               CALL DRWYAX(YMAJOR,YMINLFT(IDPLT),YORGFLG,LABEL,
     +                     NDECLF(IDPLT),TICSIZE,ATXTSIZE,AXSFONT,
     +                     ATXTASP,AXSCLR,AXSTHK)
C
C                .. DRAW LEFT MARGIN TEXT -- TICS MUST BE DRAWN FIRST SINCE
C                   THE WIDTH OF THE LABELS IS USED TO DETERMINE THE DEFAULT
C                   X POSITION OF TEXT
               IF (LFTTXT(IDPLT).EQ.' ') THEN
                  TTLTXT=COLHDR(NC)
               ELSE
                  TTLTXT=LFTTXT(IDPLT)
               ENDIF  
               ISIDE = 4
               CALL PLTYMGN(TTLTXT,ISIDE,LTXTLOC
     +                     ,LTXTFONT,LTXTSIZE,LTXTASP,LTXTCLR)
            ELSE IF (PLTRAX .AND. YAXSID.EQ.'R') THEN
               PLTRAX = .FALSE.
C
C                .. DRAW Y-AXIS AND TIC MARKS ON THE RIGHT        
               LABEL = -1
               CALL DRWYAX(YMAJOR,YMINRT(IDPLT),YORGFLG,LABEL,
     +                     NDECRT(IDPLT),TICSIZE,ATXTSIZE,AXSFONT,
     +                     ATXTASP,AXSCLR,AXSTHK)
C
C                .. DRAW RIGHT MARGIN TEXT
               IF (RTTXT(IDPLT).EQ.' ') THEN
                  TTLTXT=COLHDR(NC)
               ELSE
                  TTLTXT=RTTXT(IDPLT)
               ENDIF  
               ISIDE = 2
               CALL PLTYMGN(TTLTXT,ISIDE,RTXTLOC
     +                     ,RTXTFONT,RTXTSIZE,RTXTASP,RTXTCLR)
            ENDIF
         ENDIF   
         
         IF (FILFLG.EQ.0 .AND. PLTBAX) THEN
C
C             .. DRAW X-AXIS AND TIC MARKS; NO TICS IF YGRDTYP (VERTICAL 
C                GRID) EQUALS ZERO   
            PLTBAX = .FALSE.
            LABEL = 1
            IF (YGRDTYP(IDPLT).EQ.0) BMAJPLT=0
            CALL DRWXAX(NVAL,TICLBX,NCHRBT,BMAJPLT,XMINBT,XORGFLG,LABEL
     +                 ,TICSIZE,ATXTSIZE,AXSFONT,ATXTASP,AXSCLR,AXSTHK)
         ENDIF
C
C       ** DRAW GRID LINES -- IF X/YGRDTYP EQUALS 0, NO GRID IS DRAWN;
C          GRID LINES WILL BE DRAWN BEFORE ANY DATA IS PLOTTED IF THE FILL
C          PLOT OPTION HAS NOT BEEN SELECTED.  IF FILL HAS BEEN SELECTED, THEN
C          GRID LINES MUST BE DRAWN AFTER LAST CALL TO FILPLT.  
C
         IF (FILFLG.EQ.0) THEN
            IF (PLTVGRD) THEN
C         
C                .. DRAW VERTICAL GRID (YGRD)
               PLTVGRD=.FALSE. 
               CALL DRWYGRD(YGRDCLR,YGRDTYP(IDPLT),YGRDTHK,TICSIZE)
            ENDIF   
            IF (PLTHGRD) THEN
C
C                .. DRAW HORIZONTAL GRID (XGRD)
               PLTHGRD=.FALSE.
               CALL DRWXGRD(XGRDCLR,XGRDTYP(IDPLT),XGRDTHK,YAXSID,
     +                      TICSIZE)
            ENDIF
         ENDIF      
C
C          .. EXIT PLOT IF ECS OR F4 ENTERED     
         CALL ESCQUIT(*900)
C
         IPLTYP = INT(COLTYPE(NC)/10)
         IPLPAT = COLTYPE(NC)-IPLTYP*10
         IF (IPLTYP.EQ.1) THEN
C
C             .. LINE PLOT         
            CALL LINPLT(YVAL(1,NC),NVAL,COL1CLR(NC),COLTHK(NC)
     +                 ,IPLPAT)
C
C             ..REDRAW A BORDER AROUND PLOT AREA            
            CALL PLTBOX(AXSCLR,AXSTHK)
         ELSE IF (IPLTYP.EQ.2) THEN
C
C             .. POINT PLOT         
            CALL PNTPLT(YVAL(1,NC),NVAL,COL1CLR(NC))
         ELSE IF (IPLTYP.EQ.3) THEN
C
C             .. BARCHART         
            ICENT=0
            IDBAR=IDBAR+1
            CALL HBARCHT(NVAL,MAXBAR,IDBAR,COL1CLR(NC),COL2CLR(NC)
     +                  ,YVAL(1,NC),BSCLPLT,YSCLPLT,ICENT,IPLPAT) 
C
C             ..REDRAW A BORDER AROUND PLOT AREA            
            CALL PLTBOX(AXSCLR,AXSTHK)
         ELSE IF (IPLTYP.EQ.4) THEN
C
C             .. COLOR FILL PLOT         
            CALL FILPLT(YVAL(1,NC),NVAL,XYPEN(1,NC),COL1CLR(NC),
     +                  FILFLG,IPLPAT)
         ENDIF                
C         
         IF (FILFLG.EQ.(NCI-NCA+1)) FILFLG=0
         IF (FILFLG.EQ.0) THEN
            IF (PLTBAX) THEN
C
C                .. DRAW X-AXIS AND TIC MARKS; NO TICS IF YGRDTYP (VERTICAL 
C                   GRID) EQUALS ZERO   
               PLTBAX = .FALSE.
               LABEL = 1
               IF (YGRDTYP(IDPLT).EQ.0) BMAJPLT=0
               CALL DRWXAX(NVAL,TICLBX,NCHRBT,BMAJPLT,XMINBT,XORGFLG
     +            ,LABEL,TICSIZE,ATXTSIZE,AXSFONT,ATXTASP,AXSCLR,AXSTHK)
            ENDIF
            IF (PLTLAX .AND. YAXSID.EQ.'L') THEN
               PLTLAX = .FALSE.
C
C                .. DRAW Y-AXIS AND TIC MARKS ON THE LEFT        
               LABEL = -1
               CALL DRWYAX(YMAJOR,YMINLFT(IDPLT),YORGFLG,LABEL,
     +                     NDECLF(IDPLT),TICSIZE,ATXTSIZE,AXSFONT,
     +                     ATXTASP,AXSCLR,AXSTHK)
C
C                .. DRAW LEFT MARGIN TEXT -- TICS MUST BE DRAWN FIRST SINCE
C                   THE WIDTH OF THE LABELS IS USED TO DETERMINE THE DEFAULT
C                   X POSITION OF TEXT
               IF (LFTTXT(IDPLT).EQ.' ') THEN
                  TTLTXT=COLHDR(NC)
               ELSE
                  TTLTXT=LFTTXT(IDPLT)
               ENDIF  
               ISIDE = 4
               CALL PLTYMGN(TTLTXT,ISIDE,LTXTLOC
     +                     ,LTXTFONT,LTXTSIZE,LTXTASP,LTXTCLR)
            ENDIF
C
C             ** DRAW GRID LINES -- IF X/YGRDTYP EQUALS 0, NO GRID IS DRAWN;
C                IF FILL HAS BEEN SELECTED, THEN GRID LINES MUST BE DRAWN AFTER
C                LAST CALL TO FILPLT.  PROBLEM WITH FILL USING HIGH RESOLUTION 
C                PRINT IF LINES ARE DRAWN BEFORE THE FILL.
C
            IF (PLTVGRD) THEN
C         
C                .. DRAW VERTICAL GRID (YGRD)
               PLTVGRD=.FALSE. 
               CALL DRWYGRD(YGRDCLR,YGRDTYP(IDPLT),YGRDTHK,TICSIZE)
            ENDIF   
            IF (PLTHGRD) THEN
C
C                .. DRAW HORIZONTAL GRID (XGRD)
               PLTHGRD=.FALSE.
               CALL DRWXGRD(XGRDCLR,XGRDTYP(IDPLT),XGRDTHK,YAXSID,
     +                      TICSIZE)
            ENDIF
         ENDIF      
  100 CONTINUE         
C
C       ** DRAW PLOT TITLE AND SUBTITLE LINES
      IF (GRTITLE.EQ.' ') THEN
         TTLTXT = DATATITLE
      ELSE
         TTLTXT = GRTITLE
      ENDIF      
      NLTITL = 1
      CALL PLTTITL(TTLTXT,NLTITL,TLLOC,TLFONT,TLSIZE,TLASP,TLCLR)
      IF (GRSUBTITLE.EQ.' ') THEN
         TTLTXT = DATASUB
      ELSE
         TTLTXT = GRSUBTITLE
      ENDIF      
      NLTITL = 2
      CALL PLTTITL(TTLTXT,NLTITL,STLLOC,STLFONT,
     +             STLSIZE,STLASP,STLCLR)
C
C       ** DRAW BOTTOM MARGIN TEXT
      ISIDE=3 
      CALL PLTXMGN(BOTTXT,ISIDE,BTXTLOC
     +            ,BTXTFONT,BTXTSIZE,BTXTASP,BTXTCLR)
C
C       ** DRAW FREE TEXT -- TEXT VALUE MUST BE SPECIFIED
C
      NLTITL = 3
      CALL PLTTITL(FTXT,NLTITL,FTXTLOC,FTXTFONT,FTXTSIZE,FTXTASP,
     +             FTXTCLR)
C
C       ** DRAW LEGEND
C
      LGBRDR='Y'
      LGALIGN=0
      LGNTRY=NELMPLT
      I=0
      DO 105 NC=NCA,NCB
         I=I+1      
         LGCOLR(I)=COL1CLR(NC)
         LGTEXT(I)=COLHDR(NC)
         IPLTYP = INT(COLTYPE(NC)/10)
         IPLPAT = COLTYPE(NC)-IPLTYP*10
         IF (IPLTYP.EQ.1) THEN
C             .. LINE         
            LGLINBAR(I)='L'
            LGSTYL(I)=IPLPAT
            LGITMWID(I)=FLOAT(COLTHK(NC))
         ELSE IF (IPLTYP.EQ.2) THEN
C             .. POINT
            LGLINBAR(I)='*'
            LGSTYL(I)=1
C            CALL YSZNW2W(.01,XSZW,YSZW)
C            LGITMWID(I)=YSZW
            LGITMWID(I)=1.0
         ELSE
C             .. BAR OR FILL         
            LGLINBAR(I)='B'
            LGSTYL(I)=IPLPAT
            CALL YSZNW2W(.01,XSZW,YSZW)
            LGITMWID(I)=YSZW
         ENDIF 
  105 CONTINUE      
C
C          ..CALCULATE LEGEND LOCATION AND SAVE POSITION
      IF (LEGLOC(1).EQ.-99999.) THEN  
         XCW = BSCLPLT(1) + .75*(BSCLPLT(2)-BSCLPLT(1))
         YCW = YSCLPLT(1) + .90*(YSCLPLT(2)-YSCLPLT(1))
         CALL W2NW(XCW,YCW,LEGLOC(1),LEGLOC(2))
      ENDIF   
C   
      CALL DRWLGND(LEGEND,LGBRDR,LGNTRY,LGLINBAR,LGSTYL,LGITMWID,
     +             LGCOLR,LGTEXT,LEGCLR,LEGFONT,
     +             LEGSIZE,LEGLOC,LEGASP,LGALIGN)
C      
      RETURN
C
C       ** ERROR PROCESSING
C
  900 CONTINUE
C       .. PREMATURE EXIT USING ESC OR F4  
      RTNCODE = '1'
      GO TO 990
  905 CONTINUE  
C       .. ERROR:  NO MORE PLOTS IN THIS DATASET
      RTNCODE='2'
      GO TO 990
  910 CONTINUE  
C       .. ERROR:  NO DATA AVAILABLE FOR THIS PLOT
      RTNCODE='5'
  990 RETURN
C            
      END
      SUBROUTINE AUTOSCL(VAL,MXDATROW,NVAL,NELEM,COLAXIS,IDSCL,
     +            SCLVAL,MAJOR,NDEC,SCLPLT,MAJPLT,NODATAX,I1NODAT)
C
C       ** INPUT:
C             VAL........DATA VALUES FOR CURRENT PLOT
C             MXDATROW...NUMBER OF ROWS IN THE VAL ARRAY
C             NVAL.......MAXIMUM NUMBER OF VALUES IN A ROW
C             NELEM......NUMBER OF ELEMENTS(COLUMNS) IN CURRENT PLOT
C             COLAXIS....VERTICAL REFERENCE AXIS FOR PLOTTING EACH ELEMENT
C             IDSCL......CURRENT AXIS FOR WHICH SCALE IS CALCULATED
C                        0=LEFT   2=BOTTOM WITH NO BAR CHART  
C                        1=RIGHT  3=BOTTOM WITH BAR CHART
C             SCLVAL.....CONTROL VALUE FOR SCALING;  AUTO SCALE IF VALUE IS
C                        -99999., ELSE USE SPECIFIED MIN/MAX VALUES
C             MAJOR......CONTROL VALUE FOR MAJOR TIC MARKS
C                        <0=NUMBER OF MAJOR TIC DIVISIONS
C                         0=NO MAJOR TIC DIVISIONS
C                        >0=INCREMENT BETWEEN MAJOR TIC DIVISIONS
C             NDEC.......NUMBER OF DECIMAL POINTS USED FOR AXIS LABEL
C       ** OUTPUT:
C             SCLPLT.....AXIS SCALE VALUES  (1)=MIN   (2)=MAX
C             MAJPLT.....INCREMENT BETWEEN MAJOR TIC DIVISIONS
C             NODATAX......NO DATA FLAG -- DETERMINED FOR LEFT/RIGHT AXES
C                           0=NO DATA FOR THIS AXIS
C                           1=DATA AVAILABLE FOR THIS AXIS  
C             I1NODAT......NO DATA FLAG FOR EACH COLUMN OF CURRENT PLOT
C                           0=DATA  1=NODATA     
C
      INTEGER*1 I1NODAT(*),I1OFF,I1ON
      INTEGER*2 MXDATROW,NVAL,NELEM,COLAXIS(*),IDSCL,NDEC,NODATAX
      INTEGER*4 I4MAJ,I4FACD
      REAL *4 VAL(MXDATROW,*),SCLVAL(2),MAJOR,SCLPLT(2),MAJPLT
      DATA I1OFF/0/,I1ON/1/
C      
      IF (IDSCL.LT.2) THEN
         IF (IDSCL.EQ.0) THEN
            DO 9 N=1,NELEM
              I1NODAT(N)=I1ON
    9       CONTINUE  
         ENDIF  
C
C          .. DETERMINE IF DATA IS AVAILABLE FOR LEFT/RIGHT AXES      
         DO 15 N=1,NELEM
            IF (COLAXIS(N).EQ.IDSCL) THEN
              I1NODAT(N)=I1OFF
              DO 10 I=1,NVAL     
                 IF (VAL(I,N).NE.-99999.) GO TO 15
   10         CONTINUE
              I1NODAT(N)=I1ON
            ENDIF
   15    CONTINUE      
         NODATAX=0
         DO 16 N=1,NELEM
            IF (COLAXIS(N).EQ.IDSCL .AND. I1NODAT(N).EQ.I1OFF) GO TO 17
   16    CONTINUE      
         NODATAX=1
   17    CONTINUE      
      ENDIF
      IF (NODATAX.EQ.1) GO TO 100
C
      IF (SCLVAL(1).EQ. -99999.) THEN
         IF (IDSCL.GE.2) THEN
C
C             .. CALCULATE X-AXIS SCALE FOR PLOT
            SCLPLT(1) = 0.
            IF (IDSCL.EQ.2) THEN
C                .. WITHOUT BAR CHART            
               SCLPLT(2) = FLOAT(NVAL)
            ELSE        
C                .. WITH BAR CHART            
               SCLPLT(2) = FLOAT(NVAL+1)
            ENDIF   
         ELSE         
C
C             .. CALCULATE Y-AXIS SCALE FOR PLOT      
            VALMIN= 99999.
            VALMAX=-99999.
            DO 25 N=1,NELEM
               IF (COLAXIS(N).EQ.IDSCL) THEN
                 DO 20 I=1,NVAL     
                    IF (VAL(I,N).EQ.-99999.) GO TO 20
                    VALMAX = AMAX1(VALMAX,VAL(I,N))
                    VALMIN = AMIN1(VALMIN,VAL(I,N))
   20            CONTINUE
               ENDIF
   25       CONTINUE      
C             
            ABSMAX = AMAX1(ABS(VALMAX),ABS(VALMIN))
            FACLST = .001
            DO 30 I=1,4
               FAC = FACLST*10.
               IF (ABSMAX.LT.FAC) GO TO 35
               FACLST = FAC
   30       CONTINUE
   35       CONTINUE
            FAC = FACLST
            SCLPLT(1) = AINT(VALMIN/FAC) * FAC
            IF (VALMIN.LT.SCLPLT(1)) SCLPLT(1)= SCLPLT(1)-FAC      
            SCLPLT(2) = AINT(VALMAX/FAC) * FAC
            IF (VALMAX.GT.SCLPLT(2)) SCLPLT(2)= SCLPLT(2)+FAC      
            IF (ABS(SCLPLT(2)-SCLPLT(1)).LT.FAC)
     +                               SCLPLT(2)=SCLPLT(2)+FAC
         ENDIF   
      ELSE
C
C          .. USE SPECIFIED SCALE VALUE
         IF (IDSCL.GE.2) THEN
C             .. X-AXIS -- PREVENT ACCESSING LABELS (CVAL) OUT OF RANGE 
            SCLPLT(1) = 0.      
            SCLPLT(2) = MIN0(SCLVAL(2),FLOAT(MXDATROW))
         ELSE
            SCLPLT(1) = SCLVAL(1)      
            SCLPLT(2) = SCLVAL(2)      
         ENDIF
      ENDIF
C
C       ** CALCULATE INCREMENT FOR MAJOR TIC MARKS
C
      IF (MAJOR .LT. 0.) THEN
         FAC = 1.
         DO 40 I=1,NDEC
            FAC = 10.*FAC
   40    CONTINUE         
         ABSMAJ=AINT(ABS(MAJOR))
         FACDIF=FAC*(SCLPLT(2)-SCLPLT(1))
C         I4MAJ  = INT4(NINT(FACDIF/ABSMAJ))
C         I4FACD = INT4(NINT(FACDIF))
         CALL IROUND4((FACDIF/ABSMAJ),I4MAJ)
         CALL IROUND4(FACDIF,I4FACD)
         IF (I4MAJ*INT4(ABSMAJ) .LT. I4FACD) THEN
            I4MAJ=I4MAJ+1
         ENDIF   
         MAJPLT = FLOAT(I4MAJ)/FAC
      ELSE
         MAJPLT = MAJOR   
      ENDIF   
C
  100 RETURN
      END      
            SUBROUTINE CHKFDAT(YVAL,MXDATROW,NVAL,COLTYPE,XYPEN,NELEM,
     +                         IAXSTHK,FILFLG,I1DXFIL)
C
C       ** OBJECTIVE:  DETERMINE IF PLOT FILLING IS ALLOWED.
C                      FILLING IS ALLOWED UNDER THE FOLLOWING CONDITIONS
C                         NO MISSING DATA
C                         NO INTERSECTING DATA
C                         ALL DATA MUST FALL WITHIN THE PLOT RANGE
C                         DATA SETS FOR FILLING ARE CONSECUTIVE
C                         FILL DATA MUST PRECEDE DATA FOR ANY OTHER PLOT TYPE
C       ** NOTE:  THIS ROUTINE REQUIRES THAT ALL FILL DATA IS PLOTTED ON THE 
C                 LEFT AXIS. THE RIGHT AXIS SCALE VARIABLE IS USED TO HOLD
C                 THE PEN POSITION USED TO START FILLING.  LINE THICKNESS
C                 (LNTHK) SHOULD AGREE WITH THE VALUE USED IN FILPLT
C
C       ** INPUT:
C             YVAL........Y-AXIS DATA VALUES FOR PLOT.  ONE SET FOR
C                         EACH ELEMENT PLOTTED
C             MXDATROW....NUMBER OF ROWS IN THE YVAL ARRAY
C             NVAL........MAXIMUM NUMBER OF VALUES IN A SET
C             COLTYPE.....PLOT TYPE/PATTERN FOR EACH ELEMENT
C                         PLOT TYPE --  1=LINE  2=POINT  3=BAR  4=FILL
C             NELEM.......NUMBER OF ELEMENTS FOR CURRENT PLOT
C             IAXSTHK.....LINE THICKNESS OF AXIS
C       ** OUTPUT:
C             XYPEN(1)....X CURSOR POSITION TO START COLOR FILL
C                  (2)....Y CURSOR POSITION TO START COLOR FILL
C                         POSITIONS ARE IN WORLD COORDINATES
C             FILFLG......>0=FILL PLOT OK -- VALUE=NUMBER OF ELEMENTS TO FILL
C                          0=DO NOT FILL PLOT -- LINE PLOT ONLY
C             I1DXFIL.....INDEX FOR THE ORDER OF DRAWING FILL PLOTS
C
      INTEGER*2 NVAL,NELEM,COLTYPE(*),FILFLG
      INTEGER*1 I1DXFIL(*)
      REAL*4 YVAL(MXDATROW,*),XYPEN(2,*)
C
$INCLUDE:  'PLTSPEC.INC'
C
C
C       ** FILL DATA MUST PRECEDE DATA FOR ANY OTHER PLOT TYPE      
      FILFLG = 1
      IPLTYP = INT(COLTYPE(1)/10)
      IF (IPLTYP.NE.4) THEN
         FILFLG = 0
      ELSE
         NFIL=1
         IF (NELEM.GT.1) THEN
C
C             .. DETERMINE THE NUMBER OF CONSECUTIVE ELEMENTS FOR FILLING
            FILFLG=1
            DO 10 I=2,NELEM
               IPLTYP = INT(COLTYPE(I)/10)
               IF (IPLTYP.NE.4) GO TO 15
               NFIL=I
   10       CONTINUE
   15       CONTINUE
            IF (NFIL.LT.NELEM) THEN
C
C                .. CHECK FOR NON-CONSECUTIVE FILL DATA
               DO 20 I=NFIL+1,NELEM   
                  IPLTYP = INT(COLTYPE(I)/10)
                  IF (IPLTYP.EQ.4) FILFLG=0
   20          CONTINUE
            ENDIF
C
C             .. NO MORE CHECKS IF FILLING IS NOT ALLOWED
            IF (FILFLG.EQ.0) GO TO 100
         ENDIF
C
C             .. CALCULATE MINIMUM DATA SEPARATION AND THE MINIMUM DISTANCE
C                FROM X-AXIS IN WORLD COORDINATES
            CALL MAPWTD(XMAX,YMAX,IXD,IYD)
            LNTHK = 1
            IYDELTA = 2*LNTHK + 1 
            CALL MAPDTW(IXD,IYD+IYDELTA,XW,YW)
            YDELTA = ABS(YMAX-YW)
C            
            IYDELTA = 2*IAXSTHK + 1 
            CALL MAPDTW(IXD,IYD+IYDELTA,XW,YW)
            AXDELTA = ABS(YMAX-YW)
C            
C             .. CHECK FOR ELEMENT DATA WITH INTERSECTING POINTS 
            FILFLG = 0
            DO 55 J1=1,NFIL-1
               DO 50 J2=J1+1,NFIL
                  IF (YVAL(1,J1).LT.YVAL(1,J2)) THEN
                     DO 40 I=1,NVAL
                        IF(YVAL(I,J1)+YDELTA.GE.YVAL(I,J2)) GO TO 68
   40                CONTINUE
                  ELSE
                     DO 45 I=1,NVAL
                        IF(YVAL(I,J1)-YDELTA.LE.YVAL(I,J2)) GO TO 68
   45                CONTINUE
                  ENDIF  
   50          CONTINUE                                
   55       CONTINUE
C
C             .. CHECK THAT ALL DATA FALLS WITHIN THE X-AXIS RANGE
C
            IF (NVAL.GT.NINT(XMAX)) GO TO 68            
C
C             .. FIND THE MINIMUM DATA VALUE FOR ALL ELEMENTS.  FIND THE
C                MAXIMUM FOR EACH ELEMENT.  MAXIMUM MUST BE LESS THAN THE
C                MAXIMUM Y-VALUE AT THE AXIS.   
            YDATMN=YMAX
            DO 65 J1=1,NFIL
               XYPEN(2,J1)=YMIN
               DO 60 I=1,NVAL
                  YDATMN=AMIN1(YDATMN,YVAL(I,J1))
                  IF (YVAL(I,J1).GT.XYPEN(2,J1)) THEN
                     XYPEN(1,J1)=FLOAT(I)
                     XYPEN(2,J1)=YVAL(I,J1)
                  ENDIF
   60          CONTINUE
               IF (XYPEN(2,J1).GT.YMAX-AXDELTA) GO TO 68
   65       CONTINUE
C
C              .. CHECK THAT THE MINIMUM DATA VALUE IS GREATER THAN THE 
C                 MINIMUM Y-VALUE AT THE AXIS AND LESS THAN THE MAXIMUM
C                 Y-VALUE AT THE AXIS.  THE AXIS LINE THICKNESS MUST BE 
C                 TAKEN INTO ACCOUNT
            IF (YDATMN.GT.YMIN+AXDELTA .AND.
     +          YDATMN.LT.YMAX-AXDELTA) THEN
                FILFLG=1
            ENDIF    
C
   68       CONTINUE
            IF (FILFLG.EQ.0) GO TO 100         
C
C          .. CHECK FOR MISSING POINTS
         FILFLG=0
         DO 70 J=1,NFIL
         DO 70 I=1,NVAL
            IF (YVAL(I,J).EQ.-99999.) GO TO 75
   70    CONTINUE
C
C          .. NO MISSING POINTS   
         FILFLG = 1
   75    CONTINUE      
      ENDIF   
      IF (FILFLG.EQ.1) THEN
C
C          .. ARRANGE INDICIES FOR ORDER OF DRAWING FILL PLOTS -- LOW TO HIGH
         DO 78 J1=1,NFIL
            I1DXFIL(J1) = J1
   78    CONTINUE            
         DO 81 J1=1,NFIL-1
            I1 = I1DXFIL(J1)
            DO 80 J2=J1+1,NFIL
               I2 = I1DXFIL(J2)
               IF (XYPEN(2,I1).GT.XYPEN(2,I2)) THEN
                  I1DXFIL(J1) = I1DXFIL(J2)
                  I1DXFIL(J2) = I1
                  I1=I2
               ENDIF
   80       CONTINUE            
   81    CONTINUE            
C
C             .. FIND THE LOWER ADJACENT DATA SET -- IF IT EXISTS           
         NPTS = NVAL
         IF (NVAL.GE.NINT(XMAX)) NPTS=NPTS-1
         DO 90 KNT=1,NFIL
            J1 = I1DXFIL(KNT)
            IF (KNT.GT.1) THEN
               J2SAV = I1DXFIL(KNT-1)
            ELSE
               J2SAV = 0
            ENDIF      
            IF (J2SAV.GT.0) THEN
C
C                .. LOWER ADJACENT DATA SET FOUND.  Y_CURSOR POSITION WILL
C                   BE HALF THE MAXIMUM DISTANCE BETWEEN DATA SET POINTS
C                   MAX DISTANCE MAY NOT BE THE LAST VALUE IF IT IS ON THE
C                   AXIS
               YDIFSAV=0
               DO 84 I=1,NPTS
                  YDIF=YVAL(I,J1)-YVAL(I,J2SAV)
                  IF (YDIF.GT.YDIFSAV) THEN
                     ISAV=I
                     YDIFSAV=YDIF
                  ENDIF
   84          CONTINUE      
               XYPEN(1,J1)=FLOAT(ISAV)
               XYPEN(2,J1)=YVAL(ISAV,J1)-.5*YDIFSAV
            ELSE
C
C                .. NO LOWER DATA SET FOUND.  Y_CURSOR POSTION WILL BE HALF 
C                   THE DISTANCE BETWEEN THE MAX VALUE AND THE AXIS. 
C                   MAX VALUE MAY NOT BE THE LAST VALUE IF IT IS ON THE AXIS
               IF (NINT(XYPEN(1,J1)).EQ.NVAL .AND. 
     +                    NVAL.GE.NINT(XMAX)) THEN
                  YDATMX=YMIN
                  DO 86 I=1,NVAL-1
                     IF (YVAL(I,J1).GT.YDATMX) THEN
                        YDATMX = YVAL(I,J1)
                        ISAV   = I
                     ENDIF   
   86             CONTINUE   
                  XYPEN(1,J1)=FLOAT(ISAV)
               ELSE
                  YDATMX = XYPEN(2,J1)
               ENDIF      
               XYPEN(2,J1)=YDATMX-.5*(YDATMX-(YMIN+AXDELTA))
            ENDIF      
   90    CONTINUE   
      ENDIF   
C
  100 CONTINUE
      IF (FILFLG.GT.0) FILFLG=NFIL
      RETURN
      END                 
      SUBROUTINE HBARCHT(NBG,NBPG,INDXB,KLRFIL,KLRLIN,VALUES,XSCAL,
     +                   YSCAL,ICENT,IPLPAT)
C     
C       ** OBJECTIVE:  DRAW BAR CHART
C
C       ** INPUT:
C             NBG........NUMBER OF BAR GROUPS -- NUMBER OF ITEMS IN DATASET
C             NBPG.......NUMBER OF BARS PER BAR GROUP -- NUMBER OF DATA SETS
C             INDXB......INDEX TO POSITION OF CURRENT BAR IN THE GROUP
C             KLRFIL.....COLOR USED TO FILL THE INTERIOR OF THE BARS
C             KLRLIN.....COLOR USED FOR THE BORDER LINE AROUND THE BARS
C             VALUES.....VALUES TO BE PLOTTED
C             XSCAL......MINIMUM/MAXIMUM X-VALUES ALONG AXIS
C             YSCAL......MINIMUM/MAXIMUM Y-VALUES ALONG AXIS
C             ICENT......0=CENTER GROUP AT INDEX TO CURRENT BAR GROUP(INDXG)
C                        1=CENTER FIELD ON SCREEN -- CENTER GROUP IN SPACE 
C                          PREVIOUS TO INDXG 
C
C       ** ARGUMENTS
C
      REAL*4  VALUES(*),XSCAL(2),YSCAL(2)
      INTEGER*2 NBG,NBPG,INDXB,KLRFIL,KLRLIN,ICENT
C
C       ** LOCAL VARIABLES
      INTEGER *2 HORPIX,VERPIX      
C
C       ** DEFINE LINE ATTRIBUTES FOR BORDER OF BAR.    
C          DEFINE HATCH STYLE FOR BAR INTERIOR FILL.
C          
      LNTYP = 1
      LNTHK = 1
      CALL DEFHLN(KLRLIN,LNTYP,LNTHK)
C
C       ** DRAW LINE AT ZERO IF Y-AXIS HAS NEGATIVE VALUES
C
        IF (YSCAL(1).LT.0. .AND. YSCAL(2).GT.0.) THEN
           CALL MOVABS(XSCAL(1),0.)      
           CALL  LNABS(XSCAL(2),0.)      
        ENDIF   
C
C       **  DETERMINE THE WIDTH OF ONE BAR IN A GROUP
C
      IF (NBPG.GT.1) THEN
         BARWIDTH = 1.0 / FLOAT(NBPG + 2)
      ELSE
         BARWIDTH = 1.0 / FLOAT(NBPG + 1)
      ENDIF
C
C       ** CALCULATE THE SPACING IN WORLD COORDINATES BETWEEN BARS IN A GROUP
C
      CALL INQDRA(HORPIX,VERPIX)
      NPIX = MAX0(HORPIX/320,1)      
      CALL MAPDTW(0,0,XW1,YW1)
      CALL MAPDTW(NPIX,0,XW2,YW2)
      BARSP = XW2-XW1
C
C       ** CALCULATE THE OFFSET FOR POSITIONING THE FIRST BAR IN THE GROUP  
C            
      IF (NBPG.GT.1) THEN
         IF (ICENT.EQ.1) THEN
            OFFSET = BARWIDTH 
         ELSE
            OFFSET = BARWIDTH*FLOAT(NBPG+2) - .5*BARSP -
     +               (BARWIDTH*FLOAT(NBPG)/2.)
         END IF
      ELSE
         IF (ICENT.EQ.1) THEN
            OFFSET =  BARWIDTH/2.
         ELSE
            OFFSET =  BARWIDTH*FLOAT(NBPG+1) - BARWIDTH/2.
         END IF
      END IF
C
C       ** DETERMINE THE REFERENCE POINT FROM WHICH BARS WILL BE DRAWN
C
      YREF = AMAX1(YSCAL(1),0.)
            
C
C       ** DRAW THE BARS
C
C       .. REPEAT FOR EACH BAR GROUP -- DATA ITEM
      DO 100 INDXG=1,NBG
C
C          .. ELIMINATE MISSING DATA POINTS
         IF (VALUES(INDXG).EQ.-99999.) GO TO 100      
C
C          .. CALCULATE X-POSITION OF LEFT SIDE OF BAR         
         XW1 = FLOAT(INDXG-1)+OFFSET + FLOAT(INDXB-1)*(BARWIDTH+BARSP)
C
C          .. SET LEFT Y POSITION AND RIGHT X,Y POSITIONS OF BAR
         XW2 = XW1+BARWIDTH
         YW1 = YREF
         YW2 = VALUES(INDXG)
         IF (ABS(VALUES(INDXG)-YREF) .GT. 0.01) THEN
C
C             .. DRAW BAR AND BORDER AROUND EDGE            
            CALL SETCOL(KLRFIL)
            CALL SETHAT(IPLPAT)
            CALL BAR(XW1,YW1,XW2,YW2)
            CALL SETCOL(KLRLIN)
            CALL BOX(XW1,YW1,XW2,YW2)
         END IF
  100 CONTINUE
      CALL SETHAT(1)
      RETURN
      END
      SUBROUTINE LINPLT(VALY,NVAL,LNCLR,LNTHK,LNTYP)
C
C       ** OBJECTIVE:  DRAW AN X-Y LINE PLOT       
C
C       ** INPUT:
C             VALY.....ARRAY OF Y_VALUES
C             NVAL.....NUMBER OF POINTS TO PLOT
C             LNCLR....LINE COLOR ID NUMBER
C             LNTHK....LINE THICKNESS
C             LNTYP....LINE PATTERN 
C                         1=SOLID  2=DASH  3=DOT
C
      INTEGER*2 NVAL,LNCLR,LNTHK,LNTYP
      REAL *4 VALY(*)
      LOGICAL MOVCUR
C
C       ** DEFINE LINE ATTRIBUTES
C
      CALL DEFHLN(LNCLR,LNTYP,LNTHK)      
C
C       ** PLOT VALUES
C
      MOVCUR = .TRUE.
      DO 20 N=1,NVAL
         IF (VALY(N).EQ.-99999.) THEN
            MOVCUR = .TRUE.
         ELSE IF (MOVCUR) THEN
            MOVCUR=.FALSE.
            CALL MOVABS(FLOAT(N),VALY(N))
         ELSE
            CALL LNABS(FLOAT(N),VALY(N))
         ENDIF
   20 CONTINUE
C
      RETURN
      END   
      SUBROUTINE PNTPLT(VALY,NVAL,KLRCHR)
C
C       ** OBJECTIVE:  PLOT A CHARACTER AT THE GIVEN X-Y VALUES
C
C       ** INPUT:
C             VALY.....ARRAY OF Y_VALUES
C             NVAL.....NUMBER OF POINTS TO PLOT
C             KLRCHR...CHARACTER COLOR ID NUMBER
C
      REAL *4 VALY(*)
      CHARACTER *3 PNTCHR
      DATA PNTCHR/'&*&'/
      DATA CHRHTN/.03/
C
C       ** DEFINE CHARACTER ATTRIBUTES
C
      IDFONT = 2
      ANG = 0.
      TXTASP=1.
      CALL DEFHST(IDFONT,KLRCHR,ANG,TXTASP,CHRHTN,CHRHTW)
      CALL SETSTA(ANG)
      CALL INQSTS(PNTCHR,HEIGHT,WIDTH,OFFSET)
      OFFHT = .75*HEIGHT
      OFFWD = .5*WIDTH
C
C       ** PLOT VALUES
C
      DO 20 N=1,NVAL
         IF (VALY(N).EQ.-99999.) GO TO 20
         CALL MOVTCA(FLOAT(N)-OFFWD,VALY(N)-OFFHT)
         CALL STEXT(PNTCHR)
   20 CONTINUE
C
      RETURN
      END   
      SUBROUTINE FILPLT(VALY,NVAL,XYPEN,KLRFIL,FILFLG,IPLPAT)
C
C       ** OBJECTIVE:  DRAW AN X-Y LINE PLOT.  FILL AREA BELOW LINE
C                      WITH THE SPECIFIED COLOR       
C
C       ** INPUT:
C             VALY........ARRAY OF Y_VALUES
C             NVAL........NUMBER OF POINTS TO PLOT
C             XYPEN(1)....X CURSOR POSITION TO START COLOR FILL
C                  (2)....Y CURSOR POSITION TO START COLOR FILL
C                         POSITIONS ARE IN WORLD COORDINATES
C             KLRFIL......FILL COLOR ID NUMBER
C             FILFLG...... 0=DO NOT FILL PLOT AREA
C                         >0=COLOR FILL PLOT AREA
C
      REAL *4 VALY(*),XYPEN(2)
      INTEGER*2 FILFLG
C
$INCLUDE:  'PLTSPEC.INC'
C
      LOGICAL MOVCUR      
C
C       ** DEFINE LINE ATTRIBUTES
C
      LNTYP = 1
      LNTHK = 1
      CALL DEFHLN(KLRFIL,LNTYP,LNTHK)
      CALL SETHAT(IPLPAT)
C
C       ** DRAW LINE PLOT.  EXTEND LINE TO AXIS ON BOTH SIDES.
C      
      DO 15 N=1,NVAL
         IF (VALY(N).NE.-99999.) GO TO 16
   15 CONTINUE      
C       .. NO DATA FOUND
      GO TO 100
   16 CONTINUE 
      NBEG = N     
      CALL MOVABS(XMIN,VALY(NBEG))
      IF (FILFLG.GT.0) THEN
         MOVCUR = .FALSE.
      ELSE
         MOVCUR = .TRUE.
      ENDIF
      DO 20 N=NBEG,NVAL
         IF (VALY(N).EQ.-99999.) THEN
            MOVCUR = .TRUE.
         ELSE 
            NLAST = N
            IF (MOVCUR) THEN
               MOVCUR=.FALSE.
               CALL MOVABS(FLOAT(N),VALY(N))
            ELSE
               CALL LNABS(FLOAT(N),VALY(N))
            ENDIF
         ENDIF
   20 CONTINUE
      IF (NLAST.LT.NINT(XMAX) .AND. FILFLG.GT.0) THEN
         CALL LNABS(XMAX,VALY(NLAST))
      ENDIF   
C
C       ** COLOR FILL PLOT BELOW LINE       
C
      IF (FILFLG.GT.0) THEN
         CALL MOVABS(XYPEN(1),XYPEN(2))
         CALL DELTCU
         CALL DELHCU
         CALL FLOOD(KLRFIL)
      ENDIF   
      CALL SETHAT(1)
C
  100 CONTINUE
      RETURN
      END   
