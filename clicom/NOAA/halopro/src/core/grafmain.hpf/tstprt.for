$STORAGE:2
      PROGRAM PRTPLT
C
C     ** OBJECTIVE:  OUTPUT PLOT USING A SCREEN DUMP OR HI-RESOLUTION PRINT
C                    TYPE OF OUTPUT IS DETERMINED BY FLAG (OPTCHR) IN FILE
C                    DATACOM.CON
C
$INCLUDE: 'GRFPARM.INC'
$INCLUDE: 'GRAFVAR.INC'
$INCLUDE: 'DATAVAL.INC'
$INCLUDE: 'FRMPOS.INC'
$INCLUDE: 'CURRPLT.INC'
$INCLUDE: 'HALOENV.INC'
C
      INTEGER*2 NOKD,LENMSG
      PARAMETER(NOKD=2)
      CHARACTER*2 INCHAR,OPTCHR,EXITOPT,OKOPT(NOKD)
      CHARACTER*1 TTLSAV(2),RTNCODE
      CHARACTER*14 MSGTXT
C **DEBUG
      CHARACTER*30 KRNLPATH
C **END DEBUG      
      DATA NTTL/1/
C       .. VALID OPTION FLAGS IN FILE DATACOM.CON
      DATA OKOPT/'3 ','4 '/
C       .. OPTION FLAG WRITTEN TO DATACOM.CON WHEN EXITING TO GRAFMAN
      DATA EXITOPT/'GO'/      
C
C **DEBUG
C      DATA KRNLPATH/'^C:\CLICOM\HALO\KERNELS^'/
C      CALL SETKER(KRNLPATH)
C      CALL CKHALOER(1,'SETKER',IER)
C      IF (IER.NE.0) GO TO 995
       OPEN(999,FILE='TST2')
C **END DEBUG      
C
C       **  Open and read GRAPHICS.GDF file and store the values in the
C           GRAFVAR common block.
C
      CALL RDGRAF('GRAPHICS',ITYPE,NELEM,RTNCODE)
C
C       ** INITIAL GRAPHICS
C
      IF (RTNCODE.EQ.'0') THEN
C          .. NORMAL RETURN FROM RDGRAF -- DEFINE PALETTES
         CALL BGNHALO(1,PALETTE,PALDEF)
      ELSE
C          .. ERROR RETURN FROM RDGRAF -- USE DEFAULT PALETTES
         CALL BGNHALO(0,PALETTE,PALDEF)
         GO TO 900
      ENDIF      
C
C       ** OPEN AND READ DATACOM.CON FILE AND STORE CONSTANTS IN THE
C          DATAVAL COMMON BLOCK; CLOSE FILE
C
      CALL RDDCON(1,OKOPT,NOKD,OPTCHR,RTNCODE)
      IF (RTNCODE.EQ.'2') THEN
C          .. ERROR IN OPENING FILE      
         GO TO 905        
      ELSE IF (RTNCODE.EQ.'3') THEN
C          .. INVALID OPTION CHARACTER      
         GO TO 920
      ENDIF
      MXDATROW = NROWDIM
C
C       ** Open the GRAPHICS.API file as unit 17 and read the CURRENT DATASET
C          into memory.
C
      IDATAOPT=0
      CALL GETDSET(ITYPSET,IDATAOPT,2,NTTL,TTLSAV,INCSET,RTNCODE)
      IF (RTNCODE.NE.'0') GO TO 900
C      
C       ** INITIAL DRIVER AND SETTINGS FOR THE PRINTER
      IDPTR = ACTVPTR+1
      IDCLR = CLRMOD(IDPTR)+1
      CALL SETACTPR(PRINTR(IDCLR,IDPTR),PTRVAL(0,IDPTR),
     +              PTRASP(IDPTR),IER)
      IF (IER.NE.0) THEN
         GO TO 930
      ENDIF      
C
      IF (OPTCHR.EQ.'3 ') THEN
C          .. SCREEN DUMP      
         CALL DRAWGRF(3,RTNCODE)
         CALL SNAPST  
      ELSE IF (OPTCHR.EQ.'4 ') THEN
C          .. BETTER RESOLUTION PRINT ( THRU THE VIRTUAL RASTER INTERFACE)
         CALL GRFPRNT
      END IF
      CALL GETHAL(HALOID)
      IF (HALOID .GE. 'B' .AND. HALOID .LE. 'Z') THEN
C          .. NETWORK=1 MEANS IBM PC LAN, SEND END-OF-SPOOL CHAR 
C             TO PRINT NOW
         IF (NETWORK .EQ. 1) THEN
            CALL SPOOLEND
         ELSE
            CALL GRAFNOTE(0.3,0.8,454,202,' ',0,INCHAR)
         ENDIF
      ENDIF
C    
      CALL WRFILPOS(-1,IGRAPH,NUMCOL,IDUM)
      CALL WRTDCON(1,1,EXITOPT,RTNCODE)
      IF (RTNCODE.NE.'0') GO TO 910        
      CALL FINHALO
C          .. EXIT GRAPHICS; NORMAL RETURN
      CALL LOCATE(23,0,IERR)
      STOP 1
C
C       ** FATAL ERROR      
C
  900 CONTINUE
C          .. ERROR READING FILE: GRAPHICS.GDF  
         MSGN1=191
         MSGTXT='  GRAPHICS.GDF'
         LENMSG=14
         GO TO 990
  905 CONTINUE
C          .. ERROR READING FILE: DATACOM.CON  
         MSGN1=191
         MSGTXT='  DATACOM.CON'
         LENMSG=13
         GO TO 990
  910 CONTINUE
C          .. ERROR WRITING FILE: DATACOM.CON  
         MSGN1=192
         MSGTXT='  DATACOM.CON'
         LENMSG=13
         GO TO 990
  915 CONTINUE
C          .. ERROR READING FILE: GRAPHICS.API  
         MSGN1=191
         MSGTXT='  GRAPHICS.API'
         LENMSG=14
         GO TO 990
  920 CONTINUE
C          .. ILLEGAL OPTIONS CHARACTER  
         MSGN1=551
         MSGTXT=' '
         LENMSG=0
         GO TO 990
  930 CONTINUE
C          .. ERROR INITIALIZING PRINTER
         MSGN1=193
         MSGTXT=' '
         LENMSG=0
         GO TO 990
  990 CONTINUE         
         MSGN2=202
         XWIN=.1
         YWIN=.95
         CALL GRAFNOTE(XWIN,YWIN,MSGN1,MSGN2,MSGTXT,LENMSG,INCHAR)
  995 CONTINUE         
         CALL WRFILPOS(-1,IGRAPH,NUMCOL,IDUM)
         INCHAR = EXITOPT
         CALL WRTDCON(1,0,INCHAR,RTNCODE)
         IF (RTNCODE.NE.'0') THEN
            MSGN1=192
            MSGTXT='  DATACOM.CON'
            LENMSG=13
            CALL GRAFNOTE(XWIN,YWIN,MSGN1,MSGN2,MSGTXT,LENMSG,INCHAR)
         ENDIF            
         CALL FINHALO
         OPEN (UNIT=62,FILE='O:\DATA\SQUX',ACCESS='DIRECT',
     +         FORM='BINARY',RECL=512,STATUS='OLD',IOSTAT=IOCHK)
         IF (IOCHK.EQ.0) THEN
            CLOSE(62,STATUS='DELETE')
         ENDIF   
         CALL LOCATE(23,0,IERR)
         STOP ' '
      END
******************************************************************************
      SUBROUTINE DRAWGRF(IGOPT,RTNCODE)
C
C       ** OBJECTIVE:  CALLS ROUTINE PCTRL WHICH DRAWS THE PLOT USING THE
C                      CURRENT VALUES IN THE GRAPH DEFINITION FILE.  PAUSES
C                      AFTER PLOT AND WAITS FOR USER RESPONSE.  PRINTS ERROR
C                      MESSAGES.
C
C       ** NOTE:       THERE ARE TWO VERSIONS OF ROUTINE PCTRL.  THE ONE
C                      USED IN GRFMN2 PLOTS MAPS.  THE VERSION USED IN
C                      GRFMN134 PLOTS TIMESERIES, SKEWT, AND WINDROSE.
C       ** INPUT:
C              IGOPT...GRAPH OPTION FLAG THAT CONTROLS SELECTION OF DATA
C                      THAT WILL BE PLOTTED AND PLOT CONTROLS
C                     -1=REDRAW CURRENT PLOT -- REGRID DATA -- PAUSE (MAP ONLY)
C                      0=REDRAW CURRENT PLOT -- PAUSE
C                      1=NEXT PLOT IN CURRENT DATA FRAME -- PAUSE
C                      2=NEW DATA FRAME -- PAUSE
C                      3=REDRAW CURRENT PLOT -- NO PAUSE
C                      4=REDRAW CURRENT SCREEN FOR PRINTING -- NO PAUSE
C       ** OUTPUT:
C            RTNCODE...ONE CHARACTER ERROR FLAG
C                      '0'=NORMAL EXIT
C                      '1'=EXIT USING ESC OR F4
C                      '2'=NO MORE PLOTS IN DATASET
C                      '3'=PCTRL2 CALLED WITH PLOT TYPE = TIMESERIES(1),
C                          SKEWT(3), OR WINDROSE(4)
C                      '4'=PCTRL134 CALLED WHEN PLOT WAS MAP
C                      '5'=NO DATA AVAILABLE FOR CURRENT PLOT
C                      '6'=FILE WROSPOKE.DEF NOT AVAILABLE TO WINDROSE 
C                      NOTE:  VALUE SET IN RTNCODE IS USED TO DETERMINE IF A
C                             PLOT IS ON THE SCREEN.  RTNCODE VALUE IS SET TO 
C                             '7' BY SKEWT ROUTINES TO INDICATED PRESSURE OR
C                             TEMPERATURE IS OUT OF SORT BUT PLOT IS STILL ON
C                             SCREEN SO RTNCODE IS RESET TO '0' AFTER ERROR 
C                             MESSAGE IS DISPLAYED.  ACTION IS SIMILAR WHEN
C                             RTNCODE IS '8'.  THIS RETURN CODE IS SET BY SKEWT
C                             ROUTINES WHEN NO LINES ARE PLOTTED, BUT THE
C                             BACKGROUND IS STILL ON THE SCREEN.
C
      INTEGER*2 IGOPT
C
$INCLUDE: 'GRFPARM.INC'
$INCLUDE: 'GRAFVAR.INC'
$INCLUDE: 'CURRPLT.INC'
C
      CHARACTER INCHAR*2, RTNCODE*1, RTFLG*1
      INTEGER*2 IOPT
      LOGICAL PAUSEFLG
C
      RTNCODE='0'      
C      
C **DEBUG -- THIS ROUTINE WILL CONTROL THE PLOTTING OF 
C **DEBUG -- MULTIPLE PLOTS PER SCREEN           
      IF (IGRAPH.EQ.3) THEN
C
C          .. SKEWT -- ALL OPTIONS      
         IOPT=IGOPT
         PAUSEFLG=.FALSE.
      ELSE   
         IF (IGOPT.EQ.3) THEN
C         
C             .. REDRAW CURRENT PLOT -- NO PAUSE
            IF (IGRAPH.EQ.2) THEN
C                .. MAP
               IOPT=-1
            ELSE   
C                .. TIMESERIES,WINDROSE 
               IOPT=0
            ENDIF   
            PAUSEFLG=.FALSE.
         ELSE IF (IGOPT.EQ.4) THEN
C             .. TIMESERIES,MAP,WINDROSE -- REDRAW CURRENT SCREEN FOR PRINTING
            IOPT=IGOPT
            PAUSEFLG=.FALSE.
         ELSE
C             .. TIMESERIES,MAP,WINDROSE -- OPTIONS -1,0,1,2
            IOPT=IGOPT
            PAUSEFLG=.TRUE.
         ENDIF   
      ENDIF   
      CALL PCTRL(IOPT,RTNCODE)
C
C       ** RETURN CODE=0 INDICATES NORMAL EXIT FROM ROUTINE 
C                     =1 INDICATES ROUTINE WAS EXITED BY ESC OR F4
C
      IF (RTNCODE.EQ.'0') THEN      
         IF (PAUSEFLG) THEN
C
C             .. PROGRAM PAUSES UNTIL ANY CHARACTER FROM THE KEYBOARD OR
C                ONE OF THE MOUSE BUTTONS IS PRESSED      
            RTFLG = '1'
   50       CONTINUE
            CALL RDLOC(XP,YP,INCHAR,RTFLG)      
            IF (RTFLG.EQ.'1' .AND. (INCHAR.NE.'RE' .AND.
     +                              INCHAR.NE.'4F')) THEN
               GO TO 50
            ENDIF
         ENDIF      
      ELSE IF (RTNCODE.NE.'1' .AND. IGOPT.NE.4) THEN
C
C          ** ERROR MESSAGES ARE PRINTED ONLY IF PLOT GOES TO SCREEN AND
C             AND ROUTINE WAS NOT EXITED WITH ESC/F4
C
         RTFLG = ' ' 
         IF (RTNCODE.EQ.'2') THEN
C             .. ERROR:  NO MORE PLOTS IN FRAME      
            MSGN1=535
         ELSE IF (RTNCODE.EQ.'3') THEN
C             .. ERROR:  PCTRL2 CALLED WITH PLOT TYPE = TIMESERIES(1),
C                        SKEWT(3), OR WINDROSE(4)
            MSGN1=189
         ELSE IF (RTNCODE.EQ.'4') THEN
C             .. ERROR:  PCTRL134 CALLED WITH PLOT TYPE = MAP(2)      
            MSGN1=190
         ELSE IF (RTNCODE.EQ.'5') THEN
C             .. ERROR:  NO DATA AVAILABLE FOR CURRENT PLOT      
            MSGN1=547
         ELSE IF (RTNCODE.EQ.'6') THEN
C             .. ERROR:  FILE WROSPOKE.DEF NOT AVAILABLE TO WINDROSE 
C                        ERROR MESSAGE HANDLED IN ROUTINE WINDROSE      
            GO TO 100
         ELSE IF (RTNCODE.EQ.'7') THEN
C             .. ERROR:  PRESSURE AND/OR HEIGHT VALUES ARE OUT OF SORT
C                        SKEWT COULD NOT DRAW HEIGHT LABELS
            MSGN1=443
            RTNCODE = '0'
         ELSE IF (RTNCODE.EQ.'8') THEN
C             .. ERROR:  NO DATA TO PLOT.  EITHER VALUES ARE MISSING OR 
C                        PRESSURES ARE TOO LOW TO PLOT; A MINIMUM OF TWO 
C                        POINTS WITH PRESSURES GREATER THAN 100 ARE REQUIRED.
            MSGN1=442
            RTNCODE = '0'
         ELSE            
C             .. UNKNOWN RETURN CODE          
            MSGN1=188
            RTFLG = RTNCODE
         ENDIF
         MSGN2=202
         XWIN=.1
         YWIN=.95
         CALL GRAFNOTE(XWIN,YWIN,MSGN1,MSGN2,RTFLG,1,INCHAR)
      ELSE IF (RTNCODE.EQ.'7' .OR. RTNCODE.EQ.'8') THEN
         RTNCODE = '0'   
      ENDIF
  100 CONTINUE      
      RETURN 
      END
******************************************************************************
      SUBROUTINE GRFPRNT
C------------------------------------------------------------------------------
C     PRINT THE SCREEN ON THE ACTIVE PRINTER IN QUICK SCREEN OR 
C     HI-RESOLUTION MODE.
C------------------------------------------------------------------------------
$INCLUDE:'HALOENV.INC'
$INCLUDE:'GRFPARM.INC'
$INCLUDE:'GRAFVAR.INC'
      COMMON/DEVHNDL/IHNDLSCR,IHNDLVRI,SCRNASP,VRIASP,DEVASP
C      
      INTEGER*2     MATR(0:32),IFUNC,IER
      CHARACTER*2   INCHAR, HALOERR
      CHARACTER*1   RTNCODE
      COMMON/GRFPRNSV/MATR,IFUNC,IER,INCHAR,HALOERR,RTNCODE
C      
      LOGICAL       FIRSTCALL
      DATA FIRSTCALL /.TRUE./
      DATA MATR/33*0/
C
      CALL CLOSEG
      CALL CKHALOER(1,'CLOSEG-SCR',IER)
      IF (IER.NE.0) GO TO 250
C          
C---- PRINT A GRAPHICS PRODUCT IN HIGH RESOLUTION MODE USING HALO'S
C---- VIRTUAL RASTER INTERFACE (VRI).
C---  ACTVPTR IS THE PRINTER DESIGNATION, 0 = PRIMARY & 1 = ALTERNATE 
C
      IF (FIRSTCALL) THEN
         FIRSTCALL = .FALSE.
         IDPTR = ACTVPTR+1
         IDCLR = CLRMOD(IDPTR)+1
C      
         MAXX      = PTRVAL(0,IDPTR) - 1
         MAXY      = PTRVAL(1,IDPTR) - 1
C         
         IF (IDCLR.EQ.2) THEN
C             .. COLOR PRINTER IN COLOR MODE
            MAXC = 15
         ELSE         
            IF (PTRVAL(19,IDPTR).NE.0) THEN
C                .. B/W PRINTER WITH DITHERING         
               MAXC = 3
            ELSE
C                .. B/W PRINTER WITH NO DITHERING
               MAXC = 1
            ENDIF
         ENDIF
C      
         MATR(0)=0
         MATR(1)=2
         MATR(2)=0
         MATR(3)=0
         MATR(4)=0
         OPEN(61,FILE='O:\DATA\GRFSPCS.PRM',FORM='FORMATTED',ERR=11)
         READ (61,*,ERR=10) MATR(0),MATR(1),MATR(4)
   10    CONTINUE
         CLOSE (61)
   11    CONTINUE
C   
         CALL SETDEV(VRI)
         CALL CKHALOER(1,'SETDEV',IER)
         IF (IER.NE.0) GO TO 250
C      
         CALL INQADE(IHNDLVRI)
         CALL CKHALOER(1,'INQADE',IER)
         IF (IER.NE.0) GO TO 250
C      
         CALL SETDRA(MAXX,MAXY)
         CALL CKHALOER(1,'SETDRA-VRI',IER)
         IF (IER.NE.0) GO TO 250
         MAXXLST = MAXX
         MAXYLST = MAXY
C      
         CALL SETCRA(MAXC)
         CALL CKHALOER(1,'SETCRA-VRI',IER)
         IF (IER.NE.0) GO TO 250
         NCLRLST = MAXC
C      
         CALL SETMAT(MATR)
         CALL CKHALOER(1,'SETMAT-VRI',IER)
         IF (IER.NE.0) GO TO 250
C         
         CALL INITGR(0)
         CALL CKHALOER(1,'INITGR-VRI',IER)
         IF (IER.NE.0) GO TO 250
      ENDIF
C      
      CALL SETIEE(1)
      CALL SETASP(PTRASP(IDPTR))
C **DEBUG      
      CALL INQASP(VRIASP)
      DEVASP = PTRASP(IDPTR)
      WRITE(999,*)'GRFPRNT--PTR/INQ/SCR ASP=',PTRASP(IDPTR),
     +              VRIASP,SCRNASP
C      
C---  MESSAGE TO USER TO WAIT DURING REDRAW 
      CALL WRTMSG(18,380,3,1,0,' ',0)
C      
C---  HIGH RESOLUTION PRINT -- OPTION=4
      CALL DRAWGRF(4,RTNCODE)
      NTRY = 0
C      
C       .. IN ORDER TO OPEN THE VIEWPORT TO THE ENTIRE SCREEN, A MAX VALUE
C          EQUAL TO .999 MUST BE USED.  A VALUE OF 1.0 IS A SPECIAL SIGNAL FOR
C          HALO TO 'TURN OFF' THE VIEWPORT WHICH DOES NOT RESET ASPECT RATIOS
      CALL SETVIE(0.0,0.0,0.999,0.999,-1,-1)
C **DEBUG
      CALL SETWOR(0.,0.,1.,1.)
      CALL SETLNW(11)
      CALL SETCOL(3)
      CALL BOX(0.,0.,.99,.99)      
      CALL SETLNW(1)
      CALL SETCOL(0)
C **END DEBUG      
  200 CALL GPRINT
      NTRY = NTRY + 1
      CALL INQERR(IFUNC,IER)
      IF (IER .NE. 0) THEN
         IF (IER.LT.20 .OR. IER.GT.29 .OR. NTRY .GT.2) THEN
            IF (NTRY .GT. 2) THEN
               MSGERR = 521
            ELSE
               MSGERR = 520
            ENDIF
            CALL WRTMSG(6,MSGERR,4,1,1,' ',0)
            CALL CLRMSG(6)
         ELSE
            HALOERR = ' '
            NCHR    = 0
            IF     (IER .EQ. 20) THEN
                   MSGERR = 522
            ELSEIF (IER .EQ. 21) THEN
                   MSGERR = 519
            ELSEIF (IER .EQ. 22) THEN
                   MSGERR = 523
            ELSEIF (IER .EQ. 23) THEN
                   MSGERR = 524
            ELSEIF (IER .GT. 23) THEN
                   MSGERR = 194
                   WRITE(UNIT=HALOERR,FMT='(I2)') IER
                   NCHR = 2
            ENDIF
            CALL WRTMSG(6,MSGERR,4,0,0,HALOERR,NCHR)
            CALL WRTMSG(5,507,4,1,0,' ',0)
            CALL GETCHAR(0,INCHAR)
            CALL CLRMSG(6)
            CALL CLRMSG(5)
            IF (IER.NE.23.AND.INCHAR.NE.'ES'.AND.INCHAR.NE.'4F') THEN
               GO TO 200
            ENDIF
         ENDIF
      ENDIF
  250 CONTINUE
      CALL CLOSEG
      CALL CKHALOER(1,'CLOSEG-VRI',IER)
      IF (IER.NE.0) GO TO 275
      CALL BGNHALO(1,PALETTE,PALDEF)
      DEVASP = SCRNASP
  275 CONTINUE
      RETURN
C
C---- QUICK SCREEN PRINT. 
C
      ENTRY SNAPST 
      NTRY = 0
C      
      CALL INQVIE(X1,Y1,X2,Y2)
C       .. IN ORDER TO OPEN THE VIEWPORT TO THE ENTIRE SCREEN, A MAX VALUE
C          EQUAL TO .999 MUST BE USED.  A VALUE OF 1.0 IS A SPECIAL SIGNAL FOR
C          HALO TO 'TURN OFF' THE VIEWPORT WHICH DOES NOT RESET ASPECT RATIOS
      CALL SETVIE(0.0,0.0,0.999,0.999,-1,-1)
C **DEBUG
      CALL SETWOR(0.,0.,1.,1.)
      CALL SETLNW(11)
      CALL SETCOL(3)
      CALL BOX(0.,0.,.99,.99)      
      CALL SETLNW(1)
      CALL SETCOL(0)
  300 CALL GPRINT
      NTRY = NTRY + 1
      CALL INQERR(IFUNC,IER)
      IF (IER .NE. 0) THEN
         XLL = 0.5
         YLL = 0.2
         IF (IER.LT.20 .OR. IER.GT.29 .OR. NTRY .GT.2) THEN
            IF (NTRY .GT. 2) THEN
               MSGERR = 521
            ELSE
               MSGERR = 520
            ENDIF
            CALL GRAFNOTE(XLL,YLL,MSGERR,507,' ',0,INCHAR)
         ELSE
            HALOERR = ' '
            IF     (IER .EQ. 20) THEN
                   MSGERR =  522
            ELSEIF (IER .EQ. 21) THEN
                   MSGERR =  519
            ELSEIF (IER .EQ. 22) THEN
                   MSGERR = 523
            ELSEIF (IER .EQ. 23) THEN
                   MSGERR = 524
            ELSEIF (IER.GT.23 .OR. IER.LT.20) THEN
                   MSGERR = 194
                   WRITE(UNIT=HALOERR,FMT='(I2)') IER
            ENDIF
            NCHR = LNG(HALOERR)
            CALL GRAFNOTE(XLL,YLL,MSGERR,507,HALOERR,NCHR,INCHAR)
            IF (IER.NE.23.AND.INCHAR.NE.'ES'.AND.INCHAR.NE.'4F') THEN
               GO TO 300
            ENDIF
         ENDIF
      ENDIF
      CALL SETVIE(X1,Y1,X2,Y2,-1,-1)
  400 CONTINUE      
      RETURN
      END

**************************************************************************
      SUBROUTINE SPOOLEND
C-------------------------------------------------------------------------
C     ROUTINE TO FORCE THE PRINTING OF A FILE BY SENDING AN END-OF-SPOOL
C     CHARACTER FOR A SHARED NETWORK PRINTER.  OTHERWISE, SPOOLED PRINT 
C     FILES ARE NOT RELEASED FOR PRINTING UNTIL THE USER EXITS CLICOM.
C     VALID FOR IBM'S PC-LAN ONLY.
C
C     INPUT & OUTPUT ARGUMENTS:  NONE
C-------------------------------------------------------------------------
      INTEGER      EFLAG, NTRY
      CHARACTER*2  USERTXT
C
      NTRY = 0
  10  CALL STPSPL(EFLAG)
      IF (EFLAG .EQ. 0) THEN
         NTRY = NTRY + 1
         IF (NTRY .LT. 1) THEN
             GO TO 10
         ELSE
             CALL GRAFNOTE(0.3,0.8,454,202,' ',0,USERTXT)
         ENDIF
      ENDIF
      RETURN
      END
