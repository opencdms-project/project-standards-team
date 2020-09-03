$STORAGE:2
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
      INTEGER*2     NCLRLST,IDPRLST,MAXXLST,MAXYLST
      CHARACTER*2   INCHAR, HALOERR
      CHARACTER*1   RTNCODE
      LOGICAL       NEWVRI
      COMMON/GRFPRNSV/MATR,IFUNC,IER,NCLRLST,IDPRLST,MAXXLST,MAXYLST
     +              INCHAR,HALOERR,RTNCODE,  NEWVRI
C      
      LOGICAL       FIRSTCALL
      DATA FIRSTCALL /.TRUE./
      DATA NCLRLST,IDPRLST,MAXXLST,MAXYLST/4*0/
      DATA MATR/33*0/
C
C---- PRINT A GRAPHICS PRODUCT IN HIGH RESOLUTION MODE USING HALO'S
C---- VIRTUAL RASTER INTERFACE (VRI).
C---  ACTVPTR IS THE PRINTER DESIGNATION, 0 = PRIMARY & 1 = ALTERNATE 
C
      IDPTR = ACTVPTR+1
      IDCLR = CLRMOD(IDPTR)+1
C      
      MAXX      = PTRVAL(0,IDPTR) - 1
      MAXY      = PTRVAL(1,IDPTR) - 1
C         
      IF (IDCLR.EQ.2) THEN
C          .. COLOR PRINTER IN COLOR MODE
         MAXC = 15
      ELSE         
         IF (PTRVAL(19,IDPTR).NE.0) THEN
C             .. B/W PRINTER WITH DITHERING         
            MAXC = 3
         ELSE
C             .. B/W PRINTER WITH NO DITHERING
            MAXC = 1
         ENDIF
      ENDIF
C      
      NEWVRI = MAXXLST.NE. MAXX .OR. MAXYLST.NE.MAXY .OR.
     +         NCLRLST.NE.MAXC  .OR. FIRSTCALL            
C      
      CALL CLOSEG
      CALL CKHALOER(1,'CLOSEG-SCR',IER)
      IF (IER.NE.0) GO TO 250
C          
      IF (FIRSTCALL) THEN
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
      ENDIF
C      IF (FIRSTCALL) THEN
      IF (FIRSTCALL .OR. NEWVRI) THEN
   
         IF (NEWVRI .AND. .NOT.FIRSTCALL) THEN   
            CALL SETADE(IHNDLVRI)
            CALL CKHALOER(1,'SETADE-VRI',IER)
            IF (IER.NE.0) GO TO 250
            CALL CLOSEG
            CALL CKHALOER(1,'CLOSEG-VRI',IER)
            IF (IER.NE.0) GO TO 250
            CALL CLSDEV(IHNDLVRI)
            CALL CKHALOER(1,'CLSDEV-VRI',IER)
            IF (IER.NE.0) GO TO 250
         ENDIF
C   
         FIRSTCALL = .FALSE.
         CALL SETDEV(VRI)
         CALL CKHALOER(1,'SETDEV',IER)
         IF (IER.NE.0) GO TO 250
C      
         CALL INQADE(IHNDLVRI)
         CALL CKHALOER(1,'INQADE',IER)
         IF (IER.NE.0) GO TO 250
      ELSE   
         CALL SETADE(IHNDLVRI)
         CALL CKHALOER(1,'SETADE-VRI',IER)
         IF (IER.NE.0) GO TO 250
C   
         IF (NEWVRI) THEN   
            CALL CLOSEG
            CALL CKHALOER(1,'CLOSEG-VRI',IER)
            IF (IER.NE.0) GO TO 250
         ENDIF
      ENDIF   
      IF (NEWVRI) THEN   
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
      CALL BGNHALO(1,PALETTE,PALDEF)
      DEVASP = SCRNASP
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

      
      