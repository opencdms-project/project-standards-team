$STORAGE:2
      SUBROUTINE GRFPRNT
C------------------------------------------------------------------------------
C     PRINT THE SCREEN ON THE ACTIVE PRINTER IN QUICK SCREEN OR 
C     HI-RESOLUTION MODE.
C------------------------------------------------------------------------------
$INCLUDE:'HALOENV.INC'
$INCLUDE:'GRFPARM.INC'
$INCLUDE:'GRAFVAR.INC'
$INCLUDE:'SAVBUF.INC'
      INTEGER *4    IADDR
      INTEGER       MATR(33), SEGM, SEGMENT, FUNC, ERR
      CHARACTER*78  STUFF
      CHARACTER*2   INCHAR, HALOERR
      CHARACTER*1   RTNCODE
      LOGICAL       DITHER, COLRPRN
C
C---- PRINT A GRAPHICS PRODUCT IN HIGH RESOLUTION MODE USING HALO'S
C---- VIRTUAL RASTER INTERFACE (VRI).
C---  ACTVPTR IS THE PRINTER DESIGNATION, 0 = PRIMARY & 1 = ALTERNATE 
C
      CALL INQDRA(MX,MY)
      CALL INQASP(DEVASP)
      XD=MX+1
      YD=MY+1
      DITHER  = .TRUE.
      COLRPRN = .FALSE.
      IF (ACTVPTR .EQ. 0 ) THEN
         PTRVAL(2) = (FLOAT(PTRVAL(1))*PTRASP*YD) / (XD*DEVASP)
         MAXX      = PTRVAL(1) - 1
         MAXY      = PTRVAL(2) - 1
         IF (PTRVAL(5) .EQ. 1) THEN
            DITHER = .FALSE.
         ENDIF
         IF (PTRVAL(20) .EQ. 1) THEN
            COLRPRN = .TRUE.
         ENDIF
      ELSE
         PRVAL2(2) = (FLOAT(PRVAL2(1))*PRASP2*YD) / (XD*DEVASP)
         MAXX      = PRVAL2(1) - 1
         MAXY      = PRVAL2(2) - 1
         IF (PRVAL2(5) .EQ. 1) THEN
            DITHER = .FALSE.
         ENDIF
         IF (PRVAL2(20) .EQ. 1) THEN
            COLRPRN = .TRUE.
         ENDIF
      ENDIF
      CALL CLOSEG
      IF (COLRPRN) THEN
         MAXC = 15
      ELSE         
         IF (DITHER) THEN
            MAXC = 3
         ELSE
            MAXC = 1
         ENDIF
      ENDIF
      CALL SETDEV(VRI)
      CALL SETDRA(MAXX,MAXY)
      CALL SETCRA(MAXC)
      IADDR =  LOCFAR(BUFFER)
      SEGM = ISHFT(IADDR,-16)
      MATR(2) = (MAXBYT - (4 * 1024)) / 16
      MATR(8) = 0
      MATR(9) = SEGM
      SEGMENT = (1024*4)/16 + SEGM
      CALL SETSEG(SEGMENT)
      CALL SETMAT(MATR)
C         
C---- LOAD THE VIRTUAL RASTER INTERFACE AND SET PRINTER ATTRIBUTES
C
      CALL INITGR(0)
      CALL INQERR(FUNC,ERR)
      IF (ERR .NE. 0) THEN
         CALL WRTMSG(4,482,4,1,1,' ',0)
         CALL BGNHALO(1,PALETTE,PALDEF)
         RETURN
      ENDIF
      CALL SETIEE(1)
      IF (ACTVPTR .EQ. 0) THEN
         CALL SETASP(PTRASP)
         CALL SETPRN(PRINTR)
         CALL SETPAT(PTRVAL)
      ELSE
         CALL SETASP(PRASP2)
         CALL SETPRN(PRNTR2)
         CALL SETPAT(PRVAL2)
      ENDIF
C---  MESSAGE TO USER TO WAIT DURING REDRAW 
      CALL WRTMSG(18,380,3,1,0,' ',0)
C---  HIGH RESOLUTION PRINT -- OPTION=4
      CALL DRAWGRF(4,RTNCODE)
      NTRY = 0
      CALL INQVIE(X1,Y1,X2,Y2)
C       .. IN ORDER TO OPEN THE VIEWPORT TO THE ENTIRE SCREEN, A MAX VALUE
C          EQUAL TO .999 MUST BE USED.  A VALUE OF 1.0 IS A SPECIAL SIGNAL FOR
C          HALO TO 'TURN OFF' THE VIEWPORT WHICH DOES NOT RESET ASPECT RATIOS
      CALL SETVIE(0.0,0.0,0.999,0.999,-1,-1)
  200 CALL GPRINT
      NTRY = NTRY + 1
      CALL INQERR(FUNC,ERR)
      IF (ERR .NE. 0) THEN
         IF (ERR.LT.20 .OR. ERR.GT.29 .OR. NTRY .GT.2) THEN
            IF (NTRY .GT. 2) THEN
               MSGERR = 521
            ELSE
               MSGERR = 520
            ENDIF
            CALL WRTMSG(6,MSGERR,4,1,1,' ',0)
            CALL CLRMSG(6)
         ELSE
            IF     (ERR .EQ. 20) THEN
                   MSGERR = 522
            ELSEIF (ERR .EQ. 21) THEN
                   MSGERR = 519
            ELSEIF (ERR .EQ. 22) THEN
                   MSGERR = 523
            ELSEIF (ERR .EQ. 23) THEN
                   MSGERR = 524
            ELSEIF (ERR .GT. 23) THEN
                   MSGERR = 194
            ENDIF
            IF (MSGERR .EQ. 194) THEN
               WRITE(UNIT=HALOERR,FMT='(I2)') ERR
               CALL WRTMSG(6,MSGERR,4,0,0,HALOERR,2)
            ELSE
               CALL WRTMSG(6,MSGERR,4,0,0,' ',0)
            ENDIF
            CALL WRTMSG(5,507,4,1,0,' ',0)
            CALL GETCHAR(0,INCHAR)
            CALL CLRMSG(6)
            CALL CLRMSG(5)
            IF (ERR.NE.23.AND.INCHAR.NE.'ES'.AND.INCHAR.NE.'4F') THEN
               GO TO 200
            ENDIF
         ENDIF
      ENDIF
      CALL SETVIE(X1,Y1,X2,Y2,-1,-1)
      CALL BGNHALO(1,PALETTE,PALDEF)
C      CALL DRAWGRF(3,RTNCODE)
      RETURN
C
C---- QUICK SCREEN PRINT. 
C
      ENTRY SNAPST 
      CALL INQDRA(MX,MY)
      CALL INQASP(DEVASP)
      XD=MX+1
      YD=MY+1
      NTRY = 0
      IF (ACTVPTR .EQ. 0) THEN
         PTRVAL(2) = (FLOAT(PTRVAL(1))*PTRASP*YD) / (XD*DEVASP)
         CALL SETPRN(PRINTR)
         CALL SETPAT(PTRVAL)
      ELSE
         PRVAL2(2) = (FLOAT(PRVAL2(1))*PRASP2*YD) / (XD*DEVASP)
         CALL SETPRN(PRNTR2)
         CALL SETPAT(PRVAL2)
      ENDIF
      CALL INQVIE(X1,Y1,X2,Y2)
C       .. IN ORDER TO OPEN THE VIEWPORT TO THE ENTIRE SCREEN, A MAX VALUE
C          EQUAL TO .999 MUST BE USED.  A VALUE OF 1.0 IS A SPECIAL SIGNAL FOR
C          HALO TO 'TURN OFF' THE VIEWPORT WHICH DOES NOT RESET ASPECT RATIOS
      CALL SETVIE(0.0,0.0,0.999,0.999,-1,-1)
  300 CALL GPRINT
      NTRY = NTRY + 1
      CALL INQERR(FUNC,ERR)
      IF (ERR .NE. 0) THEN
         XLL = 0.5
         YLL = 0.2
         STUFF = ' '
         IF (ERR.LT.20 .OR. ERR.GT.29 .OR. NTRY .GT.2) THEN
            IF (NTRY .GT. 2) THEN
               MSGERR = 521
            ELSE
               MSGERR = 520
            ENDIF
            CALL GRAFNOTE(XLL,YLL,MSGERR,507,' ',0,INCHAR)
         ELSE
            IF     (ERR .EQ. 20) THEN
                   MSGERR =  522
            ELSEIF (ERR .EQ. 21) THEN
                   MSGERR =  519
            ELSEIF (ERR .EQ. 22) THEN
                   MSGERR = 523
            ELSEIF (ERR .EQ. 23) THEN
                   MSGERR = 524
            ELSEIF (ERR.GT.23 .OR. ERR.LT.20) THEN
                   MSGERR = 194
            ENDIF
            IF (MSGERR .EQ. 194) THEN
               WRITE(UNIT=HALOERR,FMT='(I2)') ERR
               CALL GRAFNOTE(XLL,YLL,MSGERR,507,HALOERR,2,INCHAR)
            ELSE
               CALL GRAFNOTE(XLL,YLL,MSGERR,507,' ',0,INCHAR)
            ENDIF
            IF (ERR.NE.23.AND.INCHAR.NE.'ES'.AND.INCHAR.NE.'4F') THEN
               GO TO 300
            ENDIF
         ENDIF
      ENDIF
      CALL SETVIE(X1,Y1,X2,Y2,-1,-1)
      RETURN
      END
