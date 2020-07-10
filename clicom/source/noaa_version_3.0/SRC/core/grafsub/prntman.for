$STORAGE:2
      SUBROUTINE PRNTMAN(PALETTE,PALDEF,PLTONSCR)
C------------------------------------------------------------------------------
C     UPPER LEVEL PROGRAM THAT MANAGES THE ROUTINES THAT 1)-REVISE PRINTER
C     FUNCTIONS (FORM FEED, RESOLUTION, ETC) THRU A CLICOM FORM; 2)-SELECTS THE
C     PRINTER TO USE; AND 3)-PRINTS THE SCREEN IN QUICK OR HI-RESOLUTION MODE.
C
C     INPUT ARGUMENTS:
C
C     PALETTE       INT2       CURRENT PALETTE NUMBER
C     PALDEF        INT2 ARRAY CURRENT DEFINITION OF ALL 12 POSSIBLE PALETTES
C     PLTONSCR      LOG        FLAG TO INDICATE IF THE PLOT IS CURRENTLY
C                              DISPLAYED ON THE SCREEN
C
C     OUTPUT ARGUMENTS:  NONE
C------------------------------------------------------------------------------
$INCLUDE: 'HALOENV.INC'
      INTEGER       PALETTE, PALDEF(16,12), HELPLVL, NOWPTR
      CHARACTER*2   INCHAR, USERTXT
      CHARACTER*1   HALOID, RTNCODE
      LOGICAL PLTONSCR
C   
C---  DISPLAY GRAPHICS PRINTER MENU. ACTVPTR IS THE PRINTER DESIGNATION,
C---  FROM HALOENV.INC: 0 = PRIMARY AND 1 = ALTERNATE. 
C
      IF (PRINTR(14:17) .EQ. 'QQQQ') THEN
         CALL GRAFNOTE(0.3,0.8,517,202,' ',0,USERTXT)
         RETURN
      ENDIF
      XL=0.1
      YL=0.2
      HELPLVL = 8
  100 CALL GRAFMNU(1,8,XL,YL,HELPLVL,INCHAR)
C
C---  LWIDTH IS PIXEL WIDTH OF ACTIVE PRINTER. A -1 SIGNIFIES THAT THE
C---  USER FAILED TO RUN THE PRINTER OPTIONS PGM PRIOR TO 1ST EVER PRINT.
C
      IF (ACTVPTR .EQ. 0) THEN
         LWIDTH = PTRVAL(1)
      ELSE
         LWIDTH = PRVAL2(1)
      ENDIF
C
C-----  CHANGE PRINTER DESIGNATION FROM PRIMARY TO ALTERNATE AND VICE VERSA
C
      IF   (INCHAR .EQ. '1 ') THEN
           IF (ACTVPTR .EQ. 0) THEN
              IF (PRNTR2(14:17) .EQ. 'QQQQ') THEN
                 CALL GRAFNOTE(0.3,0.8,518,202,' ',0,USERTXT)
              ELSE
                 ACTVPTR = 1
                 CALL GRAFNOTE(0.3,0.8,457,202,' ',0,USERTXT)
              ENDIF
           ELSE
              ACTVPTR = 0
              CALL GRAFNOTE(0.3,0.8,456,202,' ',0,USERTXT)
           ENDIF
C
C-----  UPDATE HALO PRINTER OPTIONS.
C
      ELSE IF (INCHAR .EQ. '2 ') THEN
           CALL CLOSEG
           PLTONSCR=.FALSE.
           CALL PRNTOP 
           NOWPTR = ACTVPTR
           CALL BGNHALO(1,PALETTE,PALDEF)
           ACTVPTR = NOWPTR
C
C-----  QUICK SCREEN PRINT 
C-----  NETWORK=1 MEANS IBM PC LAN, SEND END-OF-SPOOL CHAR TO PRINT NOW
C
      ELSE IF (INCHAR .EQ. '3 ') THEN
           IF (LWIDTH .NE. -1) THEN
              IF (.NOT.PLTONSCR) THEN
                 CALL DRAWGRF(3,RTNCODE)
                 PLTONSCR=.TRUE.
              ENDIF   
              CALL SNAPST  
              CALL GETHAL(HALOID)
              IF (HALOID .GE. 'B' .AND. HALOID .LE. 'Z') THEN
                 IF (NETWORK .EQ. 1) THEN
                    CALL SPOOLEND
                 ELSE
                    CALL GRAFNOTE(0.3,0.8,454,202,' ',0,USERTXT)
                 ENDIF
              ENDIF
           ELSE
              CALL GRAFNOTE(0.3,0.8,516,202,' ',0,USERTXT)
           ENDIF
C
C-----  BETTER RESOLUTION PRINT ( THRU THE VIRTUAL RASTER INTERFACE)
C-----  NETWORK=1 MEANS IBM PC LAN, SEND END-OF-SPOOL CHAR TO PRINT NOW
C
      ELSE IF (INCHAR .EQ. '4 ') THEN
           IF (LWIDTH .NE. -1) THEN
              CALL GRFPRNT
              PLTONSCR=.FALSE.
              CALL GETHAL(HALOID)
              IF (HALOID .GE. 'B' .AND. HALOID .LE. 'Z') THEN
                 IF (NETWORK .EQ. 1) THEN
                    CALL SPOOLEND
                 ELSE
                    CALL GRAFNOTE(0.3,0.8,454,202,' ',0,USERTXT)
                 ENDIF
              ENDIF
           ELSE
              CALL GRAFNOTE(0.3,0.8,516,202,' ',0,USERTXT)
           ENDIF
      ELSE IF (INCHAR .EQ. 'ES' .OR. INCHAR .EQ. '4F') THEN
           RETURN
      ELSE
           CALL BEEP
      ENDIF
      GO TO 100
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