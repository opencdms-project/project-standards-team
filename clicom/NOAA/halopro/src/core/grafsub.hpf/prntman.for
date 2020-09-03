$STORAGE:2
      SUBROUTINE PRNTMAN(PALETTE,PALDEF)
C------------------------------------------------------------------------------
C     UPPER LEVEL PROGRAM THAT MANAGES THE ROUTINES THAT 1)-REVISE PRINTER
C     FUNCTIONS (FORM FEED, RESOLUTION, ETC) THRU A CLICOM FORM; 2)-SELECTS THE
C     PRINTER TO USE; AND 3)-PRINTS THE SCREEN IN QUICK OR HI-RESOLUTION MODE.
C
C     INPUT ARGUMENTS:
C
C     PALETTE       INT2       CURRENT PALETTE NUMBER
C     PALDEF        INT2 ARRAY CURRENT DEFINITION OF ALL 12 POSSIBLE PALETTES
C                              DISPLAYED ON THE SCREEN
C
C     OUTPUT ARGUMENTS:  NONE
C------------------------------------------------------------------------------
$INCLUDE: 'HALOENV.INC'
      COMMON/DEVHNDL/IHNDLSCR,IHNDLVRI,SCRNASP,VRIASP,DEVASP
      INTEGER       PALETTE, PALDEF(16,12), HELPLVL
      CHARACTER*2   INCHAR, USERTXT
      CHARACTER*1   HALOID, RTNCODE
      LOGICAL FIRSTCALL,PRTOPT
      DATA FIRSTCALL/.TRUE./
C **DEBUG
        WRITE(999,*)'BEG PRNTMAN PALETTE=',PALETTE,PALDEF(1,4)      
C
      IDPTR = ACTVPTR+1
      IDCLR = CLRMOD(IDPTR)+1
      IF (PRINTR(1,1)(14:17) .EQ. 'QQQQ') THEN
         CALL GRAFNOTE(0.3,0.8,517,202,' ',0,USERTXT)
         GO TO 200
      ENDIF
C
C---  PTRVAL(0) IS PIXEL WIDTH OF ACTIVE PRINTER. A -1 SIGNIFIES THAT THE
C---  USER FAILED TO RUN THE PRINTER OPTIONS PGM PRIOR TO 1ST EVER PRINT.
C
      PRTOPT = PTRVAL(0,IDPTR).EQ.-1
C **DEBUG      
      WRITE(999,*)'PRNTMAN PTRVAL(0),PRTOPT=',PTRVAL(0,IDPTR),PRTOPT
      IF (FIRSTCALL) THEN
         FIRSTCALL=.FALSE.
C **DEBUG         
         WRITE(999,*)'############PRNTMAN--CALL SETACTPR--FIRSTCALL'         
         CALL SETACTPR(PRINTR(IDCLR,IDPTR),PTRVAL(0,IDPTR),
     +                 PTRASP(IDPTR),IER)
         IF (IER.NE.0) THEN
            IF (IER.GE.29 .AND. IER.LE.33) THEN
               PRTOPT = .TRUE.
C **DEBUG      
               WRITE(999,*)'PRNTMAN IER,PRTOPT=',IER,PRTOPT
            ELSE   
C **DEBUG      
               WRITE(999,*)'PRNTMAN IER,PRTOPT=',IER,PRTOPT
               GO TO 200
            ENDIF
         ENDIF      
      ENDIF                 
C   
C---  DISPLAY GRAPHICS PRINTER MENU. ACTVPTR IS THE PRINTER DESIGNATION,
C---  FROM HALOENV.INC: 0 = PRIMARY AND 1 = ALTERNATE. 
C
      XL=0.1
      YL=0.2
      HELPLVL = 8
C **DEBUG
        WRITE(999,*)'PRNTMAN BEF GRAFMNU PALETTE=',PALETTE,PALDEF(1,4)      
  100 CALL GRAFMNU(1,8,XL,YL,HELPLVL,INCHAR)
C
C-----  CHANGE PRINTER DESIGNATION FROM PRIMARY TO ALTERNATE AND VICE VERSA
C
      NMSG = 0
      IF (INCHAR .EQ. '1 ') THEN
         IF (ACTVPTR .EQ. 0) THEN
            IF (PRINTR(1,2)(14:17) .EQ. 'QQQQ') THEN
               NMSG = 518
            ELSE
               ACTVPTR = 1
               NMSG = 457
            ENDIF
         ELSE
            ACTVPTR = 0
            NMSG = 456
         ENDIF
         IDPTR = ACTVPTR+1
         IDCLR = CLRMOD(IDPTR)+1
C **DEBUG
         WRITE(999,*)'PRMALT  IDPTR,IDCLR=',IDPTR,IDCLR
         WRITE(999,*)'PRMALT PRNTR=',PRINTR(IDCLR,IDPTR)    
         FIRSTCALL=.FALSE.
C **DEBUG         
         WRITE(999,*)'############PRNTMAN--CALL SETACTPR--CHG PTR'         
         CALL SETACTPR(PRINTR(IDCLR,IDPTR),PTRVAL(0,IDPTR),
     +                 PTRASP(IDPTR),IER)
         IF (IER.NE.0) THEN
            IF (IER.GE.29 .AND. IER.LE.33) THEN
               PRTOPT = .TRUE.
C **DEBUG      
               WRITE(999,*)'PRNTMAN IER,PRTOPT=',IER,PRTOPT
            ELSE   
C **DEBUG      
               WRITE(999,*)'PRNTMAN IER,PRTOPT=',IER,PRTOPT
               GO TO 200
            ENDIF
         ENDIF      
C
C-----  UPDATE HALO PRINTER OPTIONS.
C
      ELSE IF (INCHAR .EQ. '2 ') THEN
C **DEBUG
        WRITE(999,*)'PRNTMAN BEF PRNTOP PALETTE=',PALETTE,PALDEF(1,4)      
           CALL CLOSEG
           CALL PRNTOPHP 
           PRTOPT = PTRVAL(0,IDPTR).EQ.-1
C **DEBUG
        WRITE(999,*)'PRNTMAN BEF BGNHALO PALETTE=',PALETTE,PALDEF(1,4)      
           CALL BGNHALO(1,PALETTE,PALDEF)
C
C-----  QUICK SCREEN PRINT 
C-----  NETWORK=1 MEANS IBM PC LAN, SEND END-OF-SPOOL CHAR TO PRINT NOW
C
      ELSE IF (INCHAR .EQ. '3 ') THEN
           IF (.NOT.PRTOPT) THEN
              CALL DRAWGRF(3,RTNCODE)
              CALL SNAPST  
              CALL GETHAL(HALOID)
              IF (HALOID .GE. 'B' .AND. HALOID .LE. 'Z') THEN
                 IF (NETWORK .EQ. 1) THEN
                    CALL SPOOLEND
                 ELSE
                    NMSG = 454
                 ENDIF
              ENDIF
           ELSE
              NMSG = 516
           ENDIF
C
C-----  BETTER RESOLUTION PRINT ( THRU THE VIRTUAL RASTER INTERFACE)
C-----  NETWORK=1 MEANS IBM PC LAN, SEND END-OF-SPOOL CHAR TO PRINT NOW
C
      ELSE IF (INCHAR .EQ. '4 ') THEN
           IF (.NOT.PRTOPT) THEN
              CALL GRFPRNT
              CALL GETHAL(HALOID)
              IF (HALOID .GE. 'B' .AND. HALOID .LE. 'Z') THEN
                 IF (NETWORK .EQ. 1) THEN
                    CALL SPOOLEND
                 ELSE
                    NMSG = 454
                 ENDIF
              ENDIF
           ELSE
              NMSG = 516
           ENDIF
      ELSE IF (INCHAR .EQ. 'ES' .OR. INCHAR .EQ. '4F') THEN
           GO TO 200
      ELSE
           CALL BEEP
      ENDIF
      IF (NMSG.GT.0) CALL GRAFNOTE(0.3,0.8,NMSG,202,' ',0,USERTXT)
      GO TO 100
C
  200 CONTINUE
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