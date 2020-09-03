$STORAGE:2
      PROGRAM SHOWSLID
C------------------------------------------------------------------------------
C     DISPLAY A SCREEN PREVIOUSLY SAVED AS A DISK FILE VIA SUBROUTINE WRTGSCRN
C------------------------------------------------------------------------------
$INCLUDE: 'HALOENV.INC'
      CHARACTER*24  FILENAME, DISKNM
      CHARACTER*12  GRFID
      CHARACTER*8   SCRNAME
      CHARACTER*2   RTNCODE, YESUP, YESLO
      INTEGER*2     PALDEF(16,12)/192*1/,PALETTE/12/
C
C---  OPEN FILE WHICH HAS THE NAME OF THE PREVIOUSLY SAVED GRAPHICS SCREEN
C
      CALL GETYN(1,2,YESUP,YESLO)
  10  OPEN (51,FILE='O:\DATA\SHOWSLID.PIC',STATUS='OLD',IOSTAT=IOCHK)
      IF (IOCHK .NE. 0) THEN
         CALL OPENMSG('O:\DATA\SHOWSLID.PIC  ','  SHOWSLID  ',IOCHK)
         GO TO 10
      ENDIF
      READ(51,*) GRFID, SCRNAME, (PALDEF(M,PALETTE),M=1,16)
      CLOSE(51)
      IF (GRFID .NE. 'GRAPH-SCREEN') THEN
         CALL CLS
         CALL WRTMSG(23,174,12,1,0,GRFID,12)
         CALL LOCATE(3,0,IER)
         STOP 1
      ENDIF
C
C---  INITIALIZE HALO GRAPHICS AND DISPLAY THE SAVED SCREEN
C
      DISKNM      = '                        '
      DISKNM(1:8) = 'O:\DATA\'
      NBCHR = LNG(SCRNAME)
      IPOS  = NBCHR + 9
      DISKNM(9:IPOS-1)   = SCRNAME(1:NBCHR)
      DISKNM(IPOS:IPOS+3)= '.SCR'
      IDEF=1
      CALL BGNHALO(IDEF,PALETTE,PALDEF)
      CALL DELIMSTR(DISKNM,FILENAME)
      CALL GREAD(FILENAME)
      CALL INQERR(IFN,IER)
C
C---- HALO ERROR CODES HERE
C
      XLL = 0.1
      YLL = 0.2
      IF (IER .NE. 0) THEN
         FILENAME=DISKNM
         LENMSG=LNG(FILENAME)
         IF     (IER .EQ. 11) THEN
                MSGERR = 196
         ELSEIF (IER .EQ. 15) THEN
                MSGERR = 198
         ELSEIF (IER .EQ. 16) THEN
                MSGERR = 197
         ELSE
                MSGERR = 193
                WRITE(UNIT=FILENAME,FMT='(I3)') IER
                LENMSG = 3
         ENDIF
         RTNCODE = ' '
         NBCHR = 0          
         CALL GRAFNOTE(XLL,YLL,MSGERR,202,FILENAME,LENMSG,RTNCODE)
         GO TO 90
      ENDIF
      CALL GETCHAR(1,RTNCODE)
C
C---  ASK USER WHETHER HE WANTS TO DO A SCREEN PRINT, IF A PRINTER IS DEFINED 
C 
      IF (PRINTR(14:17) .EQ. 'QQQQ') THEN
         GO TO 90
      ENDIF
      RTNCODE = ' '
      NBCHR = 0
      CALL GRAFNOTE(XLL,YLL,525,503,' ',0,RTNCODE)
      IF (RTNCODE .EQ. YESUP .OR. RTNCODE .EQ. YESLO) THEN
C
C---     CHECK TO SEE IF CLICOM IS BEING RUN THRU ON A LAN.
C---     NETWORK=1 MEANS IBM PC LAN, SEND END-OF-SPOOL CHAR TO PRINT NOW
C  
         CALL SNAPST  
         CALL GETHAL(HALOID)
         IF (HALOID .GE. 'B' .AND. HALOID .LE. 'Z') THEN
            IF (NETWORK .EQ. 1) THEN
               CALL SPOOLEND
            ELSE
               CALL GRAFNOTE(0.3,0.8,454,202,' ',0,RTNCODE)
            ENDIF
         ENDIF
      ENDIF
  90  CALL FINHALO
      STOP ' ' 
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