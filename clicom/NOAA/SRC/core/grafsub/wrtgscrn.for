$STORAGE:2
      SUBROUTINE WRTGSCRN(PALETTE,PALDEF)
C------------------------------------------------------------------------------
C     SAVE THE CURRENT SCREEN AS A HALO GRAPHICS FILE. 
C
C     INPUT ARGUMENTS:
C
C     PALETTE       INT2       CURRENT PALETTE NUMBER
C     PALDEF        INT2 ARRAY CURRENT DEFINITION OF ALL 12 POSSIBLE PALETTES
C
C     OUTPUT ARGUMENTS:  NONE
C------------------------------------------------------------------------------
      INTEGER       PALETTE,PALDEF(16,12)
      CHARACTER*32  DESCR
      CHARACTER*24  FILENAME, DISKNM
      CHARACTER*8   SCRNAME
      CHARACTER*2   RTNCODE,NOUP,NOLO
      LOGICAL       FOUND,FIRSTCALL
C
C       ** LOCAL COMMON TO SAVE SPACE IN D-GROUP
C
      COMMON /WRTGSV/ FOUND,DESCR,FILENAME,DISKNM,SCRNAME,RTNCODE
C      
      DATA FIRSTCALL/.TRUE./
C
      IF (FIRSTCALL) THEN
         FIRSTCALL=.FALSE.
         CALL GETYN(2,2,NOUP,NOLO)
      ENDIF         
C            
      DISKNM      = '                        '
      DISKNM(1:8) = 'O:\DATA\'
C
  100 XLL = 0.3
      YLL = 0.9
      SCRNAME = ' '
      NBCHR = 0
C
C---  ASK THE USER TO SUPPLY A FILE NAME TO HOLD THE SCREEN DISPLAY
C 
      CALL GRAFMSG(XLL,YLL,411,527,' ',0,0,8,SCRNAME,NBCHR)
      IF (SCRNAME .EQ. 'ES') THEN
         RETURN
      ELSE
         IF (NBCHR .GT. 0) THEN
            IPOS = NBCHR + 9
            DISKNM(9:IPOS-1)   = SCRNAME(1:NBCHR)
            DISKNM(IPOS:IPOS+3)= '.SCR'
         ELSE
            GO TO 100
         ENDIF
      ENDIF
      INQUIRE(FILE=DISKNM,EXIST=FOUND)
C
C---  INQUIRE OF THE USER WHETHER HE WANTS TO REPLACE AN EXISTING FILE
C---  WITH THE CURRENT SCREEN DISPLAY OR TO USE A NEW FILE.
C
      IF (FOUND) THEN
         RTNCODE = ' '
         NBCHR = 0          
         CALL GRAFNOTE(XLL,YLL,529,505,' ',0,RTNCODE)
         IF (RTNCODE .EQ. 'ES') THEN
            RETURN
         ELSE
            IF (RTNCODE .EQ. NOUP .OR. RTNCODE .EQ. NOLO) THEN
               GO TO 100
            ENDIF                      
         ENDIF
      ENDIF
C
C---  GET DESCRIPTION OF A NEW SAVED SCREEN FOR INCLUSION IN GRAFSCRN.IDX FILE
C
      IF (.NOT.FOUND) THEN
         DESCR = ' '
         NBCHR = 0          
         CALL GRAFMSG(XLL,YLL,530,508,' ',0,1,32,DESCR,NBCHR)
         IF (DESCR .EQ. 'ES') THEN
            RETURN
         ELSE
            CALL SCRNIDX(SCRNAME,DESCR,PALETTE,PALDEF,.TRUE.)
         ENDIF
      ENDIF
C
C---  WRITE THE SCREEN TO THE NAMED FILE AND CHECK IT STATUS WHEN FINISHED
C
      CALL DELIMSTR(DISKNM,FILENAME)
      CALL GWRITE(FILENAME)
      CALL INQERR(IFN,IER)
C
C---- IF THERE IS A HALO ERROR CONDITION, WRITE A MESSAGE TO THE USER AND
C---- REMOVE THE JUST ADDED ENTRY IN SCREEN INDEX FILE
C
      IF (IER .NE. 0) THEN
         XLL = 0.4
         YLL = 0.2
         IF     (IER .EQ. 12) THEN
                MSGERR   = 199
                FILENAME = DISKNM
                LENMSG   = LNG(FILENAME)
         ELSEIF (IER .EQ. 13) THEN
                MSGERR   = 192
                FILENAME = DISKNM
                LENMSG   = LNG(FILENAME)
         ELSE
                MSGERR   = 195
                FILENAME = '   '
                WRITE(UNIT=FILENAME(1:2),FMT='(I2)') IER
                LENMSG   = 2
         ENDIF
         RTNCODE = ' '
         NBCHR = 0          
         CALL GRAFNOTE(XLL,YLL,MSGERR,202,FILENAME,LENMSG,RTNCODE)
         IF (RTNCODE .EQ. 'ES') THEN
            RETURN
         ELSE
            GO TO 100
         ENDIF
         CALL SCRNIDX(SCRNAME,DESCR,PALETTE,PALDEF,.FALSE.)
      ENDIF
      RETURN
      END
************************************************************************ 
      SUBROUTINE SCRNIDX(GRAFNAME,GRAFDESC,PALETTE,PALDEF,ADDTO)
C------------------------------------------------------------------------------
C     ROUTINE TO STORE THE CURRENT SCREEN NAME AND DESCRIPTION INTO THE 
C     GRAPH SCREEN INDEX FILE (O:\DATA\GRAFSCRN.IDX).  
C
C     INPUT ARGUMENTS:
C
C     GRAFNAME      CHAR       FILE NAME OF SAVED GRAPHICS SCREEN
C     GRAFDESC      CHAR       DESCRIPTION FOR GRAFNAME IN SCREEN INDEX FILE
C     PALETTE       INT2       CURRENT PALETTE NUMBER
C     PALDEF        INT2 ARRAY CURRENT DEFINITION OF ALL 12 POSSIBLE PALETTES
C     ADDTO         LOGICAL    TRUE = ADD TO INDEX, FALSE = REMOVE FROM INDEX
C
C     OUTPUT ARGUMENTS:  NONE
C------------------------------------------------------------------------------
      INTEGER       PALETTE,PALDEF(16,12)
      CHARACTER*8   GRAFNAME
      CHARACTER*32  GRAFDESC
      LOGICAL       ADDTO
C
C       ** LOCAL COMMON TO SAVE SPACE IN D-GROUP
C
      CHARACTER*1 CHRRTN,LNFEED
      CHARACTER*75 INREC
      COMMON/SIDXSV/CHRRTN,LNFEED,INREC
C            
      CHRRTN = CHAR(13)
      LNFEED = CHAR(10)
      OPEN (51,FILE='O:\DATA\GRAFSCRN.IDX',STATUS='UNKNOWN'
     +     ,FORM='BINARY',ACCESS='DIRECT',RECL=75)
      IF (ADDTO) THEN
C----    ADD AN ENTRY TO THE SCREEN INDEX FILE
         IDEL = 0
         DO 100 I = 1,9999
            READ(51,REC=I,ERR=110) INREC
            IF (INREC(1:8).EQ.GRAFNAME) THEN
               INREC(10:41) = GRAFDESC
               IDEL = 0
               GO TO 200
            ELSE IF (INREC(1:8).EQ.'********') THEN
               IDEL = I
            ENDIF
100      CONTINUE
110      CONTINUE
         WRITE(INREC,'(A8,1X,A32,16I2,2A1)') GRAFNAME, GRAFDESC, 
     +        (PALDEF(KQ,PALETTE),KQ=1,16), CHRRTN, LNFEED
C
200      CONTINUE
         IF (IDEL.GT.0) THEN
            WRITE(51,REC=IDEL) INREC
         ELSE
            WRITE(51,REC=I) INREC
         ENDIF
      ELSE
C----    REMOVE AN ENTRY TO THE SCREEN INDEX FILE
         DO 300 I = 1,9999
            READ(51,REC=I,ERR=310) INREC
            IF (INREC(1:8).EQ.GRAFNAME) THEN
               INREC(1:8) = '********'              
               INREC(10:41) = 'THIS RECORD DELETED'
               WRITE(51,REC=I) INREC
               GO TO 310
            ENDIF
300      CONTINUE
310      CONTINUE
      ENDIF
      CLOSE (51)
      RETURN
      END
