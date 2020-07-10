$STORAGE:2
      SUBROUTINE VWSTNLST(LSTNAM,J4REC,MAXREC,PRTLST)
C
C       .. ARGUMENTS
      CHARACTER*8  LSTNAM
      INTEGER*2    MAXREC
      INTEGER*4    J4REC(MAXREC)
      LOGICAL      PRTLST
C
C       .. LOCAL VARIABLES
C            J.......RECORD NUMBER FOR LAST RECORD READ FROM FILE
C            I.......NUMBER OF LINES DISPLAYED IN CURRENT PAGE -- HLDREC
C            I1......LINE NUMBER OF CURSOR ON CURRENT PAGE
C            JPAGE...RECORD NUMBER IN FILE FOR FIRST RECORD IN CURRENT PAGE 
C
      INTEGER*2 MAXLIN
      PARAMETER (MAXLIN=23)
C      
      INTEGER*2 RECLEN,SRTBEG, SRTLEN,WINWIDTH
      CHARACTER*2 INCHAR
      CHARACTER*3 DEVERS
      CHARACTER*20 HELPFILE,LSTFIL
      CHARACTER*80 INID,PREVID,HLDREC(24),INREC,PREVREC
      CHARACTER*80 MSGLN1,MSGLN2
      LOGICAL FOUND,FIRSTCALL,EOFFLG,FILOPN
C
      INTEGER*2 BUFFER(1300,2)
      COMMON /WINDOW/ BUFFER
C      
      DATA FIRSTCALL /.TRUE./, HELPFILE /'P:\HELP\VALWIN.HLP'/
      DATA IWINDOW /1/
C
C       ** J4REC IS A REAL ARRAY OUTSIDE THIS ROUTINE.  IT IS USED AS
C          INTEGER*4 ONLY TO SAVE SPACE.  INITIAL TO INTEGER VALUES.
C 
      DO 10 I=1,MAXREC
         J4REC(I)=0
   10 CONTINUE         
      IF (FIRSTCALL) THEN
         FIRSTCALL = .FALSE.
         CALL GETMSG(100,MSGLN1)
         MSGLEN=LNG(MSGLN1)
         CALL GETDEASE(DEVERS)
         IF (DEVERS.EQ.'4.0') THEN
            NMSG=492
         ELSE
            NMSG=491
         ENDIF
         CALL GETMSG(NMSG,MSGLN2)
         MSG2LEN=LNG(MSGLN2)+1
         CALL GETMSG(999,MSGLN2)
      END IF    
C
C  INITIALIZE
C
      EOFFLG=.FALSE.
      FILOPN = .FALSE.
      PREVID = '        '
      I1 = 1
      IPAGE = 0
      IDBEG = 3
      IDLEN = 8
      SRTBEG = 11
      SRTLEN = 24
      RECLEN = 36
      WINWIDTH = 45
      JMIN = 1
      J = JMIN
      IWIDTH = IDLEN + SRTLEN + 1
C
C       ** OPEN THE STATION SELECTION FILE
C
      LSTFIL = 'O:\DATA\'//LSTNAM
      NCHR = LNG(LSTFIL)
      LSTFIL(NCHR+1:) = '.LST'
      OPEN (51,FILE=LSTFIL,STATUS='OLD',
     +      ACCESS='DIRECT',RECL=36,IOSTAT=ISTAT)
      IF (ISTAT.NE.0) THEN
         FOUND=.FALSE.
         GO TO 120
      ENDIF   
      FILOPN=.TRUE.
      IF (LSTNAM.NE.' ' .AND. LSTNAM.NE.'STNGEOG') THEN
         CALL CKRECNBR(RTNCODE)
         IF (RTNCODE.NE.'0') THEN
            CALL GTRECNBR(RTNCODE)
         ENDIF   
      ENDIF   
      READ(51,REC=1) NUMSTN
      IF (NUMSTN.LE.0) THEN
         FOUND=.FALSE.
         GO TO 120
      ENDIF   
C      
      IF (PRTLST) THEN            
C
C          .. OPEN THE PRINTER FILE
         OPEN(50,FILE='PRN',STATUS='UNKNOWN',FORM='FORMATTED')
      ELSE   
C
C          .. DETERMINE WHERE THE WINDOW SHOULD BE OPENED
         CALL POSLIN(JROW,JCOL)
         IF (JCOL.LT.40) THEN
            ISTRT = 80 - WINWIDTH
         ELSE
            ISTRT = 0
         END IF
C
C          .. OPEN AND CLEAR THE SCREEN WINDOW
         IEND = ISTRT + WINWIDTH - 1
         CALL OPENWIN(IWINDOW,BUFFER(1,IWINDOW),0,ISTRT,24,IEND)
      ENDIF   
C
C      **  READ ONE SCREEN OF INPUT RECORDS (MAXLIN ENTRIES)
C
100   CONTINUE
      DO 105 I2 = 1,MAXLIN
         HLDREC(I2) = ' '
105   CONTINUE
      I = 0
      IPAGE = IPAGE + 1
      J4REC(IPAGE) = J + 1
110   CONTINUE
      FOUND = .TRUE.
      J = J + 1
      IF (J.GT.NUMSTN+1) GO TO 120
      READ(51,REC=J,ERR=120) (INREC(I3:I3),I3=1,RECLEN)
      INID = INREC(IDBEG:IDBEG+IDLEN-1)
      IF (PREVID.NE.'      ') THEN
         I = I + 1
         HLDREC(I) = PREVREC
      END IF
C
      PREVID = INID
      PREVREC = INID
      PREVREC(IDLEN+2:IDLEN+SRTLEN+1) = INREC(SRTBEG:SRTBEG+SRTLEN-1)
      IF (I.LT.MAXLIN) THEN
         GO TO 110
      END IF
      GO TO 130
C
C   GET HERE IF READ END OF FILE IN INPUT FILE
C
120   CONTINUE
      EOFFLG=.TRUE.
      I = I + 1
      IF (PREVID.EQ.'        ') THEN
         HLDREC(1) = '       '
         FOUND = .FALSE.
      ELSE
         HLDREC(I) = PREVREC
      END IF
C
C   DISPLAY THE SCREEN OF DATA
C
130   CONTINUE
      IF (PRTLST) THEN
         IF (FOUND) THEN
            DO 140 I2 = 1,I
               WRITE(50,500) HLDREC(I2)
  500          FORMAT(' ',A)          
  140       CONTINUE
         ENDIF
         IF (EOFFLG) THEN
            GO TO 300
         ELSE
            GO TO 180
         ENDIF        
      ELSE   
         IROW = 0
         ICOL = 0
         CALL CLRWIN(IWINDOW)
         IF (FOUND) THEN
            DO 150 I2 = 1,I
               ICOLOR=0
               CALL DSPREC(I2,ISTRT+1,ICOLOR,HLDREC(I2),IWIDTH)
  150       CONTINUE
         END IF
C   
C          .. WRITE FUNCTION LINE      
         CALL LOCATE(24,ISTRT+2,IERR)
         CALL WRTSTR(MSGLN2,MSG2LEN,0,3)
      ENDIF   
180   CONTINUE
      IF (PRTLST) THEN
         INCHAR='DP'
      ELSE   
         IF (.NOT.FOUND) THEN
            CALL CLRWIN(IWINDOW)
            CALL LOCATE(2,ISTRT+2,IERR)
            CALL WRTSTR(MSGLN1,38,15,3)
         END IF
         CALL GETCHAR(0,INCHAR)
      ENDIF   
C
C   IF HELP WANTED, DISPLAY IT AND ASK FOR NEXT USER INPUT.
C   OTHERWISE, REMOVE HIGHLIGHT ON THIS LINE IN ANTICIPATION OF MOVING
C   TO ANOTHER LINE.
C      
      IF (INCHAR.EQ.'1F') THEN
         CALL DSPWIN(HELPFILE)
         GO TO 180
      END IF
C
C   OTHERWISE, CHECK FOR PAGE, CURSOR, OR OTHER FUNCTION KEYS AND
C   TAKE THE APPROPRIATE ACTION.
C 
      IF (INCHAR.EQ.'DP') THEN
         I1 = I + 1
      ELSE IF (INCHAR.EQ.'UP') THEN
         I1 = 0
      ELSE IF (INCHAR.EQ.'HO') THEN
         I1 = 1
      ELSE IF (INCHAR.EQ.'EN') THEN
         I1 = I
      ELSE IF (INCHAR.EQ.'DA') THEN
      ELSE IF (INCHAR.EQ.'UA') THEN
         I1 = I1 - 1
      ELSE IF (INCHAR.EQ.'RE') THEN
         I1 = I1 + 1
      ELSE IF (INCHAR.EQ.'4F') THEN
         GO TO 300
      ELSE
         CALL BEEP
         GO TO 180   
      END IF
C
C  IF SELECTED LINE IS OFF THE PAGE, SCROLL PAGE
C
      IF (I1.GT.I) THEN
         IF (I.EQ.MAXLIN) THEN
            I1 = 1
            GO TO 100
         ELSE
            I1 = I
            CALL BEEP
         END IF
      ELSE IF (I1.LT.1) THEN
         IF (IPAGE.GT.1) THEN
            IPAGE = IPAGE - 2
            I1 = MAXLIN
            J = J4REC(IPAGE+1) - 1
            PREVID = '        '
            GO TO 100
         ELSE
            I1 = 1
            CALL BEEP
         END IF
      END IF
      GO TO 180  

300   CONTINUE
      IF (PRTLST) THEN
         NCHR=LNG(LSTFIL)
         IF (FOUND) THEN
            CALL WRTMSG(2,595,12,1,0,LSTFIL,NCHR)
         ELSE
            CALL WRTMSG(2,100,12,1,0,'  :'//LSTFIL,NCHR+3)
         ENDIF   
         CLOSE(50)
      ELSE   
C
C          .. CLOSE WINDOW, RESTORE SCREEN AND RETURN
         CALL CLOSWIN(IWINDOW,BUFFER(1,IWINDOW))
      ENDIF   
      IF (FILOPN) CLOSE(51)
C
      RETURN
      END      
