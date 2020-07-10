$STORAGE:2
      SUBROUTINE GETDSET(DATTYP,DATAOPT,MSGTYP,NTTL,TTLSAV,
     +                   INCSET,RTNCODE)
C
C       ** OBJECTIVE:  
C       ** INPUT:
C             DATTYP....FLAG TO INDICATE METHOD TO FIND START OF BAND
C                       0=RECORD POSITION WITHIN FRAME
C                       1=SET INTERVAL OF TIME RECORDS
C             DATAOPT...FLAG INDICATING DIRECTION TO POSITION FILE
C             MSGTYP....FLAG TO INDICATE METHOD TO OUTPUT MESSAGES
C             NTTL......DIMENSION OF TTLSAV
C       ** OUTPUT:
C             TTLSAV....ARRAY TO SAVE TITLES/SUBTITLES
C             INCSET....FLAG TO INDICATE AN INCOMPLETE BAND
C             RTNCODE...ERROR FLAG
C
      INTEGER*2 DATTYP,DATAOPT,MSGTYP,NTTL,INCSET
      CHARACTER*(*) TTLSAV(2,NTTL)
      CHARACTER*1 RTNCODE
C
      IF (DATTYP.EQ.0) THEN
         CALL GETPSET(DATAOPT,MSGTYP,NTTL,TTLSAV,INCSET,RTNCODE)
      ELSE
         CALL GETISET(DATAOPT,MSGTYP,NTTL,TTLSAV,INCSET,RTNCODE)
      ENDIF
C
      RETURN
      END            
      SUBROUTINE GETPSET(DATAOPT,MSGTYP,NTTL,TTLSAV,INCSET,RTNCODE)
C
C       ** INPUT:
C             DATAOPT....FLAG INDICATING METHOD OF GETTING BAND
C                        0=OPEN FILE, READ CURRENT BAND
C                        1=REWIND FILE, READ FIRST BAND
C                        2=READ PREVIOUS BAND
C                        3=READ NEXT BAND
C                        4=READ CURRENT BAND
C             MSGTYP.....FLAG TO INDICATE METHOD TO OUTPUT MESSAGES
C                        1=TEXT    2=GRAPHICS
C             NTTL.......DIMENSION OF TTLSAV
C                         1=DO NOT SAVE TITLES
C                        >1=SAVE TITLES FOR EACH FRAME
C       ** OUTPUT:
C             TTLSAV.....ARRAY TO SAVE TITLES/SUBTITLES
C             INCSET.....FLAG TO INDICATE AN INCOMPLETE BAND
C                        0=COMPLETE   1=INCOMPLETE
C             RTNCODE....      
C
C       ** VARIABLE DEFINITIONS:
C             FRM1D......FRAME NUMBER IN MEMORY FOR THE START OF CURRENT BAND
C             FRM2D......FRAME NUMBER IN MEMORY FOR THE END OF CURRENT BAND
C             FRMPTR.....ARRAY POSITION FOR THE START OF EACH FRAME IN MEMORY
C             NBRFRM.....TOTAL NUMBER OF FRAMES OF DATA IN MEMORY
C             NBRFSV.....OLD VALUE FOR NBRFRM.  USED IF EOF ENCOUNTERED WITH
C                        NO DATA READ
C             NFRMBG.....ARRAY POSITION TO START ADDING DATA FOR THE NEXT
C                        FRAME THAT WILL BE READ
C
      INTEGER*2 DATAOPT,MSGTYP,NTTL,INCSET
      CHARACTER*(*) TTLSAV(2,NTTL)
      CHARACTER *1 RTNCODE
C      
$INCLUDE: 'GRFPARM.INC'
$INCLUDE: 'GRAFVAR.INC'
$INCLUDE: 'DATAVAL.INC'
$INCLUDE: 'FRMPOS.INC'
C
C       ** GRFPARM REQUIRED FOR LENTXTD
      CHARACTER *(LENTXTD) FRMTITLE,FRMSUB
      CHARACTER *1 RTNSET
      CHARACTER *2 INCHAR      
      CHARACTER*14 MSGTXT
      INTEGER*2    FILOPT,LENMSG
C      
      PARAMETER (MAXERR=3,MAXMSG=6)
      INTEGER*2 MSGIDX(MAXERR),MSGNBR(MAXMSG)
      DATA MSGNBR /382,383,191,385,386,388/
C
      NERR = 0
      NFRMBG = 1
      NBRFSV = NBRFRM
      NBRFRM = 0
      FRMPTR(1) = 1
      FRM1D = 1
      RTNSET = '0'
      RTNCODE='0'
      IF (IGRAPH.EQ.1) THEN
        NCVBG=2
        NFRMRD=NFRSET
      ELSE
        NCVBG=1
        NFRMRD=1
      ENDIF   
C      
      IF (DATAOPT.EQ.0) THEN
C      
C          ** DATAOPT=0 -- OPEN FILE; READ CURRENT BAND
         INCSET=0   
         FILOPT=0
         NBRFSV = 0
         CALL RDFRAME(CVAL(NCVBG),RVAL,MXDATROW,NFRMBG,FILOPT,IGRAPH,
     +                NUMCOL,COLHDR,FRMTITLE,FRMSUB,NFRMFN,RTNCODE)
         IF (RTNCODE.NE.'0') THEN
            NERR=NERR+1
            MSGIDX(NERR)=ICHAR(RTNCODE)-48
            GO TO 900
         ENDIF   
         FILOPT=4
      ELSE IF (DATAOPT.EQ.1) THEN
C      
C          ** DATAOPT=1 -- REWIND FILE; READ FIRST BAND
         INCSET=0   
         FILOPT=1
      ELSE IF (DATAOPT.EQ.2) THEN
C      
C          ** DATAOPT=2 -- POSITION FILE TO START OF PREVIOUS BAND
         NPREV=NFRMRD
         IF (HIROW.LT.LOWROW) NPREV=NPREV-1
         FILOPT = 5
         DO 12 I=1,NPREV
            CALL RDFRAME(CVAL(NCVBG),RVAL,MXDATROW,NFRMBG,FILOPT,IGRAPH,
     +                   NUMCOL,COLHDR,FRMTITLE,FRMSUB,NFRMFN,RTNCODE)
            IF (RTNCODE.NE.'0') GO TO 13
   12    CONTINUE
   13    CONTINUE
         IF (RTNCODE.NE.'0') THEN
            NERR=NERR+1
            MSGIDX(NERR)=ICHAR(RTNCODE)-48
            IF (RTNCODE.EQ.'2') THEN
C                .. BEGINNING OF FILE 
               IF (I.EQ.1) THEN
C
C                   .. DATA ALREADY POSITIONED AT CORRECT BEGINNING;
C                      PRINT MESSAGE;  NON-ZERO ERROR CODE LEAVES 
C                      CURRENT PLOT ON SCREEN           
                  NBRFRM = NBRFSV
                  GO TO 900
               ELSE   
C                   .. DATA NOT ALIGNED CORRECTLY AT FILE BEGINNING;
C                      RESET START OF DATA TO BAND 1; ERROR CODE
C                      OF ZERO CAUSES PLOT TO BE REDRAWN
                  NERR=NERR+1
                  MSGIDX(NERR)=6
                  RTNCODE = '0'
               ENDIF   
            ELSE
C
C                .. ERROR OTHER THAN BEGINNING OF FILE            
               GO TO 900
            ENDIF
         ENDIF
C
C          .. FILE IS NOW POSITIONED CORRECTLY TO PREVIOUS BAND
         INCSET=0   
         FILOPT=4
      ELSE IF (DATAOPT.EQ.3) THEN
C      
C          ** DATAOPT=3 -- READ NEXT BAND
         NNXT=NFRMRD
         IF (HIROW.LT.LOWROW) NNXT=NNXT-1
C          .. POSITION FILE TO START OF NEXT BAND
         FILOPT = 6
         DO 15 I=1,NNXT
            CALL RDFRAME(CVAL(NCVBG),RVAL,MXDATROW,NFRMBG,FILOPT,IGRAPH,
     +                   NUMCOL,COLHDR,FRMTITLE,FRMSUB,NFRMFN,RTNCODE)
            IF (RTNCODE.NE.'0') GO TO 16
   15    CONTINUE
   16    CONTINUE
         IF (RTNCODE.NE.'0') THEN
            NERR=NERR+1
            MSGIDX(NERR)=ICHAR(RTNCODE)-48
            IF (RTNCODE.EQ.'1') THEN
C                .. END OF FILE; ERROR CODE WILL BE SET TO NON-ZERO ON
C                   EXIT TO LEAVE CURRENT PLOT ON SCREEN
               NFRMCUR = I
               IF (RTNSET.EQ.'0') RTNSET=RTNCODE
               RTNCODE = '0'
               GO TO 50
            ELSE   
               GO TO 900
            ENDIF
         ENDIF
         FILOPT = 4
      ELSE
C
C          ** DATAOPT=4 -- READ CURRENT BAND      
         INCSET=0   
         FILOPT=4
      ENDIF   
C
C       ** READ DATA FOR EACH FRAME
C
      NFRMFN=0
      DO 20 I=1,NFRMRD
         NFRMBG = NFRMFN+1
         CALL RDFRAME(CVAL(NCVBG),RVAL,MXDATROW,NFRMBG,FILOPT,IGRAPH,
     +                NUMCOL,COLHDR,FRMTITLE,FRMSUB,NFRMFN,RTNCODE)
         IF (RTNCODE.NE.'0') GO TO 21
         NBRFRM = NBRFRM+1
         IF (NBRFRM.GT.1) FRMPTR(NBRFRM)=FRMPTR(NBRFRM-1)+NRECFRM
         NRECFRM=NFRMFN-NFRMBG+1
         IF (I.EQ.1) THEN
C             .. USE TITLE/SUBTITLE OF FIRST FRAME IN BAND
            DATATITLE=FRMTITLE
            DATASUB  =FRMSUB
C            
            NDATROW=0
            NPLTROW=0
            LORFRM  = MIN0(LOWROW,NRECFRM)
         ENDIF
         NDATROW=NDATROW+NRECFRM
         HIRFRM=NRECFRM
         IF (NTTL.GT.1) THEN
            TTLSAV(1,NBRFRM) = FRMTITLE
            TTLSAV(2,NBRFRM) = FRMSUB
         ENDIF   
         FILOPT=3
   20 CONTINUE
      I=NFRMRD+1
      HIRFRM=MIN0(HIROW,HIRFRM)
   21 CONTINUE
      IF (RTNCODE.NE.'0') THEN
         NERR=NERR+1
         MSGIDX(NERR)=ICHAR(RTNCODE)-48
         IF (RTNCODE.EQ.'1' .AND. NBRFRM.GT.0) THEN
C             .. END OF FILE -- INCOMPLETE BAND
            INCSET=1
            NERR=NERR+1
            MSGIDX(NERR)=5
            RTNCODE = '0'
         ELSE   
            GO TO 900
         ENDIF
      ENDIF
      NFRMCUR=I-1
      IF (NFRMCUR.GT.0) THEN
         NPLTROW = MAX0((NDATROW-(LORFRM-1)-(NRECFRM-HIRFRM)),1)
C          .. ADD TITLE OF LAST FRAME IN BAND ONLY IF THERE ARE MULTIPLE
C             FRAMES AND IT IS DIFFERENT
         IF (DATATITLE.NE.FRMTITLE) THEN
            NCHR=LNG(DATATITLE)
            DATATITLE(NCHR+1:) = '-'//FRMTITLE
         ENDIF   
C          .. ADD SUBTITLE OF LAST FRAME IN BAND IF THERE ARE MULTIPLE FRAMES
         IF (NBRFRM.GT.1) THEN
            NCHR =LNG(DATASUB)
            NCHR2=LNG(FRMSUB)
            IF (IGRAPH.EQ.1) THEN
               CALL SQUEZDAT(DATASUB,FRMSUB)
            ELSE IF (NCHR+NCHR2+1 .LE. LENTXTD) THEN
               DATASUB(NCHR+1:) = '-'//FRMSUB
            ELSE
               CALL SQUEZDAT(DATASUB,FRMSUB)
            ENDIF   
         ENDIF   
      ENDIF   
C
C       .. BLANK FILL UNUSED PORTION OF X-AXIS LABEL ARRAY
      IF (IGRAPH.EQ.1) THEN
         DO 45 I=NDATROW+NCVBG,MXDATROW
            CVAL(I)=' '
   45    CONTINUE         
      ENDIF
C
   50 CONTINUE     
C
C       ** POSITION FILE TO START OF BAND
C
      IF (NBRFRM.EQ.0) NBRFRM=NBRFSV
      FRM2D = NBRFRM
      FILOPT = 5
      NPREV=NFRMCUR-1
      DO 55 I=1,NPREV
         CALL RDFRAME(CVAL(NCVBG),RVAL,MXDATROW,NFRMBG,FILOPT,IGRAPH,
     +                NUMCOL,COLHDR,FRMTITLE,FRMSUB,NFRMFN,RTNCODE)
         IF (RTNCODE.NE.'0') GO TO 56
   55 CONTINUE
   56 CONTINUE
      IF (RTNCODE.NE.'0') THEN
         NERR=NERR+1
         MSGIDX(NERR)=ICHAR(RTNCODE)-48
         IF (RTNCODE.EQ.'2') THEN
C             .. ERROR: ATTEMPT TO POSITION FILE BEFORE START OF DATA
            RTNCODE='0'
         ENDIF
      ENDIF
C
      IF (NERR.GT.0) GO TO 900      
C      
      RETURN
C
C       ** ERROR PROCESSING
C
  900 CONTINUE
         DO 905 I=1,NERR
            MSGN1 = MSGNBR(MSGIDX(I))
            IF (MSGIDX(I).EQ.1 .OR. MSGIDX(I).EQ.3) THEN
               MSGTXT = '  GRAPHICS.API'
               LENMSG = 14
            ELSE
               MSGTXT = ' '
               LENMSG = 0
            ENDIF      
            IF (MSGTYP.EQ.1) THEN
C
C                .. TEXT MESSAGE
               CALL WRTMSG(2,MSGN1,12,1,1,MSGTXT,LENMSG)
            ELSE
C
C                .. GRAPHICS MESSAGE         
               MSGN2=202
               XWIN=.1
               YWIN=.95
               CALL GRAFNOTE(XWIN,YWIN,MSGN1,MSGN2,MSGTXT,LENMSG,INCHAR)
            ENDIF 
  905    CONTINUE      
         IF (RTNSET.NE.'0') RTNCODE=RTNSET
C  
      RETURN      
      END
      SUBROUTINE GETISET(DATAOPT,MSGTYP,NTTL,TTLSAV,INCSET,RTNCODE)
C
C       ** OBJECTIVE:  READ A BAND FROM .API FILE;  FILE POSITIONING
C                      IS BASED ON A SET COMPOSED OF A PREDEFINED NUMBER OF
C                      TIMES;  THIS ROUTINE IS USED ONLY FOR TIMESERIES PLOTS
C                      AS OF 8-8-91 THE DEFAULT FOR MAP IS POSITION; IT CANNOT
C                      BE CHANGED; INTERVAL WILL NOT CURRENTLY WORK.
C
C       ** INPUT:
C             DATAOPT....FLAG INDICATING METHOD OF GETTING BAND
C                        0=OPEN FILE, READ CURRENT BAND
C                        1=REWIND FILE, READ FIRST BAND
C                        2=READ PREVIOUS BAND
C                        3=READ NEXT BAND
C                        4=READ CURRENT BAND
C             MSGTYP.....FLAG TO INDICATE METHOD TO OUTPUT MESSAGES
C                        1=TEXT    2=GRAPHICS
C             NTTL.......DIMENSION OF TTLSAV
C                         1=DO NOT SAVE TITLES
C                        >1=SAVE TITLES FOR EACH FRAME
C       ** OUTPUT:
C             TTLSAV.....ARRAY TO SAVE TITLES/SUBTITLES
C             INCSET.....FLAG TO INDICATE AN INCOMPLETE BAND
C                        0=COMPLETE   1=INCOMPLETE
C             RTNCODE....FLAG TO INDICATE ERROR STATUS
C                        '0'=NO ERROR
C                        '1'=END OF FILE
C                        '2'=POSITION FILE BEFORE START OF DATA
C                        '3'=ERROR IN READING FILE
C                        '4'=ARRAY SIZE TOO SMALL FOR DATA
C                        '5'=INTERVAL BANDS ALLOWED ONLY FOR TIMESERIES/MAP
C
      INTEGER*2 DATAOPT,MSGTYP,NTTL,INCSET
      CHARACTER*(*) TTLSAV(2,NTTL)
      CHARACTER *1 RTNCODE
C      
$INCLUDE: 'GRFPARM.INC'
$INCLUDE: 'GRAFVAR.INC'
$INCLUDE: 'DATAVAL.INC'
$INCLUDE: 'FRMPOS.INC'
C
C       ** GRFPARM REQUIRED FOR LENTXTD
      CHARACTER *(LENTXTD) FRMTITLE,FRMSUB,BEGSUB,BEGTITLE
      CHARACTER *1 RTNSET
      CHARACTER *2 INCHAR      
      CHARACTER*14 MSGTXT
      INTEGER*2 FILOPT,LENMSG
C      
      PARAMETER (MAXERR=3,MAXMSG=7)
      INTEGER*2 MSGIDX(MAXERR),MSGNBR(MAXMSG)
      DATA MSGNBR /382,383,191,385,386,389,390/
C
      NERR = 0
      NFRMBG = 1
      RTNCODE='0'
      RTNSET = '0'
      FRM1D=1
      IF (IGRAPH.EQ.1) THEN
        NCVBG=2
      ELSE
        NCVBG=1
      ENDIF   
      IF (IGRAPH.NE.1 .AND. IGRAPH.NE.2) THEN
         NERR=NERR+1
         MSGIDX(NERR)=6
         RTNCODE='5'
         GO TO 900
      ENDIF   
C      
      IF (DATAOPT.EQ.0) THEN
C      
C          ** OPEN FILE; READ CURRENT BAND
         INCSET=0
         FILOPT=0
         CALL RDFRAME(CVAL(NCVBG),RVAL,MXDATROW,NFRMBG,FILOPT,IGRAPH,
     +                NUMCOL,COLHDR,FRMTITLE,FRMSUB,NFRMFN,RTNCODE)
         IF (RTNCODE.NE.'0') THEN
            NERR=NERR+1
            MSGIDX(NERR)=ICHAR(RTNCODE)-48
            GO TO 900
         ENDIF   
         FILOPT=4
         NBRFRM=0
         IF (LORFRM.EQ.0) LORFRM=LOWROW
      ELSE IF (DATAOPT.EQ.1) THEN
C      
C          ** REWIND FILE; READ FIRST BAND
         INCSET=0
         FILOPT=1
         NBRFRM=0
         LORFRM=LOWROW
      ELSE IF (DATAOPT.EQ.2) THEN
C      
C          ** POSITION FILE TO START OF PREVIOUS BAND
         NFRMBG = 1
         FILOPT = 5
         IF (LORFRM.EQ.1) THEN
C             .. FIRST FRAME OF CURRENT BAND IS NOT PART OF PREVIOUS BAND
            NRECIN = 0
            ENDFRM = 0
         ELSE
C             .. FIRST FRAME OF CURRENT BAND INCLUDES END OF PREVIOUS BAND     
            NRECIN =  LORFRM-1   
            ENDFRM = 1
         ENDIF   
C        
         IF (NBRFRM.EQ.1) THEN 
            NRECF1 = NDATROW
         ELSE
            NRECF1 = FRMPTR(2)-1
         ENDIF      
         NRECREQ = NFRSET
         NRECDAT = NFRSET
         NPREV   = 0
C
   10    CONTINUE
         IF (NRECREQ.LE.NRECIN) GO TO 12
            NRECREQ = NRECREQ - NRECIN
            CALL RDFRAME(CVAL(NCVBG),RVAL,MXDATROW,NFRMBG,FILOPT,IGRAPH,
     +                   NUMCOL,COLHDR,FRMTITLE,FRMSUB,NFRMFN,RTNCODE)
            IF (RTNCODE.NE.'0') GO TO 12
            NRECFRM=NFRMFN-NFRMBG+1
            NRECIN = NRECFRM
            ENDFRM = 0
            NPREV = NPREV + 1
            GO TO 10
   12    CONTINUE
         IF (RTNCODE.NE.'0') THEN
            NERR=NERR+1
            MSGIDX(NERR)=ICHAR(RTNCODE)-48
            IF (RTNCODE.EQ.'2') THEN
C                .. BEGINNING OF FILE 
               IF (NPREV.EQ.0 .AND. LORFRM.EQ.LOWROW) THEN
C
C                   .. DATA ALREADY POSITIONED AT CORRECT BEGINNING;
C                      PRINT MESSAGE;  NON-ZERO ERROR CODE LEAVES 
C                      CURRENT PLOT ON SCREEN           
                  GO TO 900
               ELSE
C                   .. DATA NOT ALIGNED CORRECTLY AT FILE BEGINNING;
C                      RESET BAND 1 TO LOWROW IN FRAME 1; ERROR CODE
C                      OF ZERO CAUSES PLOT TO BE REDRAWN
                  NERR=NERR+1
                  MSGIDX(NERR)=7
                  RTNCODE = '0'
                  LORFRM = LOWROW
                  NBRFRM = 0
               ENDIF    
            ELSE   
C
C                .. ERROR OTHER THAN BEGINNING OF FILE            
               GO TO 900
            ENDIF
         ELSE   
C
C             .. FILE IS NOW POSITIONED CORRECTLY TO PREVIOUS BAND;
C                CALCULATE STARTING RECORD IN FIRST FRAME OF DATA         
            NBRFRM = ENDFRM
            IF (NBRFRM.EQ.1) THEN
C                .. ALL DATA FOR PREVIOUS BAND IS CONTAINED IN FIRST FRAME
C                   OF LAST BAND USED; ALL DATA IN MEMORY; NO READ NECESSARY   
               FRMSUB = BEGSUB
               FRMTITLE = BEGTITLE
               NDATROW = NRECF1
               LORFRM = LORFRM - NRECDAT 
            ELSE
C                .. FIRST FRAME OF BAND MUST BE READ (NBRFRM=0)            
               LORFRM = NRECFRM - NRECREQ + 1
            ENDIF      
         ENDIF
         INCSET=0
         FILOPT=4
      ELSE IF (DATAOPT.EQ.3) THEN
C      
C          ** READ NEXT BAND
C
C          .. POSITION FILE TO START OF NEXT BAND
         IF (NBRFRM.GT.1) THEN
            NNXT = NBRFRM-1
         ELSE
            NNXT = 0
         ENDIF
         NRECFRM = NDATROW-FRMPTR(NBRFRM)+1
         IF (HIRFRM+1 .GT. NRECFRM) NNXT=NNXT+1
         IF (NNXT.GT.0) THEN      
C
C             .. SKIP RECORDS TO START OF NEXT BAND         
            FILOPT = 6
            DO 15 I=1,NNXT
               CALL RDFRAME(CVAL(NCVBG),RVAL,MXDATROW,NFRMBG,FILOPT,
     +              IGRAPH,NUMCOL,COLHDR,FRMTITLE,FRMSUB,NFRMFN,RTNCODE)
               IF (RTNCODE.NE.'0') GO TO 16
   15       CONTINUE
   16       CONTINUE
            IF (RTNCODE.NE.'0') THEN
               NERR=NERR+1
               MSGIDX(NERR)=ICHAR(RTNCODE)-48
               IF (RTNCODE.EQ.'1') THEN
C                   .. END OF FILE
                  IF (RTNSET.EQ.'0') RTNSET=RTNCODE
                  RTNCODE = '0'
                  GO TO 50
               ELSE   
                  GO TO 900
               ENDIF
            ENDIF
            FILOPT = 4
            NBRFRM = 0
         ELSE
C
C             .. FILE ALREADY POSITIONED AT START OF NEXT BAND (NBRFRM=1)
C                FIRST FRAME OF BAND ALREADY IN MEMORY -- SAME AS FIRST FRAME
C                OF LAST BAND USED
            FRMSUB = BEGSUB
            FRMTITLE = BEGTITLE
            FILOPT = 3   
         ENDIF  
C
C          .. DEFINE STARTING POSITION IN FIRST FRAME OF BAND         
         IF (HIRFRM+1 .GT. NRECFRM) THEN
            LORFRM = 1
         ELSE
            LORFRM = HIRFRM + 1   
         ENDIF    
      ELSE 
C      
C          ** DATAOPT=4 -- READ CURRENT BAND
         FILOPT=4
         INCSET=0
         NBRFRM=0
      ENDIF   
C
C          ** READ FORWARD
C
         IF (NBRFRM.EQ.0) THEN
C             .. READ FIRST FRAME OF BAND INTO MEMORY         
            NFRMBG = 1
            CALL RDFRAME(CVAL(NCVBG),RVAL,MXDATROW,NFRMBG,FILOPT,IGRAPH,
     +                   NUMCOL,COLHDR,FRMTITLE,FRMSUB,NFRMFN,RTNCODE)
            IF (RTNCODE.NE.'0') THEN
               NERR=NERR+1
               MSGIDX(NERR)=ICHAR(RTNCODE)-48
               GO TO 900
            ENDIF
            NBRFRM = 1
            FRMPTR(NBRFRM) = 1
            NRECFRM = NFRMFN-NFRMBG+1 
            NDATROW = NRECFRM
C             .. SAVE TITLE/SUBTITLE OF FIRST FRAME IN MEMORY
            BEGTITLE  = FRMTITLE
            BEGSUB    = FRMSUB
C             .. SET FLAG TO READ NEXT FRAME            
            FILOPT = 3
         ELSE
C             .. FIRST FRAME OF BAND IS ALREADY IN MEMORY -- SAME AS FIRST
C                FRAME OF LAST BAND USED
            NFRMFN = NDATROW
            NRECFRM = NDATROW-FRMPTR(NBRFRM)+1   
         ENDIF
C          .. USE TITLE/SUBTITLE OF FIRST FRAME IN BAND
         DATATITLE = FRMTITLE
         DATASUB   = FRMSUB
C          .. SAVE TITLES/SUBTITLES OF ALL FRAMES FOR GRAFOPTN         
         IF (NTTL.GT.1) THEN
            TTLSAV(1,NBRFRM) = FRMTITLE
            TTLSAV(2,NBRFRM) = FRMSUB
         ENDIF   
C
         NRECREQ = NFRSET
         NRECIN  = NRECFRM - LORFRM + 1           
         NRECDAT = NFRSET
C
   20    CONTINUE
         IF (NRECREQ.LE.NRECIN) GO TO 30
            NRECREQ = NRECREQ - NRECIN
            NFRMBG = NFRMFN+1
            CALL RDFRAME(CVAL(NCVBG),RVAL,MXDATROW,NFRMBG,FILOPT,IGRAPH,
     +                   NUMCOL,COLHDR,FRMTITLE,FRMSUB,NFRMFN,RTNCODE)
            IF (RTNCODE.NE.'0') GO TO 30
            NBRFRM = NBRFRM + 1
            FRMPTR(NBRFRM) = FRMPTR(NBRFRM-1) + NRECFRM
C             .. SAVE TITLES/SUBTITLES OF ALL FRAMES FOR GRAFOPTN         
            IF (NTTL.GT.1) THEN
               TTLSAV(1,NBRFRM) = FRMTITLE
               TTLSAV(2,NBRFRM) = FRMSUB
            ENDIF   
            NRECFRM=NFRMFN-NFRMBG+1
            NDATROW=NDATROW+NRECFRM
            NRFDAT=NRECREQ
            NRECIN = NRECFRM
            FILOPT=3
            GO TO 20
   30    CONTINUE
         IF (RTNCODE.NE.'0') THEN
            NERR=NERR+1
            MSGIDX(NERR)=ICHAR(RTNCODE)-48
            IF (RTNCODE.EQ.'1') THEN
C                .. END OF FILE -- INCOMPLETE BAND
               NERR=NERR+1
               MSGIDX(NERR)=5
               RTNCODE = '0'
               INCSET=1
               NRECDAT = NFRSET - NRECREQ
               NRFDAT  = NRECFRM
            ELSE   
               GO TO 900
            ENDIF
         ENDIF
         IF (NBRFRM.EQ.1) THEN
            HIRFRM = LORFRM + NRECDAT - 1
         ELSE
            HIRFRM = NRFDAT
         ENDIF
         NPLTROW = NRECDAT      
C          .. ADD TITLE OF LAST FRAME IN BAND ONLY IF THERE ARE MULTIPLE
C             FRAMES AND IT IS DIFFERENT
         IF (DATATITLE.NE.FRMTITLE) THEN
            NCHR=LNG(DATATITLE)
            DATATITLE(NCHR+1:) = '-'//FRMTITLE
         ENDIF   
C          .. ADD SUBTITLE OF LAST FRAME IN BAND IF THERE ARE MULTIPLE FRAMES
         IF (NBRFRM.GT.1) THEN
            NCHR =LNG(DATASUB)
            NCHR2=LNG(FRMSUB)
            IF (IGRAPH.EQ.1) THEN
               CALL SQUEZDAT(DATASUB,FRMSUB)
            ELSE IF (NCHR+NCHR2+1 .LE. LENTXTD) THEN
               DATASUB(NCHR+1:) = '-'//FRMSUB
            ELSE
               CALL SQUEZDAT(DATASUB,FRMSUB)
            ENDIF   
         ENDIF   
C
C       .. BLANK FILL UNUSED PORTION OF X-AXIS LABEL ARRAY
      IF (IGRAPH.EQ.1) THEN
         DO 45 I=NDATROW+NCVBG,MXDATROW
            CVAL(I)=' '
   45    CONTINUE         
      ENDIF
C
   50    CONTINUE     
C
C          ** POSITION FILE TO START OF BAND
C
         FRM2D = NBRFRM
         FILOPT = 5
         NPREV=NBRFRM-1
         DO 55 I=1,NPREV
            CALL RDFRAME(CVAL(NCVBG),RVAL,MXDATROW,NFRMBG,FILOPT,IGRAPH,
     +                   NUMCOL,COLHDR,FRMTITLE,FRMSUB,NFRMFN,RTNCODE)
            IF (RTNCODE.NE.'0') GO TO 56
   55    CONTINUE
   56    CONTINUE
         IF (RTNCODE.NE.'0') THEN
            NERR=NERR+1
            MSGIDX(NERR)=ICHAR(RTNCODE)-48
            IF (RTNCODE.EQ.'2') THEN
C                .. ERROR: ATTEMPT TO POSITION FILE BEFORE START OF DATA
               RTNCODE='0'
            ENDIF
         ENDIF
C
      IF (NERR.GT.0) GO TO 900      
C      
      RETURN
C
C       ** ERROR PROCESSING
C
  900 CONTINUE
         DO 905 I=1,NERR
            MSGN1 = MSGNBR(MSGIDX(I))
            IF (MSGIDX(I).EQ.1 .OR. MSGIDX(I).EQ.3) THEN
               MSGTXT = '  GRAPHICS.API'
               LENMSG = 14
            ELSE
               MSGTXT = ' '
               LENMSG = 0
            ENDIF      
            IF (MSGTYP.EQ.1) THEN
C
C                .. TEXT MESSAGE
               CALL WRTMSG(2,MSGN1,12,1,1,MSGTXT,LENMSG)
            ELSE
C
C                .. GRAPHICS MESSAGE         
               MSGN2=202
               XWIN=.1
               YWIN=.95
               CALL GRAFNOTE(XWIN,YWIN,MSGN1,MSGN2,MSGTXT,LENMSG,INCHAR)
            ENDIF 
  905    CONTINUE      
         IF (RTNSET.NE.'0') RTNCODE=RTNSET
C  
      RETURN      
      END
