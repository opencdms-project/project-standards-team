$STORAGE:2 
      SUBROUTINE GRAFOPTN(INCSET,IDPLT,NCPLTA,NCPLTB,DATARRAY,MXDATROW,
     +                    NDATROW,NPLTROW,ROWHDR,ACTFLG,RTNCODE)
C      
C   ROUTINE TO ALLOW USERS TO MODIFY GRAPHICS OPTIONS FOR GRAPHS IN THE
C   CLICOM GRAPHICS SYSTEM.  IT DISPLAYS A SPREADSHEET-LIKE FORM WITH
C   A GRAPH CONTROL AREA AT THE TOP AND THE DATA AREA BELOW.
C
C       ** INPUT:
C             INCSET.....FLAG TO INDICATE AN INCOMPLETE BAND
C                        0=COMPLETE   1=INCOMPLETE
C             IDPLT......PLOT NUMBER OF THE CURRENT PLOT IN THIS BAND
C             NCPLTA.....
C             NCPLTB.....
C             DATARRAY...REAL ARRAY OF PLOT DATA
C             MXDATROW...INNER DIMENSION OF DATARRAY
C             NDATROW....
C             ROWHDR.....
C             ACTFLG.....ACTION FLAG; INDICATES METHOD OF ENTRY
C                        0=INITIAL ENTRY INTO ROUTINE OR
C                          REENTRY AFTER REWIND FRAME
C                        1=REENTRY INTO ROUTINE AFTER FILE POSTIONING
C                        2=REENTRY INTO ROUTINE AFTER ADD FRAME
C       ** OUTPUT:
C             ACTFLG.....ACTION FLAG; INDICATES METHOD OF EXIT
C                         0=EXIT AFTER COMPLETING ROUTINE
C                         1=REWIND FILE; READ FIRST BAND; RETURN TO GRAFOPTN
C                         2=READ PREVIOUS BAND; RETURN TO GRAFOPTN
C                         3=READ NEXT BAND; RETURN TO GRAFOPTN
C                         4=READ CURRENT BAND; EXIT ROUTINE
C                         5=ADD A FRAME TO CURRENT BAND; RETURN TO GRAFOPTN
C                         6=REWIND BAND; RETURN TO GRAFOPTN--MAY BE USED TO 
C                           ALIGN PLOT WIDTH AND FIRST DATA COLUMN WHEN PLOT 
C                           WIDTH HAS BEEN CHANGED
C
C                        NOTE:  FOR VALUES=0,6 REWIND FILE(1) MAY BE 
C                               COMBINED WITH THE SPECIFIED ACTION.  THIS
C                               IS ACCOMPLISHED BY ADDING 10 TO THE ACTION
C                               CODE.  FOR EXAMPLE, ACTFLG=16 MEANS REWIND
C                               FILE, REWIND BAND, AND RETURN TO GRAFOPTN
C
C                               FOR VALUES=0-3 REWIND BAND(6) MAY BE 
C                               COMBINED WITH THE SPECIFIED ACTION.  THIS
C                               IS ACCOMPLISHED BY ADDING 100 TO THE ACTION
C                               CODE.  FOR EXAMPLE, ACTFLG=103 MEANS REWIND
C                               BAND, READ NEXT BAND, AND RETURN TO GRAFOPTN
C
C                               REWIND BAND AND REWIND FILE MAY BOTH BE
C                               COMBINED WITH A SPECIFIC ACTION BY ADDING
C                               110 TO THE ACTION CODE. 
C
C                               FOR VALUES=1,2,3,6 PLUS REWIND COMBINATIONS 
C                               A NEGATIVE VALUE INDICATES THAT THE FILE 
C                               POINTER WAS CHANGED AND THE CURRENT BAND MUST 
C                               BE REREAD BEFORE REPOSITIONING THE FILE
C             RTNCODE....
C
      INTEGER*2 INCSET,IDPLT,MXDATROW,NDATROW,ACTFLG
      REAL DATARRAY(MXDATROW,*)
      CHARACTER*(*) ROWHDR(*)
      CHARACTER*1 RTNCODE
C      
      PARAMETER (ITOPROW=9)
C
      PARAMETER (NCOL=8,LENCOL=8)
      CHARACTER*(LENCOL+1) STRING
C
      PARAMETER (MAXFLD=30)
      CHARACTER*28 FIELD(MAXFLD)
      CHARACTER*64 HELP1,HELP2
      CHARACTER*78 MSGLIN
      CHARACTER*12 GRAFDEF(4)
      CHARACTER*2 RTNFLAG,INCHAR,SAVFLAG
      CHARACTER*1 LETTER
      CHARACTER*8 FRMNAM(4)
      CHARACTER*20 FRMHDR(5)
      CHARACTER*3 LEGTYP(3),MAPPRJ(0:1)
      CHARACTER*4 AUTOSCL
      CHARACTER*1 CHRAXIS(0:1),TYPSET(0:1)
      CHARACTER*32 MSG2
      INTEGER*2 STRTROW,STRTCOL
      INTEGER*1 I1HLP(MAXFLD,4)
      LOGICAL PRTMSG,WRTDAT,FIRSTCALL
C
$INCLUDE: 'GRFPARM.INC'
$INCLUDE: 'GRAFVAR.INC'
$INCLUDE: 'FRMPOS.INC'
$INCLUDE: 'MODTMP.INC'
C
      DATA FIRSTCALL/.TRUE./
      DATA HELP1,HELP2 /'P:\HELP\GRFOPT00.HLP','P:\HELP\GRFOPT-D.HLP'/
      DATA I1HLP/
     1   01,02,03,04,05,06,06,06,06,10,11,12,13,13,15, 
     +   15,17,13,13,15,15,22,23,23,15,15,00,00,00,00, 
     2   01,26,03,04,00,06,06,06,06,28,11,29,30,30,31, 
     +   31,00,00,00,00,00,22,32,32,33,33,00,00,00,00, 
     3   01,34,03,04,00,00,00,00,00,00,00,35,00,00,00, 
     +   00,00,00,00,00,00,00,00,00,00,00,00,00,00,00, 
     4   01,02,03,04,00,00,00,00,00,00,00,36,37,00,00, 
     +   00,00,00,00,00,00,00,00,00,00,00,00,00,00,00/
      DATA FRMNAM /'GRFOPTTS','GRFOPTMP','GRFOPTSK','GRFOPTWR'/
C
      IF (FIRSTCALL) THEN
         FIRSTCALL=.FALSE.
         MSGLIN = ' '
         CALL GETMSG(555,MSGLIN)
         CALL PARSE1(MSGLIN,78,5,20,FRMHDR(1),RTNCODE)
         MSGLIN = ' '
         CALL GETMSG(556,MSGLIN)
         CALL PARSE1(MSGLIN,78,4,12,GRAFDEF(1),RTNCODE)
         MSGLIN = ' '
         CALL GETMSG(557,MSGLIN)
         CALL PARSE1(MSGLIN,78,3,3,LEGTYP(1),RTNCODE)
         CALL GETMSG(558,MSGLIN)
         AUTOSCL=MSGLIN(1:4)
         MSGLIN = ' '
         CALL GETMSG(559,MSGLIN)
         CALL PARSE1(MSGLIN,78,2,1,CHRAXIS(0),RTNCODE)
         MSGLIN = ' '
         CALL GETMSG(560,MSGLIN)
         CALL PARSE1(MSGLIN,78,2,1,TYPSET(0),RTNCODE)
         CALL GETMSG(999,MSGLIN)
         MSGLIN = ' '
         CALL GETMSG(576,MSGLIN)
         CALL PARSE1(MSGLIN,78,2,3,MAPPRJ(0),RTNCODE)
      ENDIF   
C
      IENTRY=ACTFLG
      ACTFLG=0
      ISAVFLD=0
C      
      IF (IENTRY.EQ.0) THEN      
C
C          ** INITIAL ENTRY INTO GRAFOPTN OR REENTRY AFTER REWIND FRAME
C
C          .. OFFSET TO CALCULATE THE START OF DATA IN ROWHDR      
         IF (IGRAPH.EQ.1) THEN
            IRHOFF = 1
         ELSE
            IRHOFF = 0
         ENDIF      
C
C          .. DISPLAY THE GRAPH-OPTIONS HEADER LINE
         CALL LOCATE(0,0,IERR)
         CALL WRTSTR(FRMHDR(1),10,14,0)
         CALL LOCATE(0,34,IERR)
         CALL WRTSTR(FRMHDR(2),13,14,0)
         CALL LOCATE(0,65,IERR)
         CALL WRTSTR(GRAFDEF(IGRAPH),12,14,0)
C      
         DO 20 I=1,MAXFLD
            FIELD(I) = ' '
   20    CONTINUE         
         CALL FILFLD(IDPLT,NPLTROW,FIELD,TYPSET,LEGTYP,MAPPRJ,AUTOSCL)
      ELSE IF (IENTRY.EQ.1) THEN
C
C          ** REENTRY AFTER FILE POSITIONING -- MODIFY FIELD VALUES
C      
         CALL MODFLD(NPLTROW,FIELD)   
      ENDIF   
C      
C       ** INITIAL ENTRY OR REENTRY AFTER FILE POSITIONING
C
      IF (IENTRY.EQ.0 .OR. IENTRY.EQ.1) THEN
         IDOPT =IDPLT
         NCOPTA=NCPLTA
         NCOPTB=NCPLTB         
C         
C          .. SET INITIAL SUBTITLE -- MAP
         IF (IGRAPH.EQ.2) THEN
            IF (NCOPTA.EQ.NCOPTB) THEN
               DATASUB = COLHDR(NCOPTA)
            ELSE
               DATASUB=COLHDR(NCOPTA)
               DO 50 NC=NCOPTA+1,NCOPTB
                  ICH=LNG(DATASUB)+1
                  DATASUB(ICH:)=', '//COLHDR(NC)
   50          CONTINUE
            ENDIF
         ENDIF         
C
         IROW = 25-ITOPROW+1
         CALL CLRMSG(IROW)
         CALL CLRMSG(IROW-1)
         CALL LOCATE(ITOPROW,0,IERR)
         CALL WRTSTR(FRMHDR(3),4,14,0)
         CALL LOCATE(ITOPROW,8,IERR)
         CALL WRTSTR(FRMHDR(4),7,14,0)
         RTNFLAG = 'G1'
         STRTROW = 1
         IROW = 1
         STRTCOL = 1
         ICOL = 1
      ENDIF
      WRTDAT=.TRUE.
C
C          .. DISPLAY THE DATA AREA FOR THE CURRENT GRAPH AND DATA SET
C      
100   CONTINUE
      IF (WRTDAT) THEN
         WRTDAT=.FALSE.
         CALL CLRMSG(3)
         CALL CLRMSG(2)
         CALL WRTSHEET(IGRAPH,ITOPROW,STRTROW,STRTCOL,NCOPTA,NCOPTB,
     +                 NUMCOL,NDATROW,ROWHDR(IRHOFF+1),DATARRAY,
     +                 MXDATROW,FRMHDR(5),CHRAXIS,COLAXIS)
      ENDIF
C
C       .. DISPLAY THE GRAPH-OPTIONS CONTROL AREA, FUNCTION KEY LINE, AND 
C          SOLICIT USER INPUT
C
      CALL LOCATE(1,0,IERR)
      CALL GETFRM(FRMNAM(IGRAPH),HELP2,FIELD,28,RTNFLAG)
      READ(FIELD(MAXFLD),800) IFLDNBR
      FIELD(MAXFLD)=' '
      IF (RTNFLAG.EQ.'4F') THEN
C          .. F4 -- EXIT, DO NOT SAVE CHANGES      
         CALL WRTMSG(2,200,12,1,0,' ',0)
         CALL LOCATE(23,50,IERR)
         CALL OKREPLY(LETTER,RTNCODE)
         IF (LETTER.EQ.'Y'.AND.RTNCODE.EQ.'0') THEN
            RTNFLAG = 'QT'
         ELSE
            RTNFLAG = 'GO'
            WRTDAT=.TRUE.
            GO TO 100
         ENDIF
      ELSE IF (RTNFLAG.EQ.'1F') THEN
C          .. F1 -- REQUEST FOR HELP      
         WRITE(HELP1(15:16),810) I1HLP(IFLDNBR,IGRAPH)
         CALL DSPWIN(HELP1)
         RTNFLAG = 'GO'
         WRTDAT=.FALSE.
         GO TO 100
      ELSE
         NCPLT=NCOPTB-NCOPTA+1
         SAVFLAG=RTNFLAG
         IRWBFLG = 0
         CALL CHKFLD(FIELD,TYPSET,LEGTYP,MAPPRJ,AUTOSCL,IGRAPH,
     +               NUMCOL,NDATROW,MXDATROW,DATATITLE,DATASUB,NCOPTA,
     +               NCOPTB,IMSG,IFLDNBR,MSG2,RTNFLAG)
         IF (RTNFLAG.EQ.'RB') THEN
            IRWBFLG = 1
            RTNFLAG = SAVFLAG
         ENDIF   
         IF ((NCOPTB-NCOPTA+1).NE.NCPLT) THEN
            WRTDAT=.TRUE.
C         
C             .. SET SUBTITLE -- MAP
            IF (IGRAPH.EQ.2) THEN
               IF (NCOPTA.EQ.NCOPTB) THEN
                  DATASUB = COLHDR(NCOPTA)
               ELSE
                  DATASUB=COLHDR(NCOPTA)
                  DO 120 NC=NCOPTA+1,NCOPTB
                     ICH=LNG(DATASUB)+1
                     DATASUB(ICH:)=', '//COLHDR(NC)
  120             CONTINUE
               ENDIF
            ENDIF         
         ENDIF   
         IF (RTNFLAG.EQ.'ER') THEN
            WRITE(FIELD(MAXFLD),800) IFLDNBR
            NCHR = LNG(MSG2)
            CALL WRTMSG(2,IMSG,12,1,1,MSG2,NCHR)
            RTNFLAG = 'GO'
         ENDIF
      ENDIF
C
      IF (RTNFLAG.EQ.'2F') THEN
C          .. F2 -- SAVE CHANGES       
         IF (INCSET.EQ.1) THEN
            CALL WRTMSG(2,396,12,1,0,' ',0)
            CALL LOCATE(23,50,IERR)
            CALL OKREPLY(LETTER,RTNCODE)
            IF (LETTER.EQ.'Y' .AND. RTNCODE.EQ.'0') THEN
               INCSET = 0
               GO TO 500
            ELSE
C                .. USER DOES NOT WANT TO SAVE CHANGES FOR AN INCOMPLETE BAND;
C                   RETURN TO CONTROL AREA; ALL ACTIONS ARE CANCELLED.
               CALL CLRMSG(3)
               CALL CLRMSG(2)
               CALL WRTMSG(3,605,12,1,1,' ',0)
               WRTDAT=.TRUE.
               RTNFLG='GO'
               GO TO 100
            END IF
         ELSE   
            GO TO 500
         ENDIF      
      ELSE IF (RTNFLAG.EQ.'QT') THEN
         GO TO 600
      ELSE IF (RTNFLAG.EQ.'GO') THEN
         GO TO 100
      ELSE   
         RTNFLAG = 'GO'
      ENDIF
C
C ---------------  USER IS NOW IN THE DATA AREA ---------------
C
      IF (WRTDAT) THEN
         WRTDAT=.FALSE.
         CALL CLRMSG(3)
         CALL CLRMSG(2)
         CALL WRTSHEET(IGRAPH,ITOPROW,STRTROW,STRTCOL,NCOPTA,NCOPTB,
     +                 NUMCOL,NDATROW,ROWHDR(IRHOFF+1),DATARRAY,
     +                 MXDATROW,FRMHDR(5),CHRAXIS,COLAXIS)
      ENDIF
C
C   DISPLAY THE ROW,COL AND DATA IN THE STATUS LINE FOR THE CURRENT 
C   CURSOR POSITION AND ACCEPT USER INPUT.
C
200   CONTINUE
      IF (IROW.EQ.0) THEN
         IF (IGRAPH.EQ.1) THEN
            CALL GRFAXIS(ITOPROW,ICOL,STRTCOL,CHRAXIS,COLAXIS(ICOL-1),
     +                   INCHAR)
         ENDIF
      ELSE IF (IROW.EQ.1) THEN
         ILEN = 28
         IF (ICOL.EQ.1) THEN
            CALL GETCELL(ITOPROW,IROW,ICOL,STRTROW,STRTCOL,DATATITLE,
     +                   ILEN,1,IGRAPH,NBRFRM,INCHAR)
         ELSE
            CALL GETCELL(ITOPROW,IROW,ICOL,STRTROW,STRTCOL,DATASUB,
     +                   ILEN,1,IGRAPH,NBRFRM,INCHAR)
         END IF
      ELSE IF (IROW.EQ.2) THEN
         IF (ICOL.GT.1) THEN
            ILEN = 28
            CALL GETCELL(ITOPROW,IROW,ICOL,STRTROW,STRTCOL,
     +                   COLHDR(ICOL-1),ILEN,1,IGRAPH,NBRFRM,INCHAR)
C         
C             .. SET SUBTITLE -- MAP
            IF (IGRAPH.EQ.2) THEN
               IF (NCOPTA.EQ.NCOPTB) THEN
                  DATASUB = COLHDR(NCOPTA)
               ELSE
                  DATASUB=COLHDR(NCOPTA)
                  DO 220 NC=NCOPTA+1,NCOPTB
                     ICH=LNG(DATASUB)+1
                     DATASUB(ICH:)=', '//COLHDR(NC)
  220             CONTINUE
               ENDIF
               IF (STRTROW.EQ.1 .AND. STRTCOL.EQ.1) THEN
                  CALL WRTSLIN(ITOPROW,1,STRTROW,STRTCOL,ROWHDR,
     +                         DATARRAY,MXDATROW)
               ENDIF
            ENDIF         
         END IF
      ELSE 
         IF (ICOL.EQ.1) THEN
            ILEN = 12
            CALL GETCELL(ITOPROW,IROW,ICOL,STRTROW,STRTCOL,
     +            ROWHDR(IRHOFF+IROW-2),ILEN,1,IGRAPH,NBRFRM,INCHAR)
         ELSE
            WRITE(STRING,820) DATARRAY(IROW-2,ICOL-1)
            CALL GETCELL(ITOPROW,IROW,ICOL,STRTROW,STRTCOL,STRING,
     +                   LENCOL+1,4,IGRAPH,NBRFRM,INCHAR)
C            READ(STRING,820) DATARRAY(IROW-2,ICOL-1)
         END IF
      END IF
C
C   TAKE APPROPRIATE ACTION FOR ANY SPECIAL KEYS ENTERED.
C
      PRTMSG = .FALSE.
      IF (INCSET.EQ.1) THEN
         IMSG=396
      ELSE
         IMSG=393
      ENDIF      
      IF (INCHAR.EQ.'1F') THEN
C          .. F1 -- REQUEST HELP      
         CALL DSPWIN(HELP2)
      ELSE IF (INCHAR.EQ.'2F') THEN
C          .. F2 -- ENTER VALUES INTO .GDF FILE      
         IF (INCSET.EQ.1) THEN
            CALL WRTMSG(2,396,12,1,0,' ',0)
            CALL LOCATE(23,50,IERR)
            CALL OKREPLY(LETTER,RTNCODE)
            IF (LETTER.EQ.'Y' .AND. RTNCODE.EQ.'0') THEN
               INCSET=0
               GO TO 500
            ELSE
C                .. USER DOES NOT WANT TO SAVE CHANGES FOR AN INCOMPLETE BAND;
C                   RETURN TO DATA AREA; ACTION IS CANCELLED.
               CALL CLRMSG(3)
               CALL CLRMSG(2)
               CALL WRTMSG(3,605,12,1,1,' ',0)
               WRTDAT=.TRUE.
            ENDIF
         ELSE
            GO TO 500   
         ENDIF   
      ELSE IF (INCHAR.EQ.'3F') THEN
C          .. F3 -- NEXT BAND      
         PRTMSG = .TRUE.
         ACTFLG=3
      ELSE IF (INCHAR.EQ.'4F') THEN
C          .. F4 -- ABORT WITH NO CHANGES      
         CALL WRTMSG(2,200,12,1,0,' ',0)
         CALL LOCATE(23,50,IERR)
         CALL OKREPLY(LETTER,RTNCODE)
         IF (LETTER.EQ.'Y' .AND. RTNCODE.EQ.'0') THEN
            GO TO 600
         ELSE
C                .. USER DOES NOT WANT TO ABORT GRAFOPTN;
C                   RETURN TO DATA AREA; ACTION IS CANCELLED.
            CALL CLRMSG(3)
            CALL CLRMSG(2)
            CALL WRTMSG(3,605,12,1,1,' ',0)
            WRTDAT=.TRUE.
         END IF
      ELSE IF (INCHAR.EQ.'7F') THEN
C          .. PREVIOUS BAND
         PRTMSG = .TRUE.
         ACTFLG=2
      ELSE IF (INCHAR.EQ.'8F') THEN
C          .. REWIND FILE      
         PRTMSG = .TRUE.
         ACTFLG=1
      ELSE IF (INCHAR.EQ.'8S') THEN
C          .. REWIND BAND      
         PRTMSG = .TRUE.
         ACTFLG=6
      ELSE IF (INCHAR.EQ.'3S') THEN
C          .. APPEND FRAME      
         ACTFLG=5
         GO TO 600
      ELSE IF (INCHAR.EQ.'UP') THEN
         GO TO 100
      ELSE 
         CALL CSRKEYS(INCHAR,ITOPROW,IROW,ICOL,NDATROW,NUMCOL)
      END IF
C
C       ** REQUEST USER RESPONSE ABOUT SAVING FIELDS BEFORE FILE POSITIONING
C
      IF (PRTMSG) THEN
         CALL WRTMSG(2,IMSG,12,1,0,' ',0)
         CALL LOCATE(23,50,IERR)
         CALL OKREPLY(LETTER,RTNCODE)
         IF (LETTER.EQ.'Y' .AND. RTNCODE.EQ.'0') THEN
            INCSET=0
            GO TO 500
         ELSE
            CALL WRTMSG(2,397,12,1,0,' ',0)
            CALL LOCATE(23,50,IERR)
            CALL OKREPLY(LETTER,RTNCODE)
            IF (LETTER.EQ.'Y' .AND. RTNCODE.EQ.'0') THEN
               GO TO 600
            ELSE
C                .. USER DOES NOT WANT REQUESTED FILE ACTION;
C                   RETURN TO DATA AREA; ACTION IS CANCELLED.
               CALL CLRMSG(3)
               CALL CLRMSG(2)
               CALL WRTMSG(3,605,12,1,1,' ',0)
               WRTDAT=.TRUE.
            END IF
         END IF
      ENDIF      
C
C   CHECK THE NEW VALUES OF ROW AND COLUMN AND WRAP, SCROLL, ETC
C   AS NECESSARY.
C
      CALL CHKCELL(IGRAPH,ITOPROW,IROW,ICOL,STRTROW,STRTCOL,NUMCOL,
     +             NDATROW,ROWHDR(IRHOFF+1),DATARRAY,MXDATROW,RTNCODE)
      IF (RTNCODE.EQ.'1' .OR. WRTDAT) THEN
         WRTDAT=.FALSE.
         CALL CLRMSG(3)
         CALL CLRMSG(2)
         CALL WRTSHEET(IGRAPH,ITOPROW,STRTROW,STRTCOL,NCOPTA,NCOPTB,
     +                 NUMCOL,NDATROW,ROWHDR(IRHOFF+1),DATARRAY,
     +                 MXDATROW,FRMHDR(5),
     +              CHRAXIS,COLAXIS)
      ENDIF
C
C   GO BACK AND SOLICIT INPUT FOR ANOTHER FIELD
C
      GO TO 200
C      
C----------------------------------------------------------------------
C
C   LOAD INFORMATION ENTERED INTO THE COMMON BLOCK AND RETURN
C
500   CONTINUE
      NPTRFLG=0
      ISAVFLD=1
      CALL LODFLD(FIELD,NDATROW,TYPSET,LEGTYP,MAPPRJ,AUTOSCL,IDPLT,
     +            NPLTROW,NCPLTA,NCPLTB,NPTRFLG,IRWFFLG)
      IF (NBRFRM.EQ.1) THEN
C          .. REWRITE DATA TO .API FILE;  TITLES AND HEADERS MAY HAVE BEEN
C             REVISED; ALLOWED ONLY IF ONE FRAME IS IN MEMORY
C             NOTE:  VALUE OF DATASUB FOR MAP IS ALWAYS BLANK
         IF (IGRAPH.EQ.2) DATASUB=' '
         CALL REWRFRM(ROWHDR(IRHOFF+1),DATARRAY,MXDATROW,NDATROW,
     +                IGRAPH,NUMCOL,COLHDR,DATATITLE,DATASUB,RTNCODE)
      ENDIF
      IF (FRM1D.GT.1) THEN
C          .. POSITION FILE TO START OF BAND
         CALL BEGDSET(RTNCODE)
         NPTRFLG=1
      ENDIF
C       .. NO AUTOMATIC REWIND OF BAND IF ACTION IS ALREADY REWIND BAND
      IF (ACTFLG.EQ.6) IRWBFLG=0
      IF (IRWFFLG.EQ.1) THEN
C          .. AUTOMATIC REWIND OF FILE TO ALIGN BANDS
         IF (ACTFLG.EQ.2 .OR. ACTFLG.EQ.3) THEN
C             .. CANCEL PREVIOUS ACTION(2=PREVIOUS BAND, 3=NEXT BAND); 
C                SET FLAG TO REWIND FILE         
            ACTFLG = 1
            CALL CLRMSG(3)
            CALL CLRMSG(2)
            CALL WRTMSG(3,604,12,1,1,' ',0)
            WRTDAT=.TRUE.
         ELSE IF (ACTFLG.NE.1) THEN
C             .. ADD REWIND FILE TO PREVIOUS ACTION(0=SAVE CHANGES THEN EXIT,
C                6=REWIND BAND THEN RETURN)
            ACTFLG = ACTFLG+10   
         ENDIF
      ENDIF
      IF (IRWBFLG.EQ.1) THEN
C          .. PRINT MESSAGE:  REWIND BAND TO ALIGN PLOT WIDTH AND DATA COLUMNS
         CALL WRTMSG(3,384,12,0,0,' ',0)
C          .. ASK USER IF AUTOMATIC BAND REWIND IS DESIRED
         CALL WRTMSG(2,534,12,1,0,' ',0)
         CALL LOCATE(23,50,IERR)
         CALL OKREPLY(LETTER,RTNCODE)
         IF (LETTER.EQ.'Y'.AND.RTNCODE.EQ.'0') THEN
            ACTFLG = ACTFLG + 100
         ENDIF
      ENDIF
      IF (NPTRFLG.EQ.1) THEN   
C          .. START/END FRAMES WERE REVISED FOR CURRENT BAND; REREAD DATA
C             SO DATA IN MEMORY AND CONTROL VARIABLES AGREE WITH FILE POSITION
         IF (ACTFLG.EQ.0) THEN
            ACTFLG=4
         ELSE
            ACTFLG=-ACTFLG
         ENDIF      
      ENDIF   
C
600   CONTINUE 
      IF (ISAVFLD.EQ.1) THEN
C          .. VALUES ARE SAVED
         RTNCODE='1'
      ELSE
C          .. VALUES ARE NOT SAVED      
         RTNCODE='0'
      ENDIF      
      CALL CLRMSG(3)
      CALL CLRMSG(2)
      RETURN
C
C       ** FORMAT STMTS
C
  800 FORMAT(I2)      
  810 FORMAT(I2.2)      
  820 FORMAT(F9.2)
      END
************************************************************************
      SUBROUTINE FILFLD(IDPLT,NPLTROW,FIELD,TYPSET,LEGTYP,MAPPRJ,
     +                  AUTOSCL)
C
C       ** INPUT:
C             IDPLT
C             NPLTROW
C             FIELD
C             TYPSET
C             LEGTYP
C             MAPPRJ
C             AUTOSCL 
C
      CHARACTER*28 FIELD(*)
      CHARACTER*1  TYPSET(0:1)
      CHARACTER*3  LEGTYP(*),MAPPRJ(0:1)
      CHARACTER*4  AUTOSCL
C
$INCLUDE: 'GRFPARM.INC'
$INCLUDE: 'GRAFVAR.INC'
$INCLUDE: 'FRMPOS.INC'
C
C   INITIALIZE FIELDS FOR CONTROL AREA FORM 
C
      FIELD(1) = GRTITLE
      FIELD(2) = GRSUBTITLE
      FIELD(3) = FTXT
      FIELD(4) = LEGTYP(LEGEND+1)
      WRITE(FIELD(5),501) NGRFSCR
C      
      IF (LOWCOL.GT.25) THEN
         IOFF=70
      ELSE
         IOFF=64
      ENDIF      
      FIELD(6) = CHAR((LOWCOL+1)+IOFF) 
      IF (HICOL.GT.25) THEN
         IOFF=70
      ELSE
         IOFF=64
      ENDIF      
      FIELD(8) = CHAR((HICOL+1)+IOFF) 
C      
      IDROW=LORFRM
      WRITE(FIELD(7),503) IDROW+2
      WRITE(FIELD(9),503) (NPLTROW+IDROW-1) + 2
C     
      IF (IGRAPH.EQ.2) THEN
C          .. MAP --- MAP PROJECTION      
         FIELD(10) = MAPPRJ(NDECRT(4))
      ELSE    
C          .. OTHER PLOTS --- BAND LOCATION      
         FIELD(10) = TYPSET(ITYPSET)      
      ENDIF   
      IF (IGRAPH.EQ.4) THEN
         WRITE(FIELD(11),502) PLTWID
      ELSE   
         WRITE(FIELD(11),501) PLTWID
      ENDIF   
C      
      IF (IGRAPH.LE.2) THEN
C
C          .. LEFT AXIS SCALE -- TIMESERIES AND MAP      
         IF (IGRAPH.EQ.1) THEN
            IDLFT = IDPLT
         ELSE
            IDLFT = 1
         ENDIF      
         FIELD(12) = LFTTXT(IDLFT)
         IF (LFTSCALE(1,IDLFT).EQ.-99999.) THEN
             FIELD(13) = AUTOSCL
             FIELD(14) = ' '
         ELSE
             WRITE(FIELD(13),520) LFTSCALE(1,IDLFT)
             WRITE(FIELD(14),520) LFTSCALE(2,IDLFT)
         END IF
         IF (IGRAPH.EQ.1) THEN
C
C             .. REAL VALUE FOR TIMESERIES          
            WRITE(FIELD(15),520) YMAJLFT(IDLFT)
         ELSE   
C
C             .. INTEGER VALUE FOR MAP
            ITEMP = YMAJLFT(IDLFT)
            WRITE(FIELD(15),506) ITEMP
         ENDIF   
         WRITE(FIELD(16),501) YMINLFT(IDLFT)
C         
C          .. RIGHT AXIS SCALE -- TIMESERIES      
         IF (IGRAPH.EQ.1) THEN
            FIELD(17) = RTTXT(IDPLT)
            IF (RTSCALE(1,IDPLT).EQ.-99999.) THEN
               FIELD(18) = AUTOSCL
               FIELD(19) = ' '
            ELSE
               WRITE(FIELD(18),520) RTSCALE(1,IDPLT)
               WRITE(FIELD(19),520) RTSCALE(2,IDPLT)
            END IF
            WRITE(FIELD(20),520) YMAJRT(IDPLT)
            WRITE(FIELD(21),501) YMINRT(IDPLT)
         END IF
C         
C          .. BOTTOM AXIS SCALE -- TIMESERIES AND MAP      
         FIELD(22) = BOTTXT
         IF (BTSCALE(1).EQ.-99999.) THEN
             FIELD(23) = AUTOSCL
             FIELD(24) = ' '
         ELSE
             WRITE(FIELD(23),520) BTSCALE(1)
             WRITE(FIELD(24),520) BTSCALE(2)
         END IF
         IF (IGRAPH.EQ.1) THEN
C
C             .. REAL VALUE FOR TIMESERIES          
            WRITE(FIELD(25),520) XMAJBT
         ELSE   
C
C             .. INTEGER VALUE FOR MAP
            ITEMP = XMAJBT
            WRITE(FIELD(25),506) ITEMP
         ENDIF   
         WRITE(FIELD(26),501) XMINBT
      ELSE IF (IGRAPH.EQ.3) THEN
C
C          .. MAX PRESSURE FOR ZOOM -- SKEWT
         WRITE(FIELD(12),520) LFTSCALE(2,2)
      ELSE
C
C          .. NUMBER OF WIND SPEED CATEGORIES,
C             NUMBER OF SPEED INTERVALS PER CATEGORY -- WINDROSE
         WRITE(FIELD(12),501) NCHRBT
         WRITE(FIELD(13),502) NDECLF(1)                  
      ENDIF   
C      
      RETURN
C
C       ** FORMAT STMTS
C
  501 FORMAT(I1)
  502 FORMAT(I2)      
  503 FORMAT(I3)      
  506 FORMAT(I6)
  520 FORMAT(F6.1)
C        
      END
************************************************************************
      SUBROUTINE MODFLD(NPLTROW,FIELD)   
C
      INTEGER*2    NPLTROW
      CHARACTER*28 FIELD(*)
C
$INCLUDE: 'GRFPARM.INC'
$INCLUDE: 'GRAFVAR.INC'
$INCLUDE: 'FRMPOS.INC'
C
      IDROW=LORFRM
      WRITE(FIELD(7),503) IDROW+2
      WRITE(FIELD(9),503) (NPLTROW+IDROW-1) + 2
C      
      RETURN
C
C       ** FORMAT STMTS
C
  503 FORMAT(I3)      
C        
      END
************************************************************************
      SUBROUTINE CHKFLD(FIELD,TYPSET,LEGTYP,MAPPRJ,AUTOSCL,
     +           IGRAPH,NUMCOL,NDATROW,MXDATROW,DATATITLE,DATASUB,
     +           NCOPTA,NCOPTB,IMSG,IFLDNBR,MSG2,RTNFLAG)
C
C       ** INPUT:
C             FIELD
C             TYPSET
C             LEGTYP
C             MAPPRJ 
C             AUTOSCL
C             IGRAPH
C             NUMCOL
C             NDATROW
C       ** OUTPUT:
C             DATATITLE
C             DATASUB 
C             NCOPTA
C             NCOPTB
C             IMSG
C             IFLDNBR
C             MSG2
C             RTNFLAG
C
      CHARACTER*28 FIELD(*)
      CHARACTER*1  TYPSET(0:1)
      CHARACTER*3  LEGTYP(*),MAPPRJ(0:1)
      CHARACTER*4  AUTOSCL
      CHARACTER*(*) MSG2,DATATITLE,DATASUB
      CHARACTER*2  RTNFLAG
C
$INCLUDE: 'GRFPARM.INC'
$INCLUDE: 'FRMPOS.INC'
$INCLUDE: 'MODTMP.INC'
C
      DATA MAXINTRVL /50/      
      DATA R6MIN /-999.9/, R6MAX /9999.9/
      DATA NGSMAX /1/
C
      MSG2 = ' '    
      IMSG = 0  
C
C   IF THERE IS AN ERROR IN THE FORM, DISPLAY A MESSAGE AND RETURN TO THE
C   FORM FOR CORRECT INFORMATION
C
C       .. ALLOWED VALUES FOR LEGEND: OFF,HOR,VER
      DO 20 I=1,3
      IF (FIELD(4).EQ.LEGTYP(I)) GO TO 25
   20 CONTINUE
      IMSG=381
      IFLDNBR=4
      WRITE(MSG2,550)(LEGTYP(I),I=1,3)
      GO TO 950
   25 CONTINUE
C
C       .. ALLOWED VALUE FOR NUMBER OF PLOTS/PAGE: 1
      READ(FIELD(5),501) ITEMP1
      IFLDNBR=5
      IF (ITEMP1.GT.NGSMAX) THEN
         IMSG = 86
         WRITE(MSG2,501) NGSMAX
         GO TO 950
      ENDIF      
C      
      IF (IGRAPH.LE.2) THEN
C       
C          .. COLUMN SPECIFICATIONS -- TIMESERIES AND MAP
         ITEMP1 = ICHAR(FIELD(6)) - 64
         ITEMP2 = ICHAR(FIELD(8)) - 64
         MAXVAL = NUMCOL+1
         IF (ITEMP2.GT.MAXVAL) THEN
            IFLDNBR = 8
            IMSG = 86
            WRITE(MSG2,550) CHAR(MAXVAL+64)
            GO TO 950
         ENDIF
         MAXVAL = MIN0(ITEMP2,MAXVAL)
         IF (ITEMP1.GT.MAXVAL) THEN
            IFLDNBR = 6
            IMSG = 86
            WRITE(MSG2,550) CHAR(MAXVAL+64)
            GO TO 950
         ENDIF
         MAXWID = ITEMP2-ITEMP1+1
C
C          .. ROW SPECIFICATIONS -- TIMESERIES AND MAP      
         READ(FIELD(7),503) ITEMP1
         READ(FIELD(9),503) ITEMP2
         MAXVAL = NDATROW+2
         IF (ITEMP2.GT.MAXVAL) THEN
            IFLDNBR = 9
            IMSG = 86
            WRITE(MSG2,502) MAXVAL
            GO TO 950
         ENDIF
         MAXVAL = MIN0(ITEMP2,MAXVAL)
         IF (ITEMP1.GT.MAXVAL) THEN
            IFLDNBR = 7
            IMSG = 86
            WRITE(MSG2,502) MAXVAL
            GO TO 950
         ENDIF
C
         IF (IGRAPH.EQ.1) THEN
C
C            .. FIND THE POSITION OF FIRST/LAST FRAME OF BAND
            ITEMP1 = ITEMP1-2
            FRMPTR(NBRFRM+1)=NDATROW+1
            DO 30 I=2,NBRFRM+1
              IF (ITEMP1.LT.FRMPTR(I)) GO TO 32
   30       CONTINUE     
   32       CONTINUE     
            LOCFRA=I-1
C          
            ITEMP2=ITEMP2-2
            DO 40 I=LOCFRA+1,NBRFRM+1
               IF (ITEMP2.LT.FRMPTR(I)) GO TO 42
   40       CONTINUE     
   42       CONTINUE     
            LOCFRB=I-1
C            
C            .. SET TITLE -- TIMESERIES               
C               USE TITLE OF FIRST FRAME IN BAND; ADD TITLE OF LAST FRAME IN
C               BAND ONLY IF THERE ARE MULTIPLE FRAMES AND IT IS DIFFERENT
            DATATITLE=TTLSAV(1,LOCFRA)
            IF ( DATATITLE.NE.TTLSAV(1,LOCFRB)) THEN
               NCHR=LNG(DATATITLE)
               DATATITLE(NCHR+1:) = '-'//TTLSAV(1,LOCFRB)
            ENDIF   
C            .. SET SUBTITLE -- TIMESERIES               
C               USE SUBTITLE OF FIRST FRAME IN BAND; ADD SUBTITLE OF LAST FRAME
C               IN BAND IF THERE ARE MULTIPLE BANDS  
            DATASUB=TTLSAV(2,LOCFRA)
            IF (LOCFRB.GT.LOCFRA) THEN
               NCHR=LNG(DATASUB)
               DATASUB(NCHR+1:) = '-'//TTLSAV(2,LOCFRB)
            ENDIF   
C
C             ..  ALLOWED VALUES FOR ITYPSET -- TIMESERIES 
            IF (FIELD(10).NE.TYPSET(0) .AND. 
     +          FIELD(10).NE.TYPSET(1)) THEN
               IMSG = 381
               IFLDNBR = 10
               WRITE(MSG2,550) (TYPSET(I),I=0,1)
               GO TO 950
            ENDIF
         ELSE
C          
C             ..  ALLOWED VALUES FOR MAP PROJECTION -- MAP 
            IF (FIELD(10).NE.MAPPRJ(0) .AND. 
     +          FIELD(10).NE.MAPPRJ(1)) THEN
               IMSG = 381
               IFLDNBR = 10
               WRITE(MSG2,550) (MAPPRJ(I),I=0,1)
               GO TO 950
            ENDIF
         ENDIF
C
C          .. MAXIMUM PLOT WIDTH FOR TIMESERIES AND MAP
         READ(FIELD(11),501) ITEMP1
         IFLDNBR=11
         IF (ITEMP1.GT.MAXWID) THEN
            IMSG = 86
            WRITE(MSG2,502) MAXWID
            GO TO 950
         ENDIF      
C
C          .. CALCULATE TEMPORARY PLOT COLUMN POINTERS
         NLOC = (ICHAR(FIELD(6))-64) - 1
         NHIC = (ICHAR(FIELD(8))-64) - 1
         IREW=0
         CALL PLTPTR(IREW,IGRAPH,NUMCOL,NLOC,NHIC,ITEMP1,
     +               NCOPTA,NCOPTB,IDUM)
         IF (MOD((NCOPTA-NLOC),ITEMP1).NE.0) THEN
C             .. WHEN THE PLOT WIDTH VALUE IS DEFINED AT THE CURRENT PLOT ID 
C                THE PLOTS ARE NOT ALIGNED PROPERLY WITH THE BEGINNING OF THE
C                BAND.  SET FLAG TO INDICATE THIS CONDITION IS PRESENT.  THE
C                USER WILL BE WARNED OF THE CONDITION WHEN HE REQUESTS A FILE
C                ACTION THAT EXITS GRAFOPTN.
            RTNFLAG = 'RB'
         ENDIF   
C
C          .. LEFT AXIS SCALE -- TIMESERIES AND MAP      
         IF (FIELD(13).NE.AUTOSCL) THEN
            IFLDNBR=13
            IMSG=165
            READ(FIELD(13),520,ERR=950) TEMP1
            IF (IGRAPH.EQ.1) THEN
               IF (TEMP1.LT.R6MIN) THEN
                  IMSG = 87
                  WRITE(MSG2,520) R6MIN
                  GO TO 950
               ELSE IF (TEMP1.GT.R6MAX) THEN
                  IMSG = 86
                  WRITE(MSG2,520) R6MAX
                  GO TO 950
               ENDIF
            ENDIF
            IFLDNBR=14
            READ(FIELD(14),520,ERR=900) TEMP2
            IF (TEMP1.GE.TEMP2) THEN
               IMSG = 185
               GO TO 950
            END IF                      
         END IF
C         
C          .. RIGHT AXIS SCALE -- TIMESERIES      
         IF (IGRAPH.EQ.1) THEN
            IFLDNBR=18
            IMSG=165
            IF (FIELD(18).NE.AUTOSCL) THEN
               READ(FIELD(18),520,ERR=950) TEMP1
               IF (TEMP1.LT.R6MIN) THEN
                  IMSG = 87
                  WRITE(MSG2,520) R6MIN
                  GO TO 950
               ELSE IF (TEMP1.GT.R6MAX) THEN
                  IMSG = 86
                  WRITE(MSG2,520) R6MAX
                  GO TO 950
               ENDIF
               IFLDNBR=19
               READ(FIELD(19),520,ERR=900) TEMP2
               IF (TEMP1.GE.TEMP2) THEN
                  IMSG = 185
                  GO TO 950
               END IF                      
            END IF
         END IF
C
C          .. BOTTOM AXIS SCALE -- TIMESERIES AND MAP      
         IF (FIELD(23).NE.AUTOSCL) THEN
            IFLDNBR=23
            IMSG=165
            READ(FIELD(23),520,ERR=950) TEMP1
            IF (IGRAPH.EQ.1) THEN
               IF (TEMP1 .NE. 0.) THEN
                  IMSG = 381
                  WRITE(MSG2,550) '0.',AUTOSCL
                  GO TO 950
               ENDIF
            ENDIF
            IFLDNBR=24
            READ(FIELD(24),520,ERR=900) TEMP2
            IF (IGRAPH.EQ.1) THEN
               IF (TEMP1.GT.MXDATROW) THEN
                  IMSG = 86
                  WRITE(MSG2,520) FLOAT(MXDATROW)
                  GO TO 950
               ENDIF      
            ENDIF   
            IF (TEMP1.GE.TEMP2) THEN
               IMSG = 185
               GO TO 950
            END IF                      
         END IF
      ELSE IF (IGRAPH.EQ.4) THEN
C
C          .. PRODUCT OF WIND SPEED INTERVALS AND GROUPS -- WINDROSE
         IFLDNBR=12
         READ(FIELD(12),501,ERR=900) ITEMP1 
         IFLDNBR=13
         READ(FIELD(13),502,ERR=900) ITEMP2 
         IF ((ITEMP1*ITEMP2).GT.MAXINTRVL) THEN
            IMSG = 379
            WRITE(MSG2,502) MAXINTRVL
            GO TO 950
         ENDIF   
      ENDIF   
      RETURN
C      
C       ** ERROR PROCESSING
C
  900 CONTINUE
      IMSG = 69      
  950 CONTINUE
      RTNFLAG = 'ER'
      RETURN
C
C       ** FORMAT STMTS
C
  501 FORMAT(I1)
  502 FORMAT(I2)      
  503 FORMAT(I3)      
  520 FORMAT(F6.1)
  550 FORMAT(3(A,:,','))
  551 FORMAT(2(I2,:,','))
C        
      END
************************************************************************
      SUBROUTINE LODFLD(FIELD,NDATROW,TYPSET,LEGTYP,MAPPRJ,AUTOSCL,
     +                  IDPLT,NPLTROW,NCPLTA,NCPLTB,NPTRFLG,IRWFFLG)
C    
C       ** INPUT: 
C             FIELD
C             NDATROW
C             TYPSET
C             LEGTYP
C             MAPPRJ
C             AUTOSCL
C             IDPLT
C       ** OUTPUT:
C             NPLTROW
C             NCPLTA
C             NCPLTB
C             NPTRFLG
C             IRWFFLG 
C
$INCLUDE: 'GRFPARM.INC'
$INCLUDE: 'GRAFVAR.INC'
$INCLUDE: 'FRMPOS.INC'
C
      CHARACTER*28 FIELD(*)
      CHARACTER*1  TYPSET(0:1)
      CHARACTER*3 LEGTYP(*),MAPPRJ(0:1)
      CHARACTER*4 AUTOSCL
C
      CHARACTER *1 LETTER,RTNCODE
C
      NPTRFLG=0
      IRWFFLG=0
C         
      GRTITLE = FIELD(1)
      GRSUBTITLE = FIELD(2)
      FTXT = FIELD(3)
C      
      DO 20 I=1,3
      IF (FIELD(4).EQ.LEGTYP(I)) GO TO 25
   20 CONTINUE
      I=1
   25 CONTINUE
      LEGEND = I-1
C
      READ(FIELD(5),501) NGRFSCR
C     
      IF (IGRAPH.LE.2) THEN 
         LOWCOL = (ICHAR(FIELD(6))-64) - 1
         HICOL  = (ICHAR(FIELD(8))-64) - 1
      ENDIF
C       .. GET BEGIN ROW VALUE FOR CURRENT BAND
      READ(FIELD(7),503) ITEMP1 
      ITEMP1 = ITEMP1-2
C       .. GET INDEX TO FRAME THAT CONTAINS BEGINNING ROW OF BAND; START 
C          FRAME WAS REVISED FOR CURRENT BAND; SET FLAG TO REREAD DATA
      FRMPTR(NBRFRM+1)=NDATROW+1
      DO 30 I=2,NBRFRM+1
         IF (ITEMP1.LT.FRMPTR(I)) GO TO 32
   30 CONTINUE     
   32 CONTINUE     
      IF (FRM1D.NE.(I-1)) THEN
         NPTRFLG=1
         FRM1D=I-1
      ENDIF   
C       .. GET END ROW VALUE FOR CURRENT BAND
      READ(FIELD(9),503) ITEMP2 
      ITEMP2=ITEMP2-2
C       .. GET INDEX TO FRAME THAT CONTAINS END ROW OF BAND; END
C          FRAME WAS REVISED FOR CURRENT BAND; SET FLAG TO REREAD DATA
      DO 40 I=FRM1D+1,NBRFRM+1
         IF (ITEMP2.LT.FRMPTR(I)) GO TO 42
   40 CONTINUE     
   42 CONTINUE     
      IF (FRM2D.NE.(I-1)) THEN
         NPTRFLG=1
         FRM2D=I-1
      ENDIF   
C      
      IF (IGRAPH.EQ.1) THEN
C          .. TIMESERIES --- BAND LOCATION   0=POSITION  1=INTERVAL
         IF (FIELD(10).EQ.TYPSET(0)) THEN
            ITYPSET=0
         ELSE
            ITYPSET=1
         ENDIF
      ELSE IF (IGRAPH.EQ.2) THEN
C          .. MAP --- MAP PROJECTION   0=MILLER  1=SCREEN   
         IF (FIELD(10).EQ.MAPPRJ(0)) THEN
            NDECRT(4)=0
         ELSE
            NDECRT(4)=1
         ENDIF
      ENDIF   
C       .. CALCULATE NUMBER OF DATA POINTS IN PLOT IN CASE RETURN IS MADE
C          WITHOUT READING DATA FROM FILE
      NPLTROW = ITEMP2-ITEMP1+1
      LORFRM = ITEMP1-FRMPTR(FRM1D)+1
      IF (ITYPSET.EQ.0) THEN
         NFRSET = FRM2D-FRM1D+1
         LOWROW=LORFRM
         HIROW=0
         IF (IGRAPH.EQ.1 .OR. IGRAPH.EQ.3) THEN
            IF (ITEMP2.EQ.FRMPTR(FRM2D+1)-1) THEN
               CALL WRTMSG(2,392,12,1,0,' ',0)
               CALL LOCATE(23,50,IERR)
               CALL OKREPLY(LETTER,RTNCODE)
               IF (LETTER.EQ.'Y') THEN
                  HIROW = 999
               ENDIF   
            ENDIF
         ENDIF
         IF (HIROW.NE.999) THEN
            HIROW=ITEMP2-FRMPTR(FRM2D)+1
         ENDIF
         NROWLST = FRMPTR(FRM2D+1)-1
         HIRFRM = MIN0(HIROW,NROWLST)
         CALL LOROWP(IGRAPH,NUMCOL,NFRSET,LOWROW,HIROW,RTNCODE)
         IF (RTNCODE.EQ.'4') IRWFFLG=1
      ELSE
         HIRFRM = ITEMP2-FRMPTR(FRM2D)+1
         NFRSET = ITEMP2-ITEMP1+1
         CALL LOROWI(IGRAPH,NUMCOL,NFRSET,LOWROW,RTNCODE)
         IF (RTNCODE.EQ.'4') IRWFFLG=1
         HIROW = 0 
      ENDIF 
C
      IF (IGRAPH.EQ.4) THEN
         READ(FIELD(11),502) PLTWID
      ELSE   
         READ(FIELD(11),501) PLTWID
      ENDIF   
      IF (IGRAPH.EQ.1 .OR. IGRAPH.EQ.2) THEN
         IF (IGRAPH.EQ.1) THEN
            NPLTCOL = NUMCOL
         ELSE   
            NPLTCOL = NUMCOL-2
         ENDIF   
         IF (PLTWID.LT.NPLTCOL) THEN
            NUMPLT = NPLTCOL / PLTWID
            IF (NUMPLT*PLTWID.LT.NPLTCOL) THEN
               NUMPLT = NUMPLT + 1
            END IF
         ELSE
            NUMPLT = 1
         END IF
C
C          .. CALCULATE PLOT COLUMN POINTERS
         IREW=0
         CALL PLTPTR(IREW,IGRAPH,NUMCOL,LOWCOL,HICOL,PLTWID,
     +               NCPLTA,NCPLTB,IDUM)
      ENDIF
C
      IF (IGRAPH.LE.2) THEN
C
C          .. LEFT AXIS SCALE -- TIMESERIES AND MAP      
         IF (IGRAPH.EQ.1) THEN
            IDLFT = IDPLT
         ELSE
            IDLFT = 1
         ENDIF      
         LFTTXT(IDLFT) = FIELD(12)
         IF (FIELD(13).EQ.AUTOSCL) THEN
            LFTSCALE(1,IDLFT) = -99999.
         ELSE
            READ(FIELD(13),520) LFTSCALE(1,IDLFT)
            READ(FIELD(14),520) LFTSCALE(2,IDLFT)
         END IF
         IF (IGRAPH.EQ.1) THEN
C
C             ** REAL VALUE FOR TIMESERIES         
            READ(FIELD(15),520) YMAJLFT(IDLFT)
         ELSE
C
C             ** INTEGER VALUE FOR MAP         
            READ(FIELD(15),506) ITEMP
            YMAJLFT(IDLFT) = FLOAT(ITEMP)
         ENDIF
         READ(FIELD(16),501) YMINLFT(IDLFT)
C         
C          .. RIGHT AXIS SCALE -- TIMESERIES      
         IF (IGRAPH.EQ.1) THEN
            RTTXT(IDPLT) = FIELD(17)
            IF (FIELD(18).EQ.AUTOSCL) THEN
               RTSCALE(1,IDPLT) = -99999.
            ELSE
               READ(FIELD(18),520) RTSCALE(1,IDPLT)
               READ(FIELD(19),520) RTSCALE(2,IDPLT)
            END IF
            READ(FIELD(20),520) YMAJRT(IDPLT)
            READ(FIELD(21),501) YMINRT(IDPLT)
         END IF
C         
C          .. BOTTOM AXIS SCALE -- TIMESERIES AND MAP      
         BOTTXT = FIELD(22)
         IF (FIELD(23).EQ.AUTOSCL) THEN
            BTSCALE(1) = -99999.
         ELSE
            READ(FIELD(23),520) BTSCALE(1)
            READ(FIELD(24),520) BTSCALE(2)
         END IF
         IF (IGRAPH.EQ.1) THEN
C
C             ** REAL VALUE FOR TIMESERIES         
            READ(FIELD(25),520) XMAJBT
         ELSE
C
C             ** INTEGER VALUE FOR MAP         
            READ(FIELD(25),506) ITEMP
            XMAJBT = FLOAT(ITEMP)
         ENDIF
         READ(FIELD(26),501) XMINBT
C
C          .. SET LAT/LON VALUES -- MAP
         IF (IGRAPH.EQ.2) THEN
            LOWLAT = LFTSCALE(1,1)
             HILAT = LFTSCALE(2,1)
            LOWLON =  BTSCALE(1)
             HILON =  BTSCALE(2)
         ENDIF     
      ELSE IF (IGRAPH.EQ.3) THEN
C
C          .. MAX PRESSURE FOR ZOOM -- SKEWT
         READ(FIELD(12),520) LFTSCALE(2,2)
      ELSE
C
C          .. NUMBER OF WIND SPEED CATEGORIES,
C             NUMBER OF SPEED INTERVALS PER CATEGORY -- WINDROSE
         READ(FIELD(12),501) NCHRBT
         READ(FIELD(13),502) NDECLF(1)                  
      ENDIF   
      RETURN
C
C       ** FORMAT STMTS
C
  501 FORMAT(I1)
  502 FORMAT(I2)      
  503 FORMAT(I3)      
  506 FORMAT(I6)      
  520 FORMAT(F6.1)
C        
      END
************************************************************************
      SUBROUTINE LOROWI(IGRAPH,NUMCOL,NFRSET,LOWROW,RTNCODE)
C
C       ** OBJECTIVE:  CALCULATE START ROW IN THE FIRST DATA FRAME WHEN
C                      INTERVAL POSITIONING IS USED
C       ** INPUT:
C             IGRAPH....
C             NUMCOL....NUMBER OF DATA COLUMNS IN A RECORD
C             NFRSET....NUMBER OF RECORDS IN A BAND
C       ** OUTPUT:
C             LOWROW....STARTING ROW IN THE FIRST FRAME OF THE FILE
C             RTNCODE....FLAG TO INDICATE ERROR STATUS
C                        '0'=NO ERROR
C                        '1'=END OF FILE
C                        '3'=ERROR IN READING FILE
C                        '4'=AUTOMATIC REWIND OF FILE TO ALIGN BANDS
C
      INTEGER*2    NFRSET,LOWROW
      CHARACTER*1  RTNCODE
C
$INCLUDE:  'FRMPOS.INC'
C            
      INTEGER*2   FILOPT
      CHARACTER*1  LETTER
      CHARACTER*1 CDUM1,CDUM2,CDUM3,CDUM4
      REAL*4      RDUM
C
      CALL RDFILPOS(IGRAPH,NUMCOL,INOWSV)
C
      NFRMBG  = 1  
      FILOPT  = 5
      NRECFRM = 0
      NREC = (FRMPTR(FRM1D)-1) + (LORFRM-1)
C
      CALL RDFRAME(CDUM1,RDUM,1,NFRMBG,FILOPT,IGRAPH,NUMCOL,CDUM2,CDUM3,
     +             CDUM4,NFRMFN,RTNCODE)  
   30 CONTINUE  
      IF (RTNCODE.NE.'0') GO TO 40
         NRECFRM = NFRMFN-NFRMBG+1
         NREC = NREC + NRECFRM
         CALL RDFRAME(CDUM1,RDUM,1,NFRMBG,FILOPT,IGRAPH,NUMCOL,CDUM2,
     +                CDUM3,CDUM4,NFRMFN,RTNCODE)  
         GO TO 30
   40 CONTINUE
      IF (RTNCODE.NE.'2') GO TO 100
      RTNCODE = '0'
      LOWROW = MOD(NREC,NFRSET) + 1
      IF (LOWROW.GT.NRECFRM .AND. NRECFRM.GT.0) THEN
C          .. FIRST BAND IN FILE MUST BEGIN IN FRAME 1;  FIRST ROW IN 
C             BAND 1 (LOWROW) IS SET TO 1
         LOWROW = 1
C          .. MESSAGE: REWIND FILE TO SET FIRST BAND TO FRAME 1
         CALL WRTMSG(3,395,12,0,0,' ',0)
         CALL WRTMSG(2,490,12,1,0,' ',0)
         CALL LOCATE(23,50,IERR)
         CALL OKREPLY(LETTER,RTNCODE)
         IF (LETTER.EQ.'Y'.AND.RTNCODE.EQ.'0') THEN
C             .. SET FLAG FOR AUTOMATIC REWIND OF FILE
            RTNCODE = '4'
         ENDIF
      ENDIF
C      
      CALL WRFILPOS(-3,IGRAPH,NUMCOL,INOWSV)
C              
  100 RETURN
      END      
************************************************************************
      SUBROUTINE LOROWP(IGRAPH,NUMCOL,NFRSET,LOWROW,HIROW,RTNCODE)
C
C       ** OBJECTIVE:  DETERMINE IF LOCATION OF BAND MUST BE REALIGNED
C                      WHEN FILE IS REWOUND -- BAND LOCATION BY POSITION
C                      WITHIN THE FRAME
C       ** INPUT:
C             NUMCOL....NUMBER OF DATA COLUMNS IN A RECORD
C             NFRSET....NUMBER OF FRAMES IN A BAND
C             LOWROW....START ROW IN THE FIRST FRAME OF THE BAND
C             HIROW.....FINAL ROW IN THE  LAST FRAME OF THE BAND
C       ** OUTPUT:
C             RTNCODE....FLAG TO INDICATE ERROR STATUS
C                        '0'=NO ERROR
C                        '1'=END OF FILE
C                        '3'=ERROR IN READING FILE
C                        '4'=AUTOMATIC REWIND OF FILE TO ALIGN BANDS
C
      INTEGER*2    NFRSET,LOWROW,HIROW      
      CHARACTER*1  RTNCODE
C
$INCLUDE:  'FRMPOS.INC'
C            
      INTEGER*2   FILOPT
      CHARACTER*1  LETTER
      CHARACTER*1 CDUM1,CDUM2,CDUM3,CDUM4
      REAL*4      RDUM
C
      CALL RDFILPOS(IGRAPH,NUMCOL,INOWSV)
      NFRSV=NBRFRM
C
      NFRMBG  = 1  
      FILOPT  = 5
      NBRFRM  = 0
C
      CALL RDFRAME(CDUM1,RDUM,1,NFRMBG,FILOPT,IGRAPH,NUMCOL,CDUM2,CDUM3,
     +             CDUM4,NFRMFN,RTNCODE)  
   30 CONTINUE  
      IF (RTNCODE.NE.'0') GO TO 40
         NBRFRM = NBRFRM + 1
         CALL RDFRAME(CDUM1,RDUM,1,NFRMBG,FILOPT,IGRAPH,NUMCOL,CDUM2,
     +                CDUM3,CDUM4,NFRMFN,RTNCODE)  
         GO TO 30
   40 CONTINUE
      IF (RTNCODE.NE.'2') GO TO 100
      RTNCODE = '0'
      IF (LOWROW.GT.HIROW) THEN
         NBRFRM  = NBRFRM + FRM1D
         NEXTRA = MOD(NBRFRM-1,NFRSET-1)
      ELSE
         NBRFRM  = NBRFRM + FRM1D-1
         NEXTRA = MOD(NBRFRM,NFRSET)
      ENDIF
      IF (NEXTRA.GT.0) THEN
C          .. MESSAGE: REWIND FILE TO SET FIRST BAND TO FRAME 1
         CALL WRTMSG(3,398,12,0,0,' ',0)
         CALL WRTMSG(2,490,12,1,0,' ',0)
         CALL LOCATE(23,50,IERR)
         CALL OKREPLY(LETTER,RTNCODE)
         IF (LETTER.EQ.'Y'.AND.RTNCODE.EQ.'0') THEN
C             .. SET FLAG FOR AUTOMATIC REWIND OF FILE
            RTNCODE = '4'
         ENDIF
      ENDIF
C      
      CALL WRFILPOS(-3,IGRAPH,NUMCOL,INOWSV)
      NBRFRM=NFRSV
C              
  100 RETURN
      END      
************************************************************************
      SUBROUTINE GETCELL(ITOPROW,IROW,ICOL,STRTROW,STRTCOL,ARGSTR,
     +                   LENSTR,FLDTYPE,IGRAPH,NBRFRM,INCHAR)
C
C   ROUTINE DISPLAYS THE ROW, COLUMN, AND VALUE OF THE CURRENT DATA CELL
C   IN THE DATA PORTION OF THE GRAFOPTN SCREEN.  IF THE USER PRESSES ENTER
C   IT ALLOWS THEM TO MODIFY THE VALUE.
C
      CHARACTER*(*) ARGSTR
      CHARACTER*2 INCHAR
      INTEGER*2 IROW,ICOL,STRTROW,STRTCOL,LENSTR,FLDTYPE
C
      PARAMETER (NCOL=8,LENCOL=8)
C
      CHARACTER*28 INSTRING
      CHARACTER*9  DATSTR
      CHARACTER*3 CHAR3
      CHARACTER*1 LETTER
      LOGICAL MSGON,DATFLG
C
      ITOP = ITOPROW+2      
      NROW = 22-ITOP
C
C       ** DATA VALUES ARE EXPRESSED TO ONE DECIMAL PLACE IN THE DATA AREA
C          AND TWO DECIMAL PLACES IN THE DISPLAY AREA -- SET UP SEPARATE
C          STRINGS FOR THE TWO AREAS    
      DATFLG = .FALSE.
      IF (IROW.GT.2.AND.ICOL.GT.1) THEN
         DATFLG = .TRUE.
         READ(ARGSTR,'(F9.2)') RVAL
         WRITE(DATSTR,'(F8.1)') RVAL
      ENDIF   
      
C
C   DISPLAY ROW AND COLUMN VALUES
C      
      CALL LOCATE(ITOPROW,4,IERR)
      WRITE(CHAR3,'(I3)') IROW
      CALL WRTSTR(CHAR3,3,15,1)

      CALL LOCATE(ITOPROW,15,IERR)
      IF (ICOL.GT.26) THEN
         IOFF=70
      ELSE
         IOFF=64
      ENDIF      
      LETTER = CHAR(ICOL+IOFF)
      CALL WRTSTR(LETTER,1,15,1)
C
C   HIGHLIGHT THE CURRENT CELL
C
C       .. CLEAR THE DISPLAY AREA FROM PREVIOUS WRITE
      CALL LOCATE(ITOPROW,18,IERR)
      CALL WRTSTR(INSTRING,28,0,0)
C       .. WRITE CURRENT STRING IN THE DISPLAY AREA       
      CALL LOCATE(ITOPROW,18,IERR)
      CALL WRTSTR(ARGSTR,LENSTR,15,1)
C       .. HIGHLIGHT THE CURRENT STRING IN THE DATA AREA
      JROW = IROW - STRTROW + 1 + ITOP
      JCOL = 3 + (ICOL-STRTCOL)*(LENCOL+1)
      CALL LOCATE(JROW,JCOL,IERR)
      IF (DATFLG) THEN
         CALL WRTSTR(DATSTR,LENCOL,15,1)
      ELSE
         CALL WRTSTR(ARGSTR,LENCOL,15,1)
      ENDIF
C
C   DISPLAY VALUE OF CURRENT CELL IN THE INPUT LINE AND SOLICIT USER
C   INPUT.  IF USER PRESSES ENTER LET THEM CHANGE THE VALUE.
C
      MSGON = .FALSE.
100   CONTINUE
      CALL GETCHAR(0,INCHAR)
      IF (MSGON) THEN
         MSGON = .FALSE.
         CALL CLRMSG(2)
      END IF
      IF (INCHAR.EQ.'RE') THEN
         IF (IROW.GT.2.AND.ICOL.GT.1) THEN
C             .. CHANGES TO DATA NOT ALLOWED         
            CALL WRTMSG(2,105,12,1,0,' ',0)
            MSGON = .TRUE.
            GO TO 100
         ELSE IF (NBRFRM.GT.1) THEN
C             .. CHANGES NOT ALLOWED WITH MULTIPLE FRAMES OF DATA IN MEMORY
            CALL WRTMSG(2,577,12,1,0,' ',0)
            MSGON = .TRUE.
            GO TO 100
         ELSE IF (IGRAPH.EQ.2 .AND. (IROW.EQ.1 .AND. ICOL.GT.1)) THEN
C             .. CHANGES NOT ALLOWED FOR MAP SUBTITLE
            CALL WRTMSG(2,578,12,1,0,' ',0)
            MSGON = .TRUE.
            GO TO 100
         ELSE
            CALL LOCATE(ITOPROW,18,IERR)
            INSTRING(1:LENSTR) = ARGSTR(1:LENSTR)
            CALL GETSTR(FLDTYPE,INSTRING,LENSTR,15,1,INCHAR)
            IF (INCHAR.NE.'4F') THEN 
               ARGSTR(1:LENSTR) = INSTRING(1:LENSTR)
            END IF
            IF (INCHAR.EQ.'2F'.OR.INCHAR.EQ.'4F') THEN
               INCHAR = '  '
            END IF
         END IF
      END IF
C
C   RE-WRITE THE CELL TO REFLECT NEW VALUE AND/OR REMOVE THE HIGHLIGHT
C
      CALL LOCATE(JROW,JCOL,IERR)
      IF (DATFLG) THEN
         CALL WRTSTR(DATSTR,LENCOL,14,0)
      ELSE
         IF (LENSTR.LT.LENCOL) THEN
            I = LENCOL/2 - LENSTR/2
            ARGSTR(I:I+LENSTR-1) = INSTRING(1:LENSTR)
         END IF
         CALL WRTSTR(ARGSTR,LENCOL,14,0)
      END IF

      RETURN
      END
***********************************************************************
      SUBROUTINE CSRKEYS(INCHAR,ITOPROW,IROW,ICOL,NDATROW,NUMCOL)
C
C   ROUTINE EVALUATES INCHAR AND ADJUSTS IROW AND ICOL IF A CURSOR KEY
C   HAS BEEN ENTERED.  
C
      CHARACTER*2 INCHAR
      PARAMETER (NCOL=8,LENCOL=8)
C
      ITOP = ITOPROW+2
      NROW = 22-ITOP
C
      IF (INCHAR.EQ.'UA') THEN
         IROW = IROW - 1
         IF (IROW.EQ.1.AND.ICOL.GT.2) THEN
            IROW = 0
         ELSE IF (IROW.EQ.2.AND.ICOL.EQ.1) THEN
            IROW = 1
         END IF
      ELSE IF (INCHAR.EQ.'DA') THEN
         IROW = IROW + 1
         IF (IROW.EQ.1.AND.ICOL.GT.2) THEN
            IROW = 2
         ELSE IF (IROW.EQ.2.AND.ICOL.EQ.1) THEN
            IROW = 3
         END IF
      ELSE IF (INCHAR.EQ.'LA') THEN
         ICOL = ICOL - 1 
      ELSE IF (INCHAR.EQ.'RA') THEN
         ICOL = ICOL + 1 
      ELSE IF (INCHAR.EQ.'TB') THEN
         ICOL = ICOL + NCOL
      ELSE IF (INCHAR.EQ.'BT') THEN
         ICOL = ICOL - NCOL
      ELSE IF (INCHAR.EQ.'DP') THEN
         IROW = IROW + NROW
      ELSE IF (INCHAR.EQ.'HO') THEN
         IROW = 1
         ICOL = 1
      ELSE IF (INCHAR.EQ.'EN') THEN
         IROW = NDATROW + 2
         ICOL = NUMCOL + 1
      ELSE IF (INCHAR.EQ.'RE') THEN
         ICOL = ICOL + 1
      ELSE IF (INCHAR(2:2).NE.' ') THEN
         CALL BEEP
      ELSE
         ICOL = ICOL + 1
      END IF
      RETURN
      END
**********************************************************************
      SUBROUTINE CHKCELL(IGRAPH,ITOPROW,IROW,ICOL,STRTROW,STRTCOL,
     +           NUMCOL,NDATROW,ROWHDR,DATARRAY,MXDATROW,RTNCODE)
C
C   ROUTINE TO CHECK THE VALUES OF IROW AND ICOL, ADJUST THEM TO STAY 
C   WITHIN THE LIMITS OF THE CURRENT SPREAD SHEET, AND WRAP, SCROLL, ETC
C   AS NECESSARY.
C
      INTEGER*2 IROW,ICOL,STRTROW,STRTCOL,NUMCOL,MAXCOL
      REAL DATARRAY(MXDATROW,*)
      CHARACTER*1 RTNCODE
      CHARACTER*(*) ROWHDR(*)
C      
      PARAMETER (NCOL=8,LENCOL=8)
C
      ITOP = ITOPROW+2
      NROW = 22-ITOP
      IF (IGRAPH.EQ.1) THEN
         MINROW = 0
      ELSE
         MINROW = 1
      ENDIF      
C
C   ADJUST IROW AND ICOL AS NECESSARY
C
      RTNCODE = '0'
      IROW = MAX0(MINROW,IROW)
      IF (ICOL.GT.NUMCOL+1) THEN
         IF (IROW.LT.NDATROW+2) THEN
            ICOL = 1
            IROW = IROW + 1
         ELSE
            ICOL = NUMCOL + 1
            IROW = NDATROW + 2
         END IF
      ELSE IF (ICOL.LE.0) THEN
         IF (IGRAPH.GT.1 .AND. IROW.EQ.1) THEN
            ICOL = 1
         ELSE   
            ICOL = NUMCOL + 1
            IROW = IROW - 1
         ENDIF   
      END IF
     
      IF (IROW.GT.NDATROW+2) THEN
         IROW = NDATROW + 2
      ELSE IF (IROW.LT.0) THEN
         IROW = MINROW
      END IF 
C
C   CHECK THAT USER IS IN A LEGAL CELL
C
      IF (IROW.EQ.0) THEN
        ICOL = MAX0(ICOL,2)
      END IF
      IF (IROW.EQ.1.AND.ICOL.GT.2) THEN
         IROW = 2
         ICOL = 2
      ELSE IF (IROW.EQ.2.AND.ICOL.LT.2) THEN
         IROW = 1
         ICOL = 2
      END IF
C
C   CHECK IF THE CELL IS AVAILABLE ON THE PRESENT SPREAD SHEET.
C   IF NOT, PAN OR SCROLL AS NECESSARY.  SET RTNCODE = '1' IF
C   THE SHEET MUST BE REDRAWN.
C        
      MAXCOL = STRTCOL + NCOL - 1         
      NCOLHF = NCOL/2
      IF (ICOL.LT.STRTCOL.OR.ICOL.GT.MAXCOL) THEN
         STRTCOL = NCOLHF * (((ICOL - 1) / NCOLHF) - 1) + 1
         STRTCOL = MAX0(STRTCOL,1)
         MAXCOL = STRTCOL + NCOL - 1         
         IF (NUMCOL+1.LT.MAXCOL) THEN
            STRTCOL = MAX0((NUMCOL-NCOL+2),1)
         ENDIF   
         RTNCODE = '1'
      END IF
C
      IF (IROW.NE.0) THEN
         IF (IROW.LT.STRTROW-1.OR.IROW.GT.STRTROW+NROW) THEN
            RTNCODE = '1'
         END IF
      END IF
C
C   IF SHEET MUST BE REDRAWN, SET NEW STRTROW.
C   ELSE SCROLL UP OR DOWN ONE LINE.
C 
      IF (RTNCODE.EQ.'1') THEN
         IF (IROW.LT.STRTROW.AND.IROW.NE.0) THEN
            STRTROW = IROW
         ELSE IF (IROW.GT.(STRTROW+NROW-1)) THEN
            STRTROW = IROW - NROW + 1
         END IF  
      ELSE
         IF (IROW.LT.STRTROW.AND.IROW.NE.0) THEN
            CALL CLTEXT(0,0,IERR)
            CALL SCROLL(0,1,ITOP+1,0,22,79)
            STRTROW = STRTROW - 1
            CALL WRTSLIN(ITOPROW,IROW,STRTROW,STRTCOL,ROWHDR,
     +                   DATARRAY,MXDATROW)
         ELSE IF (IROW.GT.(STRTROW+NROW-1)) THEN
            CALL CLTEXT(0,0,IERR)
            CALL SCROLL(1,1,ITOP+1,0,22,79)
            STRTROW = STRTROW + 1
            CALL WRTSLIN(ITOPROW,IROW,STRTROW,STRTCOL,ROWHDR,
     +                   DATARRAY,MXDATROW)
         END IF
      END IF  
C
      RETURN
      END

**********************************************************************
      SUBROUTINE WRTSHEET(IGRAPH,ITOPROW,STRTROW,STRTCOL,NCOPTA,NCOPTB,
     +   NUMCOL,NDATROW,ROWHDR,DATARRAY,MXDATROW,GAXLBL,CHRAXIS,COLAXIS)
C
C   ROUTINE WRITES THE CURRENT PAGE OF THE ENTIRE GRAFOPTN DATA AREA
C   SPREAD SHEET.
C
      INTEGER*2 STRTROW,STRTCOL,COLAXIS(*)
      REAL DATARRAY(MXDATROW,*)
      CHARACTER*(*) ROWHDR(*)
      CHARACTER *(*) GAXLBL
C
      PARAMETER (NCOL=8,LENCOL=8)
C      
      CHARACTER*(LENCOL+1) STRING
      CHARACTER*1 LETTER,CHRAXIS(0:1)
      INTEGER*2 ENDCOL
C
      ITOP = ITOPROW+2
      NROW = 22-ITOP            
      IHDRCOL = LENCOL-2
C
C   SET COLUMN LIMITS
C
      ENDCOL = STRTCOL + NCOL - 1
      ENDCOL = MIN0(ENDCOL,NUMCOL+1)
      IF (IGRAPH.EQ.1) THEN
C
C       .. WRITE THE GRAPH-AXIS COLUMN HEADER LINE
         CALL LOCATE(ITOP-1,0,IERR)
         STRING = ' '
         DO 50 ICOL = STRTCOL,ENDCOL
            IF (ICOL.EQ.1) THEN
               CALL WRTSTR(GAXLBL,10,14,0)
               CALL WRTSTR(STRING,(LENCOL+4-10),14,0)
            ELSE
               IF (ICOL.EQ.STRTCOL) THEN
                  CALL WRTSTR(STRING,3,14,0)
               END IF
               STRING(IHDRCOL:IHDRCOL) = CHRAXIS(COLAXIS(ICOL-1))
               CALL WRTSTR(STRING,LENCOL+1,14,0)
            END IF
   50    CONTINUE
      ENDIF
C
C   WRITE THE SPREAD SHEET COLUMN HEADER LINE 
C
      STRING = ' '
      CALL LOCATE(ITOP,0,IERR)
      CALL WRTSTR('   ',3,0,3)
      IOFF = 64
      DO 100 ICOL = STRTCOL,ENDCOL
         IF (ICOL.GT.26) IOFF=70
         LETTER = CHAR(ICOL+IOFF)
         STRING(IHDRCOL:IHDRCOL) = LETTER
         LTRCLR=0
         IF (ICOL-1.GE.NCOPTA .AND. ICOL-1.LE.NCOPTB) LTRCLR=4
         CALL WRTSTR(STRING,LENCOL+1,LTRCLR,3)
  100 CONTINUE
C
C   WRITE THE DATA LINES
C   NOTE:  WRTSLIN GETS TITLE/SUBTITLE FROM GRAFVAR COMMON
C
      ENDROW = STRTROW + NROW - 1
      IF (ENDROW.GT.NDATROW+2) THEN
         ENDROW = NDATROW+2
      END IF
      DO 200 IROW = STRTROW,ENDROW
         CALL WRTSLIN(ITOPROW,IROW,STRTROW,STRTCOL,ROWHDR,
     +                DATARRAY,MXDATROW)
  200 CONTINUE
      IBEG=25 - ((ENDROW-STRTROW+1)+ITOP+1)
      IEND=2
      DO 210 IROW=IBEG,IEND,-1
         CALL CLRMSG(IROW)
  210 CONTINUE       
C      
      RETURN
      END
***********************************************************************
      SUBROUTINE WRTSLIN(ITOPROW,IROW,STRTROW,STRTCOL,ROWHDR,
     +                   DATARRAY,MXDATROW)
C
C   ROUTINE TO WRITE A LINE IN THE DATA AREA SPREAD SHEET
C
      INTEGER*2 IROW,STRTROW,STRTCOL
      REAL DATARRAY(MXDATROW,*)

      PARAMETER (NCOL=8,LENCOL=8)
      CHARACTER*(*) ROWHDR(*)
      CHARACTER*(LENCOL+2) STRING
      CHARACTER*3 CHAR3
      INTEGER*2 ENDCOL
C
$INCLUDE: 'GRFPARM.INC'
$INCLUDE: 'GRAFVAR.INC'
C
C   BEGIN CODE -------------------
C
      ITOP = ITOPROW+2
      NROW = 22-ITOP       
C
      ENDCOL = STRTCOL + NCOL - 1
      ENDCOL = MIN0(ENDCOL,NUMCOL+1)
C
C       .. LOCATE EXPECTS JROW TO HAVE A RANGE OF 0-24
      JROW = IROW - STRTROW + 1 + ITOP
      CALL LOCATE(JROW,0,IERR)
C      
      WRITE(CHAR3,'(I3)') IROW
      CALL WRTSTR(CHAR3,3,0,3)

      STRING = '       '
      DO 180 ICOL = STRTCOL,ENDCOL
         IF (IROW.EQ.1) THEN
            IF (ICOL.EQ.1) THEN
               CALL WRTSTR(DATATITLE,LENCOL,14,0)
               CALL WRTSTR(STRING,1,14,0)
            ELSE IF (ICOL.EQ.2) THEN
               CALL WRTSTR(DATASUB,LENCOL,14,0)
               CALL WRTSTR(STRING,1,14,0)
            ELSE
               CALL WRTSTR(STRING,LENCOL+1,14,0)
            END IF
         ELSE IF (IROW.EQ.2) THEN
            IF (ICOL.EQ.1) THEN
               CALL WRTSTR(STRING,LENCOL+1,14,0)
            ELSE
               CALL WRTSTR(COLHDR(ICOL-1),LENCOL,14,0)
               CALL WRTSTR(' ',1,14,0)
            END IF
         ELSE
            IF (ICOL.EQ.1) THEN
               CALL WRTSTR(ROWHDR(IROW-2),LENCOL,14,0)
               CALL WRTSTR(' ',1,14,0)
            ELSE
               WRITE(STRING,520) DATARRAY(IROW-2,ICOL-1)
               CALL WRTSTR(STRING,LENCOL,14,0)
               CALL WRTSTR(' ',1,14,0)
            END IF
         END IF
180   CONTINUE
      RETURN
C
C       ** FORMAT STMTS
C
  501 FORMAT(I1)
  502 FORMAT(I2)      
  503 FORMAT(I3)      
  520 FORMAT(F8.1)
      END
**********************************************************************
      SUBROUTINE GRFAXIS(ITOPROW,ICOL,STRTCOL,CHRAXIS,COLAXIS,INCHAR)
C
C  ROUTINE ALLOWS USER TO CHANGE THE GRAPH-AXIS FOR COLUMN ICOL
C        
      PARAMETER (NCOL=8,LENCOL=8)
      INTEGER*2 ICOL,STRTCOL,COLAXIS
      CHARACTER*1 LETTER,CHRAXIS(0:1)
      CHARACTER*2 INCHAR
C
      ITOP = ITOPROW+2
      NROW = 22-ITOP
C
C       ** DISPLAY ROW AND COLUMN VALUES; CLEAR EXPANDED DISPLAY FIELD
C      
      IF (ICOL.EQ.1) THEN
         ICOL = 2
      END IF
      CALL LOCATE(ITOP-2,4,IERR)
      CALL WRTSTR('  0',3,15,1)
      IF (ICOL.GT.26) THEN
         IOFF=70
      ELSE
         IOFF=64
      ENDIF      
      LETTER = CHAR(ICOL+IOFF)
      CALL LOCATE(ITOP-2,15,IERR)
      CALL WRTSTR(LETTER,1,15,1)

      CALL LOCATE(ITOP-2,18,IERR)
      CALL WRTSTR(INCHAR,28,0,0)
C
C   HIGHLIGHT CURRENT VALUE AND ACCEPT USER INPUT
C
      JCOL = LENCOL + (ICOL-STRTCOL)*(LENCOL+1)
      CALL LOCATE(ITOP-1,JCOL,IERR)
      LETTER = CHRAXIS(COLAXIS)
      CALL GETSTR(0,LETTER,1,15,1,INCHAR)
      DO 15 I=0,1
       IF (LETTER.EQ.CHRAXIS(I)) GO TO 20
   15 CONTINUE
      I=COLAXIS  
   20 CONTINUE      
      COLAXIS = I
      CALL LOCATE(ITOP-1,JCOL,IERR)
      CALL WRTSTR(CHRAXIS(I),1,14,0)      
      RETURN
      END
      SUBROUTINE PLTPTR(IREW,IGRAPH,NUMCOL,NLOC,NHIC,NWID,
     +                  NCOLA,NCOLB,IDPLT)
C
C       ** OBJECTIVE:  CALCULATE POINTERS TO START AND END COLUMNS OF CURRENT
C                      PLOT.  IF IFLG=0 SET CURRENT PLOT TO START OF BAND
C       ** INPUT:
C             IREW......0=POINTERS SET FOR CURRENT PLOT; PLOT ID UNCHANGED
C                       1=PLOT ID AND POINTERS SET TO FIRST PLOT IN BAND
C             IGRAPH....GRAPH TYPE;  CALCULATIONS MADE FOR TIMESERIES AND MAP 
C             NUMCOL....NUMBER OF COLUMNS IN CURRENT BAND
C             NLOC......LOWEST PLOT COLUMN IN CURRENT BAND
C             NHIC......HIGHEST PLOT COLUMN IN CURRENT BAND
C             NWID......NUMBER OF COLUMNS IN CURRENT PLOT
C       ** OUTPUT:
C             NCOLA.....START COLUMN OF CURRENT PLOT 
C             NCOLB.....FINAL COLUMN OF CURRENT PLOT 
C             IDPLT.....ID NUMBER OF CURRENT PLOT IN THE BAND; DEFINED
C                       ONLY IF IREW=1
C
C       ** CALCULATIONS MADE FOR TIMESERIES AND MAP ONLY
C
      IF (IGRAPH.GT.2) GO TO 100
C      
C       ** SET PLOT ID AND STARTING COLUMN POINTER TO FIRST PLOT IN BAND
C
      IF (IREW.EQ.1) THEN
         IDPLT = 1
         IF (IGRAPH.EQ.1) THEN
            MINCOL=1 
         ELSE
            MINCOL=3
         ENDIF
         NCOLA = MAX0(NLOC,MINCOL)
      ENDIF   
C
C       ** CALCULATE END COLUMN POINTER
C
      IFRMWID = MIN0(NHIC,NUMCOL)
      NCOLB   = MIN0(NCOLA+NWID-1,IFRMWID)
C
  100 CONTINUE
      RETURN
      END      