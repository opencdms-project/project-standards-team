$STORAGE:2
      SUBROUTINE MLYSMY
C
C   ROUTINE TO PRINT THE MONTHLY SUMMARY INFORMATION FOR THE SELECTED
C      STATIONS
C
C   PROGRAM CONTROL VARIABLES
C
      PARAMETER (MAXELEM=65)
$INCLUDE:'MLYVAR.INC'
      CHARACTER*8 PREVSTN
      CHARACTER*7 OUTCNT(2)
      INTEGER*4  RECCOUNT,NRECCOUNT,IREC
      INTEGER*2  ISTRT(3),ISTOP(3),NUMDAYS(12) 
      INTEGER*4 YRMON,SRCHDATE
      CHARACTER*1 RTNCODE,PAGEFD,REPLY
C
C  INPUT DATA VARIABLES
C   
      CHARACTER LAT*7, LON*8,DISTRICT*20
      REAL ELEV
C
C   VARIABLES HOLDING ALL OF THE OUTPUT TITLES AND LEGENDS
C
      CHARACTER*54 HLDTXT
      CHARACTER*6 FRMAT1,FRMAT2
C
C   MONTHLY SUMMARY VARIABLES
C 
      CHARACTER*1 DAYFLAG,SUMTYPE,LIMTYPE(MAXELEM,3)
     +           ,MAXFLAG,MINFLAG
      CHARACTER*5 MSGTXT
      CHARACTER*6 PVAL1,PVAL2
      CHARACTER*6 PCOUNT(3)
      CHARACTER*8 PMON,PDEC(3)
      INTEGER*2 DECCNT(3),LIMCNT(3) 
      REAL TOTAL,DECTOT(3),LIMIT(MAXELEM,3),MAXVAL,MINVAL     
      COMMON /LIMITS/ LIMTYPE, LIMIT
      DATA NUMDAYS/31,28,31,30,31,30,31,31,30,31,30,31/
      DATA ISTRT/1,11,21/, ISTOP/10,20,0/
      DATA OUTCNT/'         ','         '/
      PAGEFD = CHAR(12)
      OUTCNT(2)(1:1) = CHAR(0)
C
C   INITIALIZE COUNTERS 
C
      NRECCOUNT = 0
      RECCOUNT = 0
      LINECNT = 999
C-----------------------------------------------------------------------
      DO 400 IREC=1,999999  

         CALL READDLY(DDSID,STNID,IELEM,YEAR,MONTH,VALUE,FLAG1,RTNCODE)
         RYEAR = YEAR
         RMON = MONTH
         RYRMON = RYEAR*100. + RMON
         YRMON = INT4(RYRMON)
         NRECCOUNT = NRECCOUNT + 1
         CALL LOCATE(24,15,IERR)
         WRITE(OUTCNT(1),'(I7)') NRECCOUNT
         CALL CWRITE(OUTCNT,12,IERR)
         IF (RTNCODE.EQ.'1') THEN
            GO TO 401
         ELSE IF (STNID.GT.ENDSTN.OR.(STNID.EQ.ENDSTN.AND.
     +         YRMON.GT.STRTYRMO)) THEN
            GO TO 401
         ELSE IF(STNID.LT.STARTSTN.OR.YRMON.NE.STRTYRMO) THEN
            GO TO 400
         END IF 
         IF (MONTH.EQ.2) THEN
            IF (MOD(YEAR,4).EQ.0) THEN
               NUMDAYS(MONTH) = 29
            ELSE
               NUMDAYS(MONTH) = 28
            END IF
         END IF
         ISTOP(3) = NUMDAYS(MONTH)
C
C     SKIP ELEMENTS FOR WHICH MLY STATS ARE NOT MEANINGFUL
C
         IF ((IELEM.GE.6.AND.IELEM.LE.11).OR.(IELEM.GE.29.AND.IELEM.
     +          LE.42)) THEN
            GO TO 400
         END IF
C
C     IF NEW ELEMENT ADD IT TO THE INTERNAL ELEMENT TABLE
C
         IF (RELELEM(IELEM).EQ.0) THEN
            CALL SETELEM(IELEM,NUMELEM,RELELEM,ELEM,ELEMABRV,ELEMNAME
     +                   ,ELEMUNITS,SMYFMT,VALFMT,' ',AVGTOT,RTNFLG)
            IF (RTNFLG.NE.'0') THEN
               WRITE(MSGTXT,'(I5)')MAXELEM
               CALL WRTMSG(4,104,12,1,0,MSGTXT,5)
               CALL LOCATE(22,1,IERR)
               CALL OKREPLY(REPLY,RTNCODE)        
               CALL CLRMSG(3)
               IF (REPLY.EQ.'N'.OR.RTNCODE.EQ.'1') THEN
                  CALL LOCATE(24,1,IERR)
                  STOP ' '
               ELSE
                  GO TO 401
               END IF
            END IF
         END IF
C
C     SKIP THIS ELEMENT IF THAT WAS INDICATED IN THE PARAMETER FILE
C     OTHERWISE SET VARIABLE TO INDICATE THIS ELEMENT HAS BEEN READ
C
         I4 = RELELEM(IELEM)
         IF (ESKIP(I4)) THEN
            GO TO 400
         END IF
C
         IF (.NOT.PRTELM(I4)) THEN
            PRTELM(I4) = .TRUE.
            ELMCNT = ELMCNT + 1
         END IF
C
         RECCOUNT = RECCOUNT + 1 
         WRITE(OUTCNT(1),'(I7)') RECCOUNT
         CALL LOCATE (24,44,IERR)
         CALL CWRITE(OUTCNT,12,IERR)
C         
C     SET SUMMARY TYPE AND PRINT FORMAT
C
         FRMAT1 = '(F6.1)'
         FRMAT2 = '(F5.1)'
         SUMTYPE = AVGTOT(I4)
         FRMAT1(2:5) = SMYFMT(I4)
         FRMAT2(2:5) = VALFMT(I4)
C
C     IF NEW PAGE WRITE ENDING LINES FOR PREVIOUS PAGE 
C
         IF (LINECNT+2.GT.MAXLINE) THEN
            IF (LINECNT.NE.999) THEN
               WRITE(50,200) LINCHR(7),(LINCHR(2),I5=1,24),LINCHR(5)
     +            ,(LINCHR(2),I6=1,8),LINCHR(5),(LINCHR(2),I7=1,4)
     +            ,LINCHR(5),LINCHR(2),LINCHR(5),((LINCHR(2),I8=1,8)
     +            ,LINCHR(5),I9=1,4),(LINCHR(2),I10=1,7),LINCHR(5)
     +            ,(LINCHR(2),I11=1,4),LINCHR(5),(LINCHR(2),I12=1,7)
     +            ,LINCHR(5),(LINCHR(2),I13=1,4),(LINCHR(5),(LINCHR(2)
     +            ,I14=1,5),I15=1,3),LINCHR(8)       
               WRITE(50,120) MFOOT(1),MFOOT(2) 
            END IF
            LINECNT = 7
            PRTCNT = PRTCNT + 7
            PREVSTN = 'XXXXXXX'
            DO 150 K = 32,1,-1
               IF (TITLE(1)(K:K).NE.' ') THEN
                  KLEN = K + 2
                  GO TO 160
               END IF
  150       CONTINUE
  160       CONTINUE
            HLDTXT = TITLE(1)
            HLDTXT(KLEN:54) = MONYEAR               
C
C      WRITE HEADER LINES FOR NEW PAGE
C            
            IF (FIRSTCALL) THEN
                WRITE(50,'(1X,8A1)') SETNRM
                FIRSTCALL = .FALSE.
            ELSE
                WRITE(50,'(1X,9A1)') PAGEFD,SETNRM
            END IF
            IF (HDFLAG) THEN
               LINECNT = 7 + NHDLINES
               IPAGE = IPAGE + 1
               WRITE(50,165) SETSML,PAGNAM,IPAGE,SETNRM
            END IF
            HDFLAG = .TRUE.
            WRITE(50,170) SETNRM,HLDTXT,SETNRM,SETSML
            WRITE(50,200) LINCHR(3),(LINCHR(2),I5=1,24),LINCHR(6)
     +         ,(LINCHR(2),I6=1,8),LINCHR(6),(LINCHR(2),I7=1,4)
     +         ,LINCHR(6),(LINCHR(2),I8=1,37),LINCHR(6),(LINCHR(2)
     +         ,I9=1,25),LINCHR(6),(LINCHR(2),I8=1,17),LINCHR(4)       
            WRITE(50,185) (LINCHR(1),I5=1,3),MTL13,LINCHR(1),STRTUND
     +         ,MTL14,LINCHR(1),MTL15,LINCHR(1),MTL16,STPUND,LINCHR(1)
            WRITE(50,190) LINCHR(1),MTL21,LINCHR(1),MTL22,LINCHR(1)
     +         ,MTL23,(LINCHR(1),K=1,2),(MTL24(L),LINCHR(1),L=1,4)
     +         ,MTL25(1),LINCHR(1),MTL25(2),LINCHR(1),MTL25(3)
     +         ,LINCHR(1),MTL25(4),LINCHR(1),(MTL26(M),LINCHR(1),M=1,3)
         END IF 
C
C      IF NEW STATION WRITE SEPARATOR LINE AND FIND STN INFO
C
         IF (STNID.NE.PREVSTN) THEN
            WRITE(50,200) LINCHR(9),(LINCHR(2),I5=1,24),LINCHR(11)
     +         ,(LINCHR(2),I6=1,8),LINCHR(11),(LINCHR(2),I7=1,4)
     +         ,LINCHR(11),LINCHR(2),LINCHR(11),((LINCHR(2),I8=1,8)
     +         ,LINCHR(11),I9=1,4),(LINCHR(2),I10=1,7),LINCHR(11)
     +         ,(LINCHR(2),I11=1,4),LINCHR(11),(LINCHR(2),I12=1,7)
     +         ,LINCHR(11),(LINCHR(2),I13=1,4),(LINCHR(11),(LINCHR(2)
     +         ,I14=1,5),I15=1,3),LINCHR(10)       
            LINECNT = LINECNT + 1
            PRTCNT = PRTCNT + 1
            PREVSTN = STNID
            RYEAR = YEAR
            RMONTH = MONTH
            RDATE = RYEAR*10000. + RMONTH*100. 
            SRCHDATE = INT4(RDATE)
            CALL RDGEOG(STNID,SRCHDATE,STNABRV,DISTRICT,LAT,LON
     +                    ,ELEV,RTNCODE)
            WRITE(60) STNID,STNABRV,DISTRICT,LAT,LON,ELEV
            STNCNT = STNCNT + 1
            PRTNAM = STNABRV
         ELSE
            PRTNAM = '   '
         END IF
C
C     DO THE COMPUTATION OF TOTALS,MEANS AND LIMITS COUNTS FOR THE
C     CURRENT ELEMENT.  FIRST INITIALIZE.
C
         TOTAL = 0.0
         ITOTAL = 0
         MAXVAL = -99999.
         MINVAL = 99999.
         DO 210 I3 = 1,3
            LIMCNT(I3) = 0
  210    CONTINUE                    
C
C     THEN FIND THE MEANS AND TOTALS BY 10 DAY PERIOD AS WELL
C     AS THE EXTREMES
C
         DO 240 I3 = 1,3
            DECTOT(I3) = 0.0
            DECCNT(I3) = 0 
            DO 230 I5 = ISTRT(I3),ISTOP(I3)
               IF(VALUE(I5).NE.-99999.) THEN
                  TMPVAL = VALUE(I5)
                  DECTOT(I3) = DECTOT(I3) + TMPVAL
                  DECCNT(I3) = DECCNT(I3) + 1 
                  IF (TMPVAL.GT.MAXVAL) THEN
                     MAXVAL = TMPVAL
                     MAXDAY = I5
                     MAXFLAG = ' '
                  ELSE IF (TMPVAL.EQ.MAXVAL) THEN
                     MAXDAY = I5
                     MAXFLAG = '*'
                  END IF
                  IF (TMPVAL.LT.MINVAL) THEN
                     MINVAL = TMPVAL
                     MINDAY = I5
                     MINFLAG = ' '
                  ELSE IF (TMPVAL.EQ.MINVAL) THEN
                     MINDAY = I5
                     MINFLAG = '*'
                  END IF
C
C           COUNT THE NUMBER OF TIMES THE VALUE LIMITS ARE EXCEEDEED
C
                  DO 225 I6 = 1,3
                     IF (LIMTYPE(I4,I6).EQ.'>') THEN
                        IF (TMPVAL.GT.LIMIT(I4,I6)) THEN
                           LIMCNT(I6) = LIMCNT(I6) + 1
                        END IF
                     ELSE IF (LIMTYPE(I4,I6).EQ.'<') THEN
                        IF (TMPVAL.LT.LIMIT(I4,I6)) THEN
                           LIMCNT(I6) = LIMCNT(I6) + 1
                        END IF
                     ELSE IF (LIMTYPE(I4,I6).EQ.'ò') THEN
                        IF (TMPVAL.GE.LIMIT(I4,I6)) THEN
                           LIMCNT(I6) = LIMCNT(I6) + 1
                        END IF
                     ELSE IF (LIMTYPE(I4,I6).EQ.'ó') THEN
                        IF (TMPVAL.LE.LIMIT(I4,I6)) THEN
                           LIMCNT(I6) = LIMCNT(I6) + 1
                        END IF
                     END IF
  225             CONTINUE   
               END IF
  230       CONTINUE
            TOTAL = TOTAL + DECTOT(I3)
            ITOTAL = ITOTAL + DECCNT(I3)
  240    CONTINUE
C
C      DETERMINE IF TOO MANY DAYS ARE MISSING
C 
         IMISS = NUMDAYS(MONTH) - ITOTAL
         IF (SUMTYPE.EQ.'A') THEN
            DO 250 I3 = 1,3
               IF (DECCNT(I3).GT.6) THEN
                  DECTOT(I3) = DECTOT(I3)/DECCNT(I3)
               END IF
  250       CONTINUE
            IF (IMISS.LT.9) THEN
               TOTAL = TOTAL / ITOTAL
            END IF
         END IF
C
C      FORMAT AND WRITE THE RESULTS FOR THIS ELEMENT
C
         DO 280 I3 = 1,3
            IF (DECCNT(I3).GT.6) THEN
               IF (FRMAT1(2:2).EQ.'F') THEN
                  WRITE(PDEC(I3),FRMAT1) DECTOT(I3)
               ELSE
                  NTOTAL = NINT(DECTOT(I3))
                  WRITE(PDEC(I3),FRMAT1) NTOTAL
               END IF
            ELSE
               PDEC(I3) = '   -    '
            END IF
  280    CONTINUE
         IF (IMISS.LT.9) THEN
            IF (FRMAT1(2:2).EQ.'F') THEN
               WRITE(PMON,FRMAT1) TOTAL
            ELSE
               NTOTAL = NINT(TOTAL)
               WRITE(PMON,FRMAT1) NTOTAL
            END IF
         ELSE
            PMON = '   -    '
         END IF
         IF (IMISS.EQ.0) THEN
            DAYFLAG = ' '
            DO 300 I3 = 1,3
               WRITE(PCOUNT(I3),'(I2)') LIMCNT(I3)
  300       CONTINUE 
         ELSE
            DAYFLAG = 'I'
            DO 320 I3 = 1,3
               PCOUNT(I3) = '- '
  320       CONTINUE
         END IF
         DO 340 I3 = 1,3
            IF (LIMTYPE(I4,I3).NE.'>'.AND.LIMTYPE(I4,I3).NE.'<'.AND.
     +            LIMTYPE(I4,I3).NE.'ò'.AND.LIMTYPE(I4,I3).NE.'ó') THEN
               PCOUNT(I3) = ' '
            END IF
  340    CONTINUE
         IF (FRMAT2(2:2).EQ.'F') THEN
            WRITE(PVAL1,FRMAT2) MAXVAL
            WRITE(PVAL2,FRMAT2) MINVAL
         ELSE
            IVAL = NINT(MAXVAL)
            WRITE(PVAL1,FRMAT2) IVAL            
            IVAL = NINT(MINVAL)
            WRITE(PVAL2,FRMAT2) IVAL
         END IF            
         WRITE(50,360) LINCHR(1),PRTNAM,LINCHR(1),ELEMABRV(I4)
     +       ,LINCHR(1),ITOTAL,DAYFLAG,LINCHR(1),SUMTYPE,LINCHR(1)
     +       ,(PDEC(I3),LINCHR(1),I3=1,3),PMON,LINCHR(1),PVAL1
     +       ,LINCHR(1),MAXDAY,MAXFLAG,LINCHR(1),PVAL2,LINCHR(1)
     +       ,MINDAY,MINFLAG,LINCHR(1),(PCOUNT(I3),LINCHR(1),I3=1,3)
         LINECNT = LINECNT + 1
         PRTCNT = PRTCNT + 1
  400 CONTINUE
  401 CONTINUE
      IF (RECCOUNT.EQ.0) THEN
         CALL WRTMSG(5,548,12,1,1,' ',0)
         CALL LOCATE(22,0,IERR)
         STOP ' '
      END IF
      WRITE(50,200) LINCHR(7),(LINCHR(2),I5=1,24),LINCHR(5)
     +   ,(LINCHR(2),I6=1,8),LINCHR(5),(LINCHR(2),I7=1,4)
     +   ,LINCHR(5),LINCHR(2),LINCHR(5),((LINCHR(2),I8=1,8)
     +   ,LINCHR(5),I9=1,4),(LINCHR(2),I10=1,7),LINCHR(5)
     +   ,(LINCHR(2),I11=1,4),LINCHR(5),(LINCHR(2),I12=1,7)
     +   ,LINCHR(5),(LINCHR(2),I13=1,4),(LINCHR(5),(LINCHR(2)
     +   ,I14=1,5),I15=1,3),LINCHR(8)       
      WRITE(50,120) MFOOT(1),MFOOT(2) 
      LINECNT = LINECNT + 3
      PRTCNT = PRTCNT + 3
C
      REWIND (25)
      RETURN
C
C   FORMAT STATEMENTS
C
  120 FORMAT(5X,A120,/,5X,A120)
  165 FORMAT(1X,8A1,115X,A6,1X,I2,8A1,//)
  170 FORMAT(1X,8A1,12X,A53,7X,16A1)
  185 FORMAT(1X,A1,24X,A1,8X,A1,A4,9A1,3X,A30,4X,A1,2X,A20,3X,A1
     +      ,A17,8A1,A1)
  190 FORMAT(1X,A1,1x,A20,3X,A1,A8,A1,A4,A1,1X,A1,4(A8,A1)
     +      ,2(A7,A1,A4,A1),3(A5,A1))
  200 FORMAT(1X,132A1)
  360 FORMAT(1X,A1,A24,A1,1X,A6,1X,A1,1X,I2,A1,A1,A1,A1,4(1X,A7,A1)
     +      ,2(A7,A1,1X,I2,2A1),3(2X,A2,1X,A1)) 
      END
