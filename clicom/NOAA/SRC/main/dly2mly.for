$STORAGE:2
      PROGRAM DLY2MLY
C
C     PROGRAM TO READ DATA FROM DATAEASE FORM "DAILY DATA"
C         AND COMPUTE MONTHLY SUMMARY STATISTICS ON THE DATA READ.
C         THE STATISTICS ARE WRITTEN TO MLY.DAT FOR NO-DUPE MERGE
C         WITH EXISTING DATA IN DATAEASE FORM "MONTHLY DATA".
C         MONTHLY DATA ARE GENERATED FOR ELEMENTS 2,3,5,12,13,15,16,
C         29-39, AND 49.
C
C       Output elements are generated as follows
C
C       Input      Output
C
C        002   --> 201, 204, 205, 243, 244, 245
C        003   --> 202, 206, 207, 240, 241, 242
C        005   --> 208, 210, 211, 246, 247, 248
C        049   --> 209, 212, 213
C        012   --> 249, 252, 253
C        013   --> 250, 254, 255
C        015   --> 256, 259, 260
C        016   --> 257, 261, 262
C        018   --> 214, 263, 264
C      029-039 --> 229-239
C      201+202 --> 203
C      249+250 --> 251
C      256+257 --> 258
C
      PARAMETER (NUMELEM = 21,MAXMONTHS=12,MAXDAYS=31,MAXOUT=65)
      CHARACTER*80 STRING,MSGLN1
      CHARACTER*64 FILNAME
      CHARACTER*8 STNID, PREVID,STRTSTN,ENDSTN,STRTDATE,CMDVAL(7)
      CHARACTER*1 FLAG1(MAXDAYS),RTNCODE,INLTYPE(3),LIMTYPE(NUMELEM,3)
     +           ,DATASOURCE,OUTFLAG(MAXMONTHS,MAXOUT)
      CHARACTER*7 OUTCNT(2)
      INTEGER*4 RECCOUNT, NRECCOUNT,IREC
      INTEGER*2 DDSID,YEAR, MONTH, PREVYEAR
     +         ,VALCOUNT(MAXMONTHS,MAXOUT),NUMDAYS(MAXMONTHS)
     +         ,INELEM(NUMELEM), OUTELEM(6,NUMELEM), MEAN(MAXOUT)
      INTEGER*4 YRMON, STRTYRMO,ENDYRMO
      REAL      VALUE(MAXDAYS),INLIMIT(3),LIMIT(NUMELEM,3)
     +          ,OUTVAL(MAXMONTHS,MAXOUT), INIT(MAXOUT)

      LOGICAL ELEMOK(MAXOUT),ERROR
      DATA NUMDAYS /31,28,31,30,31,30,31,31,30,31,30,31/
      DATA RECCOUNT/0/, NRECCOUNT/0/, OUTCNT/'       ','       '/
     +    ,LIMIT /63* -99999./
C
C  INPUT ELEMENTS ARE LISTED SO RELATIVE POSITION CAN BE USED AS
C  INDEX INTO THE OUTELEM ARRAY
C
      DATA INELEM /002,003,005,049,012,013,015,016,018,029,030,031
     +            ,032,033,034,035,036,037,038,039,0/
C
C  OUTELEM INDICATES THE POSITION IN THE OUTVAL ARRAY THE VALUES
C  FOR EACH OUTPUT ELEMENT ARE STORED - THE ACTUAL POSTION USED IS
C  OUTVAL - 199.  UNUSED POSITIONS ARE SET TO 200.  THERE ARE A MAX
C  OF 6 OUTPUT ELEMENT FOR EACH INPUT ELEMENT.
C   
      DATA OUTELEM /201,204,205,243,244,245
     +       ,202,206,207,240,241,242, 208,210,211,246,247,248
     +       ,209,212,213,3*200, 249,252,253,3*200 
     +       ,250,254,255,3*200, 256,259,260,3*200 
     +       ,257,261,262,3*200, 214,263,264,3*200
     +       ,229,5*200,230,5*200,231,5*200,232,5*200,233,5*200,234
     +       ,5*200,235,5*200,236,5*200,237,5*200,238,5*200,239,5*200
     +       ,203,251,258,3*200/
C
C   MEAN AND INIT ARE SET FOR EACH OUTPUT ELEMENT POSITION.  IF MEAN
C   = 1 THEN THE MONTHLY TOTALS ARE DIVIDED BY THE NUMBER OF DAYS 
C   BEFORE WRITTEN TO OUTPUT FILE.  INIT HOLDS THE VALUES USED TO
C   INITIALIZE EACH OUTPUT ELEMENT.
C 
      DATA MEAN /1,1,1,46*0,1,1,5*0,1,1,7*0/
      DATA INIT /4*0.,2*-99999.,99999.,-99999.,2*0.,4*-99999.,5*0.
     +       ,2*-99999.,99999.,-99999.,29*0.,2*-99999.,99999.
     +       ,-99999.,3*0.,2*-99999.,99999.,-99999.,2*-99999./
C----------------------------------------------------------------------
      OUTCNT(2)(1:1) = CHAR(0)
C
C   OPEN THE OUTPUT FILE NO MATTER WHAT.  THAT WAY IT WILL NOT HAVE
C   OLD DATA IN IT IF THE PROGRAM IS NOT RUN.) 
C
      OPEN(51,FILE='Q:CLIMDATA.DAT',STATUS='UNKNOWN',FORM='BINARY')
      ENDFILE 51
      REWIND 51
C
      FILNAME = 'P:\HELP\DLY2MLY.HLP'
      CALL SETMOD(3,IERR)
C
C   READ THE LIMIT CATEGORIES FOR THE ELEMENTS OF INTEREST
C
   50 CONTINUE
      OPEN(8,FILE='P:\DATA\DLY2MLY.LIM',STATUS='OLD',IOSTAT=IOCHK)
      IF(IOCHK.NE.0) THEN
         CALL OPENMSG('P:\DATA\DLY2MLY.LIM   ','DLY2MLY     ',IOCHK)
         GO TO 50
      END IF   
      ERROR = .FALSE.
      DO 150 I1 = 1,999
         READ (8,'(A78)',END=155) STRING
         CALL PARSE1(STRING,78,7,8,CMDVAL,RTNCODE)
         IF (RTNCODE.NE.'0'.AND.RTNCODE.NE.'2') THEN
            GO TO 135
         END IF
         READ(CMDVAL(1),'(I3)',ERR=135) IELEM
         READ(CMDVAL(2),'(A1)')           INLTYPE(1)
         READ(CMDVAL(3),'(F8.0)',ERR=135) INLIMIT(1)
         READ(CMDVAL(4),'(A1)')           INLTYPE(2)
         READ(CMDVAL(5),'(F8.0)',ERR=135) INLIMIT(2)
         READ(CMDVAL(6),'(A1)')           INLTYPE(3)
         READ(CMDVAL(7),'(F8.0)',ERR=135) INLIMIT(3)
         NELEM = 0
         DO 100 I2 = 1, NUMELEM
            IF (IELEM.EQ.INELEM(I2)) THEN
               NELEM = I2
            END IF
100      CONTINUE
         IF (NELEM.GT.0) THEN
            DO 130 I3 = 1,3
               LIMTYPE(NELEM,I3) = INLTYPE(I3)
               LIMIT(NELEM,I3) = INLIMIT(I3)
130         CONTINUE
         END IF
         GO TO 150
C
135      CONTINUE
         ERROR = .TRUE.
         CALL LOCATE(21,0,IERR)
         CALL WRTSTR(STRING,78,14,0)
         WRITE(MSGLN1,'(A20,I2)') 'P:/DATA/DLY2MLY.LIM',I1
         CALL WRTMSG(3,173,12,1,1,MSGLN1,22)
150   CONTINUE
155   CONTINUE
      CLOSE(8) 
      IF (ERROR) THEN
         CLOSE(51)
         STOP 2 
      END IF
C
C   GET THE SELECTION INFORMATION FROM THE USER
C
180   CONTINUE
      CALL LOCATE(1,1,IERR)
      CALL GETLIMIT(STRTSTN,ENDSTN,STRTYRMO,ENDYRMO,FILNAME,RTNCODE)
      IF (RTNCODE.EQ.'1') THEN
         CALL LOCATE(23,0,IERR)
         STOP 2
      END IF
      CALL LOCATE(12,15,IERR)
      CALL DATASRC(15,DATASOURCE,RTNCODE)
      IF (RTNCODE.NE.'0') THEN
         GO TO 180
      END IF 
C
C   OPEN THE INPUT FILE - OPENPOS REQUIRES FTN 4.0 OR LATER.  IF USING
C   FTN 3.3 YPU MUST REPLACE CALL TO OPENPOS WITH CALL TO OPENINPUT
C
      WRITE(STRTDATE,'(I6)') STRTYRMO
      CALL OPENPOS('DLY',DATASOURCE,STRTSTN,STRTDATE)
C
C   INITIALIZE
C
      PREVYEAR = 0
      PREVID = 'XXXXXXX'
C
C  WRITE THE RUNNING TOTAL LINE
C
      CALL CLRMSG(1)
      CALL LOCATE(24,0,IERR)
      CALL WRTSTR('Records Read -          Records processed - '
     +             ,44,14,0)
C
C   DO THE PROCESSING --------------------------------------------------
C
      DO 500 IREC = 1,999999
         CALL READDLY(DDSID,STNID,IELEM,YEAR,MONTH,VALUE,FLAG1
     +        ,RTNCODE)
C
C     AT THE END OF EACH STATION-YEAR WRITE OUT THE DATA FOR THAT PERIOD
C
         IF (STNID.NE.PREVID.OR.YEAR.NE.PREVYEAR.OR.RTNCODE.EQ.'1') THEN
            IF (PREVID.EQ.'XXXXXXX') GO TO 360
            DO 300 NELEM = 2,MAXOUT
               IF (ELEMOK(NELEM)) THEN
C
C          CHECK FOR MISSING OR INCOMPLETE MONTHS - ALSO CONVERT TOTALS
C          TO MEANS FOR THOSE ELEMENTS WHERE MEANS ARE WANTED (MEAN = 1)
C
                 ELEMOK(NELEM) = .FALSE.
                 DO 200 M = 1,MAXMONTHS
                     IF (VALCOUNT(M,NELEM).GT.20) THEN
                        ELEMOK(NELEM) = .TRUE.
                        IF (MEAN(NELEM).EQ.1) THEN
                           OUTVAL(M,NELEM)=
     +                        OUTVAL(M,NELEM)/VALCOUNT(M,NELEM)
                        END IF
                        IF (VALCOUNT(M,NELEM).LT.NUMDAYS(M)) THEN
                           OUTFLAG(M,NELEM) = 'I'
                        END IF
                     ELSE
                        OUTFLAG(M,NELEM) = 'M'
                        OUTVAL(M,NELEM) = -99999.
                     END IF
200               CONTINUE
               END IF
300         CONTINUE
C
C         COMPUTE AND WRITE THE MEAN MONTHLY VALUES FOR TEMP, DEWPT, RH
C         VALUES ARE GIVEN AS ARRAY POSITIONS SO 2,3,4 REFERS TO 
C         OUTPUT ELEMENTS 201, 202, 203  ETC.
C
            CALL SETMEAN(2,3,4,ELEMOK,OUTVAL,OUTFLAG,MAXMONTHS)
            CALL SETMEAN(50,51,52,ELEMOK,OUTVAL,OUTFLAG,MAXMONTHS)
            CALL SETMEAN(57,58,59,ELEMOK,OUTVAL,OUTFLAG,MAXMONTHS)
C
C       WRITE OUT THE ELEMENTS THAT HAVE BEEN GENERATED
C
            DO 350 NELEM = 2,MAXOUT
               IF (ELEMOK(NELEM)) THEN
                  JELEM = NELEM + 199
                  CALL WRTMLY(DDSID,PREVID,PREVYEAR,JELEM
     +                  ,OUTVAL(1,NELEM),OUTFLAG(1,NELEM))
               END IF
350         CONTINUE
360         CONTINUE
C
            IF (RTNCODE.NE.'0') THEN
               GO TO 501
            END IF
C
C        RESET ALL OUTPUT VALUES FOR NEW YEAR
C
            DO 370 I=1,MAXOUT
               ELEMOK(I) = .FALSE.
               DO 370 M=1,MAXMONTHS
                  OUTVAL(M,I) = INIT(I)
                  OUTFLAG(M,I) = ' '
                  VALCOUNT(M,I) = 0
370         CONTINUE  
            PREVID = STNID
            PREVYEAR = YEAR
         END IF
C
C   CHECK IF THE CURRENT RECORD SHOULD BE PROCESSED
C
         NRECCOUNT = NRECCOUNT + 1
         CALL LOCATE(24,15,IERR)
         WRITE(OUTCNT(1),'(I7)') NRECCOUNT
         CALL CWRITE(OUTCNT,12,IERR)
         RYEAR = YEAR
         RMON = MONTH
         RYRMON = RYEAR*100. + RMON
         YRMON = INT4(RYRMON)
         IF (STNID.GT.ENDSTN.OR.(STNID.EQ.ENDSTN.AND.YRMON.GT.ENDYRMO))
     +         THEN
            GO TO 501
         ELSE IF (STNID.LT.STRTSTN.OR.YRMON.LT.STRTYRMO.OR.
     +          YRMON.GT.ENDYRMO) THEN
            GO TO 500
         END IF
         RECCOUNT = RECCOUNT + 1
         CALL LOCATE(24,44,IERR)
         WRITE(OUTCNT(1),'(I7)') RECCOUNT
         CALL CWRITE(OUTCNT,12,IERR)
C
C   FIND THE ELEMENT NUMBER OF THIS ELEMENT AND THUS FIND THE
C   ELEMENT-CODES FOR THE FIRST ELEMENT THAT IS GENERATED FOR
C   THIS INPUT ELEMENT
C
         NELEM = 0
         DO 380 I2 = 1, NUMELEM
            IF (IELEM.EQ.INELEM(I2)) THEN
               NELEM = I2
               GO TO 385
            END IF
380      CONTINUE
385      CONTINUE
         IF (NELEM.EQ.0) THEN
            GO TO 500
         ELSE
            NVAL = OUTELEM(1,NELEM) - 199
            IF (NVAL.EQ.1) THEN
               GO TO 500
            END IF
         END IF
C
C   COMPUTE THE SUMMARY VALUES FOR THE CURRENT ELEMENT
C
         ELEMOK(NVAL) = .TRUE.
         DO 400 J=1,MAXDAYS
            IF (VALUE(J).NE.-99999.) THEN
               VALCOUNT(MONTH,NVAL) = VALCOUNT(MONTH,NVAL) + 1
C
C        FOR ELEMENTS 10 AND UP (029-039) SUM THE DAYS WITH WEATHER
C
               IF (NELEM.GE.10) THEN
                  IF (VALUE(J).GT.0) THEN
                     OUTVAL(MONTH,NVAL) = OUTVAL(MONTH,NVAL) + 1.0
                  END IF
C
C        FOR THE OTHER ELEMENTS, SUM THE VALUES AND CHECK 
C        THE MAXIMUM OR MINIMUM (WHICHEVER IS APPROPRIATE)
C
               ELSE
                  OUTVAL(MONTH,NVAL) = OUTVAL(MONTH,NVAL) + VALUE(J)
                  NVAL2 = OUTELEM(2,NELEM) - 199
                  NDAY = OUTELEM(3,NELEM) - 199
                  ELEMOK(NVAL2) = .TRUE.
                  ELEMOK(NDAY) = .TRUE.
                  VALCOUNT(MONTH,NVAL2) = VALCOUNT(MONTH,NVAL2) + 1
                  VALCOUNT(MONTH,NDAY) = VALCOUNT(MONTH,NDAY) + 1
                  IF (IELEM.EQ.003.OR.IELEM.EQ.013.OR.IELEM.EQ.016) THEN
                     IF (VALUE(J).EQ.OUTVAL(MONTH,NVAL2)) THEN
                        OUTFLAG(MONTH,NVAL2) = '*'
                        OUTFLAG(MONTH,NDAY) = '*'
                     ELSE IF (VALUE(J).LT.OUTVAL(MONTH,NVAL2)) THEN
                        OUTVAL(MONTH,NVAL2) = VALUE(J)
                        OUTVAL(MONTH,NDAY) = FLOAT(J)
                        OUTFLAG(MONTH,NVAL2) = ' '
                        OUTFLAG(MONTH,NDAY) = ' '
                     END IF
                  ELSE
                     IF (VALUE(J).EQ.OUTVAL(MONTH,NVAL2)) THEN
                        OUTFLAG(MONTH,NDAY) = '*'
                        OUTFLAG(MONTH,NVAL2) = '*'
                     ELSE IF (VALUE(J).GT.OUTVAL(MONTH,NVAL2)) THEN
                        OUTVAL(MONTH,NVAL2) = VALUE(J)
                        OUTVAL(MONTH,NDAY) = FLOAT(J)
                        OUTFLAG(MONTH,NVAL2) = ' '
                        OUTFLAG(MONTH,NDAY) = ' '
                     END IF
                  END IF
               END IF
            END IF
  400    CONTINUE
C
C   FOR THE FIRST THREE ELEMENTS (002, 003, 005), ADD UP THE NUMBER OF
C   DAYS THAT EXCEED GIVEN LIMIT CATEGORIES
C  
         IF (NELEM.LE.3) THEN
            DO 480 I5 = 1,3
               NVAL = OUTELEM(I5+3,NELEM) - 199
               IF (LIMIT(NELEM,I5).NE.-99999.) THEN
                  ELEMOK(NVAL) = .TRUE.
                  IF (LIMTYPE(NELEM,I5).EQ.'>') THEN
                     DO 440 J = 1,MAXDAYS
                        IF (VALUE(J).GT.LIMIT(NELEM,I5)) THEN
                           OUTVAL(MONTH,NVAL) =
     +                               OUTVAL(MONTH,NVAL) + 1.0
                        END IF
                        IF (VALUE(J).NE.-99999.) THEN
                           VALCOUNT(MONTH,NVAL) = VALCOUNT(MONTH,NVAL)+1
                        END IF
  440                CONTINUE
                  ELSE IF (LIMTYPE(NELEM,I5).EQ.'<') THEN
                     DO 450 J = 1,MAXDAYS
                        IF (VALUE(J).NE.-99999.) THEN
                           VALCOUNT(MONTH,NVAL) = VALCOUNT(MONTH,NVAL)+1
                           IF (VALUE(J).LT.LIMIT(NELEM,I5)) THEN
                              OUTVAL(MONTH,NVAL) =
     +                               OUTVAL(MONTH,NVAL) + 1.0
                           END IF
                        END IF
  450                CONTINUE
                  ELSE IF (LIMTYPE(NELEM,I5).EQ.'ò') THEN
                     DO 460 J = 1,MAXDAYS
                        IF (VALUE(J).NE.-99999.) THEN
                           VALCOUNT(MONTH,NVAL) = VALCOUNT(MONTH,NVAL)+1
                           IF (VALUE(J).GE.LIMIT(NELEM,I5)) THEN
                              OUTVAL(MONTH,NVAL) =
     +                               OUTVAL(MONTH,NVAL) + 1.0
                           END IF
                        END IF
  460                CONTINUE
                  ELSE IF (LIMTYPE(NELEM,I5).EQ.'ó') THEN
                     DO 470 J = 1,MAXDAYS
                        IF (VALUE(J).NE.-99999.) THEN
                           VALCOUNT(MONTH,NVAL) = VALCOUNT(MONTH,NVAL)+1
                           IF (VALUE(J).LE.LIMIT(NELEM,I5)) THEN
                              OUTVAL(MONTH,NVAL) =
     +                               OUTVAL(MONTH,NVAL) + 1.0
                           END IF
                        END IF
  470                CONTINUE
                  END IF
               END IF
  480       CONTINUE
         END IF
C
C    END OF MAJOR PROGRAM READ LOOP 
C
  500 CONTINUE
  501 CONTINUE
      CLOSE(51)
      CALL BEEP
      CALL BEEP
      WRITE(*,2010) RECCOUNT
 2010 FORMAT(///,'  Processing Complete',/
     +      ,2X,I7,' Records Processed')
      IF (RECCOUNT.GT.0) THEN
         STOP ' '
      ELSE
         CALL WRTMSG(5,548,12,1,1,' ',0)
         CALL LOCATE(22,0,IERR)
         STOP 2
      END IF
      END
************************************************************************
      SUBROUTINE SETMEAN(ELEM1,ELEM2,NVAL,ELEMOK,OUTVAL,OUTFLAG
     +     ,MAXMON)
C
C   ROUTINE TO CHECK IF THERE IS SUFFICIENT DATA FOR IELEM1 AND IELEM2
C   AND IF THERE IS, COMPUTE THEIR MEAN AND STORE IT INTO POSITION 
C   NVAL OF OUTVAL. 
C
      PARAMETER (MAXOUT=65,MAXMONTHS=12)
      INTEGER*2 ELEM1,ELEM2, MAXMON, NVAL
      REAL*4 OUTVAL(MAXMON,MAXOUT)
      CHARACTER*1 OUTFLAG(MAXMON,MAXOUT)
      LOGICAL ELEMOK(MAXOUT), MEANOK
C
      IF (ELEMOK(ELEM1).AND.ELEMOK(ELEM2)) THEN
         MEANOK = .FALSE.
         DO 100 M = 1,MAXMONTHS
            IF (OUTFLAG(M,ELEM1).NE.'M'.AND.OUTFLAG(M,ELEM2).NE.'M')THEN
               MEANOK = .TRUE.
               OUTVAL(M,NVAL) = (OUTVAL(M,ELEM1) + OUTVAL(M,ELEM2))/2
               IF (OUTFLAG(M,ELEM1).EQ.'I'.OR.OUTFLAG(M,ELEM2).EQ.'I')
     +                 THEN
                  OUTFLAG(M,NVAL) = 'I'
               ELSE
                  OUTFLAG(M,NVAL) = ' '
               END IF
            ELSE
               OUTVAL(M,NVAL) = -99999.
               OUTFLAG(M,NVAL) = 'M'
            END IF
100      CONTINUE
         IF (MEANOK) THEN
            ELEMOK(NVAL) = .TRUE.
         END IF
      END IF
      RETURN
      END 
************************************************************************
$page
      SUBROUTINE WRTMLY(DDSID,STNID,IYEAR,IELEM,VALUE,FLAG1)
C
C   THIS ROUTINE WRITES A MLY FORMAT DATAEASE RECORD AS A BINARY FILE.
C   THE VARIABLES WITHIN THE RECORD ARE EQUIVALENCED TO THE APPROPRIATE
C   POSITION WITHIN INREC.                      
C
      CHARACTER*8 STNID, LOCALID
      CHARACTER*4 YEAR
      CHARACTER*3 ELEM,DDS
      CHARACTER*1 FLAG1(12), INREC(84) 
      INTEGER*2 HEADER(2)
      INTEGER*2 DDSID, IYEAR, IELEM
      REAL      VALUE(12)
C
C  EQUIVALENCE THE INPUT VARIABLES TO THE INPUT RECORD STRING        
C
      EQUIVALENCE (HEADER,INREC(1)),(DDS,INREC(7)), 
     +            (LOCALID,INREC(10)),
     +            (ELEM,INREC(18)),(YEAR,INREC(21)) 
C
C   WRITE THE INFORMATION INTO THE DATEASE RECORD HEADER AND THEN
C   WRITE THE VALUES AND FLAGS             
C
      HEADER(1) = 12
      HEADER(2) = 0
      LOCALID = STNID
      WRITE(DDS,'(I3.3)') DDSID
      WRITE(YEAR,'(I4)') IYEAR 
      WRITE(ELEM,'(I3.3)') IELEM
C
      DO 200 I=1,12
         IPOS = 25 + (I-1)*5
         CALL MOVBYT(4,VALUE(I),1,INREC,IPOS)
         INREC(IPOS+4) = FLAG1(I)
200   CONTINUE
 
      WRITE(51) INREC
      RETURN
      END
