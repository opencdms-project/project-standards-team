$STORAGE:2

      PROGRAM TENDEXTR
C
C   PROGRAM TO COMPUTE 10-DAY EXTREMES AND MEANS AS REQUESTED BY READING
C   THE DATAEASE FORM "10-DAY DATA" THRU THE SUBROUTINE READ10D AND
C   WRITING A MEANS/EXTREMES DATAEASE RECORD.
C   THE PROGRAM ASKS THE USER FOR THE STATIONS AND DATE RANGE 
C   TO BE CONSIDERED AND READS THE ELEMENTS TO BE USED FROM 10DEXTR.PRM.
C   IT WILL ALSO WORK WITH A SUBSET OF DATA HELD IN THE DATAEASE
C   FORM HOLD DLY DATA.
C
      PARAMETER (MAXDECS=36, MAXELEM=40)
C
C  PROGRAM CONTROL -- INPUT VARIABLES
C
      CHARACTER*64 FILNAME
      CHARACTER*16 MONNAME(13) 
      CHARACTER*8  STRTSTN,ENDSTN,PREVID,STNID,FIELD(4),STRTDATE
      CHARACTER*2  RTNFLAG
      CHARACTER*1  DATASOURCE,RTNCODE,FLAG1(MAXDECS)
      CHARACTER*3  MSGTXT
      INTEGER*4    RECCOUNT,NRECCOUNT,IREC
      INTEGER*2    STRTYR,ENDYR,DDSID,ELEM,YEAR,
     +             DEC,READELEM,ELEMCODE(MAXELEM)
      LOGICAL DATAOK
C 
C  VARIABLES FOR COMPUTATION OF RESULTS
C
      REAL*8      MEAN(MAXDECS,MAXELEM)
      REAL*4      EXTMAX(MAXDECS,MAXELEM), EXTMIN(MAXDECS,MAXELEM),
     +            VALUE(MAXDECS)
      INTEGER*2   MAXYEAR(MAXDECS,MAXELEM), MINYEAR(MAXDECS,MAXELEM),
     +            NUMYEAR(MAXDECS,MAXELEM), LOWYR(MAXDECS,MAXELEM)
      CHARACTER*1 MAXFLAG(MAXDECS,MAXELEM), MINFLAG(MAXDECS,MAXELEM)
      CHARACTER*7 OUTCNT(2)
C
      COMMON /FLD/ MEAN,EXTMAX,EXTMIN,MAXYEAR,MINYEAR,LOWYR,NUMYEAR
     +            ,MAXFLAG,MINFLAG
      DATA OUTCNT/'       ','       '/,  DATAOK /.FALSE./
      OUTCNT(2)(1:1) = CHAR(0)
C----------------------------------------------------------------------
C     (OPEN THE OUTPUT FILE NO MATTER WHAT.  THAT WAY IT WILL NOT HAVE
C      OLD DATA IN IT IF THE PROGRAM IS NOT RUN.) 
C----------------------------------------------------------------------
      OPEN(51,FILE='Q:CLIMDATA.DAT',STATUS='UNKNOWN',FORM='BINARY')
C
      CALL GETMON(MONNAME,16)
      DO 15 K=1,MAXELEM
         ELEMCODE(K)=0
   15 CONTINUE
C-------------------------------------------------------------------------
C     READ FILE OF 10-DAY ELEMENT CODES THAT REQUIRE A MEAN/EXTREME RECORD
C-------------------------------------------------------------------------
   20 CONTINUE
      OPEN (61,FILE='P:\DATA\10DEXTR.PRM',STATUS='OLD',FORM='FORMATTED'
     +    ,IOSTAT=IOCHK)
      IF (IOCHK.NE.0) THEN
         CALL OPENMSG('P:\DATA\10DEXTR.PRM   ','DLYEXTR     ',IOCHK)
         GO TO 20
      END IF 
C      
C--------------THROW AWAY 1ST LINE OF FILE, EXPLANATION ONLY-----------
C 
      READ(61,25,END=900)
      DO 30 I=1,999
         READ(61,25,END=35) INELEM
   25    FORMAT(I3)
         IF (I.GT.MAXELEM) THEN
            WRITE(OUTCNT,'(I7)') MAXELEM
            CALL WRTMSG(3,104,12,1,1,OUTCNT,7)
            GO TO 35
         ELSE
            ELEMCODE(I) = INELEM
         END IF    
   30 CONTINUE            
   35 CONTINUE
      CLOSE(61)  
      READELEM = I - 1
      IF (READELEM.LT.1)  THEN
         GO TO 900
      END IF   
C
C   SORT ELEMENTS REQUESTED INTO ASCENDING ORDER
C
      DO 45 I = 2,READELEM
         IF (ELEMCODE(I).LT.ELEMCODE(I-1)) THEN
            DO 40 J = I,2,-1
               IF (ELEMCODE(J).LT.ELEMCODE(J-1)) THEN
                  INELEM = ELEMCODE(J)
                  ELEMCODE(J) = ELEMCODE(J-1) 
                  ELEMCODE(J-1) = INELEM
               ELSE
                  GO TO 45
               END IF
40          CONTINUE
         END IF
45    CONTINUE
C
C------------ READ CONTROL INFO  -------------------------------------
C
      FILNAME = 'P:\HELP\10DEXTR.HLP '
      CALL SETMOD(3,IERR)
      FIELD(1) = ' '
      FIELD(2) = ' '
      FIELD(3) = ' '
      FIELD(4) = ' '
      CALL CLS
   60 CONTINUE
      CALL LOCATE(1,0,IERR)
      RTNFLAG  = 'SS'
      CALL GETFRM('ANNPLOT ',FILNAME,FIELD,8,RTNFLAG)
      CALL POSLIN(IROW,IERR)
      IF (RTNFLAG.EQ.'4F') THEN
         CALL LOCATE(23,0,IERR)
         STOP 2  
      END IF
      STRTSTN = FIELD(1)
      ENDSTN  = FIELD(2)  
      READ(FIELD(3),'(I4)') STRTYR    
      READ(FIELD(4),'(I4)') ENDYR
      STRTDATE = FIELD(3)
      CALL LOCATE(12,15,IERR)
C
C--------  SELECT THE SOURCE OF THE DATA TO BE READ  --------------
C
      CALL DATASRC(8,DATASOURCE,RTNCODE)
      IF (RTNCODE.NE.'0') THEN
         GO TO 60
      END IF
C
C   OPEN THE INPUT FILE - OPENPOS REQUIRES FTN 4.0 OR LATER.  IF USING
C   FTN 3.3 YPU MUST REPLACE CALL TO OPENPOS WITH CALL TO OPENINPUT
C
      CALL OPENPOS('10D',DATASOURCE,STRTSTN,STRTDATE)

C      CALL OPENINPUT('10D',DATASOURCE)

      NRECCOUNT = 0
      RECCOUNT = 0
C      LOWYR = 9999
C
C-----------  WRITE THE RUNNING TOTAL LINE  ---------------
C
      CALL CLRMSG(1)
      CALL LOCATE(24,0,IERR)
      CALL WRTSTR('Records Read -          Records processed - '
     +             ,44,14,0)
      PREVID='XXXXXXXX'
C-----------------------------------------------------------------------
C        READ A 10-DAY RECORD AND PROCESS IF WITHIN SELECTED LIMITS    
C-----------------------------------------------------------------------
      DO 500 IREC=1,999999
         CALL READ10D(DDSID,STNID,IELEM,YEAR,VALUE,FLAG1,RTNCODE)
         NRECCOUNT = NRECCOUNT + 1
         CALL LOCATE(24,15,IERR)
         WRITE(OUTCNT(1),'(I7)') NRECCOUNT
         CALL CWRITE(OUTCNT,12,IERR)
         IF (STNID.GT.ENDSTN.OR.(STNID.EQ.ENDSTN.AND.YEAR.GT.ENDYR))
     +         THEN
            RTNCODE = '1'
         END IF
         IF (STNID.NE.PREVID.OR.RTNCODE.EQ.'1') THEN
            IF (PREVID.EQ.'XXXXXXXX')  THEN
               GO TO 270
            END IF   
C----------------------------------------------------------------------
C          NEW STATION FOUND. DO THE REQUESTED CALCULATIONS ON OLD STN
C----------------------------------------------------------------------
           DO 150 MM  = 1,READELEM
           DO 150 DEC = 1,MAXDECS
              IF(NUMYEAR(DEC,MM).GT.1) THEN
                 XYR=NUMYEAR(DEC,MM)
                 MEAN(DEC,MM)=MEAN(DEC,MM)/XYR
              END IF
  150      CONTINUE
C------------------------------------------------------------------------
C       WRITE OUT ALL OF THE MEANS/EXTREMES GENERATED FOR THIS STATION
C------------------------------------------------------------------------
           CALL WRTDEX(PREVID,READELEM,ELEMCODE,DATAOK)

           IF(RTNCODE.NE.'0') THEN
              GO TO 501
           END IF
C------------------------------------------------------------------------
C               INITIALIZE OR RESET
C------------------------------------------------------------------------
  270       CONTINUE
            IF (RTNCODE.EQ.'1'.AND.PREVID.EQ.'XXXXXXXX') THEN
               CALL WRTMSG(5,548,12,1,1,' ',0)
               CALL LOCATE(22,0,IERR)
               STOP 2
            END IF
            IF (STNID.LT.STRTSTN.OR.YEAR.LT.STRTYR.OR.
     +            YEAR.GT.ENDYR) THEN
               GO TO 500
            END IF
            PREVID=STNID

            DO 280 J=1,READELEM
               DO 280 I=1,MAXDECS
                  MEAN(I,J)    = 0.0
                  EXTMAX(I,J)  = -99999.
                  EXTMIN(I,J)  = 99999.
                  LOWYR(I,J)   = 9999
                  MAXYEAR(I,J) = -9999
                  MINYEAR(I,J) = -9999
                  MAXFLAG(I,J) = 'M'
                  MINFLAG(I,J) = 'M'
                  NUMYEAR(I,J) = 0
  280       CONTINUE 
         END IF
         IF (STNID.LT.STRTSTN.OR.YEAR.LT.STRTYR.OR.
     +          YEAR.GT.ENDYR) THEN
            GO TO 500
         END IF
C------------------------------------------------------------------------
C                 FIND RELATIVE ELEMENT NUMBER
C------------------------------------------------------------------------  
         DO 370 KK=1,READELEM
            IF(IELEM.EQ.ELEMCODE(KK))  THEN
               ELEM=KK
               GO TO 390
            ENDIF
  370    CONTINUE
         GO TO 500                       
  390    RECCOUNT = RECCOUNT + 1
         CALL LOCATE(24,44,IERR)
         WRITE(OUTCNT(1),'(I7)') RECCOUNT
         CALL CWRITE(OUTCNT,12,IERR)
C------------------------------------------------------------------------
C          TALLY MEAN/EXTREME DATA FOR REQUESTED ELEMENTS ON CURRENT STN
C------------------------------------------------------------------------
         DO 400 DEC = 1,MAXDECS 
            IF(VALUE(DEC).NE.-99999.) THEN
               REALVALUE = VALUE(DEC) 
               IF (YEAR.LT.LOWYR(DEC,ELEM)) THEN
                  LOWYR(DEC,ELEM) = YEAR
               END IF
               NUMYEAR(DEC,ELEM) = NUMYEAR(DEC,ELEM)+1
               MEAN(DEC,ELEM) = MEAN(DEC,ELEM)+REALVALUE
               IF(REALVALUE.GT.EXTMAX(DEC,ELEM)) THEN
                  EXTMAX(DEC,ELEM)  = REALVALUE 
                  MAXYEAR(DEC,ELEM) = YEAR                       
                  MAXFLAG(DEC,ELEM) = ' '
               ELSE IF(REALVALUE.EQ.EXTMAX(DEC,ELEM)) THEN
                  MAXYEAR(DEC,ELEM) = YEAR
                  MAXFLAG(DEC,ELEM) = '*'
               END IF
               IF(REALVALUE.LT.EXTMIN(DEC,ELEM)) THEN
                  EXTMIN(DEC,ELEM)  = REALVALUE 
                  MINYEAR(DEC,ELEM) = YEAR                       
                  MINFLAG(DEC,ELEM) = ' '
               ELSE IF(REALVALUE.EQ.EXTMIN(DEC,ELEM)) THEN
                  MINYEAR(DEC,ELEM) = YEAR
                  MINFLAG(DEC,ELEM) = '*'
               END IF
            END IF
  400    CONTINUE
C
C   NORMAL END
C
  500 CONTINUE
  501 CONTINUE
      CLOSE(51)
      CALL BEEP
      CALL LOCATE(24,79,IERR)
      WRITE(*,*) ' '
      IF (DATAOK) THEN
         STOP ' '
      ELSE
         WRITE(MSGTXT,'(I3)') RECCOUNT
         CALL WRTMSG(5,66,12,1,1,MSGTXT,3)
         CALL LOCATE(24,79,IERR)
         STOP 2
      END IF
C
C   REACH HERE IF AN ERROR OR ABORT IS ENCOUNTERED
C
  900 CALL CLS   
      CALL WRTMSG(10,171,12,1,0,' ',0)
      CALL LOCATE(24,79,IERR)
      WRITE(*,*) ' '
      STOP 2 
      END
$PAGE
      SUBROUTINE WRTDEX(STNID,READELEM,ELEMCODE,DATAOK)
C----------------------------------------------------------------------
C   WRITE THE 10 DAY MEAN/EXTREME DATAEASE RECORDS FOR ALL DATA FOR A
C   STATION (ONE YEAR, ALL ELEMENTS)
C----------------------------------------------------------------------
      PARAMETER (MAXDECS=36, MAXELEM=40)

C---LOCAL VARIABLES
      CHARACTER*51  OUTREC
      CHARACTER*8   LOCALID,LONGMN
      CHARACTER*4   STRTYR,YRMAX,YRMIN
      CHARACTER*3   IELEM
      CHARACTER*2   IMONTH,IDAY,CYR
      CHARACTER*1   HLDREC(51) 
      INTEGER*2     HEADER(5),DEC,MON,NUMYR
      REAL*8        DBLMN
C---PASSED VARIABLES
      REAL*8      MEAN(MAXDECS,MAXELEM)
      INTEGER*2   MAXYEAR(MAXDECS,MAXELEM), MINYEAR(MAXDECS,MAXELEM),
     +            LOWYR(MAXDECS,MAXELEM),NUMYEAR(MAXDECS,MAXELEM),
     +            ELEMCODE(MAXELEM)
      CHARACTER*1 MAXFLAG(MAXDECS,MAXELEM), MINFLAG(MAXDECS,MAXELEM)
      CHARACTER*4 EXTMAX(MAXDECS,MAXELEM), EXTMIN(MAXDECS,MAXELEM)
C
      CHARACTER*8   STNID
      INTEGER*2      READELEM
      LOGICAL DATAOK

      COMMON /FLD/ MEAN,EXTMAX,EXTMIN,MAXYEAR,MINYEAR,LOWYR,NUMYEAR
     +            ,MAXFLAG,MINFLAG
C-----------------------------------------------------------------------
C    EQUIVALENCE THE INPUT VARIABLES TO THE OUTPUT RECORD STRING       
C-----------------------------------------------------------------------
      EQUIVALENCE   (OUTREC,HLDREC)
      EQUIVALENCE   (HEADER,HLDREC(1)), (LOCALID,HLDREC(5)),
     +              (IELEM,HLDREC(13)), (IMONTH,HLDREC(16)),
     +              (IDAY,HLDREC(18)),  (STRTYR,HLDREC(20)),
     +              (CYR,HLDREC(24)),   (YRMAX,HLDREC(31)),
     +              (YRMIN,HLDREC(40)),  (LONGMN,HLDREC(44))
      EQUIVALENCE   (CYR,NUMYR),        (LONGMN,DBLMN)
C 
   10 CONTINUE
C
C  WRITE THE INFORMATION INTO THE DATEASE RECORD HEADER
C
      HEADER(1) = 12
      HEADER(2) = 0
      HEADER(3) = 0
      HEADER(4) = 0
      HEADER(5) = 0
C
C  WRITE DATA VALUES INTO THE DATEASE RECORD 
C      
      LOCALID = STNID
      DO 240 I=1,36
         MON=(I-1)/3 + 1
         DEC=I-(MON-1)*3
         WRITE(IMONTH,'(I2.2)') MON
         WRITE(IDAY,'(I2.2)')   DEC
         DO 200 N = 1,READELEM
            IF (NUMYEAR(I,N).GT.1) THEN
               WRITE(STRTYR,'(I4)') LOWYR(I,N)
               NUMYR=NUMYEAR(I,N)
               WRITE(IELEM, '(I3.3)') ELEMCODE(N)
               OUTREC(26:29) = EXTMAX(I,N)
               OUTREC(30:30) = MAXFLAG(I,N)
               WRITE(YRMAX,'(I4)')     MAXYEAR(I,N)
               OUTREC(35:38) = EXTMIN(I,N)
               OUTREC(39:39) = MINFLAG(I,N)
               WRITE(YRMIN,'(I4)')     MINYEAR(I,N)
               DBLMN = MEAN(I,N)
               WRITE(51) OUTREC
               DATAOK = .TRUE.
            END IF
200      CONTINUE
240   CONTINUE
      RETURN
      END      
