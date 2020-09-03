$STORAGE:2

      PROGRAM DLYEXTR2
C
C   PROGRAM TO COMPUTE DAILY EXTREMES AND MEANS AS REQUESTED BY READING
C   THE DATAEASE FORM "DAILY DATA" THRU THE SUBROUTINE READDLY AND
C   WRITE A DAILY MEANS/EXTREME DATAEASE RECORD FOR EACH ELEMENT AND DAY.
C   OUTPUT RECORDS ARE WRITTEN IN STATION-MONTH-DAY-ELEMENT SORT.
C   THE PROGRAM ASKS THE USER FOR THE STATIONS AND DATE RANGE 
C   TO BE PROCESSED AND IF THE DATA SHOULD BE READ FROM THE MAIN DAILY 
C   DATA FILE OR WITH THE SUBSET OF HELD IN DATAEASE FORM H DLY DATA.
C
      PARAMETER (MAXMONTHS=12, MAXDAYS=31, MAXELEM=20)
C
C  PROGRAM CONTROL -- INPUT VARIABLES
C
      CHARACTER*64 FILNAME
      CHARACTER*16 MONNAME(13) 
      CHARACTER*8  STRTSTN,ENDSTN,PREVID,STNID,STRTDATE
      CHARACTER*1  DATASOURCE,RTNCODE,FLAG1(MAXDAYS) 
      CHARACTER*3  MSGTXT
      INTEGER*4    STRTYRMO,ENDYRMO,YRMON,IREC,RECCOUNT,NRECCOUNT
      INTEGER*2    DDSID,ELEM,MONTH,YEAR,D,
     +             M,READELEM,ELEMCODE(MAXELEM),INELEM
      LOGICAL DATAOK
C 
C  VARIABLES FOR COMPUTATION OF RESULTS
C
      REAL        MEAN(MAXDAYS,MAXMONTHS,MAXELEM),
     +            EXTMAX(MAXDAYS,MAXMONTHS,MAXELEM),
     +            EXTMIN(MAXDAYS,MAXMONTHS,MAXELEM), 
     +            VALUE(MAXDAYS)
      INTEGER*2   MAXYEAR(MAXDAYS,MAXMONTHS,MAXELEM),
     +            MINYEAR(MAXDAYS,MAXMONTHS,MAXELEM),
     +            LOWYR(MAXDAYS,MAXMONTHS,MAXELEM),
     +            NUMYEAR(MAXDAYS,MAXMONTHS,MAXELEM)
      CHARACTER*1 MAXFLAG(MAXDAYS,MAXMONTHS,MAXELEM),
     +            MINFLAG(MAXDAYS,MAXMONTHS,MAXELEM)
      CHARACTER*7 OUTCNT(2)
C
      COMMON /FLD/ MEAN,EXTMAX,EXTMIN,MAXYEAR,MINYEAR,LOWYR,NUMYEAR
     +            ,MAXFLAG,MINFLAG
      DATA OUTCNT/'       ','       '/, DATAOK /.FALSE./
      OUTCNT(2)(1:1) = CHAR(0)
C----------------------------------------------------------------------
C     (OPEN THE OUTPUT FILE NO MATTER WHAT.  THAT WAY IT WILL NOT HAVE
C      OLD DATA IN IT IF THE PROGRAM IS NOT RUN.) 
C----------------------------------------------------------------------
      OPEN(51,FILE='Q:CLIMDATA.DAT',FORM='BINARY',STATUS='UNKNOWN')
      ENDFILE 51
      REWIND 51
C
      CALL GETMON(MONNAME,16)
      DO 15 K=1,MAXELEM
         ELEMCODE(K)=0
   15 CONTINUE
C------------------------------------------------------------------------
C   READ FILE OF DAILY ELEMENT CODES FOR WHICH USER WANTS MEANS/EXTREMES
C------------------------------------------------------------------------
   20 CONTINUE
      OPEN (61,FILE='P:\DATA\DLYEXTR2.PRM',STATUS='OLD',FORM='FORMATTED'
     +    ,IOSTAT=IOCHK)
      IF (IOCHK.NE.0) THEN
         CALL OPENMSG('P:\DATA\DLYEXTR2.PRM   ','DLYEXTR     ',IOCHK)
         GO TO 20
      END IF 
C      
C--------------THROW AWAY 1ST LINE OF FILE, EXPLANATION ONLY-----------
C              ALSO MAKE SURE LESS THAN MAXELEM ELEMENTS SPECIFIED
C 
      READ(61,25,END=900)
      DO 30 I=1,999
         READ(61,25,END=35) INELEM
   25    FORMAT(I3)
         IF (I.GT.MAXELEM) THEN
            WRITE(OUTCNT(1),'(I7)') MAXELEM
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
      FILNAME = 'P:\HELP\DLYEXTR2.HLP '
      CALL SETMOD(3,IERR)
   60 CONTINUE
      CALL CLS
      CALL LOCATE(1,1,IERR)
      CALL GETLIMIT(STRTSTN,ENDSTN,STRTYRMO,ENDYRMO,FILNAME,RTNCODE)
      IF (RTNCODE.EQ.'1') THEN
         CLOSE(51)
         CALL LOCATE(23,0,IERR)
         STOP 2
      END IF      
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
C   FTN 3.3 YOU MUST REPLACE CALL TO OPENPOS WITH CALL TO OPENINPUT
C
      WRITE(STRTDATE,'(I6)') STRTYRMO
      CALL OPENPOS('DLY',DATASOURCE,STRTSTN,STRTDATE)

C      CALL OPENINPUT('DLY',DATASOURCE)

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
C        READ A DAILY RECORD AND PROCESS IF WITHIN SELECTED LIMITS    
C-----------------------------------------------------------------------
      DO 500 IREC=1,999999
         CALL READDLY(DDSID,STNID,IELEM,YEAR,MONTH,VALUE,FLAG1,RTNCODE)
         RYEAR = YEAR
         RMON = MONTH
         RYRMON = RYEAR*100. + RMON
         YRMON = INT4(RYRMON)
         NRECCOUNT = NRECCOUNT + 1
         CALL LOCATE(24,15,IERR)
         WRITE(OUTCNT(1),'(I7)') NRECCOUNT
         CALL CWRITE(OUTCNT,12,IERR)
         IF (STNID.GT.ENDSTN.OR.(STNID.EQ.ENDSTN.AND.YRMON.GT.ENDYRMO))
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
           DO 150 N   = 1,READELEM
           DO 150 M   = 1,MAXMONTHS
           DO 150 D = 1,MAXDAYS
              IF(NUMYEAR(D,M,N).GT.1) THEN
                 XYR=NUMYEAR(D,M,N)
                 MEAN(D,M,N)=MEAN(D,M,N)/XYR
              END IF   
  150      CONTINUE
C------------------------------------------------------------------------
C       WRITE OUT ALL OF THE MEANS/EXTREMES GENERATED FOR THIS STATION
C------------------------------------------------------------------------
           CALL WRTDEX(PREVID,READELEM,ELEMCODE,DATAOK)

           IF (RTNCODE.NE.'0') THEN
              GO TO 501
           END IF   
C------------------------------------------------------------------------
C       INITIALIZE OR RESET
C----- -------------------------------------------------------------------
270         CONTINUE
            IF (RTNCODE.EQ.'1'.AND.PREVID.EQ.'XXXXXXXX') THEN
               CALL WRTMSG(5,548,12,1,1,' ',0)
               CALL LOCATE(22,0,IERR)
               STOP 2
            END IF
            IF (STNID.LT.STRTSTN.OR.YRMON.LT.STRTYRMO.OR.
     +            YRMON.GT.ENDYRMO) THEN
               GO TO 500
            END IF
            PREVID=STNID

            DO 280 K=1,READELEM
              DO 280 J=1,MAXMONTHS
                 DO 280 I=1,MAXDAYS
                    MEAN(I,J,K)    = 0.0
                    EXTMAX(I,J,K)  = -99999.
                    EXTMIN(I,J,K)  = 99999.
                    LOWYR(I,J,K)   = 9999
                    MAXYEAR(I,J,K) = -9999
                    MINYEAR(I,J,K) = -9999
                    MAXFLAG(I,J,K) = 'M'
                    MINFLAG(I,J,K) = 'M'
                    NUMYEAR(I,J,K) = 0
  280       CONTINUE 
         END IF
         IF (STNID.LT.STRTSTN.OR.YRMON.LT.STRTYRMO.OR.
     +          YRMON.GT.ENDYRMO) THEN
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
         DO 400 DAY = 1,MAXDAYS 
            IF(VALUE(DAY).NE.-99999.) THEN
               REALVALUE = VALUE(DAY) 
               IF (YEAR.LT.LOWYR(DAY,MONTH,ELEM)) THEN
                  LOWYR(DAY,MONTH,ELEM) = YEAR
               END IF
               NUMYEAR(DAY,MONTH,ELEM) = NUMYEAR(DAY,MONTH,ELEM)+1
               MEAN(DAY,MONTH,ELEM) = MEAN(DAY,MONTH,ELEM)+REALVALUE
               IF(REALVALUE.GT.EXTMAX(DAY,MONTH,ELEM)) THEN
                  EXTMAX(DAY,MONTH,ELEM)  = REALVALUE 
                  MAXYEAR(DAY,MONTH,ELEM) = YEAR                       
                  MAXFLAG(DAY,MONTH,ELEM) = ' '
               ELSE IF(REALVALUE.EQ.EXTMAX(DAY,MONTH,ELEM)) THEN
                  MAXYEAR(DAY,MONTH,ELEM) = YEAR
                  MAXFLAG(DAY,MONTH,ELEM) = '*'
               END IF
               IF(REALVALUE.LT.EXTMIN(DAY,MONTH,ELEM)) THEN
                  EXTMIN(DAY,MONTH,ELEM)  = REALVALUE 
                  MINYEAR(DAY,MONTH,ELEM) = YEAR                       
                  MINFLAG(DAY,MONTH,ELEM) = ' '
               ELSE IF(REALVALUE.EQ.EXTMIN(DAY,MONTH,ELEM)) THEN
                  MINYEAR(DAY,MONTH,ELEM) = YEAR
                  MINFLAG(DAY,MONTH,ELEM) = '*'
               END IF
            END IF
  400    CONTINUE
C
C   NORMAL END OF PROGRAM
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
C   REACH THIS POINT IF AN ERROR OR ABORT IS ENCOUNTERED
C
  900 CALL CLS
      CLOSE(51)
      CALL WRTMSG(10,170,12,1,0,' ',0)
      CALL LOCATE(24,79,IERR)
      WRITE(*,*) ' '
      STOP 2
      END
***********************************************************************
$PAGE
      SUBROUTINE WRTDEX(STNID,READELEM,ELEMCODE,DATAOK)
C----------------------------------------------------------------------
C   WRITE THE DAILY MEAN/EXTREME DATAEASE RECORDS FOR ALL DATA FOR A
C   STATION (ONE YEAR, ALL ELEMENTS)
C----------------------------------------------------------------------
      PARAMETER (MAXMONTHS=12, MAXDAYS=31, MAXELEM=20)
C
C---LOCAL VARIABLES
      CHARACTER*51   OUTREC 
      CHARACTER*1    HLDREC(51)
      CHARACTER*8   LOCALID,LONGMN
      CHARACTER*4   STRTYR,YRMAX,YRMIN
      CHARACTER*3   IELEM
      CHARACTER*2   IMONTH,IDAY,CYR
      INTEGER*2     HEADER(5),NUMYR,NUMDAYS(MAXMONTHS)
      REAL*8        DBLMN
C
C---PASSED VARIABLES
      REAL        MEAN(MAXDAYS,MAXMONTHS,MAXELEM)
      INTEGER*2   MAXYEAR(MAXDAYS,MAXMONTHS,MAXELEM),
     +            MINYEAR(MAXDAYS,MAXMONTHS,MAXELEM),
     +            LOWYR(MAXDAYS,MAXMONTHS,MAXELEM),
     +            NUMYEAR(MAXDAYS,MAXMONTHS,MAXELEM),
     +            ELEMCODE(MAXELEM)
      CHARACTER*1 MAXFLAG(MAXDAYS,MAXMONTHS,MAXELEM),
     +            MINFLAG(MAXDAYS,MAXMONTHS,MAXELEM)
      CHARACTER*4 EXTMAX(MAXDAYS,MAXMONTHS,MAXELEM),
     +            EXTMIN(MAXDAYS,MAXMONTHS,MAXELEM) 
C
      CHARACTER*8   STNID
      INTEGER*2     M,D,READELEM
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

      DATA NUMDAYS/31,29,31,30,31,30,31,31,30,31,30,31/
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
C  WRITE THE DATA VALUES INTO THE DATEASE RECORD 
C      
      LOCALID = STNID
      DO 250  M=1,MAXMONTHS
         LDAY=NUMDAYS(M)
         WRITE(IMONTH,'(I2.2)') M
         DO 240 D=1,LDAY
            WRITE(IDAY,'(I2.2)')   D
            DO 200 N=1,READELEM
               IF (NUMYEAR(D,M,N).GT.1) THEN
                  WRITE(STRTYR,'(I4)') LOWYR(D,M,N)
                  NUMYR=NUMYEAR(D,M,N)
                  WRITE(IELEM, '(I3.3)') ELEMCODE(N)
                  OUTREC(26:29) = EXTMAX(D,M,N)
                  OUTREC(30:30) = MAXFLAG(D,M,N)
                  WRITE(YRMAX,'(I4)')     MAXYEAR(D,M,N)
                  OUTREC(35:38) = EXTMIN(D,M,N)
                  OUTREC(39:39) = MINFLAG(D,M,N)
                  WRITE(YRMIN,'(I4)')     MINYEAR(D,M,N)
                  DBLMN=MEAN(D,M,N)
                  WRITE(51) OUTREC
                  DATAOK = .TRUE.
               END IF
200         CONTINUE
240      CONTINUE
250   CONTINUE
      RETURN
      END
