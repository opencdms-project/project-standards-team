$STORAGE:2            

      PROGRAM UNLDATA

************************************************************************
*     UNLDATA TRANSFERS DATA FROM THE DATAEASE '.DBM' FILES TO THE     *
*     TEMPORARY WORK FILES '.TWF', MAKING THE DATA AGAIN AVAILABLE TO  *
*     THE QUALITY CONTROL PROGRAMS.                                    *
************************************************************************

$INCLUDE:'VAL1.INC'

$INCLUDE:'INDEX.INC'

      CHARACTER*1 FLAG1(MAXLINE),RTNCODE,REPLY,RECID(20)
      CHARACTER*1 DATASOURCE
      CHARACTER*2 AMONTH,ADAY,AHOUR,RTNFLAG,HOURLBL(24)
      CHARACTER*3 ELEMCODE,RECTYPE,TBLRECTYP(7),ADSID
      CHARACTER*4 VALUE(MAXLINE),AYEAR
      CHARACTER*7 OUTCNT(2)
      CHARACTER*8 STNID,AFIELD(8),LOSTN,HISTN,STRTDATE
      CHARACTER*10 DATETIME
      CHARACTER*21 IDKEY,LASTID
      CHARACTER*64 HELPFILE

      INTEGER*2 YEAR,MONTH,DAY,HOUR,TBLNUMLIN(7),
     +          DDSID,ELEM,
     +          SRCHDSID,LOYR,HIYR,LOMO,HIMO,DSETID
      INTEGER*4 I4VALUE,RECCOUNT,NRECCOUNT

      REAL*4 RVALUE(MAXLINE)

      EQUIVALENCE (STNNUM,RECID(1)),(ELEMCODE,RECID(8))
      EQUIVALENCE (DATETIME,RECID(11))
      EQUIVALENCE (VALUE,RVALUE)

      DATA RECCOUNT/0/,NRECCOUNT/0/,OUTCNT/' ',' '/,LASTID/' '/
      DATA AMONTH/' '/
      DATA TBLRECTYP  /'MLY','10D','DLY','SYN','HLY','15M','U-A'/
     +    ,TBLNUMLIN /12,36,31,8,24,96,100/
      DATA HELPFILE/'P:\HELP\UNLDATA.HLP'/
     
      OUTCNT(2) = CHAR(0)

************************************************************************
*     RETRIEVE THE DATA TYPE                                           *
************************************************************************

      CALL SETMOD(3,IERR)
   10 CONTINUE
      CALL LOCATE (5,3,IERR)
      CALL GETMNU('DR-DATATYPES','  ',ITYPE)
      IF (ITYPE.EQ.0)THEN
         CALL LOCATE(23,0,IERR)
         STOP ' '
      END IF
      RECTYPE = TBLRECTYP(ITYPE)
      NUMLINE = TBLNUMLIN(ITYPE)
      CALL DATASRC(38,DATASOURCE,RTNCODE)
      IF (RTNCODE.NE.'0') THEN
         GO TO 10
      END IF
      CALL POSLIN(IROW,ICOL)

************************************************************************
*     RETRIEVE THE FIELDS FROM THE USER                                *
************************************************************************

      CALL CLS
50    CONTINUE
      CALL LOCATE (5,10,IERR)
      RTNFLAG = 'SS'
      CALL GETFRM('UNLDATA     ',HELPFILE,AFIELD,8,RTNFLAG)
      IF (RTNFLAG.EQ.'4F')THEN
         CALL LOCATE(24,0,IERR)
         STOP ' '
      END IF

      READ(AFIELD(1),'(A8)') LOSTN
      READ(AFIELD(2),'(A8)') HISTN
      READ(AFIELD(3),'(I4)') LOYR    
      READ(AFIELD(4),'(I2)') LOMO   
      READ(AFIELD(5),'(I4)') HIYR    
      READ(AFIELD(6),'(I2)') HIMO   
      READ(AFIELD(7),'(I3)') SRCHDSID 
      READ(AFIELD(8),'(I3)') DSETID 


************************************************************************
*     OPEN THE SETUP FILE AND LOAD THE ELEMENT CODES AND COLUMN        *
*     HEADERS.                                                         *
************************************************************************

      CALL GETSET(DSETID,ITYPE,RECTYPE,HOURLBL,RTNCODE)
      IF (RTNCODE.NE.'0') THEN
         CALL LOCATE(23,0,IERR)
         STOP 2
      END IF
      LASTID = ' '
C
C   OPEN THE INPUT FILE - OPENPOS REQUIRES FTN 4.0 OR LATER.  IF USING
C   FTN 3.3 YOU MUST REPLACE CALL TO OPENPOS WITH CALL TO OPENINPUT
C
         WRITE(STRTDATE,'(I4,I2.2)') LOYR,LOMO
         CALL OPENPOS(RECTYPE,DATASOURCE,LOSTN,STRTDATE)
C         CALL OPENINPUT(RECTYPE,DATASOURCE)

         CALL OPENFILES(2)

************************************************************************
*     WRITE THE RUNNING TOTAL LINE                                     *
************************************************************************

      CALL CLRMSG(1)
      CALL LOCATE(24,0,IERR)
      CALL WRTSTR('Records read -          Records processed - '
     +             ,44,14,0)

************************************************************************
*     INITIALIZE THE WORK ARRAYS.                                      *
************************************************************************

      DO 900 J=1,NUMLINE
         DO 900 K=1,NUMELEM
         VALARRAY(K,J) = ' '
         FLAGARRAY(K,J,1) = ' '
         FLAGARRAY(K,J,2) = ' '
900   CONTINUE
      FLAGARRAY(1,1,1) = '?'

************************************************************************
*     NOW READ THE DATA, FORMAT IDKEY AND SELECT PROPER RECORDS.       *
************************************************************************

1000  CONTINUE

      IF (RECTYPE .EQ. 'MLY') THEN
         CALL READMLY (DDSID,STNID,ELEM,YEAR,VALUE,FLAG1,RTNCODE)     
      ELSE IF(RECTYPE .EQ. '10D') THEN  
         CALL READ10D (DDSID,STNID,ELEM,YEAR,VALUE,FLAG1,RTNCODE)
      ELSE IF(RECTYPE .EQ. 'DLY') THEN  
         CALL READDLY (DDSID,STNID,ELEM,YEAR,MONTH,VALUE,FLAG1,RTNCODE)
      ELSE IF (RECTYPE .EQ. 'SYN') THEN
         CALL READSYN (DDSID,STNID,ELEM,YEAR,MONTH,DAY,VALUE,FLAG1
     +                ,RTNCODE)     
      ELSE IF (RECTYPE .EQ. 'HLY') THEN
         CALL READHLY (DDSID,STNID,ELEM,YEAR,MONTH,DAY,VALUE,FLAG1
     +                ,RTNCODE)     
      ELSE IF (RECTYPE .EQ. '15M') THEN
         CALL READ15M (DDSID,STNID,ELEM,YEAR,MONTH,DAY,VALUE,FLAG1
     +                ,RTNCODE)     
      ELSE IF (RECTYPE .EQ. 'U-A') THEN
         CALL READUA (DDSID,STNID,YEAR,MONTH,DAY,HOUR,NUMLVL,LVLNUM,
     +                PRESSURE,HEIGHT,TEMP,DEWPTDEP,WNDDIR,WNDSPEED,
     +                FLAG1,RTNCODE)
      ELSE
         CALL WRTMSG (4,82,12,1,0,' ',0)
         CALL LOCATE(23,0,IERR)
         STOP ' '
      END IF
      IF (RTNCODE .GT. '0') THEN
         GO TO 3000
      END IF

      NRECCOUNT = NRECCOUNT + 1
      CALL LOCATE(24,15,IERR)
      WRITE(OUTCNT,'(I7)') NRECCOUNT
      CALL CWRITE(OUTCNT,12,IERR)

      IF (STNID .LT. LOSTN) THEN
         GO TO 1000
      ELSE IF (STNID .GT. HISTN) THEN
         GO TO 3000
      ELSE IF (DDSID .NE. SRCHDSID) THEN
         GO TO 1000
      ELSE IF (YEAR .LT. LOYR) THEN   
         GO TO 1000
      ELSE IF (YEAR .GT. HIYR.AND.STNID.EQ.HISTN) THEN   
         GO TO 3000
      END IF

      IF (RECTYPE .NE. 'MLY' .AND. RECTYPE. NE. '10D') THEN
         IF (YEAR .EQ. LOYR .AND. MONTH .LT. LOMO) THEN   
            GO TO 1000
         ELSE IF (YEAR .EQ. HIYR .AND. MONTH .GT. HIMO) THEN   
            GO TO 3000
         END IF
      END IF

      WRITE(ADSID,'(I3.3)') DSETID
      WRITE(AYEAR,'(I4.4)') YEAR
      IF (RECTYPE .EQ. 'MLY' .OR. RECTYPE .EQ.'10D') THEN
         AMONTH = ' '
         ADAY = ' '
         AHOUR = ' '
      ELSE IF (RECTYPE .EQ. 'DLY') THEN
         WRITE(AMONTH,'(I2.2)') MONTH
         ADAY = ' '
         AHOUR = ' '
      ELSE IF (RECTYPE.EQ.'HLY'.OR.RECTYPE.EQ.'SYN'.OR.
     +          RECTYPE.EQ.'15M') THEN
         WRITE(AMONTH,'(I2.2)') MONTH
         WRITE(ADAY,'(I2.2)') DAY
         AHOUR = ' '
      ELSE
         WRITE(AMONTH,'(I2.2)') MONTH
         WRITE(ADAY,'(I2.2)') DAY
         WRITE(AHOUR,'(I2.2)') HOUR
      END IF

      WRITE(IDKEY,'(A8,A3,A4,3A2,)') STNID,ADSID,AYEAR,
     +                               AMONTH,ADAY,AHOUR

************************************************************************
*     LOAD VALARRAY AND FLAGARRAY INTO .TWF AND .IDX.                  *
************************************************************************

      IF (IDKEY .NE. LASTID .AND. LASTID .NE. ' ') THEN
         CALL BINDATA(LASTID,RTNCODE)
C
C          ** IF RTNCODE = 2 THEN ID NOT FOUND SO IT'S NEW (OK).
C             IF RTNCODE = 0 THEN THE PROGRAM HAS ENCOUNTERED A DUPLICATE ID
C             ASK THE USER TO CONTINUE OR QUIT.  ANY OTHER RTNCODE INDICATES
C             AN ERROR.
C
         IF (RTNCODE .NE.'2') THEN
            IF (RTNCODE .EQ. '0') THEN
               CALL WRTMSG(5,305,12,1,0,' ',0)
               CALL LOCATE(22,5,IERR)
               CALL OKREPLY(REPLY,RTNCODE)
               IF (REPLY .EQ. 'N') THEN
                  CALL WRTMSG(3,365,14,1,1,' ',0)
                  CALL LOCATE(23,0,IERR)
                  STOP ' '
               ELSE
                  CALL CLRMSG(5)
                  CALL CLRMSG(3)
               END IF
            ELSE
               CALL WRTMSG(5,51,12,1,1,RTNCODE,1)
               CALL LOCATE(23,0,IERR)
               STOP ' '
            END IF
         END IF
         CALL PUTDATA(LASTID,2,RTNCODE)
C
C          ** RE-INITIALIZE THE WORK ARRAYS TO HOLD THE NEW DATA.  NOTE THAT
C             NUMLINE AND NUMELEM ARE LISTED IN COMMON VAL1 (VAL1.INC) 
C
         DO 1100 J=1,NUMLINE
            DO 1100 K=1,NUMELEM
               VALARRAY(K,J) = ' '
1100     CONTINUE

         LINENUM = 0

      END IF

      LASTID = IDKEY

************************************************************************
*     IDENTIFY THE PROPER COLUMN OF THE FORM FOR EACH ELEMENT.         *
************************************************************************

      IF (RECTYPE .NE. 'U-A') THEN
         DO 1200 I = 1,MAXELEM
            K=I

            IF (ELEM .EQ. TBLELEM(I)) THEN
               GO TO 1300
            END IF
1200     CONTINUE

         GO TO 1000
      END IF
         
************************************************************************
*     LOAD DATA VALUES INTO VALARRAY.                                  *
************************************************************************

1300  IF (RECTYPE .NE. 'U-A') THEN 
         DO 1400 J = 1,NUMLINE
            IF (RVALUE(J) .EQ. -99999.) THEN
               VALARRAY(K,J) = ' '
            ELSE
C               I4VALUE = NINT(RVALUE(J) / TBLCONV(K))
               CALL IROUND4((RVALUE(J)/TBLCONV(K)),I4VALUE)
               WRITE(VALARRAY(K,J),'(I5,A1)') I4VALUE,FLAG1(J)
            END IF
1400     CONTINUE


      ELSE
         LINENUM = LINENUM + 1
         IF (PRESSURE .EQ. -99999.) THEN
            VALARRAY (1,LINENUM) = ' '
         ELSE
C            I4VALUE = NINT (PRESSURE / TBLCONV(1))
            CALL IROUND4((PRESSURE/TBLCONV(1)),I4VALUE)
            WRITE(VALARRAY(1,LINENUM),'(I5,A1)') I4VALUE,FLAG1(1)
         END IF
         IF (HEIGHT .EQ. -99999.) THEN
            VALARRAY (2,LINENUM) = ' '
         ELSE

C            I4VALUE = NINT (HEIGHT / TBLCONV(2))
            CALL IROUND4((HEIGHT/TBLCONV(2)),I4VALUE)
            WRITE(VALARRAY(2,LINENUM),'(I5,A1)') I4VALUE,FLAG1(2)
         END IF
         IF (TEMP .EQ. -99999.) THEN
            VALARRAY (3,LINENUM) = ' '
         ELSE
C            I4VALUE = NINT (TEMP / TBLCONV(3))
            CALL IROUND4((TEMP/TBLCONV(3)),I4VALUE)
            WRITE(VALARRAY(3,LINENUM),'(I5,A1)') I4VALUE,FLAG1(3)
         END IF
         IF (DEWPTDEP .EQ. -99999.) THEN
            VALARRAY (4,LINENUM) = ' '
         ELSE
C            I4VALUE = NINT (DEWPTDEP / TBLCONV(4))
            CALL IROUND4((DEWPTDEP/TBLCONV(4)),I4VALUE)
            WRITE(VALARRAY(4,LINENUM),'(I5,A1)') I4VALUE,FLAG1(4)
         END IF
         IF (WNDDIR .EQ. -999.) THEN
            VALARRAY (5,LINENUM) = ' '
         ELSE
C            I4VALUE = NINT (WNDDIR / TBLCONV(5))
            CALL IROUND4((WNDDIR/TBLCONV(5)),I4VALUE)
            WRITE(VALARRAY(5,LINENUM),'(I5,A1)') I4VALUE,FLAG1(5)
         END IF
         IF (WNDSPEED .EQ. -99999.) THEN
            VALARRAY (6,LINENUM) = ' '
         ELSE
C            I4VALUE = NINT (WNDSPEED / TBLCONV(6))
            CALL IROUND4((WNDSPEED/TBLCONV(6)),I4VALUE)
            WRITE(VALARRAY(6,LINENUM),'(I5,A1)') I4VALUE,FLAG1(6)
         END IF

      END IF

      RECCOUNT = RECCOUNT + 1
      CALL LOCATE(24,44,IERR)
      WRITE(OUTCNT,'(I7)') RECCOUNT
      CALL CWRITE(OUTCNT,12,IERR)

      GO TO 1000

3000  CONTINUE

************************************************************************
*     LOAD LAST SELECTED RECORD PRIOR TO STOPPING.                     *
************************************************************************

      IF (RECCOUNT.GT.0) THEN
         CALL BINDATA(LASTID,RTNCODE)
         IF (RTNCODE .NE.'2') THEN
            IF (RTNCODE .EQ. '0') THEN
               CALL WRTMSG(5,305,12,1,0,' ',0)
               CALL LOCATE(22,5,IERR)
               CALL OKREPLY(REPLY,RTNCODE)
               IF (REPLY .EQ. 'N') THEN
                  CALL WRTMSG(3,365,14,1,1,' ',0)
                  CALL LOCATE(23,0,IERR)
                  STOP ' '
               ELSE
                  CALL CLRMSG(5)
                  CALL CLRMSG(3)
                  NMSG = 365
               END IF
            ELSE
               CALL WRTMSG(5,51,12,1,1,RTNCODE,1)
               CALL WRTMSG(3,365,14,1,1,' ',0)
               CALL LOCATE(23,0,IERR)
               STOP ' '
            END IF
         END IF
         CALL PUTDATA(LASTID,2,RTNCODE)
         NMSG = 365
      ELSE   
         NMSG = 306
      ENDIF   
      CALL CLRMSG(1)
      CALL LOCATE(16,0,IERR)
      WRITE(*,3010) RECCOUNT,NRECCOUNT - RECCOUNT
      CALL WRTMSG(3,NMSG,14,1,1,' ',0)

************************************************************************
*     PROCESSING IS COMPLETE.                                          *
************************************************************************

      CLOSE(19)
      CLOSE(20)

3010  FORMAT(///,' Processing Complete',/
     +      ,2X,I7,' Records Processed',/
     +      ,2X,I7,' Records Skipped') 
      STOP ' '
      END


