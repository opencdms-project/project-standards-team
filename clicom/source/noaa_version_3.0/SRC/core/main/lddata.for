$storage:2
C
C   PROGRAM LDDATA
C
C   ROUTINE TO READ THE TEMPORARY WORK FILE(TWF) , REFORMAT IT, AND
C     WRITE IT TO DISK FOR LOADING INTO DATAEASE.
C
C     NOTE:  THE DATA-TYPE CODE (MLY,DLY...) MUST BE INCLUDED ON THE
C            COMMAND LINE CALLING THIS ROUTINE
C            EXAMPLE: "LDDATA DLY"
C
      INTERFACE TO SUBROUTINE CMDLIN(ADDRES,LENGTH,RESULT)
      INTEGER*4 ADDRES[VALUE],LENGTH[VALUE]
      CHARACTER*1 RESULT
      END
      INTERFACE TO INTEGER*2 FUNCTION SYSTEM [C]
     +        (STRING[REFERENCE])
      CHARACTER*1 STRING
      END

C----------------------------------------------------------------------
      PROGRAM LDDATA
C
$INCLUDE: 'VAL1.INC'
$INCLUDE: 'INDEX.INC'

      CHARACTER*15 DATAFILE
      CHARACTER*1 RTNCODE
      CHARACTER*3 RECTYPE
      CHARACTER*8 FIELD(7)
      CHARACTER*21 IDKEY
      CHARACTER*64 RESULT
      INTEGER*2 SORTCOL(MAXELEM)
      INTEGER*4 PSP,PSPNCHR,OFFSET
      DATA FIELD /7*'        '/
C
C   FOLLOWING STATEMENT REQUIRED FOR FORTRAN 3.3 - NOT ALLOWED IN FTN 4
c      EXTERNAL LDDATA
C
C   LOCATE SEGMENTED ADDRESS OF THE BEGINNING OF THIS PROGRAM
C
      OFFSET = #00100000
      PSP = LOCFAR(LDDATA)
C
C   COMPUTE THE BEGINNING OF THE PROGRAM SEGMENT PREFIX (PSP)
C
      PSP = (PSP - MOD(PSP,#10000)) - OFFSET 
C
C   LOCATE POSITION OF COMMAND PARAMTERS WITHIN THE PSP
C
      PSPNCHR = PSP + #80
      PSP = PSP + #81
C
C   PASS THE ADDRESS OF THE COMMAND PARAMTERS TO CMDLIN WHICH DECODES
C      THE COMMAND AND RETURNS IT AS RESULT.
C
      CALL CMDLIN(PSP,PSPNCHR,RESULT)
C
C   PULL THE COMMAND (REC-TYPE) OUT OF THE RESULT
C
      RECTYPE = RESULT(1:3)
C
      CALL SETMOD(3,IERR)
C
C   READ THE CONTROL DATA TYPE FROM THE COMMAND LINE
C
      IF (RECTYPE.EQ.'MLY') THEN
         ITYPE = 1
         NUMLINE = 12
      ELSE IF (RECTYPE.EQ.'10D') THEN
         ITYPE = 2
         NUMLINE = 36   
      ELSE IF (RECTYPE.EQ.'DLY') THEN
         ITYPE = 3
         NUMLINE = 31   
      ELSE IF (RECTYPE.EQ.'SYN') THEN
         ITYPE = 4
         NUMLINE = 8   
      ELSE IF (RECTYPE.EQ.'HLY') THEN
         ITYPE = 5
         NUMLINE = 24   
      ELSE IF (RECTYPE.EQ.'15M') THEN
         ITYPE = 6
         NUMLINE = 96   
      ELSE IF (RECTYPE.EQ.'U-A') THEN
         ITYPE = 7
         NUMLINE = 100
      ELSE
         CALL WRTMSG(3,299,12,1,0,'LDDATA',6)
         CALL WRTMSG(2,298,12,0,1,'LDDATA DLY)',11)
         STOP 2
       END IF   
C
C   OPEN THE OUTPUT DATA FILES
C
      DATAFILE = 'Q:CLIMDATA.DAT'
   20 OPEN(51,FILE=DATAFILE,STATUS='UNKNOWN',FORM='BINARY',
     +        MODE='READWRITE',SHARE='DENYRW',IOSTAT=IOCHK)
      IF (IOCHK.NE.0) THEN
         CALL OPENMSG(DATAFILE,'LDDATA      ',IOCHK)
         GO TO 20
      END IF
      DATAFILE = 'Q:\EXTREMES.DAT'
   25 OPEN(52,FILE=DATAFILE,STATUS='UNKNOWN',FORM='FORMATTED',
     +        MODE='READWRITE',SHARE='DENYRW',IOSTAT=IOCHK)
      IF (IOCHK.NE.0) THEN
          CALL OPENMSG(DATAFILE,'LDDATA      ',IOCHK)
          GO TO 25
      END IF
      ENDFILE 51
      REWIND 51
      ENDFILE 52
      REWIND 52
C
C   CALL LDBANR TO GET THE NEEDED TIME PARAMETERS, READ THE SETUP FILE
C
      IDKEY = '     '
      CALL LDBANR(ITYPE,RECTYPE,FIELD,RTNCODE)
      IF (RTNCODE.EQ.'1') THEN
          STOP 2 
      END IF
      CALL CLRMSG(1)
      CALL LOCATE(2,0,IERR)
C
C   SORT THE ELEMENT CODES IN THE KEY-ENTRY FORM INTO ASCENDING ORDER.
C   STORE THE APPROPRIATE COLUMN NUMBERS INTO SORTCOL
C
      DO 30 I = 1,NUMELEM
         SORTCOL(I) = I
30    CONTINUE      
      DO 45 I = 2,NUMELEM
         IF (TBLELEM(SORTCOL(I)).LT.TBLELEM(SORTCOL(I-1))) THEN
            DO 40 J = I,2,-1
               IF (TBLELEM(SORTCOL(J)).LT.TBLELEM(SORTCOL(J-1)))
     +                THEN
                  INCOL = SORTCOL(J)
                  SORTCOL(J) = SORTCOL(J-1) 
                  SORTCOL(J-1) = INCOL
               ELSE
                  GO TO 45
               END IF
40          CONTINUE
         END IF
45    CONTINUE
C
C   OPEN THE INDEX AND DATA FILES
C
      CALL OPENFILES(2)
C
C   REFORMAT AND WRITE THE DATA
C
      CALL DMPDATA(ITYPE,RECTYPE,FIELD,SORTCOL)
      STOP ' '
      END
C
***********************************************************************
$PAGE
      SUBROUTINE DMPDATA(ITYPE,RECTYPE,FIELD,SORTCOL)
C
C   ROUTINE TO :
C       1. DO A SEQUENTIAL READ OF THE INDEX FILE AND USE THE
C          POINTERS TO READ THE DATA FILE
C       2. WRITE THE OUTPUT DATA IN DATAEASE FORMAT 
C
C    RETURNS ERRORLEVEL 2 IF NO DATA OR AN ERROR IS ENCOUNTERED
C
$INCLUDE: 'VAL1.INC'
$INCLUDE: 'INDEX.INC'
      CHARACTER*1 RTNCODE,NULL
      CHARACTER*2 INCHAR
      CHARACTER*3 DDSID,INDDS,RECTYPE
      CHARACTER*8 FIELD(7),INSTN,STRTSTN,ENDSTN
      CHARACTER*6 FILENAME,BYRMON,EYRMON,INYRMON
      CHARACTER*10 DATE
      CHARACTER*13 INKEY
      CHARACTER*22 BKFILE,NEWFIL
      CHARACTER*25 IDXREC
      CHARACTER*40 MSGTXT
      CHARACTER*72 MESSAGE
      LOGICAL SOMEDATA(MAXELEM),WRTLIM,MSGPAUSE
C
      INTEGER*2 NEWLIM,SORTCOL(MAXELEM)
C
      REAL        ARRAY(MAXELEM,MAXLINE)
      CHARACTER*1 HFLAG,FLAG(MAXELEM,MAXLINE)
C
      NULL     = CHAR(0)
      NEWLIM   = 0
      ICOUNT   = 0
      MSGPAUSE = .FALSE.
      WRTLIM   = .FALSE.
C
      READ(19,REC=1) DELKEY,BGNIDX,NUMIDX
      IF (NUMIDX.EQ.1) THEN
         CALL WRTMSG(10,300,12,1,1,' ',0)
         STOP 2
      END IF
C
C  SELECT THE APPROPRIATE CHARACTERS FROM IDKEY AND SET THE SEARCH DATE
C 
      DDSID = FIELD(1)
      IF (RECTYPE.EQ.'MLY'.OR.RECTYPE.EQ.'10D') THEN
         BYRMON = FIELD(2)
         BYRMON(5:6) = '  '
         EYRMON = FIELD(3)
         EYRMON(5:6) = '99'
         STRTSTN = FIELD(4)
         ENDSTN = FIELD(5)
      ELSE
         BYRMON = FIELD(2)
         BYRMON(5:6) = FIELD(3)
         EYRMON = FIELD(4)
         EYRMON(5:6) = FIELD(5)
         STRTSTN = FIELD(6)
         ENDSTN = FIELD(7)
      END IF
C
C   COPY THE INDEX FILE TO THE BACKUP INDEX FILE
C
      CALL LOCATE(2,0,IERR)
      WRITE(MSGTXT,'(2A3)') RECTYPE,DDSID
      CALL WRTMSG(23,335,14,0,0,MSGTXT,6)
      FILENAME(1:3) = RECTYPE
      FILENAME(4:6) = DDSID
      BKFILE = 'P:  '
      BKFILE(3:8) = '\DATA\'
      BKFILE(9:14) = FILENAME
      BKFILE(15:18) = '.BDX'
      OPEN (60,FILE=BKFILE,STATUS='UNKNOWN',ACCESS='DIRECT',
     +         FORM='UNFORMATTED',RECL=25)
      MSGTXT = BKFILE
      CALL WRTMSG(22,336,14,0,0,MSGTXT,22)
      DO 65 I2 = 1,9999
         READ(19,REC=I2,ERR=70) IDXREC
         WRITE(60,REC=I2) IDXREC
  65  CONTINUE
  70  CONTINUE
      CLOSE(60)
C
C   MAIN LOOP - READ AND REFORMAT THE DATA.  AS THE RECORDS ARE PROCESSED
C        MARK THEM AS DELETED IN THE INDEX FILE.  IF AN UNCHECKED "RED"
C        FLAG IS ENCOUNTERED ABORT.
C
      DO 75 I=1,NUMELEM
         DO 75 J=1,NUMLINE
            FLAG(I,J) = ' '
75    CONTINUE
      NLINE = 6
      CALL LOCATE(4,0,IERR)    
      CALL WRTSTR(' Loading -                ',26,11,0)
      DO 200 I = BGNIDX,NUMIDX
         READ(19,REC=I) DELKEY,INSTN,INKEY,RECNUM
         INDDS = INKEY(1:3)
         INYRMON = INKEY(4:9)
         IF (DELKEY.LT.2.OR.INDDS.NE.DDSID.OR.
     +         (INYRMON.LT.BYRMON.OR.INYRMON.GT.EYRMON)) THEN
             GO TO 200
         END IF
         IF (INSTN.LT.STRTSTN.OR.INSTN.GT.ENDSTN) THEN
            GO TO 200
         END IF
         READ(20,REC=RECNUM) INSTN,INDDS,DATE,((VALARRAY(I1,J1)
     +      ,I1=1,NUMELEM),J1=1,NUMLINE),(((FLAGARRAY(K,L,M)
     +      ,M=1,2),K=1,NUMELEM),L=1,NUMLINE)
         CALL LOCATE(4,11,IERR)
         CALL WRTSTR(' ',1,11,0)
         CALL WRTSTR(INSTN,8,11,0)
         CALL WRTSTR(' ',1,11,0)
         CALL WRTSTR(DATE,10,11,0)
         DELKEY=1
         WRITE(19,REC=I) DELKEY,INSTN,INKEY,RECNUM
         DO 80 I1 = 1,NUMELEM
            SOMEDATA(I1) = .FALSE.
   80    CONTINUE
C
         DO 100 J1 = 1,NUMLINE
            IF (RECTYPE.EQ.'U-A'.AND.VALARRAY(1,J1).EQ.'      ') THEN
               NUMLVL = J1 - 1
               GO TO 110
            END IF
            DO 100 I1 = 1,NUMELEM 
               HFLAG = FLAGARRAY(I1,J1,1)
               IF (HFLAG.EQ.'C'.OR.HFLAG.EQ.'c'.OR.HFLAG.EQ.'D'.OR.
     +               HFLAG.EQ.'d') THEN
                  WRITE(MSGTXT,90) INSTN,DATE,TBLEABRV(I1),J1
   90             FORMAT (A8,'-',A10,2X,A6,' Line ',I2)
                  CALL DSPMSG(NLINE,77,MSGTXT,12)
                  GO TO 500
               ELSE
                  IF (VALARRAY(I1,J1).EQ.'      ') THEN
                     ARRAY(I1,J1) = -99999.
                     FLAG(I1,J1) = 'M'
                  ELSE
                     SOMEDATA(I1) = .TRUE.
                     IF (FLAGARRAY(I1,J1,2).EQ.'J'.OR.
     +                      FLAGARRAY(I1,J1,2).EQ.'K') THEN
                        CALL CHGFTLIM(ITYPE,I1,J1,INSTN,INKEY,NLINE,
     +                                WRTLIM)
                        MSGPAUSE = .NOT.WRTLIM
                     END IF
                     READ(VALARRAY(I1,J1),'(F5.0,A1)') ARRAY(I1,J1)
     +                    ,FLAG(I1,J1)
                     ARRAY(I1,J1) = ARRAY(I1,J1) * TBLCONV(I1)
                  END IF
               END IF
  100    CONTINUE
  110    CONTINUE
C
         IF (RECTYPE.EQ.'MLY') THEN
            CALL WRTMLY(INSTN,INKEY,ARRAY,FLAG,SOMEDATA,ICOUNT,SORTCOL)
         ELSE IF (RECTYPE.EQ.'10D') THEN
            CALL WRT10D(INSTN,INKEY,ARRAY,FLAG,SOMEDATA,ICOUNT,SORTCOL) 
         ELSE IF (RECTYPE.EQ.'DLY') THEN
            CALL WRTDLY(INSTN,INKEY,ARRAY,FLAG,SOMEDATA,ICOUNT,SORTCOL)
         ELSE IF (RECTYPE.EQ.'SYN') THEN
            CALL WRTSYN(INSTN,INKEY,ARRAY,FLAG,SOMEDATA,ICOUNT,SORTCOL)
         ELSE IF (RECTYPE.EQ.'HLY') THEN
            CALL WRTHLY(INSTN,INKEY,ARRAY,FLAG,SOMEDATA,ICOUNT,SORTCOL)
         ELSE IF (RECTYPE.EQ.'15M') THEN
            CALL WRT15M(INSTN,INKEY,ARRAY,FLAG,SOMEDATA,ICOUNT,SORTCOL) 
         ELSE IF (RECTYPE.EQ.'U-A') THEN
            DO 170 LVLCNT = 1,NUMLVL
               CALL WRTUA(INSTN,INKEY,ARRAY,FLAG,NUMLVL,LVLCNT,ICOUNT)
  170       CONTINUE
         END IF
  200 CONTINUE
C
C   NORMAL END - CLOSE FILES, RESORT THE INDEX FILE
C
      RTNCODE = '0'
      CLOSE(19)
      CLOSE(20)
      IF (WRTLIM) THEN
         CALL CHGDELIM(NEWLIM,NLINE)
         MSGPAUSE = NEWLIM.EQ.0
      ENDIF
      IF (NEWLIM.GT.0) THEN
         CLOSE(52)
      ELSE
         CLOSE(52,STATUS = 'DELETE')
      ENDIF
      CALL LOCATE(NLINE,0,IERR)
      IF (ICOUNT.GT.0) THEN
         CALL SRTIDX(IDXNAM,RTNCODE)
  220    CONTINUE
         IF (RTNCODE.NE.'0') THEN
            OPEN (19,FILE=IDXNAM,STATUS='OLD',ACCESS='DIRECT',
     +            FORM='UNFORMATTED',RECL=25,IOSTAT=IOCHK)
            IF(IOCHK.NE.0.) THEN
               FILNAM = IDXNAM
               CALL OPENMSG(FILNAM,'LDDATA      ',IOCHK)
               GO TO 220
            END IF   
            GO TO 500
         END IF   
         CLOSE(51)
      ELSE
         CALL WRTMSG(19,337,12,1,1,' ',0)
         STOP 2
      END IF
C
      CALL LOCATE(24,0,IERR)
      IF (NEWLIM.GT.0) THEN
         WRITE(MESSAGE,260) NEWLIM
  260    FORMAT(I5)
         CALL WRTMSG(4,340,14,0,0,MESSAGE,5)
         CALL WRTMSG(3,428,12,0,0,' ',0)
         CALL WRTMSG(2,429,12,1,1,' ',0)
         CALL LOCATE(24,0,IERR)
         STOP 1
      END IF
      IF (MSGPAUSE) THEN
         CALL WRTMSG(1,202,15,1,0,' ',0)
         CALL GETCHAR(0,INCHAR)
      ENDIF
      RETURN
C
C   GET HERE IF PROCESSING IS TO BE ABORTED
C
  500 CONTINUE
C
C   DELETE THE MODIFIED INDEX FILE AND RENAME THE BACKUP AS THE ORIGINAL
C
      CLOSE(19,STATUS = 'DELETE')
      CLOSE(20)
      CLOSE(51,STATUS = 'DELETE')
      CLOSE(52,STATUS = 'DELETE')
      BKFILE(19:19) = NULL
      NEWFIL = BKFILE
      NEWFIL (16:18) = 'IDX'
      CALL RNAME2(BKFILE,NEWFIL,IERR)
      CALL WRTMSG(3,246,12,1,1,' ',0)
      IF (IERR.NE.1) THEN
         MSGTXT = ' '
         MSGTXT(1:10) = BKFILE(9:18)
         MSGTXT(11:12) = ', '
         MSGTXT(13:22) = NEWFIL(9:18)
         CALL WRTMSG(4,159,12,1,1,MSGTXT,22)
      END IF
      STOP 2
      END
$PAGE
***********************************************************************
      SUBROUTINE CHGFTLIM(ITYPE,I,J,STNID,INKEY,NLINE,WRTLIM)
C
C   ROUTINE TO CHANGE THE RECORD LIMITS IN THE ELEM.LIM FILE 
C
      CHARACTER*78 MESSAG
      CHARACTER*13 INKEY
      CHARACTER*8 STNID,HSTNID
      CHARACTER*7 MINMAX
      CHARACTER*3 TYPEREC
      CHARACTER*1 RTNCODE
      INTEGER*2  YEAR,MONTH,SOURCE,BYEAR,EYEAR,YEARS
     +           ,MAXFLAG,MINFLAG,HTYPE,HMONTH,HELEM
      INTEGER*4  MAXDATE,MINDATE,NEWDATE,I4REC
      REAL*4     RVALUE,MAXVALUE,MINVALUE,MAXCHNG
      REAL*8     MEANVALUE,STNDRDDEV
      LOGICAL    WRTLIM,NEWLIMFLG
C
$INCLUDE: 'VAL1.INC'
C
C   FIRST DECODE THE DATE OF THE NEW RECORD
C
      READ(INKEY,'(3X,I4,I2)') YEAR,MONTH
      READ(INKEY,'(BZ,3X,I8)') NEWDATE
C      
      IF (ITYPE.LE.2) THEN
C          .. MLY DATA AND 10D DATA
         MONTH = J
         NEWDATE = NEWDATE + MONTH*100
      ELSE IF (ITYPE.EQ.3) THEN
C          .. DLY DATA
         NEWDATE = NEWDATE + J
      END IF
C
C   NEXT DECODE THE NEW RECORD VALUE
C
      READ(VALARRAY(I,J),'(F5.0,1X)') RVALUE
      RVALUE = RVALUE * TBLCONV(I)
C
C   OPEN AND READ THE ELEM.LIM FILE TO FIND THE RECORD OF INTEREST
C
  50  CONTINUE
      OPEN (31,FILE='P:\DATA\ELEM.LIM',STATUS='OLD',ACCESS='DIRECT'
     +        ,RECL=62,IOSTAT=IOCHK)
      IF(IOCHK.NE.0.) THEN
         CALL OPENMSG('P:\DATA\ELEM.LIM      ','LDDATA      ',IOCHK)
         GO TO 50
      END IF   

      CALL FNDEXT(STNID,ITYPE,MONTH,TBLELEM(I),RTNCODE
     +     ,I4REC,SOURCE,BYEAR,EYEAR,YEARS,MAXVALUE,MAXDATE,MAXFLAG
     +     ,MINVALUE,MINDATE,MINFLAG,MEANVALUE,STNDRDDEV,MAXCHNG)
      IF (RTNCODE.NE.'0') GO TO 900
C      
C       ** RECORD OF INTEREST HAS BEEN FOUND - SET NEW VALUES 
C
      NEWLIMFLG = .FALSE.
      TYPEREC = ' '
      IF (FLAGARRAY(I,J,2).EQ.'K') THEN
         IF (RVALUE.GT.MAXVALUE) THEN
            MAXDATE   = NEWDATE
            MAXVALUE  = RVALUE
            MAXFLAG   = 1
            TYPEREC   = 'MAX'
            ICH       = 5
            NEWLIMFLG = .TRUE.
         ELSE IF (RVALUE.EQ.MAXVALUE) THEN
            MAXFLAG   = 2
         END IF
      ELSE
         IF (RVALUE.LT.MINVALUE) THEN
            MINDATE   = NEWDATE
            MINVALUE  = RVALUE
            MINFLAG   = 1
            TYPEREC   = 'MIN'
            ICH       = 1
            NEWLIMFLG = .TRUE.
         ELSE IF (RVALUE.EQ.MINVALUE) THEN
            MINFLAG   = 2
         END IF
      END IF
C      
C       ** THE CALCULATION OF YEARS-OF-RECORD (YEARS) IS INCORRECT.  YEARS
C          IS A COUNT OF DIFFERENT YEARS THAT ACTUALLY CONTAIN DATA NOT
C          JUST THE DIFFERENCE BETWEEN HIGH YEAR AND LOW YEAR.  HOWEVER,
C          EVEN THE REVISED CALCULATION IS IN ERROR SINCE IT ONLY COUNTS
C          YEARS IN WHICH A MIN OR MAX WERE FLAGGED.
C                                  CODE REMOVED   JML 7-22-92
C         IF (YEAR.GT.EYEAR) THEN
C            YEARS = YEARS + YEAR - EYEAR
C            EYEAR = YEAR  
C         END IF
      IF (YEAR.GT.EYEAR) THEN
         YEARS = YEARS + 1
         EYEAR = YEAR  
         NEWLIMFLG = .TRUE.
      ELSE IF (YEAR.LT.BYEAR) THEN
         YEARS = YEARS + 1
         BYEAR = YEAR  
         NEWLIMFLG = .TRUE.
      END IF
C
C       ** REWRITE RECORD IN ELEM.LIM
C
      IELEM = TBLELEM(I)
      IF (NEWLIMFLG) THEN
         WRTLIM = .TRUE.
         WRITE(31,REC=I4REC) STNID,ITYPE,MONTH,IELEM,SOURCE
     +       ,BYEAR,EYEAR,YEARS,MAXVALUE,MAXDATE,MAXFLAG,MINVALUE
     +       ,MINDATE,MINFLAG,MEANVALUE,STNDRDDEV,MAXCHNG
  200    CONTINUE     
         OPEN (61,FILE='P:\DATA\HOLDLIM.DAT',STATUS='UNKNOWN',
     +            ACCESS='DIRECT',FORM='UNFORMATTED',RECL=27,
     +            IOSTAT=IOCHK)
         IF(IOCHK.NE.0.) THEN
            CALL OPENMSG('P:\DATA\HOLDLIM.DAT      ','LDDATA    ',IOCHK)
            GO TO 200
         END IF   
         IHLDREC = 0
  205    CONTINUE   
            IHLDREC = IHLDREC + 1  
            READ(61,REC=IHLDREC,END=210) HSTNID,HTYPE,HMONTH,HELEM,
     +                                   MINMAX
            IF (STNID.EQ.HSTNID .AND. ITYPE.EQ.HTYPE) THEN
               IF (MONTH.EQ.HMONTH .AND. IELEM.EQ.HELEM) THEN
                  GOTO 220
               ENDIF
            ENDIF
         GOTO 205
  210    CONTINUE
         MINMAX = '   -   '     
  220    CONTINUE     
         IF (TYPEREC.NE.' ') THEN
            MINMAX(ICH:ICH+2) = TYPEREC
         ENDIF
         WRITE(61,REC=IHLDREC) STNID,ITYPE,MONTH,IELEM,MINMAX,I,I4REC
         CLOSE(61)
      END IF
      CLOSE(31)
      RETURN
C
C    GET HERE IF THERE IS AN ERROR - THE LIMIT RECORD IS NOT FOUND
C
  900 CONTINUE
         WRITE(MESSAG,230) STNID,MONTH,TBLELEM(I)
  230    FORMAT(A8,1X,I2,1X,I3.3)
         CALL DSPMSG(NLINE,58,MESSAG,12)
         CALL GETMSG(341,MESSAG)
         CALL DSPMSG(NLINE,0,MESSAG,12)
         CALL GETMSG(999,MESSAG)
      CLOSE(31)
      RETURN
      END  
$PAGE
***********************************************************************
      SUBROUTINE CHGDELIM(NEWLIM,NLINE)
C
C   ROUTINE TO WRITE AN UPDATE RECORD FOR EXTREMES TO BE PASSED TO DATAEASE.
C
      CHARACTER*78 MESSAG
      CHARACTER*8 STNID
      CHARACTER*7 MINMAX
      INTEGER*2  MONTH,SOURCE,BYEAR,EYEAR,YEARS
     +           ,MAXFLAG,MINFLAG,NEWLIM
      INTEGER*4  MAXDATE,MINDATE,I4REC
      REAL*4     MAXVALUE,MINVALUE,MAXCHNG
      REAL*8     MEANVALUE,STNDRDDEV
C
      CHARACTER*10 CMAXVAL,CMINVAL,CMEAN,CSTNDRD,CMAXCHNG
      CHARACTER*8  CMAXDAT,CMINDAT
      CHARACTER*4  CBYEAR,CEYEAR,CYEARS
      CHARACTER*1  CMAXFLAG,CMINFLAG
C
$INCLUDE: 'VAL1.INC'
C
C
C       ** OPEN THE ELEM.LIM FILE 
C
  50  CONTINUE
      OPEN (31,FILE='P:\DATA\ELEM.LIM',STATUS='OLD',ACCESS='DIRECT'
     +        ,RECL=62,IOSTAT=IOCHK)
      IF(IOCHK.NE.0.) THEN
         CALL OPENMSG('P:\DATA\ELEM.LIM      ','LDDATA      ',IOCHK)
         GO TO 50
      END IF   
   55 CONTINUE     
      OPEN (61,FILE='P:\DATA\HOLDLIM.DAT',STATUS='OLD',
     +         ACCESS='DIRECT',FORM='UNFORMATTED',RECL=27,
     +         IOSTAT=IOCHK)
      IF(IOCHK.NE.0.) THEN
         CALL OPENMSG('P:\DATA\HOLDLIM.DAT      ','LDDATA    ',IOCHK)
         GO TO 55
      END IF   
C      
C       ** FOR EACH ENTRY IN HOLDLIM.DAT READ THE CORRESPONDING RECORD IN
C          ELEM.LIM AND WRITE A UPDATED DATAEASE RECORD
C
   60 CONTINUE
      READ(61,END=300) STNID,ITYPE,MONTH,IELEM,MINMAX,ICOL,I4REC
      READ(31,REC=I4REC,END=900,ERR=900)STNID,ITYPE,MONTH
     +       ,IELEM,SOURCE,BYEAR,EYEAR,YEARS,MAXVALUE,MAXDATE,MAXFLAG
     +       ,MINVALUE,MINDATE,MINFLAG,MEANVALUE,STNDRDDEV,MAXCHNG
C
C      SET VALUES BACK TO DATAEASE FORMAT AND WRITE RECORD FOR DATAEASE
C
         IF (BYEAR.EQ.-9999) THEN
            CBYEAR = '     '
         ELSE
            WRITE(CBYEAR,'(I4)') BYEAR
         END IF             
         IF (EYEAR.EQ.-9999) THEN
            CEYEAR = '     '
         ELSE
            WRITE(CEYEAR,'(I4)') EYEAR
         END IF 
         IF (YEARS.EQ.-9999) THEN
            CYEARS = '     '
         ELSE
            WRITE(CYEARS,'(I4)') YEARS
         END IF             
         IF (MAXVALUE.EQ.-9999.) THEN
            CMAXVAL = '     '
            CMAXDAT = '      '
            CMAXFLAG = ' '
         ELSE
            WRITE(CMAXVAL,'(F10.4)') MAXVALUE
            WRITE(CMAXDAT,'(I8)') MAXDATE
            WRITE(CMAXFLAG,'(I1)') MAXFLAG
         END IF
         IF (MINVALUE.EQ.-9999.) THEN
            CMINVAL = '     '
            CMINDAT = '      '
            CMINFLAG = ' '
         ELSE
            WRITE(CMINVAL,'(F10.4)') MINVALUE
            WRITE(CMINDAT,'(I8)') MINDATE
            WRITE(CMINFLAG,'(I1)') MINFLAG
         END IF
         IF (MEANVALUE.EQ.-9999.) THEN
            CMEAN = '     '
         ELSE
            WRITE(CMEAN,'(F10.4)') MEANVALUE 
         END IF             
         IF (STNDRDDEV.EQ.-9999.) THEN
            CSTNDRD = '     '
         ELSE
            WRITE(CSTNDRD,'(F10.5)') STNDRDDEV
         END IF             
         IF (MAXCHNG.EQ.-9999.) THEN
            CMAXCHNG = '     '
         ELSE
            WRITE(CMAXCHNG,'(F10.4)') MAXCHNG
         END IF             
         WRITE(52,100) STNID,ITYPE,IELEM,MONTH,SOURCE
     +        ,CBYEAR,CEYEAR,CYEARS,CMAXVAL,CMAXDAT,CMAXFLAG
     +        ,CMINVAL,CMINDAT,CMINFLAG,CMEAN,CSTNDRD,CMAXCHNG
 100     FORMAT(A8,',',I1,',',I3.3,',,',I2,',',I1,',',3(A4,',')
     +          ,2(A10,',',A8,',',A1,','),2(A10,','),A10)
         NEWLIM = NEWLIM + 1
C
         MESSAG = ' '         
         WRITE(MESSAG,120) TBLEABRV(ICOL),STNID
  120    FORMAT(' New limit record ',A6,' for station ',A8)
         CALL DSPMSG(NLINE,0,MESSAG,14)
C
         MESSAG = ' '         
         IF (MINMAX(1:3).NE.' ') THEN
            WRITE(MESSAG(1:24),121) MINMAX(1:3),CMINDAT
  121       FORMAT(5X,A3,' Date:  ',A8)
         ENDIF   
         IF (MINMAX(5:7).NE.' ') THEN
            WRITE(MESSAG(25:),121) MINMAX(5:7),CMAXDAT
         ENDIF   
         CALL DSPMSG(NLINE,0,MESSAG,14)
      GO TO 60
  300 CONTINUE
      CLOSE(31)
      CLOSE(61,STATUS='DELETE')
      RETURN
C
C       ** ERROR PROCESSING
C
  900 CONTINUE
         WRITE(MESSAG,230) STNID,MONTH,IELEM
  230    FORMAT(A8,1X,I2,1X,I3.3)
         CALL DSPMSG(NLINE,58,MESSAG,12)
         CALL GETMSG(341,MESSAG)
         CALL DSPMSG(NLINE,0,MESSAG,12)
         CALL GETMSG(999,MESSAG)
      CLOSE(31)
      RETURN
      END  
$PAGE
***********************************************************************
      SUBROUTINE DSPMSG(NLINE,MSGNUM,MSGTXT,FGCOLOR)
C
C   ROUTINE TO DISPLAY A MESSAGE LINE IN THE WINDOW FROM ROWS 6 TO 20
C     WHEN THE WINDOW FILLS, THE MESSAGES SCROLL...
C
      CHARACTER*72 MSGTXT
      INTEGER*2 NLINE,FGCOLOR
C
      IF (NLINE.GE.20) THEN
         NLINE = 20
         CALL SCROLL(1,1,6,0,20,79)
      ELSE
         NLINE = NLINE + 1
      END IF
C
      IF (MSGNUM.EQ.0) THEN
         CALL LOCATE(NLINE,0,IERR)
         CALL WRTSTR(MSGTXT,72,FGCOLOR,0)
      ELSE
         CALL WRTMSG(25-NLINE,MSGNUM,FGCOLOR,0,0,MSGTXT,35)
      END IF     
      RETURN
      END
$PAGE
************************************************************************
      SUBROUTINE WRTMLY(STNID,INKEY,ARRAY,FLAG,SOMEDATA,ICOUNT,SORTCOL)
************************************************************************
*       THIS SUBROUTINE READS A RECORD FROM THE FILE MLYXXX.TWF        *   
*       THEN WRITES A MLY FORMAT DATAEASE RECORD.                      *
************************************************************************
$INCLUDE: 'VAL1.INC'
*  LOCAL VARIABLES
      CHARACTER*8 LOCALID
      CHARACTER*4 IYEAR
      CHARACTER*3 IELEM,IDDSID
      CHARACTER*1 OUTREC(84) 
      CHARACTER*100 OUTRC2
      INTEGER*2 HEADER(2)
*  PASSED VARIABLES
      REAL        ARRAY(MAXELEM,MAXLINE)
      CHARACTER*1 FLAG(MAXELEM,MAXLINE)
      CHARACTER*13 INKEY
      CHARACTER*8 STNID
      LOGICAL SOMEDATA(MAXELEM)
      INTEGER*2 SORTCOL(MAXELEM)
C
C  EQUIVALENCE THE INPUT VARIABLES TO THE INPUT RECORD STRING 
C
      EQUIVALENCE (HEADER,OUTREC(1)),(IDDSID,OUTREC(7)), 
     +            (LOCALID,OUTREC(10)),
     +            (IELEM,OUTREC(18)),(IYEAR,OUTREC(21)) 
      EQUIVALENCE (OUTREC(1),OUTRC2)

      RTNCODE = '0'
   10 CONTINUE
C
C  WRITE THE INFORMATION INTO THE DATEASE RECORD HEADER  
C
      HEADER(1) = 12
      HEADER(2) = 0
      LOCALID = STNID
      IDDSID = INKEY(1:3)
      IYEAR = INKEY(4:7)
C
C  WRITE DATA VALUES INTO THE DATEASE RECORD  
C
      DO 300 K=1,NUMELEM
         J = SORTCOL(K)
         IF (SOMEDATA(J)) THEN
            WRITE(IELEM,'(I3.3)') TBLELEM(J)
            DO 200 I=1,12
               IPOS = 25 + (I-1)*5
               CALL MOVBYT(4,ARRAY(J,I),1,OUTRC2,IPOS)
               OUTREC(IPOS+4) = FLAG(J,I)
200         CONTINUE
            WRITE(51) OUTREC
            ICOUNT = ICOUNT + 1
         END IF
300   CONTINUE
      RETURN
      END
$PAGE
***********************************************************************
      SUBROUTINE WRT10D(STNID,INKEY,ARRAY,FLAG,SOMEDATA,ICOUNT,SORTCOL)
************************************************************************
*       THIS SUBROUTINE READS A RECORD FROM THE FILE 10DXXX.TWF        *   
*       THEN WRITES A 10D FORMAT DATAEASE RECORD.                      *
************************************************************************
$INCLUDE: 'VAL1.INC'
*  LOCAL VARIABLES
      CHARACTER*8 LOCALID
      CHARACTER*4 IYEAR
      CHARACTER*3 IELEM,IDDSID
      CHARACTER*1 OUTREC(210) 
      CHARACTER*100 OUTRC2
      INTEGER*2 HEADER(6)
*  PASSED VARIABLES
      REAL        ARRAY(MAXELEM,MAXLINE)
      CHARACTER*1 FLAG(MAXELEM,MAXLINE)
      CHARACTER*13 INKEY
      CHARACTER*8 STNID
      LOGICAL SOMEDATA(MAXELEM)
      INTEGER*2 SORTCOL(MAXELEM)
************************************************************************
*    EQUIVALENCE THE INPUT VARIABLES TO THE OUTPUT RECORD STRING       *
************************************************************************
      EQUIVALENCE (HEADER,OUTREC(1)),(IDDSID,OUTREC(13)), 
     +            (LOCALID,OUTREC(16)),
     +            (IELEM,OUTREC(24)),(IYEAR,OUTREC(27)) 
      EQUIVALENCE (OUTREC(1),OUTRC2)

      RTNCODE = '0'
   10 CONTINUE
C
C  WRITE THE INFORMATION INTO THE DATEASE RECORD HEADER
C
      HEADER(1) = 12
      HEADER(2) = 0
      HEADER(3) = 0
      HEADER(4) = 0
      HEADER(5) = 0
      HEADER(6) = 0
      LOCALID = STNID
      IDDSID = INKEY(1:3)
      IYEAR = INKEY(4:9)
C
C  WRITE DATA VALUES INTO THE DATEASE RECORD 
C      
      DO 300 K=1,NUMELEM
         J = SORTCOL(K)
         IF (SOMEDATA(J)) THEN
            WRITE(IELEM,'(I3.3)') TBLELEM(J)
            DO 200 I=1,36
               IPOS = 31 + (I-1)*5
               CALL MOVBYT(4,ARRAY(J,I),1,OUTRC2,IPOS)
               OUTREC(IPOS+4) = FLAG(J,I)
200         CONTINUE
            WRITE(51) OUTREC
            ICOUNT = ICOUNT + 1
         END IF
300   CONTINUE
      RETURN
      END

$PAGE
***********************************************************************
      SUBROUTINE WRTDLY(STNID,INKEY,ARRAY,FLAG,SOMEDATA,ICOUNT,SORTCOL)
************************************************************************
*       THIS SUBROUTINE READS A RECORD FROM THE FILE DLYXXX.TWF        *   
*       THEN WRITES A DLY FORMAT DATAEASE RECORD.                      *
************************************************************************
$INCLUDE: 'VAL1.INC'
*  LOCAL VARIABLES
      CHARACTER*8 LOCALID
      CHARACTER*6 IYEARMO
      CHARACTER*3 IELEM,IDDSID
      CHARACTER*1 OUTREC(186) 
      CHARACTER*100 OUTRC2
      INTEGER*2 HEADER(5)
*  PASSED VARIABLES
      REAL        ARRAY(MAXELEM,MAXLINE)
      CHARACTER*1 FLAG(MAXELEM,MAXLINE)
      CHARACTER*13 INKEY
      CHARACTER*8 STNID
      LOGICAL SOMEDATA(MAXELEM)
      INTEGER*2 SORTCOL(MAXELEM)
************************************************************************
*    EQUIVALENCE THE INPUT VARIABLES TO THE OUTPUT RECORD STRING       *
************************************************************************
      EQUIVALENCE (HEADER,OUTREC(1)),(IDDSID,OUTREC(12)), 
     +            (LOCALID,OUTREC(15)),
     +            (IELEM,OUTREC(23)),(IYEARMO,OUTREC(26)) 
      EQUIVALENCE (OUTREC(1),OUTRC2)

      RTNCODE = '0'
   10 CONTINUE
C
C  WRITE THE INFORMATION INTO THE DATEASE RECORD HEADER
C
      HEADER(1) = 12
      HEADER(2) = 0
      HEADER(3) = 0
      HEADER(4) = 0
      HEADER(5) = 0
      LOCALID = STNID
      IDDSID = INKEY(1:3)
      IYEARMO = INKEY(4:9)
C
C  WRITE DATA VALUES INTO THE DATEASE RECORD 
C      
      DO 300 K=1,NUMELEM
         J = SORTCOL(K)
         IF (SOMEDATA(J)) THEN
            WRITE(IELEM,'(I3.3)') TBLELEM(J)
            DO 200 I=1,31
               IPOS = 32 + (I-1)*5
               CALL MOVBYT(4,ARRAY(J,I),1,OUTRC2,IPOS)
               OUTREC(IPOS+4) = FLAG(J,I)
200         CONTINUE
            WRITE(51) OUTREC
            ICOUNT = ICOUNT + 1
         END IF
300   CONTINUE
      RETURN
      END
$PAGE
***********************************************************************
      SUBROUTINE WRTHLY(STNID,INKEY,ARRAY,FLAG,SOMEDATA,ICOUNT,SORTCOL)
************************************************************************
*       THIS SUBROUTINE READS A RECORD FROM THE FILE HLYXXX.TWF        *   
*       THEN WRITES A HLY FORMAT DATAEASE RECORD.                      *
************************************************************************
$INCLUDE: 'VAL1.INC'
*  LOCAL VARIABLES
      CHARACTER*8 LOCALID
      CHARACTER*8 IYRMODA
      CHARACTER*3 IELEM,IDDSID
      CHARACTER*1 OUTREC(151) 
      CHARACTER*100 OUTRC2
      INTEGER*2 HEADER(4)
*  PASSED VARIABLES
      REAL        ARRAY(MAXELEM,MAXLINE)
      CHARACTER*1 FLAG(MAXELEM,MAXLINE)
      CHARACTER*13 INKEY
      CHARACTER*8 STNID
      LOGICAL SOMEDATA(MAXELEM)
      INTEGER*2 SORTCOL(MAXELEM)
************************************************************************
*    EQUIVALENCE THE INPUT VARIABLES TO THE INPUT RECORD STRING        *
************************************************************************
      EQUIVALENCE (HEADER,OUTREC(1)),(IDDSID,OUTREC(10)), 
     +            (LOCALID,OUTREC(13)),
     +            (IELEM,OUTREC(21)),(IYRMODA,OUTREC(24)) 
      EQUIVALENCE (OUTREC(1),OUTRC2)

      RTNCODE = '0'
   10 CONTINUE
C
C  WRITE THE INFORMATION INTO THE DATEASE RECORD HEADER  
C
      HEADER(1) = 12
      HEADER(2) = 0
      HEADER(3) = 0
      HEADER(4) = 0
      LOCALID = STNID
      IDDSID = INKEY(1:3)
      IYRMODA = INKEY(4:11)
C
C  WRITE DATA VALUES INTO THE DATEASE RECORD       
C
      DO 300 K=1,NUMELEM
         J = SORTCOL(K)
         IF (SOMEDATA(J)) THEN
            WRITE(IELEM,'(I3.3)') TBLELEM(J)
            DO 200 I=1,24
               IPOS = 32 + (I-1)*5
               CALL MOVBYT(4,ARRAY(J,I),1,OUTRC2,IPOS)
               OUTREC(IPOS+4) = FLAG(J,I)
200         CONTINUE
            WRITE(51) OUTREC
            ICOUNT = ICOUNT + 1
         END IF
300   CONTINUE
      RETURN
      END
$PAGE
***********************************************************************
      SUBROUTINE WRT15M(STNID,INKEY,ARRAY,FLAG,SOMEDATA,ICOUNT,SORTCOL)
************************************************************************
*       THIS SUBROUTINE READS A RECORD FROM THE FILE 15MXXX.TWF        *   
*       AND WRITES A 15M FORMAT DATAEASE RECORD.                      *
************************************************************************
$INCLUDE: 'VAL1.INC'
*  LOCAL VARIABLES
      CHARACTER*8 LOCALID
      CHARACTER*8 IYRMODA
      CHARACTER*3 IELEM,IDDSID
      CHARACTER*1 OUTREC(529) 
      CHARACTER*100 OUTRC2
      INTEGER*2 HEADER(13)
*  PASSED VARIABLES
      REAL        ARRAY(MAXELEM,MAXLINE)
      CHARACTER*1 FLAG(MAXELEM,MAXLINE)
      CHARACTER*13 INKEY
      CHARACTER*8 STNID
      LOGICAL SOMEDATA(MAXELEM)
      INTEGER*2 SORTCOL(MAXELEM)
************************************************************************
*    EQUIVALENCE THE INPUT VARIABLES TO THE INPUT RECORD STRING        *
************************************************************************
      EQUIVALENCE (HEADER,OUTREC(1)),(IDDSID,OUTREC(28)), 
     +            (LOCALID,OUTREC(31)),
     +            (IELEM,OUTREC(39)),(IYRMODA,OUTREC(42)) 
      EQUIVALENCE (OUTREC(1),OUTRC2)

      RTNCODE = '0'
   10 CONTINUE
C
C  WRITE THE INFORMATION INTO THE DATEASE RECORD HEADER  
C
      HEADER(1) = 12
      DO 20 I1 = 2,13
         HEADER(I1) = 0
20    CONTINUE
      LOCALID = STNID
      IDDSID = INKEY(1:3)
      IYRMODA = INKEY(4:11)
C
C  WRITE DATA VALUES INTO THE DATEASE RECORD       
C
      DO 300 K=1,NUMELEM
         J = SORTCOL(K)
         IF ( SOMEDATA(J)) THEN
            WRITE(IELEM,'(I3.3)') TBLELEM(J)
            DO 200 I=1,96
               IPOS = 50 + (I-1)*5
               CALL MOVBYT(4,ARRAY(J,I),1,OUTRC2,IPOS)
               OUTREC(IPOS+4) = FLAG(J,I)
200         CONTINUE
            WRITE(51) OUTREC
            ICOUNT = ICOUNT + 1
         END IF
300   CONTINUE
      RETURN
      END
$PAGE
************************************************************************
      SUBROUTINE WRTSYN(STNID,INKEY,ARRAY,FLAG,SOMEDATA,ICOUNT,SORTCOL)
************************************************************************
*       THIS SUBROUTINE READS A RECORD FROM THE FILE SYNXXX.TWF        *   
*       THEN WRITES A SYN FORMAT DATAEASE RECORD.                      *
************************************************************************
$INCLUDE: 'VAL1.INC'
*  LOCAL VARIABLES
      CHARACTER*8 LOCALID
      CHARACTER*8 IYRMODA
      CHARACTER*3 IELEM,IDDSID
      CHARACTER*1 OUTREC(67) 
      CHARACTER*100 OUTRC2
      INTEGER*2 HEADER(4)
*  PASSED VARIABLES
      REAL        ARRAY(MAXELEM,MAXLINE)
      CHARACTER*1 FLAG(MAXELEM,MAXLINE)
      CHARACTER*13 INKEY
      CHARACTER*8 STNID
      LOGICAL SOMEDATA(MAXELEM)
      INTEGER*2 SORTCOL(MAXELEM)
C
C  EQUIVALENCE THE INPUT VARIABLES TO THE INPUT RECORD STRING 
C
      EQUIVALENCE (HEADER,OUTREC(1)),(IDDSID,OUTREC(6)), 
     +            (LOCALID,OUTREC(9)),
     +            (IELEM,OUTREC(17)),(IYRMODA,OUTREC(20)) 
      EQUIVALENCE (OUTREC(1),OUTRC2)

      RTNCODE = '0'
   10 CONTINUE
C
C   WRITE THE INFORMATION INTO THE DATEASE RECORD HEADER     
C
      HEADER(1) = 12
      HEADER(2) = 0
      HEADER(3) = 0
      HEADER(4) = 0
      LOCALID = STNID
      IDDSID = INKEY(1:3)
      IYRMODA = INKEY(4:11)
C
C  WRITE DATA VALUES INTO THE DATEASE RECORD 
C
      DO 300 K=1,NUMELEM
         J = SORTCOL(K)
         IF (SOMEDATA(J)) THEN
            WRITE(IELEM,'(I3.3)') TBLELEM(J)
            DO 200 I=1,8
               IPOS = 28 + (I-1)*5
               CALL MOVBYT(4,ARRAY(J,I),1,OUTRC2,IPOS)
               OUTREC(IPOS+4) = FLAG(J,I)
200         CONTINUE
            WRITE(51) OUTREC
            ICOUNT = ICOUNT + 1
         END IF
300   CONTINUE
      RETURN
      END
$PAGE
************************************************************************
      SUBROUTINE WRTUA(STNID,INKEY,ARRAY,FLAG,NUMLVL,LVLCNT,ICOUNT)
************************************************************************
*       THIS SUBROUTINE READS A RECORD FROM THE FILE U-AXXX.TWF        *   
*       THEN WRITES A U-A FORMAT DATAEASE RECORD.                      *
************************************************************************
$INCLUDE: 'VAL1.INC'
*  LOCAL VARIABLES
      CHARACTER*8 LOCALID
      CHARACTER*8 IYRMODA
      CHARACTER*2 IHOUR  
      CHARACTER*3 IDDSID,INUMLVL,ILVLNUM
      CHARACTER*1 OUTREC(62) 
      CHARACTER*62 OUTRC2
      CHARACTER*1 IFLAGS(6)
      CHARACTER*4 AVAL
      REAL        FVAL
      INTEGER*2 HEADER(2)
*  PASSED VARIABLES
      REAL        ARRAY(MAXELEM,MAXLINE)
      CHARACTER*1 FLAG(MAXELEM,MAXLINE)
      CHARACTER*13 INKEY
      CHARACTER*8 STNID
************************************************************************
*    EQUIVALENCE THE INPUT VARIABLES TO THE INPUT RECORD STRING        *
************************************************************************
      EQUIVALENCE (HEADER,OUTREC(1)),(IDDSID,OUTREC(6)),
     +            (LOCALID,OUTREC(9)),(IYRMODA,OUTREC(17)),
     +            (IHOUR,OUTREC(25)),(INUMLVL,OUTREC(27)),
     +            (ILVLNUM,OUTREC(30)),(IFLAGS,OUTREC(57))
      EQUIVALENCE (OUTREC(1),OUTRC2)

      EQUIVALENCE (AVAL,FVAL)
C
C  WRITE THE INFORMATION INTO THE DATEASE RECORD HEADER
C
      HEADER(1) = 12
      HEADER(2) = 0
      LOCALID = STNID
      IDDSID = INKEY(1:3)
      IYRMODA = INKEY(4:11)
      IHOUR = INKEY(12:13)
      WRITE(INUMLVL,'(I3.3)') NUMLVL
C
C  WRITE DATA VALUES INTO THE DATEASE RECORD  
C
C         LVLCNT = LVLCNT + 1
         WRITE(ILVLNUM,'(I3.3)') LVLCNT
         DO 200 I=1,6
            IPOS = 33 + (I-1)*4
            IF (I.EQ.5.AND.ARRAY(I,LVLCNT).EQ.-99999) THEN
               ARRAY(I,LVLCNT) = -999
            END IF
            CALL MOVBYT(4,ARRAY(I,LVLCNT),1,OUTRC2,IPOS)
            IFLAGS(I) = FLAG(I,LVLCNT)
200      CONTINUE
         WRITE(51) OUTREC
         ICOUNT = ICOUNT + 1
      RETURN
      END
