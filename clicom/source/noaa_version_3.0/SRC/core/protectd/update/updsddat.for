$STORAGE:2
$DEBUG
C     PROGRAM UPDSDDAT
C
C     NOTE:  THE VERIFY FLAG (0=DO NOT VERIFY  1=VERIFY EACH RECORD) AND
C            THE FILE NAME OF THE DATASET DIRECTORY FORM MUST BE 
C            INCLUDED ON THE COMMAND LINE CALLING THIS ROUTINE
C            EXAMPLE: "UPDSDDAT 1 DATAAAAA"
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
      PROGRAM UPDSDDAT
C
      INTEGER*4    RECCOUNT,NRECCOUNT,IREC
      CHARACTER*1  VERFLG
      CHARACTER*2  RTNFLAG
      CHARACTER*3  RECDEF(8)
      CHARACTER*6  INWDATE,INRDATE
      CHARACTER*8  DEASEFIL,FIELD(9),OUTCNT,DATEFMT(3)
      CHARACTER*20 RECPRT(2)
      CHARACTER*22 FILENAME
      CHARACTER*64 HELPFILE
      CHARACTER*78 MSGTXT
      LOGICAL VERIFY,NEWREC,NXTREC,FMTREC,KNTREC
C      
      CHARACTER*64 RESULT
      INTEGER*4 PSP,PSPNCHR,OFFSET
C
      INTEGER*1   DSDTYPE
      CHARACTER*1 DSDREC(72)
      CHARACTER*3 DDSID
      CHARACTER*6 WRTDATE,RDDATE
      CHARACTER*8 DSDFILE
      EQUIVALENCE (DSDTYPE,DSDREC(5)),(DDSID,DSDREC(6)),
     +            (DSDFILE,DSDREC(41)),
     +            (WRTDATE,DSDREC(57)),(RDDATE,DSDREC(63))
C     
      DATA RECDEF /'MLY','10D','DLY','SYN','HLY','15M','U-A','NML'/
      DATA FIELD /9*'        '/
      DATA HELPFILE/'P:\HELP\UPDSDDAT.HLP'/
      DATA DATEFMT/'MM/DD/YY', 'DD/MM/YY', 'YY/MM/DD'/
C
C   LOCATE SEGMENTED ADDRESS OF THE BEGINNING OF THIS PROGRAM
C
      OFFSET = #00100000
      PSP = LOCFAR(UPDSDDAT)
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
C   PULL THE COMMAND OUT OF THE RESULT
C
C       .. VERIFY FLAG
      VERFLG = RESULT(1:1)
      IF (VERFLG.NE.'1' .AND. VERFLG.NE.'0') GO TO 900
      VERIFY = VERFLG.EQ.'1'
C       .. FILE NAME FOR DATASET DIRECTORY FORM
      DEASEFIL = RESULT(3:10)
      WRITE(FILENAME,520) DEASEFIL
C
C       ** GET THE CURRENT DATE FORMAT FROM P:\DATA\DEDATE.CFG
C
      CALL RDDATCFG(IOUTFMT)
C
C       ** OPEN THE DATA FILE FOR THE DATASET DIRECTORY FORM
C
   20 CONTINUE
      OPEN (25,FILE=FILENAME,STATUS='OLD',FORM='BINARY',SHARE='DENYWR'
     +       ,MODE='READWRITE',ACCESS='DIRECT',RECL=72,IOSTAT=IOCHK)
      IF (IOCHK.NE.0) THEN
         CALL OPENMSG(FILENAME,'UPDSDDAT   ',IOCHK)
         GO TO 20
      END IF
C
C       ** OPEN THE FILE THAT CONTAINS A RECORD OF THE ORIGINAL AND MODIFIED
C          DATES FOR EACH RECORD IN THE DATASET DIRECTORY
C
      FILENAME = 'Q:\UPDSDDAT.OUT'
   25 CONTINUE
      OPEN (51,FILE=FILENAME,STATUS='UNKNOWN',IOSTAT=IOCHK)
      IF (IOCHK.NE.0) THEN
         CALL OPENMSG(FILENAME,'UPDSDDAT   ',IOCHK)
         GO TO 25
      END IF
C
C       ** IF AUTOMATIC REFORMATTING, GET THE ORIGINAL FORMAT OF THE
C          DATE FIELDS
C
      IF (.NOT.VERIFY) THEN
   30    CONTINUE      
         FIELD(1) = DATEFMT(IOUTFMT)
         RTNFLAG = 'FA'
         CALL LOCATE(0,0,IERR) 
         CALL GETFRM('REFMTAUT',' ',FIELD,8,RTNFLAG)
         IF (RTNFLAG.EQ.'4F') THEN
            GO TO 920
         ELSE IF (RTNFLAG.EQ.'2F') THEN
            READ(FIELD(2),530) INFMT1
            READ(FIELD(3),530) INFMT2
            CALL CLS
         ELSE
            GO TO 30
         ENDIF      
      ENDIF         
C
C       ** WRITE THE RUNNING TOTAL LINE
C
      MSGTXT = ' '
      CALL GETMSG(561,MSGTXT)
      CALL GETMSG(999,MSGTXT)
      CALL PARSE1(MSGTXT,78,2,20,RECPRT,RTNCODE)
      NCPRT1=LNG(RECPRT(1))+2
      MSGTXT = ' '
      MSGTXT(2:)=RECPRT(1)
      MSGTXT(NCPRT1+10:)=RECPRT(2)
      NCPRT2=LNG(MSGTXT)+1
      CALL CLRMSG(3)
      CALL LOCATE(22,0,IERR)
      CALL WRTSTR(MSGTXT,NCPRT2,14,0)
C      
      NRECCOUNT = 0          
      RECCOUNT = 0
      IREC = 0
   50 CONTINUE
         IREC = IREC + 1
         READ(25,REC=IREC,ERR=110) DSDREC
C
C          .. WRITE NUMBER OF RECORDS READ TO SCREEN            
         NRECCOUNT = NRECCOUNT + 1
         CALL LOCATE(22,NCPRT1,IERR)
         WRITE(OUTCNT,'(I7,1X)') NRECCOUNT
         OUTCNT(8:8) = CHAR(0)
         CALL CWRITE(OUTCNT,12,IERR)
C
         NEWREC = .TRUE.
         KNTREC = .TRUE.
         NXTREC = .FALSE.      
         FMTREC = .FALSE.      
         ITYP = DSDTYPE
C
   60    CONTINUE
            IF (NXTREC) THEN
               GO TO 100
            ELSE            
               IF (VERIFY) THEN
                  WRITE(FIELD(1),'(I4)') IREC
                  FIELD(2) = RECDEF(ITYP)
                  FIELD(3) = DDSID
                  FIELD(4) = DSDFILE
                  FIELD(5) = DATEFMT(IOUTFMT)
                  WRITE(FIELD(6),510) (WRTDATE(I:I+1),I=1,5,2)
                  WRITE(FIELD(8),510) (RDDATE(I:I+1),I=1,5,2)
                  IF (NEWREC) THEN
                     FIELD(7) = ' '
                     FIELD(9) = ' '
                  ELSE   
                     WRITE(FIELD(7),530) INFMT1
                     WRITE(FIELD(9),530) INFMT2
                  ENDIF   
                  CALL LOCATE(0,0,IERR) 
                  RTNFLAG = 'FV'
                  CALL GETFRM('REFMTDSD',HELPFILE,FIELD,8,RTNFLAG)
                  READ(FIELD(7),530) INFMT1
                  READ(FIELD(9),530) INFMT2
                  CALL CLRMSG(5)
                  IF (RTNFLAG.EQ.'4F') THEN
                     GO TO 920
                  ELSE IF (RTNFLAG.EQ.'2F') THEN   
                     FMTREC = .TRUE.
                  ELSE IF (RTNFLAG.EQ.'3F') THEN   
                     NXTREC = .TRUE.
                  ENDIF
               ELSE
                  FMTREC = .TRUE.
                  NXTREC = .TRUE.
               ENDIF
C
               INWDATE = WRTDATE
               INRDATE = RDDATE
               IF (NEWREC) THEN
                  NEWREC = .FALSE.
                  WRITE(51,500) IREC,RECDEF(ITYP),DDSID,DSDFILE,
     +                 (INWDATE(I:I+1),I=1,5,2),(INRDATE(I:I+1),I=1,5,2)
               ENDIF
               IF (FMTREC) THEN   
                  FMTREC = .FALSE.
                  CALL REFMTDAT(INFMT1,IOUTFMT,INWDATE,WRTDATE)
                  CALL REFMTDAT(INFMT2,IOUTFMT,INRDATE,RDDATE)
                  WRITE(25,REC=IREC) DSDREC
                  WRITE(51,501) (WRTDATE(I:I+1),I=1,5,2), 
     +                           (RDDATE(I:I+1),I=1,5,2)
                  IF (VERIFY) THEN
                     INFMT1 = IOUTFMT
                     INFMT2 = IOUTFMT
                     CALL WRTMSG(5,608,12,0,0,' ',0)
                  ENDIF   
C                  .. WRITE THE CURRENT COUNT OF RECORDS USED TO SCREEN
                  IF (KNTREC) THEN
                     KNTREC = .FALSE.
                     RECCOUNT = RECCOUNT + 1
                     CALL LOCATE(22,NCPRT2,IERR)
                     WRITE(OUTCNT,'(I7,1X)') RECCOUNT
                     OUTCNT(8:8) = CHAR(0)
                     CALL CWRITE(OUTCNT,12,IERR)
                  ENDIF   
               ENDIF   
            ENDIF      
         GO TO 60     
  100    CONTINUE 
      GO TO 50     
  110 CONTINUE      
      IF (.NOT.VERIFY) THEN
         CALL WRTMSG(2,609,12,1,1,' ',0)
      ENDIF   
      CLOSE(25)
      CLOSE(51)
      STOP ' '      
C
C       ** ERROR PROCESSING
C
  900 CONTINUE
C          .. ILLEGAL VERIFY CODE  
         CALL LOCATE(24,0,IERR)
         STOP 3
  920 CONTINUE
C          .. PREMATURE EXIT WITH ESC/F4  
         CLOSE(25)
         CLOSE(51)
         CALL LOCATE(24,0,IERR)
         STOP 1      
C
C       ** FORMAT STMTS
C      
  500 FORMAT(1X,I4,2X,A3,A3,1X,A8,2(2X,A2,'/',A2,'/',A2))
  501 FORMAT(22X,2(2X,A2,'/',A2,'/',A2))
  510 FORMAT(A2,'/',A2,'/',A2)
  520 FORMAT('Q:',A8,'.DBM')            
  530 FORMAT(I1)
      END
*****************************************************************************
      SUBROUTINE REFMTDAT(INFMT,IOUTFMT,OLDDATE,NEWDATE)
C
C       ** OBJECTIVE:  GET THE DATE FORMAT FROM THE DATE CONFIGURATION
C                      FILE.  RETURN THE CURRENT DATE IN THIS DATE FORMAT.
C       ** INPUT:
C             INFMT......FORMAT CODE FOR OLDDATE
C             IOUTFMT....FORMAT CODE FOR NEWDATE -- READ FROM DEDATE.CFG
C             OLDDATE....DATE THAT WILL BE REFORMATTED
C       ** OUTPUT:
C             NEWDATE....DATE IN THE FORMAT CURRENTLY USED BY DATAEASE
C
      INTEGER*2 INFMT,IOUTFMT
      CHARACTER*6 OLDDATE,NEWDATE
C
      PARAMETER (MXFMT=3)
      INTEGER*2 IDATE(3),IDXDATE(3,MXFMT,MXFMT)
C
C       ** FORMAT   DATE       NAME      
C          1        MM-DD-YY   NORTH AMERICAN
C          2        DD-MM-YY   INTERNATIONAL
C          3        YY-MM-DD   METRIC
C
C          FORMAT INDEXES
C          (IN,OUT)  1,1     2,1     3,1
C                    1,2     2,2     3,2
C                    1,3     2,3     3,3
C
C          MDY INDEXES FOR THE ABOVE FORMAT PAIRS
      DATA IDXDATE/1,2,3,  2,1,3,  2,3,1,
     +             2,1,3,  1,2,3,  3,2,1,
     +             3,1,2,  3,2,1,  1,2,3/  
C
      IF (INFMT.NE.IOUTFMT) THEN            
C
C             .. SET THE YEAR, MONTH, DAY INDEXES FOR THE CURRENT DATE FORMAT
         IDX1 = IDXDATE(1,INFMT,IOUTFMT)
         IDX2 = IDXDATE(2,INFMT,IOUTFMT)
         IDX3 = IDXDATE(3,INFMT,IOUTFMT)
C
         READ(OLDDATE,'(3I2.2)') IDATE(1),IDATE(2),IDATE(3)
         WRITE(NEWDATE,'(3I2.2)') IDATE(IDX1),IDATE(IDX2),IDATE(IDX3)
      ENDIF   
C
      RETURN
      END      
      