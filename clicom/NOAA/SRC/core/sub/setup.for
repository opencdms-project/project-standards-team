$STORAGE:2
      SUBROUTINE SETUP
C
C   PROGRAM TO DEFINE A KEY-ENTRY FORM, BUILD THE SETUP FILE, AND 
C      DEFINE THE DATA AND INDEX FILES THAT WILL BE USED
C
$INCLUDE:'VAL1.INC'
C
      PARAMETER (MAX2ELEM = (MAXELEM * 2)+2)
      CHARACTER*1 REPLY,DELMARK,RTNCODE
      CHARACTER*2 RTNFLAG
      CHARACTER*3 DATATYPE(7),DATASET,INTYPE,INDDS
      CHARACTER*6 FIELD(MAX2ELEM)
      CHARACTER*7 TEXT7
      CHARACTER*8 BLANK
      CHARACTER*18 FILNAM,IDXNAM
      CHARACTER*22 ERRFIL
      CHARACTER*64 HELP1,HELP2,HELP3,HELP4
C
      INTEGER*2 ELEMSAV,ELEMCODE(MAXELEM),OPTION,NUMCOLS
     +         ,DELKEY,NUMIDX,BGNIDX,RLNGTH,INUMLINE(7)
      INTEGER*2 HIELEM(6),LOWELEM(6)
C
      LOGICAL NEWFILE, ELEMFOUND, REINIT, NEWELEM, TFVAL
C
      DATA LOWELEM /201,401,001,101,101,101/
     +    ,HIELEM  /300,500,100,200,200,200/
      DATA DATATYPE /'MLY','10D','DLY','SYN','HLY','15M','U-A'/
     +    ,INUMLINE /12,36,31,8,24,96,100/
     +    ,BLANK /'        '/
     +    ,HELP1,HELP2 /'P:\HELP\SETUP1.HLP','P:\HELP\SETUP2.HLP'/
     +    ,HELP3,HELP4 /'P:\HELP\SETUP3.HLP','P:\HELP\SETUP4.HLP'/
C
      DELKEY = 0
      BGNIDX = 2
      NUMIDX = 1
C
C   OPEN THE SETUP FILE - NOTE IT IS A DIRECT FILE WITH 10 SLOTS
C       FOR EACH DATATYPE
C
      IRECLEN = 18 + 2*MAXELEM + 5*2
C
   10 CONTINUE
      OPEN (5,FILE='P:\DATA\SETUP.DAT',STATUS='OLD',ACCESS='DIRECT'
     +      ,RECL=IRECLEN,IOSTAT=IOCHK)
      IF(IOCHK.NE.0) THEN
C
C   IF NO SETUP FILE EXISTS, CREATE IT AND INTIALIZE 70 SLOTS AS 
C   DELETED RECORDS - OTHERWISE THEY WILL BE FULL OF TRASH WHEN 
C   RECORDS ARE CREATED AT 10,20,30, ETC...
C
         IF(IOCHK .EQ. 6416) THEN
            OPEN (5,FILE='P:\DATA\SETUP.DAT',STATUS='NEW',
     +            ACCESS='DIRECT',RECL=IRECLEN,IOSTAT=IOCHK)
            DELMARK = '*'
            DO 15 I1 = 1,70
               WRITE(5,REC=I1) DELMARK
   15       CONTINUE
         ELSE      
            CALL OPENMSG('P:\DATA\SETUP.DAT     ','SETUP       ',IOCHK)   
            GO TO 10
         END IF   
      END IF   
C
C   DETERMINE THE OPTION TO BE PERFORMED
C
  20  CONTINUE
      CALL CLS
      CALL LOCATE(1,1,IERR)
      REINIT = .FALSE.
      CALL GETMNU('SETUP-MENU  ',HELP1,OPTION)
      IF (OPTION.EQ.0) THEN
         CALL LOCATE(23,0,IERR)
         CLOSE (5)
         RETURN
      ELSE IF (OPTION.EQ.4) THEN
C
C    OPTION 4 - LIST THE CURRENT DATA FILES DEFINED
C 
         CALL CLS
         CALL LOCATE(0,0,IERR)
         CALL WRTSTR(' Datatype Dataset',17,15,1)
         IROW = 1
         ICOL = 1
         DO 25 IREC = 1,999
            READ(5,REC=IREC,ERR=30) DELMARK,INTYPE,INDDS,NUMCOLS
            IF (DELMARK.EQ.BLANK) THEN
               IF (IROW.GT.20) THEN
                   CALL LOCATE(23,1,IERR)
                   CALL WRTMSG(2,202,11,0,0,' ',0)
                   CALL GETCHAR(0,RTNCODE)
                   IF (RTNCODE.EQ.'4F') THEN
                      GO TO 20
                   END IF
                   CALL CLS
                   CALL LOCATE(0,0,IERR)
                   CALL WRTSTR(' Datatype Dataset',17,15,1)
                   IROW = 1
                END IF
                IROW = IROW + 1   
                CALL LOCATE(IROW,3,IERR)
                CALL WRTSTR(INTYPE,3,14,0) 
                CALL WRTSTR(BLANK,5,14,0)
                CALL WRTSTR(INDDS,3,14,0)
            END IF
   25    CONTINUE
   30    CONTINUE
         CALL LOCATE(23,1,IERR)
         CALL WRTMSG(2,202,11,0,0,' ',0)
         CALL GETCHAR(0,RTNCODE)
         GO TO 20
      END IF
C
C   OTHERWISE ASK FOR THE DATATYPE
C
      CALL LOCATE(1,46,IERR)
      CALL GETMNU('DR-DATATYPES',HELP1,ITYPE)
      IF (ITYPE.EQ.0) THEN
         GO TO 20
      END IF
C
C   IF UPPER AIR AND OPTION 2 - TELL USER HE CAN NOT MODIFY UPPER
C      FORMS - THEY ARE FIXED
C
      IF (ITYPE.EQ.7.AND.OPTION.EQ.2) THEN
         CALL WRTMSG(4,312,12,1,1,' ',0)
         GO TO 20
      END IF           
C
C   ELSE ASK FOR THE DATASET AND SEE IF IT HAS ALREADY BEEN DEFINED
C
35    CONTINUE
      CALL LOCATE(16,30,IERR)
      CALL WRTSTR('Dataset-ID: ',12,14,0)
      DATASET = '   '
      CALL GETSTR(3,DATASET,3,15,1,RTNFLAG)
      IF (RTNFLAG.EQ.'1F') THEN
         CALL DSPWIN(HELP2)
         GO TO 35
      ELSE IF (DATASET.EQ.'   '.OR.RTNFLAG.EQ.'4F') THEN
         GO TO 20
      END IF
      READ(DATASET,'(BN,I3)') IDATA
      WRITE(DATASET,'(I3.3)') IDATA
C
      FILNAM = 'P:\DATA\AAAAAA.TWF'
      IDXNAM = 'P:\DATA\AAAAAA.IDX'
      FILNAM(9:11) = DATATYPE(ITYPE)
      FILNAM(12:14) = DATASET
      IDXNAM(9:11) = DATATYPE(ITYPE)
      IDXNAM(12:14) = DATASET
C
C   SEARCH FOR THE DATATYPE AND DATASET-ID SPECIFIED - REMEMBER THERE
C      ARE 10 SLOTS ALLOWED FOR EACH DATATYPE SO BEGIN THE SEARCH WHERE
C      APPROPRIATE.  DATATYPES 10D AND 15M ADDED AFTER THE OTHER 5 
C
      IF (ITYPE.EQ.2) THEN
         JTYPE = 6
      ELSE IF (ITYPE.EQ.6) THEN
         JTYPE = 7
      ELSE IF (ITYPE.EQ.7) THEN
         JTYPE = 5  
      ELSE IF (ITYPE.GT.2) THEN
         JTYPE = ITYPE - 1
      ELSE 
         JTYPE = 1
      END IF
      ISTRT = (JTYPE-1)*10 + 1
      DO 40 IREC = ISTRT,999
         READ(5,REC=IREC,ERR=45) DELMARK,INTYPE,INDDS,NUMCOLS,HIGHPCT
     +        ,LOWPCT,(ELEMCODE(I1),I1=1,MAXELEM),(AQCELEM(I2),I2=1,5)
         IF (DELMARK.EQ.' '.AND.INTYPE.EQ.DATATYPE(ITYPE).AND.
     +           INDDS.EQ.DATASET) THEN
             NEWFILE = .FALSE.      
             GO TO 50
         END IF
   40 CONTINUE
   45 CONTINUE
      NEWFILE = .TRUE.
   50 CONTINUE
C
C   CHECK IF THE OPTION SELECTED AGREES WITH THE FORM SELECTED
C
      IF (NEWFILE) THEN
         IF (OPTION.NE.1) THEN
            CALL WRTMSG(7,151,12,1,1,' ',0)
            GO TO 20
         ELSE
            HIGHPCT = 2.0
            LOWPCT = 1.7
         END IF
      ELSE
         IF (OPTION.EQ.1) THEN
            CALL WRTMSG(7,152,12,1,1,' ',0)
            GO TO 20
         ELSE IF (OPTION.EQ.3) THEN
C
C          VERIFY THAT THE DATASET SHOULD BE DELETED AND DELETE IT
C
            CALL WRTMSG(7,291,12,1,0,' ',0)
            CALL LOCATE(19,1,IERR)
            CALL OKREPLY(REPLY,RTNCODE)
            IF (REPLY.EQ.'Y'.AND.RTNCODE.EQ.'0') THEN
               DELMARK = '*'
               CALL WRTMSG(5,292,11,0,0,' ',0)
               WRITE(5,REC=IREC) DELMARK,INTYPE,INDDS,NUMCOLS,HIGHPCT
     +            ,LOWPCT,(ELEMCODE(I1),I1=1,MAXELEM)
     +            ,(AQCELEM(I2),I2=1,5)
               OPEN (19,FILE=IDXNAM,STATUS='OLD',ACCESS='DIRECT',
     +               FORM='UNFORMATTED',RECL=25,IOSTAT=IOCHK)
               IF (IOCHK.NE.0) THEN
                  IF (IOCHK .NE. 6416) THEN
                     ERRFIL = IDXNAM
                     CALL OPENMSG(ERRFIL,'SETUP       ',IOCHK)   
                     RETURN
                  END IF
               ELSE   
                  CLOSE (19,STATUS='DELETE')
               END IF   
               RLNGTH = (NUMCOLS*INUMLINE(ITYPE)*8) + 21
               OPEN (20,FILE=FILNAM,STATUS='OLD',ACCESS='DIRECT',
     +               FORM='UNFORMATTED',RECL=RLNGTH,IOSTAT=IOCHK)
               IF(IOCHK.NE.0) THEN
                  IF (IOCHK .NE. 6416) THEN
                     ERRFIL = FILNAM
                     CALL OPENMSG(ERRFIL,'SETUP       ',IOCHK)   
                     RETURN
                  END IF
               ELSE   
                  CLOSE (20,STATUS='DELETE')
               END IF   
               GO TO 20 
            ELSE
               GO TO 20
            END IF
         END IF
      END IF 
C
C   IF UPPER AIR DATA TYPE, SET THE ELEMENT CODES FOR DATA ENTRY
C   AND SET AREA-QC ELEMENTS TO ALL 9'S
C
      IF (DATATYPE(ITYPE).EQ.'U-A') THEN
         DO 55 I = 1,5
            AQCELEM(I) = 999
            ELEMCODE(I) = I + 300 
  55     CONTINUE
         IELEM = 6
         ELEMCODE(6) = 306
         DO 60 I = IELEM+1,MAXELEM
            ELEMCODE(I) = 999
  60     CONTINUE
C
C   ELSE - IF NOT UPPER AIR 
C
C   INITIALIZE THE FIELDS TO BE PASSED TO THE DATA-ENTRY FORM
C
      ELSE
         FIELD(MAX2ELEM-1) = DATATYPE(ITYPE)
         FIELD(MAX2ELEM) = DATASET
         DO 65 I = 1,MAX2ELEM-2
            FIELD(I) = '    '
   65    CONTINUE
         IF (OPTION.EQ.2) THEN
            DO 70 I = 1,NUMCOLS
               I2 = (I-1)*2 + 1
               WRITE(FIELD(I2),'(I3.3)') ELEMCODE(I)
   70       CONTINUE
         END IF
C
C     SOLICIT THE ELEMENT COLUMN HEADERS FROM THE SCREEN.
C
         CALL CLS
80       CONTINUE
         CALL LOCATE(0,1,IERR)
C
C     CALL GETLFRM (A COPY OF GETFRM THAT ALLOWS MORE FIELDS) 
C
         WRITE(RTNFLAG,'(A1,I1)') 'E',ITYPE
         CALL GETLFRM('SETUPELM',HELP1,FIELD,6,RTNFLAG)
         IF (RTNFLAG.EQ.'4F') THEN
            GO TO 20
         END IF
         IELEM = 0
         NEWELEM=.FALSE.
         DO 100 I = 1,MAXELEM
            I2 = (I-1)*2 + 1
            IF (FIELD(I2).NE.'   ') THEN
               IELEM = IELEM + 1
               READ(FIELD(I2),'(BN,I3)') ELEMSAV
               IF (ELEMSAV.LT.LOWELEM(ITYPE) .OR. 
     +            (ELEMSAV.GT.HIELEM(ITYPE).AND.ELEMSAV.LE.500)) THEN
                  WRITE(TEXT7,'(''-- '',I3.3)') ELEMSAV
                  CALL WRTMSG(3,179,12,1,1,TEXT7,6)
                  CALL CLRMSG(3)
                  GO TO 80
               ENDIF
               IF (ELEMSAV.NE.ELEMCODE(IELEM)) NEWELEM=.TRUE.
               ELEMCODE(IELEM) = ELEMSAV
            END IF
  100    CONTINUE
         DO 110 I = IELEM+1,MAXELEM
            ELEMCODE(I) = 999
  110    CONTINUE
C
C     IF THE NUMBER OF ELEMENTS HAS CHANGED WARN THE USER AND 
C     SET FLAG TO RE-INITIALIZE THE DATA AND INDEX FILES
C
         REPLY = 'N'
         TFVAL = .TRUE.
         IF (OPTION.EQ.2.) THEN
            IF (IELEM.NE.NUMCOLS) THEN
               CALL WRTMSG(8,261,12,1,0,' ',0)
               CALL WRTMSG(7,262,12,0,0,' ',0)
               CALL WRTMSG(6,263,12,0,0,' ',0)
            ELSE IF (NEWELEM) THEN
               CALL WRTMSG(8,296,12,1,0,' ',0)
               CALL WRTMSG(7,297,12,0,0,' ',0)
               CALL WRTMSG(6,263,12,0,0,' ',0)
               TFVAL = .FALSE.
            ELSE
               GO TO 140
            ENDIF      
  120       CONTINUE
            CALL LOCATE(19,38,IERR)
            REPLY = ' '
            CALL GETSTR(0,REPLY,1,14,0,RTNFLAG)
            IF (RTNFLAG.EQ.'4F')THEN
               GO TO 20
            END IF
            IF (REPLY.EQ.'N')THEN
               GO TO 20
            ELSE IF (REPLY.NE.'Y') THEN
               CALL WRTMSG(3,247,12,1,0,' ',0)
               GO TO 120
            END IF
         END IF 
C
         REINIT = TFVAL
C
C    SOLICT THE NUMBER OF STANDARD DEVIATIONS TO BE USED TO FLAG ERRORS
C
  140 CONTINUE
         WRITE(FIELD(1),'(F4.2,2X)') HIGHPCT 
         WRITE(FIELD(2),'(F4.2,2X)') LOWPCT 
c         CALL LOCATE(8,10,IERR)
         CALL LOCATE(5,10,IERR)
         CALL GETLFRM('SETUPSTD',HELP3,FIELD,6,RTNFLAG)
         IF (RTNFLAG.EQ.'4F') THEN
            GO TO 20
         END IF
         READ(FIELD(1),'(BN,F4.2)') HIGHPCT
         READ(FIELD(2),'(BN,F4.2)') LOWPCT

C      GET THE ELEMENTS TO BE AVAILABLE FOR THE AREA-QC ROUTINES
C
         FIELD(11) = DATATYPE(ITYPE)
         FIELD(12) = DATASET
         DO 150 I = 1,10
            FIELD(I) = '  '
  150    CONTINUE
         IF (OPTION.EQ.2) THEN
            DO 160 I = 1,5
               I2 = (I-1)*2 + 1
               IF (AQCELEM(I).NE.999) THEN
                  WRITE(FIELD(I2),'(I3.3)') AQCELEM(I)
               ELSE
                  FIELD(I2) = '  '
               END IF
  160       CONTINUE
         END  IF
C
  165    CONTINUE
         WRITE(RTNFLAG,'(A1,I1)') 'E',ITYPE
C         CALL LOCATE(17,1,IERR)
         CALL LOCATE(13,1,IERR)
         CALL GETLFRM('SETUPQCE',HELP4,FIELD,6,RTNFLAG)
         IF (RTNFLAG.EQ.'1F') THEN
            GO TO 165
         ELSE IF (RTNFLAG.EQ.'4F') THEN
            GO TO 20
         END IF 
         IQC = 0
         DO 175 I = 1,10,2 
            IF (FIELD(I).NE.' ') THEN
               IQC = IQC + 1
               READ(FIELD(I),'(BN,I3)') AQCELEM(IQC)
               ELEMFOUND = .FALSE.
               DO 170 I2 = 1,IELEM
                  IF (AQCELEM(IQC).EQ.ELEMCODE(I2)) THEN
                     ELEMFOUND = .TRUE.
                  END IF
  170          CONTINUE
               IF (.NOT.ELEMFOUND) THEN
                  CALL WRTMSG(2,136,12,1,0,FIELD(I),3)
                  GO TO 165
               END IF
            END IF
  175    CONTINUE
         DO 180 I = IQC+1,5
            AQCELEM(I) = 999
  180    CONTINUE
      END IF
C
C   WRITE THE NEW DATASET INFORMATION TO THE SETUP FILE
C
      IF (NEWFILE) THEN
         ISTRT = (JTYPE-1)*10 + 1
         DO 200 IREC = ISTRT,999
            READ(5,REC=IREC,ERR=210) DELMARK
            IF (DELMARK.EQ.'*') THEN
               GO TO 210
            END IF
  200    CONTINUE
  210    CONTINUE
      END IF
C
      DELMARK = ' '
      WRITE(5,REC=IREC) DELMARK,DATATYPE(ITYPE),DATASET,IELEM,HIGHPCT
     +    ,LOWPCT,(ELEMCODE(I1),I1=1,MAXELEM),(AQCELEM(I2),I2=1,5)
C
C   INITIALIZE THE DATA AND INDEX FILES IF NEEDED
C
      IF (NEWFILE.OR.REINIT) THEN
         OPEN (19,FILE=IDXNAM,STATUS='OLD',ACCESS='DIRECT',
     +         FORM='UNFORMATTED',RECL=25,IOSTAT=IOCHK)
         IF (IOCHK.EQ.0) THEN
            CLOSE(19,STATUS='DELETE')
         END IF
         OPEN (19,FILE=IDXNAM,STATUS='NEW',ACCESS='DIRECT',
     +         FORM='UNFORMATTED',RECL=25,IOSTAT=IOCHK)
         IF (IOCHK.NE.0) THEN
            ERRFIL = IDXNAM
            CALL OPENMSG(ERRFIL,'SETUP       ',IOCHK)   
            RETURN
         END IF   
         WRITE(19,REC=1)DELKEY,BGNIDX,NUMIDX
         CLOSE (19)
         CALL WRTMSG(6,264,14,0,0,' ',0)
         RLNGTH = (IELEM*INUMLINE(ITYPE)*8) + 21
         OPEN (20,FILE=FILNAM,STATUS='OLD',ACCESS='DIRECT',
     +         FORM='UNFORMATTED',RECL=RLNGTH,IOSTAT=IOCHK)
         IF (IOCHK.EQ.0) THEN
            CLOSE(20, STATUS='DELETE')
         END IF       
         OPEN (20,FILE=FILNAM,STATUS='NEW',ACCESS='DIRECT',
     +         FORM='UNFORMATTED',RECL=RLNGTH,IOSTAT=IOCHK)
         IF (IOCHK.NE.0) THEN
            ERRFIL = FILNAM
            CALL OPENMSG(ERRFIL,'SETUP       ',IOCHK)   
            RETURN
         END IF   
         CLOSE (20)
         CALL WRTMSG(5,265,14,0,0,' ',0)
      END IF
C      
      CALL WRTMSG(3,266,14,0,1,' ',0)
      GO TO 20
C
      END
