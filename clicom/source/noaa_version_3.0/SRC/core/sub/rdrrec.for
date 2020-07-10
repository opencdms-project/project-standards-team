$STORAGE:2

      SUBROUTINE RDRREC(RECTYPE,RECNUM)
C
C   ROUTINE TO FIND THE RECORD OF THE DATAEASE FILE DIRECTORY
C   (RDRRAAAA.DBM )FOR THE RECTYPE INDICATED AND RETURN IT AS RECNUM. 
C   RECNUM = 0 IF NOT FOUND.  THIS ROUTINE HAS BEEN MODIFIED TO WORK WITH
C   EITHER DATAEASE VERSION 2.5 OR DATAEASE 4.0
C
C   THE FILE IS OPENED AS FILE 22.  
C
      CHARACTER*1 INREC(51),NULL,INREC4(55)
      CHARACTER*3 RECTYPE,DEVERS
      CHARACTER*20 INFRM4
      CHARACTER*24 INFORM,FORMNAME(11),HLDFORM
      INTEGER*2   HEADER,HEADR4,RECNUM

      EQUIVALENCE (HEADER,INREC(1)),(INFORM,INREC(3))

      EQUIVALENCE (HEADR4,INREC4(1)),(INFRM4,INREC4(3))
C
      DATA FORMNAME /'MONTHLY DATA','TEN DAY DATA','DAILY DATA',
     +               'SYNOPTIC DATA','HOURLY DATA',
     +               'FIFTEEN MINUTE DATA','UPPER-AIR DATA',
     +              'DATASET DIRECTORY','Daily Means/Extremes',
     +              'Means/Extremes (10D)','NORMALS'/
C
      NULL = CHAR(0)
C
      IF (RECTYPE.EQ.'MLY'.OR.RECTYPE.EQ.'mly') THEN
         ITYPE = 1
      ELSE IF (RECTYPE.EQ.'10D'.OR.RECTYPE.EQ.'10d') THEN
         ITYPE = 2
      ELSE IF (RECTYPE.EQ.'DLY'.OR.RECTYPE.EQ.'dly') THEN
         ITYPE = 3
      ELSE IF (RECTYPE.EQ.'SYN'.OR.RECTYPE.EQ.'syn') THEN
         ITYPE = 4
      ELSE IF (RECTYPE.EQ.'HLY'.OR.RECTYPE.EQ.'hly') THEN
         ITYPE = 5
      ELSE IF (RECTYPE.EQ.'15M'.OR.RECTYPE.EQ.'15m') THEN
         ITYPE = 6
      ELSE IF (RECTYPE.EQ.'U-A'.OR.RECTYPE.EQ.'u-a') THEN
         ITYPE = 7
      ELSE IF (RECTYPE.EQ.'DDS'.OR.RECTYPE.EQ.'dds') THEN
         ITYPE = 8
      ELSE IF (RECTYPE.EQ.'DLX'.OR.RECTYPE.EQ.'dlx') THEN
         ITYPE = 9
      ELSE IF (RECTYPE.EQ.'10X'.OR.RECTYPE.EQ.'10x') THEN
         ITYPE = 10
      ELSE IF (RECTYPE.EQ.'NML'.OR.RECTYPE.EQ.'nml') THEN
         ITYPE = 11
      ELSE 
         CALL WRTMSG(4,172,12,1,1,' ',0)
         STOP 2
      END IF

C
C  DETERMINE WHICH VERSION OF DATAEASE IN USE
C
      CALL GETDEASE(DEVERS)
      IF (DEVERS.EQ.'4.0') THEN
          GOTO 520
      END IF

C*****************************************************************************
C       		 CODE FOR DATAEASE VERSION 2.5                       *
C*****************************************************************************

C
C   OPEN THE RDRRAAAA.DBM FILE
C
  20  CONTINUE
      OPEN (22,FILE='Q:RDRRAAAA.DBM',STATUS='OLD',FORM='BINARY'
     +    ,ACCESS='DIRECT',RECL=51,IOSTAT=IOCHK)
      IF(IOCHK.NE.0) THEN
         CALL OPENMSG('Q:RDRRAAAA.DBM      ','RDRREC      ',IOCHK)
         GO TO 20
      END IF   
C
C   FIND THE RDRRAAAA ENTRY FOR THE RECTYPE SPECIFIED 
C
      DO 100 I = 1,999
         READ(22,REC=I,ERR=130) INREC
         IF (HEADER.EQ.14) THEN
            HLDFORM = INFORM
            DO 40 J = 1,24
               IF (HLDFORM(J:J).EQ.NULL) THEN
                  HLDFORM(J:J) = ' '
               END IF
   40       CONTINUE               
            IF (HLDFORM.EQ.FORMNAME(ITYPE)) THEN
               RECNUM = I
               RETURN
            END IF
         END IF
  100 CONTINUE
C
C   ERROR - DATAEASE FORM NAME COULD NOT BE FOUND
C
  130 CONTINUE
      CLOSE (22)
      CALL WRTMSG(2,169,12,1,1,FORMNAME(ITYPE),24)
      RECNUM = 0
      GOTO 999

C*****************************************************************************
C       		 CODE FOR DATAEASE VERSION 4.0                       *
C*****************************************************************************

C
C   OPEN THE RDRRAAAA.DBM FILE
C
 520  CONTINUE
      OPEN (22,FILE='Q:RDRRAAAA.DBM',STATUS='OLD',FORM='BINARY'
     +    ,ACCESS='DIRECT',RECL=55,IOSTAT=IOCHK)
      IF(IOCHK.NE.0) THEN
         CALL OPENMSG('Q:RDRRAAAA.DBM      ','RDRREC      ',IOCHK)
         GO TO 520
      END IF   
C
C   FIND THE RDRRAAAA ENTRY FOR THE RECTYPE SPECIFIED 
C
      DO 5100 I = 1,999
         READ(22,REC=I,ERR=5130) INREC4 
         IF (HEADR4.EQ.14) THEN
            HLDFORM = INFRM4 
            DO 540 J = 1,24
               IF (HLDFORM(J:J).EQ.NULL) THEN
                  HLDFORM(J:J) = ' '
               END IF
  540       CONTINUE               
            IF (HLDFORM.EQ.FORMNAME(ITYPE)) THEN
               RECNUM = I
               RETURN
            END IF
         END IF
 5100 CONTINUE
C
C   ERROR - DATAEASE FORM NAME COULD NOT BE FOUND
C
 5130 CONTINUE
      CLOSE (22)
      CALL WRTMSG(2,169,12,1,1,FORMNAME(ITYPE),24)
C
C
      RETURN
  999 END
