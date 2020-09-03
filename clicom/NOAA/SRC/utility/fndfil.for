$STORAGE:2

      SUBROUTINE FNDFIL(FORMNAME,FILENAME,NUMREC)
C
C   ROUTINE TO FIND THE FILE NAME FOR THE DATAEASE FORM NAME PASSED
C   IF NO FILE IS FOUND, A BLANK FILE NAME IS RETURNED.  THIS ROUTINE HAS BEEN 
C   MODIFIED TO WORK WITH DATAEASE VERSION 2.5 OR DATAEASE 4.0.
C  
      CHARACTER*1 INREC(51),NULL,INREC4(55)
      CHARACTER*2 INNUM
      CHARACTER*3 DEVER
      CHARACTER*4 INNUM4,CHRREC
      CHARACTER*14 INFILE
      CHARACTER*18 INFIL4
      CHARACTER*20 INFRM4
      CHARACTER*22 FILENAME
      CHARACTER*24 INFORM,FORMNAME
      INTEGER*2 HEADER,HEADR4
      INTEGER*4 NUMREC,NREC
      EQUIVALENCE (HEADER,INREC(1)),(INFORM,INREC(3))
     +           ,(INNUM,INREC(30)),(INFILE,INREC(34))

      EQUIVALENCE (HEADR4,INREC4(1)),(INFRM4,INREC4(3))
     +           ,(INNUM4,INREC4(30)),(INFIL4,INREC4(38))
     
      EQUIVALENCE (NREC,CHRREC)

C
      NULL = CHAR(0)
      
C 
C    DETERMINE WHICH VERSION OF DATAEASE IS IN USE
C
      CALL GETDEASE(DEVER)
      IF (DEVER.EQ.'4.0') THEN
         GOTO 540
      END IF
      
C *****************************************************************************
C *		 	CODE FOR DATAEASE 2.5
C *****************************************************************************

C
C   OPEN THE DATAEASE FORMS DIRECTORY AND FIND THE RECORD FOR THE FORM
C   WANTED
C
  40  CONTINUE
      OPEN (22,FILE='Q:RDRRAAAA.DBM',STATUS='OLD',FORM='BINARY'
     +   ,SHARE='DENYNONE',MODE='READ',IOSTAT=IOCHK)
      IF(IOCHK.NE.0) THEN
         CALL OPENMSG('Q:RDRRAAAA.DBM        ','FNDFIL      ',IOCHK)
         GO TO 40
      END IF   
C
      DO 60 I = 1,999
         READ(22,END=70) INREC
         IF (HEADER.EQ.14) THEN
            DO 50 J = 1,24
               IF (INFORM(J:J).EQ.NULL) THEN
                  INFORM(J:J) = ' '
               END IF
50          CONTINUE               
            IF (INFORM.EQ.FORMNAME) THEN
               FILENAME = INFILE
               CHRREC(1:2) = INNUM(1:2)
	       CHRREC(3:4) = 0
C NOW NREC = INNUM BECAUSE OF CHRREC EQUIVALENCE TO NREC
               NUMREC = NREC
               CLOSE(22)     
               RETURN
            END IF
         END IF
60    CONTINUE
C
C   THE FORM WANTED WAS NOT FOUND - PRINT AN ERROR MESSAGE AND RETURN
C
70    CONTINUE
      CLOSE(22)     
      NUMREC = 0
      FILENAME = '          '
      CALL WRTMSG(3,278,12,1,1,FORMNAME,24)
      GOTO 999
      
C *****************************************************************************
C *		 	CODE FOR DATAEASE 4.0
C *****************************************************************************

C
C   OPEN THE DATAEASE FORMS DIRECTORY AND FIND THE RECORD FOR THE FORM
C   WANTED
C
 540  CONTINUE
      OPEN (22,FILE='Q:RDRRAAAA.DBM',STATUS='OLD',FORM='BINARY'
     +   ,SHARE='DENYNONE',MODE='READ',IOSTAT=IOCHK)
      IF(IOCHK.NE.0) THEN
         CALL OPENMSG('Q:RDRRAAAA.DBM        ','FNDFIL      ',IOCHK)
         GO TO 540
      END IF   
C
      DO 560 I = 1,999
         READ(22,END=570) INREC4
         IF (HEADR4.EQ.14) THEN
            DO 550 J = 1,24
               IF (INFRM4(J:J).EQ.NULL) THEN
                  INFRM4(J:J) = ' '
               END IF
 550        CONTINUE               
            IF (INFRM4.EQ.FORMNAME(1:20)) THEN
               FILENAME = INFIL4
               NUMREC = INNUM4(1:4)
               CLOSE(22)     
               RETURN
            END IF
         END IF
 560  CONTINUE
C
C   THE FORM WANTED WAS NOT FOUND - PRINT AN ERROR MESSAGE AND RETURN
C
 570  CONTINUE
      CLOSE(22)     
      NUMREC = 0
      FILENAME = '          '
      CALL WRTMSG(3,278,12,1,1,FORMNAME,24)
C
C
 999  RETURN
      END      

      
