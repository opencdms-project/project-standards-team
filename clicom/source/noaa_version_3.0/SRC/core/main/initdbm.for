$STORAGE:2
      PROGRAM INITDBM
C
C       ROUTINE TO CREATE THE ZERO LENGTH DATAEASE DBM FILES WHICH ARE NOT
C       TRANSFERED BY THE DOS COPY COMMAND AT INSTALLATION TIME.  THIS PROGRAM
C       HAS BEEN MODIFIED TO USE EITHER DATAEASE VERSION 2.5 OR DATAEASE 4.0.
C
      CHARACTER*1 INREC(51),NULL,INREC4(55)
      CHARACTER*2 INNUM,INDEL,RDISK
      CHARACTER*3 DEVER
      CHARACTER*4 INNUM4,INDEL4,CHRREC,CHRDEL
      CHARACTER*14 DBMFILE,DB4FIL
      CHARACTER*16 DDISK
      CHARACTER*32 INFORM,NEWFILE,RDRFILE,INFRM4
      INTEGER*2 HEADER,HEADR4
      INTEGER*4 INUM,IDEL
      EQUIVALENCE (HEADER,INREC(1)),(INFORM,INREC(3))
     +           ,(INNUM,INREC(30)),(INDEL,INREC(32))
     +           ,(DBMFILE,INREC(34))

      EQUIVALENCE (HEADR4,INREC4(1)),(INFRM4,INREC4(3))
     +           ,(INNUM4,INREC4(30)),(INDEL4,INREC4(34)),
     +            (DB4FIL,INREC4(38))

      EQUIVALENCE (CHRREC,INUM),(CHRDEL,IDEL)
C
      NULL = CHAR(0)
      LEND =16
      CALL GETDSC(RDISK,DDISK,LEND)

C
C       DETERMINE THE VERSION OF DATAEASE CURRENTLY IN USE
C
      CALL GETDEASE(DEVER)
      IF (DEVER.EQ.'4.0') THEN
          GOTO 540
      END IF
      
C *****************************************************************************
C *             CODE FOR DATAEASE VERSION 2.5
C *****************************************************************************

C
C   OPEN THE DATAEASE FORMS DIRECTORY
C
  40  CONTINUE

       RDRFILE(1:LEND) = DDISK
       ILEN = LEND + 13
       RDRFILE (LEND+1:ILEN)='\RDRRAAAA.DBM'

      OPEN (22,FILE=RDRFILE,STATUS='OLD',FORM='BINARY'
     +   ,SHARE='DENYWR',MODE='READ',IOSTAT=IOCHK)
      IF(IOCHK.NE.0) THEN
         WRITE(*,*) 'CLICOM DataEase files not found'
         PAUSE 
      END IF   
C
C   FOR EACH OF THE FORMS WITH ZERO RECORDS - CREATE THE FILE
C
      NFILE = 0
      DO 60 I = 1,999
         READ(22,END=70) INREC
         IF (HEADER.EQ.14) THEN
            CHRREC(1:2) = INNUM(1:2)
            CHRREC(3:4) = 0
            CHRDEL(1:2) = INDEL(1:2)
            CHRDEL(3:4) = 0
            IF (INUM.EQ.0.AND.IDEL.EQ.0) THEN

               NEWFILE(1:LEND) = DDISK
               ILEN = LEND + 13
               NEWFILE(LEND+1:LEND+1) = '\'
               NEWFILE(LEND+2:ILEN) = DBMFILE(3:14)

               WRITE(*,*) NEWFILE
               OPEN (25,FILE=NEWFILE,STATUS='UNKNOWN',FORM='BINARY')
               CLOSE(25)
               NFILE = NFILE + 1
            END IF
         END IF
60    CONTINUE
70    CONTINUE
      GOTO 900

C *****************************************************************************
C *             CODE FOR DATAEASE VERSION 4.0
C *****************************************************************************

C
C   OPEN THE DATAEASE FORMS DIRECTORY
C
 540  CONTINUE

       RDRFILE(1:LEND) = DDISK
       ILEN = LEND + 13
       RDRFILE(LEND+1:ILEN)='\RDRRAAAA.DBM'

      OPEN (22,FILE=RDRFILE,STATUS='OLD',FORM='BINARY'
     +   ,SHARE='DENYWR',MODE='READ',IOSTAT=IOCHK)
      IF(IOCHK.NE.0) THEN
         WRITE(*,*) 'CLICOM DataEase files not found'
         PAUSE 
      END IF   
C
C   FOR EACH OF THE FORMS WITH ZERO RECORDS - CREATE THE FILE
C

      NFILE = 0
      DO 560 I = 1,999
         READ(22,END=570) INREC4 
         IF (HEADR4.EQ.14) THEN
            INUM = INNUM4(1:4)
            IDEL = INDEL4(1:4)
            IF (INUM.EQ.0.AND.IDEL.EQ.0) THEN

               NEWFILE(1:LEND) = DDISK
               ILEN = LEND + 13         
               NEWFILE(LEND+1:LEND+1) = '\'
               NEWFILE(LEND+2:ILEN) = DB4FIL(3:14)

               WRITE(*,*) NEWFILE
               OPEN (25,FILE=NEWFILE,STATUS='UNKNOWN',FORM='BINARY')
               CLOSE(25)
               NFILE = NFILE + 1
            END IF
         END IF
560    CONTINUE
570    CONTINUE

900   WRITE(*,*)NFILE,' DBM files created. '
      STOP ' '
      END
