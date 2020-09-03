$STORAGE:2
      PROGRAM MODCONFG
C
C
C   ROUTINE TO CHECK THE USERS CONFIG.SYS FILE AND MAKE SURE THAT ALL
C     REQUIRED CLICOM ENTRIES ARE PRESENT AND UP TO SPECIFICATIONS.
C
C     CLICOM CONFIG.SYS REQUIRED COMMANDS ARE :
C
C     SHELL=COMMAND.COM /E:512 /P 
C     SHELL=COMMAND.COM /E:32 /P  (THIS LINE FOR DOS VERSION 3.1)
C     LASTDRIVE=Q
C     FCBS=35,35
C     FILES=60
C     BUFFERS=20
C     DEVICE=XXX\ANSI.SYS    (XXX IS THE DRIVE AND DIRECTORY WHERE DOS IS)
C
C----------------------------------------------------------------------
C
      CHARACTER*80 MSG
      CHARACTER*79 LINE,TEXT(200),DOSFILE
      CHARACTER*24 DOSLOC, MODMSG(10)
      CHARACTER*6 OUTFRM
      CHARACTER*4 VAL,VAL1,VAL2
      CHARACTER*1 REPLY
      INTEGER*2 SHELL,ANSI,LASTDR,FCBS,FILES,BUFFER,MAJVER,MINVER

      LOGICAL NEWFILE,MODFILE,MODFCB,OLDDOS,QUITMOD
C
      DATA SHELL,ANSI,LASTDR,FCBS,FILES,BUFFER,NUMODS /7*0/
      DATA NEWFILE,MODFILE,MODFCB,OLDDOS,QUITMOD /5*.FALSE./
C
C   INFORM THE USER OF THE CURRENT PROCESS
C
      CALL POSLIN(IR,IC)
      MSG = 'Your CONFIG.SYS file will now be checked to determine'
      IR = IR + 1
      CALL SCRNMSGI(MSG,IR,14)
      MSG = 'if it meets CLICOM requirements.'       
      IR = IR + 1
      CALL SCRNMSGI(MSG,IR,14)
      MSG = ' '
      IR = IR + 1
      CALL SCRNMSGI(MSG,IR,14)
C
C   OPEN THE EXISTING CONFIG.SYS FILE OR OPEN A NEW ONE IF THERE IS NONE
C
      OPEN (75,FILE='C:\CONFIG.SYS',STATUS='OLD',FORM='FORMATTED'
     +      ,IOSTAT=IOCHK)
      IF (IOCHK.NE.0) THEN
         NEWFILE = .TRUE.
         OPEN (75,FILE='C:\CONFIG.SYS',STATUS='NEW',FORM='FORMATTED')
      ENDIF
C
C   ASK THE USER FOR HIS VERSION OF DOS (NEEDED TO SET THE ENVIRONMENT
C   SPACE USING THE SHELL COMMAND)
C
      OLDDOS  = .FALSE.
      CALL DOSVER(MAJVER,MINVER)
      IF (MINVER.GT.10) THEN
          MINVER = NINT(FLOAT(MINVER) / 10.)
      ENDIF
      IF (MAJVER.EQ.3.AND.MINVER.LT.2) THEN
          OLDDOS = .TRUE.
      ENDIF
      IF (MAJVER.LT.3.OR.(MAJVER.EQ.3.AND.MINVER.LT.1)) THEN
         CALL POSLIN(IR,IC)
         MSG = 'CLICOM can not be installed unless you have a version'
     +        //' of DOS 3.1'
         IR = IR + 1
         CALL SCRNMSGI(MSG,IR,14)
         MSG = 'or greater.  You must purchase and install a later'
     +         //' version of DOS.'
         IR = IR + 1
         CALL SCRNMSGI(MSG,IR,14)
         MSG = 'Installation stops here...'
         IR = IR + 2
         CALL SCRNMSGI(MSG,IR,14)
         MSG = ' '
         IR = IR + 1
         CALL SCRNMSGI(MSG,IR,12)
         STOP 3
      ENDIF
************************************************************************
C   IF THERE IS NO CURRENT CONFIG.SYS FILE WRITE THE NEW ONE AS WANTED
************************************************************************
      IF (NEWFILE) THEN
         CALL POSLIN(IR,IC)
         MSG = ' '
         IR = IR + 1
         CALL SCRNMSGI(MSG,IR,14)
         MSG = 'No existing CONFIG.SYS file found - CLICOM requires a'
     +        //' CONFIG.SYS file' 
         IR = IR + 1
         CALL SCRNMSGI(MSG,IR,14)
         MSG = 'in order to set the DOS environment space.'
         IR = IR + 1
         CALL SCRNMSGI(MSG,IR,14)
         MSG = 'Do you want the CLICOM installation to create the'
     +         //' CONFIG.SYS file (Y/N) ? '
         IR = IR + 2
         CALL SCRNMSGI(MSG,IR,11)
         MSG = ' '
         IR = IR + 1
         CALL SCRNMSGI(MSG,IR,14)
         REPLY=' '
         READ (*,'(A1)') REPLY
         IF (REPLY .EQ. 'Y' .OR. REPLY .EQ.'y') THEN
            MODFILE = .TRUE.
            IF (.NOT.OLDDOS) THEN
               WRITE(75,'(A28)') 'SHELL=COMMAND.COM /E:512 /P '
            ELSE
               WRITE(75,'(A26)') 'SHELL=COMMAND.COM /E:32 /P'
            ENDIF
            WRITE(75,'(A11)') 'LASTDRIVE=Q'
            WRITE(75,'(A10)') 'FCBS=35,35'
            WRITE(75,'(A8)')  'FILES=60'
            WRITE(75,'(A10)') 'BUFFERS=20'
   60       MSG = 'Enter the drive and directory where DOS is located'
     +            //' (e.g. C:\DOS, or C:\, etc.)'
            IR = IR + 2
            CALL SCRNMSGI(MSG,IR,11)
            MSG = ' '
            IR = IR + 1
            CALL SCRNMSGI(MSG,IR,14)
            DOSLOC = '                        '
            READ (*,'(A24)') DOSLOC
            NLNG = LNG(DOSLOC)
            IF (NLNG.LT.3 .OR. DOSLOC(2:3) .NE. ':\' .OR.
     +         (DOSLOC(NLNG:NLNG) .EQ. '\' .AND. NLNG.GT.3)) THEN
               MSG = 'Your answer is not in the correct format. '
     +               //' Try again.'
               IR = IR + 1
               CALL SCRNMSGI(MSG,IR,12)
               GO TO 60
            ENDIF
            LINE = 'DEVICE=' // DOSLOC
            NLNG = LNG(LINE)
            IF (NLNG .EQ. 10) THEN
               LINE(NLNG:NLNG+8)   = '\ANSI.SYS'
            ELSE
               LINE(NLNG+1:NLNG+9) = '\ANSI.SYS'
            ENDIF
            DOSFILE = LINE(8:NLNG+9)            
            OPEN (71,FILE=DOSFILE,STATUS='OLD',IOSTAT=IOCHK)
            IF (IOCHK.EQ.6416) THEN
               MSG = 'The DOS command files were not found in: '
     +               // DOSLOC //' Try again.'
               IR = IR + 1
               CALL SCRNMSGI(MSG,IR,12)
               GO TO 60
            ENDIF
            CLOSE (71)
            CALL GETFRMT(LINE,OUTFRM)
            WRITE(75,OUTFRM) LINE            
            CLOSE (75)
            MSG =  'The CONFIG.SYS file has been created.'
            IR = IR + 1
            CALL SCRNMSGI(MSG,IR,14)
            MSG = ' '
            IR = IR + 1
            CALL SCRNMSGI(MSG,IR,14)
            STOP 1
         ELSE
            MSG =  'The CONFIG.SYS file was not created.'//
     +             ' Installation stops here...'
            IR = IR + 1
            CALL SCRNMSGI(MSG,IR,14)
            MSG = ' '
            IR = IR + 1
            CALL SCRNMSGI(MSG,IR,14)
            CLOSE(75,STATUS='DELETE')
            STOP 2
         ENDIF
************************************************************************
C   A CONFIG.SYS FILE ALREADY EXISTS  
************************************************************************
      ELSE 
C
C     COPY THE OLD FILE TO CONFIG.BK1 (CONVERT EVERYTHING TO CAPS)
C
         CALL POSLIN(IR,IC)
         OPEN (76,FILE='C:\CONFIG.BK1',STATUS='UNKNOWN'
     +         ,FORM='FORMATTED')
         DO 100 I = 1,199
            READ(75,'(A79)',END=120) LINE
            CALL LOW2UP(LINE)
            TEXT(I) = LINE
            CALL GETFRMT(LINE,OUTFRM)
            WRITE(76,OUTFRM) LINE            
  100    CONTINUE
  120    CONTINUE
         NBRLINE = I -1
         REWIND(75)
         REWIND(76)
C
C     CHECK THE EXISTING LINES ONE BY ONE AND LOOK FOR THE COMMANDS OF
C     INTEREST (SHELL,ANSI,LASTDRIVE,FCBS,FILES,BUFFERS,REM)
C     IF THE COMMANDS ARE FOUND SET THE RESPECTIVE VARIABLE TO THE LINE
C     NUMBER - ELSE IT WILL EQUAL 0.  IF A REM IS FOUND, SKIP THE SEARCH FOR
C     OTHER COMMANDS.  JUST ECHO THE REM LINE TO THE NEW FILE
C
         NBREM = 0
         DO 150 I = 1,199
            LINE = TEXT(I)
            CALL SEARCH(I,LINE,'REM     ',NBREM,J)
            IF (NBREM .EQ. 0 ) THEN
               J = 0
               CALL SEARCH(I,LINE,'SHELL   ',SHELL,J)
               CALL SEARCH(I,LINE,'ANSI.SYS',ANSI,J)
               CALL SEARCH(I,LINE,'LASTDRIV',LASTDR,J)
               CALL SEARCH(I,LINE,'FCBS    ',FCBS,J)
               CALL SEARCH(I,LINE,'FILES   ',FILES,J)
               CALL SEARCH(I,LINE,'BUFFERS ',BUFFER,J)
            ELSE
               NBREM = 0
            ENDIF
  150    CONTINUE
C
C     ADD ANY REQUIRED COMMANDS THAT ARE MISSING FROM THE CONFIG.SYS FILE
C
         IF (SHELL.EQ.0) THEN
            NBRLINE = NBRLINE + 1
            MODFILE = .TRUE.
C            NLNG = LNG(MODMSG)
C            MODMSG(NLNG+1:NLNG+7) = ' SHELL,'
            NUMODS = NUMODS + 1
            MODMSG(NUMODS) = ' SHELL'
            IF (.NOT.OLDDOS) THEN
               TEXT(NBRLINE) =  'SHELL=COMMAND.COM /E:512 /P '
            ELSE
               TEXT(NBRLINE) =  'SHELL=COMMAND.COM /E:32 /P  '
            ENDIF
         ENDIF
         IF (LASTDR.EQ.0) THEN
            NBRLINE = NBRLINE + 1
            MODFILE = .TRUE.
C            NLNG = LNG(MODMSG)
C            MODMSG(NLNG+1:NLNG+11) = ' LASTDRIVE,'
            NUMODS = NUMODS + 1
            MODMSG(NUMODS) = ' LASTDRIVE'
            TEXT(NBRLINE) = 'LASTDRIVE=Q'
         ENDIF
         IF (ANSI.EQ.0) THEN
            NBRLINE = NBRLINE + 1
            MODFILE = .TRUE.
C            NLNG = LNG(MODMSG)
C            MODMSG(NLNG+1:NLNG+10) = ' ANSI.SYS,'
            NUMODS = NUMODS + 1
            MODMSG(NUMODS) = ' ANSI.SYS'
C
C---------- BUILD THE DEVICE=d:\directory\ANSI.SYS COMMAND     
C
  160       MSG = 'Enter the drive and directory where DOS is located'
     +            //' (e.g. C:\DOS, or C:\, etc.)'
            IR = IR + 2
            CALL SCRNMSGI(MSG,IR,11)
            MSG = ' '
            IR = IR + 1
            CALL SCRNMSGI(MSG,IR,14)
            DOSLOC = '                        '
            READ (*,'(A24)') DOSLOC
            NLNG = LNG(DOSLOC)
            IF (NLNG.LT.3 .OR. DOSLOC(2:3) .NE. ':\' .OR.
     +         (DOSLOC(NLNG:NLNG) .EQ. '\' .AND. NLNG.GT.3)) THEN
               MSG = 'Your answer is not in the correct format. '
     +               //' Try again.'
               IR = IR + 1
               CALL SCRNMSGI(MSG,IR,12)
               GO TO 160
            ENDIF
            LINE = 'DEVICE=' // DOSLOC
            NLNG = LNG(LINE)
            IF (NLNG .EQ. 10) THEN
               LINE(NLNG:NLNG+8)   = '\ANSI.SYS'
            ELSE
               LINE(NLNG+1:NLNG+9) = '\ANSI.SYS'
            ENDIF
            DOSFILE = LINE(8:NLNG+9)            
            OPEN (71,FILE=DOSFILE,STATUS='OLD',IOSTAT=IOCHK)
            IF (IOCHK.EQ.6416) THEN
               MSG = 'The DOS command files were not found in: '
     +               // DOSLOC //' Try again.'
               IR = IR + 1
               CALL SCRNMSGI(MSG,IR,12)
               GO TO 160
            ENDIF
            CLOSE (71)
            TEXT(NBRLINE) = LINE
         ENDIF
         IF (FCBS.EQ.0) THEN
            NBRLINE = NBRLINE + 1
            MODFILE = .TRUE.
C            NLNG = LNG(MODMSG)
C            MODMSG(NLNG+1:NLNG+6) = ' FCBS,'
            NUMODS = NUMODS + 1
            MODMSG(NUMODS) = ' FCBS'
            TEXT(NBRLINE) = 'FCBS=35,35 '
         ENDIF
         IF (FILES.EQ.0) THEN
            NBRLINE = NBRLINE + 1
            MODFILE = .TRUE.
C            NLNG = LNG(MODMSG)
C            MODMSG(NLNG+1:NLNG+7) = ' FILES,'
            NUMODS = NUMODS + 1
            MODMSG(NUMODS) = ' FILES'
            TEXT(NBRLINE) = 'FILES=60'   
         ENDIF
         IF (BUFFER.EQ.0) THEN
            NBRLINE = NBRLINE + 1
            MODFILE = .TRUE.
C            NLNG = LNG(MODMSG)
C            MODMSG(NLNG+1:NLNG+9) = ' BUFFERS,'
            NUMODS = NUMODS + 1
            MODMSG(NUMODS) = ' BUFFERS'
            TEXT(NBRLINE) = 'BUFFERS=20 '
         ENDIF
C
C     CHECK THE EXISTING CONFIG.SYS FILE (STORED IN ARRAY TEXT) FOR 
C     MATCHING ENTRIES.  UPDATE AS NECESSARY
C 
         DO 270 I = 1,199
            LINE = TEXT(I)
            IF (I.EQ.SHELL) THEN
               I1 = 0
               CALL SEARCH(I,LINE,'COMMAND ',I1,J)
               IF (I1.NE.I) THEN
                  MSG = ' '
                  IR = IR + 1
                  CALL SCRNMSGI(MSG,IR,14)
                  MSG = 'The existing CONFIG.SYS file contains a non-'
     +                //'DOS command processor.'
                  IR = IR + 1
                  CALL SCRNMSGI(MSG,IR,14)
                  MSG = 'The SHELL command used to increase environment' 
     +                //' space can not be used.'
                  IR = IR + 1
                  CALL SCRNMSGI(MSG,IR,14)
                  MSG = ' '
                  IR = IR + 1
                  CALL SCRNMSGI(MSG,IR,14)
                  MSG = '       '//LINE
                  IR = IR + 1
                  CALL SCRNMSGI(MSG,IR,14)
                  MSG = ' '
                  IR = IR + 1
                  CALL SCRNMSGI(MSG,IR,14)
                  MSG = 'Does this command processor currently allocate'
     +                  //' 512 bytes of DOS'
                  IR = IR + 1
                  CALL SCRNMSGI(MSG,IR,11)
                  MSG = 'environment space (Y/N) ?'
                  IR = IR + 1
                  CALL SCRNMSGI(MSG,IR,11)
                  MSG = ' '
                  IR = IR + 1
                  CALL SCRNMSGI(MSG,IR,14)
                  REPLY=' '
                  READ (*,'(A1)') REPLY
                  IF (REPLY .EQ. 'Y' .OR. REPLY .EQ.'y') THEN
                     GO TO 270
                  ELSE
                     QUITMOD = .TRUE.
                     MSG = 'See your computer manual for the means to'
     +                 //' increase the environment to 512 bytes.'
                    IR = IR + 1
                    CALL SCRNMSGI(MSG,IR,14)
                  ENDIF
               ELSE
                  J = 0
                  CALL SEARCH(I,LINE,'/E:     ',I1,J)
                  IF (J .GT. 0) THEN
                     J = J + 3
                     CALL VALUE (LINE,J,VAL)
                     READ(VAL,'(BN,I3)') IENV
                  ELSE
                     IENV = 0
                  ENDIF
                  IF (.NOT.OLDDOS) THEN
                     IF (IENV.LT.512) THEN
                        MODFILE = .TRUE.
C                        NLNG = LNG(MODMSG)
C                        MODMSG(NLNG+1:NLNG+7) = ' SHELL,'
                        NUMODS = NUMODS + 1
                        MODMSG(NUMODS) = ' SHELL'
                        TEXT(I) =  'SHELL=COMMAND.COM /E:512 /P '
                     ENDIF
                  ELSE
                     IF (IENV.LT.32) THEN
                        MODFILE = .TRUE.
C                        NLNG = LNG(MODMSG)
C                        MODMSG(NLNG+1:NLNG+7) = ' SHELL,'
                        NUMODS = NUMODS + 1
                        MODMSG(NUMODS) = ' SHELL'
                        TEXT(I) =  'SHELL=COMMAND.COM /E:32 /P  '
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
C-------- LASTDRIVE?
            IF (I.EQ.LASTDR) THEN
               J = 10
               CALL VALUE(LINE,J,VAL)
               IF (VAL(1:1).LT.'Q'.OR.
     +            (VAL(1:1).GE.'a'.AND.VAL(1:1).LE.'p')) THEN
                  MODFILE = .TRUE.
C                  NLNG = LNG(MODMSG)
C                  MODMSG(NLNG+1:NLNG+11) = ' LASTDRIVE,'
                  NUMODS = NUMODS + 1
                  MODMSG(NUMODS) = ' LASTDRIVE'
                  TEXT(I) = 'LASTDRIVE=Q'
               ENDIF
            ENDIF
C-------- FCBS?
            IF (I.EQ.FCBS) THEN
               MODFCB = .FALSE.
               J = 5
               CALL VALUE(LINE,J,VAL1) 
               CALL VALUE(LINE,J,VAL2)
               READ(VAL1,'(BN,I3)') IVAL1
               IF (IVAL1.LT.35)  THEN
                  MODFCB = .TRUE.
                  IVAL1 = 35 
               ENDIF
               READ(VAL2,'(BN,I3)') IVAL2
               IF (IVAL2.LT.35) THEN
                  MODFCB = .TRUE.
                  IVAL2 = 35 
               ENDIF
               IF (MODFCB) THEN
                  MODFILE = .TRUE.
C                  NLNG = LNG(MODMSG)
C                  MODMSG(NLNG+1:NLNG+6) = ' FCBS,'
                  NUMODS = NUMODS + 1
                  MODMSG(NUMODS) = ' FCBS'
                  IF (IVAL1.LE.99.AND.IVAL2.LE.99) THEN
                     WRITE(TEXT(I),'(A5,I2,A1,I2)') 'FCBS=',IVAL1,
     +               ',',IVAL2
                  ELSE IF (IVAL1.GT.99.AND.IVAL2.GT.99) THEN
                     WRITE(TEXT(I),'(A5,I3,A1,I3)') 'FCBS=',IVAL1,
     +               ',',IVAL2
                  ELSE IF (IVAL1.GT.99.AND.IVAL2.LE.99) THEN
                     WRITE(TEXT(I),'(A5,I3,A1,I2)') 'FCBS=',IVAL1,
     +               ',',IVAL2
                  ELSE IF (IVAL1.LE.99.AND.IVAL2.GT.99) THEN
                     WRITE(TEXT(I),'(A5,I2,A1,I3)') 'FCBS=',IVAL1,
     +               ',',IVAL2
                  ENDIF
               ENDIF
C-------- FILES?
            ELSE IF (I.EQ.FILES) THEN 
               J = 6
               CALL VALUE(LINE,J,VAL)
               READ(VAL,'(I3)') IVAL
               IF (IVAL.LT.60) THEN
                  MODFILE = .TRUE.
C                  NLNG = LNG(MODMSG)
C                  MODMSG(NLNG+1:NLNG+7) = ' FILES,'
                  NUMODS = NUMODS + 1
                  MODMSG(NUMODS) = ' FILES'
                  TEXT(I) = 'FILES=60'
               ENDIF
C-------- BUFFERS?
            ELSE IF (I.EQ.BUFFER) THEN 
               J = 8
               CALL VALUE(LINE,J,VAL)
               READ(VAL,'(I3)') IVAL
               IF (IVAL.LT.20) THEN
                  MODFILE = .TRUE.
C                  NLNG = LNG(MODMSG)
C                  MODMSG(NLNG+1:NLNG+9) = ' BUFFERS,'
                  NUMODS = NUMODS + 1
                  MODMSG(NUMODS) = ' BUFFERS'
                  TEXT(I) = 'BUFFERS=20'
               ENDIF
            ENDIF
  270    CONTINUE
         CALL POSLIN(IR,IC)
         MSG = ' '
         IR = IR + 1
         CALL SCRNMSGI(MSG,IR,14)
         IF (MODFILE) THEN
            CALL POSLIN(IR,IC)
            MSG = 'The following commands of the CONFIG.SYS file must'
            IR = IR + 1
            CALL SCRNMSGI(MSG,IR,14)
            MSG =  'be added or modified for CLICOM to work properly.'
            IR = IR + 1
            CALL SCRNMSGI(MSG,IR,14)
            IR = IR + 1
C            NLNG = LNG(MODMSG)
C            MODMSG(NLNG:NLNG) = ' '
            DO 280 KK=1,NUMODS
               IR = IR + 1
               CALL SCRNMSGI(MODMSG(KK),IR,14)
  280       CONTINUE
            MSG = ' '
            IR = IR + 1
            CALL SCRNMSGI(MSG,IR,14)
            MSG = 'Do you want the CLICOM installation to modify the'
     +            //' CONFIG.SYS file (Y/N) ? '
            IR = IR + 1
            CALL SCRNMSGI(MSG,IR,11)
            MSG = ' '
            IR = IR + 1
            CALL SCRNMSGI(MSG,IR,14)
            REPLY=' '
            READ (*,'(A1)') REPLY
            IF (REPLY .EQ. 'Y' .OR. REPLY .EQ.'y') THEN
               DO 300 I = 1,NBRLINE
                  LINE = TEXT(I)
                  CALL GETFRMT(LINE,OUTFRM)
                  WRITE(75,OUTFRM) LINE            
  300          CONTINUE
               CLOSE(75)               
               CLOSE(76)               
               MSG =  ' '
               IR = IR + 2
               CALL SCRNMSGI(MSG,IR,14)
               MSG =  'The modifications to the CONFIG.SYS file '//
     +                'are complete.'
               IR = IR + 1
               CALL SCRNMSGI(MSG,IR,14)
               MSG = ' '
               IR = IR + 1
               CALL SCRNMSGI(MSG,IR,14)
               STOP 1
            ELSE
               MSG =  'You must modify your CONFIG.SYS file before '//
     +                'CLICOM can be installed.'
               IR = IR + 1
               CALL SCRNMSGI(MSG,IR,14)
               MSG = ' '
               IR = IR + 1
               CALL SCRNMSGI(MSG,IR,14)
               CLOSE(76,STATUS='DELETE')
               STOP 2
            ENDIF
         ELSE
            CLOSE(76,STATUS='DELETE')
            IF (QUITMOD) THEN
C ---------- CONFIG.SYS FILE OK EXCEPT FOR NON-DOS SHELL COMMAND
               CALL POSLIN(IR,IC)
               MSG = 'No other modifications to CONFIG.SYS are '//
     +               'necessary.'
               IR = IR + 1
               CALL SCRNMSGI(MSG,IR,14)
               MSG = ' '
               IR = IR + 1
               CALL SCRNMSGI(MSG,IR,14)
               STOP 2
            ELSE
               CALL POSLIN(IR,IC)
               MSG = 'No modifications to CONFIG.SYS are necessary.'
               IR = IR + 1
               CALL SCRNMSGI(MSG,IR,14)
               MSG = ' '
               IR = IR + 1
               CALL SCRNMSGI(MSG,IR,14)
               STOP ' '
            ENDIF
         ENDIF
      ENDIF
      END
$PAGE
************************************************************************
      SUBROUTINE SEARCH(I,LINE,TEXT,I1,JCOL)
C
C   ROUTINE TO SEARCH A LINE FOR MATCHING TEXT 
C
C   I.......FILE LINE NUMBER BEING SEARCHED
C   LINE....LINE OF TEXT TO BE SEARCHED (CHAR*79)
C   TEXT....TEXT SEARCH STRING TO BE MATCHED (CHAR*8) LEFT-JUSTIFIED
C   I1......SET = TO I ON OUTPUT IF MATCH IS FOUND
C   JCOL....SET TO THE COLUMN THE MATCHING TEXT BEGINS IF FOUND
C   
      CHARACTER*80 MSG
      CHARACTER*79 LINE
      CHARACTER*8 TEXT
      CHARACTER*1 PRVCHR
C
C   FIRST DETERMINE HOW LONG THE TEXT STRING IS (SEARCH BACK FROM RIGHT
C   FOR FIRST NON-BLANK CHARACTER
C
      LEN = 0
      DO 100 J = 8,1,-1
         IF (TEXT(J:J).NE.' ') THEN
            LEN = J
            GO TO 110
         ENDIF
  100 CONTINUE
  110 CONTINUE
      IF (LEN.EQ.0) THEN
            CALL POSLIN(IR,IC)
            MSG = 'TEXT STRING IN SEARCH IS EMPTY.'
            IR = IR + 1
            CALL SCRNMSGI(MSG,IR,14)
         RETURN
      ENDIF
C
C   SCAN THE INPUT LINE FOR A MATCH WITH THE TEXT WANTED, IF MATCH 
C   SET I1 AND JCOL.  THE DEVICE=ANSI.SYS OR SHELL=COMMAND.COM 
C   STATEMENTS MUST BE EXACTLY ANSI.SYS OR SHELL=COMMAND.COM
C   AFTER THE =, :, OR \ SYMBOL FOR CLICOM TO WORK.
C
      DO 120 J = 1,79-LEN+1
         IF (LINE(J:J+LEN-1).EQ.TEXT(1:LEN)) THEN
            IF (TEXT(1:LEN).EQ.'ANSI'.OR.TEXT(1:LEN).EQ.'COMMAND') THEN
               PRVCHR = LINE(J-1:J-1)
               IF (PRVCHR.NE.' '.AND.PRVCHR.NE.'='.AND.PRVCHR.NE.':'
     +             .AND.PRVCHR.NE.'\') THEN
                   RETURN
               ENDIF
            ENDIF   
            I1 = I
            JCOL = J
            RETURN
         ENDIF
  120 CONTINUE
      RETURN
      END
************************************************************************
      SUBROUTINE VALUE (LINE,JCOL,VAL)
C
C   ROUTINE TO FIND THE NEXT DATA VALUE WITHIN "LINE" STARTTING FROM
C   POSITION JCOL AND RETURN THE VALUE IN VAL.  IT ASSUMES "=", ":",
C   " ", AND "," ARE DELIMETERS.
C
C   LINE....LINE OF TEXT TO BE SEARCHED (CHAR*79)
C   JCOL....COLUMN TO BEGIN THE SEARCH
C   VAL.....TEXT VALUE LOCATED (CHAR*4)
C   
      CHARACTER*80 MSG
      CHARACTER*79 LINE
      CHARACTER*4  VAL
      CHARACTER*1  TEXT
C
      DO 100 ICOL = JCOL,79
         TEXT = LINE(ICOL:ICOL)
         IF (TEXT.NE.' '.AND.TEXT.NE.'='.AND.TEXT.NE.':'.AND.
     +         TEXT.NE.',') THEN
            ISTRT = ICOL
            GO TO 150  
         ENDIF
100   CONTINUE
      CALL POSLIN(IR,IC)
      MSG = 'No value found in '//LINE
      IR = IR + 2
      CALL SCRNMSGI(MSG,IR,14)
      JCOL = 0
      RETURN
C
C   BEGINNING OF VALUE FOUND - FIND END AND SET VAL (MAX LENGTH 4 CHRS)
C
150   CONTINUE
      IEND = ISTRT + 3
      DO 160 ICOL = ISTRT,ISTRT+3
         TEXT = LINE(ICOL:ICOL)
         IF (TEXT.EQ.' '.OR.TEXT.EQ.'='.OR.TEXT.EQ.':'.OR.
     +         TEXT.EQ.',') THEN
            IEND = ICOL - 1
            GO TO 170
         ENDIF
160   CONTINUE
170   CONTINUE
      JCOL = IEND + 1
      VAL = LINE(ISTRT:IEND)
      RETURN
      END
