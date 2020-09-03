$STORAGE:2
C
C     PROGRAM RVSNINFO
C
C     ** OBJECTIVE:  THE PUPOSE OF THIS PROGRAM IS CONSTRUCTED A CALLRVSN.BAT 
C                    FILE BASE ON THE RESPONSES OF THE USER.  
C
      PROGRAM RVSNINFO
C
      CHARACTER*1  CLICPRDR,CLIGRFDR,DETYPE,BLANK,DEDRV
      CHARACTER*2  INCHAR
      CHARACTER*3  DEVERS
      CHARACTER*12 DOSDR,DEDIR
      CHARACTER*20 DEDATADR
      CHARACTER*80 OUTFRMT,MSG,LINE
      DATA BLANK /'   '/
C
      OPEN(51,FILE='C:\CALLRVSN.BAT',STATUS='UNKNOWN',FORM=
     +    'FORMATTED')
C
      CLICPRDR = BLANK
      CLIGRFDR = BLANK
      DEDATADR = BLANK
      DEDRV    = BLANK
      DEDIR    = BLANK
      DEVERS   = BLANK
      DETYPE   = BLANK
      DOSDR    = BLANK
C
 20   CONTINUE
      CALL CLS
      CALL LOCATE(1,0,IERR)
      CALL POSLIN(IR,IC)
      MSG = '    Enter the letter of the drive where the '
     +          //'CLICOM software is installed.'
      CALL SCRNMSGI(MSG,IR,14)
      CALL LOCATE(2,4,IERR)
      CALL GETSTR(0,CLICPRDR,1,15,1,RTNFLAG)
      IF (CLICPRDR.EQ.BLANK) THEN
         CALL BEEP
         GO TO 20
      END IF
 30   CONTINUE
      CALL LOCATE(3,0,IERR)
      IR = 3      
      MSG = '    Enter the letter of the drive that contains the '
     +         //'CLIGRAF directory.'
      CALL SCRNMSGI(MSG,IR,14)
      CALL LOCATE(4,4,IERR)
      CALL GETSTR(0,CLIGRFDR,1,15,1,RTNFLAG)
      IF (CLIGRFDR.EQ.BLANK) THEN
         CALL BEEP
         GO TO 30
      END IF
      CALL LOCATE(5,0,IERR)
      IR = 5
      MSG = '    Enter the drive letter and the directory name'
     +         //' that contain the DataEase '
      CALL SCRNMSGI(MSG,IR,14)
      MSG = '    program files. '
      IR = IR + 1
      CALL SCRNMSGI(MSG,IR,14)
 40   CONTINUE
      IR = 7
      MSG = '           First enter the drive:'
      CALL SCRNMSGI(MSG,IR,14)
      CALL LOCATE(8,11,IERR)
      CALL GETSTR(0,DEDRV,1,15,1,RTNFLAG)
      IF (DEDRV.EQ.BLANK) THEN
         CALL BEEP
         GO TO 40
      END IF
  50  CONTINUE
      IR = 9
      MSG = '           Now enter the directory name:     (e.g.  DE)'
      CALL SCRNMSGI(MSG,IR,14)
      CALL LOCATE(10,11,IERR)
      CALL GETSTR(0,DEDIR,12,15,1,RTNFLAG)
      IF (DEDIR.EQ.BLANK) THEN
         CALL BEEP
         GO TO 50
      END IF
  60  CONTINUE
      CALL LOCATE(11,0,IERR)
      IR = 11
      MSG = '    Enter the DataEase version number. '
     +         //'  (e.g.  2.5, 4.0, 4.2, or 4.5)'
      CALL SCRNMSGI(MSG,IR,14)
      CALL LOCATE(12,4,IERR)
      CALL GETSTR(0,DEVERS,3,15,1,RTNFLAG)
      IF (DEVERS.EQ.BLANK) THEN
         CALL BEEP
         GO TO 60
      END IF
  70  CONTINUE
      CALL LOCATE(13,0,IERR)
      IR = 13
      MSG = '    If you have DataEase version 4.2 or 4.5, do you'
     +      //' have the regular '
      CALL SCRNMSGI(MSG,IR,14)
      MSG = '    (640K version) or the extended memory version '
     +      //'installed ?'
      IR = IR + 1
      CALL SCRNMSGI(MSG,IR,14)
      MSG = '    If you have DataEase version 2.5 or version 4.0,'
     +      //'   please select R.'
      IR = IR + 1
      CALL SCRNMSGI(MSG,IR,14)
      MSG = '              R = Regular.                X = eXtended.'
      IR = IR + 1
      CALL SCRNMSGI(MSG,IR,14)
      CALL LOCATE(17,4,IERR)
      CALL GETSTR(0,DETYPE,1,15,1,RTNFLAG)
      IF (DETYPE.EQ.BLANK) THEN
         CALL BEEP
         GO TO 70
      END IF
 80   CONTINUE
      CALL LOCATE(18,0,IERR)
      IR = 18
      MSG = '    Enter the drive and the directory where the DataEase'
     +         //' climate data files '
      CALL SCRNMSGI(MSG,IR,14)
      MSG = '    are currently installed.  (e.g. C:\DEDATA)'
      IR = IR + 1
      CALL SCRNMSGI(MSG,IR,14)
      CALL LOCATE(20,4,IERR)
      CALL GETSTR(0,DEDATADR,20,15,1,RTNFLAG)
      IF (DEDATADR.EQ.BLANK) THEN
         CALL BEEP
         GO TO 80
      END IF
 90   CONTINUE
      CALL LOCATE(21,0,IERR)
      IR = 21
      MSG = '    Please specify the drive and directory which currently'
     +         //' hold the DOS command'
      CALL SCRNMSGI(MSG,IR,14)
      MSG = '    files.  (e.g. C:\DOS, etc.)  If DOS is in the root'
     +         //' directory, specify only'
      IR = IR + 1
      CALL SCRNMSGI(MSG,IR,14)
      MSG = '    the drive letter.  (e.g. C:)'
      IR = IR + 1
      CALL SCRNMSGI(MSG,IR,14)
      CALL LOCATE(24,4,IERR)
      CALL GETSTR(0,DOSDR,12,15,1,RTNFLAG)
      IF (DOSDR.EQ.BLANK) THEN
         CALL BEEP
         GO TO 90
      END IF
C
C     VERIFY THE ANSWERS THAT HAVE PROVIDED.  IF THE INFORMATIONS ARE CORRECT,
C     THEN THE PROGRAM WILL WRITE INTO A BATCH FILE C:\CALLRVSN.BAT.
C     OTHERWISE, THE PROGRAM WILL QUIT OR BEGIN AGAIN.
C
      CALL CLS
      CALL LOCATE(3,0,IERR)
      CALL POSLIN(IR,IC)
      MSG = ' Please check the answers you have provided, and press'
     +      //' [ENTER] to continue.'
      IR = IR + 1
      CALL SCRNMSGI(MSG,IR,14)
      MSG = ' If you want to change any of the information shown,'
     +     //' press [ESC] or [F4].'
      IR = IR + 1
      CALL SCRNMSGI(MSG,IR,14)
      MSG = ' No revision takes place when you press [Q]'
     +     //' to quit.'
      IR = IR + 1
      CALL SCRNMSGI(MSG,IR,14)
      MSG = ' '
      IR = IR + 2
      CALL SCRNMSGI(MSG,IR,14)
      MSG = '     CLICOM program files are currently installed on:     ' 
     +      //'     '//CLICPRDR
      IR = IR + 1
      CALL SCRNMSGI(MSG,IR,14)
      MSG = '     CLIGRAF directory is currently installed on:         '
     +      //'     '//CLIGRFDR
      IR = IR + 1
      CALL SCRNMSGI(MSG,IR,14)
      MSG = '     DataEase program files are located on:               '
     +      //'     '//DEDRV//':\'//DEDIR
      IR = IR + 1
      CALL SCRNMSGI(MSG,IR,14)
      MSG = '     DOS is located in:                                   '
     +      //'     '//DOSDR
      IR = IR + 1
      CALL SCRNMSGI(MSG,IR,14)
      MSG = '     DataEase Version installed:                          '
     +      //'     '//DEVERS
      IR = IR + 1
      CALL SCRNMSGI(MSG,IR,14)
      MSG = '     DataEase memory model (Regular or eXtended):         '
     +      //'     '//DETYPE
      IR = IR + 1
      CALL SCRNMSGI(MSG,IR,14)
      MSG = '     DataEase climate data files are currently installed'
     +      //' on:   '//DEDATADR
      IR = IR + 1
      CALL SCRNMSGI(MSG,IR,14)
      MSG = ' '
      IR = IR + 2
      CALL SCRNMSGI(MSG,IR,14)
      MSG = ' Press [ENTER] to continue.                            '
     +      //'  Press [Q] to quit.'
      IR = IR + 1
      CALL SCRNMSGI(MSG,IR,11)
      MSG = ' '
      IR = IR + 1
      CALL SCRNMSGI(MSG,IR,14)
      MSG = ' Press [ESC] or [F4] to make any changes to the'
     +      //' information shown above.'
      IR = IR + 1
      CALL SCRNMSGI(MSG,IR,11)
C
C     GET THE USER INPUT: [ESC] TO MAKE THE CHANGES
C                         [Q] TO QUIT
C                         [ENTER] TO CONTINUE
C
 100  CONTINUE
      CALL LOCATE(22,0,IERR)
      CALL GETCHAR(0,INCHAR)
      IF (INCHAR.EQ.'Q') THEN
         CLOSE(51)
         CALL LOCATE(24,0,IERR)
         STOP 3
      ELSE IF (INCHAR.EQ.'4F') THEN
         GO TO 20
      ELSE IF (INCHAR.EQ.'RE') THEN
         GO TO 110
      ELSE
         CALL BEEP
         GO TO 100
      END IF
 110  CONTINUE
      WRITE(LINE,500)CLICPRDR,CLIGRFDR,DEVERS,DETYPE,DEDRV,DEDIR,
     +      DEDATADR,DOSDR
 500  FORMAT('C:\RVSNCLI3 ',A1,1X,A1,1X,A3,1X,A1,1X,A1,1X,A12,1X,A20,
     +       1X,A12)
      CALL GETFRMT(LINE,OUTFRMT)
      WRITE(51,OUTFRMT) LINE
      CLOSE(51)
C
      STOP ' '
      END      
