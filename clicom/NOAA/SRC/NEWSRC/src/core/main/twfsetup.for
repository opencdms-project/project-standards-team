$STORAGE:2
C
C     PROGRAM TWFSETUP
C
C     ** OBJECTIVE:  THE PUPOSE OF THIS PROGRAM IS CONSTRUCTED A CALLTWF.BAT 
C                    FILE BASE ON THE RESPONSES OF THE USER.  
C
      PROGRAM TWFSETUP
C
      CHARACTER*1  CLICPRDR,CLIGRFDR,BLANK
      CHARACTER*2  INCHAR
      CHARACTER*80 OUTFRMT,MSG,LINE
      DATA BLANK /' '/
C
      OPEN(51,FILE='C:\CALLTWF.BAT',STATUS='UNKNOWN',FORM='FORMATTED')
C
      CLICPRDR = BLANK
      CLIGRFDR = BLANK
C
 20   CONTINUE
      CALL CLS
      CALL LOCATE(5,0,IERR)
      CALL POSLIN(IR,IC)
      MSG = '    Enter the letter of the drive where the '
     +          //'CLICOM software is installed.'
      CALL SCRNMSGI(MSG,IR,14)
      CALL LOCATE(6,4,IERR)
      CALL GETSTR(0,CLICPRDR,1,15,1,RTNFLAG)
      IF (CLICPRDR.EQ.BLANK) THEN
         CALL BEEP
         GO TO 20
      END IF
 30   CONTINUE
      CALL LOCATE(8,0,IERR)
      IR = 8
      MSG = '    Enter the letter of the drive that contains the '
     +         //'CLIGRAF directory.'
      CALL SCRNMSGI(MSG,IR,14)
      CALL LOCATE(9,4,IERR)
      CALL GETSTR(0,CLIGRFDR,1,15,1,RTNFLAG)
      IF (CLIGRFDR.EQ.BLANK) THEN
         CALL BEEP
         GO TO 30
      END IF
C
C     VERIFY THE ANSWERS THAT HAVE PROVIDED.  IF THE INFORMATIONS ARE CORRECT,
C     THEN THE PROGRAM WILL WRITE INTO A BATCH FILE C:\CALLTWF.BAT.
C     OTHERWISE, THE PROGRAM WILL QUIT OR BEGIN AGAIN.
C
      CALL CLS
      CALL LOCATE(5,0,IERR)
      CALL POSLIN(IR,IC)
      MSG = ' Please check the answers you have provided.  If they are'
     +      //' correct, press [Enter]'
      IR = IR + 1
      CALL SCRNMSGI(MSG,IR,14)
      MSG = ' to continue.  If you want to change any of the'
     +     //' information shown, press [ESC].'
      IR = IR + 1
      CALL SCRNMSGI(MSG,IR,14)
      MSG = ' The process stops when you press [Q] to quit.'
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
      IR = IR + 2
      CALL SCRNMSGI(MSG,IR,14)
      MSG = ' Press [ENTER] to continue.                            '
     +      //'  Press [Q] to quit.'
      IR = IR + 3
      CALL SCRNMSGI(MSG,IR,11)
      MSG = ' '
      IR = IR + 1
      CALL SCRNMSGI(MSG,IR,14)
      MSG = ' Press [ESC] to make any changes to the information'
     +      //' shown above.'
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
      WRITE(LINE,500)CLICPRDR,CLIGRFDR
 500  FORMAT('C:\MODTWF ',A1,1X,A1)
      CALL GETFRMT(LINE,OUTFRMT)
      WRITE(51,OUTFRMT) LINE
      CLOSE(51)
C
      STOP ' '
      END      
