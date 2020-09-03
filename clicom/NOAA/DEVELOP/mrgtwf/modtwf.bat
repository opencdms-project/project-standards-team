@ECHO OFF
:: 
::  This batch file is called by the CALLTWF.BAT file which is created
::  during the execution of INST-TWF.BAT file.
::
::  Passed parameters are:
::       %1 = CLICOM program drive
::       %2 = CLIGRAF program drive
::
::  **  Verify user disk drive selected before the installation continues **
:: 
IF EXIST %1:\CLICOM\PROG\ADMIN.EXE GOTO CLIGRAF
CLS
ECHO [3B[31m
ECHO  CLICOM program files were not found on %1: drive.
ECHO [1B[33m
GOTO MESSAGE
:CLIGRAF
IF EXIST %2:\CLIGRAF\DATA\PALETTE.DEF GOTO CONTINUE
CLS
ECHO [3B[31m
ECHO  No CLIGRAF directory files were found on %2: drive.
ECHO [1B[33m
GOTO MESSAGE
:CONTINUE
CLS
ECHO [1;33m[8;10H Working [5;33m . . . .[0m[1;33m
COPY %DRVLTR%:\MRGTWF\*.HLP %1:\CLICOM\HELP > NUL
COPY %DRVLTR%:\MRGTWF\MRGTWF.EXE %1:\CLICOM\PROG > NUL
::
::   SAVES THE CLICOM USERMENU.DEF FILE
::
IF EXIST %1:\CLICOM\FORM\USERMENU.DSV GOTO USER1
COPY %1:\CLICOM\FORM\USERMENU.DEF %1:\CLICOM\FORM\USERMENU.DSV > NUL
GOTO MODMENU
:USER1
COPY %1:\CLICOM\FORM\USERMENU.DEF %1:\CLICOM\FORM\USERMENU.DS1 > NUL
::
::   MODIFY THE CLICOM USER MENUS AND GRAPHIC MENUS
::
:MODMENU
%DRVLTR%:\MRGTWF\MDTWFMNU %1 %2
IF ERRORLEVEL 3 GOTO MENUSFLE
IF ERRORLEVEL 2 GOTO MISSPAR
IF ERRORLEVEL 1 GOTO USERFILE
::
::   SAVES THE CLICOM MESSAGES FILES
::
IF EXIST %1:\CLICOM\DATA\MESSAGES.TSV GOTO MESSTXT
COPY %1:\CLICOM\DATA\MESSAGES.TXT %1:\CLICOM\DATA\MESSAGES.TSV > NUL
GOTO COPYFTN
:MESSTXT
COPY %1:\CLICOM\DATA\MESSAGES.TXT %1:\CLICOM\DATA\MESSAGES.TS1 > NUL
:COPYFTN
IF EXIST %1:\CLICOM\DATA\MESSAGES.FSV GOTO MESSFTN
COPY %1:\CLICOM\DATA\MESSAGES.FTN %1:\CLICOM\DATA\MESSAGES.FSV > NUL
GOTO COPYMSG
:MESSFTN
COPY %1:\CLICOM\DATA\MESSAGES.FTN %1:\CLICOM\DATA\MESSAGES.FS1 > NUL
:COPYMSG
COPY %DRVLTR%:\MRGTWF\MESSAGES.* %1:\CLICOM\DATA > NUL
COPY %DRVLTR%:\MRGTWF\*.FRM  %1:\CLICOM\FORM > NUL
COPY %DRVLTR%:\MRGTWF\*.PRM  %1:\CLICOM\DATA > NUL
::
::   MODIFY THE FOTRAN FORM DEFINITION
::
%DRVLTR%:\MRGTWF\MDFRMIDX %1 %2
IF ERRORLEVEL 2 GOTO MISSPRM
IF ERRORLEVEL 1 GOTO FORMFILE
GOTO STEP1
:FORMFILE
CLS
ECHO [3B[31m
ECHO  The %1:\CLICOM\FORM\FTNFORM.IDX file is not found.
ECHO  The installation aborted.
ECHO [1B[33m
GOTO MESSAGE
:MENUSFLE
CLS
ECHO [3B[31m
ECHO  The menu files (C:\MRGTWF1.MNU and C:\MRGTWF2.MNU) required for
ECHO  installation are missing.  The installation aborted.
ECHO [1B[33m
GOTO MESSAGE
:USERFILE
CLS
ECHO [3B[31m
ECHO  The %1:\CLICOM\FORM\USERMENU.DEF file is not found. 
ECHO  The installation aborted.
ECHO [1B[33m
GOTO MESSAGE
:MISSPAR
ECHO [3B[31m
ECHO  The required parameter is missing.  The installation aborted.
ECHO [1B[33m
GOTO MESSAGE
:STEP1
CLS
ECHO 
ECHO [4B[1;44;37m
ECHO [14Cษออออออออออออออออออออออออออออออออออออออออออออออออออป
ECHO [14Cบ                                                  บ
ECHO [14Cบ         The installation is complete.            บ
ECHO [14Cบ                                                  บ
ECHO [14Cบ To execute the program, select choice 7 from the บ
ECHO [14Cบ CLICOM Introductory menu.  At the prompt, type:  บ
ECHO [14Cบ                                                  บ
ECHO [14Cบ             MRGTWF     press [Enter]             บ
ECHO [14Cบ                                                  บ
ECHO [14Cศออออออออออออออออออออออออออออออออออออออออออออออออออผ[1;40;33m
ECHO [5B
GOTO EXIT
:MESSAGE
ECHO 
ECHO  The information which you entered about your CLICOM system produced these
ECHO  error message(s).  Processing stops here. 
ECHO [12B
PAUSE
CLS
ECHO [2B
:QUIT
ECHO [2B[1;40;33m
ECHO [14Cษอออออออออออออออออออออออออออออออออออออออออออออออป
ECHO [14Cบ                                               บ
ECHO [14Cบ  You are now at the DOS command prompt line.  บ
ECHO [14Cบ                                               บ
ECHO [14Cบ         To continue the procedure type:       บ
ECHO [14Cบ                                               บ
ECHO [14Cบ [14C [36m %DRVLTR%:\INST-TWF %DRVLTR%[33m[17Cบ
ECHO [14Cบ                                               บ
ECHO [14Cศอออออออออออออออออออออออออออออออออออออออออออออออผ[1;40;33m
:EXIT
ECHO [5B
SET DRVLTR=
IF EXIST C:\CALLTWF.BAT DEL C:\CALLTWF.BAT
IF EXIST C:\MRGTWF?.MNU DEL C:\MRGTWF?.MNU
C:
CD\
