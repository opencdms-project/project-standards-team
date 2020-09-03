@ECHO OFF
:* 
:*  This batch file is called by the CALLMAP.BAT file which is created
:*  during the execution of INST-MAP.BAT file.
:*
:*  Passed parameters are:
:*       %1 = CLICOM program drive
:*       %2 = CLIGRAF program drive
:*
:*  **  Verify user disk drive selected before the installation continues **
:* 
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
IF NOT EXIST %2:\CLIGRAF\MAP\CLICMAP\LINESEG.DAT GOTO STEP2
ECHO [1B
ECHO The user map files already exist, the installation will replace the 
ECHO data in the existing files.
ECHO [1B[36m
%DRVLTR%:\QUERY    Do you want to replace the existing files (Y/N)? @YN
IF ERRORLEVEL 2 GOTO QUIT1
ECHO [33m
IF EXIST %2:\CLIGRAF\MAP\*.ORG COPY %2:\CLIGRAF\MAP\*.ORG %2:\CLIGRAF\MAP\*.FLE > NUL
:STEP2
CLS
ECHO [1;33m[8;10H Working [5;33m . . . .[0m[1;33m
IF NOT EXIST %2:\CLIGRAF\MAP\CLICMAP\LINESEG.DAT GOTO COPYFILE
IF EXIST %2:\CLIGRAF\MAP\CLICMAP\RP*.* DEL %2:\CLIGRAF\MAP\CLICMAP\RP*.* > NUL
IF EXIST %2:\CLIGRAF\MAP\CLICMAP\COAST.* DEL %2:\CLIGRAF\MAP\CLICMAP\COAST.* > NUL
IF EXIST %2:\CLIGRAF\MAP\CLICMAP\RIVERS.* DEL %2:\CLIGRAF\MAP\CLICMAP\RIVERS.* > NUL
IF EXIST %2:\CLIGRAF\MAP\CLICMAP\BORDERS.* DEL %2:\CLIGRAF\MAP\CLICMAP\BORDERS.* > NUL
IF EXIST %2:\CLIGRAF\MAP\CLICMAP\LAKES.* DEL %2:\CLIGRAF\MAP\CLICMAP\LAKES.* > NUL
IF EXIST %2:\CLIGRAF\MAP\CLICMAP\STATES.* DEL %2:\CLIGRAF\MAP\CLICMAP\STATES.* > NUL
IF EXIST %2:\CLIGRAF\MAP\CLICMAP\DETAILMP.* DEL %2:\CLIGRAF\MAP\CLICMAP\DETAILMP.* > NUL
DEL %2:\CLIGRAF\MAP\CLICMAP\LINESEG.* > NUL
:COPYFILE
COPY %DRVLTR%:\*.HLP %1:\CLICOM\HELP > NUL
COPY %DRVLTR%:\REPLMAP.EXE %1:\CLICOM\PROG > NUL
:*
:*   SAVES THE CLICOM USERMENU.DEF AND GRAFMENU.DEF FILES
:*
IF EXIST %1:\CLICOM\FORM\USERMENU.DSV GOTO USER1
COPY %1:\CLICOM\FORM\USERMENU.DEF %1:\CLICOM\FORM\USERMENU.DSV > NUL
GOTO GRAFMENU
:USER1
COPY %1:\CLICOM\FORM\USERMENU.DEF %1:\CLICOM\FORM\USERMENU.DS1 > NUL
:GRAFMENU
IF EXIST %1:\CLICOM\FORM\GRAFMENU.DSV GOTO GRAF1
COPY %1:\CLICOM\FORM\GRAFMENU.DEF %1:\CLICOM\FORM\GRAFMENU.DSV > NUL
GOTO MODMENU
:GRAF1
COPY %1:\CLICOM\FORM\GRAFMENU.DEF %1:\CLICOM\FORM\GRAFMENU.DS1 > NUL
:*
:*   MODIFY THE CLICOM USER MENUS AND GRAPHIC MENUS
:*
:MODMENU
%DRVLTR%:\MODMENUS %1 %2
IF ERRORLEVEL 5 GOTO MISSPAR
IF ERRORLEVEL 4 GOTO MENUSFLE
IF ERRORLEVEL 3 GOTO GRAFFILE
IF ERRORLEVEL 2 GOTO USERFILE
GOTO NEXTSTEP
:MISSPAR
ECHO [3B[31m
ECHO  The required parameter is missing.  The installation aborted.
ECHO [1B[33m
GOTO QUIT
:MENUSFLE
CLS
ECHO 
ECHO [3B[31m
ECHO  The menu files (C:\RMCLICM1.MNU, C:\RMCLICM2.MNU, and C:\RMCLICM3.MNU)
ECHO  required for installation are missing.  The installation aborted.
ECHO [1B[33m
GOTO QUIT
:GRAFFILE
CLS
ECHO 
ECHO [3B[31m
ECHO  The %1:\CLICOM\FORM\GRAFMENU.DEF file is not found.  The installation aborted.
ECHO [1B[33m
GOTO QUIT
:USERFILE
CLS
ECHO 
ECHO [3B[31m
ECHO  The %1:\CLICOM\FORM\USERMENU.DEF file is not found.  The installation aborted.
ECHO [1B[33m
GOTO QUIT
:*
:*   SAVES THE CLICOM MESSAGES FILES
:*
:NEXTSTEP
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
COPY %DRVLTR%:\MESSAGES.* %1:\CLICOM\DATA > NUL
IF EXIST %2:\CLIGRAF\MAP\CLICMAP\NUL GOTO NEXT
MD %2:\CLIGRAF\MAP\CLICMAP > NUL
:NEXT
COPY %DRVLTR%:\LINESEG.*    %2:\CLIGRAF\MAP\CLICMAP > NUL
COPY %DRVLTR%:\COAST.*      %2:\CLIGRAF\MAP\CLICMAP > NUL
COPY %DRVLTR%:\DETAILMP.RMP %2:\CLIGRAF\MAP\CLICMAP > NUL
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
ECHO [14Cบ         REPLMAP   and   press [Enter]            บ
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
:QUIT1
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
ECHO [14Cบ [14C [36m %DRVLTR%:\INST-MAP %DRVLTR%[33m[17Cบ
ECHO [14Cบ                                               บ
ECHO [14Cศอออออออออออออออออออออออออออออออออออออออออออออออผ[1;40;33m
:EXIT
ECHO [5B
SET DRVLTR=
IF EXIST C:\CALLMAP.BAT DEL C:\CALLMAP.BAT
IF EXIST C:\RMCLICM?.MNU DEL C:\RMCLICM?.MNU
C:
CD\
