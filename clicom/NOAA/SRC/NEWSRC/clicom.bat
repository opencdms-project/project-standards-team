ECHO OFF 
REM         *****     CLICOM VERSION 3.0     ***** 
IF "%1"=="EXIT" GOTO CLOSE
REM  WHEN RUNNING CLICOM FROM A NETWORK RDR COMPUTER, REPLACE THE SUBST  
REM  COMMANDS WITH NET USE COMMANDS IN THIS FILE.  
IF EXIST P:\PROG\*.* CLICOM2 
C:\CLICOM\PROG\ENVRSIZE 
IF ERRORLEVEL 1 GOTO NOSPACE 
GOTO CKSUBST 
:NOSPACE 
ECHO The amount of free DOS environment space is insufficient for CLICOM 
ECHO processing. CLICOM needs 150 bytes.  Check the CLICOM reference 
ECHO manual chapter 14.3 for more information.
PAUSE
SET  $X=
GOTO SIGNOFF
:CKSUBST
IF NOT EXIST P:\FORM\*.* SET XPATH=%PATH%
IF NOT EXIST P:\FORM\*.* GOTO NOSUBST 
c:\dos\SUBST O: /D
c:\dos\SUBST P: /D
c:\dos\SUBST Q: /D
:NOSUBST
REM --- This section initializes information necessary to start CLICOM 
SET CLIDRV=C:
SET DEPRGDRV=C:
SET DDISK=c:\DEDATA
SET GDISK=C:\CLIGRAF
SET RDISK=P:
SET DEASE=2.5
SET HALOID=A
c:\dos\SUBST O: %GDISK%
c:\dos\SUBST P: C:\CLICOM
c:\dos\SUBST Q: %DDISK%
PATH=%RDISK%\;P:\BATCH;P:\PROG;C:\UTIL;c:\dos
REM --- Change to the CLICOM program drive and start CLICOM
%CLIDRV%
CLS
SIGNON
IF NOT "%$X%" == "" GOTO BEGIN
LOGON1
LOGON2
:BEGIN
CLICOM2
:CLOSE
REM --- This section clears the environment and substituted disk drives.  
REM     The environment variables PATH and $X= are set/cleared in CLICOM2.BAT
SET CLIDRV=
SET DEPRGDRV=
SET DDISK=
SET GDISK=
SET RDISK=
SET DEASE=
SET HALOID=
SET XPATH=
c:\dos\SUBST O: /D
c:\dos\SUBST P: /D
c:\dos\SUBST Q: /D
IF  "%$X%" == "" GOTO SIGNOFF
ECHO [2B
ECHO Your CLICOM access level is still in effect.  Use menu choice B of the 
ECHO main CLICOM menu if you wish to sign-off completely. 
:SIGNOFF
ECHO [2B
ECHO You have returned to DOS - Type CLICOM to enter CLICOM...
ECHO [2B
