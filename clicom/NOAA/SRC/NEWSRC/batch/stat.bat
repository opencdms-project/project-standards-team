echo off
REM --------------  BATCH FILE TO PRODUCE STATISTICS ---------------
REM  This file displays menu screen STATMENU.SCR, retrieves a response from the
REM  user, and jumps to the section or file that carries out the choice wanted.
REM 
:BEGIN
REM ------ DISPLAY MENU 4.6, USER INPUT, AND JUMP TO SECTION REQUESTED.
ECHO ON
WRTSCRN P:\FORM\STATMENU.SCR 0
ECHO OFF
ECHO [1A[K
ECHO [0;62;48p[0;59;49p[27;48p[17;1H
QUERY Command? (Enter a number from 0 to 4) @01234
ECHO [0;62;0;62p[0;59;0;59p[27;27p
IF ERRORLEVEL 5 GOTO FOUR
IF ERRORLEVEL 4 GOTO THREE
IF ERRORLEVEL 3 GOTO TWO
IF ERRORLEVEL 2 GOTO ONE
IF ERRORLEVEL 1 GOTO DONE
GOTO BEGIN

REM -------- Print help file on screen
:ONE  
WRTHLP P:\HELP\STAT.HLP
GOTO BEGIN

REM ------- Export data by observation
:TWO
EXPOBS
GOTO BEGIN

REM -------- Produce an Instat ascii file from daily data
:THREE
ECHO *** The program to produce an INSTAT ASCII file from   ***
ECHO *** daily data has not been installed on this computer ***
PAUSE
GOTO BEGIN

REM -------- Instat statistics package
:FOUR
CALL C:\INSTAT
REM ECHO *** The INSTAT statistics package has   ***
REM ECHO *** not been installed on this computer ***
REM PAUSE
GOTO BEGIN

REM -------- Return to climate data products
:DONE
APPL

