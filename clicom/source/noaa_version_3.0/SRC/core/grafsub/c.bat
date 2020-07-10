echo off
FL /FPi /Od /c %1.FOR 
IF ERRORLEVEL 1 GOTO EXIT
LIB C:\clicom\LIB\PLTUTIL -+%1;
:EXIT
