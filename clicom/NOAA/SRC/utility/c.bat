echo off
FL /FPi /Od /c %1.FOR
IF ERRORLEVEL 1 GOTO EXIT
LIB c:\clicom\LIB\UTILITY -+%1;
:EXIT
