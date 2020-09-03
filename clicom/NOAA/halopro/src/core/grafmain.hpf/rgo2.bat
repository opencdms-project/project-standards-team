echo off
SUBST P: C:\CLICOM
c:\dos\SUBST O: C:\CLIGRAF
SET HALOID=A
SET DEASE=2.5
PROMPT $P$G
:GM2
GRFMN2
IF ERRORLEVEL 1 GOTO EXIT
CLS
::*P:\PROG\OPTMAN
OPTMAN
GOTO GM2
:EXIT
SET DEASE=
SET HALOID=
SUBST P: /D
SUBST O: /D
prompt $e[s$e[1;1H$e[0;42;30m DateÍ$d TimeÍ$t$e[6D$e[K$e[1;45HPathÍ $p$e[1;40;33m$e[u$e[KCommandÍ
