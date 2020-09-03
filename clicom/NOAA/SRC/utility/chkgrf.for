$STORAGE:2

      SUBROUTINE CHKGRF(INCHAR)
C
C   ROUTINE TO CONVERT SHIFTED FUNCTION KEYS TO STANDARD GRAPHICS 
C   CHARACTERS.
C
C   THE SHIFTED FUNCTION KEYS ARE CONVERTED ACCORDING TO THE FOLLWOING:
C
C          F1 ³    F2 Ä
C          F3 º    F4 Í
C          F5 Ú    F6 ¿
C          F7 À    F8 Ù
C          F9 É    F10 »
C
      CHARACTER*2 INCHAR
C
      IF (INCHAR.EQ.'1S') THEN
         INCHAR = '³ '
      ELSE IF (INCHAR.EQ.'2S') THEN
         INCHAR = 'Ä'
      ELSE IF (INCHAR.EQ.'3S') THEN
         INCHAR = 'º'
      ELSE IF (INCHAR.EQ.'4S') THEN
         INCHAR = 'Í'
      ELSE IF (INCHAR.EQ.'5S') THEN
         INCHAR = 'Ú'
      ELSE IF (INCHAR.EQ.'6S') THEN
         INCHAR = '¿'
      ELSE IF (INCHAR.EQ.'7S') THEN
         INCHAR = 'À'
      ELSE IF (INCHAR.EQ.'8S') THEN
         INCHAR = 'Ù'
      ELSE IF (INCHAR.EQ.'9S') THEN
         INCHAR = 'É'
      ELSE IF (INCHAR.EQ.'0S') THEN
         INCHAR = '»'
      END IF
      RETURN
      END
