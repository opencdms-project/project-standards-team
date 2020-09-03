$STORAGE:2
      INTEGER FUNCTION  LNG (STRG)
C------------------------------------------------------------------------------
C     RETURN THE ACTUAL LENGTH OF A STRING VARIABLE, NOT ITS LENGTH FROM THE
C     CHARACTER*N STATEMENT.  THE 1ST NON-BLANK CHARACTER IS CONSIDERED THE
C     TRUE END OF THE STRING.
C------------------------------------------------------------------------------
      CHARACTER*(*)     STRG
      CHARACTER*1       BLK/' '/
      NUM=LEN(STRG)
      NEND=NUM
      DO 10 MX=1,NUM
         IF(STRG(NEND:NEND).GT.BLK) GO TO 20
         NEND=NEND-1
   10 CONTINUE
   20 LNG=NEND
      RETURN
      END
