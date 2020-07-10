$STORAGE:2
      SUBROUTINE BINDATA(IDKEY,RTNCODE)
C
C     SUBROUTINE TO DO A BINARY SEARCH ON THE INDEX FILE TO LOCATE
C     ANY EXISTING STARTING ADDRESS FOR THIS FRAME IN THE DATA FILE.
C
$INCLUDE: 'VAL1.INC'
$INCLUDE: 'INDEX.INC'
C
      CHARACTER*1 RTNCODE
      CHARACTER*21 IDKEY,HLDIDX
C
      INTEGER*2 HLDDEL,ITOP,IBOT
C
C     RETURN CODE (RTNCODE)
C       '0' = KEY FOUND
C       '2' = KEY NOT FOUND
C
C     IF NUMIDX IN THE INDEX FILE IS = 1 THE DATA FILE IS EMPTY
C
      READ(19,REC=1)DELKEY,BGNIDX,NUMIDX
      ITOP = NUMIDX + 1
      IBOT = BGNIDX - 1
C
      IF (NUMIDX.EQ.1)THEN
         RECNUM = 1
         IDXNUM = 2
         RTNCODE = '2'
         DELKEY = 0
         NUMIDX = 2
         BGNIDX = 2
         RETURN
      ENDIF
C
C    DO BINARY SEARCH FOR THE RECORD WANTED
C
      HLDIDX = '    '
   50 CONTINUE
      IF (ITOP-IBOT.GT.1) THEN
         K = (ITOP+IBOT)/2
         READ (19,REC=K)HLDDEL,HLDIDX,RECNUM
         IF (IDKEY.LT.HLDIDX)THEN
            ITOP = K
            GO TO 50
         ELSE IF (IDKEY.GT.HLDIDX)THEN
            IBOT = K
            GO TO 50
         ELSE IF (IDKEY.EQ.HLDIDX) THEN
            GO TO 100
         END IF
      ELSE
         IF (IDKEY.NE.HLDIDX) THEN
            GO TO 120
         END IF
      END IF
C
C     A MATCH WAS FOUND
C
  100 IDXNUM = K
      RTNCODE = '0'
      RETURN
C
C     NO MATCH FOUND
C
  120 CONTINUE
      IF (BGNIDX.GT.2)THEN
         BGNIDX = BGNIDX  - 1
         IDXNUM = BGNIDX
         READ (19,REC=IDXNUM)HLDDEL,HLDIDX,RECNUM
         HLDIDX = '   '
         DELKEY = 0
         RTNCODE = '2'
      ELSE
         RECNUM = NUMIDX
         DELKEY = 0
         NUMIDX = NUMIDX + 1
         IDXNUM = NUMIDX
         RTNCODE = '2'
      END IF
      RETURN
      END
