$STORAGE:2
C
      SUBROUTINE FNDNXT(IDKEY,STNID,RECCODE,RTNCODE)
C
C  THIS ROUTINE IDENTIFIES THE NEXT OR PRIOR RECORD REQUESTED IF IT
C  EXISTS.
C
$INCLUDE: 'VAL1.INC'
C
$INCLUDE: 'INDEX.INC'
C
      CHARACTER*1 RTNCODE,RECCODE
      CHARACTER*21 IDKEY
      INTEGER*2 NUM
C
C     RETURN CODE (RTNCODE)
C       '0' = NEXT OR PRIOR RECORD FOUND
C       '1' = INTERNAL PROGRAM INCONSISTENCY - STOP
C       '3' = NEXT OR PRIOR RECORD REQUESTED BUT NOT AVAILABLE
C       '4' = UNDETERMINED RESULT
C
      RTNCODE = '4'
C
C   OPEN THE DATA AND INDEX FILES
C
      CALL OPENFILES(1)
C
C  CHECK TO SEE IF THE NEXT OR PRIOR RECORD HAS BEEN REQUESTED AND IF SO,
C  IS IT AVAILABLE.
C
      IF (RECCODE.EQ.'1')THEN
         CALL WRTMSG(4,72,12,1,0,' ',0)
         RTNCODE = '1'
         GO TO 90
      END IF
C
      IF (RECCODE.EQ.'0')THEN
         IF (STNID.EQ.'       ')THEN
            READ(19,REC=1)DELKEY,BGNIDX,NUMIDX
            IF (BGNIDX.GT.NUMIDX)THEN
               RTNCODE = '3'
               IMSG = 63
               GO TO 90
            ELSE
               IDXNUM = NUMIDX
            END IF
         ELSE
            NUM = IDXNUM - 1
            IF (NUM.GE.BGNIDX)THEN
               IDXNUM  = NUM
            ELSE
               RTNCODE = '3'
               IMSG = 63
               GO TO 90
           END IF
         END IF
      ELSE IF (RECCODE.EQ.'2')THEN
         IF (STNID.EQ.'       ')THEN
            READ(19,REC=1)DELKEY,BGNIDX,NUMIDX
            IF (BGNIDX.GT.NUMIDX)THEN
               RTNCODE = '3'
               IMSG = 62
               GO TO 90
            ELSE
               IDXNUM = BGNIDX
            END IF
         ELSE
            NUM = IDXNUM + 1
            IF (NUM.LE.NUMIDX)THEN
               IDXNUM = NUM
            ELSE
               RTNCODE = '3'
               IMSG = 62
               GO TO 90
            END IF
         END IF
      ELSE
         RTNCODE = '1'
         IMSG = 73
         GO TO 90
      END IF
C 
C   CORRECT INDEX FOUND - READ IDKEY
C
      READ (19,REC=IDXNUM)DELKEY,IDKEY,RECNUM
      RTNCODE = '0'
      GO TO 100
C
C
   90 CONTINUE
      CALL WRTMSG(3,IMSG,12,1,0,' ',0)
C
  100 CLOSE (19)
      CLOSE (20)
C
      RETURN
      END
