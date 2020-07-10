$STORAGE:2
C
      SUBROUTINE PUTDATA(IDKEY,FILFLG,RTNCODE)
C
C       ** OBJECTIVE:  THIS ROUTINE PERFORMS ALL OR SOME OF THE FOLLOWING
C                      TASKS DEPENDING ON THE VALUE OF THE FLAG FILFLG
C                      1 -- OPENS/CLOSES THE .IDX AND .TWF FILES
C                      2 -- SEARCHES INDEX FILE FOR THE LOCATION OF THE 
C                           OUTPUT KEY 
C                      3 -- WRITES A FRAME OF DATA TO THE CORRECT 
C                           RECORD IN THE .TWF FILE.
C
C       ** INPUT:
C             IDKEY......
C             FILFLG.....FLAG TO CONTROL OPERATIONS ON INDEX AND DATA FILES
C                        0=PUTDATA ROUTINE OPENS/CLOSES DATA AND INDEX FILES
C                          PUTDATA ROUTINE FINDS RECORD LOCATION
C                          PUTDATA ROUTINE WRITES AND SORTS RECORDS 
C                        1=CALLING ROUTINE OPENS/CLOSES DATA AND INDEX FILES
C                          PUTDATA ROUTINE FINDS RECORD LOCATION
C                          PUTDATA ROUTINE WRITES AND SORTS RECORDS 
C                        2=CALLING ROUTINE OPENS/CLOSES DATA AND INDEX FILES
C                          CALLING ROUTINE FINDS RECORD LOCATION
C                          PUTDATA ROUTINE WRITES AND SORTS RECORDS 
C
C       **OUTPUT:
C            RTNCODE.....FLAG TO INDICATE THE RESULT OF SEARCH FOR RECORD
C                        LOCATION
C                        '0'=KEY IS FOUND -- OLD RECORD     -- FILFLG=0,1
C                        '1'=NO SEARCH MADE FOR KEY         -- FILFLG=2
C                        '2'=KEY IS NOT FOUND -- NEW RECORD -- FILFLG=0,1
C 
$INCLUDE: 'VAL1.INC'
C
$INCLUDE: 'INDEX.INC'
C
      CHARACTER*1 RTNCODE
      CHARACTER*21 IDKEY,HLDIDX,IDXHLD
C
      INTEGER*2 FILFLG,HLDDEL,DELHLD,HLDNUM,NUMHLD
C
C   OPEN THE DATA AND INDEX FILES
C
      IF (FILFLG.EQ.0) THEN
         CALL OPENFILES(2)
      END IF
C
C       ** FIND THE LOCATION OF THE OUTPUT RECORD 
C
      RTNCODE = '1'
      IF (FILFLG.LT.2) THEN
         CALL BINDATA(IDKEY,RTNCODE)
      ENDIF
C
C       ** WRITE OUTPUT RECORD
C
      WRITE (19,REC=1)DELKEY,BGNIDX,NUMIDX
      DELKEY = 2
      WRITE (19,REC=IDXNUM)DELKEY,IDKEY,RECNUM
      WRITE (20,REC=RECNUM)IDKEY,((VALARRAY(I,J),I=1,NUMELEM),
     +       J=1,NUMLINE),(((FLAGARRAY(K,L,M),M=1,2),K=1,NUMELEM),
     +       L=1,NUMLINE)
C
C  SORT THE JUST ADDED RECORD TO ITS PROPER PLACE IN THE FILE
C
C  IF IDXNUM IS BETWEEN BGNIDX AND NUMIDX THIS RECORD IS AN UPDATE
C  AND IT DOES NOT NEED TO BE RESORTED
C
      IF (IDXNUM.GT.BGNIDX.AND.IDXNUM.LT.NUMIDX)THEN
         GO TO 300
      END IF
C
C  THE NEW RECORD TO BE INSERTED SHOULD HAVE BEEN WRITTEN AS THE
C  FIRST OR LAST ACTIVE FRAME
C
      IF (IDXNUM.EQ.NUMIDX)THEN
         READ (19,REC=NUMIDX)DELHLD,IDXHLD,NUMHLD
         GO TO 90
      ELSE
         READ (19,REC=BGNIDX)DELHLD,IDXHLD,NUMHLD
         GO TO 190
      END IF
C
C  NOW DO THE SORT
C
  90  DO 100 I = BGNIDX,NUMIDX
         READ (19,REC=I)HLDDEL,HLDIDX,HLDNUM
         IF (HLDIDX.LT.IDXHLD)THEN
            GO TO 100
         ELSE IF (HLDIDX.EQ.IDXHLD)THEN
            GO TO 300
         ELSE
            WRITE (19,REC=I)DELHLD,IDXHLD,NUMHLD
            GO TO 110
         END IF
  100 CONTINUE
C
      GO TO 300
C
  110 J = I + 1
      K = NUMIDX - 1
      DO 120 I = J,K
         DELHLD = HLDDEL
         IDXHLD = HLDIDX
         NUMHLD = HLDNUM
         READ (19,REC=I)HLDDEL,HLDIDX,HLDNUM
         WRITE(19,REC=I)DELHLD,IDXHLD,NUMHLD
  120 CONTINUE
      WRITE (19,REC=NUMIDX)HLDDEL,HLDIDX,HLDNUM
      GO TO 300
C
 190  DO 200 I = BGNIDX,NUMIDX - 1
         J = I + 1
         READ (19,REC=J)HLDDEL,HLDIDX,HLDNUM
         IF (HLDIDX.LT.IDXHLD)THEN
            WRITE (19,REC=I)HLDDEL,HLDIDX,HLDNUM
            GO TO 200
         ELSE IF (HLDIDX.EQ.IDXHLD)THEN
            GO TO 300
         ELSE
            WRITE (19,REC=I)DELHLD,IDXHLD,NUMHLD
            GO TO 210
         END IF
  200 CONTINUE
      WRITE (19,REC=NUMIDX)DELHLD,IDXHLD,NUMHLD
C
      GO TO 300
C
  210 J = I + 1
      DO 220 I = J,NUMIDX - 1
         DELHLD = HLDDEL
         IDXHLD = HLDIDX
         NUMHLD = HLDNUM
         READ (19,REC=I+1)HLDDEL,HLDIDX,HLDNUM
         WRITE(19,REC=I)DELHLD,IDXHLD,NUMHLD
  220 CONTINUE
      WRITE (19,REC=NUMIDX)HLDDEL,HLDIDX,HLDNUM

  300 CONTINUE
      IF (FILFLG.EQ.0) THEN
         CLOSE (19)
         CLOSE (20)
      END IF
C
      RETURN
      END
