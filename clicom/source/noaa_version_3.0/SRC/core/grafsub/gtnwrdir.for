$STORAGE:2
      SUBROUTINE GTNWRDIR(NWRDIR,RTNCODE) 
C      
      CHARACTER*78 MSGLIN
      CHARACTER*2 RTNFLAG
      CHARACTER*2  FIELD,OKVAL(3)
      CHARACTER*1  RTNCODE      
      LOGICAL OKFLG
      DATA OKVAL/' 8','08','16'/
C      
      RTNCODE='0'
      MSGLIN=' '
      IF (NWRDIR.EQ.8 .OR. NWRDIR.EQ.16) THEN
         WRITE(FIELD,'(I2.2)') NWRDIR
      ELSE
         FIELD=' '
      ENDIF   
      CALL GETMSG(564,MSGLIN)
      CALL GETMSG(999,MSGLIN)
      NCHR = LNG(MSGLIN)
      CALL LOCATE(18,0,IERR)
      CALL WRTSTR(MSGLIN,NCHR,14,0)
      CALL POSLIN(IROW,ICOL)
      CALL WRTFNC(0)
   20 CONTINUE
      CALL LOCATE(IROW,ICOL,IERR)
      CALL GETSTR(0,FIELD,2,15,1,RTNFLAG)
      IF (RTNFLAG.EQ.'4F') THEN
         RTNCODE = '1'
      ELSE IF (RTNFLAG.EQ.'1F') THEN
         MSGLIN = 'P:\HELP\GTNWRDIR.HLP'
         CALL DSPWIN(MSGLIN(1:64))
         GO TO 20
      ELSE 
         OKFLG=.FALSE.
         DO 25 I=1,3
            OKFLG = OKFLG .OR. (FIELD.EQ.OKVAL(I))
   25    CONTINUE  
         IF (OKFLG) THEN 
            READ (FIELD,'(I2)') NWRDIR
         ELSE
            CALL WRTMSG(2,69,12,1,0,'(8,16)',6)
            GO TO 20
         ENDIF   
      ENDIF   
C
      RETURN
      END      
