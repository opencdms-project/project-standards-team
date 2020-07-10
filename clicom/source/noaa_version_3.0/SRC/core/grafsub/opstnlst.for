$STORAGE:2
      SUBROUTINE OPSTNLST(STNSTAT,R4WORK,MXSTAT,MAXWRK,MAXLST,LSTNAM)
C
      INTEGER*2    MXSTAT,MAXWRK
      INTEGER*4    STNSTAT(MXSTAT)
      REAL*4       R4WORK(MAXWRK)
      CHARACTER*8  LSTNAM      
C
      LOGICAL PRTLST
      CHARACTER*1  RTNCODE
      CHARACTER*64 HELPFILE
      CHARACTER*8  SAVNAM      
C
      DATA HELPFILE /'P:\HELP\OPSTNLST.HLP'/
C
      CALL POSLIN(IROW,ICOL)
   10 CONTINUE
      CALL LOCATE(IROW,ICOL,IERR)    
      CALL GETMNU('LOOKSTN-OPT ',HELPFILE,IOPT)
      CALL CLRMSG(2)
      CALL CLRMSG(3)
      IF (IOPT.EQ.0) THEN
         GO TO 100
      ELSE
         IF ((LSTNAM.EQ.' ' .OR. LSTNAM.EQ.'STNGEOG') .AND.
     +        IOPT.NE.1) THEN
            IF (LSTNAM.EQ.'STNGEOG') THEN
               CALL WRTMSG(3,581,12,1,0,' ',0)
            ELSE
               CALL WRTMSG(3,598,12,1,0,' ',0)
            ENDIF
         ELSE IF (IOPT.EQ.1 .OR. IOPT.EQ.2) THEN
            DO 20 I=1,MXSTAT
            STNSTAT(I) = 0
   20       CONTINUE
            IF (IOPT.EQ.1) THEN
               SAVNAM=LSTNAM
               LSTNAM=' '
               NSELECT=0
            ELSE   
               CALL GTOLDLST(STNSTAT,MXSTAT,LSTNAM,NSTNLST)
               NSELECT=NSTNLST
            ENDIF   
            CALL DFSTNLST(NSELECT,STNSTAT,R4WORK,MXSTAT,MAXWRK,MAXLST,
     +                    RTNCODE)
            CALL LOCATE(IROW+10,ICOL,IERR)
            IF (RTNCODE.EQ.'0') CALL WRSTNLST(STNSTAT,MXSTAT,LSTNAM)
            IF (LSTNAM.EQ.' ') LSTNAM=SAVNAM
         ELSE IF (IOPT.EQ.3) THEN   
            CALL DLSTNLST(LSTNAM)
         ELSE IF (IOPT.EQ.4) THEN
            PRTLST=.FALSE.
            CALL VWSTNLST(LSTNAM,R4WORK,MAXWRK,PRTLST)
         ELSE IF (IOPT.EQ.5) THEN         
            PRTLST=.TRUE.
            CALL VWSTNLST(LSTNAM,R4WORK,MAXWRK,PRTLST)
         ENDIF
      ENDIF
      GO TO 10
C
  100 CONTINUE
      CALL CLRMSG(1)
      CALL CLRMSG(2)
C
C        ** R4WORK WAS USED AS INTEGER*4 IN DFSTNLST AND VWSTNLST
C           RESET TO REAL VALUES         
      DO 110 I=1,MAXWRK
         R4WORK(I) = 0.
  110 CONTINUE         
      RETURN
      END         
      