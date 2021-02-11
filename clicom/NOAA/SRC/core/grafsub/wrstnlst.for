$STORAGE:2
      SUBROUTINE WRSTNLST(STNSTAT,MXSTAT,LSTNAM)
C
      INTEGER*2 MXSTAT
      INTEGER*4 STNSTAT(MXSTAT)
      CHARACTER*8 LSTNAM
C
      CHARACTER*20 LSTFIL  
      CHARACTER*32 LSTDESC
      CHARACTER*78 MSGTXT
      CHARACTER*24 STNABRV
      CHARACTER*20 COUNTRY
      CHARACTER*8  STNID
      CHARACTER*8 INLON
      CHARACTER*7 INLAT
      CHARACTER*2 RTNFLAG,YESUP,YESLO
      CHARACTER*1 REPLY
      INTEGER*4 BDATE,EDATE,NUMSTN
      LOGICAL NEWNAME
C
      CALL GETYN(1,2,YESUP,YESLO)
      NEWNAME=.FALSE.
C
      CALL POSLIN(IROW,ICOL)
  40  CONTINUE
      CALL CLRMSG(25-IROW)
      CALL LOCATE(IROW,ICOL,IERR)
C
C       ** GET NAME OF STATION SELECTION FILE         
      IF (LSTNAM.EQ.' ') THEN
         CALL GETMSG(584,MSGTXT)
         MSGLEN = LNG(MSGTXT)
         CALL WRTSTR(MSGTXT,MSGLEN,14,0)
         CALL GETSTR(0,LSTNAM,8,15,1,RTNFLAG)
         IF (RTNFLAG.EQ.'4F') THEN
            GO TO 100
         ELSE IF (LSTNAM.EQ.' ') THEN
            GO TO 40
         ENDIF
         NEWNAME = .TRUE.
      ELSE
         CALL GETMSG(585,MSGTXT)
         MSGLEN = LNG(MSGTXT)
         CALL WRTSTR(MSGTXT,MSGLEN,14,0)
         REPLY = ' '
         CALL GETSTR(0,REPLY,1,15,1,RTNFLAG)
         IF (RTNFLAG.EQ.'4F') THEN
            GO TO 100
         ELSE IF (REPLY.EQ.YESUP(1:1)) THEN
            CALL CLRMSG(25-IROW)
            CALL LOCATE(IROW,ICOL,IERR)
            CALL GETMSG(584,MSGTXT)
            MSGLEN = LNG(MSGTXT)
            CALL WRTSTR(MSGTXT,MSGLEN,14,0)
            CALL GETSTR(0,LSTNAM,8,15,1,RTNFLAG)
            IF (RTNFLAG.EQ.'4F') THEN
               GO TO 100
            ENDIF
            NEWNAME = .TRUE.
         ENDIF
      ENDIF
      IF (NEWNAME) THEN
         CALL CLRMSG(25-(IROW+1))
         CALL LOCATE(IROW+1,ICOL,IERR)
         CALL GETMSG(587,MSGTXT)
         MSGLEN = LNG(MSGTXT)
         CALL WRTSTR(MSGTXT,MSGLEN,14,0)
         LSTDESC = ' '
         CALL GETSTR(1,LSTDESC,32,15,1,RTNFLAG)
         IF (RTNFLAG.EQ.'4F') THEN
            GO TO 100
         ENDIF
      ENDIF
C
C  OPEN THE STNGEOG.INF FILE AND INITIALIZE
C
  50  OPEN (35,FILE='P:\DATA\STNGEOG.INF',STATUS='OLD',
     +      ACCESS='DIRECT',RECL=80,SHARE='DENYWR',MODE='READ'
     +      ,IOSTAT=IOCHK)
      IF (IOCHK.NE.0) THEN
         CALL OPENMSG('P:\DATA\STNGEOG.INF   ','GRAFINIT    ',IOCHK)
         GO TO 50
      END IF
      READ(35,REC=1) NUMSTN
C
C  OPEN THE STATION SELECTION FILE
C
      LSTFIL = 'O:\DATA\'//LSTNAM
      NCHR = LNG(LSTFIL)
      LSTFIL(NCHR+1:) = '.LST'
      IF (NEWNAME) THEN
         OPEN (51,FILE=LSTFIL,STATUS='NEW',
     +            ACCESS='DIRECT',RECL=36,IOSTAT=IOCHK)
         IF (IOCHK.NE.0) THEN
            CALL CLRMSG(25-(IROW+1))
            CALL LOCATE(IROW+1,ICOL,IERR)
            CALL GETMSG(586,MSGTXT)
            MSGLEN = LNG(MSGTXT)
            CALL WRTSTR(MSGTXT,MSGLEN,12,0)
            CALL BEEP
            CALL GETCHAR(0,RTNFLAG)
            CALL CLRMSG(25-(IROW+1))
            IF (RTNFLAG.NE.YESUP.AND.RTNFLAG.NE.YESLO) THEN
               NEWNAME=.FALSE.
               GO TO 40
            ELSE
               OPEN (51,FILE=LSTFIL,STATUS='UNKNOWN',
     +                  ACCESS='DIRECT',RECL=36)
               CLOSE(51,STATUS='DELETE')
               OPEN (51,FILE=LSTFIL,STATUS='NEW',
     +                  ACCESS='DIRECT',RECL=36)
            ENDIF
         ENDIF
      ELSE
         OPEN (51,FILE=LSTFIL,STATUS='UNKNOWN',
     +            ACCESS='DIRECT',RECL=36)
         CLOSE(51,STATUS='DELETE')
         OPEN (51,FILE=LSTFIL,STATUS='NEW',
     +            ACCESS='DIRECT',RECL=36)
      ENDIF
C
      NSTN = MIN0(MXSTAT,NUMSTN+1)
      IRECL= 1
      DO 80 IRECG=2,NSTN
         IF (STNSTAT(IRECG).EQ.0) GO TO 80
         READ(35,REC=IRECG) STNID,BDATE,EDATE,STNABRV,
     +           COUNTRY,INLAT,INLON
         IRECL=IRECL+1
         WRITE(51,REC=IRECL) IRECG,STNID,STNABRV
   80 CONTINUE      
      WRITE(51,REC=1) IRECL-1
      CLOSE(51)
C
C      **  IF THIS IS A NEW STATION SELECTION LIST ENTER THE NAME AND
C          DESCRIPTION INTO THE SELECTION LIST INDEX FILE
C
      IF (NEWNAME) THEN
         CALL MPSTNIDX(LSTNAM,LSTDESC)
      ENDIF
C
  100 CONTINUE
      CALL GETMSG(999,MSGTXT)
      DO 110 I=0,2
         CALL CLRMSG(25-(IROW+I))
  110 CONTINUE       
      RETURN
      END
************************************************************************ 
      SUBROUTINE MPSTNIDX(LSTNAM,LSTDESC)
C
C   ROUTINE TO STORE THE CURRENT STATION SELECTION LIST NAME AND DESCRIPTION 
C   INTO THE STATION LIST INDEX FILE (\DATA\MPSTNLST.IDX).  
C
      CHARACTER*8 LSTNAM
      CHARACTER*32 LSTDESC
C      
      CHARACTER*1 CHRRTN,LNFEED
      CHARACTER*43 INREC
      CHRRTN = CHAR(13)
      LNFEED = CHAR(10)
C
      OPEN (51,FILE='O:\DATA\MPSTNLST.IDX',STATUS='UNKNOWN'
     +     ,FORM='BINARY',ACCESS='DIRECT',RECL=43)
      IDEL = 0
      DO 100 I = 1,9999
         READ(51,REC=I,ERR=110) INREC
         IF (INREC(1:8).EQ.LSTNAM) THEN
            INREC(10:41) = LSTDESC
            IDEL = 0
            GO TO 200
         ELSE IF (INREC(1:8).EQ.'********') THEN
            IDEL = I
         ENDIF
100   CONTINUE
110   CONTINUE
      WRITE(INREC,'(A8,1X,A32,2A1)') LSTNAM, LSTDESC,CHRRTN,LNFEED
C
200   CONTINUE
      IF (IDEL.GT.0) THEN
         WRITE(51,REC=IDEL) INREC
      ELSE
         WRITE(51,REC=I) INREC
      ENDIF
      CLOSE (51)
      RETURN
      END
