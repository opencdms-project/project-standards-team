$STORAGE:2
      SUBROUTINE GTRECNBR(MXRSTNG,RTNCODE)
C      
      CHARACTER*28 MSGTXT
      CHARACTER*24 STNABRV,LSTABRV
      CHARACTER*20 COUNTRY
      CHARACTER*8  STNID,LSTID
      CHARACTER*8 INLON
      CHARACTER*7 INLAT
      CHARACTER*1 RTNCODE
      INTEGER*4 BDATE,EDATE
      LOGICAL RDLST,RDGEOG,FOUND
C
C  OPEN THE STNGEOG.INF FILE AND INITIALIZE
C
20    OPEN (35,FILE='P:\DATA\STNGEOG.INF',STATUS='OLD',
     +      ACCESS='DIRECT',RECL=80,SHARE='DENYWR',MODE='READ'
     +      ,IOSTAT=IOCHK)
      IF (IOCHK.NE.0) THEN
         CALL OPENMSG('P:\DATA\STNGEOG.INF   ','GRAFINIT    ',IOCHK)
         GO TO 20
      END IF
C
      RTNCODE='3'        
      READ(51,REC=1,IOSTAT=ISTAT,ERR=110) NRECL      
C
      RTNCODE='0'        
      RDLST  = .TRUE.
      RDGEOG = .TRUE.
      FOUND  = .FALSE.
      IGREC  = 1
      LRECOUT= 1
      LSTID  = ' '
C
      DO 100 LRECIN=2,NRECL+1
         READ(51,REC=LRECIN,IOSTAT=ISTAT) IDUM,LSTID,LSTABRV
         IF (ISTAT.NE.0) THEN
            RTNCODE='3'
            GO TO 110
         ENDIF   
         RDLST=.FALSE.
   30    CONTINUE
         IF (RDGEOG) THEN
            IGREC=IGREC+1
            IF (IGREC.GT.MXRSTNG) THEN
               IF (.NOT.FOUND .OR. LRECIN.LT.NRECL+1) THEN
C                   .. MAX STATIONS IN STNGEOG.INF PROCESSED BEFORE ALL LIST 
C                      STNID ARE FOUND
                  WRITE(MSGTXT,'(I5)') MXRSTNG-1
                  CALL WRTMSG(3,614,12,0,0,MSGTXT,5)
                  CALL WRTMSG(2,550,12,1,1,LSTID,8)
                  RTNCODE='1'
               ENDIF
               GO TO 110
            ENDIF
            READ(35,REC=IGREC,IOSTAT=ISTAT) STNID,BDATE,EDATE,STNABRV,
     +                                      COUNTRY,INLAT,INLON
            IF (ISTAT.NE.0) THEN
               RTNCODE='2'
               GO TO 110
            ENDIF   
            RDGEOG=.FALSE.
         ENDIF   
         IF (STNID.LT.LSTID) THEN
            RDGEOG=.TRUE.
         ELSE IF (STNID.EQ.LSTID) THEN
            FOUND = .TRUE.
            RDGEOG=.TRUE.
            LSTABRV = STNABRV
            IGRECSV = IGREC
         ELSE 
C             .. STATION ID IN STNGEOG.INF IS GREATER THAN STATION ID IN LIST
            IF (FOUND) THEN
               LRECOUT=LRECOUT+1
               WRITE(51,REC=LRECOUT) IGRECSV,LSTID,LSTABRV
               FOUND = .FALSE.
            ELSE   
C                .. LIST ID NOT FOUND IN STNGEOG.INF            
               CALL WRTMSG(2,588,12,1,1,LSTID,8)
               RTNCODE='1'
            ENDIF   
            RDLST=.TRUE.
         ENDIF
         IF (.NOT.RDLST) GO TO 30      
  100 CONTINUE
  110 CONTINUE
      IF (RTNCODE.EQ.'2'.OR.RTNCODE.EQ.'3') THEN
            IF (RTNCODE.EQ.'3') THEN
               MSGTXT='STATION SELECTION LIST'
            ELSE
               MSGTXT='STNGEOG.INF'
            ENDIF   
            LGTH = LNG(MSGTXT)+1
            WRITE(MSGTXT(LGTH:),'(2X,I4.4)') ISTAT
            LGTH = LNG(MSGTXT)
            CALL WRTMSG(3,191,12,1,1,MSGTXT,LGTH)
      ENDIF
      IF (FOUND) THEN
         LRECOUT=LRECOUT+1
         WRITE(51,REC=LRECOUT) IGRECSV,LSTID,LSTABRV
      ENDIF   
      WRITE(51,REC=1) LRECOUT-1
      CLOSE(35)
      RETURN
      END         