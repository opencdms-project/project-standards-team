$STORAGE:2
      SUBROUTINE MAPCOORD(IROW,XLATMN,XLATMX,XLONMN,XLONMX,RTNFLAG)
C
C   ROUTINE TO RETRIEVE MAP CORNER COORDINATES IN FLOATING POINT
C   DEGREES FROM THE USER.
C
      INTEGER*2   IDEG,IMIN,ISEC
      REAL*4      XLATLON(4)
      LOGICAL     NZFLG
      CHARACTER*4 FIELD(12), NEGZERO(8)
      CHARACTER*2 RTNFLAG
      DATA  NEGZERO/'-   ','-  0','- 0 ','- 00','-0  ','-0 0','-00 ',
     +              '-000'/
C
C  CONVERT COORDINATES FROM HUNDRETHS TO MINUTES FOR DISPLAY
C  SPECIAL FORMATING REQUIRED FOR A LAT/LON VALUE OF -0 DEGREES AND X MINUTES
C  BECAUSE THERE IS NO SUCH THING AS A NEGATIVE ZERO IN INTEGER FORMAT
C     
      XLATLON(1) = XLATMN 
      XLATLON(2) = XLATMX  
      XLATLON(3) = XLONMN  
      XLATLON(4) = XLONMX  
C
      J1 = -2
      DO 20 J=1,4
         J1 = J1 + 3
         IDEG = INT(XLATLON(J))
         X    = (ABS(XLATLON(J)-FLOAT(IDEG))) * 60.
         IMIN = INT(X)
         X    = X - FLOAT(IMIN)
         ISEC = NINT(X * 60.)
         IF (J.LT.3) THEN
            WRITE(FIELD(J1),'(I3.2)') IDEG
         ELSE
            WRITE(FIELD(J1),'(I4.3)') IDEG
         ENDIF
         WRITE(FIELD(J1+1),'(I2.2)') IMIN
         WRITE(FIELD(J1+2),'(I2.2)') ISEC
         IF (XLATLON(J) .LT. 0.0 .AND. IDEG.EQ.0) THEN
            FIELD(J1)(1:1) = '-'
         ENDIF
   20 CONTINUE         
      
C
C  ASK FOR THE MAP BOUNDARIES IN LATITUDE AND LONGITUDE
C
60    CONTINUE
      CALL LOCATE(IROW,5,IERR)
      CALL GETFRM('MAPCOORD',' ',FIELD,4,RTNFLAG)
      IF (RTNFLAG.EQ.'4F') THEN
         RETURN
      END IF
C
C       ** CONVERT THE LAT/LON COORDINATES FROM DEGREES AND MINUTES TO 
C          REAL DEGREES
C         
      J1 = -2
      DO 80 J=1,4
         J1 = J1 + 3
C          ..  SPECIAL CHECK TO DETECT -0 DEGREES 
         NZFLG = .TRUE.
         DO 70 K=1,8
            IF (FIELD(J1).EQ.NEGZERO(K)) GO TO 72
   70    CONTINUE
         NZFLG = .FALSE.
   72    CONTINUE   
C
         IF (J.LT.3) THEN
            READ(FIELD(J1),   '(BN,I3)') IDEG
         ELSE   
            READ(FIELD(J1),   '(BN,I4)') IDEG
         ENDIF   
         READ(FIELD(J1+1), '(BN,I2)') IMIN
         READ(FIELD(J1+2), '(BN,I2)') ISEC
C         
         IF (IDEG.LT.0 .OR. NZFLG) THEN
            XLATLON(J) = FLOAT(IDEG)-FLOAT(IMIN)/60.-FLOAT(ISEC)/3600.
         ELSE
            XLATLON(J) = FLOAT(IDEG)+FLOAT(IMIN)/60.+FLOAT(ISEC)/3600.
         END IF
   80 CONTINUE                    
C   
      XLATMN = XLATLON(1)
      XLATMX = XLATLON(2)
      XLONMN = XLATLON(3)
      XLONMX = XLATLON(4)
C
C
C   MAKE SURE UPPER LIMITS ARE >= THE LOWER LIMITS
C
      IF (XLONMN.GE.XLONMX) THEN
         CALL WRTMSG(2,253,12,1,0,' ',0)
         GO TO 60
      ELSE IF (XLATMN.GE.XLATMX) THEN
         CALL WRTMSG(2,254,12,1,0,' ',0)
         GO TO 60
      END IF
      RETURN
      END
