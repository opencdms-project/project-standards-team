$STORAGE:2

      SUBROUTINE WRTVAL(INVAL,DATAFLAG,IROW,ICOL)
C
C   ROUTINE TO WRITE A DATA VALUE - INCLUDES CODE TO WRITE VALUES IN 
C      DIFFERENT FOREGROUND AND BACKGROUND COLORS DEPENDING ON THE
C      VALUE OF DATAFLAG
C
      CHARACTER*6 INVAL
      CHARACTER*1 DATAFLAG 
      INTEGER*2 BLACK,BLUE,CYAN,RED,BOLDYELLOW,BOLDWHITE
     +         ,HDRLTR,NRMLTR,WRNLTR,ERRLTR,NRMBG,WRNBG,ERRBG
      DATA BLACK/0/, BLUE/1/, CYAN/3/, RED/4/, BOLDYELLOW/14/,
     +     BOLDWHITE/15/
C
      CALL LOCATE(IROW,ICOL,IERR)
C
C   SET CHARACTERS FOR COLOR MODE
C
         HDRLTR = BOLDYELLOW
         NRMLTR = BOLDWHITE
         WRNLTR = BOLDWHITE
         ERRLTR = BOLDWHITE
         NRMBG = BLUE
         WRNBG = CYAN
         ERRBG = RED 
C
      IF (DATAFLAG.EQ.'#') THEN
         CALL WRTSTR(INVAL,6,HDRLTR,BLACK)
      ELSE IF (DATAFLAG.EQ.' '.OR.DATAFLAG.EQ.'A'.OR.
     +         DATAFLAG.EQ.'a'.OR.DATAFLAG.EQ.'e') THEN
         CALL WRTSTR(INVAL,6,NRMLTR,NRMBG)
      ELSE IF (DATAFLAG.EQ.'B'.OR.DATAFLAG.EQ.'D'.OR.
     +         DATAFLAG.EQ.'b'.OR.DATAFLAG.EQ.'d'.OR.
     +         DATAFLAG.EQ.'f') THEN
         CALL WRTSTR(INVAL,6,WRNLTR,WRNBG)
      ELSE IF (DATAFLAG.EQ.'C'.OR.DATAFLAG.EQ.'c') THEN
         CALL WRTSTR(INVAL,6,ERRLTR,ERRBG)
      ELSE
         CALL WRTMSG(3,54,12,1,0,DATAFLAG,1)
         CALL WRTSTR(INVAL,6,ERRLTR,ERRBG)
      END IF
C
      RETURN
      END

