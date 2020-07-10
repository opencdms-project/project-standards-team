$STORAGE:2
      SUBROUTINE TOTQC(RECTYPE,HOURLBL,STRTELEM,STRTLINE,RTNCODE)
C
C  ROUTINE TO COMPUTE FORM TOTALS FOR THOSE ELEMENTS THAT HAVE ONE OF
C  THE FORM TOTAL QC CHECKS SPECIFIED AND DISPLAY OR CHECK THE TOTALS
C  AS APPROPRIATE
C
$INCLUDE: 'VAL1.INC'
$INCLUDE: 'ELEMCHKS.INC'
      PARAMETER (MAXX = 10)
C
      CHARACTER*8 RLTOTAL
      CHARACTER*6 DSPTOT
      CHARACTER*2 HOURLBL
      CHARACTER*3 RECTYPE
      CHARACTER*1 RTNCODE,REPLY
      INTEGER*2 STRTELEM,STRTLINE
     +         ,CHK02(MAXELEM),CHK07(MAXELEM),CHK54(MAXELEM)
      INTEGER*4 FRMTOT(MAXELEM),IVALUE
      LOGICAL TOTFND(MAXELEM), DISPLY, TOTDSP
C
      RTNCODE = '0'
      DO 40 I = 1,NUMELEM
         FRMTOT(I) = 0
         CHK02(I) = 0
         CHK07(I) = 0
         CHK54(I) = 0
         TOTFND(I) = .FALSE.
   40 CONTINUE         
C
C  FIND THE TYPES OF TOTALS WANTED
C
      DO 60 IELEM = 1,NUMELEM
         DO 50 ICHK = 1,MAXCHK
            JCHK = CHKTYP(IELEM,ICHK)
            IF (JCHK.EQ.0) THEN
               GO TO 60
            END IF
            IF (JCHK.EQ.02) THEN
               CHK02(IELEM) = ICHK
            ELSE IF (JCHK.EQ.07) THEN
               CHK07(IELEM) = ICHK
            ELSE IF (JCHK.EQ.54) THEN
               CHK54(IELEM) = ICHK
            END IF
   50    CONTINUE
   60 CONTINUE            
C
C  COMPUTE FORM TOTALS FOR EACH ELEMENT REQUESTED
C
      DO 150 IELEM = 1,NUMELEM
         IF (CHK02(IELEM)+CHK07(IELEM)+CHK54(IELEM).GT.0) THEN
            DO 100 ILINE = 1,NUMLINE
               IF (VALARRAY(IELEM,ILINE).NE.'      ')THEN
                  READ(VALARRAY(IELEM,ILINE),'(I5)') IVALUE
                  FRMTOT(IELEM) = FRMTOT(IELEM) + IVALUE
                  TOTFND(IELEM) = .TRUE.
               END IF
  100       CONTINUE
         END IF
  150 CONTINUE
      IROW = 21         
      DISPLY = .TRUE.
C
C   FIND IF THE FIRST TOTAL TO BE DISPLAYED IS ON THE FORM - IF NOT, 
C   CHANGE STRTELEM AND REDISPLAY THE FORM
C
      DO 160 IELEM = 1,NUMELEM
         IF (CHK07(IELEM).GT.0) THEN
            IF (IELEM.LT.STRTELEM.OR.IELEM.GT.(STRTELEM+MAXX-1))THEN
               STRTELEM = 5 * (((IELEM - 1) / 5) - 1) + 1
               IF (STRTELEM.LT.1) THEN
                  STRTELEM = 1
               END IF 
               DISPLY = .FALSE.
            END IF
            GO TO 170
         END IF
  160 CONTINUE
  170 CONTINUE
C
C   DISPLAY A FORM WITH THE TOTALS DISPLAYED (MAX OF MAXX COLUMNS)
C   AND ASK USER IF TOTALS ARE OK
C
  180 CONTINUE      
      TOTDSP = .FALSE.
      DO 200 I = 1,MAXX
         IELEM = STRTELEM + I - 1
         IF (IELEM.LE.NUMELEM) THEN
            IF (TOTFND(IELEM).AND.CHK07(IELEM).GT.0) THEN
               TOTDSP = .TRUE.
               IF (.NOT.DISPLY) THEN
                  CALL WRTPAGE(RECTYPE,HOURLBL,STRTELEM,STRTLINE)
                  DISPLY = .TRUE.
               END IF            
               ICOL = (I-1)*7 + 7
               WRITE(DSPTOT,'(I5,1X)') FRMTOT(IELEM)
               CALL WRTVAL(DSPTOT,'C',IROW,ICOL) 
            END IF
         END IF
  200 CONTINUE         
      IF (TOTDSP) THEN
         CALL LOCATE(22,0,IERR)
         CALL DSPERR('_','C',0,I)
         REPLY = ' '
         CALL BEEP
         CALL LOCATE(23,1,IERR)
         CALL OKREPLY(REPLY,RTNCODE)
         CALL CLRMSG(4)
         CALL CLRMSG(3)
         CALL CLRMSG(2)
         IF (REPLY.EQ.'N') THEN   
            RTNCODE = '1'
            RETURN
         END IF
      END IF
      IF (IELEM.LT.NUMELEM) THEN
         STRTELEM = STRTELEM + 10
         DISPLY = .FALSE.
         GO TO 180
      END IF
C
C   COMPARE THE COMPUTED TOTALS WITH GLOBAL OR STN LIMITS IF REQUESTED
C      
      DO 300 I = 1,NUMELEM
         IF (TOTFND(I).AND.CHK02(I)+CHK54(I).GT.0) THEN
            TOTAL = FLOAT(FRMTOT(I)) * TBLCONV(I)
            REPLY = ' '
            IF (CHK02(I).GT.0) THEN
               IF (TOTAL.GT.CHKVL2(I,CHK02(I))) THEN
                  REPLY = 'E'
               ELSE IF (TOTAL.LT.CHKVL1(I,CHK02(I))) THEN
                  REPLY = 'D'
               END IF
            END IF
            IF (CHK54(I).GT.0) THEN
               IF (TBLSTDDEV(I).LT.0) THEN
                  IF (TOTAL.GT.TBLMEAN(I)*CHKVL1(I,CHK54(I))) THEN
                     REPLY = 'Y'
                  END IF
               END IF
            END IF
            IF (REPLY.NE.' ') THEN
               CALL DSPERR(REPLY,'C',0,I)
               CALL LOCATE(22,60,IERR)
               CALL WRTSTR(TBLEABRV(I),6,12,0)
               TOTAL = FLOAT(FRMTOT(I)) * TBLCONV(I)
               WRITE(RLTOTAL,'(F8.2)') TOTAL
               CALL WRTSTR(' ',1,12,0)
               CALL WRTSTR(RLTOTAL,8,12,0) 
               CALL BEEP
               REPLY = ' '
               CALL LOCATE(23,1,IERR)
               CALL OKREPLY(REPLY,RTNCODE)
               CALL CLRMSG(3)
               CALL CLRMSG(2)
               IF (REPLY.EQ.'N') THEN   
                  RTNCODE = '1'
                  RETURN
               END IF
            END IF
         END IF
  300 CONTINUE      
      RETURN
      END               
      