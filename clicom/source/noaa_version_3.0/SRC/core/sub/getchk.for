$STORAGE:2
      SUBROUTINE GETCHK(RECTYPE)
C
C   ROUTINE TO LOAD THE ELEMCHKS QC-CONTROL FILE INFORMATION INTO THE
C   COLUMNS APPROPRIATE FOR EACH ELEMENT
C
C   NOTE: THIS ROUTINE READS THE ELEMCHKS.DAT FILE AS UNIT 5 - THE SAME
C         UNIT AS IS USED TO READ THE SETUP FILE (BUT A DIFFERENT FILE)
C
$INCLUDE: 'VAL1.INC'
$INCLUDE: 'ELEMCHKS.INC'
C
      CHARACTER*1 RTNCODE
      CHARACTER*2 INCKL1,INCKL2
      CHARACTER*3 RECTYPE
      CHARACTER*10 CMDVAL(7)
      CHARACTER*22 CHKFILE
      CHARACTER*24 MESSAGE
      CHARACTER*80 STRING      
      INTEGER*2 INCHK,INELM1,INELM2,CHKCNT(MAXELEM)
      REAL*4    INCKV1,INCKV2
      LOGICAL ERROR
      DATA CHKFILE /'P:\DATA\ELEMCHKS.DAT'/
C
      DO 30 IELEM = 1,MAXELEM
         CHKCNT(IELEM) = 0
         DO 30 ICHK = 1,MAXCHK
            CHKTYP(IELEM,ICHK) = 0
   30 CONTINUE            
   40 CONTINUE
C
C   DETERMINE AND OPEN THE QC COMMAND FILE (SKIP THE 1ST LINE)
C
      ERROR = .FALSE.
C
      CHKFILE(18:20) = RECTYPE
      IF (RECTYPE.EQ.'SYN') THEN
         CHKFILE(18:20) = 'HLY'
      END IF
      OPEN (5,FILE=CHKFILE,STATUS='OLD',FORM='FORMATTED'
     +       ,IOSTAT=IOCHK)
      IF (IOCHK.NE.0) THEN
         CALL OPENMSG(CHKFILE,'GETCHK      ',IOCHK)
         GO TO 40
      END IF
      READ(5,'(1X)',END=85)
C
C   READ THE COMMAND FILE AND PARSE (COMMMA DELIMITTED)
C
      DO 80 J1 = 2,9999
         READ (5,'(A78)',END=85) STRING
         CALL PARSE1(STRING,78,7,10,CMDVAL,RTNCODE)
         IF (RTNCODE.NE.'0'.AND.RTNCODE.NE.'2') THEN
            GO TO 75
         END IF
         READ(CMDVAL(1),'(I3)',ERR=75) INELM1
         READ(CMDVAL(2),'(I2)',ERR=75) INCHK 
C
C     MAKE SURE ALL REQUIRED FIELDS ARE PRESENT - OTHERWISE IT'S
C     PROBABLY A FORMAT ERROR
C
         IF (CMDVAL(4).EQ.'   '.OR.CMDVAL(7).EQ.'   ') THEN
            IF (INCHK.LE.03.OR.INCHK.EQ.06.OR.INCHK.EQ.08.OR.
     +          INCHK.EQ.53.OR.INCHK.EQ.54) THEN
               GO TO 75
            END IF
         END IF
         IF (INCHK.GE.03.AND.INCHK.LE.05) THEN
            IF (CMDVAL(3).EQ.' '.OR.CMDVAL(5).EQ.' ') THEN
               GO TO 75    
            END IF
         END IF
         READ(CMDVAL(3),'(A2)') INCKL1
         READ(CMDVAL(4),'(F8.0)',ERR=75) INCKV1
         READ(CMDVAL(5),'(I3)',ERR=75) INELM2
         READ(CMDVAL(6),'(A2)') INCKL2
         READ(CMDVAL(7),'(F8.0)',ERR=75) INCKV2
C  
C    SET THE CHECKS INTO THE FIELDS THAT ARE USED BY LINEQC
C
         DO 60 IELEM = 1,NUMELEM
            IF (INELM1.EQ.TBLELEM(IELEM)) THEN
               IF (CHKCNT(IELEM).GT.MAXCHK-1) THEN
                  WRITE(MESSAGE,'(A20,1X,I3)') CHKFILE,J1
                  CALL LOCATE(21,0,IERR)
                  CALL WRTSTR(STRING,78,14,0)
                  CALL WRTMSG(3,167,12,1,1,MESSAGE,24)
                  GO TO 60
               END IF
               IF (INCHK.GE.3.AND.INCHK.LE.5) THEN
                  IF (INELM2.LE.0) THEN
                     GO TO 60
                  ELSE
                     DO 50 J = 1,NUMELEM
                       IF (INELM2.EQ.TBLELEM(J)) THEN
                          INELM2 = J
                          GO TO 55
                       END IF
  50                 CONTINUE
                     GO TO 60
  55                 CONTINUE
                  END IF
               END IF                       
               CHKCNT(IELEM) = CHKCNT(IELEM) + 1
               ICHK = CHKCNT(IELEM)
               CHKTYP(IELEM,ICHK) = INCHK
               CHKRL1(IELEM,ICHK) = INCKL1
               CHKVL1(IELEM,ICHK) = INCKV1
               CHKELM(IELEM,ICHK) = INELM2
               CHKRL2(IELEM,ICHK) = INCKL2
               CHKVL2(IELEM,ICHK) = INCKV2
            END IF
   60    CONTINUE       
         GO TO 80
C
C    WRITE ERROR MESSAGES IF FORMAT ERROR
C
   75    CONTINUE
         WRITE(MESSAGE,'(A20,1X,I3)') CHKFILE,J1
         CALL LOCATE(21,0,IERR)
         CALL WRTSTR(STRING,78,14,0)
         CALL WRTMSG(3,168,12,1,1,MESSAGE,24)
         ERROR = .TRUE.
   80 CONTINUE
C
C    NORMAL STOP
C
   85 CONTINUE
      CLOSE(5)
      IF (ERROR) THEN
         STOP 2
      END IF
      RETURN
      END
         