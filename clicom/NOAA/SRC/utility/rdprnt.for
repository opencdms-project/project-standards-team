$STORAGE:2

      SUBROUTINE RDPRNT(SETNRM,SETLRG,SETSML,STRTUND,STPUND,LINCHR,
     +                  SETLJLPI,LJRESET)
C
C   ROUTINE TO READ THE PRINTER CONFIGURATION FILE AND RETURN 
C   THE PRINTER CONTROL CODES
C
      CHARACTER*1  SETNRM(8),SETLRG(8),SETSML(8),STRTUND(8),STPUND(8)
     +            ,SETLJLPI(8),LJRESET(8),LINCHR(11),HOLD1,HOLD2
      INTEGER*2    INCODE(7,8)
C
C   THE 11 LINCHR VARIABLES HOLD THE CHARACTERS TO BE USED FOR THE
C   FOLLOWING CHARACTERS RESPECTIVELY
C                                   ³ Ä Ú ¿ Á Â À Ù Ã ´ Å 
C 
      DO 20 J = 1,8
         SETNRM(J) = CHAR(0)
         SETLRG(J) = CHAR(0)
         SETSML(J) = CHAR(0)
         STRTUND(J) = CHAR(0)
         STPUND(J) = CHAR(0)
         SETLJLPI(J)=CHAR(0)
         LJRESET(J)=CHAR(0)
   20 CONTINUE
C
   30 CONTINUE
      OPEN (7,FILE='P:\DATA\PRINTER.CFG',STATUS='OLD',SHARE='DENYWR'
     +       ,MODE='READ',IOSTAT=IOCHK)
      IF(IOCHK.NE.0.) THEN
         CALL OPENMSG('P:\DATA\PRINTER.CFG   ','RDPRNT      ',IOCHK)
         GO TO 30
      END IF   
C
      READ(7,'(2A1)')HOLD1,HOLD2
      READ(7,'(56I3)',END=110) ((INCODE(M,N),N=1,8),M=1,7)
      IF (HOLD1.EQ.'H'.OR.HOLD1.EQ.'D')THEN
         DO 50 J = 1,8
            SETNRM(J) = CHAR(INCODE(1,J))
            SETLRG(J) = CHAR(INCODE(2,J))
            SETSML(J) = CHAR(INCODE(3,J))
            STRTUND(J) = CHAR(INCODE(4,J))
            STPUND(J) = CHAR(INCODE(5,J))
            SETLJLPI(J)=CHAR(INCODE(6,J))
            LJRESET(J)=CHAR(INCODE(7,J))
   50    CONTINUE
      ELSE
         DO 70 K = 1,8
            SETNRM(K) = ' '
            SETLRG(K) = ' '
            SETSML(K) = ' '
            STRTUND(K) = ' '
            STPUND(K) = ' '
            SETLJLPI(K)=' '
            LJRESET(K)=' '
   70    CONTINUE
      END IF        
C
      READ(7,'(11A1)') (LINCHR(I),I=1,11)
  110 CONTINUE
      CLOSE(7)
      RETURN
      END  
