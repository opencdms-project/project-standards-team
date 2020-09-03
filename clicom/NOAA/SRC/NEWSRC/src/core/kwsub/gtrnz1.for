C
C
      SUBROUTINE GTRNZ1 (CRDIN,INDEF,TPARIN,CRDIO,IODEF,TPARIO,
     +                   IPFILE,IFLG,ICHK)   
C
      IMPLICIT  REAL (A-H,O-Z)
      INTEGER*4 INDEF(3),IODEF(3)
      DIMENSION CRDIN(1),CRDIO(1),TPARIN(1),TPARIO(1),COORD(2)
      COMMON /MAP/ MAPPRJ
C
      COORD(1) = CRDIN(1)
      COORD(2) = CRDIN(2)
C
      IF (ICHK.EQ.2) GOTO 200

C  ****************************************************************
C                                                 
C                        FORWARD TRANSFORMATION. 
C
C  ****************************************************************
C
  100  CONTINUE
       IF (MAPPRJ.EQ.17) GOTO 117
       IF (MAPPRJ.EQ.18) GOTO 118
       IF (MAPPRJ.EQ.25) GOTO 118
             GOTO 118
C
c
 117  CALL PF17Z0 (COORD,CRDIO,IPFILE,IFLG) 
      GO TO 900 
C
 118  CALL PF18Z0 (COORD,CRDIO,IPFILE,IFLG) 
      GOTO 900
c
C  *****************************************************************
C                                                 
C                        INVERSE TRANSFORMATION. 
C
C  *****************************************************************
C
  200 CONTINUE
       IF (MAPPRJ.EQ.17) GOTO 217
       IF (MAPPRJ.EQ.18) GOTO 218
       IF (MAPPRJ.EQ.25) GOTO 218
             GOTO 218
C
 217  CALL PI17Z0 (COORD,CRDIO,IPFILE,IFLG)   
      GO TO 500
C
 218  CALL PI18Z0 (COORD,CRDIO,IPFILE,IFLG)   
      GOTO 500
C
  500 CONTINUE
CCC   COORD(1) = CRDIO(1)
CCC   COORD(2) = CRDIO(2)
CCC   IF (IFLG .NE. 0) THEN
        DO 940 I = 1, 2
        CALL UNITZ0(CRDIO(I),IUNIT,CRDIO(I),IODEF(3),IPFILE,IFLG)
940     CONTINUE
        RETURN
CC      ENDIF
C
      IF (IODEF(1) .EQ. 0) GO TO 920 
CCCC  520 IF (IODEF(2).GT.60 .OR. IODEF(1).EQ.1) GO TO 900 
CCCKW           IF (IPFILE .NE. 0) WRITE (IPFILE,2050) IODEF(2)
CCCKW 2050 FORMAT (' ILLEGAL TARGET ZONE NUMBER = ',I6)  
      IFLG = 6
      RETURN 
C
  900  RETURN 
  920  RETURN 
C   
      RETURN 
      END 
