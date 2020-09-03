$STORAGE:2
      SUBROUTINE HALOINIT(RTNCODE)
C------------------------------------------------------------------------------
C     INITIALIZE THE HALO GRAPHICS ENVIRONMENT VARIABLES IN COMMON BLOCK
C     HALOENV WITH THE VALUES FROM A USER'S HALO ENVIRONMENT FILE.
C
C     OUTPUT ARGUMENT:
C
C     RTNCODE  CHAR  STATUS OF ATTEMPT TO INITIATE GRAPHICS. 0=OK, 1=ERROR
C------------------------------------------------------------------------------
$INCLUDE:'HALOENV.INC'
C
      CHARACTER*1  HALOID, RTNCODE
      LOGICAL FIRSTCALL
      DATA FIRSTCALL/.TRUE./
C
      IF (FIRSTCALL) THEN
         FIRSTCALL = .FALSE.      
         HALOID = ' '
         CALL GETHAL (HALOID)
         IF (HALOID .EQ. ' ') THEN
            RTNCODE = '1'
         ELSE
            ENVFILE = 'P:\HALO\HALOGRF .ENV'
            ENVFILE(16:16) = HALOID
         ENDIF
      ENDIF   
C      
      OPEN (UNIT=44,FILE=ENVFILE,FORM='FORMATTED')
      READ(44,*) DEVICE,DEVMODE,AQCMODE,NETWORK,ACTVPTR,
     +         ((PRINTR(I,J),I=1,2),(PTRVAL(I,J),I=0,MXPTRV),
     +           UNTFLG(J),CLRFLG(J),CLRMOD(J),PTRASP(J),J=1,2)
      CLOSE (44)
      VRI = '^P:\HALO\DRIVERS\AHDVRI.DSP^'
      RTNCODE = '0'
      RETURN
      END
