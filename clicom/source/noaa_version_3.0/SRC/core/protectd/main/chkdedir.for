$STORAGE:2
C
C     PROGRAM CHKDEDIR
C
C  THIS PROGRAM READS COMMAND LINE PARAMETERS AND CHECKS THE FORMAT OF THE
C  DATAEASE PROGRAM FILES SPECIFICATION.  
C
C ---------------------------------------------------------------------
      INTERFACE TO SUBROUTINE CMDLIN2(ADDRES,LENGTH,RESULT)
      INTEGER*4   ADDRES[VALUE],LENGTH[VALUE]
      CHARACTER*1 RESULT
      END
C ---------------------------------------------------------------------
      PROGRAM CHKDEDIR
C
      CHARACTER*80 MSG
      CHARACTER*40 RESULT
      CHARACTER*24 DELOC
      INTEGER*4    PSP, PSPNCHR, OFFSET
      LOGICAL      BADSPEC
C
C  LOCATE SEGMENTED ADDRESS OF THE BEGINNING OF THIS PROGRAM
C
      OFFSET = #00100000
      PSP = LOCFAR(CHKDEDIR)
C
C  COMPUTE THE BEGINNING OF THE PROGRAM SEGMENT PREFIX (PSP)
C
      PSP = (PSP - MOD(PSP,#1000)) - OFFSET
C
C  LOCATE THE POSITION OF COMMAND PARAMETERS WITHIN PSP
C
      PSPNCHR = PSP + #80
      PSP = PSP + #81
C
C  PASS THE ADDRESS OF THE COMMAND PARAMETERS TO CMDLIN2 WHICH DECODES
C  THE COMMAND AND RETURNS IT AS RESULT.
C
      CALL CMDLIN2(PSP,PSPNCHR,RESULT)
C
C  PULL THE COMMANDS OUT OF THE RESULT
C     DELOC    -- DEASE PROGRAM FILES, DRIVE AND DIRECTORY 
C
      DELOC    = '     '
      DO 40 L=1,40
         IF (RESULT(L:L) .NE. ' ') THEN
            DELOC = RESULT(L:40)
            GO TO 50
         ENDIF
   40 CONTINUE
C
C  CHECK FORMAT OF DATAEASE PROGRAM DIRECTORY SPECIFICATION
C
   50 CONTINUE
      NLNG = LNG(DELOC)
      BADSPEC = .FALSE.
      BADSPEC = BADSPEC .OR. RESULT.EQ.' '
      BADSPEC = BADSPEC .OR. DELOC(2:3).NE.':\'
      BADSPEC = BADSPEC .OR. DELOC(4:4).EQ.' '
      BADSPEC = BADSPEC .OR. DELOC(NLNG:NLNG).EQ.'\ '
      IF (BADSPEC) THEN
         CALL POSLIN(IR,IC)
         MSG = 'Error in DATAEASE program or data drive specification.'
         IR = IR + 1
         CALL SCRNMSGI(MSG,IR,12)
         MSG = 'Data entered:  '//DELOC
         IR = IR + 1
         CALL SCRNMSGI(MSG,IR,14)
         MSG = 'Both the drive and directory name must be specified'
     +      //' for the location of the'
         IR = IR + 2
         CALL SCRNMSGI(MSG,IR,14)
         MSG = 'DATAEASE program or data files.  Use the following'
     +      //' sample format --  C:\DE'  
         IR = IR + 1
         CALL SCRNMSGI(MSG,IR,14)
C         MSG = 'Processing stops here.'
C         IR = IR + 1
C         CALL SCRNMSGI(MSG,IR,14)
         MSG = ' '
         IR = IR + 1
         CALL SCRNMSGI(MSG,IR,14)
         STOP 3
      ENDIF   
C
  100 CONTINUE    
      STOP ' '
      END        
                