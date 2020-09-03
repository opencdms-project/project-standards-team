$STORAGE:2
C
C     PROGRAM CHKDOSDR
C
C  THIS PROGRAM READS COMMAND LINE PARAMETERS AND CHECKS THE FORMAT OF THE
C  DOS PROGRAM FILES SPECIFICATION.  
C
C ---------------------------------------------------------------------
      INTERFACE TO SUBROUTINE CMDLIN2(ADDRES,LENGTH,RESULT)
      INTEGER*4   ADDRES[VALUE],LENGTH[VALUE]
      CHARACTER*1 RESULT
      END
C ---------------------------------------------------------------------
      PROGRAM CHKDOSDR
C
      CHARACTER*80 MSG
      CHARACTER*40 RESULT
      CHARACTER*24 DOSLOC
      INTEGER*4    PSP, PSPNCHR, OFFSET
      LOGICAL      BADSPEC
C
C  LOCATE SEGMENTED ADDRESS OF THE BEGINNING OF THIS PROGRAM
C
      OFFSET = #00100000
      PSP = LOCFAR(CHKDOSDR)
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
C     DOSLOC    -- DOS PROGRAM FILES, DRIVE AND DIRECTORY 
C
      DOSLOC    = '     '
      DO 40 L=1,40
         IF (RESULT(L:L) .NE. ' ') THEN
            DOSLOC = RESULT(L:40)
            GO TO 50
         ENDIF
   40 CONTINUE
C
C  CHECK FORMAT OF DATAEASE PROGRAM DIRECTORY SPECIFICATION
C
   50 CONTINUE
      NLNG = LNG(DOSLOC)
      BADSPEC = .FALSE.
      BADSPEC = BADSPEC .OR. RESULT.EQ.' '
      IF (NLNG .LT. 3) THEN
         BADSPEC = BADSPEC .OR. DOSLOC(2:2).NE.':' .OR. NLNG.LE.1
      ELSE
         BADSPEC = BADSPEC .OR. DOSLOC(2:3).NE.':\'
         BADSPEC = BADSPEC .OR. DOSLOC(NLNG:NLNG).EQ.'\'
      ENDIF
      IF (BADSPEC) THEN
         CALL POSLIN(IR,IC)
         MSG = 'Error in DOS program directory specification.'
         IR = IR + 1
         CALL SCRNMSGI(MSG,IR,12)
         MSG = 'Data entered:  '//DOSLOC
         IR = IR + 1
         CALL SCRNMSGI(MSG,IR,14)
         MSG = 'The specification must have both the drive and '         
     +         // 'directory (e.g. C:\DOS)'
         IR = IR + 2
         CALL SCRNMSGI(MSG,IR,14)
         MSG = 'or just the drive letter (e.g. C:).'
         IR = IR + 1
         CALL SCRNMSGI(MSG,IR,14)
C         MSG = 'Processing stops here.  Select choice 3 or 6 from the'
C         IR = IR + 1
C         CALL SCRNMSGI(MSG,IR,14)
C         MSG = 'CLICOM Installation Menu to enter a revised value.'
C         IR = IR + 1
C         CALL SCRNMSGI(MSG,IR,14)
         MSG = ' '
         IR = IR + 1
         CALL SCRNMSGI(MSG,IR,14)
         STOP 3
      ENDIF   
C
  100 CONTINUE    
C---- A LENGTH OF 2 MEANS THAT THE USER ENTERED A ROOT DIRETCORY FOR THE DOS
C---- LOCATION.  SPECIAL PROCESSING IN *.MDF FILES, SO SET RETURN CODE, 2. 
      IF (NLNG .EQ. 2) THEN
         STOP 2
      ENDIF
      STOP ' '
      END        
                