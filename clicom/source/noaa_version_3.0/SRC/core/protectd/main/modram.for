$STORAGE:2
C
C   PROGRAM MODRAM
C
C     PROGRAM TO ENTER THE COMMANDS NECESSARY TO CREATE A RAM DISK IN THE
C     USERS CONFIG.SYS FILE.  THE PATH TO DOS AND THE TYPE OF DOS (IBM-PC
C     OR MS-DOS) ARE PASSED AS PARAMETERS.
C
C     DEVICE=XXX\VDISK.SYS 512 /E      (IBM-PC DOS SYNTAX FOR DOS VERSION < 5)
C     DEVICE=XXX\RAMDRIVE.SYS 512 /E   (MS-DOS SYNTAX)
C
      INTERFACE TO SUBROUTINE CMDLIN2(ADDRES,LENGTH,RESULT)
      INTEGER*4 ADDRES[VALUE],LENGTH[VALUE]
      CHARACTER*1 RESULT
      END
C.----------------------------------------------------------------------
      PROGRAM MODRAM
C
      CHARACTER*80 MSG
      CHARACTER*79 LINE,TXTHMEM,TEXT(200),TXTOUT
      CHARACTER*40 RESULT
      CHARACTER*24 DOSLOC
      CHARACTER*6  OUTFRM
      CHARACTER*2 DOSTYP
      INTEGER*4 PSP,PSPNCHR,OFFSET
      INTEGER*2 LRAM,LVDSK,LHMEM,MAJVER,MINVER
      LOGICAL MODFILE
C
      DATA LRAM,LVDSK,LHMEM /3*0/, MODFILE /.FALSE./
C
C   LOCATE SEGMENTED ADDRESS OF THE BEGINNING OF THIS PROGRAM
C
      OFFSET = #00100000
      PSP = LOCFAR(MODRAM)
C
C   COMPUTE THE BEGINNING OF THE PROGRAM SEGMENT PREFIX (PSP)
C   (THE CONSTANT IN NEXT STMT COULD ALSO BE #10000  25-SEP-91)
C
      PSP = (PSP - MOD(PSP,#1000)) - OFFSET 
C
C   LOCATE POSITION OF COMMAND PARAMTERS WITHIN THE PSP
C
      PSPNCHR = PSP + #80
      PSP = PSP + #81
C
C   PASS THE ADDRESS OF THE COMMAND PARAMTERS TO CMDLIN2 WHICH DECODES
C      THE COMMAND AND RETURNS IT AS RESULT.
C
      CALL CMDLIN2(PSP,PSPNCHR,RESULT)
      IF (RESULT.EQ.' ') GO TO 900
C
C   PULL THE COMMANDS (RAMDISK AND DOSLOC) OUT OF THE RESULT
C
      DOSTYP  = RESULT(1:2)
      DOSLOC  = RESULT(4:40)
C
C   OPEN THE EXISTING CONFIG.SYS FILE OR OPEN A NEW ONE IF THERE IS NONE
C
  50  CONTINUE
      OPEN (75,FILE='C:\CONFIG.SYS',STATUS='OLD',FORM='FORMATTED'
     +      ,IOSTAT=IOCHK)
      IF (IOCHK.NE.0) THEN
         CALL POSLIN(IR,IC)
         MSG = ' '
         IR = IR + 1
         CALL SCRNMSGI(MSG,IR,14)
         MSG = 'Error opening CONFIG.SYS file. No changes can be made.'
         IR = IR + 1
         CALL SCRNMSGI(MSG,IR,12)
         GO TO 900
      ENDIF
C
C   ASK THE USER FOR HIS VERSION OF DOS (NEEDED TO SET THE ENVIRONMENT
C   SPACE USING THE SHELL COMMAND)
C
   40 CONTINUE
         CALL POSLIN(IR,IC)
         MSG = ' '
         IR = IR + 1
         CALL SCRNMSGI(MSG,IR,14)
C
C     CHECK THE EXISTING LINES ONE BY ONE AND LOOK FOR THE VDISK OR
C     RAMDRIVE COMMAND.  IF FOUND, NO MODIFICATIONS TO CONFIG.SYS WILL
C     BE DONE.  IF NOT FOUND, ADD THE CORRECT FORMAT FOR A RAM DISK TO
C     THE END OF THE CONFIG.SYS FILE. 
C
         NBREM = 0
         DO 100 I = 1,199
C
C             .. READ THE CONFIG.SYS FILE; CONVERT CHARACTERS TO UPPER CASE;
C                SAVE UPPER CASE VERSION OF LINE  
            READ(75,'(A79)',END=120) LINE
            CALL LOW2UP(LINE)
            TEXT(I) = LINE
C            
            CALL SEARCH(I,LINE,'REM     ',NBREM,J)
            IF (NBREM .EQ. 0 ) THEN
               J = 0
               CALL SEARCH(I,LINE,'HIMEM   ',LHMEM,J)
               CALL SEARCH(I,LINE,'VDISK   ',LVDSK, J)
               CALL SEARCH(I,LINE,'RAMDRIVE',LRAM,  J)
            ELSE
               NBREM = 0
            ENDIF
  100    CONTINUE
  120    CONTINUE
         NBRLINE = I -1
         REWIND(75)
C
         CALL DOSVER(MAJVER,MINVER)
         IF (LVDSK.EQ.0 .AND. LRAM.EQ.0) THEN
            MODFILE = .TRUE.
            IF (MAJVER.GE.5 .AND. LHMEM.EQ.0) THEN
               TXTHMEM = 'DEVICE='
               MLNG = LNG(DOSLOC)
               TXTHMEM(8:8+MLNG-1) = DOSLOC
               MLNG = LNG(TXTHMEM)
               IF (TXTHMEM(MLNG:MLNG) .EQ. '\') THEN
                  MLNG = MLNG - 1
               ENDIF               
               TXTHMEM(MLNG+1:MLNG+10) = '\HIMEM.SYS'
            ELSE   
               TXTHMEM = ' '
            ENDIF        
            LINE = 'DEVICE='
            MLNG = LNG(DOSLOC)
            LINE(8:8+MLNG-1) = DOSLOC
            MLNG = LNG(LINE)
            IF (LINE(MLNG:MLNG) .EQ. '\') THEN
               MLNG = MLNG - 1
            ENDIF               
            IF (DOSTYP.EQ.'PC' .AND. MAJVER.LT.5) THEN
               LINE(MLNG+1:MLNG+21) = '\VDISK.SYS 384 512 /E'
            ELSE
               LINE(MLNG+1:MLNG+24) = '\RAMDRIVE.SYS 384 512 /E'
            ENDIF
         ENDIF
         CALL POSLIN(IR,IC)
         MSG = ' '
         IR = IR + 1
         CALL SCRNMSGI(MSG,IR,14)
         IF (MODFILE) THEN
            MSG = 'The RAM disk command : ' // LINE
            IR = IR + 1
            CALL SCRNMSGI(MSG,IR,14)
            MSG =  'has been added to your CONFIG.SYS file.'
            IR = IR + 1
            CALL SCRNMSGI(MSG,IR,14)
            MSG = ' '
            IR = IR + 1
            CALL SCRNMSGI(MSG,IR,14)
C
C             ** WRITE THE CONFIG.SYS FILE
C
C             .. IF REQUIRED, ADD FIRST LINE:  DEVICE=HIMEM.SYS
            IF (TXTHMEM .NE. ' ') THEN
               CALL GETFRMT(TXTHMEM,OUTFRM)
               WRITE(75,OUTFRM) TXTHMEM
            ENDIF
C
C             .. COPY THE LINES IN THE ORIGINAL CONFIG.SYS FILE
            DO 80 I=1,NBRLINE
               TXTOUT = TEXT(I)
               CALL GETFRMT(TXTOUT,OUTFRM)
               WRITE(75,OUTFRM) TXTOUT            
   80       CONTINUE
C
C             .. ADD LINE FOR RAM DRIVE AT THE END   
            CALL GETFRMT(LINE,OUTFRM)
            WRITE(75,OUTFRM) LINE
         ELSE
            MSG = 'A RAM disk command already exists in your '
     +            // 'CONFIG.SYS file.'       
            IR = IR + 1
            CALL SCRNMSGI(MSG,IR,14)
            MSG = 'No modifications are necessary.'
            IR = IR + 1
            CALL SCRNMSGI(MSG,IR,14)
            MSG = ' '
            IR = IR + 1
            CALL SCRNMSGI(MSG,IR,14)
         ENDIF
      CLOSE (75)
      STOP ' '
C
C       ** ERROR EXIT
C
  900 CONTINUE
      CALL POSLIN(IR,IC)
      IR = IR + 1
      MSG = 'Stop execution due to unrecoverable errors'
      CALL SCRNMSGI(MSG,IR,12)
      MSG = ' '
      IR = IR + 1
      CALL SCRNMSGI(MSG,IR,14)
      STOP 5
      END
************************************************************************
      SUBROUTINE SEARCH(I,LINE,TEXT,I1,JCOL)
C
C   ROUTINE TO SEARCH A LINE FOR MATCHING TEXT 
C
C   I.......FILE LINE NUMBER BEING SEARCHED
C   LINE....LINE OF TEXT TO BE SEARCHED (CHAR*79)
C   TEXT....TEXT SEARCH STRING TO BE MATCHED (CHAR*8) LEFT-JUSTIFIED
C   I1......SET = TO I ON OUTPUT IF MATCH IS FOUND
C   JCOL....SET TO THE COLUMN THE MATCHING TEXT BEGINS IF FOUND
C   
      CHARACTER*80 MSG
      CHARACTER*79 LINE
      CHARACTER*8 TEXT
C
C   FIRST DETERMINE HOW LONG THE TEXT STRING IS (SEARCH BACK FROM RIGHT
C   FOR FIRST NON-BLANK CHARACTER
C
      LEN = 0
      DO 100 J = 8,1,-1
         IF (TEXT(J:J).NE.' ') THEN
            LEN = J
            GO TO 110
         ENDIF
  100 CONTINUE
  110 CONTINUE
      IF (LEN.EQ.0) THEN
            CALL POSLIN(IR,IC)
            MSG = 'TEXT STRING IN SEARCH IS EMPTY.'
            IR = IR + 1
            CALL SCRNMSGI(MSG,IR,14)
         RETURN
      ENDIF
C
C   SCAN THE INPUT LINE FOR A MATCH WITH THE TEXT WANTED, IF MATCH 
C   SET I1 AND JCOL.
C
      DO 120 J = 1,79-LEN+1
         IF (LINE(J:J+LEN-1).EQ.TEXT(1:LEN)) THEN
            I1 = I
            JCOL = J
            RETURN
         ENDIF
  120 CONTINUE
      RETURN
      END

      