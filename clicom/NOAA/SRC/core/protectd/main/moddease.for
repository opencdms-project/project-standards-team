$STORAGE:2
C
C     PROGRAM MODDEASE
C
C  THIS PROGRAM READS COMMAND PARAMETERS AND WRITES THE APPROPRIATE
C  DATAEASE COMMANDS TO 8 BATCH FILES:
C       1. FRM-ARCH.BAT 
C       2. SORT2.BAT
C       3. MRG-ARC2.BAT
C       4. MERGE2.BAT
C       5. CLICOM2.BAT
C       6. CLICOM2.MON
C       7. DATAPROD.BAT
C       8. DATAPRD2.BAT
C
C  PASSED PARAMETER ARE:
C     %CLIPGDRV%  = CLICOM DRIVE
C           %2    = DATAEASE DIRECTORY
C           %3    = DATEASE VERSION
C           %4    = MEMORY TYPE (REGULAR OR EXTENDED) 
C ---------------------------------------------------------------------
      INTERFACE TO SUBROUTINE CMDLIN2(ADDRES,LENGTH,RESULT)
      INTEGER*4   ADDRES[VALUE],LENGTH[VALUE]
      CHARACTER*1 RESULT
      END
C ---------------------------------------------------------------------
      PROGRAM MODDEASE
C
      CHARACTER*80 MSG     
      CHARACTER*79 INBAT(300)
      CHARACTER*40 RESULT
      CHARACTER*30 FILEIN(8)
      CHARACTER*24 DELOC
      CHARACTER*9  DEDIR
      CHARACTER*6  OUTFRM
      CHARACTER*3  DEVERS
      CHARACTER*2  CLIDRV, DEDRIVE
      CHARACTER*1  MEMTYP,RTNCODE, DETYPE
      INTEGER*4    PSP, PSPNCHR, OFFSET
      LOGICAL      MISSFILE(8),ERRFLAG(8),FLAG
      DATA         FLAG/.FALSE./
C
C  LOCATE SEGMENTED ADDRESS OF THE BEGINNING OF THIS PROGRAM
C
      OFFSET = #00100000
      PSP = LOCFAR(MODDEASE)
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
C  PULL THE COMMANDS OUT OF THE RESULT :
C    DELOC    --   DEASE PROGRAM FILES, DRIVE AND DIRECTORY
C    MEMTYP   --   DATAEASE USES REGULAR OR EXTENDED MEMORY 
C    DETYPE   --   DATAEASE VERSION
C
      DELOC    = '     '
      DO 40 L=1,40
         IF (RESULT(L:L) .NE. ' ') THEN
            DELOC = RESULT(10:40)
            GO TO 50
         ENDIF
   40 CONTINUE
C
C  CHECK FORMAT OF DATAEASE PROGRAM DIRECTORY SPECIFICATION
C
   50 CONTINUE
      CLIDRV = RESULT(1:2)
      DELOC  = RESULT(10:33)
      MEMTYP = RESULT(8:8)
      DEVERS = RESULT(4:6)
      DEDRIVE = DELOC(1:2)
      DEDIR   = DELOC(3:11)
C
C  DETERMINE THE VERSION OF DATAEASE CURRENTLY IN USE BASED ON THE COMMAND.
C  IF THE VERSION OF DATAEASE IS 2.5 OR 4.0 SET DETYPE=1.  OTHERWISE, SET 
C  DETYPE = 2.
C
      IF (DEVERS.EQ.'2.5'.OR.DEVERS.EQ.'4.0') THEN
         DETYPE = '1'
      ELSE
         DETYPE = '2'
      END IF        
C
C  UPDATING CLICOM BATCH FILES THAT USE DATAEASE
C
         CALL POSLIN(IR,IC)
         MSG = '      Updating CLICOM batch files that use DataEase'
         IR = IR + 1
         CALL SCRNMSGI(MSG,IR,14)
         MSG = ' '  
         IR = IR + 1
         CALL SCRNMSGI(MSG,IR,14)
C
C  UPDATE THE FRM-ARCH.BAT FILE
C
      FILEIN(1) = 'P:\BATCH\FRM-ARCH.BAT'
C
      OPEN (75,FILE=FILEIN(1),STATUS='OLD',FORM='FORMATTED',
     +      IOSTAT=IOCHK)
      IF (IOCHK.NE.0)THEN
         CLOSE(75)
         MISSFILE(1) = .TRUE.
         CALL DISPMSG(FILEIN(1),MISSFILE(1))
         GO TO 60
      END IF
      CALL MODBATCH(FILEIN(1),INBAT,DEDIR,OUTFRM,DETYPE,MEMTYP,RTNCODE)
      IF (RTNCODE.EQ.'1') THEN
         ERRFLAG(1) = .TRUE.
         CALL ERRFILE (FILEIN(1))
      END IF
  60  CONTINUE
C
C  UPDATE THE SORT2.BAT FILE
C
      FILEIN(2) = 'P:\BATCH\SORT2.BAT'
C
      OPEN (75,FILE=FILEIN(2),STATUS='OLD',FORM='FORMATTED',
     +      IOSTAT=IOCHK)
      IF (IOCHK.NE.0)THEN
         CLOSE(75)
         MISSFILE(2) = .TRUE.
         CALL DISPMSG(FILEIN(2),MISSFILE(2))
         GO TO 160
      END IF
      CALL MODBATCH(FILEIN(2),INBAT,DEDIR,OUTFRM,DETYPE,MEMTYP,RTNCODE)
      IF (RTNCODE.EQ.'1') THEN
         ERRFLAG(2) = .TRUE.
         CALL ERRFILE (FILEIN(2))
      END IF
C
C  UPDATE THE MRG-ARC2.BAT FILE
C
 160  CONTINUE
      FILEIN(3) = 'P:\BATCH\MRG-ARC2.BAT'
C
      OPEN (75,FILE=FILEIN(3),STATUS='OLD',FORM='FORMATTED',
     +      IOSTAT=IOCHK)
      IF (IOCHK.NE.0)THEN
         CLOSE(75)
         MISSFILE(3) = .TRUE.
         CALL DISPMSG(FILEIN(3),MISSFILE(3))
         GO TO 260
      END IF
C
      CALL MODBATCH(FILEIN(3),INBAT,DEDIR,OUTFRM,DETYPE,MEMTYP,RTNCODE)
      IF (RTNCODE.EQ.'1') THEN
         ERRFLAG(3) = .TRUE.
         CALL ERRFILE (FILEIN(3))
      END IF
C
C  UPDATE THE MERGE2.BAT FILE
C
 260  CONTINUE
      FILEIN(4) = 'P:\BATCH\MERGE2.BAT'
C
      OPEN (75,FILE=FILEIN(4),STATUS='OLD',FORM='FORMATTED',
     +      IOSTAT=IOCHK)
      IF (IOCHK.NE.0)THEN
         CLOSE(75)
         MISSFILE(4) = .TRUE.
         CALL DISPMSG(FILEIN(4),MISSFILE(4))
         GO TO 389
      END IF
C
      DO 300 I = 1,300
         READ(75,'(A79)',END=310)INBAT(I)
  300 CONTINUE
C
  310 INCNT = I - 1
      DO 311 I = 1,300
         IF (INBAT(I)(1:7).NE.':DEASE2')THEN
            FLAG = .TRUE. 
         ELSE
            GO TO 312
         END IF
  311    CONTINUE
         IF (FLAG) THEN
             GO TO 388
         END IF
  312    J = I + 2
         INBAT(J)(3:) = DEDIR
         IF (INBAT(J)(1:2).EQ.'CD'.OR.INBAT(J)(1:2).EQ.'cd')THEN
            GO TO 314
         END IF
         GO TO 388
  314    J = J + 1
         IF (INBAT(J)(7:8).EQ.'Q:'.OR.INBAT(J)(7:8).EQ.'q:')THEN
             GOTO 318
         END IF 
         GOTO 388
  318    CONTINUE
C
      DO 325 I = J,INCNT
         IF (INBAT(I)(1:7).NE.':DEASE4')THEN
            FLAG = .TRUE.
         ELSE
            GO TO 331
         END IF
  325    CONTINUE
         IF (FLAG) THEN 
            GO TO 388
         END IF
  331    K = I + 4
         INBAT(K)(3:11) = DEDIR
         IF (INBAT(K)(1:2).EQ.'CD'.OR.INBAT(K)(1:2).EQ.'cd')THEN
            GO TO 335
         END IF
         GO TO 388
  335    K = K + 1
         IF (MEMTYP.EQ.'X'.OR.MEMTYP.EQ.'x')THEN
            IF (DETYPE.EQ.'2')THEN
               INBAT(K)(1:5) = 'DE16M'
            END IF 
         ELSE
            INBAT(K)(1:5) = 'DEASE'
         END IF
         IF (INBAT(K)(7:8).EQ.'Q:'.OR.INBAT(K)(7:8).EQ.'q:')THEN
            GO TO 338
         END IF
         GO TO 388
  338    CONTINUE
C
      DO 340 I = K,INCNT
         IF (INBAT(I)(1:8).NE.':NEWEXTR')THEN
            FLAG = .TRUE.
         ELSE
            GO TO 341
         END IF
  340    CONTINUE
         IF (FLAG) THEN 
            GO TO 388
         END IF
  341    N = I + 5
         INBAT(N)(3:11) = DEDIR
         IF (INBAT(N)(1:2).EQ.'CD'.OR.INBAT(N)(1:2).EQ.'cd')THEN
            GO TO 345
         END IF
         GO TO 388
  345    N = N + 1
         IF (MEMTYP.EQ.'X'.OR.MEMTYP.EQ.'x')THEN
            IF (DETYPE.EQ.'2')THEN
               INBAT(N)(1:5) = 'DE16M'
            END IF 
         ELSE
            INBAT(N)(1:5) = 'DEASE'
         END IF
         IF (INBAT(N)(7:8).EQ.'Q:'.OR.INBAT(N)(7:8).EQ.'q:')THEN
            GO TO 348
         END IF
      GO TO 388
  348 CONTINUE
C
      REWIND 75
      DO 385 I = 1,INCNT
         CALL GETFRMT(INBAT(I),OUTFRM)
         WRITE(75,OUTFRM)INBAT(I)
 385  CONTINUE
C
      CLOSE(75)
      MISSFILE(4) = .FALSE.
      CALL DISPMSG(FILEIN(4),MISSFILE(4))
      GO TO 389
 388  CONTINUE
      CLOSE(75)
      ERRFLAG(4) = .TRUE.
      CALL ERRFILE(FILEIN(4))
C
C  UPDATE THE CLICOM2.BAT AND CLICOM2.MON FILES
C 
  389 CONTINUE
      IPASS = 0
  390 CONTINUE
      IPASS = IPASS + 1
      IF (IPASS.EQ.1) THEN
         FILEIN(5) = 'P:\BATCH\CLICOM2.BAT'
         OPEN (75,FILE=FILEIN(5),STATUS='OLD',FORM='FORMATTED',
     +         IOSTAT=IOCHK)
         IF (IOCHK.NE.0)THEN
            CLOSE(75)
            MISSFILE(5) = .TRUE.
            CALL DISPMSG(FILEIN(5),MISSFILE(5))
            GO TO 390
         END IF
      ELSE   
         FILEIN(6) = 'P:\BATCH\CLICOM2.MON'
         OPEN (75,FILE=FILEIN(6),STATUS='OLD',FORM='FORMATTED',
     +         IOSTAT=IOCHK)
         IF (IOCHK.NE.0)THEN
            CLOSE(75)
            MISSFILE(6) = .TRUE.
            CALL DISPMSG(FILEIN(6),MISSFILE(6))
            GO TO 460
         END IF
      ENDIF   
C
      DO 400 I = 1,300
         READ(75,'(A79)',END=410)INBAT(I)
  400 CONTINUE
C
  410 INCNT = I - 1
      DO 411 I = 1,300
         IF (INBAT(I)(1:6).NE.':DEASE')THEN
            FLAG = .TRUE.
         ELSE
            GO TO 421
         END IF
  411    CONTINUE
         IF (FLAG) THEN
            GO TO 430
         END IF
  421    J = I + 2
         INBAT(J)(3:11) = DEDIR
         IF (INBAT(J)(1:2).EQ.'CD'.OR.INBAT(J)(1:2).EQ.'cd')THEN
            GO TO 415
         END IF
         GO TO 430
  415    J = J + 1
         IF (MEMTYP.EQ.'X'.OR.MEMTYP.EQ.'x')THEN
            IF (DETYPE.EQ.'2')THEN
               INBAT(J)(1:5) = 'DE16M'
            END IF
         ELSE
            INBAT(J)(1:5) = 'DEASE'
         END IF
         IF (INBAT(J)(7:8).EQ.'Q:'.OR.INBAT(J)(7:8).EQ.'q:')THEN
             GOTO 440
         END IF 
C
  430 CONTINUE
      IF (IPASS.EQ.1)THEN
         CLOSE(75)
         ERRFLAG(5) = .TRUE.
         CALL ERRFILE (FILEIN(5))
         GO TO 390
      ELSE
         CLOSE(75)
         ERRFLAG(6) = .TRUE.
         CALL ERRFILE (FILEIN(6))
         GO TO 460
      END IF
  440 REWIND 75 
      DO 450 I = 1,INCNT
         CALL GETFRMT(INBAT(I),OUTFRM)
         WRITE(75,OUTFRM)INBAT(I)
  450 CONTINUE
C
      IF (IPASS.EQ.1) THEN
         CLOSE(75)
         MISSFILE(5) = .FALSE.
         CALL DISPMSG(FILEIN(5),MISSFILE(5))
         GO TO 390
      ELSE
         CLOSE(75)
         MISSFILE(6) = .FALSE.
         CALL DISPMSG(FILEIN(6),MISSFILE(6))
      END IF
C
C  UPDATE THE DATAPROD.BAT FILE
C
 460  CONTINUE
      FILEIN(7) = 'P:\BATCH\DATAPROD.BAT'
C
      OPEN (75,FILE=FILEIN(7),STATUS='OLD',FORM='FORMATTED',
     +      IOSTAT=IOCHK)
      IF (IOCHK.NE.0)THEN
         CLOSE(75)
         MISSFILE(7) = .TRUE.
         CALL DISPMSG(FILEIN(7),MISSFILE(7))
         GO TO 560
      END IF
C
      DO 500 I = 1,300
         READ(75,'(A79)',END=510)INBAT(I)
  500 CONTINUE
C
  510 INCNT = I - 1
      DO 511 I = 1,300
         IF (INBAT(I)(1:7).NE.':DEASE2')THEN
            FLAG = .TRUE. 
         ELSE
            GO TO 512
         END IF
  511    CONTINUE
         IF (FLAG) THEN
             GO TO 555
         END IF
  512    J = I + 2
         INBAT(J)(3:) = DEDIR
         IF (INBAT(J)(1:2).EQ.'CD'.OR.INBAT(J)(1:2).EQ.'cd')THEN
            GO TO 514
         END IF
         GO TO 555
  514    J = J + 1
         IF (INBAT(J)(7:8).EQ.'Q:'.OR.INBAT(J)(7:8).EQ.'q:')THEN
             GOTO 518
         END IF 
         GOTO 555
  518    CONTINUE
C
      DO 525 I = J,INCNT
         IF (INBAT(I)(1:7).NE.':DEASE4')THEN
            FLAG = .TRUE.
         ELSE
            GO TO 531
         END IF
  525    CONTINUE
         IF (FLAG) THEN 
            GO TO 555
         END IF
  531    K = I + 2
         INBAT(K)(3:11) = DEDIR
         IF (INBAT(K)(1:2).EQ.'CD'.OR.INBAT(K)(1:2).EQ.'cd')THEN
            GO TO 532
         END IF
         GO TO 555
  532    K = K + 1
         IF (MEMTYP.EQ.'X'.OR.MEMTYP.EQ.'x')THEN
            IF (DETYPE.EQ.'2')THEN
               INBAT(K)(1:5) = 'DE16M'
            END IF 
         ELSE
            INBAT(K)(1:5) = 'DEASE'
         END IF
         IF (INBAT(K)(7:8).EQ.'Q:'.OR.INBAT(K)(7:8).EQ.'q:')THEN
            GO TO 540
         END IF
         GO TO 555
C
  540 REWIND 75
      DO 550 I = 1,INCNT
         CALL GETFRMT(INBAT(I),OUTFRM)
         WRITE(75,OUTFRM)INBAT(I)
  550 CONTINUE
C
      CLOSE(75)
      MISSFILE(7) = .FALSE.
      CALL DISPMSG(FILEIN(7),MISSFILE(7))
      GO TO 560
  555 CONTINUE
      CLOSE(75)
      ERRFLAG(7) = .TRUE.
      CALL ERRFILE(FILEIN(7))
C
C  UPDATE THE DATAPRD2.BAT FILE
C
 560  CONTINUE 
      FILEIN(8) = 'P:\BATCH\DATAPRD2.BAT'
C
      OPEN (75,FILE=FILEIN(8),STATUS='OLD',FORM='FORMATTED',
     +      IOSTAT=IOCHK)
      IF (IOCHK.NE.0)THEN
         CLOSE(75)
         MISSFILE(8) = .TRUE.
         CALL DISPMSG(FILEIN(8),MISSFILE(8))
         GO TO 900
      END IF
      CALL MODBATCH(FILEIN(8),INBAT,DEDIR,OUTFRM,DETYPE,MEMTYP,RTNCODE)
      IF (RTNCODE.EQ.'1') THEN
         ERRFLAG(8) = .TRUE.
         CALL ERRFILE (FILEIN(8))
      END IF
C
C  ERROR ROUTINES
C
 900  CONTINUE
      DO 910 K = 1, 8
      IF (MISSFILE(K).OR.ERRFLAG(K)) THEN
         CLOSE(75)
         CALL ERRMSG
      END IF
 910  CONTINUE
      CALL POSLIN(IR,IC)
      MSG = '  '
      IR = IR + 1
      CALL SCRNMSGI(MSG,IR,12)
      MSG = '      Finished updating CLICOM batch files.'
      IR = IR + 1
      CALL SCRNMSGI(MSG,IR,14)
      STOP ' '
      END

*******************************************************************************

        SUBROUTINE ERRMSG
C
C       THIS ROUTINE DISPLAY AN ERROR MESSAGE IF THE BATCH FILES WHICH ARE
C       NOT FINd IN THE EXPECTED PLACE.  
C
        CHARACTER*80 MSG     
C
        CALL POSLIN(IR,IC)
        MSG = '  '
        IR = IR + 1
        CALL SCRNMSGI(MSG,IR,14)
        MSG=' Error(s) were found in the batch files listed above.'
        IR = IR + 1
        CALL SCRNMSGI(MSG,IR,14)
        MSG=' 1. Use the DOS copy command to load missing files from'
     +    //' CLICOM Installation'
        IR = IR + 1
        CALL SCRNMSGI(MSG,IR,14)
        MSG='    diskette number 1.'
        IR = IR + 1
        CALL SCRNMSGI(MSG,IR,14)
        MSG=' 2. Use your text editor to correct the errors found in'
     +      //' the batch files.'
        IR = IR + 1
        CALL SCRNMSGI(MSG,IR,14)
        MSG=' 3. Refer to the trouble shooting section in the'
     +      //' CLICOM Installation Guide for'
        IR = IR + 1
        CALL SCRNMSGI(MSG,IR,14)
        MSG='    more information.'
        IR = IR + 1
        CALL SCRNMSGI(MSG,IR,14)
        MSG=' '
        IR = IR + 1
        CALL SCRNMSGI(MSG,IR,14)
        STOP 3
        RETURN
        END

*******************************************************************************

	SUBROUTINE MODBATCH(FILEIN,INBAT,DEDIR,OUTFRM,DETYPE,MEMTYP,
     +                      RTNCODE)

C
C       THIS ROUTINE WILL MODIFY BATCH FILES AS FOLLOW :
C      	    1. SORT2.BAT
C           2. MRG-ARC2.BAT
C           3. DATAPRD2.BAT
C           4. FRM-ARCH.BAT
C       
      CHARACTER*30 FILEIN
      CHARACTER*79 INBAT(300)
      CHARACTER*9  DEDIR
      CHARACTER*6  OUTFRM
      CHARACTER*1  MEMTYP,RTNCODE,DETYPE
      LOGICAL      MISSFILE,FLAG
      DATA         FLAG/.FALSE./
C
      RTNCODE = '0'
      DO 100 I = 1,300
         READ(75,'(A79)',END=110)INBAT(I)
  100 CONTINUE
C
  110 INCNT = I - 1
      DO 115 I = 1,300
         IF (INBAT(I)(1:7).NE.':DEASE2')THEN
            FLAG = .TRUE.
         ELSE
            GO TO 121
         END IF
  115    CONTINUE
         IF (FLAG) THEN
            RTNCODE = '1'
            GO TO 200
         END IF
  121    J = I + 2
         INBAT(J)(3:11) = DEDIR
         IF (INBAT(J)(1:2).EQ.'CD'.OR.INBAT(J)(1:2).EQ.'cd')THEN
            GO TO 124
         END IF
         RTNCODE = '1'
         GO TO 200
  124    J = J + 1
         IF (INBAT(J)(7:8).EQ.'Q:'.OR.INBAT(J)(7:8).EQ.'q:')THEN
             GOTO 125
         END IF 
         RTNCODE = '1'
         GO TO 200
  125    CONTINUE
C
      DO 128 I = J,INCNT
         IF (INBAT(I)(1:7).NE.':DEASE4')THEN
            FLAG = .TRUE.
         ELSE
            GO TO 131  
         END IF
  128    CONTINUE
         IF (FLAG) THEN
            RTNCODE = '1'
            GO TO 200
         END IF
  131    K = I + 4
         INBAT(K)(3:11) = DEDIR
         IF (INBAT(K)(1:2).EQ.'CD'.OR.INBAT(K)(1:2).EQ.'cd')THEN
            GO TO 135
         END IF
         RTNCODE = '1'
         GO TO 200
  135    K = K + 1
         IF (MEMTYP.EQ.'X'.OR.MEMTYP.EQ.'x')THEN
            IF (DETYPE.EQ.'2') THEN
               INBAT(K)(1:5) = 'DE16M'
            END IF
         ELSE
            INBAT(K)(1:5) = 'DEASE'
         END IF
         IF (INBAT(K)(7:8).EQ.'Q:'.OR.INBAT(K)(7:8).EQ.'q:')THEN
            GO TO 140
         END IF
         RTNCODE = '1'
         GO TO 200
C
  140 REWIND 75
      DO 150 I = 1,INCNT
         CALL GETFRMT(INBAT(I),OUTFRM)
         WRITE(75,OUTFRM)INBAT(I)
  150 CONTINUE
         MISSFILE = .FALSE.
         CALL DISPMSG(FILEIN,MISSFILE)
  200 CONTINUE
      CLOSE(75)
      RETURN
      END

*******************************************************************************

      SUBROUTINE DISPMSG (FILEIN,MISSFILE)
C
C     THE ROUTINE DISPLAYS A MESSAGE IF THE BATCH FILE HAS BEEN MODIFIED OR 
C     IT IS NOT FOUND 
C
      CHARACTER*30 FILEIN
      CHARACTER*80 MSG
      LOGICAL      MISSFILE 
C
      NLNG = LNG(FILEIN)
      IF ( MISSFILE) THEN
         CALL POSLIN(IR,IC)
         MSG = ' The required batch file, '//FILEIN(1:NLNG)//
     +         ', was not found'
         IR = IR + 1
         CALL SCRNMSGI(MSG,IR,12)
         RETURN
      ELSE
         CALL POSLIN(IR,IC)
         MSG = ' Batch file modified = '//FILEIN(1:NLNG)
         IR = IR + 1
         CALL SCRNMSGI(MSG,IR,14)
         RETURN
      END IF
      END

*******************************************************************************

      SUBROUTINE ERRFILE (FILEIN)
C
C     THE ROUTINE DISPLAYS A MESSAGE IF THE BATCH FILE FOUND A FORMAT ERROR
C
      CHARACTER*30 FILEIN
      CHARACTER*80 MSG
C
      NLNG = LNG(FILEIN)
      CALL POSLIN(IR,IC)
      MSG = ' A format error has been found in batch file, '
     +    //FILEIN(1:NLNG)
      IR = IR + 1
      CALL SCRNMSGI(MSG,IR,12)
      RETURN
      END

