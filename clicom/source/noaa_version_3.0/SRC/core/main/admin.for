$STORAGE:2
C
C   PROGRAM ADMIN
C
C   THIS ROUTINE PROVIDES ACCESS TO THE SYSTEM ADMINISTRATION ROUTINES
C   WHICH ALLOW THE USER TO MODIFY THE CONFIGURATION OF THE CLICOM 
C   FORTRAN ROUTINES AND FILES
C
C   DEFINE THE INTERFACE TO THE C ROUTINE "SYSTEM"
C
      INTERFACE TO INTEGER*2 FUNCTION SYSTEM [C]
     +        (STRING[REFERENCE])
      CHARACTER*1 STRING
      END
C
      PROGRAM ADMIN
C
      INTEGER*2 CHOICE,SYSTEM
      CHARACTER*4 INLVL
      CHARACTER*64 FILNAME
C
      FILNAME = 'P:\HELP\ADMIN.HLP'
      CALL SETMOD(3,IERR)
      CALL GETAUT(INLVL)
      IF (INLVL.NE.'HI  ') THEN
         CALL WRTMSG(12,145,12,1,1,' ',0)
         CALL LOCATE(24,0,IERR)
         STOP 8
      END IF   
C
   20 CONTINUE
      CALL CLS
      CALL LOCATE(2,10,IERR)
      CALL GETMNU('SYS-ADMIN   ',FILNAME,CHOICE)
C
      IF (CHOICE.EQ.1) THEN
30       CONTINUE
         CALL CLS
         CALL LOCATE(2,10,IERR)
         CALL GETMNU('ADM-KEYENTRY','  ',CHOICE)
         IF (CHOICE.EQ.0) THEN
            GO TO 20
         ELSE IF (CHOICE.EQ.1) THEN
            CALL SETUP
         ELSE IF (CHOICE.EQ.2) THEN
            I = SYSTEM('PACKTWF'C)
         ELSE IF (CHOICE.EQ.3) THEN
            I = SYSTEM('ED P:\\DATA\\ELEMCHKS.MLY'C)
         ELSE IF (CHOICE.EQ.4) THEN
            I = SYSTEM('ED P:\\DATA\\ELEMCHKS.10D'C)
         ELSE IF (CHOICE.EQ.5) THEN
            I = SYSTEM('ED P:\\DATA\\ELEMCHKS.DLY'C)
         ELSE IF (CHOICE.EQ.6) THEN
            I = SYSTEM('ED P:\\DATA\\ELEMCHKS.HLY'C)
         ELSE IF (CHOICE.EQ.7) THEN
            I = SYSTEM('ED P:\\DATA\\ELEMCHKS.15M'C)
         ELSE IF (CHOICE.EQ.8) THEN
            I = SYSTEM('ED P:\\DATA\\ELEMCHKS.U-A'C)
         END IF
         GO TO 30
      ELSE IF (CHOICE.EQ.2) THEN
40       CONTINUE
         CALL CLS
         CALL LOCATE(2,10,IERR)
         CALL GETMNU('ADM-LIM-FILE','  ',CHOICE)
         IF (CHOICE.EQ.0) THEN
            GO TO 20
         ELSE IF (CHOICE.EQ.1) THEN
            I = SYSTEM('ED P:\\DATA\\MLYPUB.LIM'C)
         ELSE IF (CHOICE.EQ.2) THEN
            I = SYSTEM('ED P:\\DATA\\DLY2MLY.LIM'C)
         ELSE IF (CHOICE.EQ.3) THEN
            I = SYSTEM('ED P:\\DATA\\DLY210D.LIM'C)
         END IF
         GO TO 40
      ELSE IF (CHOICE.EQ.3) THEN
C          .. CONFIGURATION OF DISPLAY DEVICES      
50       CONTINUE
         CALL CLS
         CALL LOCATE(2,10,IERR)
         CALL GETMNU('SELDEVCONFIG','  ',CHOICE)
         IF (CHOICE.EQ.0) THEN
            GO TO 20
         ELSE IF (CHOICE.EQ.1) THEN
            CALL CFGPRNT
         ELSE IF (CHOICE.EQ.2) THEN
            CALL LOCATE(23,0,IERR)
            STOP 7
C           HALOENV.EXE -- CHANGE RETURN CODE TO 7 FOR CLICOM2.BAT
         END IF
         GO TO 50
      ELSE IF (CHOICE.EQ.4) THEN
          CALL LOCATE(23,0,IERR)
          STOP 3
C         BUILDMAP.EXE
      ELSE IF (CHOICE.EQ.5) THEN
         CALL SETUSERS
      ELSE IF (CHOICE.EQ.6) THEN
         CALL LOCATE(23,0,IERR)
         STOP 4
C        DEFFORM.EXE
      ELSE IF (CHOICE.EQ.7) THEN
         CALL LOCATE(23,0,IERR)
         STOP 5 
C        DEFMENU.EXE
      ELSE IF (CHOICE.EQ.8) THEN
         CALL LOCATE(23,0,IERR)
         STOP 6
C        DEFSCRN.EXE
      ELSE IF (CHOICE.EQ.9) THEN
         CALL LOCATE(23,0,IERR)
         CALL LISTFORM
      ELSE IF (CHOICE.EQ.10) THEN
         CALL LISTMENU
      ELSE IF (CHOICE.EQ.11) THEN
         CALL MSGS2TXT
      ELSE IF (CHOICE.EQ.12) THEN
         CALL TXT2MSGS
      ELSE IF (CHOICE.EQ.0) THEN
         CALL LOCATE(23,0,IERR)
         STOP 8
      END IF
C
      GO TO 20
      END
*************************************************************************
The GETELEM subroutine is included in this module (as well as being in the
UTILITY library because of an apparent compiler error.  When this routine is
removed the program fails to stop properly whenever this routine is used
during execution (whenever a user requests a selection of element codes
when defining a key-entry form)
************************************************************************
      SUBROUTINE GETELEM(ELEMCODE,ITYPE)
C
C   ROUTINE TO DISPLAY THE ELEM.DEF FILE AND ALLOW THE USER TO 
C      SELECT THE ELEMENT-CODE WANTED
C
C      ELEMENT-CODE = SPACES ON RETURN IF NONE SELECTED
C
      CHARACTER*3 ELEMCODE
      CHARACTER*80 ELEMREC
C
C   OPEN THE ELEM.DEF FORTRAN FILE
C
20    CONTINUE
      OPEN(30,FILE='P:\DATA\ELEM.DEF',STATUS='OLD',ACCESS='DIRECT'
     +      ,RECL=110,SHARE='DENYWR',MODE='READ',IOSTAT=IOCHK)
      IF (IOCHK.NE.0) THEN
         CALL OPENMSG('P:\DATA\ELEM.DEF      ','GETELEM     ',IOCHK)
         GO TO 20
      END IF
C
C   CALL THE VALUE-WINDOW ROUTINE TO ALLOW USER TO SELECT ELEMENT-CODE
C      
      CALL VALWIN(30,ELEMREC,80,2,ITYPE)
      ELEMCODE = ELEMREC(1:3)
C
      CLOSE(30)
      RETURN
      END
