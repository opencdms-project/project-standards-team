$STORAGE:2
      SUBROUTINE GETGSCRN (GRAFSCRN,COLORS,RTNCODE)
C------------------------------------------------------------------------------
C   ROUTINE TO DISPLAY A LIST OF EXISTING GRAPHICS SCREENS TO THE USER SO 
C   THEY CAN SELECT ONE. ---> THIS ROUTINE IS ASSOCIATED WITH GRAFINIT <---
C
C     INPUT ARGUMENTS: NONE
C
C     OUTPUT ARGUMENTS:
C
C     GRAFSCRN   CHAR   NAME OF THE GRAPHICS SCREEN SELECTED (CHAR*8)
C     COLORS     CHAR   NUMBERS FOR COLORS OF SAVED PALETTE
C     RTNCODE    CHAR   RETURN STATUS:  0 = SELECTION MADE, 1 = NO SELECTION, 
C                       2 = FILE ERROR WITH SELECTION
C
C------------------------------------------------------------------------------
      CHARACTER*73 DESCREC
      CHARACTER*32 COLORS
      CHARACTER*20 SCRNFILE
      CHARACTER*8 GRAFSCRN
      CHARACTER*1 RTNCODE
      INTEGER*2   KROW,KCOL,KER
C
C   ALLOW USER TO SELECT GRAPH FROM SCREEN DEFINITION INDEX FILE
C
   10 OPEN (51,FILE='O:\DATA\GRAFSCRN.IDX',STATUS='OLD',FORM='BINARY'
     +     ,ACCESS='DIRECT',RECL=75)
      COLORS  = ' '
      RTNCODE = '0'
      DESCREC = ' '
      CALL POSLIN(KROW,KCOL)
      CALL VALWIN(51,DESCREC,73,7,ICNTRL)
      CALL LOCATE(KROW,KCOL,KER)
      CLOSE(51)
      GRAFSCRN = DESCREC(1:8)
      IF (GRAFSCRN .EQ. '  ') THEN
         RTNCODE = '1'
      ELSE 
         IF (ICNTRL .EQ. 1) THEN
            CALL DELSCRN(GRAFSCRN)
            GO TO 10
         ENDIF
      ENDIF
C      
C  BUILD SCR FILE NAME AND OPEN IT TO MAKE SURE IT EXISTS
C
      IF (RTNCODE .EQ. '0') THEN
         SCRNFILE = 'O:\DATA\'//GRAFSCRN
         DO 50 I1 = 20,1,-1
            IF (SCRNFILE(I1:I1) .NE. ' ') THEN
               GO TO 60
            ENDIF
   50    CONTINUE
   60    CONTINUE
         I1 = I1 + 1
         SCRNFILE(I1:I1+3) = '.SCR'         
         OPEN (51,FILE=SCRNFILE,STATUS='OLD',FORM='FORMATTED',
     +        MODE='READ',IOSTAT=ICHK)
         IF (ICHK.NE.0) THEN
            RTNCODE = '2'
            RETURN
         ENDIF
         CLOSE(51)
         COLORS = DESCREC(42:73)
      ENDIF
      RETURN
      END
      