$STORAGE:2
      SUBROUTINE OUTMAN(PLTONSCR)
C------------------------------------------------------------------------------
C     UPPER LEVEL PROGRAM THAT MANAGES THE OUTPUT DISPOSITION (PRINTER, 
C     PLOTTER, OR DISK) OF THE GRAPH ON THE MONITOR'S SCREEN.
C
C     INPUT ARGUMENTS:
C
C     PLTONSCR      LOG        FLAG TO INDICATE IF THE PLOT IS CURRENTLY
C                              DISPLAYED ON THE SCREEN.  ARGUMENT TO PRNTMAN.
C
C     OUTPUT ARGUMENTS:  NONE
C------------------------------------------------------------------------------
      LOGICAL PLTONSCR  
$INCLUDE: 'GRFPARM.INC'
$INCLUDE:'GRAFVAR.INC'
      INTEGER*2   HELPLVL
      CHARACTER*2 INCHAR
      CHARACTER *28 GRAFNAME
C
C   1.Save graph definition  2.Save screen  3.Printer  4.Plotter
C
      XL = 0.1
      YL = 0.2
      HELPLVL = 5
  100 CALL GRAFMNU(1,5,XL,YL,HELPLVL,INCHAR)

      IF (INCHAR.EQ.'1 ') THEN
         GRAFNAME = GDFNAME     
         ITYPE = IOBSTYP
         ITEMP = 0 
         CALL WRTGRAF(GRAFNAME,ITYPE,ITEMP,INCHAR)
      ELSE IF (INCHAR.EQ.'2 ') THEN
         CALL WRTGSCRN(PALETTE,PALDEF)
      ELSE IF (INCHAR.EQ.'3 ') THEN
         CALL PRNTMAN(PALETTE,PALDEF,PLTONSCR)
      ELSE IF (INCHAR.EQ.'4 ') THEN
C ** DEBUG       
         MSGN1=549
         MSGN2=202
         XWIN=.1
         YWIN=.95
         CALL GRAFNOTE(XWIN,YWIN,MSGN1,MSGN2,' ',0,INCHAR)
C         CALL PLOTMAN(...) 
      ELSE IF (INCHAR .EQ. '4F' .OR. INCHAR .EQ. 'ES') THEN
         RETURN
      END IF
      GO TO 100
      END
