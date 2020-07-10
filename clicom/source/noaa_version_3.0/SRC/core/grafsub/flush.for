$STORAGE:2
      SUBROUTINE FLUSH(STRING,X1,Y1,H1,A1,F1,PATH,LIM)
C------------------------------------------------------------------------------
C     ADJUST THE TEXT LOCATION FOR THAT TEXT WHICH IS FLUSH WITH AN EDGE OF
C     THE PLOT AFTER THE USER MODIFIES THE SIZE, FONT, OR LOCATION.
C
C     INPUT ARGUMENTS:
C
C     STRING        CHAR      TEXT STRING THAT WAS CHANGED
C     X1,Y1         REAL      CURRENT POSITION OF TEXT STRING (BOTTOM-CENTER
C                             IN NORMALIZED WORLD COORDINATES)
C     H1            REAL      CURRENT TEXT HEIGHT (NWC)
C     A1            REAL      CURRENT TEXT ASPECT RATIO
C     F1            INT2      CURRENT TEXT FONT
C     PATH          INT2      PATH OF TEXT - NOT CHANGABLE
C     LIM           INT2      ALIGNMENT TO BE CHECKED: 
C                             0 = NO CHECKS
C                             1 = LEFT AND RIGHT SIDE EDGE CHECK
C                             2 = TOP AND BOTTOM EDGE CHECK
C                             3 = BOTH
C     OUTPUT ARGUMENTS:
C
C     X1,Y1         REAL      REVISED POSITION OF TEXT STRING 
C------------------------------------------------------------------------------
$INCLUDE: 'GRFPARM.INC'
$INCLUDE: 'GRAFVAR.INC'
      CHARACTER *(*)  STRING
      CHARACTER*2     ANSWER, YESUP, YESLO
      LOGICAL         FIRSTCALL
      INTEGER         F1, PATH, LIM
      REAL            HG,WD,OFS,WH1,XP1,YP1,XTP1,YTP1,XB1,YB1,XB2,
     +                YB2,XLF,YBT,XRT,YTP
      COMMON /FLSHSV/ HG,WD,OFS,WH1,XP1,YP1,XTP1,YTP1,XB1,YB1,XB2,
     +                YB2,XLF,YBT,XRT,YTP,ANSWER,YESUP,YESLO
      DATA            FIRSTCALL /.TRUE./
      IF (FIRSTCALL) THEN
         CALL GETYN(1,2,YESUP,YESLO)
         FIRSTCALL = .FALSE.
      ENDIF
C
C   CONVERT FROM NORMALIZED TO WORLD COORDINATES
C
      CALL NW2W(X1,Y1,XP1,YP1)
      CALL YSZNW2W(H1,DX,WH1)
C
C  SET STROKE TEXT FONT AND ATTRIBUTES
C
      CALL SETCFON(F1)
      CALL SETSTE(WH1,A1,PATH)
      CALL INQSTS(STRING,HG,WD,OFS)
C
C  DETERMINE THE BOX WHICH SURROUNDS THE TEXT AT THE CURRENT POSITION
C
      CALL COMTPOS(XP1,YP1,WD,HG,PATH,XTP1,YTP1,XB1,YB1,XB2,YB2,LIM)
      CALL NW2W(GANWLF,GANWBT,XLF,YBT)
      CALL NW2W(GANWRT,GANWTP,XRT,YTP)
C
C  DETERMINE WHETHER CURRENT TEXT POSITION IS FLUSH WITH AN EDGE. IF FLUSH,
C  ASK USER TO SELECT POSITION AS ALWAYS FLUSH OR EXACT.
C
      IF (LIM .EQ. 1 .OR. LIM .EQ. 3) THEN
         IF (XB1 .EQ. XLF .OR. XB2 .EQ. XRT) THEN
C------------  ASK FLUSH QUESTION.  LEFT/RIGHT SIDES
            ANSWER=' '
            CALL GRAFNOTE(.1,.95,544,545,' ',0,ANSWER)
            IF (ANSWER .EQ. YESUP .OR. ANSWER .EQ. YESLO) THEN
               IF (XB1 .EQ. XLF) THEN
                  X1 = 0.0
               ELSE
                  X1 = 1.0
               ENDIF
            ENDIF
         ENDIF
      ENDIF

      IF (LIM .EQ. 2 .OR. LIM .EQ. 3) THEN
         IF (YB1 .EQ. YBT .OR. YB2 .EQ. YTP) THEN
C------------  ASK FLUSH QUESTION.  TOP/BOTTOM SIDES
            ANSWER=' '
            CALL GRAFNOTE(.1,.95,544,545,' ',0,ANSWER)
            IF (ANSWER .EQ. YESUP .OR. ANSWER .EQ. YESLO) THEN
               IF (YB1 .EQ. YBT) THEN
                  Y1 = 0.0
               ELSE
                  Y1 = 1.0
               ENDIF
            ENDIF
         ENDIF
      ENDIF

      RETURN
      END
