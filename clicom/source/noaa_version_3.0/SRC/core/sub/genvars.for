$STORAGE:2
      SUBROUTINE GENVARS(ICOL,ILINE,FLAGHOLD)
C
C  THIS SUBROUTINE CALLS A SET OF SUBROUTINES TO GENERATE THE 
C  MOISTURE PARAMETERS IN A DATA SET.  MISSING ELEMENTS ARE GENERATED
C  WHILE VALUES ENTERED ARE CHECKED AGAINST THE GENERATED VALUES
C
C  A DATA FLAG OF 'G' IS PLACED IN THE 6TH POSITION OF EACH SUCCESSFULLY
C  GENERATED ELEMENT.
C
C  QUALITY FLAGS:
C       FLAGHOLD(ICOL,1,1):
C            A = GENERATED SUCCESSFULLY
C            B = NOT GENERATED
C            C = ERROR
C       FLAGHOLD(ICOL,1,2):
C            A = NO ERROR
C            a = INPUT DATA TOO EXTREME FOR GENERATION
C            b = GENERATION SUPPRESSED BY VALIDATOR
C            c = INSUFFICIENT INPUT DATA FOR GENERATION
C            W = COMPUTED VALUE DOES NOT AGREE WITH ENTERED VALUE
C
$INCLUDE: 'VAL1.INC'
$INCLUDE: 'ELEMCHKS.INC'
$INCLUDE: 'GENMOIST.INC'
C
      CHARACTER*1 FLAGHOLD(MAXELEM,MAXCHK,2), FLGHLD(NUMGEN)
      CHARACTER*6 HLDVAL
      INTEGER*2 ILINE,ASPFLG
      REAL*4 ARRAY(NUMGEN), VAL,SP, DB, DP, WB, RH, VP, MR
      REAL*4 SPHOLD,HGTPRM, HGTWGT,MISSING
C
      DATA HGTPRM/.0185/
      DATA HGTWGT/.016/
      DATA MISSING /-9999.0/
C
C  IF A VALIDATOR HAS ALREADY SUPPRESSED THIS FIELD DO NOT GENERATE
C  IT NOW.
C
      IF (FLAGARRAY(ICOL,ILINE,2).EQ.'b')THEN
          FLAGHOLD(ICOL,ILINE,1) = FLAGARRAY(ICOL,ILINE,1)
          FLAGHOLD(ICOL,ILINE,2) = FLAGARRAY(ICOL,ILINE,2)
          RETURN
      ELSE
         FLAGHOLD(ICOL,1,1) = 'A'
         FLAGHOLD(ICOL,1,2) = 'A'
      END IF
C
C  READ THE EXISTING MOISTURE VARIABLES INTO THE WORK ARRAY.  MOVE
C  -9999.0 INTO THE ARRAY FOR ANY MISSING DATA OR ELEMENTS NOT IN
C  THIS DATA SET.
C
      DO 20 I = 1,NUMGEN
         J = TBLGEN(I)
         IF (J.LT.1)THEN
            ARRAY(I) = MISSING
            GO TO 20
         END IF
         IF (VALARRAY(J,ILINE).EQ.'      '.OR.
     +            VALARRAY(J,ILINE)(6:6).EQ.'G') THEN
            ARRAY(I) = MISSING
         ELSE
            READ(VALARRAY(J,ILINE),'(F5.0,A1)')ARRAY(I),
     +            FLGHLD(I)
         END IF
   20 CONTINUE
C
C  CHECK EACH OF THE SEVEN ELEMENTS AND SCALE THEM.  ALSO CONVERT THEM
C  TO METRIC UNITS IF THEY ARE CURRENTLY IN ENGLISH UNITS
C
C  STATION PRESSURE 
      IF (ARRAY(1).NE.MISSING)THEN
         SP = ARRAY(1) * TBLCONV(TBLGEN(1))
      ELSE
         SP = MISSING
      END IF
C  DRY BULB TEMP
      IF (ARRAY(2).NE.MISSING)THEN
         DB = ARRAY(2) * TBLCONV(TBLGEN(2))
         IF (DATFRM(2).EQ.'F  ')THEN
            CALL FTOC(DB)
         END IF
      ELSE
         DB = MISSING
      END IF
C  DEW POINT TEMP
      IF (ARRAY(3).NE.MISSING)THEN
         DP = ARRAY(3) * TBLCONV(TBLGEN(3))
         IF (DATFRM(3).EQ.'F  ')THEN
            CALL FTOC(DP)
         END IF
      ELSE
         DP = MISSING
      END IF
C  WET BULB TEMP
      IF (ARRAY(4).NE.MISSING)THEN
         WB = ARRAY(4) * TBLCONV(TBLGEN(4))
         IF (DATFRM(4).EQ.'F  ')THEN
            CALL FTOC(WB)
         END IF
      ELSE
         WB = MISSING
      END IF
C  RELATIVE HUMIDITY
      IF (ARRAY(5).NE.MISSING)THEN
         RH = ARRAY(5) * TBLCONV(TBLGEN(5))
      ELSE
         RH = MISSING
      ENDIF
C  VAPOR PRESSURE
      IF (ARRAY(6).NE.MISSING)THEN
         VP = ARRAY(6) * TBLCONV(TBLGEN(6))
      ELSE
         VP = MISSING
      ENDIF
C  MIXING RATIO
      IF (ARRAY(7).NE.MISSING)THEN
         MR = ARRAY(7) * TBLCONV(TBLGEN(7))
      ELSE
         MR = MISSING
      END IF
C
C  CHECK SP (STATION PRESSURE).  IF IT IS MISSING TRY TO USE ONE FROM 
C  THE PREVIOUS LINE. OTHERWISE, GENERATE ONE FROM THE U.S. STANDARD 
C  ATMOSPHERE. 
C
      IF (SP.EQ.MISSING)THEN
         IF (TBLGEN(1).EQ.0.OR.ILINE.LE.1) THEN
            HLDVAL = '      '
         ELSE 
            HLDVAL = VALARRAY(TBLGEN(1),ILINE-1)(1:5)
         END IF 
         IF (ILINE.GT.1.AND.HLDVAL.NE.'      ') THEN
            READ(HLDVAL,'(F5.0,A1)')ARRAY(1),FLGHLD(1)
            SP = ARRAY(1) * TBLCONV(TBLGEN(1))
            IF (DATFRM(1).EQ.'INS') THEN
               SP = SP * 33.8639
            END IF
         ELSE
            FLGHLD(1) = 'G'
            IF (STNELEV.EQ.MISSING)THEN
               SP = MISSING
            ELSE IF (STNELEV.LT.500)THEN
               SP = 1013.2
            ELSE
               SP = 1013.2
               SPHOLD = 1013.2
               M1 = STNELEV / 500
               DO 100 I = 1,M1
                  HGTPRM = HGTPRM - HGTPRM * HGTWGT 
                  SP = SP - SPHOLD * HGTPRM
  100       CONTINUE
            END IF
         END IF
      ELSE
         IF (DATFRM(1).EQ.'INS') THEN
            SP = SP * 33.8639
         END IF
      END IF
C
C  CHECK THE REMAINING SIX ELEMENTS AND CALCULATE THEM IF THERE ARE
C  ENOUGH OTHER ELEMENTS FOR THE CALCULATION.
C
C  IF THE DRY BULB TEMPERATURE IS MISSING OR LESS THAN -40 DEGREES CELSIUS
C  OR THE RELATIVE HUMIDITY IS LESS THAN 5 PERCENT DO NOT GENERATE VALUES.
C
      IF (DB.EQ.MISSING.OR.(DP.EQ.MISSING.AND.WB.EQ.MISSING.AND.
     +       RH.EQ.MISSING)) THEN
         IF (ICOL.NE.TBLGEN(1).AND.ICOL.NE.TBLGEN(2)) THEN
            IF (VALARRAY(ICOL,ILINE).EQ.'      '.OR.
     +            VALARRAY(ICOL,ILINE)(6:6).EQ.'G') THEN
               VALARRAY(ICOL,ILINE) = '      '
               FLAGHOLD(ICOL,1,2) = 'c'
            END IF
         END IF
         RETURN
      ELSE IF (DB.LT.-40.0.OR.(RH.LT.5.0.AND.RH.NE.MISSING)) THEN
         IF (ICOL.NE.TBLGEN(2)) THEN
            IF (VALARRAY(ICOL,ILINE).EQ.'      '.OR.
     +            VALARRAY(ICOL,ILINE)(6:6).EQ.'G') THEN
               VALARRAY(ICOL,ILINE) = '      '
               FLAGHOLD(ICOL,1,2) = 'a'
            END IF
         END IF
         RETURN
      END IF
C
C  CALCULATE AND CHECK RH - USE WET BULB IF HAVE IT AND PRESSURE.
C  IF DON'T HAVE PRESSURE BUT HAVE DEW POINT, USE THE DEW POINT
C
      IF (WB.EQ.MISSING.OR.SP.EQ.MISSING) THEN
         GO TO 205
      ELSE IF (DP.NE.MISSING.AND.FLGHLD(1).EQ.'G') THEN
         GO TO 205
      END IF
      IF (FLGHLD(4).EQ.'G')THEN
         ASPFLG = 1
      ELSE
         ASPFLG = ASPTWB   
      ENDIF
      CALL GENRH2(ASPFLG,DB,WB,SP,VAL)
      IF (VAL.LT.5.0.OR.VAL.GT.100.0) THEN
         IF (RH.EQ.MISSING.OR.FLGHLD(5).EQ.'G')THEN
            VALARRAY(TBLGEN(5),ILINE) = '      '
            FLAGHOLD(TBLGEN(5),1,2) = 'a'
         END IF
         GO TO 210
      END IF
      IF (ICOL.EQ.TBLGEN(5)) THEN
         IVAL = VAL / TBLCONV(ICOL) + .5
         IF (RH.EQ.MISSING.OR.FLGHLD(5).EQ.'G')THEN
            WRITE (VALARRAY(ICOL,ILINE),50) IVAL, 'G'
 50         FORMAT (I5,A1)
         ELSE
            READ (VALARRAY(ICOL,ILINE),'(I5,1X)') JVAL
            IDIFF = NINT(1.0/TBLCONV(ICOL))
            IF (IVAL.LT.JVAL-IDIFF.OR.IVAL.GT.JVAL+IDIFF) THEN
               FLAGHOLD(ICOL,1,1) = 'C'
               FLAGHOLD(ICOL,1,2) = 'W'
            END IF
         END IF
         RETURN
      ELSE
         IF (RH.EQ.MISSING.OR.FLGHLD(5).EQ.'G')THEN
            RH = VAL
         END IF
      END IF
      GO TO 210
C
C  IF GET HERE AND DEW POINT IS MISSING - THERE'S NOT EHOUGH DATA TO
C  GENERATE RH.  OTHERWISE COMPUTE RH USING DP
C
  205 CONTINUE
      IF (DP.EQ.MISSING)THEN
         IF (RH.EQ.MISSING) THEN
            FLAGHOLD(TBLGEN(5),1,2) = 'c'
         END IF
         GO TO 210
      END IF
      CALL GENRH1(DB,DP,VAL)
      IF (VAL.LT.5.0.OR.VAL.GT.100.0) THEN
         IF (RH.EQ.MISSING.OR.FLGHLD(5).EQ.'G')THEN
            VALARRAY(TBLGEN(5),ILINE) = '      '
            FLAGHOLD(TBLGEN(5),1,2) = 'a'
         END IF
         GO TO 210
      END IF
C
C  CHECK COMPUTED VALUE AGAINST VALUE ENTERED - IF NONE ENTERED
C  LOAD COMPUTED VALUE

      IF (ICOL.EQ.TBLGEN(5))THEN
         IVAL = VAL / TBLCONV(ICOL) + .5
         IF (RH.EQ.MISSING.OR.FLGHLD(5).EQ.'G')THEN
            WRITE (VALARRAY(ICOL,ILINE),50)IVAL, 'G'
         ELSE
            READ (VALARRAY(ICOL,ILINE),'(I5,1X)') JVAL
            IDIFF = NINT(1.0/TBLCONV(ICOL))
            IF (IVAL.LT.JVAL-IDIFF.OR.IVAL.GT.JVAL+IDIFF) THEN
               FLAGHOLD(ICOL,1,1) = 'C'
               FLAGHOLD(ICOL,1,2) = 'W'
            END IF
         END IF
         RETURN
      ELSE
         IF (RH.EQ.MISSING.OR.FLGHLD(5).EQ.'G')THEN
            RH = VAL
         END IF
      END IF
C
C  CHECK AND CALC DP IF NEEDED
C
  210 CONTINUE
      IF (RH.EQ.MISSING)THEN
         IF (ICOL.EQ.TBLGEN(3))THEN
            IF (DP.EQ.MISSING) THEN
               VALARRAY(ICOL,ILINE) = '      '
               IF (FLAGHOLD(TBLGEN(5),1,2).EQ.'a') THEN
                  FLAGHOLD(ICOL,1,2) = 'a'
               ELSE
                  FLAGHOLD(ICOL,1,2) = 'c'
               END IF
            END IF
            RETURN
         END IF
         GO TO 220
      END IF
      CALL CALCDP(DB,RH,VAL)
      IF (ICOL.EQ.TBLGEN(3))THEN
         IF (DATFRM(3).EQ.'F  ')THEN
            CALL CTOF(VAL)
         END IF
         IVAL = VAL / TBLCONV(ICOL) + .5
         IF (DP.EQ.MISSING.OR.FLGHLD(3).EQ.'G')THEN
            WRITE (VALARRAY(ICOL,ILINE),50)IVAL, 'G'
         ELSE
            READ (VALARRAY(ICOL,ILINE),'(I5,1X)') JVAL
            IF (IVAL.LT.JVAL-1.OR.IVAL.GT.JVAL+1) THEN
               FLAGHOLD(ICOL,1,1) = 'C'
               FLAGHOLD(ICOL,1,2) = 'W'
            END IF
         END IF
         RETURN
      ELSE
         IF (DP.EQ.MISSING.OR.FLGHLD(3).EQ.'G')THEN
            DP = VAL
         END IF
      END IF
C
C  CHECK AND CALC WB IF NEEDED
C
  220 CONTINUE
      IF (DP.EQ.MISSING.OR.SP.EQ.MISSING) THEN
         IF (ICOL.EQ.TBLGEN(4)) THEN
            IF (WB.EQ.MISSING.OR.FLGHLD(4).EQ.'G')THEN
               VALARRAY(ICOL,ILINE) = '      '
               IF (FLAGHOLD(TBLGEN(3),1,2).EQ.'a') THEN
                  FLAGHOLD(ICOL,1,2) = 'a'
               ELSE
                  FLAGHOLD(ICOL,1,2) = 'c'
               END IF
            END IF
            RETURN
         END IF
         GO TO 230
      ELSE IF (DP.LE.-30.0) THEN
         IF (ICOL.EQ.TBLGEN(4))THEN
            IF (WB.EQ.MISSING.OR.FLGHLD(4).EQ.'G')THEN
               VALARRAY(ICOL,ILINE) = '      '
               FLAGHOLD(ICOL,1,2) = 'a'
            END IF
            RETURN
         END IF
         GO TO 230
      ELSE IF (RH.LE.12.AND.DP.LT.-10.0)THEN
         IF (ICOL.EQ.TBLGEN(4))THEN
            IF (WB.EQ.MISSING.OR.FLGHLD(4).EQ.'G')THEN
               VALARRAY(ICOL,ILINE) = '      '
               FLAGHOLD(ICOL,1,2) = 'a'
            END IF
            RETURN
         END IF
         GO TO 230
      END IF
      CALL CALCWB(DB,DP,SP,VAL)
      IF (ICOL.EQ.TBLGEN(4))THEN
         IF (DATFRM(4).EQ.'F  ')THEN
             CALL CTOF(VAL)
         END IF
         IVAL = VAL / TBLCONV(ICOL) + .5
         IF (WB.EQ.MISSING.OR.FLGHLD(4).EQ.'G')THEN
            WRITE (VALARRAY(ICOL,ILINE),50)IVAL, 'G'
         ELSE IF (ASPTWB.EQ.1) THEN
            READ (VALARRAY(ICOL,ILINE),'(I5,1X)') JVAL
            IF (IVAL.LT.JVAL-3.OR.IVAL.GT.JVAL+3) THEN
               FLAGHOLD(ICOL,1,1) = 'C'
               FLAGHOLD(ICOL,1,2) = 'W'
            END IF
         END IF
         RETURN
      END IF
C
C  CHECK AND CALC VP IF NEEDED
C
  230 CONTINUE
      IF (RH.EQ.MISSING) THEN
         IF (ICOL.EQ.TBLGEN(6))THEN
            IF (VP.EQ.MISSING.OR.FLGHLD(6).EQ.'G')THEN
               VALARRAY(TBLGEN(6),ILINE) = '      '
               IF (FLAGHOLD(TBLGEN(5),1,2).EQ.'a') THEN
                  FLAGHOLD(ICOL,1,2) = 'a'
               ELSE
                  FLAGHOLD(TBLGEN(6),1,2) = 'c'
               END IF
            END IF
            RETURN
         END IF
         GO TO 240
      END IF
      CALL CALCVP(DB,RH,VAL)
      IF (ICOL.EQ.TBLGEN(6))THEN
         IVAL = VAL / TBLCONV(ICOL) + .5
         IF (VP.EQ.MISSING.OR.FLGHLD(6).EQ.'G')THEN
            WRITE (VALARRAY(TBLGEN(6),ILINE),50)IVAL, 'G'
            VP = VAL
         ELSE
            READ (VALARRAY(ICOL,ILINE),'(I5,1X)') JVAL
            IF (IVAL.LT.JVAL-1.OR.IVAL.GT.JVAL+1) THEN
               FLAGHOLD(ICOL,1,1) = 'C'
               FLAGHOLD(ICOL,1,2) = 'W'
            END IF
         END IF
         RETURN
      ELSE 
         IF (VP.EQ.MISSING.OR.FLGHLD(6).EQ.'G')THEN
            VP = VAL
         END IF
      END IF
C
C  CHECK AND CALC MR IF NEEDED
C
  240 CONTINUE
      IF (VP.EQ.MISSING.OR.SP.EQ.MISSING)THEN
         IF (ICOL.EQ.TBLGEN(7))THEN
            IF (MR.EQ.MISSING.OR.FLGHLD(7).EQ.'G')THEN
               VALARRAY(TBLGEN(7),ILINE) = '      '
               IF (FLAGHOLD(TBLGEN(6),1,2).EQ.'a') THEN
                  FLAGHOLD(ICOL,1,2) = 'a'
               ELSE
                  FLAGHOLD(TBLGEN(7),1,2) = 'c'
               END IF
            END IF
         END IF
         RETURN
      END IF
      CALL CALCMR(VP,SP,VAL)
      IF (ICOL.EQ.TBLGEN(7))THEN
         IVAL = VAL / TBLCONV(TBLGEN(7)) + .5
         IF (MR.EQ.MISSING.OR.FLGHLD(7).EQ.'G')THEN
            WRITE (VALARRAY(TBLGEN(7),ILINE),50) IVAL, 'G'
         ELSE
            READ (VALARRAY(ICOL,ILINE),'(I5,1X)') JVAL
            IF (IVAL.LT.JVAL-1.OR.IVAL.GT.JVAL+1) THEN
               FLAGHOLD(ICOL,1,1) = 'C'
               FLAGHOLD(ICOL,1,2) = 'W'
            END IF
         END IF
         RETURN
      ELSE
         IF (MR.EQ.MISSING.OR.FLGHLD(7).EQ.'G')THEN
            MR = VAL
         END IF
      END IF
C
      RETURN
      END
