$STORAGE:2
      SUBROUTINE GETSET(DSETID,ITYPFLG,RECTYPE,HOURLBL,RTNCODE)
C
C       ** OBJECTIVE:  ROUTINE OPENS THE SETUP FILE; 
C          IF ITYPFLG < 0, ROUTINE ENDS AT THIS POINT.
C          IF ITYPFLG > 0 THEN ROUTINE LOADS THE ELEMENT CODES
C          AND COLUMN HEADERS.  IT ALSO READS THE DATAQC.PRM FILE FOR
C          CONTROLLING INFORMATION ON GENERATING MOISTURE VARIABLES FOR
C          SYNOPTIC AND HOURLY DATA
C
C       ** INPUT:
C            DSETID.....DATASET ID -- INTEGER VALUE BETWEEN 001 AND 999
C            ITYPFLG....INDEX TO  OBSERVATION TYPE -- 1=MLY, 2=10D, 3=DLY,
C                       4=SYN, 5=HLY, 6=15M, 7=U-A -- ALSO A FLAG TO INDICATE
C                       WHEN TO EXIT ROUTINE.
C                       <0 = EXIT AFTER OPENING SETUP FILE AND CALCULATING
C                            RECORD LENGTH OF .TWF FILE
C                       >0 = EXECUTE ENTIRE ROUTINE 
C            RECTYPE....OBSERVATION TYPE -- DLY, MLY, ETC.
C       ** OUTPUT:
C            HOURLBL....HOUR LABELS FOR HOURLY AND SYNOPTIC DATA
C            RTNCODE....ERROR FLAG
C                       0=NORMAL EXIT
C                       1=ERROR EXIT -- DATASET NOT FOUND IN SETUP FILE
C                                    -- ERROR IN DATAQC.PRM FILE          
C
$INCLUDE: 'INDEX.INC'
$INCLUDE: 'VAL1.INC'
$INCLUDE: 'GENMOIST.INC'
      CHARACTER*1 DELMARK,RTNCODE
      CHARACTER*2 HOURLBL(*)
      CHARACTER*3 RECTYPE,DATASET,INTYPE,INDDS,ELEMCODE
      CHARACTER*4 GENPRM(4)
      CHARACTER*16 ELEMNAME
      CHARACTER*80 SCRATCH
      INTEGER*2 DSETID,GENELEM(NUMGEN)
      LOGICAL ENDRDSET
C
C       ** GET THE VALUE OF ITYPE FROM ITYPFLG; SET FLAG THAT DETERMINES THE
C          LOCATION OF EXIT FROM PROGRAM;  
C          TRUE=EXIT AFTER READING SETUP.DAT
C
      IF (ITYPFLG.LT.0) THEN
         ENDRDSET = .TRUE.
      ELSE   
         ENDRDSET = .FALSE.
      ENDIF   
      ITYPE = ABS(ITYPFLG)
C
C       ** OPEN SETUP.DAT FILE
C
      IRECLEN = 18 + 2*MAXELEM + 5*2
  30  CONTINUE
      OPEN (5,FILE='P:\DATA\SETUP.DAT',STATUS='OLD',ACCESS='DIRECT'
     +      ,RECL=IRECLEN,SHARE='DENYWR',MODE='READ',IOSTAT=IOCHK)
      IF(IOCHK.NE.0) THEN
         CALL OPENMSG('P:\DATA\SETUP.DAT     ','GETSET       ',IOCHK)
         GO TO 30
      END IF   
C
C       ** SEARCH FOR THE DATATYPE AND DATASET-ID SPECIFIED - REMEMBER 
C          THERE ARE 10 SLOTS ALLOWED FOR EACH DATATYPE SO BEGIN THE 
C          SEARCH WHERE APPROPRIATE (10D AND 15M ADDED AFTER THE OTHER 5)
C
      WRITE(DATASET,'(I3.3)') DSETID
      IF (ITYPE.EQ.2) THEN
         JTYPE = 6
      ELSE IF (ITYPE.EQ.6) THEN
         JTYPE = 7
      ELSE IF (ITYPE.EQ.7) THEN
         JTYPE = 5  
      ELSE IF (ITYPE.GT.2) THEN
         JTYPE = ITYPE - 1
      ELSE 
         JTYPE = 1
      END IF
      ISTRT = (JTYPE-1)*10 + 1
      DO 40 IREC = ISTRT,999
         READ(5,REC=IREC,ERR=45) DELMARK,INTYPE,INDDS,NUMELEM
     +         ,HIGHPCT,LOWPCT
     +         ,(TBLELEM(I1),I1=1,MAXELEM),(AQCELEM(I2),I2=1,5)
         IF (DELMARK.EQ.' '.AND.INTYPE.EQ.RECTYPE.AND.
     +           INDDS.EQ.DATASET) THEN
             GO TO 50
         END IF
   40 CONTINUE
C
C       ** IF DATASET NOT FOUND - PRINT ERROR MESSAGE AND RETURN
C
   45 CONTINUE
         CALL WRTMSG(4,64,12,1,1,' ',0)
         CLOSE(5)
         RTNCODE = '1'
         RETURN
   50 CONTINUE
      CLOSE(5)
C
C       ** CALCULATE THE RECORD LENGTH FOR THE .TWF FILE -- NUMELEM IS READ 
C          FROM THE FILE SETUP.DAT FOR THE CURRENT KEY ENTRY FORM.  NUMLINE 
C          IS DEFINED IN THE CALLING ROUTINE FOR THE CURRENT DATA TYPE.  
      RLNGTH = (NUMELEM * NUMLINE * 8) + 21
C      
C          ** EXIT ROUTINE IF FLAG IS SET      
      IF (ENDRDSET) THEN
         RTNCODE = '0'
         RETURN
      ENDIF   
C
C       ** FIND THE ABBREVIATIONS AND SCALE FACTORS THAT GO WITH THE ELEMENTS
C
   55 CONTINUE
      OPEN(5,FILE='P:\DATA\ELEM.DEF',STATUS='OLD',ACCESS='DIRECT'
     +      ,RECL=110,SHARE='DENYWR',MODE='READ',IOSTAT=IOCHK)
      IF(IOCHK.NE.0) THEN
         CALL OPENMSG('P:\DATA\ELEM.DEF      ','GETSET       ',IOCHK)
         GO TO 55
      END IF   
C
      DO 60 I = 1,NUMELEM
         READ (5,REC=TBLELEM(I)) ELEMCODE,ELEMNAME,TBLEABRV(I)
     +          ,SCRATCH,TBLCONV(I)
   60 CONTINUE
      CLOSE(5)
      DO 70 I = NUMELEM+1,MAXELEM
         TBLEABRV(I) = '      '
   70 CONTINUE 
C
C       ** BUILD THE INDEX AND DATA FILE NAMES
C
      FILNAM = 'P:\DATA\AAAAAA.TWF'
      IDXNAM = 'P:\DATA\AAAAAA.IDX'
      FILNAM(9:11) = RECTYPE
      FILNAM(12:14) = DATASET
      IDXNAM(9:11) = RECTYPE
      IDXNAM(12:14) = DATASET
C
C       ** OPEN THE DATAQC PARAMETER FILE
C
   80 CONTINUE
      OPEN(9,FILE='P:\DATA\DATAQC.PRM',STATUS='OLD',IOSTAT=IOCHK)
      IF(IOCHK.NE.0) THEN
         CALL OPENMSG('P:\DATA\DATAQC.PRM   ','GETSET       ',IOCHK)
         GO TO 80
      END IF   
C
C       ** IF THIS IS SYNOPTIC OR HOURLY DATA, READ THE PARAMETERS THAT 
C          CONTROL GENERATION OF MOISTURE VARIABLES
C
      IF (ITYPE.EQ.4.OR.ITYPE.EQ.5) THEN
C
C          ** READ THE PARAMETERS AS COMMA DELIMITTED TEXT (CONVERT ALL
C             LOWER CASE TEXT TO UPPER AND CHECK VALUES
C
         DO 100 I = 1,NUMGEN
            READ(9,'(A80)',END=210) SCRATCH
C            IF (I.EQ.4) THEN
C                .. ELEMENT 102 HAS AN ADDITIONAL FIELD TO INDICATE WHETHER
C                   THE MEASUREMENT WAS ASPIRATED, NON-ASPIRATED OR MIXED
C               NBRVAL=4
C            ELSE
C                .. ALL ELEMENTS EXCEPT 102 HAVE THREE FIELDS            
C               NBRVAL=3
C            ENDIF      
            NBRVAL=3
            CALL PARSE1(SCRATCH,80,NBRVAL,4,GENPRM,RTNCODE)
            IF (RTNCODE.GT.'0') THEN
               GO TO 210
            END IF               
            READ(GENPRM(1),'(BN,I3)',ERR=210) GENELEM(I)
C
C             .. INITIAL THE ASPIRATED CODE WITH VALUE READ IN 
C                FROM THE DATAQC.PRM FILE
C            
C            IF (I.EQ.4) THEN
C               IF(GENELEM(I).EQ.102) THEN
C                  ASPCODE = GENPRM(4)(1:1)
C                  CALL LOW2UP(ASPCODE)
C                   .. CHECK THAT THE INPUT CODE IS ONE OF THE FOLLOWING
C                      'A'=ASPIRATED    'N'=NON-ASPIRATED 
C                      'M'=MIXTURE OF ASPIRATED/NON-ASPIRATED 
C                  IF (ASPCODE.NE.'A' .AND.
C     +                ASPCODE.NE.'N' .AND.
C     +                ASPCODE.NE.'M') THEN
C                      GO TO 210   
C                  ENDIF
C               ELSE
C                  GO TO 210   
C               ENDIF     
C            ENDIF         
            GENCODE(I) = GENPRM(2)(1:1)
            IF (GENCODE(I).EQ.'y') THEN
               GENCODE(I) = 'Y'
            ELSE IF (GENCODE(I).EQ.'n'.OR.GENCODE(I).EQ.' ') THEN
               GENCODE(I) = 'N'
            END IF
            IF (GENCODE(I).NE.'Y'.AND.GENCODE(I).NE.'N') THEN
               GO TO 210
            END IF
            DATFRM(I)  = GENPRM(3)
            IF (DATFRM(I).EQ.'Ins'.OR.DATFRM(I).EQ.'ins') THEN
               DATFRM(I) = 'INS'
            ELSE IF (DATFRM(I).EQ.'Mb'.OR.DATFRM(I).EQ.'mb') THEN
               DATFRM(I) = 'MB '
            ELSE IF (DATFRM(I).EQ.'Pct'.OR.DATFRM(I).EQ.'pct') THEN
               DATFRM(I) = 'PCT'
            ELSE IF (DATFRM(I).EQ.'f  ') THEN
               DATFRM(I) = 'F '
            ELSE IF (DATFRM(I).EQ.'c  ') THEN
               DATFRM(I) = 'C  '
            END IF
            IF (GENELEM(I).GE.101.AND.GENELEM(I).LE.103) THEN
               IF (DATFRM(I).NE.'F  '.AND.DATFRM(I).NE.'C  ') THEN
                  GO TO 210
               END IF
            ELSE IF (GENELEM(I).EQ.105) THEN
               IF (DATFRM(I).NE.'PCT') THEN
                  GO TO 210
               END IF
            ELSE IF (GENELEM(I).EQ.106) THEN
               IF (DATFRM(I).NE.'INS'.AND.DATFRM(I).NE.'MB ') THEN
                  GO TO 210
               END IF
            ELSE IF (GENELEM(I).EQ.166.OR.GENELEM(I).EQ.172) THEN
               IF (DATFRM(I).NE.'  ') THEN
                  GO TO 210
               END IF
            ELSE
               GO TO 210
            END IF  
  100    CONTINUE
C
C          ** DETERMINE WHICH ELEMENTS IN THIS DATA SET ARE TO BE GENERATED 
C             AND WHAT COLUMN THEY ARE IN.
C
         DO 130 I = 1,NUMELEM
            GENTBL(I) = 'N'
  130    CONTINUE
C
         DO 150 I = 1,NUMGEN
            TBLGEN(I) = 0
            DO 140 J = 1,NUMELEM
               IF (GENELEM(I).EQ.TBLELEM(J))THEN
                  TBLGEN(I) = J
                  IF(GENCODE(I).EQ.'Y')THEN
                     GENTBL(J) = 'Y'
                  END IF
               END IF
  140       CONTINUE
  150    CONTINUE
      END IF
      CLOSE(9)
C
C       ** READ THE HOUR LABELS FOR HOURLY AND SYNOPTIC DATA FROM DATAQC.PRM 
C
      RTNCODE = '0'
      CALL RDHRLBL(ITYPE,HOURLBL,RTNCODE)
C
      RETURN
C
C       ** REACH THIS POINT IF DATAQC.PRM FILE IS TOO SHORT SO AN EOF WAS 
C          READ, OR A RECORD CONTAINED A FORMAT ERROR.
C
210   CONTINUE
      WRITE(INDDS,'(I2)') I
      CALL WRTMSG(3,174,12,1,1,INDDS,2)
      CLOSE(9)
      RTNCODE = '1'
      RETURN

      END      
