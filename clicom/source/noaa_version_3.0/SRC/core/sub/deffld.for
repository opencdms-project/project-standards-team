$STORAGE:2

      SUBROUTINE DEFFLD(ENDCOL,FLDFGC,FLDBGC)
C
C   ROUTINE TO DEFINE A FIELD ON A DATA ENTRY FORM 
C
      CHARACTER*80 BLANK
      CHARACTER*10 HLDCHR
      CHARACTER*2 INCHAR,RTNFLAG
      CHARACTER*1 RTNCODE
      CHARACTER*72 MESSAGE
      INTEGER*2   ENDCOL,DELFLD,FLDNUM,FLDFGC,FLDBGC
      LOGICAL NEWFLD,MODIFY,DEFLT
C
$INCLUDE: 'FRMFLD.INC'
C
      DATA BLANK /'                     '/
C
      CALL POSLIN(IROW,ICOL)
      CALL ACTPAG(1,IERR)
      FLDNUM = 0
      DELFLD = 0
      DO 10 I = 1,NUMFLD
         IF (FLENGTH(I).GT.0) THEN
            IF (IROW.EQ.FLDROW(I).AND.ICOL.GE.FLDCOL(I).AND.ICOL.LE.
     +            FLDCOL(I)+FLENGTH(I)-1) THEN
               FLDNUM = I
               GO TO 12
            END IF
         ELSE IF (DELFLD.EQ.0) THEN
            DELFLD = I
         END IF
   10 CONTINUE
   12 CONTINUE
      IF (FLDNUM.GT.0) THEN
         NEWFLD = .FALSE. 
         MODIFY = .FALSE.
         IROW = FLDROW(FLDNUM)
         ICOL = FLDCOL(FLDNUM)
         WRITE(MESSAGE,15) FLDNUM
   15    FORMAT('Field ',I2)
      ELSE
         MODIFY = .TRUE.
         IF (DELFLD.EQ.0) THEN
            NEWFLD = .TRUE.
            NUMFLD = NUMFLD + 1
            FLDNUM = NUMFLD
         ELSE
            NEWFLD = .FALSE.
            FLDNUM = DELFLD
         END IF
C
C     INITIALIZE VALUES IF IT'S A NEW FIELD
C
         FLDTYPE(FLDNUM) = ' '
         FLENGTH(FLDNUM) = 0
         LKFILE(FLDNUM) = '  '
         LKFLEN(FLDNUM) = 0
         LKFRECFLD(FLDNUM) = 0
         LKFPOS(FLDNUM) = 0
         HIGHLIM(FLDNUM) = '        '
         LOWLIM(FLDNUM) = '      '
         DEFAULT(FLDNUM) = '     '
         RANGE(FLDNUM) = .FALSE.
         LOOKUP(FLDNUM) = .FALSE.
         NOENTRY (FLDNUM) = .FALSE.
         FLDROW(FLDNUM) = IROW
         FLDCOL(FLDNUM) = ICOL
         WRITE(MESSAGE,30) FLDNUM
   30    FORMAT('Defining field ',I2)
      END IF
   40 CONTINUE
      CALL CLS
      CALL LOCATE(1,1,IERR)
      CALL WRTSTR(MESSAGE,18,12,0)
      CALL LOCATE(2,1,IERR)
C
C   GET THE FIELD TYPE -----------------------------------------------
C
      CALL WRTSTR(
     +'Field Type?(T-Text,S-Numeric String,I-Integer,R-Real,C-Copy) - '
     +     ,63,14,0)
   50 CONTINUE
      IF (FLDTYPE(FLDNUM).EQ.'N') THEN
         FLDTYPE(FLDNUM) = 'I'
      END IF
      CALL LOCATE(2,64,IERR)
      CALL WRTSTR(FLDTYPE(FLDNUM),1,15,1)
      CALL LOCATE(2,64,IERR)
      IF (MODIFY) THEN 
         CALL GETCHAR(0,INCHAR)
         IF (INCHAR.EQ.'4F') THEN
            GO TO 500
C
C      IF "C" IS ENTERED - COPY THE DEFINITION OF ANOTHER FIELD
C
         ELSE IF (INCHAR.EQ.'C ') THEN
            CALL WRTSTR(' Copy Field ',12,15,1)
  60        CONTINUE
            CALL LOCATE(4,1,IERR)
            INCHAR = '  '
            CALL WRTSTR('Field number to be copied? ',27,14,0)
            CALL GETSTR(3,INCHAR,2,15,1,RTNFLAG)
            IF (RTNFLAG.EQ.'4F'.OR.INCHAR.EQ.'  ') THEN
               GO TO 500
            END IF
            READ(INCHAR,'(BN,I2)',ERR=75) INFLD
            IF (INFLD.GT.NUMFLD.OR.INFLD.EQ.FLDNUM) THEN
               CALL WRTMSG(1,131,12,1,0,' ',0)
               GO TO 60
            ELSE
               CALL DITTOFLD(FLDNUM,INFLD,IROW,ICOL,ENDCOL,RTNCODE)
               IF (RTNCODE.EQ.'0') THEN
                  MODIFY = .FALSE.
                  WRITE(MESSAGE,30) FLDNUM
                  GO TO 40
               ELSE
                  GO TO 500
               END IF
            END IF
C
C      OTHERWISE - GET  THE DEFINITION FOR THIS FIELD TYPE
C
         ELSE IF (INCHAR.NE.'RE') THEN
            IF (INCHAR(2:2).EQ.' ') THEN
               FLDTYPE(FLDNUM) = INCHAR(1:1)
            ELSE
               CALL BEEP
               GO TO 50
            END IF
         END IF
      END IF   
      IF (FLDTYPE(FLDNUM).EQ.'I ') THEN
         CALL WRTSTR(' Integer ',9,15,1)
      ELSE IF (FLDTYPE(FLDNUM).EQ.'T ') THEN
         CALL WRTSTR(' Text ',6,15,1)
      ELSE IF (FLDTYPE(FLDNUM).EQ.'S ') THEN
         CALL WRTSTR(' Numeric String',15,15,1)
      ELSE IF (FLDTYPE(FLDNUM).EQ.'R ') THEN
         CALL WRTSTR(' Real Number ',13,15,1)
      ELSE
         CALL BEEP
         GO TO 50
      END IF 
C
C   GET AND CHECK THE FIELD LENGTH -----------------------------------
C
      CALL LOCATE(4,1,IERR)
      CALL WRTSTR('Field Length ? ',15,14,0)
      IF (FLENGTH(FLDNUM).EQ.0) THEN
          INCHAR = '  '
      ELSE IF (FLENGTH(FLDNUM).LT.10) THEN
          WRITE(INCHAR,'(I1)') FLENGTH(FLDNUM)
      ELSE 
         WRITE(INCHAR,'(I2)') FLENGTH(FLDNUM)
      END IF
      CALL WRTSTR(INCHAR,2,15,1)
   70 CONTINUE
      CALL LOCATE(4,16,IERR)
      IF (MODIFY) THEN
         CALL GETSTR(0,INCHAR,2,15,1,RTNFLAG)
         IF (RTNFLAG.EQ.'4F') THEN
            GO TO 500
         END IF
      END IF
      IF (INCHAR.NE.'RE') THEN
         READ(INCHAR,'(BN,I2)',ERR=75) ILEN
      ELSE
         ILEN = FLENGTH(FLDNUM)
      END IF
      IF (ILEN.GT.0) THEN
         IF (ILEN+ICOL-1.GT.ENDCOL) THEN
            CALL WRTMSG(1,132,12,1,0,' ',0)
            GO TO 70
         ELSE
            GO TO 90
         END IF
      END IF
   75 CONTINUE
      CALL BEEP
      GO TO 70
   90 CONTINUE
      IF ((FLDTYPE(FLDNUM).EQ.'I'.OR.FLDTYPE(FLDNUM).EQ.'R').AND.
     +     ILEN.GT.10) THEN
         CALL WRTMSG(1,133,12,1,0,' ',0) 
         GO TO 70
      END IF 
      FLENGTH(FLDNUM) = ILEN
      CALL WRTFNC(4)
C
C   ASK IF LOWERCASE LETTERS SHOULD BE ALLOWED -----------------------
C
      IROW2 = 4
      IF (FLDTYPE(FLDNUM).EQ.'T') THEN 
         IROW2 = IROW2 + 1     
         CALL LOCATE(IROW2,1,IERR)
         CALL WRTSTR('Allow lowercase letters on input ? (Y/N) '
     +       ,41,14,0)
         CALL CHKYN(IROW2,42,MODIFY,LOWCASE(FLDNUM),INCHAR)
         IF (INCHAR.EQ.'4F') THEN
            GO TO 500
         ELSE IF (INCHAR.EQ.'2F') THEN
            GO TO 480
         ELSE IF (INCHAR.EQ.'8F') THEN
            GO TO 40
         END IF
      END IF
C
C   GET THE FIELD LIMITS ---------------------------------------------
C
      IROW2 = IROW2 + 2
      CALL LOCATE(IROW2,1,IERR)
      CALL WRTSTR('Do range check on input ? (Y/N) ',32,14,0)
  100 CONTINUE
      CALL CHKYN(IROW2,33,MODIFY,RANGE(FLDNUM),INCHAR)
      IF (INCHAR.EQ.'4F') THEN
         GO TO 500
      ELSE IF (INCHAR.EQ.'2F') THEN
         GO TO 480
      ELSE IF (INCHAR.EQ.'8F') THEN
         GO TO 40
      END IF
      IF (RANGE(FLDNUM)) THEN  
         IROW2 = IROW2 + 1
         CALL LOCATE(IROW2,1,IERR)  
         CALL WRTSTR(' Low Limit ',11,14,0)
         CALL WRTSTR(LOWLIM(FLDNUM),66,15,1)
         CALL LOCATE(IROW2,12,IERR)
         IF (MODIFY) THEN
            CALL GETSTR(0,LOWLIM(FLDNUM),66,15,1,RTNFLAG)
         END IF
         IF (LOWLIM(FLDNUM)(1:5).EQ.'FIELD') THEN
             READ(LOWLIM(FLDNUM),'(BN,6X,I2)',ERR=105) IFLD
             IF (IFLD.NE.FLDNUM.AND.IFLD.LT.NUMFLD) THEN
                IF (FLDTYPE(FLDNUM).EQ.FLDTYPE(IFLD)) THEN
                   GO TO 107
                ELSE
                   IROW2 = IROW2 - 1
                   CALL WRTMSG(1,134,12,1,0,' ',0)
                   MODIFY = .TRUE.
                   GO TO 100
                END IF
             END IF
  105        CONTINUE
             CALL WRTMSG(1,135,12,1,0,' ',0)
             IROW2 = IROW2 - 1
             GO TO 100
  107        CONTINUE
         END IF    
         IROW2 = IROW2 + 1
         CALL LOCATE(IROW2,1,IERR)  
         CALL WRTSTR('High Limit ',11,14,0)
         CALL WRTSTR(HIGHLIM(FLDNUM),66,15,1)
         CALL LOCATE(IROW2,12,IERR)
         IF (MODIFY) THEN
            CALL GETSTR(0,HIGHLIM(FLDNUM),66,15,1,RTNFLAG)
         END IF
         IF (HIGHLIM(FLDNUM)(1:5).EQ.'FIELD') THEN
             READ(HIGHLIM(FLDNUM),'(BN,6X,I2)',ERR=115) IFLD
             IF (IFLD.NE.FLDNUM.AND.IFLD.LT.NUMFLD) THEN
                IF (FLDTYPE(FLDNUM).EQ.FLDTYPE(IFLD)) THEN
                   GO TO 117
                ELSE
                   CALL WRTMSG(1,134,12,1,0,' ',0)
                   IROW2 = IROW2 - 2
                   MODIFY = .TRUE.
                   GO TO 100
                END IF
             END IF
  115        CONTINUE
             CALL WRTMSG(1,135,12,1,0,' ',0)
             IROW2 = IROW2 - 2
             GO TO 100
  117        CONTINUE
         END IF    
      END IF
C
C   GET THE DEFAULT FIELD VALUE --------------------------------------
C
      IROW2 = IROW2 + 2
      CALL LOCATE(IROW2,1,IERR)
      CALL WRTSTR('Default values on input ? (Y/N) ',32,14,0)
      IF (DEFAULT(FLDNUM).EQ.'            ') THEN
          DEFLT = .FALSE.
      ELSE 
          DEFLT = .TRUE.
      END IF
      CALL CHKYN(IROW2,33,MODIFY,DEFLT,INCHAR)
      IF (INCHAR.EQ.'4F') THEN
         GO TO 500
      ELSE IF (INCHAR.EQ.'2F') THEN
         GO TO 480
      ELSE IF (INCHAR.EQ.'8F') THEN
         GO TO 40
      END IF
      IF (DEFLT) THEN
         IROW2 = IROW2 + 1
         CALL LOCATE(IROW2,1,IERR)  
         CALL WRTSTR(' Default ',9,14,0)
         CALL WRTSTR(DEFAULT(FLDNUM),66,15,1)
         CALL LOCATE(IROW2,10,IERR)
         IF (MODIFY) THEN
            CALL GETSTR(0,DEFAULT(FLDNUM),66,15,1,RTNFLAG)
         END IF
      ELSE
         DEFAULT(FLDNUM) = '           '
      END IF
C
C   GET LOOKUP-FILE INFORMATION --------------------------
C
      IF (NUMFLD.LT.2) THEN
         GO TO 300
      END IF
      IROW2 = IROW2 + 2
      CALL LOCATE(IROW2,1,IERR)
      CALL WRTSTR('Lookup Data from another file ? (Y/N)',37,14,0)
      CALL CHKYN(IROW2,38,MODIFY,LOOKUP(FLDNUM),INCHAR)
      IF (INCHAR.EQ.'4F') THEN
         GO TO 500
      ELSE IF (INCHAR.EQ.'2F') THEN
         GO TO 480
      ELSE IF (INCHAR.EQ.'8F') THEN
         GO TO 40
      END IF
      IF (LOOKUP(FLDNUM)) THEN
C
C      GET LOOKUP FILE NAME
C
         IROW2 = IROW2 + 1
         CALL LOCATE(IROW2,1,IERR)  
         CALL WRTSTR('Full file name (use / for backslash) ',37,14,0)
         CALL WRTSTR(LKFILE(FLDNUM),22,15,1)
         CALL LOCATE(IROW2,38,IERR)
         IF (MODIFY) THEN
            CALL GETSTR(0,LKFILE(FLDNUM),22,15,1,RTNFLAG)
         END IF
         IROW2 = IROW2 + 1
C
C      GET LOOKUP FILE RECORD LENGTH
C
         CALL LOCATE(IROW2,1,IERR)  
         CALL WRTSTR('File record length ',19,14,0)
         IF (LKFLEN(FLDNUM).EQ.0) THEN
            HLDCHR = '  '
         ELSE
            WRITE(HLDCHR,'(I3)') LKFLEN(FLDNUM)
         END IF
         CALL WRTSTR(HLDCHR,3,15,1)
  165    CONTINUE
         CALL LOCATE(IROW2,20,IERR)
         IF (MODIFY) THEN
            CALL GETSTR(0,HLDCHR,3,15,1,RTNFLAG)
            IF (RTNFLAG.EQ.'4F') THEN
               GO TO 500
            ELSE IF (RTNFLAG.EQ.'8F') THEN
               GO TO 40
            END IF
         END IF
         READ(HLDCHR,'(BN,I3)',ERR=170) LKFLEN(FLDNUM)
         GO TO 175
  170    CONTINUE
         CALL WRTMSG(1,69,12,1,0,' ',0)
         GO TO 165
  175    CONTINUE
         IROW2 = IROW2 + 1
C
C      GET LOOKUP FIELD NUMBER
C
         CALL LOCATE(IROW2,1,0)
         CALL WRTSTR('Lookup field number ',20,14,0)
         IF (LKFRECFLD(FLDNUM).EQ.0) THEN
            HLDCHR = '  '
         ELSE
            WRITE(HLDCHR,'(I2)') LKFRECFLD(FLDNUM)
         END IF
         CALL WRTSTR(HLDCHR,2,15,1)
  185    CONTINUE
         CALL LOCATE(IROW2,21,IERR)
         IF (MODIFY) THEN
            CALL GETSTR(0,HLDCHR,2,15,1,RTNFLAG)
            IF (RTNFLAG.EQ.'4F') THEN
               GO TO 500
            ELSE IF (RTNFLAG.EQ.'8F') THEN
               GO TO 40
            END IF
         END IF
         READ(HLDCHR,'(BN,I2)',ERR=190) LKFRECFLD(FLDNUM)
         IFLD = LKFRECFLD(FLDNUM)
         IF (IFLD.NE.FLDNUM.AND.IFLD.LE.NUMFLD.AND.
     +        FLDTYPE(IFLD).EQ.'I'.AND.FLENGTH(IFLD).GT.0) THEN
            GO TO 195
         END IF
  190    CONTINUE
         CALL WRTMSG(1,69,12,1,0,' ',0)
         MODIFY = .TRUE.
         GO TO 185
  195    CONTINUE
         IROW2 = IROW2 + 1
C
C      GET LOOKUP FILE RECORD POSITION TO BE USED
C
         CALL LOCATE(IROW2,1,IERR)  
         CALL WRTSTR('File position to be used ',25,14,0)
         IF (LKFPOS(FLDNUM).EQ.0) THEN
            HLDCHR = '  '
         ELSE
            WRITE(HLDCHR,'(I3)') LKFPOS(FLDNUM)
         END IF
         CALL WRTSTR(HLDCHR,3,15,1)
  200    CONTINUE
         CALL LOCATE(IROW2,26,IERR)
         IF (MODIFY) THEN
            CALL GETSTR(0,HLDCHR,3,15,1,RTNFLAG)
            IF (RTNFLAG.EQ.'4F') THEN
               GO TO 500
            ELSE IF (RTNFLAG.EQ.'8F') THEN
               GO TO 40
            END IF
         END IF
         READ(HLDCHR,'(BN,I3)',ERR=210) LKFPOS(FLDNUM)
         GO TO 220
  210    CONTINUE
         CALL WRTMSG(1,69,12,1,0,' ',0)
         GO TO 200
  220    CONTINUE
      END IF
C
C   FIND OUT IF PREVENT DATA ENTRY IN THIS FIELD ---------------------
C
  300 CONTINUE
      IROW2 = IROW2 + 2
      CALL LOCATE(IROW2,1,IERR)
      CALL WRTSTR('Prevent data entry ? (Y/N)',26,14,0)
      CALL CHKYN(IROW2,27,MODIFY,NOENTRY(FLDNUM),INCHAR)
      IF (INCHAR.EQ.'4F') THEN
         GO TO 500
      ELSE IF (INCHAR.EQ.'2F') THEN
         GO TO 480
      ELSE IF (INCHAR.EQ.'8F') THEN
         GO TO 40
      END IF
C
C   VERIFY THE INFORMATION ENTERED -----------------------------------
C
  380 CONTINUE
      CALL LOCATE(22,10,IERR)
      CALL WRTSTR(' F2 Enter, F4 Abort, F7 Delete, F8 Modify ',41,15,3)
  400 CONTINUE
      CALL GETCHAR(0,INCHAR)
      IF (INCHAR.EQ.'2F') THEN
         GO TO 480
      ELSE IF (INCHAR.EQ.'4F') THEN
         GO TO 500
      ELSE IF (INCHAR.EQ.'7F') THEN
         CALL ACTPAG(0,IERR)
         CALL LOCATE(IROW,ICOL,IERR)
         CALL WRTSTR(BLANK,FLENGTH(FLDNUM),14,0)
         FLENGTH(FLDNUM) = 0
         RETURN
      ELSE IF (INCHAR.EQ.'8F') THEN
         MODIFY = .TRUE.
         GO TO 40
      ELSE
         CALL BEEP
         GO TO 400
      END IF
C
C   WRITE THE FIELD TO THE FORM --------------------------------------
C
  480 CONTINUE
      CALL ACTPAG(0,IERR)
      CALL LOCATE(IROW,ICOL,IERR)
      CALL WRTSTR(BLANK,FLENGTH(FLDNUM),FLDFGC,FLDBGC)
      RETURN
C
C   ABORT THE CURRENT FIELD
C
  500 CONTINUE
      IF (NEWFLD) THEN
         NUMFLD = NUMFLD - 1
      ELSE IF (FLDNUM.EQ.DELFLD) THEN
         FLENGTH(FLDNUM) = 0
      END IF
      CALL ACTPAG(0,IERR)
      CALL LOCATE(IROW,ICOL,IERR)
      RETURN
      END

$PAGE

      SUBROUTINE SAVFLD
C
C   ROUTINE TO WRITE THE FIELD DEFINITIONS TO DISK
C
$INCLUDE: 'FRMFLD.INC'
C
      INTEGER*2 IOLD,INEW,OLDFLD(MAXFLD),NEWFLD(MAXFLD)
     +         ,TROW(MAXFLD),TCOL(MAXFLD)
      CHARACTER*2 HLDCHR
      CHARACTER*1 REPLY,RTNCODE,SORTFLDS
C
C   ASK THE USER IF HE WANTS THE FIELDS SORTED ON OUTPUT
C
      CALL LOCATE(23,1,IERR)
      CALL WRTSTR('Sort the fields by row and column - ',36,12,0)
      CALL BEEP
      CALL OKREPLY(REPLY,RTNCODE)
      IF (REPLY.EQ.'Y'.AND.RTNCODE.EQ.'0') THEN
         SORTFLDS = 'Y'
      ELSE
         SORTFLDS = 'N'
      END IF
C
C   REMOVE DELETED FIELDS AND SORT BY ROW AND COLUMN IF REQUESTED
C
      IOLD = 0
      INEW = 0
   50 CONTINUE
      IOLD = IOLD + 1
      IF (FLENGTH(IOLD).GT.0) THEN
         INEW = INEW + 1
         IF (INEW.EQ.1) THEN
            OLDFLD(INEW) = IOLD
            NEWFLD(IOLD) = INEW
            TROW(INEW) = FLDROW(IOLD)
            TCOL(INEW) = FLDCOL(IOLD)
         ELSE
            DO 100 I = INEW-1,1,-1
               IF (FLDROW(IOLD).GT.TROW(I).OR.(FLDROW(IOLD).EQ.
     +               TROW(I).AND.FLDCOL(IOLD).GT.TCOL(I)).OR.
     +               SORTFLDS.EQ.'N') THEN
                  DO 80 I2 = INEW, I+2, -1
                     OLDFLD(I2) = OLDFLD(I2-1)
                     NEWFLD(OLDFLD(I2)) = I2
                     TROW(I2) = TROW(I2-1)
                     TCOL(I2) = TCOL(I2-1)
   80             CONTINUE
                  OLDFLD(I+1) = IOLD
                  NEWFLD(IOLD) = I+1
                  TROW(I+1) = FLDROW(IOLD)
                  TCOL(I+1) = FLDCOL(IOLD)
                  GO TO 120
               END IF
  100       CONTINUE
            DO 110 I2 = INEW, 2, -1
               OLDFLD(I2) = OLDFLD(I2-1)
               NEWFLD(OLDFLD(I2)) = I2
               TROW(I2) = TROW(I2-1)
               TCOL(I2) = TCOL(I2-1)
  110       CONTINUE
            OLDFLD(1) = IOLD
            NEWFLD(IOLD) = 1
            TROW(1) = FLDROW(IOLD)
            TCOL(1) = FLDCOL(IOLD)
         END IF
      END IF
  120 CONTINUE
C
      IF (IOLD.LT.NUMFLD) THEN
         GO TO 50
      END IF
C
C    REPLACE ANY INTERNAL FIELD NUMBERS WITH THE NEW VALUES
C
      DO 200 I = 1,INEW
         IFLD = OLDFLD(I)
         IF (RANGE(IFLD)) THEN
            IF (LOWLIM(IFLD)(1:5).EQ.'FIELD') THEN
               READ(LOWLIM(IFLD),'(BN,6X,I2)') IFLD2
               WRITE(HLDCHR,'(I2)') NEWFLD(IFLD2)
               LOWLIM(IFLD)(7:8) = HLDCHR
               IF (NEWFLD(IFLD2).GT.INEW.OR.FLDTYPE(NEWFLD(IFLD2)).NE.
     +               FLDTYPE(IFLD)) THEN
                  RANGE(IFLD) = .FALSE.
               END IF
            END IF
            IF (HIGHLIM(IFLD)(1:5).EQ.'FIELD') THEN
               READ(HIGHLIM(IFLD),'(BN,6X,I2)') IFLD2
               WRITE(HLDCHR,'(I2)') NEWFLD(IFLD2)
               HIGHLIM(IFLD)(7:8) = HLDCHR
               IF (NEWFLD(IFLD2).GT.INEW.OR.FLDTYPE(NEWFLD(IFLD2)).NE.
     +               FLDTYPE(IFLD)) THEN
                  RANGE(IFLD) = .FALSE.
               END IF
            END IF
         END IF
         IF (LOOKUP(IFLD)) THEN
            LKFRECFLD(IFLD) = NEWFLD(LKFRECFLD(IFLD))
         END IF
  200 CONTINUE
C
C   WRITE THE FIELDS IN THE NEW ORDER
C 
      WRITE (15) INEW,(FLDTYPE(OLDFLD(I)),FLDROW(OLDFLD(I))
     +                ,FLDCOL(OLDFLD(I)),FLENGTH(OLDFLD(I))
     +                ,LOWCASE(OLDFLD(I))
     +                ,RANGE(OLDFLD(I)),LOWLIM(OLDFLD(I))
     +                ,HIGHLIM(OLDFLD(I)),DEFAULT(OLDFLD(I))
     +                ,LOOKUP(OLDFLD(I)),LKFILE(OLDFLD(I))
     +                ,LKFLEN(OLDFLD(I)),LKFRECFLD(OLDFLD(I))
     +                ,LKFPOS(OLDFLD(I)),NOENTRY(OLDFLD(I)),I=1,INEW) 
      RETURN
      END
$PAGE

      SUBROUTINE CHKYN(IROW,ICOL,MODIFY,OUTPUT,INCHAR)
C
C   ROUTINE TO CHECK FOR A Y/N RESPONSE AND SET THE OUTPUT VARIABLE
C      TO TRUE IF YES AND FALSE IF NO
C
C     IF FUNCTION KEYS F2, F4, OR F8 ARE ENTERED IT RETURNS THEM
C
      LOGICAL OUTPUT,MODIFY
      CHARACTER*2 INCHAR

  100 CONTINUE
      CALL LOCATE(IROW,ICOL,IERR)
      IF (OUTPUT) THEN   
         CALL WRTSTR('Y    ',5,14,0)
         INCHAR = 'Y ' 
      ELSE
         CALL WRTSTR('N    ',5,14,0)
         INCHAR = 'N '
      END IF
      CALL LOCATE(IROW,ICOL,IERR)
      IF (MODIFY) THEN 
         CALL GETCHAR(0,INCHAR)
         IF (INCHAR.EQ.'4F'.OR.INCHAR.EQ.'2F'.OR.INCHAR.EQ.'8F') THEN
            RETURN
         ELSE IF (INCHAR.EQ.'N ') THEN
            OUTPUT = .FALSE.
         ELSE IF (INCHAR.EQ.'Y') THEN
            OUTPUT = .TRUE.
         ELSE IF (INCHAR.NE.'RE') THEN
            CALL BEEP
            GO TO 100
         END IF
      END IF
      IF (OUTPUT) THEN
         CALL WRTSTR(' Yes ',5,15,1)
      ELSE
         CALL WRTSTR(' No ',4,15,1)
      END IF
C
      RETURN
      END
$PAGE
      SUBROUTINE DITTOFLD(FLDNUM,INFLD,IROW,ICOL,ENDCOL,RTNCODE)
C
C   ROUTINE TO COPY THE FIELD DEFINITION OF FIELD INFLD TO FIELD
C   NUMFLD
C
      CHARACTER*1 RTNCODE
      CHARACTER*2 INCHAR
      INTEGER*2   INFLD,ENDCOL,FLDNUM
C
$INCLUDE: 'FRMFLD.INC'
C
      RTNCODE = '0'
C
C   CHECK IF THERE IS ROOM FOR THE SPECIFIED FIELD AT THE CURRENT
C     CURSOR POSITION
C
      IF (FLENGTH(INFLD)+ICOL-1.GT.ENDCOL) THEN
         CALL WRTMSG(1,132,12,1,0,' ',0)
         RTNCODE = '1'
         CALL WRTMSG(20,202,14,0,0,' ',0)
         CALL GETCHAR(0,INCHAR)
         RETURN
      END IF
C 
C   COPY THE FIELD DEFINTIION FOR THE FIELD SPECIFIED
C
      FLDROW(FLDNUM) = IROW
      FLDCOL(FLDNUM) = ICOL
      FLDTYPE(FLDNUM) = FLDTYPE(INFLD)
      LOWCASE(FLDNUM) = LOWCASE(INFLD)
      RANGE(FLDNUM) = RANGE(INFLD)
      FLENGTH(FLDNUM) = FLENGTH(INFLD) 
      HIGHLIM(FLDNUM) = HIGHLIM(INFLD) 
      LOWLIM(FLDNUM) = LOWLIM(INFLD) 
      DEFAULT(FLDNUM) = DEFAULT(INFLD) 
      LOOKUP(FLDNUM) = LOOKUP(INFLD) 
      LKFILE(FLDNUM) = LKFILE(INFLD) 
      LKFLEN(FLDNUM) = LKFLEN(INFLD) 
      LKFRECFLD(FLDNUM) = LKFRECFLD(INFLD) 
      LKFPOS(FLDNUM) = LKFPOS(INFLD) 
      NOENTRY(FLDNUM) = NOENTRY(INFLD)
      RETURN
      END