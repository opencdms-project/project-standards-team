$STORAGE:2
C
      SUBROUTINE PRTFRM(FORMNAME,FRMHT,FRMWID,NCHAR,ILINE,IPAGE)
C
C   ROUTINE TO PRINT THE DEFINITION OF THE CURRENT FORM
C
C   DEFINITION OF PASSED VARIABLES
C
C     FORMNAME...NAME OF THE CURRENT FORM
C     FRMHT......FORM HEIGHT
C     FRMWID.....FORM WIDTH
C     NCHAR......80 X 23 ARRAY CONTAINING THE SCREEN DEFINITION OF
C                THE CURRENT FORM WITH THE INTEGER VALUES OF THE CHARS
C     ILINE......THE CURRENT LINE NUMBER ON THE OUTPUT PAGE
C     IPAGE......THE CURRENT OUTPUT PAGE NUMBER 
C
      CHARACTER*80 FRMLIN(24),BDRLIN,SCALE
      CHARACTER*14 TYPDEF
      CHARACTER*8 FORMNAME
      CHARACTER*1 HCHAR,ICASE,IRANGE,IENTRY,ILOOK
      INTEGER*2 FRMWID,FRMHT,NCHAR(80,23)
C
$INCLUDE: 'FRMFLD.INC'
C
C   SET THE BORDER LINE AND SCALE
C
      DATA SCALE /'1       10        20        30        40        50  
     +     60        70        80'/
C
      DO 50 I = 1,80
         IF (MOD(I,5).EQ.0) THEN
            BDRLIN(I:I) = '+'
         ELSE
            BDRLIN(I:I) = '-'
         END IF
   50 CONTINUE
C
C     DETERMINE IF THIS FORM WILL FIT ON THIS PAGE
C
      IF (ILINE.GT.4) THEN
         ILEN = FRMHT + NUMFLD + 13 + 3
         DO 100 IFLD = 1,NUMFLD
            IF (RANGE(IFLD)) THEN
               ILEN = ILEN + 2
            END IF
            IF (LOOKUP(IFLD)) THEN
               ILEN = ILEN + 2
            END IF
            IF (DEFAULT(IFLD).NE.'  ') THEN
               ILEN = ILEN + 1
            END IF
  100    CONTINUE
         IF (ILINE+ILEN.GT.61) THEN
            DO 110 I2 = ILINE,61
               WRITE(50,'(10X)')
  110       CONTINUE
            IF (IPAGE.GT.0) THEN
               IF (IPAGE.LT.10) THEN
                  WRITE(50,115) IPAGE           
  115             FORMAT(37X,'PAGE - ',I1)
               ELSE
                  WRITE(50,116) IPAGE
  116             FORMAT(37X,'PAGE - ',I2)
               END IF
            END IF
            WRITE(50,118)
  118       FORMAT(1H1,31X,'CLICOM FORTRAN Forms',/,1X,80('Ä'),/)            
            ILINE = 4
            IPAGE = IPAGE + 1
         ELSE 
            WRITE(50,120)
  120       FORMAT(/,1X,80('='),/)
            ILINE = ILINE + 3
         END IF
      END IF
C
C     DRAW THE FORM AS IT APPEARS ON THE SCREEN
C
      WRITE(50,130) FORMNAME
  130 FORMAT(2X,'Form ',A8,/,2X,'---- --------',/)
      WRITE(50,140) SCALE
      WRITE(50,140) BDRLIN        
  140 FORMAT(1X,A80)
C
C  FIRST SET THE FORM DISPLAY CHARACTERS
C
      DO 160 J = 1,FRMHT
         FRMLIN(J) = '  '
         DO 150 I1 = 1,FRMWID
            HCHAR = CHAR(NCHAR(I1,J))
            FRMLIN(J)(I1:I1) = HCHAR
  150    CONTINUE
  160 CONTINUE           
C
C  THEN SET THE FIELD LOCATIONS TO UNDERSCORES
C 
      DO 180 IFLD = 1,NUMFLD
         J1 = FLDROW(IFLD) + 1
         I1 = FLDCOL(IFLD) + 1
         I2 = I1 + FLENGTH(IFLD) - 1
         DO 170 I3 = I1,I2
            FRMLIN(J1)(I3:I3) = '_'
  170    CONTINUE
  180 CONTINUE
C
C  WRITE THE RESULTING FORM
C
      DO 200 J = 1,FRMHT
         WRITE(50,140) FRMLIN(J)
         ILINE = ILINE + 1
  200 CONTINUE
      WRITE(50,140) BDRLIN        
      WRITE(50,140) SCALE
      ILINE = ILINE + 13
C
C  WRITE THE FIELD DEFINITIONS
C
      WRITE(50,210) 
  210 FORMAT(/,2X,'Field Definitions',/,2X,'-----------------',/)
      WRITE(50,220)
  220 FORMAT(1X,'Number  Type           Row  Col Length Low-case '
     +    ,'Range-Chk  Look-up  No-entry',/
     +    ,1X,'------  -------------- ---  --- ------ -------- '
     +    ,'---------  -------  --------')
      DO 400 IFLD = 1,NUMFLD
         ICASE = ' '
         ILEN = 1
         IF (FLDTYPE(IFLD).EQ.'T') THEN
            TYPDEF = 'Text'
            IF (LOWCASE(IFLD)) THEN
               ICASE = 'Y'
            ELSE
               ICASE = 'N'
            END IF
         ELSE IF (FLDTYPE(IFLD).EQ.'S'.OR.FLDTYPE(IFLD).EQ.'N') THEN
            TYPDEF = 'Numeric String'
         ELSE IF (FLDTYPE(IFLD).EQ.'I') THEN
            TYPDEF = 'Integer'
         ELSE IF (FLDTYPE(IFLD).EQ.'R') THEN
            TYPDEF = 'Real Number'
         ELSE
            TYPDEF = '  '
         END IF
         IF (RANGE(IFLD)) THEN
            IRANGE = 'Y'
            ILEN = ILEN + 2
         ELSE
            IRANGE = 'N'
         END IF
         IF (NOENTRY(IFLD)) THEN
            IENTRY = 'Y'
         ELSE
            IENTRY = 'N'
         END IF
         IF (LOOKUP(IFLD)) THEN
            ILOOK = 'Y'
            ILEN = ILEN + 2
         ELSE
            ILOOK = 'N'
         END IF
         IF (DEFAULT(IFLD).NE.'     ') THEN
            ILEN = ILEN + 1
         END  IF
         IF (ILINE+ILEN.GT.61) THEN
            DO 230 I2 = ILINE,61
               WRITE(50,'(10X)')
  230       CONTINUE
            IF (IPAGE.GT.0) THEN
               IF (IPAGE.LT.10) THEN
                  WRITE(50,115) IPAGE           
               ELSE
                  WRITE(50,116) IPAGE
               END IF
            END IF
            WRITE(50,118)
            ILINE = 6
            WRITE(50,220)
            IPAGE = IPAGE + 1
         END IF
         WRITE(50,240) IFLD,TYPDEF,FLDROW(IFLD),FLDCOL(IFLD)
     +        ,FLENGTH(IFLD),ICASE,IRANGE,ILOOK,IENTRY
  240    FORMAT(3X,I2,4X,A14,2X,I2,3X,I2,3X,I2,6X,A1,9X,A1
     +        ,9X,A1,8X,A1)
         ILINE = ILINE + 1
         IF (RANGE(IFLD)) THEN
            WRITE(50,250) LOWLIM(IFLD),HIGHLIM(IFLD)
  250       FORMAT(12X,'Low Limit : ',A72,/
     +            ,12x,'High Limit: ',A72)
            ILINE = ILINE + 2
         END IF
         IF (DEFAULT(IFLD).NE.'     ') THEN
            WRITE(50,260) DEFAULT(IFLD)
  260       FORMAT(12X,'Default   : ',A72)
            ILINE = ILINE + 1
         END IF
         IF (LOOKUP(IFLD)) THEN
            WRITE(50,270) LKFILE(IFLD),LKFLEN(IFLD),LKFPOS(IFLD)
     +         ,LKFRECFLD(IFLD)
  270       FORMAT(12X,'Lookup File: ',A20,5X,'Record Length: ',I3,/
     +            ,12X,'Position used: ',I3,15X
     +            ,'Field used for lookup: ',I2)
            ILINE = ILINE + 2
         END IF
  400 CONTINUE
C
      RETURN
      END