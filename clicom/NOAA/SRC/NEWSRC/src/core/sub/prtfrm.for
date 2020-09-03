$STORAGE:2
C
      SUBROUTINE PRTFRM(FORMNAME,FRMHT,FRMWID,NCHAR,ILINE,IPAGE,RTNERR)
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
      CHARACTER*80 FRMLIN(24),BDRLIN,SCALE,PRTTEXT(18),MSGTXT
      CHARACTER*14 TYPDEF
      CHARACTER*25 PRTFILE
      CHARACTER*8  FORMNAME
      CHARACTER*1  HCHAR,ICASE,IRANGE,IENTRY,ILOOK,PRTCHR(6),RTNERR
      INTEGER*2    FRMWID,FRMHT,NCHAR(80,23)
C
$INCLUDE: 'FRMFLD.INC'
C
C   SET THE BORDER LINE AND SCALE
C
      DATA SCALE /'1       10        20        30        40        50  
     +     60        70        80'/
C
C
      RTNERR = '0'
      PRTFILE = 'USER\PRTFRM.PRM'
      DO 10 K = 1,18
         PRTTEXT(K) = '     ' 
  10   CONTINUE
C
C     *** READ THE TEXT FOR ALL FIELDS FORM PARAMETER FILE
C
      MSGTXT = '  '
      OPEN(59,FILE=PRTFILE,STATUS='OLD',FORM='FORMATTED',IOSTAT=IOCHK)
      IF (IOCHK.NE.0) THEN
         CALL CLS
         WRITE(MSGTXT,'(A15,1X,A6,1X,I4)')PRTFILE,'PRTFRM',IOCHK
         CALL WRTMSG(5,2,12,1,1,MSGTXT,28)
         RTNERR = '2'
         RETURN
      END IF
C
      DO 30 I = 1,18
         READ(59,*,ERR=800) PRTTEXT(I)
  30  CONTINUE
      READ(59,*,ERR=800) (PRTCHR(I),I = 1,6)
      CLOSE(59)
C
      OPEN (50,FILE='PRN',STATUS='UNKNOWN',FORM='FORMATTED')
      CALL LOCATE(23,0,IERR)
      CALL WRTSTR(PRTTEXT(18),14,12,0)
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
                  WRITE(50,115) PRTTEXT(1),IPAGE           
  115             FORMAT(37X,'(A7,I1)')
               ELSE
                  WRITE(50,116) PRTTEXT(1),IPAGE
  116             FORMAT(37X,'(A7,I2)')
               END IF
            END IF
            WRITE(50,118)PRTTEXT(2)
  118       FORMAT(1H1,31X,A22,/,1X,80('ฤ'),/)            
            ILINE = 4
            IPAGE = IPAGE + 1
         ELSE 
            WRITE(50,120)
  120       FORMAT(/,1X,80('อ'),/)
            ILINE = ILINE + 3
         END IF
      END IF
C
C     DRAW THE FORM AS IT APPEARS ON THE SCREEN
C
      WRITE(50,130) PRTTEXT(3),FORMNAME
  130 FORMAT(2X,A6,1X,A8,/,2X,'อออออ  ออออออออ',/)
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
      WRITE(50,210) PRTTEXT(4)
  210 FORMAT(/,2X,A17,/,2X,17('อ'),/)
      WRITE(50,220)PRTTEXT(5),PRTTEXT(6)
  220 FORMAT(1X,A76,/,1X,A76)
      DO 400 IFLD = 1,NUMFLD
         ICASE = ' '
         ILEN = 1
         IF (FLDTYPE(IFLD).EQ.PRTCHR(3)) THEN
            TYPDEF = PRTTEXT(7)
            IF (LOWCASE(IFLD)) THEN
               ICASE = PRTCHR(1)
            ELSE
               ICASE = PRTCHR(2)
            END IF
         ELSE IF (FLDTYPE(IFLD).EQ.PRTCHR(4).OR.FLDTYPE(IFLD).EQ.
     +           PRTCHR(2)) THEN
            TYPDEF = PRTTEXT(8)
         ELSE IF (FLDTYPE(IFLD).EQ.PRTCHR(5)) THEN
            TYPDEF = PRTTEXT(9)
         ELSE IF (FLDTYPE(IFLD).EQ.PRTCHR(6)) THEN
            TYPDEF = PRTTEXT(10)
         ELSE
            TYPDEF = '  '
         END IF
         IF (RANGE(IFLD)) THEN
            IRANGE = PRTCHR(1)
            ILEN = ILEN + 2
         ELSE
            IRANGE = PRTCHR(2)
         END IF
         IF (NOENTRY(IFLD)) THEN
            IENTRY = PRTCHR(1)
         ELSE
            IENTRY = PRTCHR(2)
         END IF
         IF (LOOKUP(IFLD)) THEN
            ILOOK = PRTCHR(1)
            ILEN = ILEN + 2
         ELSE
            ILOOK = PRTCHR(2)
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
            WRITE(50,250) PRTTEXT(11),LOWLIM(IFLD),PRTTEXT(12),
     +                    HIGHLIM(IFLD)
  250       FORMAT(12X,A12,A72,/,12x,A12,A72)
            ILINE = ILINE + 2
         END IF
         IF (DEFAULT(IFLD).NE.'     ') THEN
            WRITE(50,260) PRTTEXT(13),DEFAULT(IFLD)
  260       FORMAT(12X,A12,A72)
            ILINE = ILINE + 1
         END IF
         IF (LOOKUP(IFLD)) THEN
            WRITE(50,270) PRTTEXT(14),LKFILE(IFLD),PRTTEXT(15),
     +                    LKFLEN(IFLD),PRTTEXT(16),LKFPOS(IFLD),
     +                    PRTTEXT(17),LKFRECFLD(IFLD)
  270       FORMAT(12X,A13,A20,5X,A15,I3,/,12X,A15,I3,15X,A23,I2)
            ILINE = ILINE + 2
         END IF
  400 CONTINUE
C
      RETURN
C
C     *** ERROR READING FILE ***
C  
  800 CONTINUE
      CALL CLS
      CALL WRTMSG(5,142,12,1,1,PRTFILE,22)
      RTNERR = '2'
      RETURN
      END
