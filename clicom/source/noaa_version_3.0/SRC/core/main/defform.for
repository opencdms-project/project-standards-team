$STORAGE:2

      PROGRAM DEFFORM
C
C   ROUTINE TO DEFINE, MODIFY, OR DELETE FORTRAN PROGRAM FORMS
C   THAT ARE ACCESSED THRU THE GETFRM SUBROUTINE.
C
      CHARACTER*78 MSGLIN
      CHARACTER*64 FILNM1,FILNM2,MESSAG(3)
      CHARACTER*41 DESCREC
      CHARACTER*32 FORMDESC
      CHARACTER*20 FORMFILE
      CHARACTER*8 FORMNAME
      CHARACTER*2 INCHAR,RTNFLAG,FRMDEF(3)
      CHARACTER*1 REPLY,RTNCODE
      INTEGER*2 FRMWID,FRMHT,FGCOLOR,BGCOLOR,STRTCOL,ENDCOL,STRTROW
     +         ,ENDROW ,HLDCHR,HLDFGC,HLDBGC,BDRFG,BDRBG,BOXTYP
     +         ,NCHAR(80,23),FFGCLR(80,23),FBGCLR(80,23)
     +         ,MSGLEN(3),FLDFGC,FLDBGC
      LOGICAL BORDER,INFLD,NEWFRM,DUMMY,INBOX,CPYFLG
C
$INCLUDE: 'FRMFLD.INC'
C
C   INITIALIZE HELP FILE NAMES AND MESSAGE TEXT
C
      INBOX = .FALSE.
      FILNM1 = 'P:\HELP\DEFFORM1.HLP'
      FILNM2 = 'P:\HELP\DEFFORM2.HLP'
      CALL GETMSG(374,MSGLIN)
      CALL PARSE1(MSGLIN,78,3,64,MESSAG,RTNCODE)
      CALL GETMSG(373,MSGLIN)
      CALL GETMSG(999,MSGLIN)
      DO 60 IMSG = 1,3
         DO 50 I = 64,1,-1
            IF (MESSAG(IMSG)(I:I).NE.' ') THEN
               MSGLEN(IMSG) = I
               GO TO 60
            END IF
50       CONTINUE
60    CONTINUE
C
110   CONTINUE
      CALL ACTPAG(0,IERR)
      FGCOLOR = 14
      BGCOLOR = 0
      FLDFGC = 15
      FLDBGC = 1
      BDRFG = 11
      BDRBG = 0
      BOXTYP = 2
      NUMFLD = 0
      STRTROW = 0
      STRTCOL = 0
C
C   ASK FOR THE FORM NAME OR SELECT FROM LIST OF EXISTING FORMS
C
120   CONTINUE
      CALL CLS
      ICOL = 40 - MSGLEN(1)/2
      CALL LOCATE(2,ICOL,IERR)
      CALL WRTSTR(MESSAG(1),MSGLEN(1),11,0)
      CALL WRTFNC(16)
      FORMNAME = '        '
      FORMDESC = ' '
      CALL LOCATE(5,0,IERR)
      CALL WRTSTR(MSGLIN,78,14,0)
125   CONTINUE
      ICNTRL = 1
      CPYFLG = .FALSE.
      CALL LOCATE(6,0,IERR)
      CALL WRTSTR(MESSAG(2),MSGLEN(2),14,0)
      CALL WRTSTR(': ',2,14,0)
      CALL GETSTR(0,FORMNAME,8,15,1,RTNFLAG)
C
C    F1 - HELP
C
      IF (RTNFLAG.EQ.'1F') THEN
         CALL DSPWIN(FILNM1)
         GO TO 125
C
C    SHIFT-F1  - BRING UP WINDOW OF POSSIBLE VALUES.  IF THE USER
C    PRESS F7 IN THAT ROUTINE HE WANTS THAT RECORD DELETED.  ROUTINE
C    RETURNS IFLAG = 1
C
      ELSE IF (RTNFLAG.EQ.'1S') THEN
         OPEN (51,FILE='P:\FORM\FTNFORM.IDX',STATUS='OLD'
     +       ,FORM='BINARY',ACCESS='DIRECT',RECL=43)
         DESCREC = ' '
         CALL VALWIN(51,DESCREC,41,3,IFLAG)
         CLOSE(51)
         FORMNAME = DESCREC(1:8)
         FORMDESC = DESCREC(10:41)
         IF (FORMNAME.EQ.' ') THEN
            GO TO 125
         ELSE IF (IFLAG.EQ.1) THEN
            CALL DELFORM(FORMNAME)
            GO TO 120
         ELSE
            ICNTRL = 2
            CALL LOCATE(6,0,IERR)
            CALL WRTSTR(MESSAG(2),MSGLEN(2),14,0)
            CALL WRTSTR(': ',2,14,0)
            CALL WRTSTR(FORMNAME,8,15,1)
         END IF
      ELSE IF (RTNFLAG.EQ.'4F') THEN
         STOP ' '
      ELSE IF (FORMNAME.EQ.' ') THEN
         GO TO 125
      ELSE IF (RTNFLAG.EQ.'2S') THEN
C      
C  SHIFT F2 -- COPY FORM DEFINITION FROM AN EXTERNAL FILE    
C
         CPYFLG = .TRUE.   
      ELSE IF (RTNFLAG.NE.'2F') THEN   
         GO TO 125
      END IF
C
C   BUILD FORM FILE NAME AND CHECK IF IT EXISTS.  IF IT WAS SELECTED
C   FROM THE FTNFORM INDEX FILE AND IT DOES NOT EXIST, DELETE IT FROM
C   THE INDEX FILE.  OTHERWISE READ IN THE DEFINITION.
C
      FORMFILE = 'P:\FORM\AAAAAAAA.FRM'
      FORMFILE(9:16) = FORMNAME
      DO 130 I = 9,16
         IF (FORMFILE(I:I).EQ.' ') THEN
            FORMFILE(I:I) = 'A'
         END IF
130   CONTINUE
      OPEN(15,FILE=FORMFILE,STATUS='OLD',FORM='UNFORMATTED'
     +    ,IOSTAT=IOCHK)
      IF (IOCHK.EQ.0) THEN
         NEWFRM = .FALSE.
         READ(15) FRMHT,FRMWID,BORDER
         READ(15) I1,I2,I3
$INCLUDE: 'RDFLDS.INC'
         CLOSE(15)
      ELSE
         NEWFRM = .TRUE.
         CLOSE(15)
         IF (ICNTRL.EQ.2) THEN
            CALL WRTMSG(4,182,12,0,0,' ',0)
            CALL WRTMSG(3,183,12,1,1,' ',0)
            CALL DELFORM(FORMNAME)
            GO TO 120
         END IF
      END IF
C
C       ** COPY A FORM DEFINITION FROM ANOTHER FILE
C
      IF (CPYFLG) THEN
         CALL CPYFRM(FORMNAME,NEWFRM)
         GO TO 120
      ENDIF             
C
C  FORM NOT FOUND -  NEW FORM TO BE DEFINED. INITIALIZE
C
      IF (NEWFRM) THEN
         FRMDEF(1) = '  '
         FRMDEF(2) = '  '
         FRMDEF(3) = '  '
         IWID = 0
         IHT = 0
C
C  FORM FOUND - SET HEIGHT AND WIDTH FROM DEFINITION FILE
C 
      ELSE
         WRITE(FRMDEF(1),'(I2)') FRMWID
         WRITE(FRMDEF(2),'(I2)') FRMHT
         IWID = FRMWID
         IHT = FRMHT
         IF(BORDER) THEN
           FRMDEF(3) = 'Y'
         ELSE
           FRMDEF(3) = 'N'
         END IF
      END IF
C
C   ALLOW USER TO SET OR MODIFY FORM HEIGHT AND WIDTH
C      
140   CONTINUE
      CALL LOCATE(10,1,IERR)
      CALL GETFRM('DEFFORM1','  ',FRMDEF,2,RTNFLAG)
      IF (RTNFLAG.EQ.'4F') THEN
         GO TO 120
      END IF
      READ(FRMDEF(1),'(BN,I2)') FRMWID
      READ(FRMDEF(2),'(BN,I2)') FRMHT
      IF (FRMDEF(3).EQ.'Y') THEN
         BORDER = .TRUE.
         JHT = FRMHT - 2
         JWID = FRMWID - 2
      ELSE IF (FRMDEF(3).EQ.'N') THEN
         BORDER = .FALSE.
         JHT = FRMHT - 1
         JWID = FRMWID - 1
      ELSE
         CALL WRTMSG(2,225,12,1,0,' ',0)
         GO TO 140
      END IF
      IF (BORDER.AND.FRMHT.LT.3) THEN
          FRMHT = 3
          JHT = 1
      END IF
C
C  CHECK THAT THE NEW FORM SIZE WILL HOLD ALL FIELDS ALREADY DEFINED
C 
      IF (.NOT.NEWFRM) THEN
         DO 180 I1 = 1,NUMFLD
            IF (FLDROW(I1).GT.JHT) THEN
               FRMWID = IWID
               FRMHT = IHT
               CALL WRTMSG(2,117,12,1,0,' ',0)
               GO TO 140
            END IF
            IF (FLDCOL(I1)+FLENGTH(I1)-1.GT.JWID) THEN
               CALL WRTMSG(2,118,12,1,0,' ',0)
               FRMWID = IWID
               FRMHT = IHT
               GO TO 140
            END IF
180      CONTINUE
      END IF
C
C   DRAW THE FORM FOR DEFINITION OR MODIFICATION
C
      CALL CLS
      CALL LOCATE(0,0,IERR)
      IF (.NOT.NEWFRM) THEN
         I2ROW = 0
         I2COL = 0
         CALL DSPFRM(FORMNAME,IHT,IWID,DUMMY,I2ROW,I2COL
     +       ,RTNCODE)
$INCLUDE: 'RDFLDS.INC'
         CLOSE(15)
      END IF
C
C  DRAW THE BORDER AROUND THE FORM AND SET FORM CORNER COORDINATES
C
      ENDCOL = FRMWID
      ENDROW = FRMHT
      IF (BORDER) THEN
         STRTROW = STRTROW + 1
         ENDROW = ENDROW - 2
         STRTCOL = STRTCOL + 1
         ENDCOL = ENDCOL - 2
         CALL DRWBOX(0,0,FRMWID-1,FRMHT-1,BOXTYP,BDRFG,BDRBG)
      ELSE
         ENDROW = ENDROW - 1
         ENDCOL = ENDCOL - 1
      END IF
      IROW = STRTROW
      ICOL = STRTCOL
      CALL WRTFNC(3)
C
C  ---------- READ THE FORM DEFINITION FROM THE USER ----------
C
  200 CONTINUE
C
C   FIRST POSITION THE CURSOR AND CHECK IF CURRENTLY WITHIN A 
C   DATA ENTRY FIELD
C
      CALL LOCATE(IROW,ICOL,IERR)
      INFLD = .FALSE.
      DO 220 I1 = 1,NUMFLD
         IF (FLENGTH(I1).GT.0) THEN
            IF (IROW.EQ.FLDROW(I1).AND.ICOL.GE.FLDCOL(I1).AND.
     +            ICOL.LE.FLDCOL(I1)+FLENGTH(I1)-1) THEN
               INFLD = .TRUE.  
               GO TO 230
            END IF
         END IF
  220 CONTINUE
  230 CONTINUE
C
C   READ THE USER INPUT CHARACTER 
C
      CALL GETCHAR(1,INCHAR)
C
C   A NORMAL CHARACTER HAS BEEN ENTERED
C
      IF (INCHAR(2:2).EQ.' ') THEN
         IF (.NOT.INFLD) THEN 
            JCHAR = ICHAR(INCHAR(1:1))
            CALL CHRWRT(JCHAR,BGCOLOR,FGCOLOR,1)
         END IF
         ICOL = ICOL + 1
C
C   OR A CONTROL CHARACTER HAS BEEN ENTERED
C
      ELSE
         IF (INCHAR.EQ.'LA'.OR.INCHAR.EQ.'BS') THEN
            ICOL = ICOL - 1
         ELSE IF (INCHAR.EQ.'RA') THEN
            ICOL = ICOL + 1
         ELSE IF (INCHAR.EQ.'UA') THEN
            IROW = IROW - 1
         ELSE IF (INCHAR.EQ.'DA') THEN
            IROW = IROW + 1
         ELSE IF (INCHAR.EQ.'RE') THEN
            ICOL = ENDCOL + 1
         ELSE IF (INCHAR.EQ.'HO') THEN
            ICOL = STRTCOL
            IROW = STRTROW
         ELSE IF (INCHAR.EQ.'EN') THEN
            ICOL = ENDCOL
            IROW = ENDROW
C
C     F1 - HELP
C
          ELSE IF (INCHAR.EQ.'1F') THEN
             CALL DSPWIN(FILNM2)
C
C     F2 - SAVE THE CURRENT FORM
C
         ELSE IF (INCHAR.EQ.'2F') THEN
            IF (.NOT.NEWFRM) THEN
               CALL CHGNAM(FORMNAME,MESSAG(3),MSGLEN(3),RTNFLAG)
               IF (RTNFLAG.EQ.'4F') THEN
                  GO TO 200
               END IF
            END IF
            CALL SAVFRM(FORMNAME,FORMDESC,FRMHT,FRMWID,BORDER)
            GO TO 110
C
C     F3 - SET NEW COLORS
C
         ELSE IF (INCHAR.EQ.'3F') THEN
            INFG = FGCOLOR
            INBG = BGCOLOR
            CALL SETCLR(INFG,INBG)
            IF (INFG.GE.0) THEN
               FGCOLOR = INFG
               BGCOLOR = INBG
            END IF
C
C     Shift-F3 - CHANGE BORDER COLORS/CHARACTERS
C
         ELSE IF (INCHAR.EQ.'3S') THEN
            IF (BORDER) THEN
               CALL ACTPAG(1,IERR)
               CALL CLS
  240          CONTINUE
               CALL LOCATE(1,45,IERR)
               CALL WRTMSG(24,226,14,0,0,' ',0)
               INCHAR = 'D'
               CALL GETSTR(0,INCHAR,1,15,1,RTNFLAG)
               IF (INCHAR.EQ.'S ') THEN
                  BOXTYP = 1
               ELSE IF (INCHAR.EQ.'D ') THEN
                  BOXTYP = 2
               ELSE IF (INCHAR.EQ.'B ') THEN
                  BOXTYP = 3
               ELSE
                  CALL LOCATE(2,1,IERR)
                  CALL WRTMSG(22,227,12,1,0,' ',0)
                  GO TO 240
               END IF
               IF (RTNFLAG.EQ.'4F') THEN
                  CALL ACTPAG(0,IERR)
               ELSE
                  INFG = BDRFG
                  INBG = BDRBG
                  CALL SETCLR(INFG,INBG)
                  IF (INFG.GE.0) THEN
                     BDRFG = INFG
                     BDRBG = INBG
                     CALL DRWBOX(0,0,FRMWID-1,FRMHT-1,BOXTYP,BDRFG
     +                   ,BDRBG)
                  END IF
               END IF
            END IF
C
C     F4 - ABANDON THE CURRENT FORM
C
         ELSE IF (INCHAR.EQ.'4F') THEN
            CALL ACTPAG(1,IERR)
            CALL CLS
            CALL LOCATE(23,28,IERR)
            CALL WRTMSG(2,228,12,1,0,' ',0)
            CALL OKREPLY(REPLY,RTNCODE)
            CALL ACTPAG(0,IERR)
            IF (REPLY.EQ.'Y'.AND.RTNCODE.EQ.'0') THEN
               GO TO 110
            END IF
C
C     F5 - DRAW A BOX ON THE FORM
C
         ELSE IF (INCHAR.EQ.'5F') THEN
            CALL CBOX(INBOX,FGCOLOR,BGCOLOR)
C
C      F7 - DELETE THE CURRENT LINE AND ANY FIELDS IN IT
C        
         ELSE IF (INCHAR.EQ.'7F') THEN
            CALL SCROLL(1,1,IROW,STRTCOL,ENDROW,ENDCOL)
            DO 250 I1 = 1,NUMFLD
               IF (FLENGTH(I1).GT.0) THEN
                  IF (FLDROW(I1).EQ.IROW) THEN
                     FLENGTH(I1) = 0
                  ELSE IF (FLDROW(I1).GT.IROW) THEN
                     FLDROW(I1) = FLDROW(I1) - 1
                  END IF
               END IF
  250       CONTINUE
C
C      F8 - INSERT A NEW LINE
C        
         ELSE IF (INCHAR.EQ.'8F') THEN
            DO 300 I1 = 1,NUMFLD
               IF (FLENGTH(I1).GT.0.AND.FLDROW(I1).EQ.ENDROW) THEN
                  CALL BEEP
                  GO TO 200
               END IF
  300       CONTINUE
            CALL SCROLL(0,1,IROW,STRTCOL,ENDROW,ENDCOL)
            DO 340 I1 = 1,NUMFLD
               IF (FLENGTH(I1).GT.0) THEN
                  IF (FLDROW(I1).GE.IROW) THEN
                     FLDROW(I1) = FLDROW(I1) + 1
                  END IF
               END IF
  340       CONTINUE
C
C      F9 - PRINT THE DEFINITION OF THIS FORM
C
         ELSE IF (INCHAR.EQ.'9F') THEN
            ILINE = 65
            CALL LOCATE(24,0,IERR)
            CALL WRTSTR(' Printing... ',13,140,0)
            DO 400 J = 0,FRMHT-1
               DO 400 I = 0,FRMWID-1
                  CALL LOCATE(J,I,IERR)
                  CALL QRTEXT(NCHAR(I+1,J+1),FBGCLR(I+1,J+1),
     +                  FFGCLR(I+1,J+1))
  400       CONTINUE
            OPEN (50,FILE='PRN',STATUS='UNKNOWN',FORM='FORMATTED')
            CALL PRTFRM(FORMNAME,FRMHT,FRMWID,NCHAR,ILINE,-10)
            CLOSE (50)
            CALL WRTFNC(3)
C
C  F10 - DEFINE OR MODIFY A FIELD
C
         ELSE IF (INCHAR.EQ.'0F') THEN
            CALL DEFFLD(ENDCOL,FLDFGC,FLDBGC)
C
C  SHIFT F10 - SET NEW COLORS FOR FIELD
C
         ELSE IF (INCHAR.EQ.'0S') THEN
            INFG = FLDFGC
            INBG = FLDBGC
            CALL SETCLR(INFG,INBG)
            IF (INFG.GE.0) THEN
               FLDFGC = INFG
               FLDBGC = INBG
            END IF
C
C   DELETE A CHARACTER IN THE CURRENT LINE
C
         ELSE IF (INCHAR.EQ.'DE') THEN
            IF (INFLD) THEN
               CALL BEEP
            ELSE IF (ICOL.LT.ENDCOL) THEN
               DO 420 I1 = ICOL+1, ENDCOL
                  CALL LOCATE(IROW,I1,IERR)
                  CALL QRTEXT(HLDCHR,HLDBGC,HLDFGC)
                  CALL LOCATE(IROW,I1-1,IERR)
                  CALL CHRWRT(HLDCHR,HLDBGC,HLDFGC,1)
  420          CONTINUE
               CALL LOCATE(IROW,ENDCOL,IERR)
               CALL WRTSTR(' ',1,14,0)
               DO 440 I1 = 1,NUMFLD
                  IF (FLENGTH(I1).GT.0) THEN
                     IF (FLDROW(I1).EQ.IROW.AND.FLDCOL(I1).GT.ICOL) THEN
                        FLDCOL(I1) = FLDCOL(I1) - 1
                     END IF
                  END IF
  440          CONTINUE
            ELSE
               CALL WRTSTR(' ',1,14,0)
            END IF
C
C   INSERT A CHARACTER IN THE CURRENT LINE
C
         ELSE IF (INCHAR.EQ.'IN') THEN
            IF (INFLD) THEN
               CALL BEEP
            ELSE IF (ICOL.LT.ENDCOL) THEN
               DO 460 I1 = 1,NUMFLD
                  IF (FLENGTH(I1).GT.0.AND.FLDROW(I1).EQ.IROW.AND.
     +                   FLDCOL(I1)+FLENGTH(I1)-1.EQ.ENDCOL) THEN
                     CALL BEEP
                     GO TO 200
                  END IF
  460          CONTINUE
               DO 480 I1 = ENDCOL-1,ICOL,-1
                  CALL LOCATE(IROW,I1,IERR)
                  CALL QRTEXT(HLDCHR,HLDBGC,HLDFGC)
                  CALL LOCATE(IROW,I1+1,IERR)
                  CALL CHRWRT(HLDCHR,HLDBGC,HLDFGC,1)
  480          CONTINUE
               CALL LOCATE(IROW,ICOL,IERR)
               CALL WRTSTR(' ',1,14,0)
               DO 500 I1 = 1,NUMFLD
                  IF (FLENGTH(I1).GT.0) THEN
                     IF (FLDROW(I1).EQ.IROW.AND.FLDCOL(I1).GT.ICOL) THEN
                        FLDCOL(I1) = FLDCOL(I1) + 1
                     END IF
                  END IF
  500          CONTINUE
            ELSE
               CALL WRTSTR(' ',1,14,0)
            END IF
C
C   OTHERWISE AN INVALID CHARACTER HAS BEEN ENTERED
C  
         ELSE
            CALL BEEP
         END IF
      END IF
C
C   CHECK THE CURSOR POSITION AND WRAP IF REQUIRED
C
      IF (ICOL.LT.STRTCOL) THEN
         ICOL = ENDCOL
         IROW = IROW - 1
      ELSE IF (ICOL.GT.ENDCOL) THEN
         ICOL = STRTCOL
         IROW = IROW + 1
      END IF
      IF (IROW.LT.STRTROW) THEN
          IROW = ENDROW
      ELSE IF (IROW.GT.ENDROW) THEN
          IROW = STRTROW
      END IF
C
      GO TO 200
      END
$PAGE
***********************************************************************
      SUBROUTINE SAVFRM(FORMNAME,FORMDESC,FRMHT,FRMWID,BORDER)
C
C   ROUTINE TO SAVE THE CURRENT DATA ENTRY FORM TO DISK
C
      CHARACTER*80 MSGLIN
      CHARACTER*43 INREC
      CHARACTER*32 FORMDESC
      CHARACTER*20 FORMFILE
      CHARACTER*8 FORMNAME
      CHARACTER*2 RTNFLAG
      CHARACTER*1 CHRRTN,LNFEED
      INTEGER*2 FRMWID,FRMHT,NCHAR(80,23),FGCOLOR(80,23),BGCOLOR(80,23)
      LOGICAL BORDER
      CHRRTN = CHAR(13)
      LNFEED = CHAR(10)
C
C   ASK THE USER FOR THE DESCRIPTION OF THIS FORM AND ENTER IT INTO
C   THE FORTRAN FORM DEFINITION INDEX FILE.
C
      CALL GETMSG(587,MSGLIN)
      CALL GETMSG(999,MSGLIN)
      MSGLEN = LNG(MSGLIN)
      CALL LOCATE(21,1,IERR)
      CALL WRTSTR(MSGLIN,MSGLEN,14,0)
      CALL GETSTR(1,FORMDESC,32,15,1,RTNFLAG)
C
C   DETERMINE AND SET THE NAME OF THE FORM DEFINITION FILE
C
      FORMFILE = 'P:\FORM\AAAAAAAA.FRM'
      FORMFILE(9:16) = FORMNAME
      DO 50 I = 9,16
         IF (FORMFILE(I:I).EQ.' ') THEN
            FORMFILE(I:I) = 'A'
         END IF
50    CONTINUE
C
C   SAVE THE FORM ON THE SCREEN TO DISK
C
      DO 100 J = 0,FRMHT-1
         DO 100 I = 0,FRMWID-1
            CALL LOCATE(J,I,IERR)
            CALL QRTEXT(NCHAR(I+1,J+1),BGCOLOR(I+1,J+1),
     +                  FGCOLOR(I+1,J+1))
  100 CONTINUE
      OPEN (15,FILE=FORMFILE,STATUS='UNKNOWN',FORM='UNFORMATTED'
     +        ,MODE='WRITE')
      WRITE(15) FRMHT,FRMWID,BORDER
      WRITE(15) ((NCHAR(I,J),BGCOLOR(I,J),FGCOLOR(I,J),J=1,FRMHT),
     +            I=1,FRMWID)
C
C   SAVE THE FIELD DEFINITIONS FOR THE FORM
C
      CALL SAVFLD
      CLOSE(15)
C
C   SAVE OR UPDATE THIS ENTRY IN THE FTNFORM INDEX FILE
C
      OPEN (51,FILE='P:\FORM\FTNFORM.IDX',STATUS='UNKNOWN'
     +     ,FORM='BINARY',ACCESS='DIRECT',RECL=43)
      IDEL = 0
      DO 150 I = 1,9999
         READ(51,REC=I,ERR=160) INREC
         IF (INREC(1:8).EQ.FORMNAME) THEN
            INREC(10:41) = FORMDESC
            IDEL = 0
            GO TO 200
         ELSE IF (INREC(1:8).EQ.'********') THEN
            IDEL = I
         END IF
150   CONTINUE
160   CONTINUE
      WRITE(INREC,'(A8,1X,A32,2A1)') FORMNAME, FORMDESC,CHRRTN,LNFEED
C
200   CONTINUE
      IF (IDEL.GT.0) THEN
         WRITE(51,REC=IDEL) INREC
      ELSE
         WRITE(51,REC=I) INREC
      END IF
      CLOSE (51)
C
      RETURN
      END
***********************************************************************
       SUBROUTINE CHGNAM(FORMNAME,MESSAG,MSGLEN,RTNFLAG)
C
C   ROUTINE TO ASK IF A MODIFIED FORM SHOULD BE WRITTEN UNDER A NEW
C       NAME 
C
      CHARACTER*64 MESSAG
      CHARACTER*8 FORMNAME,NEWNAME
      CHARACTER*2 INCHAR,RTNFLAG,NOUP,NOLO,YESUP,YESLO
      CHARACTER*20 FORMFILE
      CHARACTER*8 BLANK
      DATA BLANK /'        '/
C
      CALL GETYN(1,2,YESUP,YESLO)
      CALL GETYN(2,2,NOUP,NOLO)
  40  CONTINUE
      FORMFILE = 'P:\FORM\AAAAAAAA.FRM'
      CALL WRTMSG(3,230,14,0,0,' ',0)
      CALL LOCATE(22,45,IERR)
      CALL GETCHAR(0,INCHAR)
      IF (INCHAR.EQ.YESUP .OR. INCHAR.EQ.YESLO)  THEN
         NEWNAME = BLANK
         CALL WRTSTR('  ',2,14,0)
         CALL WRTSTR(MESSAG,MSGLEN,14,0)
         CALL WRTSTR(': ',2,14,0)
         CALL GETSTR(0,NEWNAME,8,15,1,RTNFLAG)
         IF (NEWNAME.EQ.BLANK.OR.RTNFLAG.EQ.'4F') THEN
            GO TO 40
         ELSE
            DO 60 I = 1,8
               IF (NEWNAME(I:I).EQ.BLANK) THEN
                  GO TO 80
               END IF
               INAME = I
   60       CONTINUE
   80       CONTINUE
            FORMFILE (9:9+INAME-1) = NEWNAME(1:INAME) 
         END IF
      ELSE IF (INCHAR.EQ.NOUP .OR. INCHAR.EQ.NOLO) THEN
         RTNFLAG = '  '
         RETURN
      ELSE IF (INCHAR.EQ.'4F') THEN
         RTNFLAG = '4F'
         CALL CLRMSG(3)
         RETURN
      ELSE
         CALL BEEP
         GO TO 40
      END IF
C
C   MAKE SURE THE NEW FORM NAME DOES NOT ALREADY EXIST
C
      OPEN (15,FILE=FORMFILE,STATUS='OLD',IOSTAT=IOCHK
     +        ,SHARE='DENYWR',MODE='READ')
      IF (IOCHK.EQ.0) THEN
          CALL WRTMSG(1,229,12,1,0,' ',0)
          CLOSE(15)
          GO TO 40
      ELSE
         FORMNAME = NEWNAME
      END IF
      RTNFLAG = '  '
      RETURN
      END

***********************************************************************
      SUBROUTINE DELFORM (FORMNAME)
C
C   ROUTINE TO DELETE A FORTRAN FORM DEFINITION FILE AND REMOVE ITS ENTRY 
C   FROM THE FTNFORM INDEX FILE.
C
      CHARACTER*43 INREC
      CHARACTER*64 FORMFILE
      CHARACTER*8 FORMNAME
C
C   MAKE SURE THAT THIS FORM IS NOT PROTECTED FROM DELETION
C
      IF (FORMNAME.EQ.'DEFMENUS'.OR.FORMNAME.EQ.'DEFFORM1') THEN
         CALL WRTMSG(5,116,12,1,1,' ',0)
         RETURN
      END IF
C      
C  BUILD FORM FILE NAME,  OPEN IT, AND DELETE IT
C
      FORMFILE = 'P:\FORM\AAAAAAAA.FRM'
      FORMFILE(9:16) = FORMNAME
      DO 30 I = 9,16
         IF (FORMFILE(I:I).EQ.' ') THEN
            FORMFILE(I:I) = 'A'
         END IF
   30 CONTINUE
      OPEN (51,FILE=FORMFILE,STATUS='OLD',FORM='UNFORMATTED'
     +     ,MODE='WRITE',IOSTAT=ICHK)
      IF (ICHK.NE.6416.AND.ICHK.NE.0) THEN
         CALL OPENMSG(FORMFILE,'DELFORM     ',ICHK)
      END IF
      CLOSE(51,STATUS='DELETE')
C
C   FIND AND REMOVE THE ENTRY FROM THE FTNFORM INDEX FILE
C      
      OPEN (51,FILE='P:\FORM\FTNFORM.IDX',STATUS='OLD',FORM='BINARY'
     +     ,ACCESS='DIRECT',RECL=43)
      DO 100 I = 1,9999
         READ(51,REC=I,ERR=110) INREC
         IF (INREC(1:8).EQ.FORMNAME) THEN
            INREC(1:8) = '********'              
            INREC(10:41) = 'THIS RECORD DELETED'
            WRITE(51,REC=I) INREC
            GO TO 110
         END IF
100   CONTINUE
110   CONTINUE
      CLOSE(51)
      FORMNAME = ' '
      RETURN
      END
      SUBROUTINE CPYFRM(FORMNAME,NEWFRM)
C
C       ** OBJECTIVE:  COPY A FORM FROM ANOTHER FILE AND MAKE AN ENTRY IN THE
C                      FORM INDEX TABLE
C     
C       ** INPUT:
C             FORMNAME.....NAME OF THE FORM WHOSE DEFINITION WILL BE COPIED
C             NEWFRM.......FLAG TO INDICATE WHETHER THIS FORM IS A NEW ENTRY
C                          T=NEW ENTRY   F=EXISTING FORM
C
      CHARACTER*8 FORMNAME
      LOGICAL NEWFRM
C      
      CHARACTER*80 MSGLIN
      CHARACTER*43 INREC
      CHARACTER*42 SRCFILE
      CHARACTER*32 FORMDESC
      CHARACTER*20 FORMFILE
      CHARACTER*2 RTNFLAG
      CHARACTER*1 REPLY,RTNCODE,CHRRTN,LNFEED
      INTEGER*2 FRMWID,FRMHT,NCHAR(80,23),FGCOLOR(80,23),BGCOLOR(80,23)
      LOGICAL BORDER,CPYFLG
C
$INCLUDE: 'FRMFLD.INC'
C
C      
      CHRRTN = CHAR(13)
      LNFEED = CHAR(10)
      CPYFLG = .TRUE.
      IF (.NOT.NEWFRM) THEN
C
C          .. FORM NAME EXISTS -- ASK USER TO OVERWRITE OLD FORM
         CALL WRTMSG(5,412,14,1,0,' ',0)
         CALL LOCATE(21,5,IERR)
         CALL OKREPLY(REPLY,RTNCODE)
         CALL CLRMSG(5)
         CALL CLRMSG(4)
         IF (REPLY.EQ.'Y')THEN
C             .. OVERWRITE OLD FORM -- ASK USER TO RECONSIDER OVERWRITE
            CALL WRTMSG(5,402,14,0,0,' ',0)
            CALL WRTMSG(4,404,14,0,0,' ',0)
            CALL LOCATE(21,22,IERR)
            CALL OKREPLY(REPLY,RTNCODE)
            CALL CLRMSG(5)
            CALL CLRMSG(4)
            IF (REPLY.EQ.'N')THEN
C                .. DO NOT OVERWRITE OLD FORM
               CPYFLG = .FALSE.                
            ENDIF
         ELSE
C             .. DO NOT OVERWRITE OLD FORM
            CPYFLG = .FALSE.
         ENDIF      
      ENDIF   
      IF (CPYFLG) THEN
C
C          ** GET THE NAME OF THE SOURCE FILE
C      
         SRCFILE = ' '
   20    CALL LOCATE(16,30,IERR)
         CALL WRTMSG(5,357,14,0,0,' ',0)
         CALL LOCATE(21,5,IERR)
         CALL GETSTR(0,SRCFILE,42,15,1,RTNFLAG)
         CALL CLRMSG(5)
         CALL CLRMSG(4)
         IF (RTNFLAG .EQ. '4F' .OR. SRCFILE .EQ. ' ') THEN
            GO TO 100
         ENDIF
C
C          ** READ THE FORM DEFINITION FROM THE SOURCE FILE
C         
         OPEN(15,FILE=SRCFILE,STATUS='OLD',FORM='UNFORMATTED'
     +          ,IOSTAT=IOCHK)
         IF(IOCHK.NE.0) THEN
            CALL OPENMSG(SRCFILE,'DEFFORM     ',IOCHK)
            GO TO 20
         END IF
         READ(15,ERR=900) FRMHT,FRMWID,BORDER
         READ(15,ERR=900) ((NCHAR(I,J),BGCOLOR(I,J),FGCOLOR(I,J),
     +                      J=1,FRMHT),I=1,FRMWID)
$INCLUDE: 'RDFLDS.INC'
         CLOSE(15)
C
C       ** CONSTRUCT THE FORM DEFINITION FILE FROM THE FORM NAME          
C
         FORMFILE = 'P:\FORM\AAAAAAAA.FRM'
         FORMFILE(9:16) = FORMNAME
         DO 30 I = 9,16
            IF (FORMFILE(I:I).EQ.' ') THEN
               FORMFILE(I:I) = 'A'
            END IF
   30    CONTINUE
C
C          ** OPEN THE FORM DEFINITION FILE AND WRITE THE DEFINITION
C             THAT WAS READ FROM THE SOURCE FILE
C
         OPEN (15,FILE=FORMFILE,STATUS='UNKNOWN',FORM='UNFORMATTED'
     +           ,MODE='WRITE')
         WRITE(15) FRMHT,FRMWID,BORDER
         WRITE(15) ((NCHAR(I,J),BGCOLOR(I,J),FGCOLOR(I,J),J=1,FRMHT),
     +               I=1,FRMWID)
         WRITE(15) NUMFLD,(FLDTYPE(I),FLDROW(I),FLDCOL(I),FLENGTH(I)
     +            ,LOWCASE(I),RANGE(I),LOWLIM(I),HIGHLIM(I),DEFAULT(I)
     +            ,LOOKUP(I),LKFILE(I),LKFLEN(I),LKFRECFLD(I)
     +            ,LKFPOS(I),NOENTRY(I),I=1,NUMFLD) 
         CLOSE(15)
C
C          ** SAVE OR UPDATE THIS ENTRY IN THE FTNFORM INDEX FILE
C
         FORMDESC = ' '
         OPEN (51,FILE='P:\FORM\FTNFORM.IDX',STATUS='UNKNOWN'
     +           ,FORM='BINARY',ACCESS='DIRECT',RECL=43)
         IDEL = 0
         DO 50 I = 1,9999
            READ(51,REC=I,ERR=60) INREC
            IF (INREC(1:8).EQ.FORMNAME) THEN
               FORMDESC = INREC(10:41) 
               IDEL = 0
               GO TO 60
            ELSE IF (INREC(1:8).EQ.'********') THEN
               IDEL = I
            END IF
   50    CONTINUE
   60    CONTINUE
C
C          ** ASK THE USER FOR THE DESCRIPTION OF THIS FORM AND ENTER IT 
C             INTO THE FORTRAN FORM DEFINITION INDEX FILE.
C
         CALL GETMSG(587,MSGLIN)
         CALL GETMSG(999,MSGLIN)
         MSGLEN = LNG(MSGLIN)
         CALL LOCATE(21,1,IERR)
         CALL WRTSTR(MSGLIN,MSGLEN,14,0)
         CALL GETSTR(1,FORMDESC,32,15,1,RTNFLAG)
         WRITE(INREC,'(A8,1X,A32,2A1)') FORMNAME, FORMDESC,
     +                                  CHRRTN,LNFEED
C
         IF (IDEL.GT.0) THEN
            WRITE(51,REC=IDEL) INREC
         ELSE
            WRITE(51,REC=I) INREC
         END IF
         CLOSE (51)
      ENDIF
  100 CONTINUE
      RETURN
C
C       ** ERROR PROCESSING
C
  900 CONTINUE
      LGTH = LNG(SRCFILE)
      CALL WRTMSG(4,191,12,1,1,SRCFILE,LGTH)
      RETURN
      END
      