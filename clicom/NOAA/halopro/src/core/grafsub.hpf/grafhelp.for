$STORAGE:2

      SUBROUTINE GRAFHELP(HELPLEVEL,XWIN,YWIN)
C*************************************************************************
C  ** DEBUG  CORRECTIONS TO DAVE'S ORIGINAL VERSION
C     1) 8-4-89  AFTER OPENING VIEWPORT TO ENTIRE SCREEN THE WORLD COORDINATES
C                ARE SET TO A RANGE OF 0-1. RATHER THAN THE OLD WORLD COORD
C                CALL TO MOVHCA(XOLD,YOLD) IS REMOVED.  THESE CHANGES ARE IN
C                CODE JUST BEFORE OPENING THE TEXT WINDOW AND PRIOR TO
C                LOOP 40.  
C*************************************************************************
C
C  THIS ROUTINE PRINTS A TEXT FILE IN A GRAPHICS SCREEN WINDOW WITH A
C  PAUSE AT THE END OF EACH PAGE.  IT RETURNS IF AN ESC KEY IS HIT,
C  SCROLLS UP IF A PgUp IS HIT AND GOES TO THE NEXT PAGE IF A PgDn IS HIT.
C  CHARACTERS WITHIN THE MESSAGE FILE DELIMITTED BY ^ CHARACTERS ARE
C  WRITTEN IN RED (THESE ^ MESSAGES CAN NOT SPAN LINES. EACH LINE MUST
C  HAVE ITS OWN SET OF ^'S)
C
C      INPUT: HELPLEVEL..GRAPHICS HELP FILE NUMBER
C             XWIN,YWIN..LOWER LEFT WINDOW CORNER IN NORMALIZED DEVICE COORDS
C
C  ---> MAXIMUM OF 40 CHARACTERS PER LINE AND 100 LINES PER FILE <---
C
$INCLUDE:  'DEVATRB.INC'
$INCLUDE:  'SYSFONT.INC'
      PARAMETER (MXTXTLN=9)
C **DEBUG
      CHARACTER*78 MESSAGE      
      CHARACTER*42 TESTLINE
      CHARACTER*40 TXTLINE(100)
      CHARACTER*20 FILNAME
      CHARACTER*3  DEVERS
      CHARACTER*2 INCHAR
      INTEGER*2 HELPLEVEL
      REAL HEIGHT,XOLD,YOLD
      DATA FILNAME /'P:\HELP\GRAF-01.HLP'/
      DATA TESTLINE /'&MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM&'/
      DATA ISPFAC /.6/,XTRA_X/.01/
C
      CALL GETDEASE(DEVERS)
      IF (DEVERS.EQ.'4.0') THEN
         NMSG=492
      ELSE
         NMSG=491
      ENDIF      
      CALL GETMSG(NMSG,MESSAGE)
      CALL GETMSG(999,MESSAGE)
      LGTH = MIN0(40,LNG(MESSAGE))
      TESTLINE(2:)= MESSAGE(1:LGTH)//'&'
C
C   DETERMINE AND SAVE THE CURRENT VIEWPORT, COORDINATES,CROSS-HAIR
C   CURSOR LOCATION AND XOR MODE
C
      CALL INQVIE(XN1,YN1,XN2,YN2)
      CALL INQWOR(X1OLD,Y1OLD,X2OLD,Y2OLD)
      CALL INQHCU(XOLD,YOLD,IDUMMY)
      CALL INQXOR(IXOR)
      CALL SETXOR(0)
C
C  DETERMINE HOW MUCH SPACE IS REQUIRED FOR THIS MENU IN THE CURRENT
C  WORLD COORD SYSTEM AND DEFINE WINDOW BIG ENOUGH TO HOLD IT.
C  ADD 2% OF SCREEN TO WINDOW SIZE TO ACCOUNT FOR WINDOW BORDER AND SHADOWS.
C
C       .. IN ORDER TO OPEN THE VIEWPORT TO THE ENTIRE SCREEN, A MAX VALUE
C          EQUAL TO .999 MUST BE USED.  A VALUE OF 1.0 IS A SPECIAL SIGNAL FOR
C          HALO TO 'TURN OFF' THE VIEWPORT WHICH DOES NOT RESET ASPECT RATIOS
      CALL INQASP(DASPBEF)
      CALL SETVIE(0.,0.,0.999,0.999,-1,-1)
      CALL INQASP(DASPBFW)
      CALL SETWOR(0.,0.,1.,1.)
      CALL INQASP(DASPINQ)
C **DEBUG      
      WRITE(999,*)'GRAFHELP DASPBEF/BFW/INQ=',DASPBEF,DASPBFW,DASPINQ
C         
      ICLR    = 0
      IDFONT  = ISYSFNT
      STDASP  = SYSASP
      TXTHTND = SYSHTND
      IASPFLG = 0
      CALL DFHSTWIN(IDFONT,ICLR,TXTHTND,IASPFLG,STDASP,ADJASP)
C
C   LOAD THE FILE TO BE PRINTED INTO TXTLINE (MAX 100 LINES)
C
      WRITE(INCHAR,'(I2.2)') HELPLEVEL
      FILNAME(14:15) = INCHAR
      OPEN(59,FILE=FILNAME,STATUS='OLD',FORM='FORMATTED'
     +      ,SHARE='DENYWR',MODE='READ')
C **DEBUG
      WRITE(999,*)'TESTLINE BEG=',TESTLINE
C **END DEBUG        
      CALL INQSTS(TESTLINE,HEIGHT,CWIDTH,OFFSET)
      WIDTH = CWIDTH
      IMAX = 0
      DO 70 I = 1,100
         READ(59,'(A40)',END=80) TXTLINE(I)
         NUMLINE = I
         LGTH = LNG(TXTLINE(I))
         TESTLINE(2:)=TXTLINE(I)(1:LGTH)//'&'
         CALL INQSTS(TESTLINE,HEIGHT,CWIDTH,OFFSET)
         IF (CWIDTH.GT.WIDTH) THEN
            WIDTH = CWIDTH
            IMAX  = I
         ENDIF   
   70 CONTINUE
   80 CONTINUE
      CLOSE(59)
C         
      IF (IMAX.EQ.0) THEN
         LGTH = MIN0(40,LNG(MESSAGE))
         TESTLINE(2:)= MESSAGE(1:LGTH)//'&'
      ELSE
         LGTH = LNG(TXTLINE(IMAX))
         TESTLINE(2:)=TXTLINE(IMAX)(1:LGTH)//'&'
      ENDIF   
C **DEBUG
      WRITE(999,*)'TESTLINE MAX=',TESTLINE
C **END DEBUG        
      CALL INQSTS(TESTLINE,HEIGHT,WIDTH,OFFSET)
      XWORLD = WIDTH 
      YWORLD = (HEIGHT+ISPFAC*OFFSET)*FLOAT(MXTXTLN+3)
      CALL MAPWTN(XWORLD,YWORLD,XWN,YWN)
      CALL INQVIE(XNDUL,YNDUL,XNDLR,YNDLR)
      XWN = XWN + .02 + XTRA_X
      YWN = (YNDLR-YWN) + .02
C
C   OPEN THE TEXT WINDOW 
C
      XWIN1 = XWIN
      XWIN2 = XWN + XWIN1
      YWIN1 = YWIN - YWN
      YWIN2 = YWIN
C **DEBUG
         WRITE(999,*)'##### GRAFHELP'
         WRITE(999,*)'+++++ MAX WIDTH,ADJASP=',WIDTH,ADJASP
         WRITE(999,*)'+++++ HT,OFFSET,TOTHT=',HEIGHT,OFFSET,
     +                      HEIGHT+ISPFAC*OFFSET
         WRITE(999,*)'+++++ XWIN1,XWIN2=',XWIN1,XWIN2
         WRITE(999,*)'+++++ YWIN1,YWIN2=',YWIN1,YWIN2
C ** END DEBUG
      CALL GRAFWIN(1,XWIN1,YWIN1,XWIN2,YWIN2)
C
C       .. THE ASPECT RATIO, ASP0, IS FOR A WORLD COORDINATE SYSTEM SET
C          TO (0.,0.,1.,1.), FIND THE ASPECT RATIO IN THE CURRENT SYSTEM.
      ICLR    = 1
      CALL DFWINASP(TESTLINE,IDFONT,ICLR,TXTHTND,
     +              XTRA_X,ADJASP,WINASP)
C
C   FIND TEXT HEIGHT IN WINDOW COORDS
C
      CALL INQSTS(TESTLINE,HEIGHT,WIDTH,OFFSET)
      HTLIN = HEIGHT+ISPFAC*OFFSET
C **DEBUG
         WRITE(999,*)'+++++ MAX WIDTH,ADJASP=',WIDTH,ADJASP
         WRITE(999,*)'+++++ HT,OFFSET,HTLIN=',HEIGHT,OFFSET,HTLIN
         CALL INQWOR(XLOW,YLOW,XHIGH,YHIGH)
         WRITE(999,*)'+++++ XLOW,XHIGH=',XLOW,XHIGH
         WRITE(999,*)'+++++ YLOW,YHIGH=',YLOW,YHIGH
         WRITE(999,*)'+++++ END GRAFHELP'
C ** END DEBUG
C
C   PRINT THE TEXT FILE ONE PAGE AT A TIME AND CHECK INPUT FROM USER
C
      IPAGE = 1
      CALL GWINWRT(IPAGE,NUMLINE,TXTLINE,HTLIN)
  100 CONTINUE
      CALL GETCHAR(0,INCHAR)
      IF (INCHAR.EQ.'DP'.OR.INCHAR.EQ.'DA') THEN
         CALL GWINWRT(IPAGE,NUMLINE,TXTLINE,HTLIN)
      ELSE IF (INCHAR.EQ.'UP'.OR.INCHAR.EQ.'UA') THEN
         IF (IPAGE.GT.2) THEN
            IPAGE = IPAGE - 2
            CALL GWINWRT(IPAGE,NUMLINE,TXTLINE,HTLIN)
         ELSE
            CALL BEEP
         END IF
      ELSE IF (INCHAR.EQ.'HO') THEN
         IPAGE = 1
         CALL GWINWRT(IPAGE,NUMLINE,TXTLINE,HTLIN)
      ELSE IF (INCHAR.EQ.'4F'.OR.INCHAR.EQ.'ES') THEN
         GO TO 150
      END IF
      GO TO 100  
C
C   RESTORE THE WINDOW AND EXIT.
C
150   CONTINUE
      CALL GRAFWIN(0,XWIN1,YWIN1,XWIN2,YWIN2)
      CALL SETVIE(XN1,YN1,XN2,YN2,-1,-1)
      CALL SETWOR(X1OLD,Y1OLD,X2OLD,Y2OLD)
      CALL MOVHCA(XOLD,YOLD)
      CALL SETXOR(IXOR)
      RETURN
      END
***********************************************************************

      SUBROUTINE GWINWRT(IPAGE,NUMLINE,TXTLINE,HEIGHT)
C
C   ROUTINE TO WRITE A WINDOW WORTH OF TXTLIN TO THE SCREEN AND MOVE 
C   PAGE POINTER TO THE NEXT PAGE.
C
      PARAMETER (MXTXTLN=9)
      CHARACTER*40 TXTLINE(100),MORTXT,ENDTXT
      CHARACTER*81 MESSAGE
      CHARACTER*3  DEVERS
      LOGICAL FRSTCL
      DATA FRSTCL /.TRUE./ ,MESSAGE,MORTXT,ENDTXT /3*' '/
C
C   ON FIRST CALL, READ ALL MESSAGE TEXT
C
      IF (FRSTCL) THEN
         FRSTCL = .FALSE.
         CALL GETDEASE(DEVERS)
         CALL GETMSG(310,MESSAGE)
         DO 50 J = 1,80
            IF (MESSAGE(J:J).EQ.',') THEN
               ISPLIT = J
               GO TO 55
            END IF
50       CONTINUE
55       CONTINUE
         MORTXT = '^ '
         MORTXT(2:ISPLIT-2) = MESSAGE(2:ISPLIT-2)
         MORTXT(ISPLIT-1:ISPLIT-1) = '^' 
         ENDTXT = '^  '
         DO 60 J = ISPLIT+2,80
            IF (MESSAGE(J:J).EQ.'''') THEN
               GO TO 65
            END IF
            J1 = J - ISPLIT  
            ENDTXT(J1:J1) = MESSAGE(J:J)
60       CONTINUE
65       CONTINUE
         J1 = J1 + 1
         ENDTXT (J1:J1) = '^'
         IF (DEVERS.EQ.'4.0') THEN
            NMSG=492
         ELSE
            NMSG=491
         ENDIF      
         CALL GETMSG(NMSG,MESSAGE)
         CALL GETMSG(999,MESSAGE)
      END IF
C
C  SET POINTER TO CURRENT LINE NUMBER IN THE TEXT ARRAY
C
      ISTRT = (IPAGE - 1) * MXTXTLN + 1
      IEND = ISTRT + MXTXTLN-1
      IF (ISTRT.GT.NUMLINE) THEN
         CALL BEEP
         RETURN
      ELSE IF (IEND.GT.NUMLINE) THEN
         IEND = NUMLINE
      END IF
      IPAGE = IPAGE + 1
C
C  DISPLAY THE CURRENT PAGE 
C
      I1 = 0
      CALL SETCOL(3)
      CALL CLR
      CALL INQWOR(XLOW,YLOW,XHIGH,YHIGH)
      XPOS = XLOW
      DO 200 I = ISTRT,IEND
         I1 = I1 + 1
         YPOS = YHIGH - I1*HEIGHT
         CALL MOVTCA(XPOS,YPOS)
         CALL DELTCU( )
         CALL DSPTEXT(TXTLINE(I))
200   CONTINUE
C
C  TELL USER IF THERE IS MORE OR THIS IS THE END OF THE TEXT
C
      I1 = I1 + 1
      YPOS = YHIGH - I1*HEIGHT
      CALL MOVTCA(XPOS,YPOS)
      CALL DELTCU( )
      IF (IEND.EQ.NUMLINE) THEN
         CALL DSPTEXT(ENDTXT)
      ELSE
         CALL DSPTEXT(MORTXT)
      END IF
C
C       .. DISPLAY FUNCTION KEY LINE       
C      YPOS = YHIGH - (MXTXTLN+2)*HEIGHT
      YPOS = YLOW
      CALL MOVTCA(XPOS,YPOS)
      CALL DELTCU( )
      CALL DSPTEXT(MESSAGE)
      RETURN
      END 
************************************************************************
      SUBROUTINE DSPTEXT(FIELD)
C
C  ROUTINE TO WRITE A STRING WITH THE TEXT BETWEEN ^'S WRITTEN IN RED .
C  BLANK LINES ARE SKIPPED.
C
      CHARACTER*40 FIELD
C
      IF (FIELD.EQ.'                    ') THEN
         RETURN
      END IF
      CALL SETSTC(0,0)
      ISKP = 0
      DO 100 I1 = 1,40
         IF (FIELD(I1:I1).EQ.'^') THEN
C            IF (ICOLOR.EQ.3) THEN
C               ICOLOR = 2
C               CALL SETTCL(0,ICOLOR)
C            ELSE 
C               ICOLOR = 3
C               CALL SETTCL(0,ICOLOR)
C            END IF
            ISKP = ISKP + 1
         ELSE
            CALL STEXT('&'//FIELD(I1:I1)//'&')
         END IF
100   CONTINUE
C
      RETURN
      END
