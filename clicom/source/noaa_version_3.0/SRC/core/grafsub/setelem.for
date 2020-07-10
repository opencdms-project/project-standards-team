$STORAGE:2
      SUBROUTINE SETELEM(ITYPE,IGRAPH,GRAFELEM,ELEMTEXT,NELEM,RTNFLAG)
C
C   ROUTINE TO ALLOW USERS TO SELECT THE ELEMENTS THEY WISH TO PLOT.
C   IT ALLOWS SELECTION OF UP TO 24 ELEMENTS AND ALSO ALLOWS USERS TO 
C   SPECIFY LONG TERM MEANS AND EXTREMES AT THE SAME RESOLUTION AS THE
C   DATA FOR MONTHLY, DAILY AND 10 DAY DATA.  IN OTHER WORDS YOU CAN
C   SELECT MONTHLY MEANS FOR MLY DATA, DAILY MEANS FOR DLY, ETC.
C   THESE SPECIAL ELEMENTS ARE INDICATED WITH A PREFIX DEFINED BELOW.
C
C   ---> ROUTINE ASSOCIATED WITH GRAFINIT <---
C
C   1000 - INDICATES LONG TERM MEAN 
C   2000 - INDICATES LONG TERM EXTREME MAX 
C   3000 - INDICATES LONG TERM EXTREME MIN 
C
      PARAMETER (MXELEM=24)
      INTEGER*2 GRAFELEM(MXELEM),NELEM,ITYPE,IGRAPH,HIELEM(6),LOWELEM(6)
     +          ,EFLAG
      CHARACTER*80 ELEMREC
      CHARACTER*64 HELPFILE
      CHARACTER*28 ELEMTEXT(MXELEM)
      CHARACTER*4 FIELD(24)
      CHARACTER*2 RTNFLAG
      DATA LOWELEM /201,401,001,101,101,101/
     +    ,HIELEM  /300,500,100,200,200,200/
     +    ,HELPFILE /'P:\HELP\GRFINIT3.HLP'/
C
C   INITIALIZE
C
      IF (IGRAPH.EQ.2) THEN
         IMAX = 22
      ELSE
         IMAX = 24
      END IF

      DO 50 I1 = 1,IMAX
         IF (GRAFELEM(I1).EQ.0) THEN
            FIELD(I1) = '    '
         ELSE IF (GRAFELEM(I1).GE.1000) THEN
            WRITE(FIELD(I1),'(I4)') GRAFELEM(I1)
         ELSE
            WRITE(FIELD(I1),'(I3.3,1X)') GRAFELEM(I1)
         END IF
50    CONTINUE 
55    CONTINUE        
      OPEN(30,FILE='P:\DATA\ELEM.DEF',STATUS='OLD',ACCESS='DIRECT'
     +      ,RECL=110,SHARE='DENYWR',MODE='READ',IOSTAT=IOCHK)
      IF (IOCHK.NE.0) THEN
         CALL OPENMSG('P:\DATA\ELEM.DEF      ','GRAFINIT    ',IOCHK)
         GO TO 55
      END IF

      CALL CLS
      CALL WRTMSG(24,368,14,0,0,' ',0)
      CALL WRTMSG(22,369,14,0,0,' ',0)
      CALL WRTMSG(21,370,14,0,0,' ',0)
      CALL WRTMSG(20,371,14,0,0,' ',0)
      CALL WRTFNC(9)
C
C   PRINT ANY EXISTING ELEMENT SELECTIONS AND THEIR DEFINITIONS
C
70    CONTINUE
      DO 80 I1 = 1,IMAX
         IF (I1.LE.12) THEN
            IROW = I1 + 7
            ICOL = 1
         ELSE
            IROW = I1 - 5
            ICOL = 42
         END IF
         READ(FIELD(I1),'(I4)') GRAFELEM(I1)
         IF (GRAFELEM(I1).GT.0) THEN
            CALL ELEMTXT(GRAFELEM(I1),IELEM,ELEMTEXT(I1),EFLAG)
         ELSE
            ELEMTEXT(I1) = ' '
         END IF
         CALL LOCATE(IROW,ICOL,IERR)
         CALL WRTSTR(FIELD(I1),4,15,1)
         CALL LOCATE(IROW,ICOL+6,IERR)
         CALL WRTSTR(ELEMTEXT(I1),28,15,1)
         IF (I1.GT.12) THEN
            IROW = 8
         END IF
80    CONTINUE      
C
C       ** USER IS NOT ALLOWED TO REVISE/ADD TO SKEWT ELEMENTS
C
      IF (IGRAPH.EQ.3) THEN
   85    CALL GETCHAR(0,RTNFLAG)
         IF (RTNFLAG.EQ.'1F') THEN
C             .. F1 - DISPLAY CURRENT HELP WINDOW
            CALL DSPWIN(HELPFILE)
            GO TO 85
         ELSE   
C             .. FOR ANY OTHER INPUT CHARACTER RETURN
            GO TO 500
         ENDIF
      ENDIF      
C
C   READ EACH OF THE ELEMENTS WANTED FROM THE USER AND DISPLAY THE
C   ELEMENT DEFINITION.  APPEND EXTR MAX, MIN, MEAN ETC TO THE NAME
C   IF THE SPECIAL CODES > 1000 ARE USED.  PROVIDE LIST OF ELEMENT
C   DEFINITIONS TO THE USER IF HE PRESSES SHIFT-F1.
C
      I1 = 0
100   CONTINUE      
         EFLAG = 0
         I1 = I1 + 1
         IF (I1.LE.12) THEN
            IROW = I1 + 7
            ICOL = 1
         ELSE
            IROW = I1 - 5
            ICOL = 42
         END IF
110      CONTINUE
         CALL LOCATE(IROW,ICOL,IERR)
         CALL GETSTR(3,FIELD(I1),4,15,1,RTNFLAG)
         CALL CLRMSG(3)
         CALL CLRMSG(2)
C
C    F1 - DISPLAY CURRENT HELP WINDOW
C         
         IF (RTNFLAG.EQ.'1F') THEN
            CALL DSPWIN(HELPFILE)
            GO TO 110
C
C    SHIFT-F1 - LOOKUP ELEMENT DEFINITIONS AND DISPLAY CHOICE LIST
C         
         ELSE IF (RTNFLAG.EQ.'1S') THEN
            CALL VALWIN(30,ELEMREC,80,2,ITYPE)
            IF (ELEMREC(1:3).EQ.' ') THEN
               GO TO 110
            ELSE
               FIELD(I1) = ELEMREC(1:3)
               CALL LOCATE(IROW,ICOL,IERR)
               CALL WRTSTR(FIELD(I1),4,15,1)
            END IF
C
C    F5 - CLEAR ALL FIELDS
C
         ELSE IF (RTNFLAG.EQ.'5F') THEN
            DO 200 I1 = 1,IMAX
               FIELD(I1) = ' '
200         CONTINUE
            GO TO 70
         END IF
         CALL LOCATE(IROW,ICOL+6,IERR)
         IF (FIELD(I1).EQ.'    ') THEN
            ELEMTEXT(I1) = '  '
            CALL WRTSTR(ELEMTEXT(I1),28,15,1)
         ELSE
            READ(FIELD(I1),'(I4)') GRAFELEM(I1)
            CALL ELEMTXT(GRAFELEM(I1),IELEM,ELEMTEXT(I1),EFLAG)
            CALL WRTSTR(ELEMTEXT(I1),28,15,1)
C
C      CHECK THE ELEMENT CODE AGAINST THE OBS-TYPE AND DISPLAY A WARNING
C      MESSAGE IF THEY DO NOT AGREE.
C
            IF (EFLAG.EQ.1) THEN
               CALL WRTMSG(3,180,12,1,0,' ',0)
               CALL LOCATE(IROW,ICOL,IERR)
               CALL WRTSTR(FIELD(I1),4,15,7)
               CALL LOCATE(IROW,ICOL+6,IERR)
               CALL WRTSTR(ELEMTEXT(I1),28,15,7)
            ELSE 
C                .. CHECK THAT ELEMENT CODES ARE VALID FOR THE DATA TYPE
C                   IELEM IS THE ELEMENT CODE WITH THE FLAG FOR MEANS, ETC
C                   REMOVED (1000,2000,3000).  GRAFELEM IS THE ACTUAL CODE
C                   THAT WAS SELECTED    
               IF (IELEM.LT.LOWELEM(ITYPE) .OR. 
     +            (IELEM.GT.HIELEM(ITYPE).AND.IELEM.LE.500) .OR.
     +             GRAFELEM(I1).GT.4000) THEN
                  NMSG = 179
               ELSE IF (GRAFELEM(I1).GT.1000 .AND. ITYPE.GT.3) THEN
                  NMSG = 406
               ELSE IF (GRAFELEM(I1).GT.1000 .AND. IGRAPH.GT.2) THEN
                  NMSG = 407
               ELSE
                  NMSG = 0
               ENDIF
               IF (NMSG.GT.0) THEN      
                  CALL WRTMSG(3,NMSG,12,1,0,' ',0)
                  CALL LOCATE(IROW,ICOL,IERR)
                  CALL WRTSTR(FIELD(I1),4,15,7)
                  CALL LOCATE(IROW,ICOL+6,IERR)
                  CALL WRTSTR(ELEMTEXT(I1),28,15,7)
                  EFLAG = 1
               ENDIF
            END IF
         END IF
C         
C   CHECK FUNCTION KEYS ENTERD AND/OR GET MORE ELEMENTS IF THEY ARE 
C   ALLOWED FOR THIS GRAPH TYPE
C         
         IF (EFLAG.EQ.1) THEN
            IF (I1.GE.1) THEN
                I1 = I1 - 1
            END IF
            IF (RTNFLAG.NE.'4F') RTNFLAG='  ' 
         ELSE IF (RTNFLAG.EQ.'UA'.OR.RTNFLAG.EQ.'BT') THEN
            IF (I1.GT.1) THEN
                I1 = I1 - 2
            END IF
         ELSE IF (RTNFLAG.EQ.'RA') THEN
            IF (I1.LE.12) THEN
               I1 = I1 + 11
            END IF
         ELSE IF (RTNFLAG.EQ.'LA') THEN
            IF (I1.GT.12) THEN
               I1 = I1 - 13
            END IF
         END IF
         IF (RTNFLAG.NE.'2F'.AND.RTNFLAG.NE.'4F') THEN
            IF (IGRAPH.EQ.4) THEN
               IF (I1.LT.2) THEN
                  GO TO 100
               ELSE   
                  CALL GETCHAR(0,RTNFLAG)
               END IF
            ELSE IF (I1.LT.IMAX) THEN
               GO TO 100
            END IF
         END IF
C         
C  DETERMINE HOW MANY ELEMENTS HAVE BEEN SPECIFIED AND FILL IN ANY GAPS
C
      NELEM = 0
      IF (RTNFLAG.EQ.'2F') THEN
         DO 350 I1 = 1, IMAX
            IF (FIELD(I1).NE.'    ') THEN
               NELEM = NELEM + 1
               IF (NELEM.LT.I1) THEN
                  READ(FIELD(I1),'(I4)') GRAFELEM(NELEM)
                  FIELD(I1) = ' '
               END IF
            END IF
  350    CONTINUE
C
C          ** A STANDARD ELEMENT CODE MUST BE SPECIFIED WITH MEANS/EXTREMES
C   
         DO 360 I=1,NELEM
            IF (GRAFELEM(I).LT.1000) GO TO 361
  360    CONTINUE              
         CALL WRTMSG(3,403,12,1,0,' ',0)
         GO TO 70
  361    CONTINUE       
C
C          ** WIND ROSE REQUIRES TWO ELEMENT CODES
C  
         IF (IGRAPH.EQ.4 .AND. NELEM.NE.2) THEN
            CALL WRTMSG(3,439,12,1,0,' ',0)
            GO TO 70
         ENDIF   
      ENDIF
C         
500   CONTINUE
      CLOSE (30)         
      RETURN
      END

      SUBROUTINE ELEMTXT(IELEM,JELEM,ELEMTEXT,EFLAG)
C
      CHARACTER*78 MSGLIN        
      CHARACTER*28 ELEMTEXT
      CHARACTER*16 ELEMDEF
      CHARACTER*12 ELEMPRFX(3)
      CHARACTER*6  ELEMABRV
      CHARACTER*3  ELEMCODE   
      CHARACTER*2  RTNCODE
      INTEGER*2    EFLAG
      LOGICAL      FIRSTCALL
      DATA FIRSTCALL/.TRUE./
C
      IF (FIRSTCALL) THEN
         FIRSTCALL=.FALSE.
         MSGLIN = ' '
         CALL GETMSG(317,MSGLIN)
         CALL PARSE1(MSGLIN,78,3,12,ELEMPRFX,RTNCODE)
         CALL GETMSG(999,MSGLIN)
      ENDIF
C
      IF (IELEM.GT.1000) THEN
         JELEM = IELEM/1000
         JELEM = IELEM - JELEM*1000
      ELSE
         JELEM = IELEM
      END IF
      
      READ (30,REC=JELEM,ERR=100) ELEMCODE,ELEMDEF,ELEMABRV
      READ(ELEMCODE,'(I3)',ERR=100) KELEM
      IF (KELEM.NE.JELEM) THEN
         GO TO 100
      END IF         
      ELEMTEXT = ELEMABRV
      IF (IELEM.GT.3000) THEN
C          .. RECORD MIN      
         NCHR=LNG(ELEMPRFX(1))
         ELEMTEXT = ELEMPRFX(1)(1:NCHR)//' '//ELEMABRV
      ELSE IF (IELEM.GT.2000) THEN
C          .. RECORD MAX      
         NCHR=LNG(ELEMPRFX(2))
         ELEMTEXT = ELEMPRFX(2)(1:NCHR)//' '//ELEMABRV
      ELSE IF (IELEM.GT.1000) THEN
C          .. NORMAL     
         NCHR=LNG(ELEMPRFX(3))
         ELEMTEXT = ELEMPRFX(3)(1:NCHR)//' '//ELEMABRV
      END IF
      EFLAG = 0
      RETURN
100   CONTINUE 
      EFLAG = 1    
      ELEMTEXT = ' '
      END
