$STORAGE:2
 
      SUBROUTINE GETLFRM(FORMNAME,HELPFILE,FIELD,FLDLEN,RTNFLAG)
C
C   SUBROUTINE TO WRITE AND READ A GENERIC FORM AS DESCRIBED BY THE   
C       INPUT PARAMETERS.                                               
C
C          FORMNAME...NAME OF THE FORM TO BE USED
C          HELPFILE...IF NOT = SPACES USER CAN PRESS F1 TO VIEW
C                     THIS HELP FILE (CHAR*64)
C                     ** REFER TO FIELD WHEN GETFRM IS CALLED FROM GRAFOPTN
C                        AND HELP IS REQUESTED 
C          FIELD......ARRAY OF THE FIELD VALUES RETURNED 
C                     IF CALLED BY GRAFOPTN, FIELD(1,MAXFLD) IS A SPECIAL CASE
C                         INPUT:  START FIELD POSITION IF NOT BLANK
C                        OUTPUT:  IF RTNFLAG=1F, FIELD(1,MAXFLD) CONTAINS THE
C                                 FIELD NUMBER FOR WHICH HELP IS REQUESTED
C          FLDLEN.....THE CHARACTER LENGTH OF FIELD
C          RTNFLAG....2 CHARACTER RETURN CODE 
C                 ON INPUT: 
C                    NORMALLY A SPACE
C                    IF = BN  ROUTINE CALLED BY BANNER SO ALLOW:
C                             1. USE OF THE F3 AND SHIFT F3 KEYS
C                             2. WINDOW LOOKUP OF STATION-IDS
C                       = SS  ALLOW WINDOW LOOKUP OF STN-ID
C                       = MM  ALLOW WINDOW LOOKUP OF MAP NAMES
C                       = E?  ALLOW WINDOW LOOKUP OF ELEM-CODES
C                             ? = ITYPE (1=7, WITH 0 = ALL TYPES)
C                       = FA  DISABLE F5 (CLEAR FORM) -- ALLOW F2
C                       = FV  ALLOW F3 (FIND NXT) -- DISABLE F5 (CLEAR FORM)
C                       = G1  FIRST CALL BY GRAFOPTN SO ALLOW USE OF 
C                             PgDn -- DISABLE F5 (CLEAR FORM)
C                       = GO  SUBSEQUENT CALL BY GRAFOPTN SO ALLOW
C                             PgDn AND RETURN CURSOR TO THE LAST
C                             FIELD USED IN THE PREVIOUS CALL --
C                             DISABLE F5 (CLEAR FORM)
C                       = P1  FIRST CALL BY CFGPRNT -- PAGE 1 OF FORM
C                             ALLOW PgDn
C                       = P2  SECOND CALL BY CFGPRNT -- PAGE 2 OF FORM
C                             ALLOW PgUp
C                 ON OUTPUT:
C                      LAST KEY READ IF IT WAS A CONTROL KEY.
C
C  ----> NOTE:  IF THIS PROGRAM IS CHANGED YOU MUST CREATE A NEW VERSION
C               OF THE GETFRM SUBROUTINE.  IT IS IDENTICAL TO THIS 
C               ROUTINE EXCEPT IN IT MAXFLD = 30...  GETLFRM IS USED
C               ONLY BY THE SETUP AND CFGPRNT SUBROUTINES.
C
      PARAMETER (MAXFLD=90)
      INTEGER*2 STRTROW,STRTCOL,FRMWID,FRMHT,FLDLEN
      CHARACTER*1 FIELD(FLDLEN,MAXFLD)
      CHARACTER*1 RTNCODE
      CHARACTER*2 RTNFLAG
      CHARACTER*64 HELPFILE
      LOGICAL BORDER,BANNER,HELP,STNS,ELEMS,GRAF1,GRAF2,MAPS,EXITSV,
     +        CFGPRT1,CFGPRT2,DSDFMT,F5ON
C
      CHARACTER*1 FLDTYPE(MAXFLD)
      CHARACTER*8 FORMNAME
      CHARACTER*22 LKFILE(MAXFLD)
      INTEGER*2    FLDROW(MAXFLD),FLDCOL(MAXFLD),FLENGTH(MAXFLD)
      INTEGER*2    LKFLEN(MAXFLD),LKFRECFLD(MAXFLD),LKFPOS(MAXFLD)
      CHARACTER*72 HIGHLIM(MAXFLD),LOWLIM(MAXFLD),DEFAULT(MAXFLD)
     +            ,AFIELD(MAXFLD)
      LOGICAL     LOWCASE(MAXFLD),RANGE(MAXFLD),LOOKUP(MAXFLD)
     +           ,NOENTRY(MAXFLD)
      COMMON /SCRTCH/ HIGHLIM,LOWLIM,DEFAULT,AFIELD
      COMMON /SCRTC2/ NUMFLD,FLDROW,FLDCOL,FLENGTH,NOENTRY
C
      CALL POSLIN(IROW,ICOL)
C
C   DETERMINE WHAT ACTIONS ARE ALLOWED DEPENDING ON THE INITIAL
C   VALUE OF RTNFLAG.  THEN WRITE THE FUNCTION KEY LINE
C
      BANNER  = .FALSE.
      HELP    = .FALSE.
      STNS    = .FALSE.
      ELEMS   = .FALSE.
      MAPS    = .FALSE.
      GRAF1   = .FALSE.
      GRAF2   = .FALSE.
      CFGPRT1 = .FALSE.
      CFGPRT2 = .FALSE.
      DSDFMT  = .FALSE.
      F5ON    = .TRUE.
      IF (RTNFLAG.EQ.'BN') THEN
         BANNER = .TRUE.
         STNS = .TRUE.
         RTNFLAG = '  '
         CALL WRTFNC(6)
      ELSE IF (RTNFLAG.EQ.'SS') THEN
         STNS = .TRUE.
         RTNFLAG = '  '
         CALL WRTFNC(9)
      ELSE IF (RTNFLAG.EQ.'MM') THEN
         MAPS = .TRUE.
         RTNFLAG = '  '
         CALL WRTFNC(9)
      ELSE IF (RTNFLAG.EQ.'FA') THEN
         F5ON   = .FALSE.
         CALL WRTFNC(14)
      ELSE IF (RTNFLAG.EQ.'FV') THEN
         DSDFMT = .TRUE.
         F5ON   = .FALSE.
         CALL WRTFNC(15)
      ELSE IF (RTNFLAG.EQ.'G1') THEN
         GRAF1 = .TRUE.
         F5ON  = .FALSE.
         CALL WRTFNC(10)
      ELSE IF (RTNFLAG.EQ.'GO') THEN
         GRAF2 = .TRUE.
         F5ON  = .FALSE.
         CALL WRTFNC(10)
      ELSE IF (RTNFLAG.EQ.'P1') THEN
         CFGPRT1 = .TRUE.
         CALL WRTFNC(12)
      ELSE IF (RTNFLAG.EQ.'P2') THEN
         CFGPRT2 = .TRUE.
         CALL WRTFNC(13)
      ELSE IF (RTNFLAG(1:1).EQ.'E') THEN
         ELEMS = .TRUE.
         READ(RTNFLAG,'(1X,I1)') ITYPE
         RTNFLAG = '  '
         CALL WRTFNC(9)
      ELSE IF (HELPFILE(1:1).NE.' ') THEN
         HELP = .TRUE.
         CALL WRTFNC(7)
      ELSE
         CALL WRTFNC(1)
      END IF
      IF (HELPFILE(1:1).NE.' ') THEN
         HELP = .TRUE.
      END IF
C
C    WRITE THE FORM TO THE SCREEN AND RETRIEVE FIELD DEFINITIONS
C
      IF (.NOT.GRAF2) THEN
         CALL DSPFRM(FORMNAME,FRMHT,FRMWID,BORDER,IROW,ICOL,RTNCODE)
         IF (RTNCODE.NE.'0') THEN
            RTNFLAG = '4F'
            RETURN
         END IF
$INCLUDE: 'RDFLDS.INC'
         CLOSE(15)
      END IF
      STRTROW = IROW
      STRTCOL = ICOL
C
C     DRAW THE INITIAL FIELD VALUES
C
   20 CONTINUE
      IF (.NOT.GRAF2) THEN
         DO 80 IFLD = 1,NUMFLD 
            IF (FIELD(1,IFLD)(1:FLDLEN).NE.'     ') THEN
               AFIELD(IFLD) = FIELD(1,IFLD)(1:FLDLEN)
            ELSE
               IF (DEFAULT(IFLD)(1:2).NE.'""')THEN
                  AFIELD(IFLD) = DEFAULT(IFLD)
               END IF
            END IF
C
C      IF THIS IS A LOOKUP FIELD - DO THE LOOKUP
C
            IF (LOOKUP(IFLD)) THEN
               CALL LOOKIT(IFLD,MAXFLD,AFIELD,FLENGTH,LKFILE
     +             ,LKFLEN,LKFRECFLD,LKFPOS,RTNCODE)
               IF (RTNCODE.EQ.'1') THEN
                  MSGNUM = 91
                  GO TO 470
               ELSE IF (RTNCODE.EQ.'2') THEN
                  MSGNUM = 47
                  GO TO 470
               END IF
            END IF 
C  
            IROW = STRTROW + FLDROW(IFLD)
            ICOL = STRTCOL + FLDCOL(IFLD)
            CALL LOCATE(IROW,ICOL,IERR)
            CALL WRTSTR(AFIELD(IFLD),FLENGTH(IFLD),15,1)
   80    CONTINUE
      END IF
C
C   RETRIEVE THE FIELDS
C
      IF (GRAF2) THEN
         IF (FIELD(1,MAXFLD)(1:2).NE.'  ') THEN
            READ(FIELD(1,MAXFLD)(1:2),'(I2)') IFIELD
         ENDIF   
      ELSE
         IFIELD = 1  
      END IF
      RTNFLAG = '  '
  100 CONTINUE
      IROW = STRTROW + FLDROW(IFIELD)
      ICOL = STRTCOL + FLDCOL(IFIELD)
      CALL LOCATE(IROW,ICOL,IERR)
C
C   IF THIS IS A LOOKUP FIELD - DO THE LOOKUP
C
      IF (LOOKUP(IFIELD)) THEN
         CALL LOOKIT(IFIELD,MAXFLD,AFIELD,FLENGTH,LKFILE
     +       ,LKFLEN,LKFRECFLD,LKFPOS,RTNCODE)
         IF (RTNCODE.EQ.'1') THEN
            MSGNUM = 91
            GO TO 470
         ELSE IF (RTNCODE.EQ.'2') THEN
            MSGNUM = 47
            GO TO 470
         END IF
         CALL WRTSTR(AFIELD(IFIELD),FLENGTH(IFIELD),15,1)
      END IF 
      CALL LOCATE(IROW,ICOL,IERR)
C
C  IF THIS IS A DEFAULT TO A PREVIOUS FIELD - DO THE DEFAULT
C
      IF (DEFAULT(IFIELD)(1:2).EQ.'""')THEN
         J = 0
         DO 110 I = 15,3,-1
            IF (DEFAULT(IFIELD)(I:I).EQ.' ')THEN
               IF (J.GT.0)THEN
                  GO TO 112
               END IF
               GO TO 110
            END IF
            IF (DEFAULT(IFIELD)(I:I).EQ.',')THEN
               GO TO 112
            END IF
            IF (DEFAULT(IFIELD)(I:I).GT.'9'.OR.
     +          DEFAULT(IFIELD)(I:I).LT.'0')THEN
                   J = 100
                   GO TO 112
            ELSE
               J = J * 10
               READ (DEFAULT(IFIELD)(I:I),'(I1)')K
               J = J + K
            END IF
  110    CONTINUE
  112       IF (J.LT.1.OR.J.GT.IFIELD)THEN
               NOENTRY(IFIELD) = .FALSE.
               AFIELD(IFIELD)(1:10) = '**********'
            ELSE
               NOENTRY(IFIELD) = .TRUE.
               AFIELD(IFIELD) = AFIELD(J)
            END IF
            CALL WRTSTR(AFIELD(IFIELD),FLENGTH(IFIELD),15,1)
      END IF
C
C   RETRIEVE THE FIELD VALUE - UNLESS NO ENTRY IS SET
C
      IF (NOENTRY(IFIELD)) THEN
         IF ((RTNFLAG.EQ.'UA'.OR.RTNFLAG.EQ.'LA'.OR.RTNFLAG.EQ.'HO'
     +       .OR.RTNFLAG.EQ.'BT')
     +       .AND.IFIELD.EQ.1) THEN
            RTNFLAG = '  '
         END IF
      ELSE
C
C   RETRIEVE THE FIELD VALUE
C
         IF (FLDTYPE(IFIELD).EQ.'T') THEN
            IF (LOWCASE(IFIELD)) THEN
               ITYP = 1
            ELSE
               ITYP = 0
            END IF   
         ELSE IF (FLDTYPE(IFIELD).EQ.'R')THEN
            ITYP = 4
         ELSE
            ITYP = 3
         END IF
         CALL GETSTR (ITYP,AFIELD(IFIELD),FLENGTH(IFIELD),15,1,RTNFLAG)
      END IF
      CALL CLRMSG(2)
C
C   CHECK THE CURSOR CONTROL KEYS RETURNED
C
      IF (BANNER) THEN
         IF (RTNFLAG.EQ.'3F'.OR.RTNFLAG.EQ.'3S') THEN
            GO TO 500
         END IF
      END IF
      IF (DSDFMT .AND. RTNFLAG.EQ.'3F') THEN
         GO TO 500
      END IF
      EXITSV = (RTNFLAG.EQ.'2F')                                   .OR.
     +         (RTNFLAG.EQ.'DP' .AND. (GRAF1.OR.GRAF2.OR.CFGPRT1)) .OR.
     +         (RTNFLAG.EQ.'UP' .AND. CFGPRT2)
      IF (RTNFLAG.EQ.'1F'.AND.HELP) THEN
         IF (GRAF1 .OR. GRAF2) THEN
            WRITE(FIELD(1,MAXFLD)(1:2),'(I2)') IFIELD
            GO TO 500
         ELSE
            CALL DSPWIN(HELPFILE)
         ENDIF
      ELSE IF (RTNFLAG.EQ.'1S') THEN
         IF (STNS.AND.FLENGTH(IFIELD).EQ.8) THEN
            CALL GETSTN(AFIELD(IFIELD))
         ELSE IF (MAPS.AND.FLENGTH(IFIELD).EQ.8) THEN
            CALL GETMAP(AFIELD(IFIELD))
         ELSE IF (ELEMS.AND.FLENGTH(IFIELD).EQ.3) THEN
            CALL GETELEM(AFIELD(IFIELD),ITYPE)
         ELSE
            CALL BEEP
         END IF
      ELSE IF (RTNFLAG.EQ.'4F') THEN
         GO TO 500
      ELSE IF (RTNFLAG.EQ.'5F' .AND. F5ON) THEN
         DO 210 I2 = 1,NUMFLD  
            FIELD(1,I2)(1:FLDLEN) = '                    '
  210    CONTINUE
         GO TO 20
C
C   BACK-TAB AND LEFT-ARROW RETURN TO THE PREVIOUS FIELD
C
      ELSE IF (RTNFLAG.EQ.'BT'.OR.RTNFLAG.EQ.'LA') THEN
         IF (IFIELD.GT.1) THEN
            IFIELD = IFIELD - 1
         ELSE
            CALL BEEP
         END IF
C
C  HOME RETURNS TO FIELD 1
C
      ELSE IF (RTNFLAG.EQ.'HO') THEN
         IFIELD = 1
C
C  UP-ARROW MOVES TO THE PREVIOUS LINE (IF THERE IS ONE)
C
      ELSE IF (RTNFLAG.EQ.'UA') THEN
         CALL FNDFLD(0,IFIELD,NEWFLD)
         IFIELD = NEWFLD
C
C  OTHER KEYS WILL ATTEMPT TO MOVE TO NEXT FIELD - CHECK THIS FIELD 
C  FOR VALIDITY
C
      ELSE
         IF (RANGE(IFIELD)) THEN
            CALL CHKLIM(IFIELD,AFIELD,FLDTYPE(IFIELD),FLENGTH(IFIELD)
     +           ,LOWLIM(IFIELD),HIGHLIM(IFIELD),RTNCODE)
            IF (RTNCODE.NE.'0') THEN
               GO TO 100
            END IF
         END IF
C
C   DOWN-ARROW IF WANT TO MOVE DOWN A LINE CHECK FIELDS BEING SKIPPED
C         
         IF (RTNFLAG.EQ.'DA') THEN
            CALL FNDFLD(1,IFIELD,NEWFLD)
            IF (NEWFLD.GT.IFIELD+1) THEN
               DO 240 IFLD = IFIELD+1, NEWFLD
                  IF (RANGE(IFLD)) THEN
                     CALL CHKLIM(IFLD,AFIELD,FLDTYPE(IFLD),FLENGTH(IFLD)
     +                  ,LOWLIM(IFLD),HIGHLIM(IFLD),RTNCODE)
                     IF (RTNCODE.NE.'0') THEN
                        IFIELD = IFLD
                        GO TO 100
                     END IF  
                  END IF
  240         CONTINUE
           END IF
           IFIELD = NEWFLD
C
C   IF F2 ENTERED - WANT TO LEAVE THE FORM HERE - CHECK REMAINING FIELDS
C
         ELSE IF (EXITSV) THEN
            IF (IFIELD.LT.NUMFLD) THEN
               DO 250 IFLD = IFIELD+1, NUMFLD
                  IF (RANGE(IFLD)) THEN
                     CALL CHKLIM(IFLD,AFIELD,FLDTYPE(IFLD),FLENGTH(IFLD)
     +                  ,LOWLIM(IFLD),HIGHLIM(IFLD),RTNCODE)
                     IF (RTNCODE.NE.'0') THEN
                        IFIELD = IFLD
                        GO TO 100
                     END IF  
                  END IF
  250         CONTINUE
           END IF
           GO TO 500
C   
C   OTHERWISE CONTINUE TO NEXT FIELD
C
         ELSE IF (RTNFLAG.EQ.'EN') THEN
            IFIELD = NUMFLD  
260         CONTINUE
            IF (NOENTRY(IFIELD)) THEN
               IFIELD = IFIELD - 1
               GO TO 260
            END IF    
         ELSE IF (RTNFLAG.EQ.'TB'.OR.RTNFLAG.EQ.'RA'.OR.RTNFLAG.EQ.'RE'
     +          .OR.RTNFLAG.EQ.'  ') THEN
            IF (IFIELD.LT.NUMFLD) THEN
               IFIELD = IFIELD + 1
            ELSE
               IF (.NOT.CFGPRT2) THEN
                  RTNFLAG = '  '
                  GO TO 500
               ENDIF
            END IF            
         ELSE
            CALL BEEP
         END IF
      END IF
      GO TO 100

C
C   WRITE ERROR MESSAGES
C
  470 CONTINUE
      CALL WRTMSG(2,MSGNUM,12,1,0,' ',0)
      IF (IFIELD.LT.NUMFLD) THEN
         IFIELD = IFIELD + 1
      ELSE
         IFIELD = IFIELD - 1
      END IF
      GO TO 100
C
C   STORE THE VALUES INTO THE OUTPUT FIELD AND EXIT
C
  500 CONTINUE
      CALL CLRMSG(2)
      CALL LOCATE(STRTROW,STRTCOL,IERR)
      DO 550 IFLD = 1,NUMFLD
         FIELD(1,IFLD)(1:FLDLEN) = AFIELD(IFLD)
  550 CONTINUE
      RETURN
      END
***********************************************************************
      SUBROUTINE LOOKIT(IFIELD,MAXFLD,AFIELD,FLENGTH,LKFILE
     +       ,LKFLEN,LKFRECFLD,LKFPOS,RTNCODE)
C
C   ROUTINE TO LOOKUP A FIELD VALUE FOR LOOKUP FIELDS
C
      CHARACTER*1 HLDREC(500), RTNCODE
      CHARACTER*72 LKFLD
C
      CHARACTER*22 LKFILE(MAXFLD)
      INTEGER*2    FLENGTH(MAXFLD),LKFLEN(MAXFLD),LKFRECFLD(MAXFLD)
     +            ,LKFPOS(MAXFLD)
      CHARACTER*72 AFIELD(MAXFLD)
C
      IFLD = LKFRECFLD(IFIELD)
      IPOS = LKFPOS(IFIELD)
      IF (AFIELD(IFLD).NE.' ') THEN
         DO 40 I2 = 1,22
            IF (LKFILE(IFIELD)(I2:I2).EQ.'/') THEN
               LKFILE(IFIELD)(I2:I2) = '\'
            END IF
  40     CONTINUE
         OPEN(58,FILE=LKFILE(IFIELD),STATUS='OLD',ACCESS='DIRECT'
     +       ,RECL=LKFLEN(IFIELD),IOSTAT=IOCHK,SHARE='DENYWR'
     +       ,MODE='READ')
         IF (IOCHK.NE.0) THEN
            RTNCODE = '1'
            RETURN
         END IF
         READ(AFIELD(IFLD),'(BN,I4)') IREC
         READ(58,REC=IREC,ERR=460) (HLDREC(I),I=1,LKFLEN(IFIELD))
         CLOSE(58)
         LKFLD = ' '
         DO 60 I2 = 1,FLENGTH(IFIELD)
            LKFLD(I2:I2) = HLDREC(IPOS+I2-1)
  60     CONTINUE
         AFIELD(IFIELD) = LKFLD
      END IF
      RTNCODE = '0'
      RETURN
C
460   CONTINUE
      RTNCODE = '2'
      RETURN
      END
$PAGE
***********************************************************************
      SUBROUTINE CHKLIM(IFIELD,AFIELD,FLDTYPE,FLDLEN,LOWLIM
     +           ,HIGHLIM,RTNCODE)
C
C   ROUTINE TO CHECK THE CURRENT FIELD AGAINST DEFINED LIMITS
C
      PARAMETER (MAXFLD=30)
      INTEGER*2 IFIELD,FLDLEN
      REAL    RLIMIT,RVALUE
      INTEGER*4 ILIMIT,IVALUE
      CHARACTER*72 AFIELD(MAXFLD),LOWLIM,HIGHLIM,TSTLIM
      CHARACTER*1 FLDTYPE,RTNCODE
      CHARACTER*10 IFRMT,FFRMT
      DATA IFRMT,FFRMT/'(BN,I10)','(BN,F10.0)'/
C
      RTNCODE = '0'

      TSTLIM = '         '
      IF (FLDLEN.LT.72) THEN
         AFIELD(IFIELD)(FLDLEN+1:72) = '     '
      END IF
      IF (FLDTYPE.EQ.'R') THEN
         READ(AFIELD(IFIELD),FFRMT) RVALUE
      ELSE IF (FLDTYPE.EQ.'I') THEN
         READ(AFIELD(IFIELD),IFRMT) IVALUE
      END IF
C
      IF (LOWLIM.NE.'       ') THEN
         TSTLIM = '   '
         IF (LOWLIM(1:5).EQ.'FIELD') THEN
            READ(LOWLIM,'(BN,6X,I2)') IFLD
            TSTLIM = AFIELD(IFLD)(1:FLDLEN) 
         ELSE
            TSTLIM = LOWLIM(1:FLDLEN)
         END IF
         IF (FLDTYPE.EQ.'R') THEN
            READ(TSTLIM,FFRMT) RLIMIT
            IF (RVALUE.LT.RLIMIT) THEN
               GO TO 410
            END IF
         ELSE IF (FLDTYPE.EQ.'I') THEN
            READ(TSTLIM,IFRMT) ILIMIT
            IF (IVALUE.LT.ILIMIT) THEN
               GO TO 410
            END IF
         ELSE 
            IF (AFIELD(IFIELD).LT.TSTLIM) THEN
               GO TO 410
            END IF
         END IF
      END IF    
      IF (HIGHLIM.NE.'       ') THEN
         TSTLIM = '  '
         IF (HIGHLIM(1:5).EQ.'FIELD') THEN
            READ(HIGHLIM,'(BN,6X,I2)') IFLD
            TSTLIM = AFIELD(IFLD)(1:FLDLEN) 
         ELSE
            TSTLIM = HIGHLIM(1:FLDLEN)
         END IF
         IF (FLDTYPE.EQ.'R') THEN
            READ(TSTLIM,FFRMT) RLIMIT
            IF (RVALUE.GT.RLIMIT) THEN
               GO TO 420
            END IF
         ELSE IF (FLDTYPE.EQ.'I') THEN
            READ(TSTLIM,IFRMT) ILIMIT
            IF (IVALUE.GT.ILIMIT) THEN
               GO TO 420
            END IF
         ELSE 
            IF (AFIELD(IFIELD).GT.TSTLIM) THEN
               GO TO 420
            END IF
         END IF
      END IF    

      RETURN
       
  410 CONTINUE
      MSGNUM = 87
      CALL WRTMSG(2,MSGNUM,12,1,0,TSTLIM,40)
      RTNCODE = '1'
      RETURN 
  420 CONTINUE
      MSGNUM =  86
      CALL WRTMSG(2,MSGNUM,12,1,0,TSTLIM,40)
      RTNCODE = '1'
      RETURN
      END 
$PAGE
************************************************************************
      SUBROUTINE FNDFLD(UPDWN,IFIELD,NEWFLD)
C
C   ROUTINE TO FIND THE NEAREST FIELD ABOVE OR BELOW THE CURRENT FIELD
C
C     PASSED PARAMETERS:
C
C          UPDWN.....< 0 MEANS GO UP, > 0 MEANS GO DOWN
C          IFIELD....CURRENT FIELD
C          NEWFLD....NEW FIELD NUMBER (= IFIELD IF NONE FOUND)
C
      INTEGER*2 UPDWN,IFIELD,NEWFLD,NUMFLD,FLDDST,MINDST
      PARAMETER (MAXFLD=90)
      INTEGER*2 FLDROW(MAXFLD),FLDCOL(MAXFLD),FLENGTH(MAXFLD)
      LOGICAL NOENTRY(MAXFLD)
      COMMON /SCRTC2/ NUMFLD,FLDROW,FLDCOL,FLENGTH,NOENTRY
C
C  SEARCH FORWARD IF REQUESTED -------------------------------------
C
      NEWFLD = IFIELD
      MINDST = 999
      IF (UPDWN.GT.0) THEN
         IF (IFIELD.LT.NUMFLD) THEN
C
C     FIRST FIND A FIELD IN A ROW FURTHER DOWN
C         
            DO 100 IFLD = IFIELD+1,NUMFLD
               IF (NOENTRY(IFLD)) THEN
                  GO TO 100
               END IF
               IF (FLDROW(IFLD).GT.FLDROW(IFIELD)) THEN
                  NXTROW = FLDROW(IFLD)      
C
C         THEN FIND A FIELD THAT INCLUDES THE SAME COLUMNS AS THE 
C         CURRENT FIELD - IF FOUND THEN TAKE IT AND RETURN, ELSE FIND
C         THE CLOSEST FIELD ON THE SAME LINE
C 
                  DO 70 JFLD = IFLD,NUMFLD
                     IF (NOENTRY(JFLD)) THEN
                        GO TO 70
                     END IF
                     IF (FLDROW(JFLD).EQ.NXTROW) THEN
                        IF (FLDCOL(IFIELD).GE.FLDCOL(JFLD).AND.
     +                       FLDCOL(IFIELD).LE.FLDCOL(JFLD) +
     +                                         FLENGTH(JFLD)) THEN
                           NEWFLD = JFLD
                           RETURN
                        ELSE
                           FLDDST = ABS(FLDCOL(IFIELD)-FLDCOL(JFLD))
                           IF (FLDDST.LT.MINDST) THEN
                              MINDST = FLDDST
                              MINFLD = JFLD                  
                           END IF
                        END IF
                     END IF   
   70             CONTINUE
                  NEWFLD = MINFLD
                  RETURN
               END IF      
  100       CONTINUE
C
C       IF GET HERE NO FIELD BELOW FOUND
C
            CALL BEEP
            RETURN
         ELSE        
            CALL BEEP
            RETURN       
         END IF
C
C   ELSE SEARCH BACKWARD -------------------------------------------
C
      ELSE
         IF (IFIELD.GT.1) THEN
C
C     FIRST FIND A FIELD IN A ROW ABOVE
C         
            DO 200 IFLD = IFIELD-1,1,-1
               IF (NOENTRY(IFLD)) THEN
                  GO TO 200
               END IF
               IF (FLDROW(IFLD).LT.FLDROW(IFIELD)) THEN
                  NXTROW = FLDROW(IFLD)
C
C         THEN FIND A FIELD THAT INCLUDES THE SAME COLUMNS AS THE 
C         CURRENT FIELD - IF FOUND THEN TAKE IT AND RETURN, ELSE FIND
C         THE CLOSEST FIELD ON THE SAME LINE
C 
                  DO 170 JFLD = IFLD,1,-1
                     IF (NOENTRY(JFLD)) THEN
                        GO TO 170
                     END IF
                     IF (FLDROW(JFLD).EQ.NXTROW) THEN
                        IF (FLDCOL(IFIELD).GE.FLDCOL(JFLD).AND.
     +                       FLDCOL(IFIELD).LE.FLDCOL(JFLD) +
     +                                         FLENGTH(JFLD)) THEN
                           NEWFLD = JFLD
                           RETURN
                        ELSE
                           FLDDST = ABS(FLDCOL(IFIELD)-FLDCOL(JFLD))
                           IF (FLDDST.LT.MINDST) THEN
                              MINDST = FLDDST
                              MINFLD = JFLD  
                           END IF                
                        END IF
                     END IF   
  170             CONTINUE
                  NEWFLD = MINFLD
                  RETURN
               END IF      
  200       CONTINUE
C
C       IF GET HERE NO FIELD ABOVE FOUND
C
            CALL BEEP
            RETURN
         ELSE        
            CALL BEEP
            RETURN       
         END IF
      END IF
      END
      