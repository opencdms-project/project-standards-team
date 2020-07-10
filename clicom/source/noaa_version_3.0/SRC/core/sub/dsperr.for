************************************************************************
$STORAGE:2
C
      SUBROUTINE DSPERR(FLAG,FLAG1,ILINE,IELEM)
C
C   THIS SUBROUTINE DISPLAYS THE REASON A FIELD FAILED ONE OF THE
C   VALIDATION CHECKS.  FLAG(2) FOR THE ELEMENT IN QUESTION IS CONVERTED
C   TO AN INTEGER AND USED AS AN INDEX TO THE ERROR TABLE 'ERRMSG' AND
C   THE ERROR MESSAGE IS DISPLAYED ON LINE 22 OF THE SCREEN.
C
$INCLUDE:'VAL1.INC'
      CHARACTER*1 FLAG,FLAG1
      INTEGER*2 IFLAG
      CHARACTER*78 ERRMSG(39)
      LOGICAL FRSTCALL,MSGON
      DATA FRSTCALL/.TRUE./,MSGON/.FALSE./
C
C  ERROR MESSAGES 1-39 FROM \DATA\MESSAGES.FTN WILL BE LOADED FROM 
C  DISK WHEN THIS ROUTINE IS CALLED FOR THE FIRST TIME
C
      IF (FRSTCALL)THEN
   10    CONTINUE
         DO 20 I = 1,39
            CALL GETMSG(I,ERRMSG(I))
  20     CONTINUE
         CALL GETMSG(999,ERRMSG)
         FRSTCALL = .FALSE.
      END IF
C
C  SET THE MESSAGE INDEX NUMBER ACCORDING TO THE FLAG VALUE
C
      IF (FLAG.EQ.' ')THEN
         IFLAG = 65
      ELSE
         IFLAG = ICHAR(FLAG)
      END IF
      I = IFLAG - 64
      IF (I.LT.1.OR.I.GT.38) THEN
         I = 39
      END IF
C
      IF (MSGON) THEN
         CALL CLRMSG(2)
      END IF
      IF (I.EQ.1) THEN
         IF (MSGON) THEN
            CALL CLRMSG(3)
         END IF
         MSGON = .FALSE.
         RETURN
      ELSE 
         MSGON = .TRUE.
         IF (FLAG1.EQ.'C'.AND.PASSWD.EQ.'DE')THEN
            CALL WRTMG(3,ERRMSG(I),1)
         ELSE
            CALL WRTMG(3,ERRMSG(I),0)
         END IF
C
C   DISPLAY RELATED COLUMN INFO IF APPROPRIATE
C 
         IF (I.GE.6.AND.I.LE.8) THEN
            CALL RELMSG(I,IELEM,ERRMSG(28))
         ELSE IF (I.EQ.23) THEN
            CALL HTMSG(ILINE,IELEM)   
         END IF   
      END IF
      RETURN
      END
C************************************************************************
$PAGE    
      SUBROUTINE HTMSG(ILINE,IELEM)
C      
      CHARACTER*78 ERRMSG      
C      
$INCLUDE:'VAL1.INC'
$INCLUDE:'RAOBQC.INC'
C      
      IF (TBLELEM(IELEM).EQ.302 .AND. ILINE.GT.0) THEN
         CALL GETMSG(46,ERRMSG)
         CALL GETMSG(999,ERRMSG)
         MAXL = LEN(ERRMSG)
C
         LGTH = LNG(ERRMSG)
         IF (LGTH+32.LE.MAXL) THEN
            WRITE(ERRMSG(LGTH+1:),500) HEIGHT(ILINE),COMPHT(ILINE)
            CALL WRTMG(2,ERRMSG,0)
            ERRMSG(1:) = ' '
         ENDIF   
      ENDIF
C      
      RETURN
C
C       ** FORMAT STMT      
  500 FORMAT(0PF15.6,2X,0PF15.6)
      END
C************************************************************************      
