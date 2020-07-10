$STORAGE:2

      SUBROUTINE CFGPRNT
C
C   ROUTINE TO BUILD THE PRINTER CONFIGURATION FILE 
C   IT INCLUDES CODES TO PRINT VARYING TEXT SIZES AS WELL AS
C   THE CODES TO PRINT GRAPHICS CHARACTERS OR THEIR EQUIVALENT
C
      CHARACTER*64 FILENAME
      CHARACTER*24 FIELD(20),FIELD2(12)
      CHARACTER*3  HEXHLD
      CHARACTER*2  RTNFLAG
      CHARACTER*1  LINCHR(11),REPLY,ACHAR
      INTEGER*2    RTNCODE,PRTCODE(7,8),ILINE(11),HLDNUM(11)
      INTEGER*2    DECHLD
C
      EQUIVALENCE (FIELD(9),FIELD2(1))
C
      DATA FILENAME /'P:\HELP\CFGPRNT.HLP'/
C
      OPEN (7,FILE='P:\DATA\PRINTER.CFG',STATUS='OLD')
C
      READ(7,'(A1,A1)') FIELD(1), FIELD(9)
      READ(7,'(7A24)') (FIELD(I),I=2,8)
      READ(7,'(11A1)') (LINCHR(I),I=1,11)  
      REWIND(7)
C
C   IF THE ORIGINAL USER SELECTED HEXIDECIMAL FOR THE PRINTER
C   CONTROLS,  WE MUST CONVERT THE DECIMAL VALUES JUST READ IN
C   TO HEX BEFORE DISPLAYING THE FORM.
C
      IF (FIELD(1)(1:1).EQ.'H')THEN
         DO 10 I = 2,8
            DO 12 J = 1,24,3
               READ(FIELD(I)(J:J+2),'(I3)')DECHLD
               CALL DEC2HEX(HEXHLD,3,DECHLD,RTNCODE)
               FIELD(I)(J:J+2) = HEXHLD
   12       CONTINUE
   10    CONTINUE
      END IF         
C
C   TRANSLATE THE LINE CHARACTERS FROM THE DECIMAL VALUE TO HEX
C
      DO 15 I = 1,11
         ILINE(I) = ICHAR(LINCHR(I))
         IF (FIELD(9)(1:1).EQ.'H')THEN
            CALL DEC2HEX(HEXHLD,3,ILINE(I),RTNCODE)
            FIELD(I+9) = HEXHLD
         ELSE
            WRITE(FIELD(I+9),'(I3.3)') ILINE(I)
         END IF
   15 CONTINUE         
C
C   READ THE FIRST DATA ENTRY FORM
C
   19 CONTINUE
      CALL CLS
   20 CONTINUE
      CALL LOCATE(1,0,IERR)
      RTNFLAG = 'P1'
      CALL GETLFRM('CFGPRNT1',FILENAME,FIELD,24,RTNFLAG)
      IF (RTNFLAG.EQ.'4F') THEN
         CALL LOCATE(24,0,IERR)
         RETURN
C      ELSE IF (RTNFLAG.EQ.'2F' .OR. RTNFLAG.EQ.'DP') THEN
      ELSE 
         IF (FIELD(1)(1:1).NE.'D'.AND.FIELD(1)(1:1).NE.'H')THEN
            GO TO 20    
         END IF
C      ELSE   
C         GO TO 20
      END IF
C
C       .. CHECK THE PRINTER CONTROLS
C
      IF (FIELD(1)(1:1).EQ.'H')THEN
         DO 50 I = 1,7
            J = I + 1
            CALL HEX2DEC(FIELD(J),24,3,HLDNUM,11,RTNCODE)
            IF (RTNCODE.NE.0)THEN
               WRITE(ACHAR,'(I1)')RTNCODE
               CALL WRTMSG(5,409,38,0,0,'ACHAR',1)
               GO TO 19
            END IF   
            DO 60 K = 1,8
               PRTCODE(I,K) = HLDNUM(K)
   60       CONTINUE
   50    CONTINUE
      ELSE
         DO 70 I = 1,7
            J = I + 1
            READ(FIELD(J),'(8I3)') (PRTCODE(I,K),K=1,8)
   70    CONTINUE
      END IF
C
C   READ THE SECOND DATA ENTRY FORM
C
   99 CONTINUE
      CALL CLS
  100 CONTINUE
      CALL LOCATE(1,0,IERR)
      RTNFLAG = 'P2'
      CALL GETLFRM('CFGPRNT2',FILENAME,FIELD2,24,RTNFLAG)
      IF (RTNFLAG.EQ.'4F') THEN
         CALL LOCATE(24,0,IERR)
         RETURN
      ELSE IF (RTNFLAG.EQ.'2F') THEN
         IF (FIELD(9)(1:1).NE.'D'.AND.FIELD(9)(1:1).NE.'H')THEN
            GO TO 100
         END IF
      ELSE IF (RTNFLAG.NE.'UP') THEN  
         GO TO 100    
      ELSE 
C          .. RTNFLAG EQUALS 'UP'      
         IF (FIELD(9)(1:1).NE.'D'.AND.FIELD(9)(1:1).NE.'H')THEN
            GO TO 100
         ELSE
            GO TO 19   
         END IF
      END IF
C
C       .. CHECK THE GRAPHICS CHARACTER CODES
C
      IF (FIELD(9)(1:1).EQ.'H')THEN
         DO 150 I = 1,7
            J = I + 9
            CALL HEX2DEC(FIELD(J),24,3,HLDNUM,11,RTNCODE)
            IF (RTNCODE.NE.0)THEN
               WRITE(ACHAR,'(I1)')RTNCODE
               CALL WRTMSG(5,409,38,0,0,'ACHAR',1)
               GO TO 99
            END IF   
            ILINE(I) = HLDNUM(1)
  150    CONTINUE
      ELSE
         DO 170 I = 1,11
            J = I + 9
            READ(FIELD(J),'(I3)') ILINE(I)
  170    CONTINUE
      END IF
C
C  DISPLAY THE CODES
C
      CALL WRTMSG(4,221,14,0,0,' ',0)
      DO 320 I = 1,11
         JCOL = I*3     
         CALL LOCATE(22,JCOL,IERR)
         CALL CHRWRT(ILINE(I),0,14,1)
  320 CONTINUE
      CALL LOCATE(22,36,IERR)
      CALL OKREPLY(REPLY,RTNFLAG)
      IF (REPLY.EQ.'4F')THEN
         GO TO 9999   
      ELSE IF (REPLY.EQ.'N') THEN
         CALL CLRMSG(3)
         CALL CLRMSG(4)
         GO TO 100
      ELSE
C
C    TRANSLATE THE DECIMAL LINE VALUES TO THE CHARACTERS THEY REPRESENT
C
         DO 330 I = 1,11
            LINCHR(I) = CHAR(ILINE(I))
  330    CONTINUE            
      END IF
C
C   WRITE THE PRINTER CONTROL CODES AND LINE CHARACTERS TO DISK
C
      WRITE(7,'(A1,A1)')FIELD(1),FIELD(9)
      WRITE(7,'(7(8I3.3))') ((PRTCODE(N,M),M=1,8),N=1,7)
      WRITE(7,'(11A1)') (LINCHR(I),I=1,11)  
C      
      CLOSE(7)
      CALL LOCATE(24,0,IERR)

 9999 RETURN
      END  
