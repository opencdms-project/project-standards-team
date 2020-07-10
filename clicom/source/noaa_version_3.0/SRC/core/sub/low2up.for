$STORAGE:2
      SUBROUTINE LOW2UP(LINE)
C
C       ** CONVERT LINE OF CHARACTERS TO UPPERCASE
C      
      CHARACTER*(*) LINE
      CHARACTER*1  INCHAR
      LINLEN = LEN(LINE)
      DO 20 I=1,LINLEN
         INCHAR = LINE(I:I)
         IF (INCHAR.GE.'a'.AND.INCHAR.LE.'z') THEN
             NCHAR = ICHAR(INCHAR)
             NCHAR = NCHAR - 32
             INCHAR = CHAR(NCHAR)
             LINE(I:I)=INCHAR
         END IF
   20 CONTINUE
      RETURN
      END
