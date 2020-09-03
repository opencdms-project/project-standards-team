$STORAGE:2
      SUBROUTINE  NAMECAPS(GRAFNAME)
C------------------------------------------------------------------------------
C     SWITCH LOWER CASE LETTERS IN A GRAPHICS FILE NAME TO UPPER CASE
C
C     INPUT AND OUTPUT ARGUMENTS:
C
C     GRAFNAME  CHARACTER  FILE NAME WITH LOWER CASE LETTERS.  REPLACED 
C                          INLINE WITH THE UPPER CASE LETTERS
C
C------------------------------------------------------------------------------
      CHARACTER*(*) GRAFNAME
      INTEGER*2     NBCHR, NCHAR
C
      NBCHR = LEN(GRAFNAME)
      DO 10 I1=1,NBCHR
         IF (GRAFNAME(I1:I1).GE.'a'.AND.GRAFNAME(I1:I1).LE.'z') THEN
             NCHAR = ICHAR(GRAFNAME(I1:I1))
             NCHAR = NCHAR - 32
             GRAFNAME(I1:I1) = CHAR(NCHAR)
         ENDIF
  10  CONTINUE
      RETURN
      END
