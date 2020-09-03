C
C
C
        SUBROUTINE KWNSTR (XB,STRG2,J)
        CHARACTER*256 STRING,STRG2
C
C      **** INPUT PARAMETERS  XB = FLOATING POINT NUMBER
C
C      **** OUTPUT PARAMETRS  STRG2 = STRING BACK
C                                 J = NUMBER CHARAC BACK
C
C
         CALL WDBCON (XB,XX)
         XX = XX / 10000.0
C
        STRING = '                    '
        STRG2  = '                    '
        WRITE (STRING,34) XX
        READ  (STRING,33) STRG2
C
 33     FORMAT(A256)
 34     FORMAT(F12.4)
C
          J = 0
        DO 500 I=1,256
        IF (STRG2(I:I) .EQ. ' ') GOTO 500
            J = J + 1
            STRG2(J:J) = STRG2(I:I)
500     CONTINUE
C
        DO 505 I=J,256
505        STRG2(I:I) = ' '
C
        J = J - 1
        RETURN
        END
C
C
C
        SUBROUTINE KWNST2 (XB,STRG2,J)
        CHARACTER*256 STRING,STRG2
C
C      **** INPUT PARAMETERS  XB = FLOATING POINT NUMBER
C
C      **** OUTPUT PARAMETRS  STRG2 = STRING BACK
C                                 J = NUMBER CHARAC BACK
C
C
         XX = XB
C
        STRING = '                    '
        STRG2  = '                    '
        WRITE (STRING,34) XX
        READ  (STRING,33) STRG2
C
 33     FORMAT(A256)
 34     FORMAT(F12.4)
C
          J = 0
        DO 500 I=1,256
        IF (STRG2(I:I) .EQ. ' ') GOTO 500
            J = J + 1
            STRG2(J:J) = STRG2(I:I)
500     CONTINUE
C
        DO 505 I=J,256
505        STRG2(I:I) = ' '
C
        J = J - 1
        RETURN
        END
