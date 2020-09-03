C
C
C
       SUBROUTINE KWMARK (TIC,XX,YY)
C
C      ***  INPUT PARAMETERS   TIC = LENGTH IN INCHES 
C                               XX = LONGITUDE VALUE
C                               YY = LATITUDE  VALUE
C
c
       X1 = XX
       Y1 = YY
       IP = -3
       CALL KWTRAN (X1,Y1,IP)
       CALL SCLCON (X1,Y1,XNEW,YNEW)
C
       IP3 = 3
       IP2 = 2
       XX1 = XNEW - TIC
       XX2 = XNEW + TIC
       XX3 = XNEW
       XX4 = XNEW
       YY1 = YNEW
       YY2 = YNEW
       YY3 = YNEW - TIC
       YY4 = YNEW + TIC
C
        CALL KWBUFF (XX1,YY1,IP3)
        CALL KWBUFF (XX2,YY2,IP2)
        CALL KWBUFF (XX3,YY3,IP3)
        CALL KWBUFF (XX4,YY4,IP2)
C
        RETURN
        END
