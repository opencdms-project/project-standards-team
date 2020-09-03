$STORAGE:2

      SUBROUTINE PARSE1(STRING,LENSTR,NBRITEM,LNGITEM,ITEMS,RTNCODE)
C
C  PARSE A STRING OF ITEMS SEPARATED BY COMMAS AND PUT EACH ITEM
C  FOUND INTO THE ARRAY ITEMS
C
C     PASSED VARIABLES  
C         STRING....THE STRING TO BE PARSED 
C         LENSTR....THE NUMBER OF CHARACTERS IN STRING
C         NBRITEM...THE EXPECTED NUMBER OF ITEMS TO BE FOUND
C         LNGITEM...THE MAXIMUM LENGTH OF AN ITEM
C         ITEMS.....THE CHARACTER ARRAY TO HOLD THE PARSED OUTPUT
C                   (CHAR*LNGITEM DIMENSIONED NBRITEM)
C         RTNCODE ...VALUES:
C                      0=OK;
C                      1=ITEM TOO LONG TO FIT IN ARRAY,ITEMS;
C                      2=FEWER ITEMS FOUND THAN IN NBRITEM;
C                      3=MORE ITEMS FOUND THAN IN NBRITEM;
C                      4=CONDITIONS 1 & 2;
C                      5=CONDITIONS 1 & 3
C  
C------  PASSED VARIABLES  -------------- 
C  
      INTEGER*2     NBRITEM,LNGITEM
      CHARACTER     STRING*(*)
      CHARACTER*1   ITEMS(LNGITEM,NBRITEM),RTNCODE
C
C------  LOCAL VARIABLES  -----------------
C  
      LOGICAL      TRUNCATE
      CHARACTER*1  BLANK, COMMA
      INTEGER*2    L, LPOS
      DATA         BLANK/' '/, COMMA/','/
C
      TRUNCATE=.FALSE.
      L   =1
      LPOS=0      
      RTNCODE='0'
      DO 20 I=1,NBRITEM
      DO 20 II=1,LNGITEM
         ITEMS(II,I)=BLANK
   20 CONTINUE
      DO 50 J=1,LENSTR
         IF(STRING(J:J).NE.BLANK) THEN
            K=J
            DO 30 JJ=2,LENSTR
               JEND=LENSTR+2-JJ
               IF(STRING(JEND:JEND).NE.BLANK) THEN
                  KEND=JEND
                  GO TO 100
               END IF
   30       CONTINUE        
         END IF
   50 CONTINUE
      GO TO 300
C----------------------------------------------------------------------
C       START SEARCH AT FIRST NON-BLANK CHARACTER
C----------------------------------------------------------------------
  100 DO 200 J=K,KEND
         IF(LPOS.EQ.0.AND.STRING(J:J).EQ.BLANK) THEN
            GO TO 200
         END IF
         IF(STRING(J:J).EQ.COMMA) THEN
            L=L+1
            LPOS=0
            TRUNCATE=.FALSE.
            IF(L.GT.NBRITEM) THEN
               IF(RTNCODE.EQ.'1') THEN
                  RTNCODE='5'
               ELSE
                  RTNCODE='3'
               END IF
               RETURN
            END IF
         ELSE
            IF(.NOT.TRUNCATE) THEN
               LPOS=LPOS+1
               IF(LPOS.GT.LNGITEM) THEN
                  RTNCODE='1'
                  TRUNCATE=.TRUE.
               ELSE
                  ITEMS(LPOS,L)=STRING(J:J)
               END IF
            END IF
         END IF
  200 CONTINUE
  300 IF(L.NE.NBRITEM) THEN
         IF(RTNCODE.EQ.'1') THEN
            RTNCODE='4'
         ELSE
            RTNCODE='2'
         END IF
      END IF
      RETURN
      END
