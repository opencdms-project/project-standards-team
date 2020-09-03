C
C
      SUBROUTINE KWWEVR (T1,W1,ICUT,IXSZ,IYSZ,NUMM,IFLG,IXCUT,IYCUT,IER)
C ** REVISION ** (JML 07-30-93)      
C    ADD IER TO ARGUMENTS; DECLARE IER I*2
       INTEGER*1 ICUT
       INTEGER*2 IXSZ,IYSZ,NUMM,IFLG,IXCUT,IYCUT,ICTFLG,IER
       DIMENSION ICUT(IXSZ,IYSZ)
       DIMENSION T1(IXSZ,IYSZ), W1(IXSZ,IYSZ)
C      kw11/3/91 changed VAL(3,400) to val(3,1) 
       COMMON /KWSVR2/  VAL(3,60)
       COMMON /KWCTCK/ ICTFLG
C
       ICTFLG = IFLG
C
       IXSIZ = IXSZ
       IYSIZ = IYSZ
       NUM   = NUMM

                   XL =  1.
                   XR = REAL (IXSIZ)
                   YB =  1.
                   YT = REAL (IYSIZ)
C
C                 dist =  PARMS(1)
C                  ilr =  PARMS(2)
C                  ibt =  PARMS(3)
C                iweav =  PARMS(4)
C                 idec =  PARMS(5)
C                ifine =  PARMS(6)
C                centr =  PARMS(7)
C               icheck =  PARMS(8)
                 NUMV =  0
C
                 IF (IXSIZ.GT.IYSIZ) THEN
                     DIST = IYSIZ / 2.
                 ELSE
                     DIST = IXSIZ / 2.
                 ENDIF
C
                  ilr =  0.0
                  ibt =  0.0
                iweav =  1.0
                 idec =  2.0
                ifine =  2
                centr =  -1.0
               icheck =  0
                 NUMV =  0
C
CCC       ICHECK = 1    DIMENSION OF VAL MUST BE AT LEAST 2 TIMES IH
CCC       ICHECK = 2    DIMENSION LESS THAN 11
CCC       ICHECK = 3    LAT/LON OUTSIDE RANGE OF KWINTP
CCC       ICHECK = 4    RECEIVED NO DATA VALUES FOR ARRAY
CCC       ICHECK = 5    INSUFFICENT DATA TO FILL ARRAY
C
        CALL KWINTP (VAL,NUMV,T1,W1,IXSIZ,IYSIZ,XL,XR,YB,YT,DIST,
     +               ILR,IBT,IWEAV,IDEC,IFINE,CENTR,ICHECK,NUM)
C
         IF (IFLG.NE.0) THEN
         IXC = IXCUT
         IYC = IYCUT
         CALL KWCUTCN(ICUT,W1,IXSZ,IYSZ,NUMM,IXC,IYC)
         ENDIF
C
C ** REVISION ** (JML 07-30-93) SET IER TO ICHECK
        IER = ICHECK  
        RETURN
        END
