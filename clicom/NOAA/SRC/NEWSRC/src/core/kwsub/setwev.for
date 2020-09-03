       SUBROUTINE SETWEV (PARMS,T1,W1,IXSIZ,IYSIZ,NUM)
       DIMENSION T1(IXSIZ,IYSIZ), W1(IXSIZ,IYSIZ), PARMS(1)
       DIMENSION VAL(3,240)
C
                   XL =  1.
                   XR = REAL (IXSIZ)
                   YB =  1.
                   YT = REAL (IYSIZ)
                 dist =  PARMS(1)
                  ilr =  PARMS(2)
                  ibt =  PARMS(3)
                iweav =  PARMS(4)
                 idec =  PARMS(5)
                ifine =  PARMS(6)
                centr =  PARMS(7)
               icheck =  PARMS(8)
                 NUMV =  0
C
        CALL kwintp (VAL,NUMV,T1,W1,IXSIZ,IYSIZ,XL,XR,YB,YT,DIST,
     +               ILR,IBT,IWEAV,IDEC,IFINE,CENTR,ICHECK,NUM)
c
        RETURN
        END
