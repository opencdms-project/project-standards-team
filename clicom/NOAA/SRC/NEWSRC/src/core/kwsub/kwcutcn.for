
C
C
C
      SUBROUTINE KWCUTCN(ICUT,W,IX,IY,IUNIT)
c
      INTEGER*2  IX,IY,IUNIT
      DIMENSION  W(IX,IY)
      INTEGER*1  ICUT(IX,IY)
CCC3/7/91      REAL*4     ICUT(IX,IY)
      COMMON /KWCHOP/ M,TUX(4,1500)
C
C
C***     TESTING CUTCON 2/4/91
         DO 361 I=1,IY
         DO 362 J=1,IX
               ICUT(J,I) = 0
 362     CONTINUE
 361     CONTINUE
C
         KWOUNT = 0
C
         IXINC = 2
         IYINC = 2
C
         DO 100 IY1=1,IY
         DO 200 IX1=1,IX
            IXSTR = IX1 - IXINC
            IXEND = IX1 + IXINC
            IF (IXSTR.LT.1)  IXSTR = 1
            IF (IXEND.GT.IX) IXEND = IX
            IYSTR = IY1 - IYINC
            IYEND = IY1 + IYINC
            IF (IYSTR.LT.1) IYSTR = 1
            IF (IYEND.GT.IY) IYEND = IY
            ICHK = 0
         DO 300 IYLP = IYSTR,IYEND
         DO 400 IXLP = IXSTR,IXEND
            IF (W(IXLP,IYLP).EQ.1.0) ICHK = 1
 400     CONTINUE
 300     CONTINUE
         IF (ICHK.EQ.1) THEN
            ICUT(IX1,IY1) = 0
         ELSE
            KWOUNT = KWOUNT + 1
            ICUT(IX1,IY1) = 1
         ENDIF
 200     CONTINUE
 100     CONTINUE
C
C
         M = 0
         AA = KWOUNT
         CALL KWPARM(47,AA)
         CALL KCUTGD(ICUT,IX,IY,KWOUNT,IUNIT)
C
C
      RETURN
      END
C
      SUBROUTINE KCUTGD(XXOUT,K,L,MMAX,NUMFIL)
c
      INTEGER*2  K,L,NUMFIL
      INTEGER*1  XXOUT(K,L)
CCCC3/7       DIMENSION  XXOUT(K,L)
      DIMENSION IOFLO(2)
      COMMON /KWIGCN/ ZCON(55)
      COMMON /KWCHOP/ M,TUX(4,1500)
c
           HI = ZCON(10)
          WID = ZCON(11)
CC           HI = L
CC          WID = K
C
      DO 60 I = 1,2
      IF(I.EQ.1) GO TO 48
      IPOWRH = 5 - IPTEN
      PHI = WID
      GO TO 49
   48 PHI = HI
   49 IPTEN = 0
   50 IF(PHI.LT.10.) GO TO 52
      PHI = PHI * .1
      IPTEN = IPTEN + 1
      GO TO 50
   52 IF(IPTEN.NE.0) GO TO 60
   54 IF(PHI.GE.1.) GO TO 60
      PHI = PHI * 10.
      IPTEN = IPTEN - 1
      GO TO 54
   60 CONTINUE
      IPOWRW = 5 - IPTEN
      IPOWR = MAX0(IPOWRH,IPOWRW)
      POWR = 10. ** IPOWR
      DEL = 15. * 10. ** (-1-IPOWR)
      SWG = WID / K
      SHG = HI /  L
      I1 = 1
      I2 = 2
      I3 = 3
      I4 = 4
      DO 100 N = 1,2
      IOFLO(N) = 0
      I = 0
      J = 0
      DO 5 NN = 1,MMAX
      DO 5 III = 1,4
    5 TUX(III,NN) = 0.
      M = 0
      MFN = 0
      MLN = 0
      MFO = 0
      MLO = 0
   15 GO TO (20,22),N
   20 J = J + 1
      IF(J.GT.L) GO TO 40
      I = 0
      GO TO 24
   22 I = I + 1
      IF(I.GT.K) GO TO 46
      J = 0
   24 IF(IOFLO(N).EQ.1) GO TO 16
      MFO = MFN
      MLO = M
      MFN = M + 1
   10 GO TO (30,32),N
   30 I = I + 1
      IF(I.GT.K) GO TO 8
      GO TO 34
   32 J = J + 1
      IF(J.GT.L) GO TO 8
   34 CONTINUE
      IF(XXOUT(I,J)) 1,10,1
    1 M = M + 1
      IF(M.GT.MMAX) GO TO 6
      TUX(2,M) = SWG * I
      TUX(1,M) = TUX(2,M) - SWG
      TUX(4,M) = SHG * J
      TUX(3,M) = TUX(4,M) - SHG
      DO 4 II  = 1,4
      ITUX = TUX(II,M) * POWR + .5
      TUX(II,M) = ITUX / POWR
    4 CONTINUE
      IF(M.EQ.1) GO TO 10
      M1 = M -1
      IF(ABS(TUX(I3,M)-TUX(I3,M1)).GT.DEL) GO TO 10
      IF(ABS(TUX(I2,M1)-TUX(I1,M)).GT.DEL) GO TO 10
      TUX(I2,M1) = TUX(I2,M)
      M = M1
      GO TO 10
    6 IOFLO(N) = 1
      M = MMAX
    8 MLN = M
      IF((J.EQ.1.AND.N.EQ.1).OR.(I.EQ.1.AND.N.EQ.2)) GO TO 15
      IF(MLN.LT.MFN) GO TO 15
      IF(ABS(TUX(I4,MLO)-TUX(I3,MFN)).GT.DEL) GO TO 15
      MM = MLN
      MFNN = MFN
      DO 14 II = MFO,MLO
      DO 13 JJ = MFN,MLN
   11 IF(JJ.GT.MM) GO TO 13
      IF(ABS(TUX(I1,JJ)-TUX(I1,II)).GT.DEL) GO TO 13
      IF(ABS(TUX(I2,JJ)-TUX(I2,II)).GT.DEL) GO TO 13
      IF(ABS(TUX(I3,JJ)-TUX(I4,II)).GT.DEL) GO TO 13
      TUX(I4,II) = TUX(I4,JJ)
      IF(II.LT.MFNN) MFNN = II
      MM = MM - 1
      DO 12 NN = JJ,MM
      DO 12 III = 1,4
   12 TUX(III,NN) = TUX(III,NN+1)
      GO TO 11
   13 CONTINUE
   14 CONTINUE
      M = MM
      IF((IOFLO(N).EQ.1).AND.(M.LT.MMAX)) IOFLO(N) = 0
      MFN = MFNN
      IF(MFN.GT.M) MFN = M
      GO TO 15
   16 IF(N.EQ.2) GO TO 46
   40 IT = I1
      I1 = I3
      I3 = IT
      IT = I2
      I2 = I4
      I4 = IT
C
C -------------------------------------------------------------
C
ccccc      WRITE (*,*) 'SUB CUTCON NN = ',M
C
CCCCC       WRITE (97) ((TUX(III,NN),III=1,4),NN=1,M) 

      OPEN (NUMFIL,FILE='SCRATCH.WRK',ACCESS='SEQUENTIAL')
      DO 1000 III = 1,4
      DO 1001  NN = 1,M
         WRITE  (NUMFIL,*) TUX(III,NN)
1001  CONTINUE
1000  CONTINUE
C
      CLOSE(NUMFIL)
C
C -------------------------------------------------------------
C
      MSAVE = M
  100 CONTINUE
   46 CONTINUE
      IF(MSAVE-M) 70,75,72
   75 IF(IOFLO(1).EQ.0) GO TO 70
      IF(IOFLO(2).EQ.0) GO TO 72
      PRINT 76
   76 FORMAT(91H0VALUE OF MAX IS TOO SMALL IN CALL TO CUTCON.  INCREASE
     1MAX OR DECREASE SIZE OF ARRAY XOUT.)
   70 M = MSAVE
C
C -------------------------------------------------------------
C
      OPEN (NUMFIL,FILE='SCRATCH.WRK',ACCESS='SEQUENTIAL',
     +STATUS='OLD')
      DO 1002 III = 1,4
      DO 1003  NN = 1,M
         READ  (NUMFIL,*) TUX(III,NN)
1003  CONTINUE
1002  CONTINUE
C
C
      CLOSE(NUMFIL)
C
C -------------------------------------------------------------
C
   72 ZM = M
      ZCON(47) = ZM
      ZCON(55) = 0.
      DO 90 NN = 1,M
      IF(TUX(1,NN).LE.DEL) TUX(1,NN) = 0.
      IF(ABS(TUX(2,NN)-WID).LE.DEL) TUX(2,NN) = WID
      IF(TUX(3,NN).LE.DEL) TUX(3,NN) = 0.
      IF(ABS(TUX(4,NN)-HI).LE.DEL) TUX(4,NN) = HI
   90 CONTINUE
C
      RETURN
      END
