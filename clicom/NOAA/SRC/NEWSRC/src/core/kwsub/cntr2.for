C
C
C
      SUBROUTINE KWINTP(VAL,NUMV,AREA,WAREA,IH,IV,XL,XR,XB,XT,DIST,
     C  ILR,IBT,IWEAV,IDEC,IFINE ,CENTR,ICHEK,NUMFIL)
       INTEGER*2 IFLAG,NFILE,I,J
       DIMENSION VAL(3,1),AREA(IH,IV),    WAREA(IH,IV)
       DIMENSION WGT(15,7)
c
      DATA WGT/
     C .23, .44, .61, .75, .86, .94, .98,1.00, .98, .94, .86, .75, .61,
     C .44, .23, .22, .42, .59, .73, .84, .92, .97, .98, .97, .92, .84,
     C .73, .59, .42, .22, .17, .38, .55, .69, .80, .88, .92, .94, .92,
     C .88, .80, .69, .55, .38, .17, .09, .30, .47, .61, .72, .80, .84,
     C .86, .84, .80, .72, .61, .47, .30, .09,0.00, .19, .36, .50, .61,
     C .69, .73, .75, .73, .69, .61, .50, .36, .19,0.00,0.00, .05, .22,
     C .36, .47, .55, .59, .61, .59, .55, .47, .36, .22, .05,0.00,0.00,
     C0.00, .05, .19, .30, .38, .42, .44, .42, .38, .30, .19, .05,0.00,
     C0.00/
      DATA ICALL/0/
c
             NFILE = NUMFIL
                        icall=0
                         irec=0
C
C KW1C TO SEE IF WORK AREA IS SUFF LARGE
C
      IF((IH .GT.10) .AND. (IV .GT.10)) GO TO 10
CCCC      PRINT 5
CCCKW 5     FORMAT(' DIMENSIONS IS LESS THAN 11.')
       ICHEK = 2
      DO 7 J=1,IV
      DO 7 I=1,IH
    7 AREA(I,J)=99999.
      RETURN
10    CONTINUE
      IF(ICALL)15,12,15
12    NVAL = 3*NUMV
      NIH = 2*IH
      NIH3=NIH/3.
      ICALL = 1
      IF(NIH .LE. NVAL) GO TO 15
CCCCCCCC      PRINT 14
CCCCCCCC14    FORMAT(' WARNING: THE DIMENSION OF VAL MUST  LEAST 2 TIMES IH ')
15    CONTINUE
C
C CALCULATE TRANSLATION FROM DEGREES TO UNITS
C
      DH = (XR-XL)/(IH-1)
      DV = (XT-XB)/(IV-1)
      ADDH = XL - (DH/2)
      ADDV = XB - (DV/2)
      NDIST = (DIST+1.) / ( (ABS(DV) + ABS(DH) ) * .5)
C
C PLACE VALUES IN ARRAY AND COUNTER ARRAY
C
      L = IH*IV
      IF(NUMV .NE. 0) GO TO 19
      XMIN = 0
      DO 16 J=1,IV
      DO 16 I=1,IH
      IF(WAREA(I,J).EQ. 0) GO TO 16
      IF(AREA(I,J) .LE. XMIN) XMIN=AREA(I,J)-1
16    CONTINUE
      IF(XMIN .EQ. 0) GO TO 21
      DO 18 J=1,IV
      DO 18 I=1,IH
   18 AREA(I,J)=AREA(I,J)-XMIN
   21 DO 17 J=1,IV
      DO 17 I=1,IH
       IF(WAREA(I,J) .EQ. 0) AREA(I,J)=0
17    CONTINUE
      GO TO 110
   19 DO 20 J=1,IV
      DO 20 I=1,IH
      AREA(I,J)=0
   20 WAREA(I,J)=0
      XMIN = 0
      DO 50 I = 1,NUMV
      IF(VAL(3,I) .LE. XMIN) XMIN =VAL(3,I)- 1
50    CONTINUE
      IF(XMIN .EQ. 0) GO TO 60
      DO 55 I = 1,NUMV
55    VAL(3,I) = VAL(3,I) - XMIN
60    DO 100 I = 1,NUMV
      LAT = (VAL(1,I) - ADDV)/DV
      LAT = LAT + 1
      IF(LAT .LT. 1 .OR. LAT .GT. IV) GO TO 70
      LONG = (VAL(2,I) - ADDH)/DH
      LONG = LONG + 1
      IF(LONG .LT. 1 .OR. LONG .GT. IH) GO TO 70
      GO TO 80
   70 XV = VAL(3,I) + XMIN
CCC      PRINT 71, I,VAL(1,I),VAL(2,I),XV
CCCKW   71 FORMAT(' VALUE NUMBER ',I5,' WITH LATITUDE ',F8.2,',LONGITUDE ',F8
CCCKW     C.2,/, ' AND VALUES ',F8.3,' WAS OUTSIDE THE RANGE YOU SENT KWINTP.
CCCKW     C')
      ICHEK = 3
      GO TO 100
80    CONTINUE
      AREA(LONG,LAT) = VAL(3,I) + AREA(LONG,LAT)
      WAREA(LONG,LAT) = WAREA(LONG,LAT) + 1
100   CONTINUE
      IF(XMIN .EQ. 0) GO TO 110
      DO 105 I = 1,NUMV
105   VAL(3,I) = VAL(3,I)  + XMIN
C
C KW1C TO SEE IF LEFT AND RIGHT ARE THE SAME
C IF SO MOVE RIGHT TO LEFT
C
  110 IF(ILR)120,200,120
120   DO 130 I = 1,IV


cccc      WRITE (*,*) 'KWINTP ',I,IV,IH

        AXX1 = AREA(1,I)
        AXX2 = AREA(IH,I)

        AREA(1,I) = AXX1 +AXX2
CCC      AREA(1,I) = AREA(1,I) + AREA(IH,I)
        BXX1 = WAREA(1,I)
        BXX2 = WAREA(IH,I)
        WAREA(1,I) = BXX1 + BXX2
CCC      WAREA(1,I) = WAREA(1,I) + WAREA(IH,I)
130   CONTINUE
      DO 140 I = 1,IV
      AREA(IH,I) = 0
      WAREA(IH,I) = 0
140   CONTINUE
C
C KW1C BOTTOM  AND TOP FOR THE SAME SITUATION
C
  200 IF(IBT)220,290,220
220   DO 230 J = 1,IH
      AREA(J,1) = AREA(J,1) + AREA(J,IV)
230   WAREA(J,1) = WAREA(J,1) + WAREA(J,IV)
      DO 240 J = 1,IH
      AREA(J,IV) = 0
240   WAREA(J,IV) = 0
C
C TAKE AVERAGE
C
290   DO 300 J = 1,IV
      DO 300 I = 1,IH
      IF(WAREA(I,J) .EQ. 0) GO TO 300
      AREA(I,J) = AREA(I,J)/WAREA(I,J)
300   CONTINUE
c
c
c         {  write  array to disk for space  }
c
c       {  open unit NUMFIL for extra work space    } 
c
       CALL KWOPSCR(NFILE,IFLAG)
CC1/11/91      open (NUMFIL,access='direct',form='unformatted',recl=4,
CC1/11/91     +STATUS='SCRATCH')
c
 97    format (f10.2)
               irec=0
         do 5100 ik=1,ih
         do 5101 jk=1,iv
            irec=irec+1
cccccc      workw1(ik,jk)=warea(ik,jk)  
            write (NUMFIL,rec=irec) warea(ik,jk)
5101      continue
5100      continue
c
c      WRITE(97)((WAREA(IK,JK),IK=1,IH),JK=1,IV)
c
c
C
C BEGIN PROCESS OF FILLING BASE LINES
C DO LEFT-RIGHT FIRST
C
      IF(ILR .EQ. 0) GO TO 310
      LEFT = 1
      IRIGHT = IH - 1
      GO TO 370
C
C FIND LEFT BORDER
C
310   DO 320 I = 1,IH
      DO 320 J = 1,IV
      IF(WAREA(I,J) .NE. 0) GO TO 330
320   CONTINUE
325   CONTINUE
CCC   PRINT 326
      ICHEK = 4
CCCKW  326 FORMAT(' RECEIVED NO DATA VALUES FOR ARRAY')
      GO TO 980
330   LEFT = I
C
C FIND RIGHT BORDER
C
      DO 340 II = 1,IH
      I = IH + 1 - II
      DO 340 J = 1,IV
      IF(WAREA(I,J) .NE. 0) GO TO 350
340   CONTINUE
      GO TO 325
350   IRIGHT = I
      IF( LEFT .EQ. 1) GO TO 360
C
C FILL IN LEFT SIDE
C
      IWHERE =  LEFT - 1
      DO 355 I = 1,IWHERE
      DO 355 J = 1,IV
355   AREA(I,J) = 99999.
C
360   IF(IRIGHT .EQ. IH) GO TO 370
C
C FILL IN RIGHT SIDE
C
      IWHERE = IRIGHT + 1
      DO 365 I = IWHERE,IH
      DO 365 J = 1,IV
365   AREA(I,J) = 99999.
370   CONTINUE
      IF(IBT .EQ. 0) GO TO 410
      IBOTTO=1
      ITOP = IV - 1
      GO TO 470
C
C FIND BOTTOM
C
410   DO 420 J = 1,IV
      DO 420 I = LEFT,IRIGHT
      IF(WAREA(I,J) .NE. 0) GO TO 430
420   CONTINUE
      GO TO 325
  430 IBOTTO=J
C
C FIND TOP
C
      DO 440 JJ = 1,IV
      J = IV + 1 - JJ
      DO 440 I = LEFT,IRIGHT
      IF(WAREA(I,J) .NE. 0) GO TO 450
440   CONTINUE
      GO TO 325
450   ITOP = J
C
C FILL BOTTOM
C
      IF(IBOTTO .EQ. 1) GO TO 460
      IWHERE=IBOTTO-1
      DO 455 J = 1,IWHERE
      DO 455 I = LEFT,IRIGHT
455   AREA(I,J) = 99999.
460   IF(ITOP .EQ. IV) GO TO 470
      IWHERE = ITOP + 1
      DO 465 J = IWHERE,IV
      DO 465 I = LEFT,IRIGHT
465   AREA(I,J) = 99999.
470   CONTINUE
C
C BASE LINE FILL IN
C
      DO 480 J=1,IV
      DO 480 I=1,IH
  480 WAREA(I,J)=0
      IF(ILR .EQ. 1) GO TO 590
C
C FILL LEFT THEN RIGHT
C
      IWHERE = LEFT
      MULT = 1
  483 DO 550 J=IBOTTO,ITOP
      IF(AREA(IWHERE,J) .EQ. 0) GO TO 484
      WAREA(IWHERE,J) = AREA(IWHERE,J)
      GO TO 550
484   TOT = 0
      WT = 0
      DO 510 LL= 1,15
      M = J - 8 + LL
      IF(M .GE. IBOTTO) GO TO 485
      IF(IBT .EQ. 0) GO TO 510
      M = M + IV - 1
      GO TO 490
485   IF(M .LE. ITOP) GO TO 490
      IF(IBT .EQ. 0) GO TO 510
      M = M - IV + 1
490   DO 500 K = 1,7
      N = IWHERE + MULT*(K-1)
      IF((MULT.EQ.1).AND.(N.GT.IRIGHT)) GO TO 500
      IF((MULT.EQ.(-1)).AND.(N.LE.LEFT)) GO TO 500
      IF(AREA(N,M) .EQ. 0) GO TO 500
      TOT = TOT + AREA(N,M)*WGT(LL,K)
      WT = WT + WGT(LL,K)
500   CONTINUE
510   CONTINUE
      IF(WT .EQ. 0) GO TO 550
      WAREA(IWHERE,J) = TOT/WT
550   CONTINUE
      IF(IWHERE .EQ. IRIGHT) GO TO 590
      IWHERE = IRIGHT
      MULT = -1
      GO TO 483
590   CONTINUE
      IF(IBT .EQ. 1) GO TO 800
C
C FILL BOTTOM THEN TOP
C
      IWHERE=IBOTTO
      MULT = 1
683   DO 750 I = LEFT,IRIGHT
C
      IF(AREA(I,IWHERE) .EQ. 0) GO TO 684
      WAREA(I,IWHERE) = AREA(I,IWHERE)
      GO TO 750
684   TOT = 0
      WT = 0
      DO 710 LL= 1,15
      M = I - 8 + LL
      IF(M .GE. LEFT) GO TO 685
      IF(ILR .EQ. 0) GO TO 710
      M = M + IH - 1
      GO TO 690
685   IF(M .LE. IRIGHT) GO TO 690
      IF(ILR .EQ. 0) GO TO 710
      M = M - IH + 1
690   DO 700 K = 1,7
      N = IWHERE + MULT*(K-1)
      IF((MULT.EQ.1).AND.(N.GT.ITOP)) GO TO 700
      IF((MULT.EQ.(-1)).AND.(N.LE.IBOTTO)) GO TO 700
      IF(AREA(M,N) .EQ. 0) GO TO 700
      TOT = TOT + AREA(M,N)*WGT(LL,K)
      WT = WT + WGT(LL,K)
700   CONTINUE
710   CONTINUE
      IF(WT .EQ. 0) GO TO 750
      WAREA(I,IWHERE) = TOT/WT
750   CONTINUE
      IF(IWHERE .EQ. ITOP) GO TO 800
      IWHERE = ITOP
      MULT = -1
      GO TO 683
800   CONTINUE
      IF(ILR .EQ. 1) GO TO 820
      DO 810 J= IBOTTO, ITOP
      AREA(LEFT,J) = WAREA(LEFT,J)
810   AREA(IRIGHT,J) = WAREA(IRIGHT,J)
820   CONTINUE
      IF(IBT .EQ. 1) GO TO 840
      DO 830 I = LEFT,IRIGHT
      AREA(I,IBOTTO)=WAREA(I,IBOTTO)
830   AREA(I,ITOP) = WAREA(I,ITOP)
840   IF(ILR .EQ. 1) GO TO 860
      DO 850 J = IBOTTO,ITOP
      IF(AREA(LEFT,J) .EQ. 0) GO TO 470
      IF(AREA(IRIGHT,J) .EQ. 0) GO TO 470
850   CONTINUE
860   IF(IBT .EQ. 1) GO TO 900
      DO 870 I = LEFT,IRIGHT
      IF(AREA(I,IBOTTO) .EQ. 0) GO TO 470
      IF(AREA(I,ITOP) .EQ. 0) GO TO 470
870   CONTINUE
900   CONTINUE
      CALL       KW2D(AREA,WAREA,IH,IV,LEFT,IRIGHT,IBOTTO,ITOP,
     C NDIST,ILR,IBT,IWEAV,*980,*990)
980   ICHEK  = 1
      DO 985 J=1,IV
      DO 985 I=1,IH
      IF(AREA(I,J) .EQ. 0) AREA(I,J)= 99999.
985   CONTINUE
      GO TO 1030
990   ICHEK  = 0
c
c       { close unit NUMFIL and open again }
c     
                   close (unit=NUMFIL)
          CALL KWOPSCR(NFILE,IFLAG)
cc1/11/91        open (NUMFIL,file='scratch.wrk',access='direct',
cc1/11/91     +  form='unformatted',recl=4)
c
              irec=0
         do 5102 ik=1,ih
         do 5103 jk=1,iv
            irec=irec+1
cccc                    warea(ik,jk)=workw1(ik,jk)
           read (NUMFIL,rec=irec) warea(ik,jk)
5103      continue
5102      continue
c      REWIND 97
c      READ(97)((WAREA(IK,JK),IK=1,IH),JK=1,IV)
C
c
                   close (unit=NUMFIL)
          CALL KWOPSCR(NFILE,IFLAG)    
cc1/11/91        open (NUMFIL,file='scratch.wrk',access='direct',
cc1/11/91     +  form='unformatted',recl=4)
c
              irec=0
         do 5104 ik=1,3
         do 5105 jk=1,nih3
            irec=irec+1
cccccc                  workw2(ik,jk)=val(ik,jk)
         write (NUMFIL,rec=irec) val (ik,jk)
5105      continue
5104      continue
c
c
c                 REWIND 97
c         WRITE(97)((VAL(IK,JK),IK=1,3),JK=1,NIH3)
C
      CALL KW1G (VAL,AREA,WAREA,IH,IV,LEFT,IRIGHT,IBOTTO,ITOP,ILR,
     C IBT,IDEC,IFINE,CENTR)
c
c
                   close (unit=NUMFIL)
          CALL KWOPSCR(NFILE,IFLAG)
cc1/11/91        open (NUMFIL,file='scratch.wrk',access='direct',
cc1/11/91     +  form='unformatted',recl=4)
c
              irec=0
         do 5106 ik=1,3
         do 5107 jk=1,nih3
            irec=irec+1
cccccc                  val (ik,jk)= workw2(ik,jk)
          read (NUMFIL,rec=irec) val (ik,jk)
5107      continue
5106      continue
c                  REWIND 97
c          READ(97)((VAL(IK,JK),IK=1,3),JK=1,NIH3)

 1030 IF(IBT)1040,1060,1040
1040  DO 1050 I = 1,IH
1050  AREA(I,IV) = AREA(I,1)
 1060 IF(ILR)1070,1090,1070
1070  DO 1080 I = 1,IV
1080  AREA(IH,I) = AREA(1,I)
1090  IF(XMIN .EQ. 0) GO TO 1100
      DO 1095 J=1,IV
      DO 1095 I=1,IH
      IF(AREA(I,J) .NE. 99999.) AREA(I,J)=AREA(I,J)+XMIN
1095  CONTINUE
1100  CONTINUE
           close (unit=NUMFIL)
c      REWIND 97
      RETURN
      end
C
C
C
C
C
C
      SUBROUTINE KW1C(BLINE,VER,VERC,JJ)
CCC      INTEGER*2 BLINE,VER,VERC
      DIMENSION  VER(1), VERC(1),BLINE(1)
      COMMON/KW2DS/ISKIP,CHANGE,MAX
      DO 340 K = 1, MAX
      IF(BLINE(K) .NE. 0) GO TO 340
      GO TO 350
340   CONTINUE
       VER(JJ) = 1
       VERC(JJ) = 0
      GO TO 400
350    VERC(JJ) = CHANGE
400   CONTINUE
      RETURN
      END
C
C
C
C
C
      SUBROUTINE KW1G (STORE,B,N,IH,IV,IL,IR,IB,IT,ILR,IBT,IDEC,IFINE,
     C CENTR)
CCCC      INTEGER*2 N
      DIMENSION STORE(IH,2),B(IH,IV),N(IH,IV),WT(2),W(9),A(9)
      REAL N

      DATA R/.70710678/
      DATA WT/6.82842712,4./
      DATA RW/4.82842712/
C WT(I) = THE SUM OF THE WEIGHTS OF THE POINTS AROUNT THE POINT (K,J)
C RW = R*WT(1)
C
      IF(IFINE .LT. 0) IFINE = 0
      ITIMES = IFINE + 1
      MTRUNC = 0
      M = 1
8     CONTINUE
      DO 40 J = IB,IT
      JD = J - 1
      JU = J + 1
      IF(JD .GE. IB) GO TO 3030
      IF(IBT)3025,3023,3025
3023  JD = 0
      GO TO 3040
3025  JD = IT
      GO TO 3040
3030  IF(JU .LE. IT) GO TO 3040
      IF(IBT)3035,3033,3035
3033  JU = 0
      GO TO 3040
3035  JU = 1
3040  CONTINUE
      L = J - 2
      LL = MOD(J,2)
      IF(LL .EQ. 0) LL = 2
      IF(L .LT.IB) GO TO 20
      DO 10 K=IL,IR
10    B(K  ,L) = STORE(K,LL)
20    DO 40 K = IL,IR
      IF(N(K,J))25,30,25
25    IF(CENTR)26,30,30
26    STORE(K,LL) = B(K,J)
      GO TO 40
30    KL = K - 1
      KR = K + 1
      IF(KL .GE. IL) GO TO 3010
      IF(ILR)3005,3003,3005
3003  KL = 0
      GO TO 3020
3005  KL = IR
      GO TO 3020
3010  IF(KR .LE. IR) GO TO 3020
      IF(ILR)3015,3013,3015
3013  KR = 0
      GO TO 3020
3015  KR = 1
3020  CONTINUE
      IF(M .EQ. 2) GO TO 32
      DO 31 I = 1,4
31    W(I) = R
      IF(KL*JD)3108,3105,3108
3105  A(1) = 0
      W(1) = 0
      GO TO 3110
3108  IF(N(KL,JD) .NE. 0) W(1) = RW
      A(1) = W(1)*B(KL,JD)
 3110 IF(KL*JU)3118,3115,3118
3115  A(2) = 0
      W(2) = 0
      GO TO 3120
3118  IF(N(KL,JU) .NE. 0) W(2) = RW
      A(2) = W(2)*B(KL,JU)
 3120 IF(KR*JD)3128,3125,3128
3125  A(3) = 0
      W(3) = 0
      GO TO 3130
3128  IF(N(KR,JD) .NE. 0) W(3) = RW
      A(3) = W(3)*B(KR,JD)
 3130 IF(KR*JU)3138,3135,3138
3135  A(4) = 0
      W(4) = 0
      GO TO 3140
3138  IF(N(KR,JU) .NE. 0) W(4) = RW
      A(4) = W(4)*B(KR,JU)
3140  CONTINUE
      GO TO 3250
32    DO 3210 I = 1,4
      A(I) = 0
3210  W(I) = 0
3250  DO 33 I = 5,8
33    W(I) = 1
      IF(KL)3308,3305,3308
3305  A(5) = 0
      W(5) = 0
      GO TO 3310
3308  IF(N(KL,J) .NE. 0) W(5) = WT(M)
      A(5) = W(5)*B(KL,J)
 3310 IF(JU)3318,3315,3318
3315  A(6) = 0
      W(6) = 0
      GO TO 3320
3318  IF(N(K,JU) .NE. 0) W(6) = WT(M)
      A(6) = W(6)*B(K,JU)
 3320 IF(JD)3328,3325,3328
3325  A(7) = 0
      W(7) = 0
      GO TO 3330
3328  IF(N(K,JD).NE. 0) W(7) = WT(M)
      A(7) = W(7)*B(K,JD)
 3330 IF(KR)3338,3335,3338
3335  A(8) = 0
      W(8) = 0
      GO TO 3340
3338  IF(N(KR,J) .NE. 0)W(8) = WT(M)
      A(8) = W(8)*B(KR,J)
3340  CONTINUE
      W(9) = WT(M)
      TOT = 0
      DO 34 I = 1,8
34    TOT = TOT + W(I)
      IF(CENTR.LT.0.) GO TO 35
      IF(N(K,J).NE.0) W(9) = CENTR * TOT
35    A(9) = B(K,J)*W(9)
      TOT = TOT + W(9)
      VALUE = 0
      DO 36 I = 1,9
      VALUE = VALUE + A(I)
   36 CONTINUE
      STORE(K  ,LL) = VALUE/TOT
40    CONTINUE
      L = IT - 1
45    LL = MOD(L,2)
      IF(LL .EQ. 0)LL = 2
      DO 50 K = IL,IR
50    B(K  ,L) = STORE(K,LL)
      IF(L .EQ. IT   ) GO TO 60
      L = IT
      GO TO 45
60    CONTINUE
      IF(M .EQ. 2) GO TO 65
      M = 2
      GO TO 8
65    CONTINUE
      IF(MTRUNC .EQ. 0) GO TO 66
      IF(MTRUNC .LT. ITIMES) GO TO 70
66    XMULT= 10**IDEC
      DIV = 1./XMULT
      DO 68 J = IB,IT
      DO 68 K = IL,IR
      ITEMP  = B(K,J)*XMULT+ .5
      B(K,J) = ITEMP*DIV
68    CONTINUE
70    CONTINUE
      IF(MTRUNC .EQ.ITIMES) RETURN
      MTRUNC = MTRUNC + 1
      M = 1
      GO TO 8
      END
C
C
C
      SUBROUTINE  KW1Q(BLINE)
CCC      INTEGER*2 BLINE
      DIMENSION BLINE(1)
      COMMON/KW2DS/ISKIP,CHANGE,MAX
      K = 0
60    K = K + 1
      IF(K .GT. MAX-2 ) GO TO 100
      IF(BLINE(K) .EQ. 0) GO TO 60
      IF(BLINE(K+1) .NE. 0) GO TO 60
      DO 70 J = 2,ISKIP
      IF((K+J) .GT. MAX) GO TO 100
      IF(BLINE(K+J) .NE. 0) GO TO 80
70    CONTINUE
      K = K + ISKIP
75    IF((K+1) .GT. MAX -2) GO TO 100
      IF(BLINE(K+1))60,77,60
77    K = K + 1
      GO TO 75
80    BJUMP = (BLINE(K+J) - BLINE(K ))/J
      J1 = J - 1
      DO 90 L = 1,J1
90    BLINE(K +L) = BLINE(K ) + (BJUMP*L)
      CHANGE = 1
      K = K + J1
      GO TO 60
100   CONTINUE
      RETURN
      END
C
C
C
C
C
C
      SUBROUTINE KW2D(AREA,WAREA,IH,IV,LEFT,IRIGHT,IBOTTO,ITOP,
     C NDIST,ILR,IBT,IWEAV,*,*)
C
C SUBROUTINE KW2D CHOOSES KW2D PROCEDURE AND KW1CS TO SEE WHEN
C IT IS COMPLETED
C
CCCC      INTEGER*2 WAREA
      COMMON/KW2DS/ISKIP,CHANGE,MAX
      DIMENSION AREA(IH,IV), WAREA(1)
      DATA CALL/0./
C
      CALL = CALL + 1
      ISKIP = NDIST
      HORIZC = 0
      VERC = 0
      L = IH*IV
      DO 5 I = 1,L
5     WAREA(I) = 0
      M = MAX0(IV,IH)
      IHOR = 2*M
      IHORC = 4*M
      IVER = 6*M
      IVERC = 8*M
      M = M/2
      IHOR1 = IHOR + 1
      IHORC1 = IHORC + 1
      IVER1 = IVER + 1
      IVERC1 = IVERC + 1
      IPATH = 1
      IF(IWEAV .EQ. 1) IPATH = 5
      IF(ILR .EQ. 1) IPATH = IPATH + 1
      IF(IBT .EQ. 1) IPATH = IPATH + 2
      UPDN = ITOP - IBOTTO + 1-.1
      XLFTRT= IRIGHT - LEFT + 1  -.1
      IF(IBT)45,25,45
   25 DO 30 I = 1,IBOTTO
30    WAREA(IHOR+I) = 1
      DO 35 I = ITOP,IV
35    WAREA(IHOR+I) = 1
   45 IF(ILR)65,50,65
50    DO 55 I = 1,LEFT
55    WAREA(IVER + I) = 1
      DO 60 I = IRIGHT,IH
60    WAREA(IVER + I) = 1
65    CONTINUE
      GO TO (100,200,100,200,600,600,700,700),IPATH
C
  100 CALL KW2F (AREA,WAREA,IH,IV,LEFT,IRIGHT,IBOTTO,ITOP,WAREA(IHOR1),
     C  WAREA(IHORC1))
      GO TO 900
C
  200 CALL KW2E(AREA,WAREA,IH,IV,IBOTTO,ITOP,       WAREA(IHOR1),
     C  WAREA(IHORC1))
      GO TO 900
C
  600 CALL KW2H (AREA,WAREA,IH,IV,LEFT,IRIGHT,IBOTTO,ITOP,WAREA(IVER1),
     C WAREA(IVERC1))
      GO TO 950
C
700   CALL KW2G(AREA,WAREA,IH,IV,LEFT,IRIGHT,      WAREA(IVER1),
     C WAREA(IVERC1))
      GO TO 950
C
C KW1C TO SEE IF ARRAY IS FILLED  HORIZONTALLY
C
900   HORIZ = 0
      DO 910 JJ = IBOTTO,ITOP
910   HORIZ = HORIZ + WAREA(IHOR + JJ)
      IF(HORIZ .GT. UPDN) RETURN 2
      HORIZC = 0
      DO 920 JJ = IBOTTO,ITOP
920   HORIZC = HORIZC + WAREA(IHORC + JJ)
      IF((VERC + HORIZC) .EQ. 0) ISKIP =ISKIP + NDIST
      IF(ISKIP .LE. M) GO TO 945
CCC   PRINT 940,CALL
      ICHEK = 5
CCCKW   940 FORMAT(' INSUFFICIENT DATA TO FILL ARRAY NUMBER ',F5.0)
CCCC  RETURN 1
  945 GO TO (600,600,700,700,600,600,700,700),IPATH
C
C KW1C TO SEE IF ARRAY IS FILLED VERTICALLY
C
950   VER = 0
      DO 960 JJ = LEFT,IRIGHT
960   VER = VER + WAREA(IVER + JJ)
      IF(VER .GT. XLFTRT) RETURN 2
      VERC = 0
      DO 970 JJ = LEFT,IRIGHT
      WRITE(999,*)'IVERC,JJ,WAREA=',IVERC,JJ,WAREA(IVERC + JJ)
970   VERC = VERC + WAREA(IVERC + JJ)
      IF((VERC+ HORIZC) .EQ. 0) ISKIP = ISKIP + NDIST
      WRITE(999,*)'VERC,HORIZC,V+H=',VERC,HORIZC,(VERC+HORIZC)
      WRITE(999,*)'ISKIP,NDIST,M=',ISKIP,NDIST,M
      IF(ISKIP .LE. M) GO TO 995
      ICHEK = 5
C **DEBUG      
      WRITE(999,941) CALL
  941 FORMAT(' INSUFFIC DATA TO FILL ARRAY NBR ',F5.0)
      RETURN 1
  995 GO TO (100,200,100,200,100,200,100,200),IPATH
      STOP
      END
C
C
C
C
C
      SUBROUTINE KW2F(AREA,BLINE,IH,IV,IL,IR,IB,IT,CNT,CNTC)
CCCC      INTEGER*2 BLINE,CNT,CNTC
       DIMENSION AREA(IH,IV),BLINE(1),CNT(1),CNTC(1)
      COMMON/KW2DS/ISTEP,CHANGE,MAX
C
      DO 200 JJ = IB,IT
      CHANGE = 0
      IF(CNT(JJ) .NE. 0) GO TO 200
      DO 50 K = IL,IR
50    BLINE(K-IL+1) = AREA(K,JJ)
      MAX = IR-IL+1
      CALL  KW1Q(BLINE)
      DO 130 K = IL,IR
130   AREA(K,JJ) = BLINE(K-IL+1)
      CALL KW1C(BLINE,CNT,CNTC,JJ)
200   CONTINUE
      RETURN
      END 
C
C
C
      SUBROUTINE KW2E(AREA,BLINE,IH,IV,IB,IT,CNT,CNTC)
CCCC      INTEGER*2 BLINE,CNT,CNTC
      DIMENSION AREA(IH,IV),BLINE(1),CNT(1),CNTC(1)
      COMMON/KW2DS/ISTEP,CHANGE,MAX
C
      DO 200 JJ = IB,IT
      CHANGE = 0
      IF(CNT(JJ) .NE. 0) GO TO 200
      IH1 = IH - 1
      DO 50 K = 1,IH1
50    BLINE(K) = AREA(K,JJ)
      DO 60 K = 1,IH1
60    BLINE(K+IH1) = BLINE(K)
      MAX = IH1 + IH1
      CALL  KW1Q(BLINE)
      DO 110 J = 1,IH1
      IF(BLINE(J) .EQ. 0) BLINE(J) = BLINE(J + IH1)
110   CONTINUE
      DO 130 J = 1,IH1
130   AREA(J,JJ) = BLINE(J)
      MAX = IH1
      CALL KW1C(BLINE,CNT,CNTC,JJ)
200   CONTINUE
      RETURN
      END
C
C
C
      SUBROUTINE KW2H(AREA,BLINE,IH,IV,IL,IR,IB,IT,CNT,CNTC)
CCCC      INTEGER*2  BLINE,CNT,CNTC
      DIMENSION AREA(IH,IV), BLINE(1),CNT(1),CNTC(1)
      COMMON/KW2DS/ISTEP,CHANGE,MAX
      DO 200 JJ = IL,IR
      CHANGE = 0
      IF(CNT(JJ) .NE. 0) GO TO 200
      DO 50 K = IB,IT
50    BLINE(K - IB + 1) = AREA(JJ,K)
      MAX = IT - IB + 1
      CALL  KW1Q(BLINE)
      DO 130 K = IB,IT
130   AREA(JJ,K) = BLINE(K - IB + 1)
      CALL KW1C(BLINE,CNT,CNTC,JJ)
200   CONTINUE
      RETURN
      END
C
C
C
      SUBROUTINE KW2G(AREA,BLINE,IH,IV,IL,IR,CNT,CNTC)
CCCC      INTEGER*2  BLINE,CNT,CNTC
      DIMENSION AREA(IH,IV),BLINE(1),CNT(1),CNTC(1)
      COMMON/KW2DS/ISTEP,CHANGE,MAX
C
      DO 200 JJ = IL,IR
      CHANGE = 0
      IF(CNT(JJ) .NE. 0) GO TO 200
      IV1=IV-1
      DO 50 K = 1,IV1
50    BLINE(K) = AREA(JJ,K)
      DO 60 K = 1,IV1
60    BLINE(K+IV1) = BLINE(K)
      MAX = IV1 + IV1
      CALL  KW1Q(BLINE)
      DO 110 J = 1,IV1
      IF(BLINE(J) .EQ. 0) BLINE(J) = BLINE(J + IV1)
110   CONTINUE
      DO 130 J = 1,IV1
130   AREA(JJ,J) = BLINE(J)
      MAX = IV1
      CALL KW1C(BLINE,CNT,CNTC,JJ)
200   CONTINUE
      RETURN
      END
C
C
C
C
C
C
      SUBROUTINE KWCUTCN(ICUT,W,IXSIZ,IYSIZ,IUNIT,IXINC,IYINC)
c
      INTEGER*2  IXSIZ,IYSIZ,IUNIT
      DIMENSION  W(IXSIZ,IYSIZ)
      INTEGER*1  ICUT(IXSIZ,IYSIZ)
      COMMON /KWCHOP/ M,TUX(4,300)
C
C
C***     TESTING CUTCON 2/4/91
         DO 361 I=1,IYSIZ
         DO 362 J=1,IXSIZ
               ICUT(J,I) = 0
 362     CONTINUE
 361     CONTINUE
C
         KWOUNT = 0
C
CCCKW         IXINC = 2
CCCKW         IYINC = 2
C
         DO 100 IY1=1,IYSIZ
         DO 200 IX1=1,IXSIZ
            IXSTR = IX1 - IXINC
            IXEND = IX1 + IXINC
            IF (IXSTR.LT.1)  IXSTR = 1
            IF (IXEND.GT.IXSIZ) IXEND = IXSIZ
            IYSTR = IY1 - IYINC
            IYEND = IY1 + IYINC
            IF (IYSTR.LT.1) IYSTR = 1
            IF (IYEND.GT.IYSIZ) IYEND = IYSIZ
            ICHK = 0
         DO 300 IYLP = IYSTR,IYEND
         DO 400 IXLP = IXSTR,IXEND
            IF (W(IXLP,IYLP).EQ.1.0) ICHK = 1
 400     CONTINUE
 300     CONTINUE
         IF (ICHK.EQ.1) THEN
            ICUT(IX1,IY1) = 0
         ELSE
            ICUT(IX1,IY1) = 1
            KWOUNT = KWOUNT + 1
         ENDIF
 200     CONTINUE
 100     CONTINUE
C
C
         M = 0
         AA = KWOUNT
         CALL KWPARM(47,AA)
         CALL KCUTGD(ICUT,IXSIZ,IYSIZ,KWOUNT,IUNIT)
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
C      
C ** REVISION ** (JML 11-12-92)
C    ADD NCOLTUX -- USED TO CHECK THAT MMAX DOES NOT INDEX PAST ARRAY BOUNDS 
      COMMON /KWCHOP/ M,TUX(4,300)
      NCOLTUX = 300
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
C
C ** REVISION ** (JML 11-12-92)
C    ADD MXCOL -- USED TO CHECK THAT MMAX DOES NOT INDEX PAST ARRAY BOUNDS 
C
      MXCOL = MIN0(NCOLTUX,MMAX)
      DO 5 NN = 1,MXCOL
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
CCCKW      PRINT 76
CCCKW    76 FORMAT(91H0VALUE OF MAX IS TOO SMALL IN CALL TO CUTCON.  INCREASE
CCCKW      1MAX OR DECREASE SIZE OF ARRAY XOUT.)
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