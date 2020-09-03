C
C
C        XMN   = Minimum longitude value : West  negative : East  positive
C        YMN   = Minimum latitude  value : South negative : North positive
C        XMX   = Maximum longitude value : West  negative : East  positive
C        YMX   = Maximum latitude  value : South negative : North positive
C        XINC  = Longitude whole degree increment value
C        YINC  = Latitude  whole degree increment value
C        XDIV  = Longitude division number between XINC
C        YDIV  = Latitude  division number between YINC
C        TIC   =  0   FULL GRID
C              <> 1   TIC MARKS  | TIC LENGTH IS VALUE OF TIV
C        CMV   = Distance to move character from tic length or border
C        CHGT  = Character height
C        LABEL = 1  PLOT LABELS
C              = 0  DONOT PLOT LABELS
C        IFONT = 0  Vector font
c              = 1  DOT FONT
C
C
      SUBROUTINE KWGRID (XMN,YMN,XMX,YMX,XINC,YINC,XDV,YDV,
     +                   TIC,CMV,CHGT,LABEL,IFONT)
      CHARACTER*256     STRING
      COMMON /KWLIM/    XLF,XRT,YBT,YTP 
      COMMON /KWSWCP/   ICOVER
      COMMON /KWCM  /   CETLON
      INTEGER*2         ICT,NCHR2,LABEL,IFONT,XDV,YDV
C
         CALL KIQDST(JMPSAV,DSTSAV)
         IJMP = 0
         CALL KSTDST(IJMP,DSTSAV)
C
               YY = YMN
               XX = XMN
               IP = -3                 
              CALL KWTRAN (XX,YY,IP) 
              CALL SCLCON(XX,YY,XBCK,YBCK)
                   IP = 3
              CALL KWBUFF(XX,YY,IP)
C
         XDIV   = XDV
         YDIV   = YDV
         ICOVSV = ICOVER
         ICOVER = 0
           ITIC = 0
       IF (TIC.NE.0.0) THEN
           ITIC   = 1
           TICLEN = TIC
       ENDIF
C
         IF (CETLON.NE.0.0) THEN
             XMIN = -180.0 
             XMAX =  180.0
         ELSE
             XMIN = XMN 
             XMAX = XMX
         ENDIF
C  
          IF (XMIN.LT.-180.0) XMIN = -180.
          IF (XMAX.GT. 180.0) XMAX =  180.
          YMIN = YMN 
          YMAX = YMX 
          IF (YMIN.LT.-90.0) YMIN = -90.
          IF (YMAX.GT. 90.0) YMAX =  90.
C
            X1 =  XMIN
            X2 =  XMAX
            Y1 =  YMIN 
            Y2 =  YMAX
           STP = .05
            BB =  1.0 
           IP3 = 3
           IP2 = 2
           ANGLE  = 0.0
C
C  *******************   LATITUDE GRIDS ****************************
C
         IF (YINC.LE.0.0) GOTO 4500
C
          YY = AMOD(Y1,1.0)
          IF (YY.NE.0.0)   Y1 = Y1 - YY
          IF (YY.LT.-90.0) YY = -90.0
         YS1 = Y1 - YINC
         YS2 = Y1
       CALL KGRDYD(X1,YS1,YS2,YDIV,XLF,YBT,XRT,YTP,ITIC,TICLEN)
C
C
              IBB = 0
              ICT = 0
       DO 101  YY = Y1, Y2, YINC
           PASNUM = YY
             YPAS = YY
               XX = X1
               IP = -3                 
              CALL KWTRAN (XX,YPAS,IP) 
              CALL SCLCON(XX,YPAS,XBCK,YBCK)
C
              IF (YBCK.LT.YBT) GOTO 101
              IF (YBCK.GT.YTP) GOTO 101
C
              IF (ITIC.EQ.0) THEN
                  XX2 = XRT 
                 IF ((YBCK.NE.YBT).AND.(YBCK.NE.YTP)) THEN
                     CALL KWBUFF (XLF,YBCK,IP3)
                     CALL KWBUFF (XX2,YBCK,IP2)
                 ENDIF 
              ELSE
                  XX2 = XRT + TICLEN
                  CALL KWBUFF(XRT ,YBCK,IP3)
                  CALL KWBUFF(XX2    ,YBCK,IP2)
              ENDIF
C
               IPAS = 0
                 XS = XX2 + CMV
                 YS = YBCK 
              PASN2 = ABS(PASNUM)
           CALL KWNSTR(PASN2,STRING,NCHAR)
              NCHAR = NCHAR -1
         DO 2001 IS = 1,NCHAR
           IF (STRING(IS:IS).EQ.'.') THEN
               IFLG= 0
               IF (STRING(IS+1:IS+1).EQ.'0') IFLG=IFLG+1
               IF (STRING(IS+2:IS+2).EQ.'0') IFLG=IFLG+1
               IF (IFLG.NE.2) GOTO 2001
               NCHAR = IS - 1
               GOTO 2002
            ENDIF
2001      CONTINUE
2002      CONTINUE
         IF (STRING(NCHAR-2:NCHAR-2) .EQ. '0') THEN
         IF (STRING(NCHAR-1:NCHAR-1) .EQ. '0') NCHAR = NCHAR - 2
         ENDIF
C
           NCHAR = NCHAR + 1
              IF (PASNUM.LT.0.0) STRING(NCHAR:NCHAR) = 'S'
              IF (PASNUM.GT.0.0) STRING(NCHAR:NCHAR) = 'N'
              NCHAR = NCHAR + 1
             DO 2003 IS=NCHAR,20
2003         STRING(IS:IS) = ' '
C
            if (pasnum.eq.0.0) then
                string = 'EQ'
                NCHAR  = 2
             endif
c
           NCHR2 = NCHAR
C
       IF (CHGT.NE.0.0) THEN
       IF (LABEL.EQ.1)
     +    CALL KWLAB  (STRING,XS,YS,NCHR2,ICT,ANGLE,CHGT,IFONT)
       ENDIF
C
C         ********** DIVISION INTERVAL  ******
                IBB = IBB + 1
           IF (IBB.EQ.1) GOTO 1102
        IF (YDIV.GT.0.0) THEN
                YS2 = YY
      CALL KGRDYD(X1,YS1,YS2,YDIV,XLF,YBT,XRT,YTP,ITIC,TICLEN)
         ENDIF
1102    CONTINUE
                YS1 = YY
C -------------------------------------------------------------
101     CONTINUE
C
      IF (YDIV.GT.0.0) THEN
         YS2 = YY
         CALL KGRDYD(X1,YS1,YS2,YDIV,XLF,YBT,XRT,YTP,ITIC,TICLEN)
      ENDIF
     
c
C**********************   LONGITUDE  **********************
C
4500   CONTINUE
C
       IF (XINC.LE.0.0) GOTO 4501
C
C
          XX = AMOD(X1,1.0)
          IF (XX.NE.0.0)   XX = X1 - XX
          IF (XX.LT.-180.0) YY = -180.0
         XS1 = X1 - XINC
         XS2 = X1
       CALL KGRDXD(Y1,XS1,XS2,XDIV,XLF,YBT,XRT,YTP,ITIC,TICLEN) 
c
        ICROSS = 0
           ICT = 2
        DO 201  XX = X1, X2, XINC
               XXX = XX
           IF (XXX.LT.-180.0) XXX = XXX + 360.00
           IF (XXX.GT. 180.0) XXX = XXX - 360.00
            PASNUM = XXX
          IF (ABS(PASNUM).EQ.180.0) ICROSS = ICROSS + 1
              IPAS = 1
              XPAS = XXX
                YY = Y1 
                IP = -3
           CALL KWTRAN (XPAS,YY,IP) 
           CALL SCLCON (XPAS,YY,XBCK,YBCK)
C
           IF (XBCK.LT.XLF)  GOTO 201
           IF (XBCK.GT.XRT)  GOTO 201
C
           IF (ITIC.EQ.0.0) THEN
               YY2 = YBT
              IF ((XBCK.NE.XLF).AND.(XBCK.NE.XRT)) THEN
                   CALL KWBUFF (XBCK,YTP,IP3)
                   CALL KWBUFF (XBCK,YY2,IP2)
              ENDIF
           ELSE
               YY2 = YBT - TICLEN
               CALL KWBUFF(XBCK,YBT   ,IP3)
               CALL KWBUFF(XBCK,YY2,IP2)
           ENDIF
                XS = XBCK 
CCC7/8/90 CHANGED ICT = 2   ********            YS = YY2 - CMV - .02
                YS = YY2 - CMV
C
                 PASN2 = ABS(PASNUM)
          IF (PASN2.EQ.180.0.AND.ICROSS.EQ.2) GOTO 201
           CALL KWNSTR(PASN2,STRING,NCHAR)
                NCHAR = NCHAR -1
           DO 3001 IS = 1,NCHAR
           IF (STRING(IS:IS).EQ.'.') THEN
               IFLG= 0
               IF (STRING(IS+1:IS+1).EQ.'0') IFLG=IFLG+1
               IF (STRING(IS+2:IS+2).EQ.'0') IFLG=IFLG+1
           IF (IFLG.NE.2) GOTO 3001
                NCHAR = IS - 1
               GOTO 3002
            ENDIF
3001        CONTINUE
3002        CONTINUE
         IF (STRING(NCHAR-2:NCHAR-2) .EQ. '0') THEN
         IF (STRING(NCHAR-1:NCHAR-1) .EQ. '0') NCHAR = NCHAR - 2
         ENDIF
C
           NCHAR = NCHAR + 1
              IF (PASNUM.LT.0.0) STRING(NCHAR:NCHAR) = 'W'
              IF (PASNUM.GT.0.0) STRING(NCHAR:NCHAR) = 'E'
              NCHAR = NCHAR + 1
             DO 3003 IS=NCHAR,20
3003         STRING(IS:IS) = ' '
C
           IF (PASNUM.EQ.0.0.AND.CETLON.EQ.0.0) THEN
               STRING = 'CM'
               NCHAR  = 2
           ENDIF
C
           NCHR2 = NCHAR
C
       IF (CHGT.NE.0.0) THEN
       IF (LABEL.EQ.1) 
     +    CALL KWLAB  (STRING,XS,YS,NCHR2,ICT,ANGLE,CHGT,IFONT)
        ENDIF
C
C         ********** DIVISION INTERVAL  ******
                IBB = IBB + 1
           IF (IBB.EQ.1) GOTO 2102
      IF (XDIV.GT.0.0) THEN
                XS2 = XX
        CALL KGRDXD(Y1,XS1,XS2,XDIV,XLF,YBT,XRT,YTP,ITIC,TICLEN) 
      ENDIF
2102  CONTINUE
                  XS1 = XX
C
201     CONTINUE
4501    CONTINUE
C
         ICOVER    = ICOVSV
C
         CALL KSTDST(JMPSAV,DSTSAV)

       RETURN 
       END  
C
C
C
C
      SUBROUTINE KGRDYD(X1,YS1,YS2,YDIV,XLF,YBT,XRT,YTP,ITIC,TICLEN)
C
C         ********** DIVISION INTERVAL  ******
C
                IP3 = 3
                IP2 = 2
              YINC2 = (YS2-YS1) / (YDIV+1)
       DO 1101  YYA = YS1,YS2 , YINC2
               YPAS = YYA
                 XX = X1
                 IP = -3                 
              CALL KWTRAN (XX,YPAS,IP) 
              CALL SCLCON(XX,YPAS,XBCK,YBCK)
C
             IF (YBCK.LT.YBT) GOTO 1101
             IF (YBCK.GT.YTP) GOTO 1101
C
              IF (ITIC.EQ.0) THEN
                  XX2 = XRT 
                 IF ((YBCK.NE.YBT).AND.(YBCK.NE.YTP)) THEN
                     CALL KWBUFF (XLF,YBCK,IP3)
                     CALL KWBUFF (XX2,YBCK,IP2)
                 ENDIF 
              ELSE
                  XX2 = XRT + TICLEN * .5
                  CALL KWBUFF(XRT ,YBCK,IP3)
                  CALL KWBUFF(XX2    ,YBCK,IP2)
              ENDIF
1101    CONTINUE
        RETURN
        END
C
C
C
C
      SUBROUTINE KGRDXD(Y1,XS1,XS2,XDIV,XLF,YBT,XRT,YTP,ITIC,TICLEN) 
C
C         ********** DIVISION INTERVAL  ******
C
                IP3 = 3
                IP2 = 2
              XINC2 = (XS2-XS1) / (XDIV+1)
       DO 2101  XXA = XS1,XS2 , XINC2
               XPAS = XXA
                 YY = Y1 
                 IP = -3
           CALL KWTRAN (XPAS,YY,IP) 
           CALL SCLCON (XPAS,YY,XBCK,YBCK)
C
           IF (XBCK.LT.XLF)  GOTO 2101
           IF (XBCK.GT.XRT)  GOTO 2101
C
           IF (ITIC.EQ.0.0) THEN
               YY2 = YBT
              IF ((XBCK.NE.XLF).AND.(XBCK.NE.XRT)) THEN
                   CALL KWBUFF (XBCK,YTP,IP3)
                   CALL KWBUFF (XBCK,YY2,IP2)
              ENDIF
           ELSE
               YY2 = YBT - TICLEN * .5
               CALL KWBUFF(XBCK,YBT   ,IP3)
               CALL KWBUFF(XBCK,YY2,IP2)
           ENDIF
2101    CONTINUE
2102    CONTINUE
        RETURN
        END
