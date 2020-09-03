C
C
      SUBROUTINE KWLABL(XX,YY,PVAL2,ICQ2)
      COMMON /KWMID2/ NUMPTS,PVAL,ICQ
C  
      PVAL = PVAL2
      ICQ    = ICQ2
C
      CALL KWFLUSH
C
      RETURN
      END      
C
C ---------------------------------------------------------
C
      SUBROUTINE KWFR8N (IP,XP,YP)
      COMMON /KWMNMX/ XMNLON,YMNLAT
      COMMON /KWMID2/ NUMPTS,PVAL,ICQ
C
                          X1 = XP
       IF (XMNLON.LE.0.)  X1 = X1 - ABS(XMNLON)  
       IF (XMNLON.GT.0.)  X1 = X1  + (XMNLON)
                          Y1 = YP
       IF (YMNLAT.LE.0.0) Y1 = Y1 - ABS(YMNLAT)  
       IF (YMNLAT.GT.0.0) Y1 = Y1 + YMNLAT
C  
        CALL KWSTRE (X1,Y1,IP)
c
C
        return
        end
C
C---------------------------------------------------------------
C
       SUBROUTINE KWSTRE(X,Y,IP)
c       character*1 abug
       COMMON /KWMID2/ NUMPTS,PVAL,ICQ
       INTEGER*2       TOTPTS,ICHECK,IUNIT,ICTFLG
       COMMON /KWMID3/ TOTPTS,STOR(2,1)
       COMMON /KCTDST/ PASDST
       COMMON /KWCTCK/ ICTFLG
       DATA  XOLD /0.0/
       DATA  YOLD /0.0/
c
       CALL KINQBF(ICHECK,IUNIT)
C       
       IF (TOTPTS.LE.0) THEN
          WRITE (*,*) ' *** ERROR IN KWLIB *** '
          STOP
       ENDIF
C
C       *******  NEW SECTION ADDED FOR CUTCON  ********
C              
       IF (ICTFLG.EQ.1) THEN
           DSTCTR = PASDST / 2.0
           XDIFF = (XOLD-X)
           YDIFF = (YOLD-Y)
           DST = SQRT(XDIFF*XDIFF+YDIFF*YDIFF)
           IF (DST.GT.DSTCTR) THEN
               IP = 3 
               CALL KWFLUSH
               NUMPTS = 0
           ENDIF
        ENDIF
C  *****************************************************
C
                    NUMPTS = NUMPTS + 1
        IF (ICHECK.NE.1) THEN
            STOR(1,NUMPTS) = X 
            STOR(2,NUMPTS) = Y
        ELSE
            WRITE (IUNIT,REC=NUMPTS) X,Y
        ENDIF
c
                 ick = totpts *.7
        if (numpts.eq.TOTPTS.AND.ICHECK.NE.1) then
           CALL KWFLUSH
                 NUMPTS = 1
         STOR(1,NUMPTS) = X 
         STOR(2,NUMPTS) = Y 
        ENDIF
c
       XOLD = X
       YOLD = Y
C
       RETURN
       END 
C
C -----------------------------------------------------------------
C
       SUBROUTINE KWBOX (XTRAN,YTRAN,CHRHGT,NCHAR,ICNT,ANG,IFLG)
c       character*1 abug
       INTEGER*2        NUMBOX,ICUR,TOTBOX
       COMMON /KWINER/  NUMBOX,ICUR,BOX(10,1)
       COMMON /KWSVLM/  TOTBOX
c
       IF (NUMBOX.GT.TOTBOX) THEN
           IFLG = 3
           RETURN
       ENDIF
C
       IF (TOTBOX.LE.0) THEN
           WRITE (*,40) 
  40       FORMAT('*** ERROR IN KWLIB TOTBOX  ***')
           STOP
       ENDIF
C
       IFLG = 0
        IP3 = 3
        IP2 = 2
        Y1  = CHRHGT / 2.
        Y2  = -Y1
        dd  = real(nchar) 
        XX  = (CHRHGT *0.65 ) *  dd 
        if (xx.eq.0.0) xx = chrhgt
C
       IF (ICNT.EQ.1) THEN
           X1 = XX / 2.0
           X2 = -X1
       ELSE
           X2 = 0.0
           X1 = XX
       ENDIF
C
       call kinqcw(xdist,ydist)
c       
       x1 = x1 + xtran + xdist
       x2 = x2 + xtran - xdist
       y2 = y2 + ytran - ydist 
       y1 = y1 + ytran + ydist
C
c
c    -----------------  check boxes --------------------
c
                 IFLG = 0
c
          IF (NUMBOX.EQ.0) GOTO 443
C
            DO 444 II = 1,NUMBOX   
              IF (IFLG.EQ.1) GOTO 444
                 IFLG = 0
 45     FORMAT (I3,4F10.3)
        IF (X1.LT.BOX(1,II) .AND. X2.LT.BOX(1,II)) GOTO 444
        IF (X1.GT.BOX(3,II) .AND. X2.GT.BOX(3,II)) GOTO 444
        IF (Y1.LT.BOX(2,II) .AND. Y2.LT.BOX(2,II)) GOTO 444
        IF (Y1.GT.BOX(4,II) .AND. Y2.GT.BOX(4,II)) GOTO 444
                 IFLG = 1
444     CONTINUE
443     CONTINUE
C
C
       IF (IFLG.EQ.0)  CALL SETBOX (X2,Y2,X1,Y1,ANG) 
C
       RETURN
       END
C
C
C
C
C---------------------------------------------------------------
C
       SUBROUTINE KWFLUSH
       CHARACTER*256    STRING
       COMMON /KWMID2/  NUMPTS,PVAL,ICQ
       INTEGER*2        TOTPTS,IUNIT,ICHECK
       COMMON /KWMID3/  TOTPTS,STOR(2,1)
       COMMON /KWSWCP/  ICOV 
       COMMON /PLOTRN/  XSCNPG,YSCNPG,XPLTPG,YPLTPG,SCNFLP
       COMMON /KWSVLB/  PKWLBL 
       COMMON /KWLIM/   XLEFT,XRIGHT,YBOT,YTOP 
       COMMON /KWCHAR/  CHARHT,FONTCK,IDECPL,LABCK
       INTEGER*2        ICNT2,NCHR2,FONTCK
       DATA XOLD/0.0/
       DATA YOLD/0.0/
C
        IF (NUMPTS.LE.1) RETURN
C
        CALL KINQBF(ICHECK,IUNIT)
C 
C               ------    LABELS     ---------
c
            X = PKWLBL
        IF (LABCK.EQ.0) GOTO 98
c
        CALL KIQDST(IJPSV,DSTSAV)
        IJMP = 1
        DIST = .75
        CALL KSTDST(IJMP,DIST)
C
        IB1 = 0
        IF (IDECPL.NE.0) IB1 = 1
C
        XX = X
C
        CALL KWNST2(X,STRING,NCHAR)
        NCHAR = NCHAR - 4 + IDECPL + IB1
C
         ICNT = 1
          ANG = 0.0
          ISW = 0
          ifirst = 0
C
            i = numpts / 2
 336   CONTINUE
            I = I + 1
        IF (I.EQ.NUMPTS) THEN
        IF (ISW.EQ.1) goto 337
c
            I = 0
          ISW = 1 
          GOTO 336
        ENDIF
C
       IF (ICHECK.NE.1) THEN
           X1 = STOR(1,I)
           Y1 = STOR(2,I)
       ELSE
           READ (IUNIT,REC=I) X1,Y1
       ENDIF
c
          ipp = -3
        call kwtran (x1,y1,ipp)
        CALL SCLCON(X1,Y1,XMV,YMV)
c
         if (xmv.lt.xleft)  goto 98
         if (xmv.gt.xright) goto 98
         if (ymv.lt.ybot)   goto 98
         if (ymv.gt.ytop)   goto 98
         if (ifirst.eq.0) then
             ifirst=1
             xsv = xmv
             ysv = ymv
         endif
c
        call kwbox(xmv,ymv,charht,nchar,icnt,ang,iflg)
        IF (IFLG.EQ.1) GOTO 336
         if (IFLG.eq.2) then
         xmv = xsv
         ymv = ysv
         goto 386
         endif
C
 386   continue
       ICNT2 = 1
       NCHR2 = NCHAR
       CALL KWLAB (STRING,XMV,YMV,NCHR2,ICNT2,ANG,CHARHT,FONTCK)
C
 98    CONTINUE
C
        CALL KIQDST(IJPSV,DSTSAV)
        IPP = 3
       XOLD  = 0.0
        DO 100 I=1,NUMPTS
           IF (ICHECK.NE.1) THEN
              X1 = STOR(1,I)
              Y1 = STOR(2,I)
           ELSE
              READ(IUNIT,REC=I) X1,Y1
           ENDIF
C
        XOLD = X1
        YOLD = Y1
C
           CALL KWTRAN (X1,Y1,IPP)
          IPP = 2
100     CONTINUE
        CALL KSTDST(IJPSV,DSTSAV)
c
 337    CONTINUE
          NUMPTS = 0
       RETURN
       END 
c
c
C
c ------------------------------------------------------------------
C
       SUBROUTINE SETBOX (BX1,BY1,BX2,BY2,ANGLE) 
       INTEGER*2        NUMBOX,ICUR,iboxck
       COMMON /KWINER/   NUMBOX,ICUR,BOX(10,1)
c
         NUMBOX = NUMBOX + 1
            RAD = .01745329
         cosang = cos(angle*rad)
         sinang = sin(angle*rad)
c
         XMIN =  amin1(bx1,bx2)
         XMAX =  amax1(bx1,bx2)
         YMIN =  amin1(by1,by2)
         YMAX =  amax1(by1,by2)
c
           DX = XMAX - XMIN
           DY = YMAX - YMIN
        XTRAN = XMIN + DX / 2.
        YTRAN = YMIN + DY / 2.
           DX = (XMAX - XMIN) / 2.
           DY = (YMAX - YMIN) / 2.
C
         BXX1 = BX1 - XTRAN
         BYY1 = BY1 - YTRAN
         BXX2 = BX2 - XTRAN
         BYY2 = BY2 - YTRAN
c
           x1 =  ( bxx1*cosang - byy1*sinang) + xtran
           y1 =  ( byy1*cosang + bxx1*sinang) + ytran
           x2 =  ( bxx1*cosang - byy2*sinang) + xtran
           y2 =  ( byy2*cosang + bxx1*sinang) + ytran
           x3 =  ( bxx2*cosang - byy2*sinang) + xtran
           y3 =  ( byy2*cosang + bxx2*sinang) + ytran
           x4 =  ( bxx2*cosang - byy1*sinang) + xtran
           y4 =  ( byy1*cosang + bxx2*sinang) + ytran
c
       BOX (1,NUMBOX) = bx1 
       BOX (2,NUMBOX) = by1 
       BOX (3,NUMBOX) = bx2 
       BOX (4,NUMBOX) = by2 
       BOX (5,NUMBOX) = COSANG
       BOX (6,NUMBOX) = SINANG
       BOX (7,NUMBOX) = DX
       BOX (8,NUMBOX) = DY
       BOX (9,NUMBOX) = XTRAN
       BOX(10,NUMBOX) = YTRAN
C
       call kinqbx(iboxck)
       if (iboxck.eq.1) then
           ip3 = 3
           ip2 = 2
           call kwdev (bx1,by1,ip3)
           call kwdev (bx1,by2,ip2)
           call kwdev (bx2,by2,ip2)
           call kwdev (bx2,by1,ip2)
           call kwdev (bx1,by1,ip2)
       endif    
C
       return
       end
c
c ------------------------------------------------------------------
c
      SUBROUTINE kwcver (X11,Y11,IP)
      INTEGER*2         NUMBOX,IC
      COMMON /KWINER/   NUMBOX,IC,BOX(10,1)
      DIMENSION         S(4,100)
      DATA X22,Y22      /0.0,0.0/
c       
          IF (IP.EQ.3)  then
              x22 = x11
              y22 = y11
              return
           ENDIF
C
               IZERO = 0
                 IP3 = 3
                 IP2 = 2
              NUMSEG = 0
                XSAV = X11
                YSAV = Y11
                smll = 0.00001
C
              NUMSEG = 1
         S(1,NUMSEG) = X22
         S(2,NUMSEG) = Y22
         S(3,NUMSEG) = X11
         S(4,NUMSEG) = Y11
c
         DO 500 IC = 1, NUMBOX 
                DX = BOX (7,IC)
                DY = BOX (8,IC)
             ILOOP = 0
c
 450     CONTINUE
             ILOOP = ILOOP + 1
         IF (ILOOP.GT.NUMSEG) GOTO 500
c
         if (s(1,iloop).le.-31000.0) goto 450
           X22 = S(1,ILOOP) 
           Y22 = S(2,ILOOP)
           X11 = S(3,ILOOP)
           Y11 = S(4,ILOOP) 
C
         IF (X11.LT.BOX(1,IC).AND.X22.LT.BOX(1,IC)) GOTO 450
         IF (X11.GT.BOX(3,IC).AND.X22.GT.BOX(3,IC)) GOTO 450
         IF (Y11.GT.BOX(4,IC).AND.Y22.GT.BOX(4,IC)) GOTO 450
         IF (Y11.LT.BOX(2,IC).AND.Y22.LT.BOX(4,IC)) GOTO 450
C
           BX1 = X11 - BOX(9 ,IC)
           BY1 = Y11 - BOX(10,IC)
           BX2 = X22 - BOX(9 ,IC)
           BY2 = Y22 - BOX(10,IC)
            x1 = bx1*box(5,ic) - by1*(-box(6,ic)) 
            y1 = by1*box(5,ic) + bx1*(-box(6,ic))
            x2 = bx2*box(5,ic) - by2*(-box(6,ic)) 
            y2 = by2*box(5,ic) + bx2*(-box(6,ic))
           ISW = 0
C
       call kwmode (x1,y1,dx,dy,ix1,iy1)
       call kwmode (x2,y2,dx,dy,ix2,iy2)
c
      if (ix1*ix2.ne.1 .and.iy1*iy2.ne.1) goto 1
              goto 450
c
c
1      if (ix1.eq.IZERO) goto 2
       x1d = dx * ix1
       y1d = y1 + (y2-y1) * (x1d - x1) / (x2 - x1)
       call kwmode (x1d,y1d,dx,dy,ix1,iy1)
       goto 3
c
2      x1d = x1
       y1d = y1
c
3      if (iy1 .eq. 0) goto 4
       y1d = dy * iy1
       x1d = x1 + (x2 - x1) * (y1d - y1) / (y2 - y1)
c
4      if (abs(x1d-x1).lt.smll.and.abs(y1d-y1).lt.smll) goto 5
               ISW = 1
       S(1,NUMSEG) = (x1d*box(5,ic)-y1d*(box(6,ic)))+box( 9,ic)
       S(2,numseg) = (y1d*box(5,ic)+x1d*(box(6,ic)))+box(10,ic)
       S(3,NUMSEG) = (x1 *box(5,ic)- y1*(box(6,ic)))+box( 9,ic)
       S(4,numseg) = (y1 *box(5,ic)+ x1*(box(6,ic)))+box(10,ic)
c
5      if (ix2 .eq. 0) goto 6
       x2d = dx * ix2
       y2d = y1 + (y2 - y1) * (x2d - x1) / (x2 - x1)
       call kwmode (x2d,y2d,dx,dy,ix2,iy2)
       goto 7
c
6      x2d = x2
       y2d = y2
c
7      if (iy2 .eq. 0) goto 8
       y2d = dy * iy2
       x2d = x1 + (x2 - x1) * (y2d - y1) / (y2 - y1)
c
8      if (abs(x2-x2d) .lt. smll.and.abs(y2-y2d).lt.smll) then
           if (isw.eq.1) goto 450
           s(1,numseg) = -31000.0
           goto 450
       endif
c
       IF (ISW.EQ.1) NUMSEG = NUMSEG + 1
       S(1,NUMSEG) = (x2d*box(5,ic)-y2d*(box(6,ic)))+box(9,ic)
       S(2,numseg) = (y2d*box(5,ic)+x2d*(box(6,ic)))+box(10,ic)
       S(3,NUMSEG) = (x2*box(5,ic) -y2 *(box(6,ic)))+box(9,ic)
       S(4,numseg) = (y2*box(5,ic) +x2 *(box(6,ic)))+box(10,ic)
                goto 500
C
500    CONTINUE
C
        DO 600 I = 1 , NUMSEG
           IF (S(1,I).le.-31000.0) GOTO 600
          CALL BUFCOV (S(1,I),S(2,I),IP3)
          CALL BUFCOV (S(3,I),S(4,I),IP2)
600       CONTINUE
C
       X22 = XSAV
       Y22 = YSAV
       return
       end
c
C
c ------------------------------------------------------------------
C
      SUBROUTINE BUFCOV (X,Y,IP)    
      COMMON /PLOTRN/   XSCNPG,YSCNPG,XPLTPG,YPLTPG,SCNFLP
      DATA XOLD,YOLD    /0.0,0.0/
c
       IF (XOLD.EQ.X.AND.YOLD.EQ.Y) GOTO 999 
C
       CALL KWDEV (X,Y,IP)
c       
999    CONTINUE
       XOLD = X
       YOLD = Y
C
       RETURN
       END 
c
c------------------------------------------------------
c
      SUBROUTINE KWCLIP (x1,y1,x2,y2)
      common /outer/ dx,dy
C       .. COMMON ALLOWS ROUTINE BGKWCLIP TO INITIAL XOLD,YOLD TO
C          CURRENT GRAPHICS CURSOR POSITION.  CURRENTLY BGKWCLIP IS ONLY
C          CALLED FROM KWLAND WHEN A MAP IS DRAWN.
      COMMON /KWCLIPXY/ XOLD,YOLD
      DATA XOLD,YOLD /0.0,0.0/
c
         x1d = x1 
         y1d = y1 
         x2d = x2 
         y2d = y2 
       IZERO = 0
C
                call kwmode (x1d,y1d,dx,dy,ix1,iy1)
                call kwmode (x2d,y2d,dx,dy,ix2,iy2)
       if (ix1*ix2.eq.1 .or.iy1*iy2.eq.1) return
       if (ix1.eq.IZERO) goto 1
c
       xx = dx * ix1
                      xa  = (x2d-x1d)
                  if (xa.eq.0.0) then
                          goto 885
                   endif  
       y1d = y1d + (y2d-y1d) * (xx-x1d) / xa
885    x1d = xx
                call kwmode (x1d,y1d,dx,dy,ix1,iy1)
1      if (iy1.eq.IZERO) goto 2
       yy  = dy * iy1
                      ya  = (y2d-y1d)
                  if (ya.eq.0.0) then
                          x1d = x1d
                          y1d = yy
                          goto 2
                   endif  
       x1d = x1d + (x2d-x1d)*(yy-y1d) / ya
       y1d = yy
2      if (ix2.eq.IZERO) goto 3
       xx  = dx * ix2
                      xa  = (x2d-x1d)
                  if (xa.eq.0.0) then
                          y2d = y1d
                          goto 888
                   endif  
       y2d = y1d + (y2d-y1d)* (xx-x1d) /  xa
888    x2d = xx
                call kwmode (x2d,y2d,dx,dy,ix2,iy2)
3      if (iy2.eq.IZERO) goto 4
       yy  = dy * iy2
                     ya  = (y2d-y1d)
                  if (ya.eq.0.0) then
                          x2d = x1d
                          y2d = yy
                          goto 4
                   endif  
       x2d = x1d +(x2d-x1d) * (yy-y1d) / ya
       y2d = yy
4      continue
                 xdif = abs (x2d-x1d)
                 ydif = abs (y2d-y1d)
                 zzz  = 0.00001
      if ( (xdif.lt.zzz) .and. (ydif.lt.zzz)) return
c
        ip3 = 3
        ip2 = 2
C
      IF (X1D.EQ.XOLD .AND. Y1D.EQ.YOLD) THEN
              call kwbuff (x2d,y2d,ip2)
      ELSE
              call kwbuff (x1d,y1d,ip3)
              call kwbuff (x2d,y2d,ip2)
      ENDIF
C
              XOLD = X2D
              YOLD = Y2D
c
       return
       end
c
c
c
       SUBROUTINE KWMODE (x,y,dx,dy,ix,iy)
c
       ix = 0
       iy = 0
              if (abs(x).gt.dx) ix = sign (1.0,x)
              if (abs(y).gt.dy) iy = sign (1.0,y)
       return
       end
C
       SUBROUTINE  KWFIELD
     + (T,W,ICUT,IXSIZ,IYSIZ,XMNLN,YMNLT,XMXLN,YMXLT)
       INTEGER*2 IXSIZ,IYSIZ
       INTEGER*1   ICUT(IXSIZ,IYSIZ)
       DIMENSION   T(IXSIZ,IYSIZ),W(IXSIZ,IYSIZ)
       CHARACTER*256 STRING
       INTEGER*2 ICNTR,NCHR2,FONTCK
C
        CALL SETSTC(8,0)
        XDIFF = XMXLN - XMNLN
        YDIFF = YMXLT - YMNLT
          xstep = xdiff / ixsiz
          ystep = ydiff / iysiz
C
            IBY = -1
          YSTART = YMNLT - (YSTEP) 
       DO 500 IY = 1, IYSIZ  
          XSTART = XMNLN  - (XSTEP)
          YSTART = YSTART + YSTEP
             IBX = -1
             IBY = IBY * (-1)
c
       DO 501 IX = 1, IXSIZ
          XSTART = XSTART + XSTEP
             IBX = IBX * (-1)
         IF (IBX.LT.0) GOTO 501
c
         ICHK = W(IX,IY)
        IF (ICUT(IX,IY).EQ.1) GOTO 501
C
       NUMDEC = 1 
        CHRHT = 0.03
           X1 = XSTART
           Y1 = YSTART
            X = T(IX,IY)
        XX = X
        CALL KWNST2(X,STRING,NCHAR)
        NCHR2  = NCHAR - 2
        FONTCK = 0
        ICNTR  = 1
        XX     = X1
        YY     = Y1
               IP = -3                 
              CALL KWTRAN (XX,YY,IP) 
              CALL SCLCON(XX,YY,XBCK,YBCK)
        CALL KWLAB (STRING,XBCK,YBCK,NCHR2,ICNTR,ANG,CHRHT,FONTCK)
C
 501   CONTINUE
 500   CONTINUE
C
        CALL SETSTC(9,0)
C
       RETURN
       END 
C
C
       SUBROUTINE  KWTXTNM (X1,Y1,VAL,IDECPL)
       CHARACTER*256 STRING
       INTEGER*2 ICNTR,NCHR2,FONTCK
c
        IB1 = 0
        IF (IDECPL.NE.0) IB1 = 1
C
        CHRHT = 0.05
        CALL KWNST2(VAL,STRING,NCHAR)
CCKW        NCHR2  = NCHAR - 2
        NCHR2 = NCHAR - 4 + IDECPL + IB1
C
        FONTCK = 0
        ICNTR  = 1
        XX     = X1
        YY     = Y1
            IP = -3                 
        CALL KWTRAN (XX,YY,IP) 
        CALL SCLCON(XX,YY,XBCK,YBCK)
        CALL KWLAB (STRING,XBCK,YBCK,NCHR2,ICNTR,ANG,CHRHT,FONTCK)
C
       RETURN
       END 
