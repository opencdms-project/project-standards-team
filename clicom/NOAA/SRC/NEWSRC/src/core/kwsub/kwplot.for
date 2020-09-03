c
c
c
      SUBROUTINE KWPLOT(XIN,YIN,IP)
      DATA XOLD /0/
      DATA YOLD /0/
C
      CALL SCLCON (XIN,YIN,XX,YY)
c
      if (ip.eq.3) then 
          xold = xx
          yold = yy  
          return
      endif
c
      call kwclip (xold,yold,xx,yy)
C
      XOLD = XX
      YOLD = YY
C
       RETURN
       END
C
c -------------------------------------------------------------
C
      SUBROUTINE kwbuff (XIN,YIN,IP)
      COMMON /KWSWCP/   ICOV 
C  
       if (icov.eq.1) then
           call kwcver(xin,yin,ip)
       else
           call kwdev (xin,yin,ip)
       endif
c       
       RETURN
       END
C
c -------------------------------------------------------------
c
      SUBROUTINE KWDEV (XIN,YIN,IP)
      COMMON /PLOTRN/   XSCNPG,YSCNPG,XPLTPG,YPLTPG,SCNFLP
      COMMON /KDSTCK/  IJMP,PLTDST
      DATA  XOLD,YOLD /0.0,0.0/
C  
         YS = YIN + YSCNPG
         XS = XIN + XSCNPG
C
        IF (IP.LT.2) IP = 3
        IF (IP.GT.3) IP = 3
c
         IF (IJMP.EQ.1) THEN
             XDIFF = ABS(XOLD-XS)
             YDIFF = ABS(YOLD-XS)
             IF (XDIFF.GT.PLTDST) IP = 3
        ENDIF
C
       XOLD = XS
       YOLD = YS
c
              if (ip.eq.3) call movabs(xs,ys)
              if (ip.eq.2) call  lnabs(xs,ys)
C
       RETURN
       END
C
c -------------------------------------------------------------
C
      SUBROUTINE SCLCON(XIN,YIN,XOUT,YOUT)
      COMMON /MAP/      MAPPRJ
      COMMON /KWSCLE1/  SCNSCF , XSCNMV , YSCNMV ,XSCL,YSCL
      COMMON /KWSCL2/   XSCF,YSCF
c
      IF (MAPPRJ.LT.24) THEN
         XOUT = (XIN * SCNSCF - XSCNMV)
         YOUT = (YIN * SCNSCF - YSCNMV)
      ELSE
         XOUT = (XIN * XSCF - XSCNMV)
         YOUT = (YIN * YSCF - YSCNMV)
      ENDIF
C
      RETURN
      END 
c
