C
C
C
C
       SUBROUTINE  KWINQL (X1,Y1,X2,Y2)
       COMMON /KWLIM/    XLF,XRT,YBT,YTP 
       COMMON /PLOTRN/   XSCNPG,YSCNPG,XPLTPG,YPLTPG,SCNFLP
C
C      INPUT PARAMETERS :  NONE
C
c     OUTPUT PARAMETERS : X1 = Minimum x-device coordinate
c                         Y1 = Minimum y-device coordinate
c                         X2 = Maximum x-device coordinate
c                         Y2 = Maximum y-device coordinate
C
       X1 = XLF + XSCNPG
       Y1 = YBT + YSCNPG
       X2 = XRT + XSCNPG
       Y2 = YTP + YSCNPG
C
       RETURN
       END
