C
C
C
      SUBROUTINE KWAREA (XSCLE,YSCLE,XPAGMV,YPAGMV,
     .                   XMNLON,YMNLAT,XMXLON,YMXLAT,CM)
c
      COMMON /KWSCLE1/ SCNSCF,XSCNMV,YSCNMV,XSCL2,YSCL2
      COMMON /PLOTRN/  XSCNPG,YSCNPG,XPLTPG,YPLTPG,SCNFLP
      COMMON /MAP/    MAPPRJ
      COMMON /KWCM  /   CETLON
C
c     ********* set default clipping box limits ************
c 
      xclp = 0.00
      yclp = -0.004
      yclp = 0.01
      xclp = 0.00
      call ksetcw (xclp,yclp)
      call ksetbx(0)
C
CCC      MAPPRJ = 18
CCC      MAPPRJ = 25
C
      CETLON = CM
      XSCL2  = XSCLE
      YSCL2  = YSCLE
      XSCNPG = XPAGMV
      YSCNPG = YPAGMV
c
      XMNLN = XMNLON
      XMXLN = XMXLON
      YMXLT = YMXLAT
      YMNLT = YMNLAT
C
      CALL KWSCAL(XMNLON,YMNLAT,XMXLON,YMXLAT)
C
      RETURN
      END
