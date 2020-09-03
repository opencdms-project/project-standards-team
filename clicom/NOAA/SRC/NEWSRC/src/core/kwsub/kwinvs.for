C
C
C
      SUBROUTINE KWINVS (XXIN ,YYIN , XBCK ,YBCK,ICHK)
C
C     ****  INPUT PARAMETERS  XXIN = X-SCREEN COORDINATE
C                             YYIN = Y-SCREEN COORDINATE
C       
C     ***  OUTPUT PARAMETERS  XBCK = LONGITUDE VALUE
C                             YBCK = LATITUDE  VALUE
C                             ICHK = ERROR CODE   0 = GOOD
C                                          ( <> 0 ) = BAD RANGE
C
C
      COMMON /KWSCLE1/  SCNSCF , XSCNMV , YSCNMV ,XSCL,YSCL
      COMMON /KWSCL2/   XSCF,YSCF
      COMMON /SAVINP/   TSAVIN(13) , TSAVIO(13)
      COMMON /PLOTRN/   XSCNPG,YSCNPG,XPLTPG,YPLTPG,SCNFLP
      COMMON /MAINPS/   PROJIN, PROJOT, DATAIN, DATAOT, ZONEIN,
     .                  ZONEOT, IPFILE
      COMMON /MAP/      MAPPRJ
      DIMENSION        INDEF(3),IODEF(3)
      DIMENSION        CRDIN(2),CRDIO(2),TPARIN(13),TPARIO(13)
      INTEGER*2        ICHK
C
      IF (MAPPRJ.LT.24) THEN
          XSCLE = SCNSCF
          YSCLE = XSCLE
      ELSE
          XSCLE = XSCF
          YSCLE = YSCF
      ENDIF
C
            XIN = XXIN
            YIN = YYIN
            XX = (XIN - XSCNPG)
           XIN = ( XX + XSCNMV ) / XSCLE 
            YY = YIN - YSCNPG
           YIN = ( YY + YSCNMV ) / YSCLE
C       
                CRDIN(1) = XIN
                CRDIN(2) = YIN
C
                INDEF(1) = PROJOT
                INDEF(2) = ZONEOT
                INDEF(3) = DATAOT 
                IODEF(1) = PROJIN
                IODEF(2) = ZONEIN
                IODEF(3) = DATAIN
                 IPFLE   = IPFILE
C
            DO 251 I  = 1 , 13
            TPARIO(I) = 0.0
251         TPARIN(I) = TSAVIO(I)
C            
       IGO = 2
       CALL GTRNZ0 (CRDIN,INDEF,TPARIN,CRDIO,IODEF,TPARIO, 
     .                   IPFLE,IFLG,IGO)
C
       IF ( IFLG .NE. 0 ) THEN
          ICHK = IFLG
           return
       ENDIF
c
        XBCK  = CRDIO(1) 
        YBCK  = CRDIO(2)

        XDEG  = XBCK /100.
       IXDEG  = XDEG
       IXSEC  = NINT((XDEG - IXDEG)*100)
        XX    = IXDEG / 100.
       IXDEG  = XX
       IXMIN  = NINT((XX - IXDEG)*100)
       XDEG   = IXDEG
       XMIN   = ABS(IXMIN)
       XSEC   = ABS(IXSEC)
C
        YDEG  = YBCK /100.
       IYDEG  = YDEG
       IYSEC  = NINT((YDEG - IYDEG)*100) 
        YY    = IYDEG / 100.
       IYDEG  = YY
       IYMIN  = NINT((YY - IYDEG) * 100)
       YDEG   = IYDEG
       YMIN   = ABS(IYMIN)
       YSEC   = ABS(IYSEC)
C
       XADD   = XMIN/60. + XSEC/3600.
                      SW =  1
       IF (XDEG.LT.0) SW = -1
       XBCK = XDEG + (SW*XADD)
C
       YADD   = YMIN/60. + YSEC/3600.
                      SW =  1
       IF (YDEG.LT.0) SW = -1
       YBCK = YDEG + (SW*YADD)
C
       RETURN
       END
