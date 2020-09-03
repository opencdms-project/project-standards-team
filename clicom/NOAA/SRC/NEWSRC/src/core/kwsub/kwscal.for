C
C
C
      SUBROUTINE KWSCAL (XMNLON,YMNLAT,XMXLON,YMXLAT)
C
          LONFLG = 0
          LATFLG = 0
             XMN = -180.0
             YMN = -90.0
             XMX =  180.0
             YMX =   90.0
C
             CALL SQRSCL ( XMN,YMN,XMX,YMX )
C
             if (xmxlon - xmnlon .lt. 360.0) lonflg = 1
             if (ymxlat - ymnlat .lt. 180.0) latflg = 1
c
             IF (LONFLG.EQ.1 .OR. LATFLG.EQ.1) THEN
                 CALL ZOMSCL (XMNLON,YMNLAT,XMXLON,YMXLAT)
              ENDIF
C
      RETURN
      END
c
c
c
      SUBROUTINE SQRSCL (XMNLON,YMNLAT,XMXLON,YMXLAT)
      COMMON /SAVINP/   TSAVIN(13),TSAVIO(13)
      COMMON /MAINPS/   PROJIN, PROJOT, DATAIN, DATAOT, ZONEIN,
     .                  ZONEOT, IPFILE
      COMMON /SWTCH/    SWITCH
      COMMON /ijmp/     ISKIP
      COMMON /PASS2/    IJMP
      COMMON /KWCM  /   CETLON
      COMMON /MAP/      MAPPRJ
      INTEGER*4         SWITCH
C
CCCCCC6/12/90        MAPPRJ = 18
        ISKIP  = 0
        SWITCH = 0
CCCCC        CETLON = 0.0
C
C   ******************    SET - UP     ************************
          DO 200 I = 1 , 13
             TSAVIN(I) = 0.0
             TSAVIO(I) = 0.0
200       CONTINUE
C
             PROJIN  = 0.0
             PROJOT  = MAPPRJ
             DATAIN  = 5.0
             DATAOT  = 2.0
             ZONEIN  = 101.
             ZONEOT  = 101.
             IPFILE  = 6.0
C
               CALL WDBCON (CETLON  ,CMB)
           TSAVIO(5) =  CMB 
C
C            ** THE NAME OF THE ROUTINE SETUP WAS CHANGED TO SCLSETUP 
C               BECAUSE IT CONFLICTED WITH THE SETUP CALLED FROM THE
C               ADMIN PROGRAM.
C               CALL SETUP      ------- JML 7-16-92
C
           CALL SCLSETUP
C
C
C
C ***********************   LIMITS  ********************************
C 
C 
                       XMIN = CETLON + XMNLON
                       XMAX = CETLON + XMXLON
           IF (XMIN.LT.-180.0) XMIN = XMIN + 360.0
           IF (XMAX.GT. 180.0) XMAX = XMAX - 360.0
                       YMIN = YMNLAT
                       YMAX = YMXLAT
C
                                IP = -3
         CALL KWTRN2 (XMIN,YMIN,IP)
                                IP = -3
         CALL KWTRN2 (XMAX,YMAX,IP)
C
          XMAX  = ABS(XMIN)
       if (xmax .eq. xmin ) xmin = - xmax
c
        CALL SCLSCN (XMIN,XMAX,YMIN,YMAX)
C
333     CONTINUE
C
         ICHK = 1
        ISKIP = 1
C 
       RETURN
       END
c
c -----------------------  ZOMSCL  ------------------------------
c
      SUBROUTINE ZOMSCL(XMNLON,YMNLAT,XMXLON,YMXLAT)
      COMMON /KWCM  /   CETLON
C
       XMIN = XMNLON
       XMAX = XMXLON
       YMIN = YMNLAT
       YMAX = YMXLAT
C
       IFLAG = 0
       XDIFF = ABS(XMAX-XMIN)
       IF (XDIFF.GT.359.5) IFLAG = 1
C
       IF (IFLAG.EQ.1) THEN
                              XMIN = CETLON + XMIN
                              XMAX = CETLON + XMAX
          IF (XMIN.LT.-180.0) XMIN = XMIN + 360.0
          IF (XMAX.GT. 180.0) XMAX = XMAX - 360.0
        ENDIF
C
                                IP = -3
         CALL KWTRAN (XMIN,YMIN,IP)
                                IP = -3
         CALL KWTRAN (XMAX,YMAX,IP)
C
        IF (IFLAG.EQ.1) THEN
        IF (XMIN.EQ.XMAX.AND.XMIN.LT.0.0) XMAX = ABS(XMAX)
        IF (XMIN.EQ.XMAX.AND.XMIN.GT.0.0) XMIN = -XMAX
        ENDIF
C
         CALL SCLSCN (XMIN,XMAX,YMIN,YMAX)
C
         RETURN
         END
c
c
c
c
      SUBROUTINE        SCLSCN (XMIN,XMAX,YMIN,YMAX)
      COMMON /KWSCLE1/  SCNSCF , XSCNMV , YSCNMV ,XSCLE,YSCLE
      COMMON /KWSCL2/   XSCNSF , YSCNSF
      COMMON /KWLIM/    XLEFT,XRIGHT,YBOT,YTOP 
      COMMON /OUTER/    DX,DY
      COMMON /MAP/      MAPPRJ
C
           IFLAG = 0
           SCALE = XSCLE
C
          X1    = AMAX1(XMAX,XMIN)
          X2    = AMIN1(XMAX,XMIN)
          XMAX  = X1
          XMIN  = X2
          Y1    = AMAX1(YMAX,YMIN)
          Y2    = AMIN1(YMAX,YMIN)
C
          YMAX  = Y1
          YMIN  = Y2
C
      IF (MAPPRJ.LT.24) THEN
 100     CONTINUE
c
            SCL   = SCALE
            XDIFF = XMAX - XMIN
            YDIFF = YMAX - YMIN
          XSCNSF  = SCL / XDIFF
          YSCNSF  = SCL / YDIFF
          SCNSCF  = ABS(XSCNSF)
          IF (YDIFF.GT.XDIFF) SCNSCF = ABS(YSCNSF)
C
         XSCNMV = (XMIN + XDIFF/2.0) * SCNSCF
         YSCNMV = (YMIN + YDIFF/2.0) * SCNSCF
C
              XLEFT = (XMIN * SCNSCF - XSCNMV)
              YBOT  = (YMIN * SCNSCF - YSCNMV)
             XRIGHT = (XMAX * SCNSCF - XSCNMV)
             YTOP   = (YMAX * SCNSCF - YSCNMV)
                 DX = XRIGHT
                 DY = YTOP 
c
       YDIFF = ABS (YTOP - YBOT )
       XDIFF = ABS (XRIGHT - XLEFT)
       IF  (YDIFF.GT.YSCLE .OR .XDIFF.GT.XSCLE) THEN
           SCALE = SCALE - .1
           GOTO 100
       ENDIF
C
       ELSE
C           -------------- FITIT  PROJECTION --------------
C
            XDIFF  = XMAX - XMIN
            YDIFF  = YMAX - YMIN
            XSCNSF = XSCLE / XDIFF
            YSCNSF = YSCLE / YDIFF
            XSCNMV = (XMIN + XDIFF/2.0) * XSCNSF
            YSCNMV = (YMIN + YDIFF/2.0) * YSCNSF
C
              XLEFT = (XMIN * XSCNSF - XSCNMV)
              YBOT  = (YMIN * YSCNSF - YSCNMV)
             XRIGHT = (XMAX * XSCNSF - XSCNMV)
             YTOP   = (YMAX * YSCNSF - YSCNMV)
                 DX = XRIGHT
                 DY = YTOP 
       ENDIF
       
C
       RETURN
       END  
