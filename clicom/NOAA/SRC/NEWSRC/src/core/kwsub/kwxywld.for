c
c
c
       SUBROUTINE KWXYWLD (XLON,YLAT,XWLD,YWLD,IFLAG)
c
c     ****  INPUT PARAMETERS  XLON = LONGITUDE COORDINATE
C                             YLAT = LATITUDE  COORDINATE
C
C     **** OUTPUT PARAMETERS  XWLD = X - WORLD COORDINATE
C                             YWLD = Y - WORLD COORDINATE
C                            IFLAG = ERROR CODE   0 = GOOD
C                                             <> =0 = BAD LAT/LON RANGE
c
c
       INTEGER*2 IFLAG
       COMMON /PLOTRN/ XSCNPG,YSCNPG,XPLTPG,YPLTPG,SCNFLP
C
       IP = -3
       IFLAG = 0
       CALL KWTRAN (XLON,YLAT,IP)
C
       IF (IP.NE.-3) THEN
           IFLAG = 1
           RETURN
       ENDIF
C
       CALL SCLCON(XLON,YLAT,XBCK,YBCK)
C
       XWLD = XBCK + XSCNPG
       YWLD = YBCK + YSCNPG
C
       RETURN
       END
