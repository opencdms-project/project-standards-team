C
C
C
      SUBROUTINE KWTRAN ( XIN , YIN ,IP )
      COMMON /MAINPS/  PROJIN, PROJOT, DATAIN, DATAOT, ZONEIN,
     .                  ZONEOT, IPFILE
      COMMON /SAVINP/  TSAVIN(13),TSAVIO(13)
C
      INTEGER*4        INDEF(3),IODEF(3),STATUS  
      DIMENSION        CRDIN(2),CRDIO(2),TPARIN(13),TPARIO(13)
      COMMON /MAP/      MAPPRJ
      COMMON /IJMP/     ISKIP
      DATA  IPSW / 0/
C
          RADEG =  57.2957795 
C
              IF (XIN .LT.-180.0) RETURN
              IF (XIN .GT. 180.0) RETURN
              IF (YIN .LT.-90.0)  RETURN
              IF (YIN .GT. 90.0)  RETURN
C
                 XDEG = XIN / RADEG
                 YDEG = YIN / RADEG
C       
            CRDIN(1) = XDEG   
            CRDIN(2) = YDEG  
C
       CALL GTRNZ0 (CRDIN,INDEF,TPARIN,CRDIO,IODEF,TPARIO, 
     .                   IPFLE,IFLG,1)        
C
C
C
        IF (IFLG.GT.3 .OR.IFLG.LT.0)  THEN
               IPSW = IFLG
               IP   = IFLG 
               RETURN
        ENDIF
C
        XIN  = CRDIO(1) 
        YIN  = CRDIO(2)
C
        if (ip.eq.-3) return
c
        IF (IPSW.GT.3) IP = 3
C
        CALL KWPLOT (XIN,YIN,IP)
        IPSW = 0
C
       RETURN
       END
C
C
C
C
       SUBROUTINE KWTRN2 ( XIN , YIN ,IP )
       COMMON /MAINPS/  PROJIN, PROJOT, DATAIN, DATAOT, ZONEIN,
     .                  ZONEOT, IPFILE
       COMMON /SAVINP/  TSAVIN(13),TSAVIO(13)
C
      INTEGER*4        INDEF(3),IODEF(3),STATUS  
      DIMENSION        CRDIN(2),CRDIO(2),TPARIN(13),TPARIO(13)
      COMMON /MAP/      MAPPRJ
      COMMON /IJMP/     ISKIP
      DATA  IPSW / 0/
      DATA  XOLD /0.0/
C
          RADEG =  57.2957795 
C
           IF (IP.EQ.-6) THEN
              XDEG = XIN
              YDEG = YIN
                IP = -3
               GOTO 225
            ENDIF
C
           IF (XIN .LT.-180.0) RETURN
           IF (XIN .GT. 180.0) RETURN
           IF (YIN .LT.-90.0)  RETURN
           IF (YIN .GT. 90.0)  RETURN
c
225      continue
c
           IF (ISKIP.EQ.0) THEN
                 CALL WDBCON (XIN,XDEG)
                 CALL WDBCON (YIN,YDEG)
           ELSE
                 XDEG = XIN / RADEG
                 YDEG = YIN / RADEG
           ENDIF
C
C       
            CRDIN(1) = XDEG   
            CRDIN(2) = YDEG  
C
                INDEF(1) = PROJIN   
                INDEF(2) = ZONEIN  
                INDEF(3) = DATAIN 
                IODEF(1) = PROJOT    
                IODEF(2) = ZONEOT    
                IODEF(3) = DATAOT    
                 IPFLE   = IPFILE
C
               DO 250 I  = 1 , 13
               TPARIN(I) = TSAVIN(I)
250            TPARIO(I) = TSAVIO(I)
c
       CALL GTRNZ0 (CRDIN,INDEF,TPARIN,CRDIO,IODEF,TPARIO, 
     .                   IPFLE,IFLG,0)
C
C
        IF (IFLG.GT.3 .OR.IFLG.LT.0)  THEN
               IPSW = IFLG
               IP   = IFLG 
               RETURN
        ENDIF
C
        XIN  = CRDIO(1) 
        YIN  = CRDIO(2)
C
        if (ip.eq.-3) return
c
       IPSW = 0
C
       RETURN
       END 
c
c
c
c
           SUBROUTINE WDBCON (DLAT,YD) 
C
           Y1 = DLAT 
                         ISWITCH =  1
          IF (Y1.LT.0.0) ISWITCH = -1
           Y1 = ABS(Y1)
C
          IYD =   Y1 
           YD = IYD
           YM = (Y1 - YD) * 60.
          IYM = YM
           YS = ( YM - IYM) * 60.
C
           YD = REAL(IYD * 10000)
           YM = REAL(IYM * 100 )
           YD = (YD+YM+YS) * ISWITCH
C
C
          RETURN
          END
