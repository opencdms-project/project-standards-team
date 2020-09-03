       SUBROUTINE KSETDEV
       COMMON /DEVPRM/ DEVICE(2,10)
C
       XMN = 0.0
       YMN = 0.0
       XMX = 638.0
       YMX = 348.0
       DEVICE(1,1) = XMN
       DEVICE(1,2) = YMX
       DEVICE(1,3) = XMX
       DEVICE(1,4) = YMX
       DEVICE(1,5) = XMX - XMN
       DEVICE(1,6) = YMX - YMN
C
       RETURN
       END
