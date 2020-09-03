C
C
C
       SUBROUTINE KWINTC(XMNLN,YMNLT,XMXLN,YMXLT,IX,IY,SAMPLE,
     +             SMOOTH,PLTINC,CHRHT,FONTCK)
C
       INTEGER*2       IX,IY,FONTCK
       COMMON /kwmnmx/ XMNLON,YMNLAT
       COMMON /KWCHAR/ CHARHT,FONTPS,IDECPL,LABCHK
       COMMON /KCTDST/  DSTPAS
C
       CHARHT = CHRHT
       FONTPS = FONTCK
C
       XMNLON = XMNLN
       YMNLAT = YMNLT
       DSTPAS = PLTINC
C
            CALL KW1P (1,3) 
C
            XL = 0.0  
            XR = ABS (XMXLN - XMNLN)
            XB = 0.0 
            XT = ABS (YMXLT - YMNLT)
C
            CALL KWSUBJ (XL,XB,XR,XT)
c
c      ** changing the object space from 0-1 increases the ***
c         points on the contour line. 
c                                                    
            CALL KWOBJG ( 0.0 ,0.0 , 0.25,0.25)
c
            CALL KWMODC
C
            CALL KWPARM(2,REAL(IX))  
            CALL KWPARM(3,REAL(IY)) 
            CALL KWPARM(4,REAL(IX))  
            CALL KWPARM(5,REAL(IY))  
C ADD 2/4/91
            CALL KWPARM(10,XT)
            CALL KWPARM(11,XR)
C
            CALL KWPARM(19,SAMPLE)
            CALL KWPARM(21,PLTINC)
            CALL KWPARM(24,SMOOTH)
ccc            CALL KWPARM(33,0.0)
            CALL KWPARM(48,0.0)  
C
       
         RETURN
         END
