C
C
C
      SUBROUTINE SCLSETUP
C      
C       ** THE NAME OF THIS ROUTINE WAS CHANGED FROM SETUP TO SCLSETUP
C          BECAUSE IT CONFLICTED WITH THE ROUTINE SETUP THAT IS CALLED
C          FROM THE ADMIN PROGRAM.  THIS ROUTINE IS CALLED FROM KWSCAL.   
C          SUBROUTINE SETUP        ---- JML 7-16-92
C
      COMMON /MAINPS/   PROJIN, PROJOT, DATAIN, DATAOT, ZONEIN,
     .                  ZONEOT, IPFILE
      COMMON /SAVINP/  TSAVIN(13),TSAVIO(13)
      DIMENSION        CRDIN(2),CRDIO(2),TPARIN(13),TPARIO(13)
      INTEGER*4        INDEF(3),IODEF(3)  
C
C
           DO 250 I = 1 ,13
           TPARIN(I)= TSAVIN(I)
250        TPARIO(I)= TSAVIO(I)
C
C
                X =    0.0
                Y =    0.0
C
               CALL WDBCON (X,XDEG)
               CALL WDBCON (Y,YDEG)
               CRDIN(1) = XDEG  
               CRDIN(2) = YDEG
C
                INDEF(1) = PROJIN   
                INDEF(2) = ZONEIN  
                INDEF(3) = DATAIN 
C                
                IODEF(1) = PROJOT    
                IODEF(2) = ZONEOT    
                IODEF(3) = DATAOT    
c
                IPFILE   = 6       
c
      CALL GTRNZ0 (CRDIN,INDEF,TPARIN,CRDIO,IODEF,TPARIO, 
     .                   IPFILE,IFLG,0)
C
      RETURN
      END
