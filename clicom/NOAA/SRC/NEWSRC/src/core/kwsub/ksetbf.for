C
C   These subroutine set the flag to either write to disk
c   or store in the array /KWMID3/ NUMPTS,STOR(2,NUMPTS)
C   If writing to disk, set numpts = 1 in main subroutine.
C
       SUBROUTINE KSETBF(ICHECK,IUNIT)
       INTEGER*2  ICHECK,IUNIT,IFLG,NUMUNT
       COMMON /KSAVBF/ IFLG,NUMUNT
              IFLG = ICHECK
            NUMUNT = IUNIT
       RETURN
       END
C
C
       SUBROUTINE KINQBF(ICHECK,IUNIT)
       INTEGER*2  ICHECK,IUNIT,IFLG,NUMUNT
       COMMON /KSAVBF/ IFLG,NUMUNT
            ICHECK =  IFLG 
            IUNIT  =  NUMUNT
       RETURN
       END
       
       