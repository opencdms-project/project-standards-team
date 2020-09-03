C       ** REVISION JML -- 1-14-91
C          CHANGE P: TO O: IN FILE REFERENCES
C
C       *** SUBROUTINE TO CHECK ON SCRATCH FILE FOR INTERPOLATION ****
C
        SUBROUTINE KWOPSCR(NUMFIL,IFLAG)
        INTEGER*2  IFLAG,NUMFIL
C
        open (NUMFIL,file='O:\MAP\scratch.wrk',access='direct',
     +  ERR=998,form='unformatted',recl=4)
C
        IFLAG = 0
        GOTO 999
 998    IFLAG = 1
 999    CONTINUE
        RETURN
        END
