C       ** REVISION JML -- 4-28-92
C          ADD INTEGER*2 DEFAULT; DELETE XCENT,YCENT FROM KSETUP CALL;
C          REMOVE DECLARATION OF IBORD, ICLOR;  SPECIFY I*2 FOR KWXSCN,KWYSCN;
C
$STORAGE:2  
      SUBROUTINE KSETUP(MAPRJ,PLTWLF,PLTWRT,PLTWBT,PLTWTP,
     +           XPLTSZ,YPLTSZ,XPLTCEN,YPLTCEN,XKWMX,YKWMX)
      INTEGER*2          KWXSCN,KWYSCN
      COMMON /MAP/ MAPPRJ
C
C       ** INPUT:
C             MAPRJ.........PROJECTION   0 = MILLER   1 = X/Y SCREEN
C             PLTWLF........X-COORDINATE OF LEFT   EDGE OF PLOT AREA
C             PLTWRT........X-COORDINATE OF RIGHT  EDGE OF PLOT AREA
C             PLTWBT........Y-COORDINATE OF BOTTOM EDGE OF PLOT AREA
C             PLTWTP........Y-COORDINATE OF TOP    EDGE OF PLOT AREA
C
C       ** OUTPUT
C              XPLTSZ.......The maximum size for the longitude (veiw-port)
C              YPLTSZ.......The maximum size for the latitude  (veiw-port)
C              XPLTCEN......The translation value to move map in x-direction
C              YPLTCEN......The translation value to move map in y-direction
C              XKWMX........
C              YKWMX........
C
C
C
C      *******    CHECK MAP PROJECTION LIMITS  **********
C
         MAPPRJ = MAPRJ
         IF ((MAPPRJ.LE.0) .or. (MAPPRJ.GT.1))  MAPPRJ = 18
         IF ( MAPPRJ.EQ.1)  MAPPRJ = 25
C
C
C     ********  determine view-port area from plot-area ******
c
c     ** inquire viewport parms and fill area **
c
C       ** REVISION JML -- 4-28-92
C          CORRECT ORDER OF TOP AND BOTTOM IN CALL TO INQUIRE VIEWPORT;
C          REMOVE SETTING OF VIEWPORT        
      CALL INQVIE(VEWWLF,VEWWTP,VEWWRT,VEWWBT)
C      CALL INQVIEWPORT(VEWWLF,VEWWBT,VEWWRT,VEWWTP)
C      IBORD = 0
C      ICLOR = 6
C      CALL SETVIEWPORT(VEWWLF,VEWWBT,VEWWRT,VEWWTP,IBORD,ICLOR)
C
C     ** inquire device coordinates and scale to inches **
c
      call inqdra(kwxscn,kwyscn)
C       ** REVISION JML -- 4-28-92      
C          ADD ASPECT RATIO TO CALCULATION OF YSIZE      
      CALL INQASP(ASP)
      XSIZE  = KWXSCN / 100.
      YSIZE  = (FLOAT(KWYSCN)/ASP) / 100.
C
c     ** scale area to view port percentages / set new world coordinates***
c
      XKWLF = (VEWWLF * XSIZE)
      YKWBT = (VEWWBT * YSIZE)
      XKWRT = (VEWWRT * XSIZE)
      YKWTP = (VEWWTP * YSIZE)
      XKWMX = XKWRT - XKWLF
C       ** REVISION JML -- 4-28-92
C          CORRECT SUBTRACTION ORDER OF BOTTOM/TOP      
      YKWMX = YKWBT - YKWTP
      CALL SETWOR(0.0,0.0,XKWMX,YKWMX)
C
C     ** scale page area by user defined percentages and center point  **
C
      XPLTSZ  = XKWMX * (PLTWRT - PLTWLF)
      YPLTSZ  = YKWMX * (PLTWTP - PLTWBT)
C       ** REVISION JML -- 4-28-92
C          REMOVE CALCULATION OF XPLTCEN,YPLTCEN, XCENT,YCENT       
C      XPLTCEN = XKWMX / 2.0
C      YPLTCEN = YKWMX / 2.0
C
c     *** X/Y PROJECTION ... Position map to the user plot area limits
c
C       ** REVISION JML -- 4-28-92
C          REMOVE MAP PROJECTION CONDITION FOR CALCULATION OF XPLTCEN,YPLTCEN;
C         REMOVE CALCULATION OF XCENT,YCENT       
C      IF (MAPPRJ.EQ.25) THEN
         XPLTCEN = (XPLTSZ/2.0) + (XKWMX*PLTWLF) 
         YPLTCEN = (YPLTSZ/2.0) + (YKWMX*PLTWBT)
C      ENDIF
C
      RETURN
      END
