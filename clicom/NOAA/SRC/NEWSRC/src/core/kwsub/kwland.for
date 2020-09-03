C
C
C
       SUBROUTINE KWLAND (XMIN,YMIN,XMAX,YMAX,ICODE,IFLAG)
       INTEGER*2         ICODE,IFLAG
       CHARACTER*72      FILE1,FILE2
       COMMON /KWCM  /   CETLON
       COMMON /KDSTCK/   IJMP,PLTDST
       COMMON /KWCLIPXY/ XOLD,YOLD
C
         IJMP = 1
         pltdst = 0.9
C         
C          ** INITIAL XOLD,YOLD IN KWCLIP TO THE CURRENT GRAPHICS CURSOR 
C             POSITION
         CALL BGKWCLIP
c
c        ** fix 10/24/90  KRW
C
         if (cetlon.eq.0.0) then
             pltdst = 1.0
         endif
C
         XDIST = ABS(XMAX-XMIN)
         IF (XDIST.LE.10) PLTDST = 5.0
C
C        ************************************             
c
         IFLAG = 0
C
C      *** ADD NEW SUBROUTINE TO ALLOW OPENING DATA POINTER FILE ***
C
         CALL KWOPLND(ICODE,IFLAG)
         IF (IFLAG.NE.0) GOTO 888
C
CC1/1/91        IF (ICODE.EQ.1) 
CC1/1/91     +  OPEN (12,FILE='P:\MAP\COAST.FLE',ERR=888,STATUS='OLD')
CC1/1/91        IF (ICODE.EQ.2) 
CC1/1/91     +  OPEN (12,FILE='P:\MAP\RIVERS.FLE',ERR=888,STATUS='OLD')
CC1/1/91        IF (ICODE.EQ.3)
CC1/1/91     +  OPEN (12,FILE='P:\MAP\BOUNDARY.FLE',ERR=888,STATUS='OLD')
CC1/1/91        IF (ICODE.EQ.4)
CC1/1/91     +  OPEN (12,FILE='P:\MAP\LAKES.FLE',ERR=888,STATUS='OLD')
CC1/1/91        IF (ICODE.EQ.5) 
CC1/1/91     +  OPEN (12,FILE='P:\MAP\STATES.FLE',ERR=888,STATUS='OLD')
C
C
        ICMCHK = 0
        IF (CETLON.NE.0.0) THEN
          IF ((CETLON.LT.XMIN).AND.(CETLON.LT.XMAX)) ICMCHK = 1
          IF ((CETLON.GT.XMIN).AND.(CETLON.GT.XMAX)) ICMCHK = 1
             IF (CETLON.LT.0.0) THEN
                IF ((XMIN.LE.0.0).AND.(XMAX.LE.0.0))   ICMCHK = 0
                IF ((XMIN.GE.0.0).AND.(XMAX.GE.0.0))   ICMCHK = 0
             ENDIF
             IF (CETLON.GE.0.0) THEN
                IF ((XMIN.LE.0.0).AND.(XMAX.LE.0.0))   ICMCHK = 0
                IF ((XMIN.GE.0.0).AND.(XMAX.GE.0.0))   ICMCHK = 0
             ENDIF
        ENDIF
C     
C
       READ (12,*,END=999,ERR=999) IOUTER
c
        DO 100 LOOP=1,iouter
C
        READ(12,55,END=999,ERR=999) FILE1
        READ(12,55,END=999,ERR=999) FILE2
        READ(12,* ,END=999,ERR=999) ixsw,iysw,XMN,XMX,YMN,YMX
  55    FORMAT(A72)
C
       IF ((CETLON.NE.0.0).AND.(ICMCHK.EQ.1)) GOTO 450
C
        IF (XMX.LT.XMIN) GOTO 100
        IF (XMN.GT.XMAX) GOTO 100
        IF (YMX.LT.YMIN) GOTO 100
        IF (YMN.GT.YMAX) GOTO 100
            GOTO 500
C
 450    CONTINUE
C
        IF (YMX.LT.YMIN) GOTO 100
        IF (YMN.GT.YMAX) GOTO 100
C
            IF (XMN.LT.XMIN) GOTO 500
            IF (XMX.LT.XMIN) GOTO 500
            IF (XMN.GT.XMAX) GOTO 500
            IF (XMX.GT.XMAX) GOTO 500
        GOTO 100
C
 500    CONTINUE
CCCC        WRITE (*,*) 'KWLAND ',CETLON,XMN,XMX,XMIN,XMAX,ICMCHK
C
C       **** 15 = INDEX FILE
C       **** 20 = WDB2  FILE
C
        OPEN (15,FILE=FILE2,ACCESS='DIRECT',
     .  FORM='UNFORMATTED', RECL=24)   
        OPEN (20,FILE=FILE1,ACCESS='DIRECT',
     .  FORM='UNFORMATTED', RECL=512)
C
        XTIMES = IXSW
        YTIMES = IYSW
C
        CALL REDCOM (XMAX,XMIN,YMAX,YMIN,XTIMES,YTIMES,ICMCHK)
C
100     CONTINUE
         GOTO 999
C
888      CONTINUE
         IFLAG = 1
C
 999    CONTINUE
        CLOSE (12)
        CLOSE (15)
        CLOSE (20)
        IJMP = 0
        RETURN
        END
C
C
C
C
C
        SUBROUTINE REDCOM (XMAX,XMIN,YMAX,YMIN,XTIMES,YTIMES,ICMCHK)
        COMMON /KWCM  /   CETLON
C
C
C       PROGRAM READS IN THE COMPRESSED WDB2 FORMAT.
C       LINE SEGMENTS ARE REFERENCED BY A RECORD AND INDEX NUMBER.
C       
C
        INTEGER*2 STORE
        COMMON /RECPAS/ L,IREC,INDEX,IRCLST,STORE(256)
C
C
CC
        IRCLST = 0
             L = 256
            DD = 10000.0
           IP3 = 3
           IP2 = 2
          INDX = 0
C 
100     continue
                INDX = INDX + 1
        READ (15,REC = INDX)    IREC,INDEX,XMN,XMX,YMN,YMX 
        IF (IREC.LE.0) GOTO 999
C
C    ***   CHECK MAX/MIN OF EACH LINE SEGMENTS ****
C
C
C
        
       IF ((CETLON.NE.0.0).AND.(ICMCHK.EQ.1)) GOTO 450
C
        IF (XMX.LT.XMIN) GOTO 100
        IF (XMN.GT.XMAX) GOTO 100
        IF (YMX.LT.YMIN) GOTO 100
        IF (YMN.GT.YMAX) GOTO 100
            GOTO 500
C
 450    CONTINUE
C
        IF (YMX.LT.YMIN) GOTO 100
        IF (YMN.GT.YMAX) GOTO 100
            IF (XMN.LT.XMIN) GOTO 500
            IF (XMX.LT.XMIN) GOTO 500
            IF (XMN.GT.XMAX) GOTO 500
            IF (XMX.GT.XMAX) GOTO 500
        GOTO 100
C
C
 500    CONTINUE
C                        
C        ****   READ IN INDEX NUMBER   ******
C
        IF (IRCLST.NE.IREC) READ (20,rec=IREC) (STORE(K), K=1,L )
        IRCLST = IREC
C
C       *****          FIND 1st XDEG / YDEG VALUES *********
C
            J = INDEX - 1
         CALL RECOVER (J,A)
         XDEG = A - DD
         CALL RECOVER (J,B)
           XX = (XDEG + B / DD) * XTIMES

         CALL RECOVER (J,A)
         YDEG = A - DD
         CALL RECOVER (J,B)
           YY = (YDEG + B / DD) * YTIMES
C
C
        CALL LINSEG (XX,YY,IP3)
C
C
C   ***  CONVERT EACH POINT BACK TO ORIGINAL VALUE BY CALLING
C   ***  RECOVER. CHECK THE VALUE OF A TO SEE...
C   ***          A = 31000  = END OF LINE SEGMENT   
C   ***          A >  9999  = DEGREE VALUE CHANGE
C   ***          A <  9999  = FRACTIONAL PART
C   
C
110    CALL RECOVER (J,A)
C
       IF (A.EQ.31000) GOTO 100
C
       IF (A.GT.9999.0) THEN  
           XDEG = A - DD
           CALL RECOVER(J,A) 
       ENDIF
C                                  
       CALL RECOVER (J,B)   
       IF (B.GT.9999.0) THEN
           YDEG = B - DD
           CALL RECOVER(J,B)
       ENDIF
C
       XX = (XDEG + A / DD) * XTIMES
       YY = (YDEG + B / DD) * YTIMES
C
      CALL LINSEG (XX,YY,IP2)
C
       GOTO 110
C
 999   CONTINUE
       RETURN
       END
C
C
C      *******     WRITE POINTS OUT  ********
C
       SUBROUTINE LINSEG (XX,YY,IP)
C
       CALL KWTRAN (XX,YY,IP)
C
       RETURN
       END
C
C
C      *********   CONVERT POINTS ***********
C
       SUBROUTINE RECOVER(J,A)
       INTEGER*2 STORE
       COMMON /RECPAS/ L,IREC,INDEX,IRCLST,STORE(256)
C
C
           J = J + 1
       IF (J.GT.L) THEN
             IREC = IREC + 1
             READ (20,rec=IREC) (STORE(K) , K = 1 , L )
                J = 1
           IRCLST = IREC
       END IF
C
       A = STORE(J)
C
       IF (A.EQ.31000) RETURN
C
       RETURN
       END 
 
******************************************************************************
C
      SUBROUTINE BGKWCLIP
C
C       ** THIS ROUTINE DETERMINES THE CURRENT GRAPHICS CURSOR LOCATION AND
C          USES THE POSITION TO INITIAL THE VALUE OF XOLD/YOLD IN KWCLIP.
C          CURRENTLY THIS ROUTINE IS CALLED ONLY FROM KWLAND.  PREVIOUSLY
C          KWCLIP ALWAYS RETAINED THE LAST CURSOR POSITION THAT IT DEFINED
C          AS THE CURRENT CURSOR POSTION.  HOWEVER, ONCE KWLAND IS EXITED
C          IT IS POSSIBLE THAT A ROUTINE OTHER THAN KWCLIP CAN CHANGE THE
C          GRAPHICS CURSOR POSITION.  THIS ROUTINE MAKES SURE THAT KWCLIP
C          HAS THE CURRENT POSITION OF THE GRAPHICS CURSOR BEFORE THE MAP
C          LINE SEGEMNTS ARE STARTED.
C
      COMMON /KWCLIPXY/  XOLD,YOLD
      COMMON /PLOTRN/   XSCNPG,YSCNPG,XPLTPG,YPLTPG,SCNFLP
C
C       ** REVISION JML 3-9-94 -- INQGCUR TO INQGCU
      CALL INQGCU(XS,YS,ICOLOR)
      XOLD = XS - XSCNPG
      YOLD = YS - YSCNPG
      RETURN
      END
