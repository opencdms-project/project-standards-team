$STORAGE:2
      SUBROUTINE WNRMAN(IWRFLG,IPCTRNG,IBARWID,NUMRNG)
C
C       ** OBJECTIVE:
C       ** INPUT:
C             IWRFLG....FLAG TO CONTROL VARIABLE THAT IS REVISED
C                       1= NUMBER OF RINGS   2=PERCENT OUTER RING  
C                       3= WIDTH OF SMALLEST BAR
C             IPCTRNG...PERCENT OUTER RING
C             IBARWID...WIDTH OF SMALLEST BAR
C             NUMRNG....NUMBER OF RINGS
C       ** OUTPUT:
C             IPCTRNG...REVISED PERCENT OUTER RING IF IWRFLG=2
C             IBARWID...REVISED WIDTH OF SMALLEST BAR IF IWRFLG=3
C             NUMRNG....REVISED NUMBER OF RINGS IF IWRFLG=1
C
      REAL*4        YWNERR
      CHARACTER*4   MSGTXT
      CHARACTER*3   INCHAR
      CHARACTER*6   CHRFMT
      DATA YWNERR/.98/
C
      CALL XYWNDO(XWINLF,YWINTP,XWINRT,YWINBT)
C      
   50 CONTINUE   
      IF (IWRFLG.EQ.1) THEN
C          .. NUMBER OF WINDROSE RINGS      
         MSGN1=565
         MSGN2=566
         WRITE(MSGTXT,503) NUMRNG
         NCHTXT = 3
         MXCHAR = 2
      ELSE IF (IWRFLG.EQ.2) THEN
C          .. PERCENT OF OUTER RING      
         MSGN1=569
         MSGN2=570
         WRITE(MSGTXT,504) IPCTRNG
         NCHTXT = 4
         MXCHAR = 3
      ELSE IF (IWRFLG.EQ.3) THEN
C          .. WIDTH OF SMALLEST BAR      
         MSGN1=573
         MSGN2=574
         WRITE(MSGTXT,502) IBARWID
         NCHTXT = 2
         MXCHAR = 1
      ELSE
C          .. ILLEGAL FLAG VALUE
         GO TO 100
      ENDIF            
      CALL GRAFMSG(XWINRT+.01,YWINBT,MSGN1,MSGN2,MSGTXT,NCHTXT,
     +             3,MXCHAR,INCHAR,NCHAR)
      IF (INCHAR.NE.'ES') THEN
         WRITE(CHRFMT,520) NCHAR
         READ(INCHAR,CHRFMT) ITEMP
         MSGTXT=' '
         NCHTXT=0
         IF (IWRFLG.EQ.1) THEN
            IF (ITEMP.LT.1  .OR. ITEMP.GT.10) THEN
               MSGN1=567
            ELSE IF (ITEMP.GT.IPCTRNG) THEN
               MSGN1=568
               WRITE(MSGTXT,504) IPCTRNG
               NCHTXT = 4
            ELSE
               MSGN1=0
               NUMRNG = ITEMP
            ENDIF   
         ELSE IF (IWRFLG.EQ.2) THEN      
            IF (ITEMP.LT.1 .OR. ITEMP.GT.100) THEN
               MSGN1=571
            ELSE IF (ITEMP.LT.NUMRNG) THEN
               MSGN1=572
               WRITE(MSGTXT,503) NUMRNG
               NCHTXT = 3
            ELSE
               MSGN1=0
               IPCTRNG = ITEMP
            ENDIF   
         ELSE      
C             .. WIDTH OF SMALLEST BAR            
            IF (ITEMP.LT.1 .OR. ITEMP.GT.3) THEN
               MSGN1=575
            ELSE
               MSGN1=0
               IBARWID = ITEMP
            ENDIF   
         ENDIF   
         IF (MSGN1.GT.0) THEN
            MSGN2=202
            CALL GRAFNOTE(XWINLF,YWNERR,MSGN1,MSGN2,MSGTXT,NCHTXT,
     +                    INCHAR)
            GO TO 50
         ENDIF
      ENDIF   
  100 RETURN
C
C       ** FORMAT STMTS
C
  501 FORMAT(I1)
  502 FORMAT(I2)
  503 FORMAT(I3)
  504 FORMAT(I4)
  520 FORMAT('(I',I1,')')
      END