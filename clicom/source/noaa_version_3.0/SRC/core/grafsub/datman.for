$STORAGE:2
      SUBROUTINE DATMAN(INCSET)
C
$INCLUDE: 'GRFPARM.INC'
$INCLUDE: 'GRAFVAR.INC'
$INCLUDE: 'DATAVAL.INC'
$INCLUDE: 'FRMPOS.INC'
$INCLUDE: 'MODTMP.INC'
$INCLUDE: 'CURRPLT.INC'
C
      CHARACTER RTNCODE*1
C
      IACTFLG=0
   50 CONTINUE      
         IACTFLG=MAX0(IACTFLG,0)
         IF (IACTFLG.EQ.1 .AND. ISAVFLD.EQ.0) IACTFLG=0
         CALL GRAFOPTN(INCSET,IDPLT,NCA,NCB,RVAL,MXDATROW,NDATROW,
     +                 NPLTROW,CVAL,IACTFLG,RTNCODE)
         IF (RTNCODE.EQ.'1') THEN
C             .. CHANGES MADE IN GRAFOPTN WERE SAVED.  DISPLAY CURRENT VALUES
C                ON FORM IF GRAFOPTN IS RE-ENTERED  
            ISAVFLD=1
         ELSE
C             .. CHANGES MADE IN GRAFOPTN WERE NOT SAVED.  IF GRAFOPTN IS
C                RE-ENTERED, RE-INITIAL VALUES DISPLAYED ON FORM
            ISAVFLD=0
         ENDIF      
C
C          ** SET FLAGS FOR FILE POSITIONING ACTIONS
C     
         IFILACT = IACTFLG
         IACTFLG = 0
         IRDCURR = 0
         IREWB   = 0
         IREWF   = 0
         IF (IFILACT.LT.0) THEN
C             .. NEGATIVE VALUE INDICATES START/END FRAME OF CURRENT
C                BAND REVISED--REREAD BAND BEFORE REPOSITIONING FILE
C                SET FLAG TO REREAD BAND; ADDITIONAL FILE ACTIONS REMAIN
            IRDCURR = 1
            IFILACT = ABS(IFILACT)
         ENDIF  
         IF (IFILACT.GT.99) THEN
C             .. REWIND BAND 
C                SET FLAG TO REWIND BAND; ADDITIONAL FILE ACTIONS REMAIN
            IREWB = 1
            IFILACT = IFILACT - 100
         ENDIF
         IF (IFILACT.GT.9) THEN
C             .. REWIND FILE 
C                SET FLAG TO REWIND FILE; ADDITIONAL FILE ACTIONS REMAIN
            IREWF = 1
            IFILACT = IFILACT - 10
         ENDIF
         IF (IFILACT.EQ.4) THEN
C             .. START/END FRAME OF CURRENT BAND REVISED
C                SET FLAG TO REREAD BAND; REMAINING ACTION IS EXIT
            IRDCURR = 1
            IFILACT = 0
         ENDIF
C
C          ** PROCESS FILE ACTIONS
C         
         MSGTYP =1            
         NTTL=MXFRMD
         IF (IFILACT.EQ.5) THEN
C             .. ADD A FRAME TO CURRENT DATA IN MEMORY--NO ACTIONS REMAIN
            CALL ADDFRM(RTNCODE)
            IF (RTNCODE.EQ.'0') CALL WRTMSG(2,394,12,0,1,' ',0)
            IACTFLG=2
         ELSE
            IF (IRDCURR.EQ.1) THEN
C                .. START/END FRAME OF CURRENT BAND REVISED--REREAD BAND
C                   FILE ACTIONS REMAIN  
               IDATAOPT=4
               CALL GETDSET(ITYPSET,IDATAOPT,MSGTYP,NTTL,TTLSAV,
     +                      INCSET,RTNCODE)
            ENDIF
            IF (IREWF.EQ.1) THEN
C                .. REWIND FILE; READ FIRST FRAME; FILE ACTIONS REMAIN  
               IDATOPT=1 
               CALL GETDSET(ITYPSET,IDATAOPT,MSGTYP,NTTL,TTLSAV,
     +                      INCSET,RTNCODE)
            ENDIF
            IF (IREWB.EQ.1) THEN
C                .. REWIND BAND; FILE ACTIONS REMAIN  
               CALL PLTPTR(IREWB,IGRAPH,NUMCOL,LOWCOL,HICOL,PLTWID,
     +                     NCA,NCB,IDPLT)
C                .. SET FLAG TO RETURN TO GRAFOPTN 
               IF (IFILACT.GT.0) IACTFLG=-1
            ENDIF   
C
C             ** PROCESS FINAL FILE ACTION
C            
            IF (IFILACT.EQ.0) THEN
C                .. SET FLAG TO EXIT TO GRAFMAN
               IACTFLG=0
            ELSE IF (IFILACT.EQ.6) THEN
C                .. REWIND BAND            
               IREWB=1
               CALL PLTPTR(IREWB,IGRAPH,NUMCOL,LOWCOL,HICOL,PLTWID,
     +                     NCA,NCB,IDPLT)
C                .. SET FLAG TO RETURN TO GRAFOPTN (FLAG=6)
               IACTFLG=-1
            ELSE   
C                .. IFILACT IS 1,2,3 -- REPOSITION FILE/FRAME 
C                      1=REWIND FILE   2=PREVIOUS BAND   3=NEXT BAND
C                   SET FLAG TO RETURN TO GRAFOPTN              
               IDATAOPT = IFILACT
               CALL GETDSET(ITYPSET,IDATAOPT,MSGTYP,NTTL,TTLSAV,
     +                      INCSET,RTNCODE)
               IF (IACTFLG.NE.-1) IACTFLG=1
            ENDIF   
         ENDIF   
      IF (IACTFLG.NE.0) GO TO 50
C      
      RETURN
      END            
