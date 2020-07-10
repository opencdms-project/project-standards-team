$STORAGE:2

      SUBROUTINE AQC1(NUMREC)
C
C   ROUTINE TO READ THE TEMPORARY WORK FILE(TWF) , REFORMAT IT, AND
C     WRITE IT TO AN AREA-FORMAT DISK FILE. 
C
$INCLUDE: 'VAL1.INC'
$INCLUDE: 'INDEX.INC'
$INCLUDE: 'AREAQC.INC'

      CHARACTER LAT*7, LON*8, STNABRV*24
C
      READ(SRCHKEY,'(I3,I4,2I2)') DSETID,YEAR,MONTH,DAY
      CALL OPENAQC(ELMFILE,NRECL,STNFILE,1,'NEW')
C
C   INITIALIZE ELEMENTS OBSERVED SO SYSTEM WON'T FLAG STATIONS THAT 
C   DON'T OBSERVE ALL ELEMENTS
C
      DO 90 I1 = 1,MAXELEM
         RELELEM(I1) = -8888
  90  CONTINUE
C
C   POSITION ELEMFILE(61) TO RECORD 2 TO LEAVE SPACE FOR NUMBER OF 
C   RECORDS IN RECORD NUMBER 1
C
      IDUMMY = 0
      WRITE(61) IDUMMY     
C
C   MAIN LOOP - READ, SCALE AND REFORMAT THE DATA.  
C
      NUMREC = 0
      ISTN = 0
      CALL LOCATE(10,5,IERR)
      CALL WRTSTR('Loading - ',10,11,0)
      DO 450 I = BGNIDX,NUMIDX
         READ(19,REC=I) DELKEY,STNID,INKEY,RECNUM
         IF (DELKEY.LT.2.OR.INKEY.NE.SRCHKEY) THEN
             GO TO 450
         END IF
         READ(20,REC=RECNUM) IDKEY,((VALARRAY(I1,J1),I1=1,NUMELEM)
     +        ,J1=1,NUMLINE),(((FLAGARRAY(K,L,M),M=1,2),K=1,NUMELEM)
     +        ,L=1,NUMLINE)
         CALL LOCATE(10,15,IERR)
         CALL WRTSTR(STNID,8,11,0)
C
C   FIND THE NAME AND LOCATION OF THIS STATION, CONVERT THE LAT AND LON
C   TO DEGRESS AND TENTHS AND WRITE IT ALL TO THE ELEMQC FILE
C
         EYEAR = YEAR
         EMONTH = MONTH
         EDAY = DAY
         SRCHDATE = EYEAR*10000. + EMONTH*100. + EDAY
         RTNCODE = ' '
         CALL RDGEOG(STNID,SRCHDATE,STNABRV,COUNTRY,LAT,LON
     +        ,ELEV,RTNCODE)
         IF (RTNCODE.EQ.'1')THEN
            MSGNUM = 75
            MESSAGE = STNID 
         ELSE IF (RTNCODE.EQ.'2')THEN
            MSGNUM = 74
            MESSAGE = STNID
         ELSE
            MSGNUM = 85
            MESSAGE = ' '
         END IF
         IF (RTNCODE.NE.'0')THEN
            CALL WRTMSG(3,MSGNUM,12,1,1,MESSAGE,8)
            IF (RTNCODE.EQ.'2') THEN
               GO TO 450
            END IF
         END IF
         CALL LAT2REAL(LAT,LON,YCOORD,XCOORD)
         ISTN = ISTN + 1
         WRITE(60) ISTN,XCOORD,YCOORD,STNID
C
C   FIND THE MEANS AND STANDARD DEVIATIONS FOR THIS STATION IF WANTED
C
         KEYQC(1:8) = STNID
         CALL RDLIMS(KEYQC,'A',RTNCODE)
         IF (RTNCODE.EQ.'1') THEN
            RETURN
         END IF
C
C   SCALE AND WRITE THE DATA TO THE OUTPUT ELEMENT FILE
C
         CALL WRTQC(NUMREC)

  450 CONTINUE
C
C   NORMAL END - CLOSE FILES (PASS RTNCODE = 'C' TO RDGEOG TO CLOSE 35)
C
      CLOSE(19)
      CLOSE(20)
      CLOSE(31)
      RTNCODE = 'C'
      CALL RDGEOG(STNID,SRCHDATE,STNABRV,COUNTRY,LAT,LON
     +        ,ELEV,RTNCODE)
      CLOSE(60)
      WRITE(61,REC=1) NUMREC
      CLOSE(61)
      RETURN
      END
************************************************************************
$PAGE
      SUBROUTINE AQC2(IELEM,ILINE)
C
C   ROUTINE TO READ THE TEMPORARY ELEMENT FILE(\DATA\QCELEM.AQC) AND
C     CONTOUR IT BY DISPLAYING THE STN LOCATIONS IN A COLOR-CODE
C     WHICH DEPENDS ON THE VALUE FOR THE STATION.
C
$INCLUDE: 'VAL1.INC'
$INCLUDE: 'ELEMCHKS.INC'
$INCLUDE: 'AREAQC.INC'
C      CHARACTER*1 HALOID
      CHARACTER*1 ERRFLG
      CHARACTER*3 ELEMCODE,DEVERS
      CHARACTER*12 NAMEXT
      CHARACTER*20 HLDTEXT
C      CHARACTER*32 ENVFILE,HALDEV
      CHARACTER*78 MSGLIN
      INTEGER*2 QCCOL
      INTEGER*2 IDUM1,IDUM2(16)
      LOGICAL CHGMOD,FRSTCL
      REAL VALUE(5)
C
      DATA ELEMCODE /'  '/, FRSTCL /.TRUE./
C
C   ON FIRST CALL INITIALIZE MESSEGES AND HALOID
C
      IF (FRSTCL) THEN
         FRSTCL = .FALSE.
         CALL GETDEASE(DEVERS)
         IF (DEVERS.EQ.'4.0') THEN
            NMSGDE=499
         ELSE
            NMSGDE=498
         ENDIF
      END IF
C      
C  OPEN THE INPUT FILES
C
      CALL OPENAQC(ELMFILE,NRECL,STNFILE,2,'OLD')
C
C   REQUEST THE SPECIFIC ELEMENT TO BE CONTOURED AND FIND ITS PLACE
C   IN THE SETUP FILE AND IN THE QCELEM FILE
C
  100 CONTINUE
      CHGMOD = .TRUE.
      CHGVAL = .FALSE.
      CALL CLS
      IROW = 5
      CALL LOCATE(IROW,1,IERR)
      CALL WRTSTR(MSGLN(2),60,14,0)
      CALL GETSTR(3,ELEMCODE,3,15,1,RTNFLAG)
      IF (RTNFLAG.EQ.'4F') THEN
         GO TO 900
      END IF
C 
      READ(ELEMCODE,'(I3)') JELEM
      DO 130 I1 = 1,NUMELEM
         IF (TBLELEM(I1).EQ.JELEM) THEN
            ELMNAM = TBLEABRV(I1)
            IELEM = I1
            GO TO 140
         END IF
  130 CONTINUE
  140 CONTINUE         
      DO 150 I1 = 1,NUMQC
         IF (JELEM.EQ.AQCELEM(I1)) THEN
            QCCOL = I1
            GO TO 160
         END IF
  150 CONTINUE
      CALL WRTMSG(3,324,12,1,1,' ',0)
      GO TO 100
  160 CONTINUE
C
C   DETERMINE THE PLOT COLOR CATEGORIES
C 
      IF (SCLELEM(QCCOL).GT.0) THEN
         IF (SCLELEM(QCCOL).LE.100) THEN
            VALCATS(1) = -1.5
            VALCATS(2) = -1.0
            VALCATS(3) = -0.5
            VALCATS(4) = 0.5
            VALCATS(5) = 1.0
            VALCATS(6) = 1.5
         ELSE   
            ICHK = SCLELEM(QCCOL) - 100
            I1 = ELMCOL(QCCOL)
            STPVAL = (CHKVL2(I1,ICHK) - CHKVL1(I1,ICHK)) / 5.0
            VALCATS(1) = CHKVL1(I1,ICHK)
            VALCATS(2) = VALCATS(1) + STPVAL
            VALCATS(3) = VALCATS(2) + STPVAL
            VALCATS(4) = VALCATS(3) + STPVAL
            VALCATS(5) = VALCATS(4) + STPVAL
            VALCATS(6) = CHKVL2(I1,ICHK)
         END IF
      ELSE
         CALL WRTMSG(3,324,12,1,1,' ',0)
         GO TO 100
      END IF
C
C   REQUEST THE SPECIFIC TIME TO BE SELECTED
C
      WRITE(INDATE,'(I2)') ILINE
  200 CONTINUE
      IROW = 10
      CALL LOCATE(IROW,1,IERR)
      CALL WRTSTR(MSGLN(3),49,14,0)
      CALL GETSTR(3,INDATE,2,15,1,RTNFLAG)
      IF (RTNFLAG.EQ.'4F') THEN
         GO TO 100
      END IF
      READ(INDATE,'(BN,I2)') ILINE
      IF (ILINE.LT.0.OR.ILINE.GT.NUMLINE) THEN
         CALL LOCATE(20,1,IERR)
         CALL WRTMSG(3,326,12,1,1,' ',0)
         GO TO 200
      END IF
C
C   OPEN AND INITIALIZE HALO ENVIRONMENT 
C
      CALL BGNHALO(10,IDUM1,IDUM2)      
C       .. IN ORDER TO OPEN THE VIEWPORT TO THE ENTIRE SCREEN, A MAX VALUE
C          EQUAL TO .999 MUST BE USED.  A VALUE OF 1.0 IS A SPECIAL SIGNAL FOR
C          HALO TO 'TURN OFF' THE VIEWPORT WHICH DOES NOT RESET ASPECT RATIOS
      CALL SETVIE(0.0,0.0,0.999,0.999,-1,0)
      CALL INQDRA(MXPCOL,MXPROW)
      CALL SETHAT(1)

      IF (MXPROW.GT.200) THEN
         IMODE = 2
       ELSE
         IMODE = 1
      END IF
  220 CONTINUE
C
C   COPY THE SAVED COUNTRY OUTLINE TO THE SCREEN AND SET THE WORLD COORDS
C     
      CALL DRWMAP(XLONMN,XLATMN,XLONMX,XLATMX,
     +            XMIN1,YMIN1,XMAX1,YMAX1,NAMEXT,ERRFLG)
      IF (ERRFLG.NE.'0') THEN
C          .. ERROR:  FILE CONTAINING MAP SCREEN COULD NOT BE OPENED
         CALL CLOSEG
         CALL WRTMSG(4,196,12,0,0,'O:DATA\'//NAMEXT,20)
         CALL WRTMSG(3,599,12,1,1,' ',0)
         GO TO 900
      ENDIF
      CALL SETWOR(XMIN1,YMIN1,XMAX1,YMAX1)
C
C   SET COLORS AND BOX SIZES TO BE USED FOR DISPLAY DEPENDING UPON 
C   GRAPHICS MODE BEING USED
C
      CALL INQDRA(IMAX,JMAX)
      XWIDTH = (XMAX1 - XMIN1) / FLOAT(IMAX)
      IF (IMODE.EQ.1) THEN
         PLTSIZ(1) = 6.*XWIDTH
         PLTSIZ(2) = 4.*XWIDTH
         PLTSIZ(3) = 2.*XWIDTH
         PLTSIZ(4) = 2.*XWIDTH
         PLTSIZ(5) = 2.*XWIDTH
         PLTSIZ(6) = 4.*XWIDTH
         PLTSIZ(7) = 6.*XWIDTH
         PLTCLR(1) = 1
         PLTCLR(2) = 1
         PLTCLR(3) = 1
         PLTCLR(4) = 3
         PLTCLR(5) = 2
         PLTCLR(6) = 2
         PLTCLR(7) = 2
      ELSE   
         DO 250 I3 = 1,7
            PLTSIZ(I3) = 9.*XWIDTH
            PLTCLR(I3) = I3
  250    CONTINUE               
         DO 260 I3 = 1,9
            CALL SETXPA(I3,EGAREG(I3))
  260    CONTINUE
      END IF
C
C  DRAW THE COLOR AND COMMAND KEY FOR THE USER
C
      CALL MOVTCA(XMIN1,YMIN1)
      CALL SETTEX(1,1,0,0)
      CALL SETTCL(4,0)
      CALL TEXT('!!')
      DO 360 I2 = 1,7
         CALL SETCOL(PLTCLR(I2))
         X = XMIN1 + FLOAT(I2)*PLTSIZ(1)
         CALL BAR(X,YMIN1,X+PLTSIZ(I2),YMIN1+PLTSIZ(I2))
  360 CONTINUE
      CALL SETTCL(4,0)
      X = XMIN1 + 8.*PLTSIZ(1)
      CALL MOVTCA(X,YMIN1)
      CALL TEXT('!!')
      XPOS = XMIN1 + (XMAX1-XMIN1) * .20
      CALL MOVTCA(XPOS,YMIN1)      
      CALL TEXT('!Time=!')
      HLDTEXT = '!'//INDATE(1:2)//'!'
      CALL TEXT(HLDTEXT)
      CALL TEXT('!  Elem=!')
      HLDTEXT = '!'//ELMNAM//'!'
      CALL TEXT(HLDTEXT)
      IF (IMODE.EQ.2) THEN
         CALL GETMSG(NMSGDE,MSGLIN)
         CALL GETMSG(999,MSGLIN)
         LGTH = LNG(MSGLIN)
         XPOS = XMIN1 + (XMAX1-XMIN1) * .5
         CALL MOVTCA(XPOS,YMIN1)      
         CALL GRFTXT(LGTH,MSGLIN,4,0)
         CALL DELTCU
      END IF
C
C   READ AND PLOT THE DATA VALUES IN THE APPROPRIATE COLOR
C
      IPLOT = 0
      DO 385 I1 = 1,MAXQVAL
         HLDVAL(I1) = -99999.
  385 CONTINUE         
      REWIND(61)
      READ (61) NUMREC
      DO 400 I2 = 1,NUMREC
         READ(61,END=410) J1,XCOORD,YCOORD,ISTN,(VALUE(I1),I1=1,NUMQC)
         IF (J1.NE.ILINE) THEN
            GO TO 400
         END IF
         IF (ISTN.GT.MAXQVAL) THEN
            WRITE(*,*)
     +       'TOO MANY STATIONS FOUND - INCREASE MAXQVAL IN AREAQC.INC'
            WRITE(*,*) ' RECOMPILE AREAQC AND AQCINF AND RELINK DATAQC'
            PAUSE
            STOP
         END IF   
         IF (XCOORD.GE.XLONMN.AND.XCOORD.LE.XLONMX.AND.YCOORD.GE.XLATMN
     +        .AND.YCOORD.LE.XLATMX.AND.VALUE(QCCOL).NE.-99999.) THEN
            HLDVAL(ISTN) = VALUE(QCCOL)
            IPLOT = IPLOT + 1
            CALL PLTVAL(VALUE(QCCOL),XCOORD,YCOORD,VALCATS
     +          ,PLTSIZ,PLTCLR)
         END IF
  400 CONTINUE
  410 CONTINUE
      IF (IPLOT.LT.1) THEN
         CALL SETTEX(2,1,0,0)
         CALL MOVTCA(XMIN1+3.*PLTSIZ(1),YMIN1+(YMAX1-YMIN1)*.1)
         CALL GETMSG(65,MSGLIN)
         CALL GETMSG(999,MSGLIN)
         LGTH = LNG(MSGLIN)
         CALL GRFTXT(LGTH,MSGLIN,7,0)
         CALL SETTEX(1,1,0,0)
      ENDIF   
      XPOS = XMIN1 + (XMAX1-XMIN1) * .18
      YPOS = YMIN1 + PLTSIZ(1)
      CALL MOVHCA(XPOS,YPOS)
      CALL ORGLOC(XPOS,YPOS)
C
C   SOLICIT THE NEXT COMMAND FROM THE USER
C
  610 CONTINUE
      CALL RDLOC(XPOS,YPOS,INCHAR,RTNCODE)
      IF (ZXPOS.NE.XPOS.OR.ZYPOS.NE.YPOS) THEN
         ZXPOS = XPOS
         ZYPOS = YPOS
         CALL MOVHCA(XPOS,YPOS)
      END IF
      IF (INCHAR.EQ.'XX' .OR. INCHAR.EQ.'YY') THEN
         GO TO 610
      END IF                
C
C   A CONTROL COMMAND HAS BEEN ENTERED
C
C   F2 - RETURN TO ASK USER FOR ELEMENT AND TIME
C
      IF (INCHAR.EQ.'2F') THEN
         CALL CLOSEG
         CALL SETMOD(3,IERR)
         GO TO 100
C
C   F3 - ADD 1 TO TIME AND PLOT NEW DATA
C
      ELSE IF (INCHAR.EQ.'3F') THEN
         ILINE = ILINE + 1
         IF (ILINE.GT.NUMLINE) THEN
            ILINE = 1
            INDATE = ' 1'
            CALL CLOSEG
            CALL SETMOD(3,IERR)
            GO TO 100
         ELSE
            WRITE(INDATE,'(I2)') ILINE
            CALL SETCOL(0)
            CALL CLR
            CHGMOD = .TRUE.
            GO TO 220         
         END IF
C
C   SHIFT-F3 - SUBTRACT 1 FROM TIME AND PLOT NEW DATA
C
      ELSE IF (INCHAR.EQ.'3S') THEN
         ILINE = ILINE - 1
         IF (ILINE.LT.1) THEN
            ILINE = 1
            INDATE = ' 1'
            CALL CLOSEG
            CALL SETMOD(3,IERR)
            GO TO 100
         ELSE
            WRITE(INDATE,'(I2)') ILINE
            CALL SETCOL(0)
            CALL CLR
            CHGMOD = .TRUE.
            GO TO 220         
         END IF
C
C   F4 - EXIT
C
      ELSE IF (INCHAR.EQ.'4F') THEN
         CALL CLOSEG
         GO TO 900
C
C   F8 OR SPACE - PRINT INFORMATION FOR THE STATION INDICATED
C
      ELSE IF (INCHAR.EQ.'8F' .OR. INCHAR.EQ.'  ' .OR.
     +         (RTNCODE.EQ.'1'.AND.INCHAR.EQ.'RE')) THEN
         XLOW = XPOS - PLTSIZ(1)/2.
         XHIGH = XLOW + PLTSIZ(1)
         YLOW = YPOS - PLTSIZ(1)/2.
         YHIGH = YLOW + PLTSIZ(1) 
         REWIND(60)
         READ(60) IDUM
         DO 750 I = 1,9999
            READ(60,END=760) ISTN,XCOORD,YCOORD,STNID
            IF (XCOORD.GE.XLOW.AND.XCOORD.LE.XHIGH.AND.
     +             YCOORD.GE.YLOW.AND.YCOORD.LE.YHIGH) THEN
               CALL AQCINF(MXPCOL,MXPROW,XMIN1,XMAX1,ELEMCODE
     +             ,ILINE,CHGMOD)
               CALL SETWOR(XMIN1,YMIN1,XMAX1,YMAX1)
               IF (CHGVAL) THEN
                  CALL CLOSEG
                  GO TO 900
               ELSE
                  GO TO 760
               END IF
            END IF
  750    CONTINUE
  760    CONTINUE
      END IF
      CALL MOVHCA(XPOS,YPOS)
      CALL ORGLOC(XPOS,YPOS)
      GO TO 610 
C
C   CLOSE FILES AND STOP 
C
  900 CONTINUE
      CALL SETMOD(3,IERR)
      CLOSE (40)
      CLOSE (60)
      CLOSE (61)
      RETURN
      END
************************************************************************ 

      SUBROUTINE PLTVAL(VALUE,XCOORD,YCOORD,VALCATS,PLTSIZ,PLTCLR)
C
C   ROUTINE TO PLOT THE DATA VALUE ON THE SCREEN IN THE APPROPRIATE
C   LOCATION AND COLOR
C
      INTEGER*2 PLTCLR(7)
      REAL VALUE,VALCATS(6),PLTSIZ(7)
C
      IF (VALUE.LT.VALCATS(1)) THEN
          JCLR = 1
      ELSE IF (VALUE.LT.VALCATS(2)) THEN
          JCLR = 2
      ELSE IF (VALUE.LT.VALCATS(3)) THEN
          JCLR = 3
      ELSE IF (VALUE.GT.VALCATS(6)) THEN
          JCLR = 7
      ELSE IF (VALUE.GT.VALCATS(5)) THEN
          JCLR = 6
      ELSE IF (VALUE.GT.VALCATS(4)) THEN
          JCLR = 5
      ELSE 
          JCLR = 4
      END IF         
C
      X = XCOORD - PLTSIZ(1)/2.
      Y = YCOORD - PLTSIZ(1)/2.
      CALL SETCOL(PLTCLR(JCLR))
      CALL BAR(X,Y,X+PLTSIZ(JCLR),Y+PLTSIZ(JCLR))
      RETURN
      END
************************************************************************
      SUBROUTINE CHGDAT
C
C   ROUTINE TO CHANGE THE DATA VALUES IN THE QCELEM FILE FOR THE STATION
C   INDICATED TO THE CURRENT VALUES IN VALARRAY
C      
$INCLUDE:'VAL1.INC'
$INCLUDE: 'AREAQC.INC'
C
      CALL CLS
      CALL OPENAQC(ELMFILE,NRECL,STNFILE,1,'OLD')
C
C   DO BINARY SEARCH TO FIND THE FIRST RECORD FOR THE STATION OF
C   INTEREST
C
      ISRCH = (ISTN * 100) + 1
      READ(61,REC=1) NUMREC
      ITOP = NUMREC + 2
      IBOT = 1
  100 CONTINUE
      IF (ITOP-IBOT.GT.1) THEN
         K = (ITOP+IBOT)/2
         READ(61,REC=K) J1,XCOORD,YCOORD,INSTN
         IFND = INSTN*100 + J1
         IF (ISRCH.LT.IFND) THEN
            ITOP = K
            GO TO 100
         ELSE IF (ISRCH.GT.IFND) THEN
            IBOT = K
            GO TO 100
         END IF
      ELSE IF (ISRCH.NE.IFND) THEN
         WRITE(*,*) 'Internal error in CHGDAT routine of AREAQC'
         WRITE(*,*) 'Station changed can not be found in QCELEM.AQC'
         PAUSE
         CLOSE(61)
         STOP
      END IF
C
C       ** POSITION FILE TO THE FIRST RECORD FOR THE STATION OF INTEREST
C          SINCE RECORD 1 SPECIFIES THE NUMBER OF RECORDS IN THE FILE, THE
C          FIRST DATA RECORD IS RECORD 2.  THEREFORE K, WHICH SPECIFIES A
C          DATA RECORD, MUST BE >=2 AND K-1 CANNOT BE ZERO.
C
      READ(61,REC=K-1) J1      
C     
      CALL WRTQC(NUMREC)
      CLOSE(61)
      RETURN
      END          
************************************************************************

      SUBROUTINE OPENAQC(ELMFILE,NRECL,STNFILE,ICNTRL,FLSTAT)
C
C   ROUTINE TO OPEN THE TEMPORARY QC FILES (ICNTRL = 1 ELEM, = 2 BOTH) 
C
      CHARACTER*22 ELMFILE,STNFILE
      CHARACTER*3 FLSTAT
C      
      IF (FLSTAT.EQ.'OLD') THEN
  80     CONTINUE
         OPEN(61,FILE=ELMFILE,STATUS='OLD',ACCESS='DIRECT',RECL=NRECL 
     +       ,IOSTAT=IOCHK) 
         IF (IOCHK.NE.0) THEN
            CALL OPENMSG(ELMFILE,'AQC2        ',IOCHK)
            GO TO 80
         END IF
      ELSE
         OPEN(61,FILE=ELMFILE,STATUS='UNKNOWN',ACCESS='DIRECT'
     +        ,RECL=NRECL) 
      END IF    
C
C   OPEN STATION FILE IF WANTED
C
      IF (ICNTRL.EQ.2) THEN
         IF (FLSTAT.EQ.'OLD') THEN
 180        CONTINUE
            OPEN(60,FILE=STNFILE,STATUS='OLD',FORM='UNFORMATTED' 
     +       ,IOSTAT=IOCHK) 
            IF (IOCHK.NE.0) THEN
               CALL OPENMSG(STNFILE,'AQC2        ',IOCHK)
               GO TO 180
            END IF
         ELSE
            OPEN(60,FILE=STNFILE,STATUS='UNKNOWN',FORM='UNFORMATTED') 
         END IF         
      END IF
      RETURN
      END
***********************************************************************
C
      SUBROUTINE WRTQC(NUMREC)
C
C   ROUTINE TO WRITE THIS FRAME OF DATA TO THE QCELEM FILE (61)
C      
$INCLUDE: 'VAL1.INC'
$INCLUDE: 'AREAQC.INC'
C
      REAL VALUE(MAXQCELM)
      LOGICAL ELEMOK(MAXQCELM)
C
C   CHECK EACH AREAQC ELEMENT TO SEE IF IT SHOULD BE SCALED AND 
C   IF MEAN AND S.D. ARE AVAILABLE
C
      DO 100 I2 = 1,NUMQC
         I1 = ELMCOL(I2)
         ICHK = SCLELEM(I2)
         ELEMOK(I2) = .FALSE.
         IF (ICHK.GT.0) THEN
            IF (ICHK.LE.100) THEN         
               IF (TBLSTDDEV(I1).NE.99999.0.AND.TBLSTDDEV(I1).GT.0)
     +              THEN
                  ELEMOK(I2) = .TRUE.
               END IF
            ELSE
               ELEMOK(I2) = .TRUE.
            END IF
         END IF
  100 CONTINUE            
C
C   WRITE THE VALUES TO FILE 61
C
      DO 200 J1 = 1,NUMLINE
         DO 150 I2 = 1,NUMQC
            I1 = ELMCOL(I2)
            VALUE(I2) = -99999.
            IF (ELEMOK(I2)) THEN
               IF (VALARRAY(I1,J1).NE.'  ') THEN
                  READ(VALARRAY(I1,J1),'(F5.0,1X)') VALUE(I2)
                  VALUE(I2) = VALUE(I2) * TBLCONV(I1)
                  IF (SCLELEM(I2).LE.100) THEN
                     VALUE(I2) = (VALUE(I2)-TBLMEAN(I1))/TBLSTDDEV(I1) 
                  END IF
               END IF
            END IF
  150    CONTINUE
      WRITE(61) J1,XCOORD,YCOORD,ISTN,(VALUE(I2),I2=1,NUMQC)
      NUMREC = NUMREC + 1
  200 CONTINUE
      RETURN
      END
***********************************************************************
$PAGE
      SUBROUTINE AQCINF(MXPCOL,MXPROW,XMIN,XMAX,ELEMCODE
     +     ,ILINE,CHGMOD)
C
C   ROUTINE TO PLOT ADDITIONAL DATA FOR ALL STATIONS NEAR THE STATION
C   AT LOCATION XCOORD,YCOORD.  AS A FIRST GUESS, IT SEARCHES AN AREA
C   30% OF SCREEN WINDOW HEIGHT AND 25% OF WIDTH.
C
$INCLUDE: 'VAL1.INC'
$INCLUDE: 'INDEX.INC'
$INCLUDE: 'AREAQC.INC'
      PARAMETER (MAXSTN=20)
      CHARACTER*78 PRTLIN
      CHARACTER*8 PRTSTN,KEYSTN,HLDSTN(MAXSTN)
      CHARACTER*7 PRTVAL(3)
      CHARACTER*3 ELEMCODE
      INTEGER*2  STNSRT(10),KEYELM
      REAL VALUE,HLDX(MAXSTN),HLDY(MAXSTN),PRVX,PRVY,TMPVAL
     +    ,KEYROW,KEYCOL
      LOGICAL STNOK(MAXSTN),CHGMOD,VALUEOK
C
      DATA PRVX,PRVY /-999.0,-999.0/
C
C   SWAP VIDEO PAGES AND IF THIS STATION IS THE SAME AS THE
C   PREVIOUS STN JUMP TO THE END OF THIS ROUTINE
C
      CALL DISPLAY(2)
      CALL SETSCR(2)
      IF (.NOT.CHGMOD) THEN
         IF (XCOORD.EQ.PRVX.AND.YCOORD.EQ.PRVY) THEN
            GO TO 560
         END IF
      END IF
      PRVX = XCOORD
      PRVY = YCOORD      
C
C   CLEAR THE SCREEN
C      
      CALL SETCOL(0)
      CALL CLR
      CHGMOD = .FALSE.
C
C   DETERMINE THE SURROUNDING TIMES (MONTH,DAY,OR,HOUR DEPENDING
C   ON DATA TYPE)
C
      MINTIM = ILINE - 1
      MAXTIM = ILINE + 1
      IF (MINTIM.LT.1) THEN
         MINTIM = 1
         MAXTIM = 3
      END IF
      IF (MAXTIM.GT.NUMLINE) THEN
         MAXTIM = NUMLINE
         MINTIM = NUMLINE - 2
      END IF      
C
C   DETERMINE THE 3 ELEMENTS TO BE PRINTED - ONE ON EITHER SIDE OF
C   THE CURRENT ELEMENT OF INTEREST.  THE VALUES FOUND (MINELM,MAXELM)
C   ARE THE INDEX NUMBERS OF THOSE ELEMENTS IN THE SETUP FILE
C
      READ(ELEMCODE,'(I3)') IELEM
      DO 100 I = 1,NUMELEM
         IF (TBLELEM(I).EQ.IELEM) THEN
            ELMNAM = TBLEABRV(I)
            KEYELM = I
            MAXELM = I + 1
            MINELM = I - 1
            IF (MINELM.LT.1) THEN
               MINELM = 1
               MAXELM = 3
            END IF
            IF (MAXELM.GT.NUMELEM) THEN
               MAXELM = NUMELEM
               MINELM = MAXELM - 2
               IF (MINELM.LT.1) THEN
                   MINELM = 1
               END IF
            END IF
            GO TO 110
         END IF
100   CONTINUE
110   CONTINUE        
      KEYCOL = FLOAT(KEYELM - MINELM + 1)
      KEYCOL = 15. + (KEYCOL-1.) * 8.  
C
C   PRINT THE HEADINGS FOR THE DATA TO BE PRINTED AND INITIALIZE
C
      CALL SETWOR(0.0,0.0,80.,24.)
      CALL SETTCL(14,0)
      CALL MOVTCA(0.,23.)
      WRITE(PRTLIN,130) 
     + '!','STN-ID   Time ',(TBLEABRV(I),I=MINELM,MAXELM),'!'
130   FORMAT(A1,A14,3(2X,A6),A1)
      CALL TEXT(PRTLIN)
      DO 140 I = 1,3
         PRTVAL(I) = '   '
140   CONTINUE         
      YROW = 2.
C      
      CALL OPENFILES(1)
C
C   FIND ALL OF THE STATIONS WITHIN THE INITIAL SEARCH AREA
C   (ABOUT 15%) OF THE SCREEN) - ALSO MAKE SURE THERE IS A VALUE
C   FOR THE CENTRAL STATION AT THIS TIME.  IF NOT, QUIT.
C
      RANGE = XMAX - XMIN
      RADIUS = RANGE * 0.2
200   CONTINUE
      REWIND (60)
      READ(60) IDUMMY
      DO 250 I = 1,MAXSTN
         HLDX(I) = -999
         STNOK(I) = .FALSE.
250   CONTINUE
      NUMSTN = 0
      JSTN = 0
      VALUEOK = .FALSE.
      DO 400 I5 = 1,9999
         READ(60,END=410) INSTN,XCORD,YCORD,PRTSTN
         I6 = I5
         IF (INSTN.NE.JSTN) THEN
            JSTN = INSTN
            DISTAN = (XCOORD-XCORD)**2 + (YCOORD-YCORD)**2
            DISTAN = SQRT(DISTAN)
            IF (DISTAN.LE.RADIUS.AND.HLDVAL(INSTN).NE.-99999.) THEN
               NUMSTN = NUMSTN + 1
               IF (NUMSTN.EQ.MAXSTN) THEN
                  RADIUS = RADIUS * .5
                  GO TO 200
               END IF   
               HLDX(NUMSTN) = XCORD
               HLDY(NUMSTN) = YCORD
               HLDSTN(NUMSTN) = PRTSTN
               STNOK(NUMSTN) = .TRUE.
               IF (XCORD.EQ.XCOORD.AND.YCORD.EQ.YCOORD) THEN
                  VALUEOK = .TRUE.
               END IF
            END IF
         END IF
400   CONTINUE
410   CONTINUE
      IF (.NOT.VALUEOK) THEN
         CALL MOVTCA(0.,2.)
         CALL GETMSG(99,PRTLIN)
         CALL GETMSG(999,PRTLIN)
         LGTH = LNG(PRTLIN)
         PRTLIN(LGTH+3:) = STNFILE
         LGTH = LNG(PRTLIN)
         CALL GRFTXT(LGTH,PRTLIN,7,0)
         CHGVAL = .FALSE.
         PRVX = 0
         PRVY = 0
         CALL DISPLAY(1)
         CALL SETSCR(1)
         RETURN
      END IF
C
C   IF ONLY ONE STATION, SKIP INCREASED SEARCH AND SORT
C
      IF (I6.LT.2) THEN
         NUMSTN = 1
         NUMTOT = 1
         STNSRT(1) = 1
         GO TO 495
      END IF
C
C   IF LESS THAN 5 STATIONS FOUND, INCREASE THE SEARCH AREA 
C
      IF (NUMSTN.LT.5) THEN
         RADIUS = RADIUS * 1.1
         IF (RADIUS.LT.RANGE*.4) THEN
            GO TO 200
         END IF
      END IF
C
C   IF MORE THAN 7 STNS HAVE BEEN FOUND, FIND THE NUMBER WITHIN A
C   DECREASING RADIUS UNTIL 7 OR LESS QUALIFY
C
      NUMTOT = NUMSTN
      IF (NUMSTN.GT.7) THEN
450      CONTINUE
         XTRA = 7 / NUMSTN 
         XTRA = (1.0 - XTRA) / 4
         RADIUS = RADIUS - XTRA*RADIUS
         NUMSTN = 0
         DO 460 I5 = 1,NUMTOT         
            DISTAN = (XCOORD-HLDX(I5))**2 + (YCOORD-HLDY(I5))**2
            DISTAN = SQRT(DISTAN)
            IF (DISTAN.LE.RADIUS) THEN                    
               NUMSTN = NUMSTN + 1
            ELSE
               STNOK(I5) = .FALSE.
            END IF
460      CONTINUE
         IF (NUMSTN.GT.7) THEN
            GO TO 450
         END IF                 
      END IF
C
C   SORT THE STATIONS FOUND INTO REVERSE LATITUDE ORDER
C
      NUMSTN = 0
      DO 470 I5 = 1,NUMTOT                             
         IF (STNOK(I5)) THEN
            NUMSTN = NUMSTN + 1                    
            STNSRT(NUMSTN) = I5
         END IF
470   CONTINUE         
C
      JSTN = 2
      IF (NUMSTN.LT.2) THEN
         GO TO 495
      END IF
480   CONTINUE
      ISTN1 = STNSRT(JSTN)
      ISTN0 = STNSRT(JSTN-1)
      IF (HLDY(ISTN1).GT.HLDY(ISTN0)) THEN
         IHLD = STNSRT(JSTN)
         STNSRT(JSTN) = STNSRT(JSTN-1)
         STNSRT(JSTN-1) = IHLD
         IF (JSTN.GT.2) THEN
            JSTN = JSTN - 1
            GO TO 480               
         END IF
      END IF
      JSTN = JSTN + 1
      IF (JSTN.LE.NUMSTN) THEN
         GO TO 480
      END IF
C
C   SET UP THE PLOT AREA TO MATCH THE STATIONS LOCATED
C
      XTOP = -999.
      XBOT = 999.
      YTOP = -999.
      YBOT = 999
      DO 490 I5 = 1,NUMTOT
         IF (STNOK(I5)) THEN  
            IF (HLDX(I5).LT.XBOT) THEN
               XBOT = HLDX(I5)
            END IF
            IF (HLDX(I5).GT.XTOP) THEN
               XTOP = HLDX(I5) 
            END IF
            IF (HLDY(I5).LT.YBOT) THEN
               YBOT = HLDY(I5)
            END IF
            IF (HLDY(I5).GT.YTOP) THEN
               YTOP = HLDY(I5)
            END IF
         END IF
490   CONTINUE         
      MNROW = MXPROW * .10
      MXROW = MXPROW * .85 
      MNCOL = MXPCOL/2
      MXCOL = MXPCOL - 72
C
C   DRAW THE PLOT BOX AND MISC INFO
C
      CALL SETCOL(11)
      CALL BOX(39.,2.,79.,22.)
      XRNG = (XTOP - XBOT) * 100.0
      CALL MOVTCA(51.,23.)
      WRITE(PRTLIN,'(A6,F4.0,A9)') '!<----',XRNG,'KM ---->!'
      CALL TEXT(PRTLIN)
      CALL MOVTCA(47.,1.)
      WRITE(PRTLIN,'(A1,A5,I2,4X,A6,A6,A1)') 
     +                 '!','Time=',ILINE,' Elem=',ELMNAM,'!'
      CALL TEXT(PRTLIN)
C
C   FIND THE DATA VALUES FOR THE STATIONS SELECTED AND PRINT THEM
C   
495   CONTINUE
      KEYSTN = 'XXXXXXXX'
      DO 550 I6 = 1,NUMSTN
         I5 = STNSRT(I6)                             
         IDKEY(1:8) = HLDSTN(I5)
         IDKEY(9:21) = SRCHKEY
         CALL BINDATA(IDKEY,RTNCODE)
         READ(20,REC=RECNUM,IOSTAT=ISTAT) 
     +      IDKEY,((VALARRAY(I,J),I=1,NUMELEM),
     +      J=1,NUMLINE),(((FLAGARRAY(K,L,M),M=1,2),K=1,NUMELEM),
     +      L=1,NUMLINE)
         IF (ISTAT.NE.0) THEN
            IF (ISTAT.EQ.-1 .OR. ISTAT.EQ.6501) THEN
               CALL GETMSG(382,PRTLIN)
               ISTAT = 0
            ELSE
               CALL GETMSG(191,PRTLIN)
            ENDIF
            CALL GETMSG(999,PRTLIN)
            LGTH = LNG(PRTLIN)
            IF (ISTAT.NE.0) THEN
               WRITE(PRTLIN(LGTH+2:),'(I4.4)') ISTAT
               LGTH = LNG(PRTLIN)
            ENDIF   
            PRTLIN(LGTH+3:)=FILNAM
            LGTH = LNG(PRTLIN)
            CALL MOVTCA(0.,2.)
            CALL GRFTXT(LGTH,PRTLIN,7,0)
            CLOSE(19)
            CLOSE(20)
            GO TO 650
         ENDIF   
         PRTSTN = HLDSTN(I5)
         IF (NUMSTN.GT.1) THEN
            X1 = (HLDX(I5) - XBOT) / (XTOP-XBOT) 
     +               * (MXCOL - MNCOL) + MNCOL 
            Y1 = (HLDY(I5) - YBOT) / (YTOP-YBOT) 
     +               * (MXROW - MNROW) + MNROW 
            X4 = X1/8.0
            Y4 = Y1/(FLOAT(MXPROW)/25.0)
            IF (HLDX(I5).EQ.XCOORD.AND.HLDY(I5).EQ.YCOORD) THEN
               ICLR = 7
               KEYSTN = HLDSTN(I5)
            ELSE
               ICLR = 4
            END IF
            CALL MOVTCA(X4,Y4)
            CALL GRFTXT(8,PRTSTN,PLTCLR(ICLR),0)
         ELSE   
            KEYSTN = HLDSTN(I5)
         END IF
         DO 520 J2 = MINTIM,MAXTIM
            DO 500 I2 = MINELM,MAXELM
               I3 = I2 - MINELM + 1
               IF (VALARRAY(I2,J2).NE.'  ') THEN
                  READ(VALARRAY(I2,J2),'(F5.0,1X)') VALUE
                  VALUE = VALUE * TBLCONV(I2)
                  WRITE(PRTVAL(I3),'(F7.2)') VALUE
                  IF (KEYSTN.EQ.HLDSTN(I5).AND.J2.EQ.ILINE.AND.
     +                 I2.EQ.KEYELM) THEN
                     TMPVAL = VALUE
                  END IF                        
               ELSE
                  PRTVAL(I3) = '   '
               END IF
500         CONTINUE
            WRITE(PRTLIN,510) PRTSTN,J2,(PRTVAL(I3),I3=1,3)
510         FORMAT(A8,2X,I2,2X,3(1X,A7))
            YROW = YROW + 1.
            IF (HLDX(I5).EQ.XCOORD.AND.HLDY(I5).EQ.YCOORD) THEN
               ICLR = 14
               IF (J2.EQ.ILINE) THEN
                  KEYROW = YROW
               END IF
            ELSE IF (J2.EQ.ILINE) THEN
               ICLR = 14
            ELSE
               ICLR = 4
            END IF
            CALL MOVTCA(0.,25.-YROW)
            CALL GRFTXT(38,PRTLIN,ICLR,0)
            PRTSTN = '  '
520      CONTINUE                                               
550   CONTINUE
      CLOSE(19)
      CLOSE(20)
560   CONTINUE    
      CALL MOVTCA(KEYCOL,25.-KEYROW)
      WRITE(PRTLIN,'(F7.2)') TMPVAL
      CALL GRFTXT(7,PRTLIN,PLTCLR(7),0)
C
C   GET THE COMMAND FROM THE USER AND SET FLAG TO CHANGE VALUES
C   IF REQUESTED
C
      CALL MOVTCA(0.,0.)
      CALL GRFTXT(10,'F8-Modify',7,0)
      CALL DELTCU
600   CONTINUE
      CALL RDLOC(XPOS,YPOS,INCHAR,RTNCODE)
      IF (INCHAR.EQ.'8F') THEN      
         CHGVAL = .TRUE.
      ELSE
          CHGVAL = .FALSE.
      END IF
      IF (INCHAR.EQ.'XX' .OR. INCHAR.EQ.'YY') THEN
         GO TO 600
      END IF 
  650 CONTINUE      
C
C   SET ACTIVE PAGE BACK TO ZERO AND RETURN
C      
      CALL DISPLAY(1)
      CALL SETSCR(1)
      RETURN
      END   
***********************************************************************

      SUBROUTINE DRWMAP(XMIN,YMIN,XMAX,YMAX,
     +                  XMIN1,YMIN1,XMAX1,YMAX1,NAMEXT,ERRFLG)
C
C   ROUTINE TO COPY THE SAVED BACKGROUND FOR THE CURRENT MAP TO THE SCREEN
C   AND SET COORDINATES
C
C        INPUT: XMIN,YMIN,XMAX,YMAX
C              THE LAT/LON COORDINATES (FLOATING PT DEGREES) OF THE
C              PLOT AREA.
C       OUTPUT: XMIN1,YMIN1,XMAX1,YMAX1 
C              THE LAT/LON COORDINATES OF THE CORNERS OF THE SCREEN
C
C
$INCLUDE: 'VAL1.INC'
$INCLUDE: 'AREAQC.INC'
C
      CHARACTER*12 NAMEXT
      CHARACTER*1  ERRFLG
C
C       .. IN ORDER TO OPEN THE VIEWPORT TO THE ENTIRE SCREEN, A MAX VALUE
C          EQUAL TO .999 MUST BE USED.  A VALUE OF 1.0 IS A SPECIAL SIGNAL FOR
C          HALO TO 'TURN OFF' THE VIEWPORT WHICH DOES NOT RESET ASPECT RATIOS
      CALL SETVIE(0.,0.,0.999,0.999,-1,-1)
      CALL SETWOR(0.,0.,1.,1.)
      NCHR2 = LNG(MAPFILE)-4
      NCHR1=NCHR2+1
      DO 30 I=1,8
      NCHR1=NCHR1-1
      IF (MAPFILE(NCHR1:NCHR1).EQ.'\') GO TO 32
   30 CONTINUE
      NCHR1=NCHR2-8
   32 CONTINUE    
      NCHR1=NCHR1+1
      NAMEXT = MAPFILE(NCHR1:NCHR2)//'.QSC' 
      CALL RDBKGND(NAMEXT,ERRFLG)
      IF (ERRFLG.NE.'0') THEN
         GO TO 100
      ENDIF
C      
      CALL BGNQCMAP(XLONMN,XLATMN,XLONMX,XLATMX,XKWMX,YKWMX)
C
C   COMPUTE THE LAT/LON VALUES THAT CORRESPOND TO THE CORNERS OF 
C   ENTIRE SCREEN AREA
C 
      CALL KWINQL(X1,Y1,X2,Y2)
      X1 = X1 / XKWMX
      X2 = X2 / XKWMX
      Y1 = Y1 / YKWMX
      Y2 = Y2 / YKWMX
C
C  CALCULATE THE SIZE OF THE PLOTTING AREA 
C
      XPLOT = X2 - X1
      YPLOT = Y2 - Y1
C
C  SCALE WORLD COORDINATES SO THAT PLOTTING AREA IS MAPPED
C  TO (XMIN,YMIN),(XMAX,YMAX)
C
      XMIN1 = XMIN - (X1*(XMAX-XMIN)/XPLOT)      
      XMAX1 = XMAX + ((1.-X2)*(XMAX-XMIN)/XPLOT)      
      YMIN1 = YMIN - (Y1*(YMAX-YMIN)/YPLOT)      
      YMAX1 = YMAX + ((1.-Y2)*(YMAX-YMIN)/YPLOT)      
C      
      CALL INQCRA(MAXCLR)
      CALL SETCOL(MAXCLR)
      CALL KWBORD
C      
  100 RETURN
      END
      