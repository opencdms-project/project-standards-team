$storage:2
      PROGRAM NORMAL
C
C  This program computes monthly normals,  either provisional or 
C  standard, from DataEase daily data and write them to the file
C  Q:CLIMDATA.DAT for NODUPMRG into the DataEase form "NORMALS".
C  NOTE THAT THE OUTPUT FILE MUST BE SORTED BEFORE NODUPMRG.
C     
C  Elements that are to have normals computed are specified in the parameter
C  file NORMAL.PRM.  Normals for Mean Temperature (calculated from max and
C  min temperatures) and Days with Precipitation > 1.0 millimeter
C  (calculated from Daily Precipitaion) may be generated.  See the parameter
C  file for documentation on how to set this up.
C
C  This program is based on the rules set forth in the WMO publication
C  "Calculation of Monthly and Annual 30-Year Standard Normals"
C  (WMO TD/No. 341).
C
C  Currently, only a portion of the calculation rules are implemented.  These
C  are methods (a), (b), and (c) as described in Section V.1 of WMO TD/No. 341.
C 
C*****************************************************************************
C This program was originally developed by Zhiang Quain, a visiting
C scientist from the People's Republic of China.  The program has been
C modified by the CLICOM support offices to be more consistant with
C CLICOM programming standards and with WMO TD/No. 341.
C*****************************************************************************

      PARAMETER (MAXDAYS=31,MAXMONTHS=12,MAXYEARS=30,MAXELEMS=30)

      CHARACTER*1 DATASOURCE,RTNCODE,FLAG1(MAXDAYS)
      CHARACTER*2 RTNFLAG
      CHARACTER*3 RECTYPE,ELEM
      CHARACTER*7 OUTCNT(2)
      CHARACTER*8 STNID,STNWANTED
      CHARACTER*20 ELUNITS,UNITLBL(4)
      CHARACTER*28 MSG1
      CHARACTER*64 FILNAME
      CHARACTER*82 SCRATCH
      INTEGER*2 DDSID,IELEM,YEAR,MONTH,NUMDAYS(MAXMONTHS),MAXPTR
     +          ,GPCPPTR,GTMNPTR,NPEND,IFAC(2),IELMSUN(2)
      INTEGER*4 I1,NRECCOUNT,RECCOUNT
      REAL*4 SUM4,SCALE4,SUM1,SUM2,TPRCP
      REAL*4 VALUE(MAXDAYS),RMAXMLY,RMINMLY
      LOGICAL OKMLY,OKTEMP,GPRCP,GTMEAN,CVTPRCP,ACCUM,WTNMLFLG,
     *        CHKSUN,CHKPCP
 
      
$INCLUDE:'NORMAL.INC'
C
      DATA GPRCP/.FALSE./ GTMEAN/.FALSE./
      DATA NUMDAYS/31,28,31,30,31,30,31,31,30,31,30,31/
      DATA RECTYPE/'DLY'/ 
      DATA NRECCOUNT/0/ ,RECCOUNT/0/ ,OUTCNT/'       ','       '/
      DATA IELMSUN/084,268/
C      
      OUTCNT(2)(1:1) = CHAR(0)
      WTNMLFLG = .FALSE.     
C
C  Open the output file no matter what. That way it will not have 
C  old data in it if the program is not run.
C
   10 CONTINUE
      OPEN(51,FILE='Q:CLIMDATA.DAT',STATUS='UNKNOWN',
     +        IOSTAT=IOCHK,FORM='BINARY')
      IF (IOCHK.NE.0) THEN
         CALL OPENMSG('Q:CLIMDATA.DAT        ','NORMAL     ',IOCHK)
         GOTO 10                           
      END IF    
      ENDFILE 51
      REWIND 51
C 
C READ INPUT AND OUTPUT ELEMENTS FROM PARAMETER FILE, AND SET UP POINTERS IF
C TMEAN AND/OR DAYS WITH PRCP > 1.0 MILLIMETER ARE TO BE GENERATED.
C
   20 OPEN (61,FILE='P:\DATA\NORMAL.PRM',STATUS='OLD',
     +      FORM='FORMATTED',IOSTAT=IOCHK)
      IF (IOCHK.NE.0) THEN
          CALL OPENMSG('P:\DATA\NORMAL.PRM    ',
     +          'NORMAL      ',IOCHK)
          GOTO 20
      END IF
      DO 21 I=1,3
         READ(61,*)
   21 CONTINUE
      READ(61,*) (UNITLBL(I),I=1,4)      
      READ(61,*) NUMELEM
      IF (NUMELEM.LT.1.OR.NUMELEM.GT.MAXELEMS) THEN
          MSG1 = 'P:\DATA\NORMAL.PRM'
          CALL WRTMSG (3,199,12,1,1,MSG1,22)
          STOP 2
      END IF
C      
      CHKSUN = .FALSE.
      CHKPCP = .FALSE.
      DO 30 I = 1,NUMELEM
          READ (61,*) INELEM(I), OUTELEM(I), NORMTYP(I), DSTATUS(I)
          IF (INELEM(I).EQ.084) THEN
             CHKSUN = .TRUE.
          ELSE IF (INELEM(I).EQ.005 .AND. OUTELEM(I).EQ.246 .AND. 
     +        NORMTYP(I).EQ.'G') THEN
              CHKPCP = .TRUE.
              GPRCP = .TRUE.
              GPCPPTR = I
              NORMTYP(I) = 'C'
          ELSE IF (INELEM(I).EQ.003 .AND. OUTELEM(I).EQ.203 .AND. 
     +        NORMTYP(I).EQ.'G') THEN
              GTMEAN = .TRUE.
              GTMNPTR = I
              NORMTYP(I) = 'M'
          ELSE IF (NORMTYP(I).NE.'M' .AND. NORMTYP(I).NE.'T' .AND.
     +             NORMTYP(I).NE.'C') THEN     
             WRITE(MSG1,'(2(1X,I3.3),2(1X,A1))')INELEM(I), OUTELEM(I),
     +                                          NORMTYP(I), DSTATUS(I)
             LGTH = LEN(MSG1)
             CALL WRTMSG (3,611,12,1,1,MSG1,LGTH)
             STOP 2
          END IF
   30 CONTINUE
      CLOSE(61)
   
C
C IF GENERATING TMEAN FROM TMAX AND TMIN, BE SURE TMAX IS AN INPUT ELEMENT
C
   
      IF (GTMEAN) THEN
         DO 40, I = 1,NUMELEM
            IF (INELEM(I).EQ.002) THEN
                MAXPTR = I
                GOTO 45
            END IF
   40    CONTINUE
      END IF
      GTMEAN = .FALSE.
   45 CONTINUE
C
C       ** READ UNITS FROM ELEMENT DEFINITION FILE TO DETERMINE IF
C          UNIT CONVERSION IS NECESSARY FOR SELECTED ELEMENTS
C   
      IF (CHKSUN .OR. CHKPCP) THEN
   50    CONTINUE
         OPEN(30,FILE='P:\DATA\ELEM.DEF',STATUS='OLD',ACCESS='DIRECT'
     +          ,RECL=110,SHARE='DENYWR',MODE='READ',IOSTAT=IOCHK)
         IF(IOCHK.NE.0) THEN
            CALL OPENMSG('P:\DATA\ELEM.DEF      ','NORMAL       ',IOCHK)
            GO TO 50
         END IF
         IF (CHKPCP) THEN
C
C             .. Retrieve element definition for precipitation to determine 
C                the units the data values are in.  If the data values are 
C                measured in inches, set logical variable to indicate such 
C                so that, when computing Days with Precipitation ò 1 
C                Millimeter, the data values for precipitation can be 
C                converted to millimeters. 
C
            NREC = 5
            READ (30,REC=NREC) ELEM,SCRATCH,ELUNITS,SCALE4
            CVTPRCP = .FALSE.
            IF (ELUNITS .EQ. UNITLBL(1)) THEN
               CVTPRCP = .TRUE.
            ELSE IF (ELUNITS .NE. UNITLBL(2)) THEN
               WRITE(MSG1,2101) NREC,ELEM,ELUNITS
 2101          FORMAT(I3.3,'-',A3,'-',A)         
               LGTH = LNG(MSG1)
               CALL WRTMSG (3,93,12,1,1,MSG1,LGTH)
               STOP 2
            END IF
         END IF
         IF (CHKSUN) THEN
C
C             .. Retrieve element definitions for Daily total amount of Sunshine
C                and Monthly total amount of Sunshine to determine the units the 
C                data values are in.  If the data values are measured in different
C                units, a conversion factor is calculated.
            DO 54 NF=1,2
               NREC= IELMSUN(NF)
               READ (30,REC=NREC) ELEM,SCRATCH,ELUNITS,SCALE4
               IF (ELUNITS .EQ. UNITLBL(3)) THEN
                  IFAC(NF) = 1
               ELSE IF (ELUNITS .EQ. UNITLBL(4)) THEN
                  IFAC(NF) = 60
               ELSE    
                  WRITE(MSG1,2101) NREC,ELEM,ELUNITS
                  LGTH = LNG(MSG1)
                  CALL WRTMSG (3,93,12,1,1,MSG1,LGTH)
                  STOP 2
               END IF
   54       CONTINUE
            IF (IFAC(1).EQ.IFAC(2)) THEN
               SUNCONV = 1.
            ELSE   
               SUNCONV = FLOAT(IFAC(1))/FLOAT(IFAC(2))
            ENDIF   
         ENDIF   
C
         CLOSE (30)
      ENDIF   
C
C Retrieve station-id and data-source from user.  Handle function keys for
C help and stationlist.      
C
   60 CONTINUE
      FILNAME = 'P:\HELP\NORMAL.HLP'
      CALL SETMOD(3,IERR)                    
      CALL WRTMSG(24,243,14,0,0,' ',0)
      CALL WRTMSG(23,244,14,0,0,' ',0)
      STNWANTED = '        '      
      CALL LOCATE(5,15,IERR)
      CALL WRTSTR('STATION-ID ',11,14,0)
      CALL WRTSTR(STNWANTED,8,15,1)
      CALL WRTFNC(9)
   70 CONTINUE
      CALL LOCATE(5,26,IERR)
      CALL GETSTR(0,STNWANTED,8,15,1,RTNFLAG)
      IF (RTNFLAG.EQ.'1F') THEN
         CALL DSPWIN(FILNAME)
         GOTO 70
      ELSE IF (RTNFLAG.EQ.'1S') THEN
         CALL GETSTN(STNWANTED)
         CALL LOCATE(5,26,IERR)
         CALL WRTSTR(STNWANTED,8,15,1)
         IF (STNWANTED.EQ.'        ') THEN
            GOTO 70
         END IF
      ELSE IF (RTNFLAG.EQ.'4F') THEN
         CALL LOCATE(24,0,IERR)
         STOP 2
      ELSE IF (RTNFLAG.NE.'  '.AND.RTNFLAG.NE.'2F'.AND.
     +         RTNFLAG.NE.'RE') THEN
         CALL BEEP
         GOTO 70
      END IF
      CALL LOCATE(8,20,IERR)
      CALL DATASRC(20,DATASOURCE,RTNCODE)
      IF (RTNCODE.NE.'0') THEN
         CALL LOCATE(24,0,IERR)
         STOP 2
      END IF
C
C  Open the input file
C
      CALL OPENINPUT(RECTYPE,DATASOURCE)
C
C  Write the running total line
C 
      CALL CLRMSG(1)
      CALL LOCATE(23,0,IERR)
      CALL WRTSTR('Records Read -          Records Processed - '
     +             ,44,14,0)
      STNID  = 'ZZZZZZZZ'
      PREVID = 'ZZZZZZZZ'
      NPEND = 1900
      YEAR  = 9999
      CALL INITVALS
      TPRCP = -99999.
C
C  Do the processing ---------------------
C               
      DO 200 I1 = 1,999999
         CALL READDLY(DDSID,STNID,IELEM,YEAR,MONTH,VALUE,FLAG1,RTNCODE)
         NRECCOUNT = NRECCOUNT + 1
         WRITE(OUTCNT,'(I7)') NRECCOUNT              
         CALL LOCATE(23,15,IERR)
         CALL CWRITE(OUTCNT,12,IERR)
C
C  Make sure that processing starts after 1900
C
         IF (YEAR.LE.1900) GOTO 200
         MD = MOD((YEAR-1901),MAXYEARS)
         MD1 = MOD((LATEYR-1901),MAXYEARS)
C
C If have reached the end of a normals period, compute the normal values and
C write them out.
C
         IF (YEAR.GT.NPEND
     +      .OR.STNID.NE.PREVID.OR.RTNCODE.NE.'0') THEN
            IF (NUMOUT.NE.0.AND.PREVID.NE.'ZZZZZZZZ') THEN
               CALL COMPNORM(MD1,WTNMLFLG)
            END IF
            IF (RTNCODE.NE.'0') THEN
               GOTO 900
            END IF
            CALL INITVALS
            TPRCP = -99999.

            IF (STNID.NE.PREVID) THEN
               PREVID = STNID
               NPEND = 1900
            END IF
260         IF ( YEAR.LE. NPEND) GOTO 265
               NPEND=NPEND+MAXYEARS
               GOTO 260
265         CONTINUE                
         END IF
C
C  Check if current station is in the one wanted
C
         IF (STNWANTED.NE.'ALL     ') THEN
            IF (STNID.LT.STNWANTED) GOTO 200
            IF (STNID.GT.STNWANTED) GOTO 900
         END IF
C
C  Check if current element is in the list of elements to be processed
C
         NELEM = 0
         DO 250 J = 1,NUMELEM
            IF (IELEM.EQ.INELEM(J)) THEN
               NELEM = J
               GOTO 275
            END IF
  250    CONTINUE                     
         IF (NELEM.EQ.0) GOTO 200
C
  275    CONTINUE
         RECCOUNT = RECCOUNT + 1
         WRITE(OUTCNT,'(I7)') RECCOUNT
         CALL LOCATE(23,44,IERR)
         CALL CWRITE(OUTCNT,12,IERR)
C
C  Get the beginning year of observation period within each 30-year span  
C
         IF (YEAR.LT.BGNYEAR(MONTH,NELEM)) THEN
            BGNYEAR(MONTH,NELEM) = YEAR
            NYEAR1(MONTH,NELEM) = MD + 1
         END IF 
C
C  Get the ending year of observation period within each 30-year span.
C  If all data are missing for a whole month or even for a whole year, 
C  set the year-month total or mean to -99999.
C
         IF (YEAR.GT.ENDYEAR(MONTH,NELEM)) THEN 
            ENDYEAR(MONTH,NELEM) = YEAR
            NYEAR(MONTH,NELEM) = YEAR - BGNYEAR(MONTH,NELEM) 
     +                         + NYEAR1(MONTH,NELEM)
            NTEST(MONTH,NELEM) = NTEST(MONTH,NELEM) + 1
            L1 = NYEAR1(MONTH,NELEM) + NTEST(MONTH,NELEM) - 1
            L2 = NYEAR(MONTH,NELEM) 
            IF (L1.NE.L2) THEN
               DO 300 L = L1,L2-1
                  OUT(L,MONTH,NELEM) = -99999.
                  XMAX(L,MONTH,NELEM) = -99999.
                  XMIN(L,MONTH,NELEM) = -99999.
                  IF (INELEM(NELEM).EQ.005.AND.GPRCP) THEN
                     OUT(L,MONTH,GPCPPTR) = -99999.
                     XMAX(L,MONTH,GPCPPTR) = -99999.
                     XMIN(L,MONTH,GPCPPTR) = -99999.
                  END IF
                  IF ((INELEM(NELEM).EQ.002.OR.INELEM(NELEM).EQ.003)
     +                 .AND.GTMEAN) THEN
                     OUT(L,MONTH,GTMNPTR) = -99999.
                     XMAX(L,MONTH,GTMNPTR) = -99999.
                     XMIN(L,MONTH,GTMNPTR) = -99999.
                  END IF 
  300          CONTINUE 
               NTEST(MONTH,NELEM) = NTEST(MONTH,NELEM) + L2 - L1
            END IF 
         END IF
C
C  Using array NDX to keep track of the elements which have actually
C  been read into the program from array INELEM. 
C
         DO 350 J = 1,NUMELEM
            IF (NDX(J).NE.0) THEN
               IF (NDX(J).EQ.NELEM) GOTO 375
            ELSE
               NDX(J) = NELEM
               NUMOUT = J
               IF (INELEM(NDX(J)).EQ.005.AND.GPRCP) THEN
                  NDX(J+1) = GPCPPTR
                  NUMOUT = J + 1
               END IF
               IF (INELEM(NDX(J)).EQ.003.AND.GTMEAN) THEN
                  NDX(J+1) = GTMNPTR
                  NUMOUT = J + 1
               END IF
               GOTO 375
            END IF
  350    CONTINUE       
C
C  Check for missing data in a month and compute month mean or total.  
C  Tests for missing daily data are applied here.
C
  375    CONTINUE
         IF (MONTH.EQ.2) THEN
            IF (MOD(YEAR,4).NE.0) THEN
                NUMDAYS(MONTH) = 28
            ELSE
               IF (MOD(YEAR,100).EQ.0.) THEN
                  IF (MOD(YEAR,400).EQ.0) THEN
                     NUMDAYS(MONTH) = 29
                  ELSE
                     NUMDAYS(MONTH) = 28
                  END IF
               ELSE
                  NUMDAYS(MONTH) = 29
               END IF
            END IF
         END IF 
         MISSDAYS = 0
         SUM1 = 0.
         OKMLY = .TRUE.
         ACCUM = .FALSE.
         RMAXMLY = -99999.
         RMINMLY = 99999.
         DO 400 I = 1,NUMDAYS(MONTH)
            IF (VALUE(I).EQ.-99999.) THEN
               IF (NORMTYP(NELEM).EQ.'M') THEN
                  IF ((I+3).LE.NUMDAYS(MONTH).AND.VALUE(I+1)
     +                .EQ.-99999..AND.VALUE(I+2).EQ.-99999.
     +                .AND.VALUE(I+3).EQ.-99999.) THEN
                     OKMLY = .FALSE.
                     GOTO 425
                  END IF
                  MISSDAYS = MISSDAYS + 1
                  IF (MISSDAYS.GT.5) THEN
                     OKMLY = .FALSE.
                     GOTO 425
                  END IF
               ELSE IF (NORMTYP(NELEM).EQ.'T') THEN
                  IF ((I+3).LT.NUMDAYS(MONTH)) THEN
                      K = I + 3
                  ELSE
                      K = NUMDAYS(MONTH)
                  END IF
                  DO 420 J = I,K
                      IF (FLAG1(J).EQ.'A') THEN
                         ACCUM = .TRUE.
                      END IF
  420             CONTINUE
                  IF (.NOT.ACCUM) THEN
                      OKMLY = .FALSE.
                      GOTO 425
                  ELSE
                      IF (K.LE.4.AND.TPRCP.EQ.-99999.) THEN
                          OKMLY = .FALSE.
                          GOTO 425
                      END IF
                  END IF
               ELSE
                  OKMLY = .FALSE.        
               END IF   
            ELSE
               IF (NORMTYP(NELEM).EQ.'C') THEN
                  IF (VALUE(I) .NE. 0.) SUM1=SUM1+1.
               ELSE
                  SUM1 = SUM1 + VALUE(I)
               ENDIF   
            END IF            
  400    CONTINUE 
  
  425    CONTINUE
C
C SAVE PRECIPITATION VALUE FOR LAST DAY OF MONTH SO THAT CHECK FOR
C ACCUMULATION ACROSS MONTHS CAN BE PERFORMED.
C
         IF (INELEM(NELEM).EQ.005) THEN
            TPRCP = VALUE(NUMDAYS(MONTH))
         END IF  
C
C  Get monthly extreme maximum and minimun 
C
         IF (OKMLY) THEN
            IF (NORMTYP(NELEM).EQ.'M') THEN
               OUT(NYEAR(MONTH,NELEM),MONTH,NELEM) 
     +                  = SUM1 / (NUMDAYS(MONTH)-MISSDAYS)
               DO 450 I = 1,NUMDAYS(MONTH)
                  IF (VALUE(I).NE.-99999.) THEN
                     IF (VALUE(I).GT.RMAXMLY) THEN
                        RMAXMLY = VALUE(I)
                     END IF
                     IF (VALUE(I).LT.RMINMLY) THEN
                        RMINMLY = VALUE(I)
                     END IF
                  END IF
  450          CONTINUE
            ELSE IF (NORMTYP(NELEM).EQ.'T') THEN
               IF (INELEM(NELEM).EQ.084.AND.OUTELEM(NELEM).EQ.268) THEN 
                  OUT(NYEAR(MONTH,NELEM),MONTH,NELEM) = SUM1*SUNCONV
               ELSE   
                  OUT(NYEAR(MONTH,NELEM),MONTH,NELEM) = SUM1 
               ENDIF   
               DO 475 I = 1,NUMDAYS(MONTH)
                  IF (VALUE(I).NE.-99999. .AND. FLAG1(I).NE.'A') THEN
                     IF (VALUE(I).GT.RMAXMLY) THEN
                        RMAXMLY = VALUE(I)
                     END IF
                     IF (VALUE(I).LT.RMINMLY) THEN
                        RMINMLY = VALUE(I)
                     END IF
                  END IF
  475          CONTINUE
            ELSE 
C                .. NORMAL TYPE IS COUNT            
               OUT(NYEAR(MONTH,NELEM),MONTH,NELEM) = SUM1 
               RMAXMLY = SUM1
               RMINMLY = 0.
            END IF
            XMAX(NYEAR(MONTH,NELEM),MONTH,NELEM) = RMAXMLY
            XMIN(NYEAR(MONTH,NELEM),MONTH,NELEM) = RMINMLY
         ELSE 
            OUT(NYEAR(MONTH,NELEM),MONTH,NELEM) = -99999.
            XMAX(NYEAR(MONTH,NELEM),MONTH,NELEM) = -99999.
            XMIN(NYEAR(MONTH,NELEM),MONTH,NELEM) = -99999.
         END IF
C
C  If parameter file indicates, count days with daily preciptation greater
C  than or equal to 1mm
C  
         IF (INELEM(NELEM).EQ.005.AND.GPRCP) THEN
            SUM2 = 0.
            NYEAR(MONTH,GPCPPTR) = NYEAR(MONTH,NELEM)
            NYEAR1(MONTH,GPCPPTR) = NYEAR1(MONTH,NELEM)
            BGNYEAR(MONTH,GPCPPTR) = BGNYEAR(MONTH,NELEM)
            ENDYEAR(MONTH,GPCPPTR) = ENDYEAR(MONTH,NELEM)
            DO 500 I = 1,NUMDAYS(MONTH)
               IF (VALUE(I).EQ.-99999.) THEN
                  OUT(NYEAR(MONTH,GPCPPTR),MONTH,GPCPPTR) = -99999.
                  XMAX(NYEAR(MONTH,GPCPPTR),MONTH,GPCPPTR) = -99999.
                  XMIN(NYEAR(MONTH,GPCPPTR),MONTH,GPCPPTR) = -99999.
                  GOTO 200
               ELSE
                   IF (CVTPRCP) THEN
                        VALUE(I) = VALUE(I) * 25.4
                   END IF       
                   IF (VALUE(I).GE.1.0) THEN
                      SUM2 = SUM2 + 1.
                   END IF 
               END IF
  500       CONTINUE
            OUT(NYEAR(MONTH,GPCPPTR),MONTH,GPCPPTR) = SUM2            
            XMAX(NYEAR(MONTH,GPCPPTR),MONTH,GPCPPTR) = SUM2
            XMIN(NYEAR(MONTH,GPCPPTR),MONTH,GPCPPTR) = 0.
         END IF
C
C  If parameter file indicates, compute monthly average temperature from
C  daily maximum and minimum
C
         IF (INELEM(NELEM).EQ.003.AND.GTMEAN) THEN
            IF (ENDYEAR(MONTH,MAXPTR).EQ.ENDYEAR(MONTH,NELEM)) THEN
               NYEAR(MONTH,GTMNPTR) = MIN0(NYEAR(MONTH,NELEM),
     +                                NYEAR(MONTH,MAXPTR))
               NYEAR1(MONTH,GTMNPTR) = MAX0(NYEAR1(MONTH,NELEM),
     +                                 NYEAR1(MONTH,MAXPTR))
               BGNYEAR(MONTH,GTMNPTR) = MAX0(BGNYEAR(MONTH,NELEM)
     +                             ,BGNYEAR(MONTH,MAXPTR))
               ENDYEAR(MONTH,GTMNPTR) = ENDYEAR(MONTH,NELEM)
               DO 600 I = 1,NUMDAYS(MONTH)
                  IF (TEMP(I).EQ.-99999..OR.VALUE(I).EQ.-99999.) THEN
                     TEMP(I) = -99999.
                  ELSE
                     TEMP(I) = (TEMP(I)+VALUE(I)) / 2.
                  END IF
  600          CONTINUE
               MISS4 = 0
               SUM4 = 0.
               OKTEMP = .TRUE.
               RMAXT = -99999.
               RMINT = 99999.
               DO 625 I = 1,NUMDAYS(MONTH)
                  IF (TEMP(I).EQ.-99999.) THEN
                     IF ((I+3).LE.NUMDAYS(MONTH).AND.TEMP(I+1)
     +                   .EQ.-99999..AND.TEMP(I+2).EQ.-99999.
     +                   .AND.TEMP(I+3).EQ.-99999.) THEN
                        OKTEMP = .FALSE.
                     END IF
                     MISS4 = MISS4 + 1
                     IF (MISS4.GT.5) THEN
                        OKTEMP = .FALSE.
                     END IF
                  ELSE
                     SUM4 = SUM4 + TEMP(I)
                     IF (TEMP(I).GT.RMAXT) THEN
                        RMAXT = TEMP(I)
                     END IF
                     IF (TEMP(I).LT.RMINT) THEN
                        RMINT = TEMP(I)
                     END IF
                  END IF
  625          CONTINUE
               IF (OKTEMP) THEN
                  OUT(NYEAR(MONTH,GTMNPTR),MONTH,GTMNPTR) 
     +                        = SUM4 / (NUMDAYS(MONTH)-MISS4)
                  XMAX(NYEAR(MONTH,GTMNPTR),MONTH,GTMNPTR) = RMAXT
                  XMIN(NYEAR(MONTH,GTMNPTR),MONTH,GTMNPTR) = RMINT
               ELSE
                  OUT(NYEAR(MONTH,GTMNPTR),MONTH,GTMNPTR) = -99999.
                  XMAX(NYEAR(MONTH,GTMNPTR),MONTH,GTMNPTR) = -99999.
                  XMIN(NYEAR(MONTH,GTMNPTR),MONTH,GTMNPTR) = -99999.
               END IF   
            END IF
         END IF
C
C IF COMPUTING TMEAN, SAVE TMAX VALUES FOR COMPUTING TMEAN WHEN TMIN IS
C ENCOUNTERED (SHOULD BE NEXT READ).
C
         IF (INELEM(NELEM).EQ.002.AND.GTMEAN) THEN
            DO 650 I = 1,NUMDAYS(MONTH)
               TEMP(I) = VALUE(I)
  650       CONTINUE
         END IF

         LATEYR = YEAR

  200 CONTINUE
C
C Processing completed.  Write out summary and close files.
C
  900 CONTINUE
      CLOSE (51)
      CALL LOCATE(24,79,IERR)
      WRITE(*,2100) RECCOUNT
 2100 FORMAT(///,' Processing Complete',/,2X,I7,' Records Processed')     
      IF (WTNMLFLG) THEN
         STOP ' '
      ELSE
         CALL WRTMSG(5,94,12,1,1,' ',0)
         STOP 2     
      END IF
      END        
C******************************************************************************
$PAGE
C
      SUBROUTINE SDPQ(T,N,AVG,SD,PQ,K)
C
C  This subroutine computes standard deviation, 
C  percentiles/quintiles from array T(N).
C
      INTEGER*2 LEVEL(5)
      REAL*4 T(N),PQ(5),D(5)
      CHARACTER*2 K
      LOGICAL INTCH
      
      DATA LEVEL /10,25,50,75,90/
C
C  Sort in ascending order with Shell Sort
C
      H = N
 100  IF (H.GT.1) THEN
         H = (H+1) / 2
 200     INTCH = .FALSE.
         NN = N - H
         DO 300 I = 1,NN
            J = I + H
            IF (T(I).GT.T(J)) THEN
               S = T(I)
               T(I) = T(J)
               T(J) = S
               INTCH = .TRUE.
            END IF
  300    CONTINUE
         IF (INTCH) THEN
            GOTO 200
         ELSE 
            GOTO 100
         END IF
      END IF
C
C   Compute percentiles or quintiles
C
      SUM = 0.
      DO 400 I = 1,N
         SUM = SUM + T(I)
 400  CONTINUE
      AVG = SUM / FLOAT(N)
      SD = -99999.
      IF (K.EQ.'QT') THEN
         DO 500 I = 1,4
            D(I) = I * (N+1) / 5.
            M = D(I)
            PQ(I) = T(M) + (T(M+1)-T(M))*(D(I)-FLOAT(M))
  500    CONTINUE
         PQ(5) = T(N)
      ELSE IF (K.EQ.'  ') THEN
         DO 600 I = 1,5
            PQ(I) = -99999.
  600    CONTINUE
      ELSE
         IF (K.EQ.'SD') THEN
            SD = 0.
            DO 700 I = 1,N
               SD = SD + (T(I)-AVG) ** 2 
  700       CONTINUE
            SD = SQRT(SD/(N-1))
         END IF
         DO 800 I = 1,5
            D(I) = LEVEL(I) * (N+1) / 100.
            M = D(I)
            PQ(I) = T(M) + (T(M+1)-T(M))*(D(I)-FLOAT(M))
  800    CONTINUE
      END IF
      RETURN
      END
C******************************************************************************
$PAGE
      SUBROUTINE EXTREME(RVAL,EXTR,FLAG,IYEAR,I,J,K,MAX)
C
C  This subroutine gets the extreme and returns it back. FLAG is set
C  to '*' to indicate the extreme value also occured in later years. The 
C  logical variable MAX indicates whether an extreme max or an extreme min
C  is being sought.
C
      CHARACTER*1 FLAG
C      INTEGER*4 IX,IY
      LOGICAL MAX
      REAL*4 RVAL,EXTR
      
      IF (RVAL.EQ.-99999.) THEN
          RETURN
      END IF
      IF (EXTR.EQ.99999..OR.EXTR.EQ.-99999.) THEN
          EXTR = RVAL
          IYEAR = I + J - K
          FLAG = ' '
          RETURN
      END IF
C      IY = NINT(EXTR*10.)
C      IX = NINT(RVAL*10.)
      IF (MAX) THEN
         IF (RVAL.GT.EXTR) THEN
            EXTR = RVAL
            IYEAR = I + J - K
            FLAG = ' '
         ELSE IF (RVAL.EQ.EXTR) THEN
            FLAG = '*'
         END IF
      ELSE
         IF (RVAL.LT.EXTR) THEN
            EXTR = RVAL
            IYEAR = I + J - K
            FLAG = ' '
         ELSE IF (RVAL.EQ.EXTR) THEN
            FLAG = '*'
         END IF
      END IF
      RETURN
      END 
C******************************************************************************
$PAGE
      SUBROUTINE COMPNORM (MD1,WTNMLFLG)
C
C  Process data and write out normals for every required 30-year period 
C  beginning with 1901-1930 or from starting operation of the station. 
C******************************************************************************
C
C   NOTES ON VARIABLE NAMES IN THIS SUBROUTINE
C
C   TMAX = ARRAY OF MONTHLY MAXIMUM VALUES (NOT MAX TEMPS!)
C   TMIN = ARRAY OF MONTHLY MINIMUM VALUES (NOT MIN TEMPS!)
C   TMEAN = ARRAY OF MONTHLY MEAN VALUES (NOT MEAN TEMPS!)
C
C  THE ABOVE ARRAYS (TMAX, TMIN, AND TMEAN) ARE USED IN THE COMPUTATION OF
C  STANDARD DEVIATIONS AND PERCENTILES/QUINTILES BY THE SUBROUTINE SDPQ
C------------------------------------------------------------------------------
C   MISSA = NUMBER OF MONTHLY VALUES MISSING BETWEEN END OF DATA AND
C           END OF THE NORMAL PERIOD
C   MISSB = NUMBER OF MONTHLY VALUES MISSING BETWEEN START OF NORMAL PERIOD
C           AND START OF THE DATA 
C   MISSW = NUMBER OF MONTHLY VALUES MISSING BETWEEN START AND END OF THE DATA
C           VALUES
C
C   THESE VARIBLES ARE USED IN COMPUTATIONS TO DETERMINE IF THE DATA
C   COMPLETENESS MEETS THE STANDARDS SET FORTH IN WMO TD/NO. 341
C******************************************************************************
 
      PARAMETER (MAXDAYS=31,MAXMONTHS=12,MAXYEARS=30,MAXELEMS=30)

      INTEGER*2 NA(MAXELEMS),NB(MAXELEMS),MD1,IBGN(MAXYEARS)
     +          ,IEND(MAXYEARS),IMONTH,IELEM
      CHARACTER*2 KEY
      REAL*4 TMEAN(MAXYEARS),TMAX(MAXYEARS),TMIN(MAXYEARS)
     +       ,YRMAX(MAXYEARS),YRMIN(MAXYEARS)
      LOGICAL OK,OKANNUAL,WTNMLFLG
      CHARACTER*24 WORK
      CHARACTER*24 BLANK
C       .. REFERS TO SECTION OF CODE COMMENTED OUT; SEE LATER COMMENTS     
C     REAL*4 YRSUM(MAXYEARS)
C
$INCLUDE:'NORMAL.INC'
C
      DATA BLANK /' '/, WORK /'Writing                '/

      ISTART = LATEYR - MD1
      ISTOP = ISTART + MAXYEARS - 1
C
C Compute the monthly normals, extremes, and statistics for each element
C

      DO 800 LLL = 1,NUMOUT
          IELEM = NDX(LLL)
          OKANNUAL = .TRUE.
          NA(IELEM) = 0
          NB(IELEM) = MAXYEARS + 1

C         
C     The following DO-loop checks for missing data and determines for each
C     30-year period (for a given month) the number of missing values After
C     and Before observation period as well as those Within it. BITVAL is 
C     set to 1 to indicate the year(s) for which data are available, and to
C     0 for the year(s) which have data missing.
C

          DO 100 IMONTH = 1,MAXMONTHS
              IF (BGNYEAR(IMONTH,IELEM).EQ.9999) THEN
                  OKANNUAL = .FALSE.
                  GOTO 100
              END IF
              DO 25 I = 1 ,MAXYEARS
                  BITVAL(I) = 0
   25         CONTINUE
              MISSA = MAXYEARS - NYEAR(IMONTH,IELEM)
              MISSB = NYEAR1(IMONTH,IELEM) - 1
              MISSW = 0
              OK = .TRUE.
              SUMMEAN = 0.
              EXTRMAX = -99999.
              EXTRMIN = 99999.
C
C   Check for missing data within the period for which data is available within
C   the thirty-year period.  If missing data rules are not met, the year-month
C   is skipped, otherwise, the year-month value is used in the computation of
C   the normal and extreme values.
C
              
              DO 50 L1 = NYEAR1(IMONTH,IELEM),NYEAR(IMONTH,IELEM)
                  IF (OUT(L1,IMONTH,IELEM).NE.-99999.) THEN
                      SUMMEAN = SUMMEAN + OUT(L1,IMONTH,IELEM)
                      TMEAN(L1-NYEAR1(IMONTH,IELEM)+1-MISSW) =
     +                     OUT(L1,IMONTH,IELEM)
                      TMAX(L1-NYEAR1(IMONTH,IELEM)+1-MISSW) =
     +                     XMAX(L1,IMONTH,IELEM)
                      TMIN(L1-NYEAR1(IMONTH,IELEM)+1-MISSW) = 
     +                     XMIN(L1,IMONTH,IELEM)
C                      CALL EXTREME(XMAX(L1,IMONTH,IELEM),EXTRMAX
                      CALL EXTREME(OUT(L1,IMONTH,IELEM),EXTRMAX
     +                        ,MAXAGAIN,YEARMAX,BGNYEAR(IMONTH,IELEM)
     +                        ,L1,NYEAR1(IMONTH,IELEM),.TRUE.)
C                      CALL EXTREME(XMIN(L1,IMONTH,IELEM),EXTRMIN,
                      CALL EXTREME(OUT(L1,IMONTH,IELEM),EXTRMIN,
     +                      MINAGAIN,YEARMIN,BGNYEAR(IMONTH,IELEM),L1,
     +                      NYEAR1(IMONTH,IELEM),.FALSE.) 
                      BITVAL(L1) = 1
                  ELSE
                      MISSW = MISSW + 1
                      IF ((L1+3).LE.NYEAR(IMONTH,IELEM)
     +                  .AND.OUT((L1+1),IMONTH,IELEM).EQ.-99999.
     +                  .AND.OUT((L1+2),IMONTH,IELEM).EQ.-99999.
     +                  .AND.OUT((L1+3),IMONTH,IELEM).EQ.-99999.) THEN
                             OK = .FALSE.
                      END IF
                  END IF 
   50         CONTINUE
C 
C If no extreme value was found, then no acceptable data was found for the
C given month.  Thus, computation of the monthly normal will be skipped, and
C a flag (OKANNUAL) is set to indicate not to compute an annual normal.
C 
              IF (EXTRMAX.EQ.-99999.OR.EXTRMIN.EQ.99999.) THEN
                  OKANNUAL = .FALSE.
                  GOTO 100
              END IF

C
C  Since data was found for the month, apply the rules for missing data to 
C  the monthly values.  If > 3 year-month values missing before or after the
C  observing period, or if the total number of missing year-month values is
C  > 5, the montly normal must be considered a "Provisional Normal" (OK = 
C  .FALSE.).  If the total number of data years is less than ten (NUMYEARS), no
C  normal value can be computed.  If data completeness rules are met, the
C  normal and statistics will be computed, and a record written to the
C  output file.
C
              MISSING = MISSA + MISSB + MISSW
              IF (MISSA.GT.3.OR.MISSB.GT.3.OR.MISSING.GT.5) THEN
                  OK = .FALSE.
              END IF
              NUMYEAR = MAXYEARS - MISSING
              IF (NUMYEAR.LT.10) THEN
                 OKANNUAL = .FALSE.
                 GOTO 100
              END IF

C
C Data completeness rules are met, so compute monthly normal and statistics.
C
C Note that standard deviations are only computed for continuous elements
C (e.g. Maximum Temperature - or any input element with type = M) which
C can be assumed to be normally distributed.  Zero-bounded elements (e.g.
C precipitation - or any element with type = T or C) are not distributed
C normally (best fit is Gammma Distribution), so standard deviations are
C not computed.  Also, after this program was originally coded, it was 
C determined that the MEAN MLY MAX, MEAN MLY MIN, and their respective 
C standard deviations are meaningless for elements that are counted 
C (e.g. Number of Days with Precipitation > 1 mm - or anything else
C with type = C), thus the conditional calling of SDPQ for the MAX and MIN 
C arrays.
C 
C
  
              RNORMAL(IMONTH) = SUMMEAN / NUMYEAR
              IF (.NOT.OK) THEN
                  NMLFLAG(IMONTH,IELEM) = 'P'
              END IF
              IF (INELEM(IELEM).EQ.005
     +           .AND.NORMTYP(IELEM).EQ.'T') THEN
                    KEY = 'QT'
              ELSE IF (NORMTYP(IELEM).EQ.'M') THEN
                    KEY = 'SD'
              ELSE IF (NORMTYP(IELEM).EQ.'T') THEN
                    KEY = 'PT'
              ELSE IF (NORMTYP(IELEM).EQ.'C') THEN
                    KEY = 'PT'
              ELSE 
                    KEY = '  '
                    RNORMAL(IMONTH) = ANINT(RNORMAL(IMONTH))
              END IF
              CALL SDPQ(TMEAN,NUMYEAR,AVGMEAN,SDM,PQM,KEY)
              IF (NORMTYP(IELEM).NE.'C') THEN
                  CALL SDPQ(TMAX,NUMYEAR,AVGMAX,SDX,PQX,KEY)
                  CALL SDPQ(TMIN,NUMYEAR,AVGMIN,SDN,PQN,KEY)
              ELSE
                  DO 75 N = 1,5
                     PQX(N) = -99999.
                     PQN(N) = -99999.
   75             CONTINUE
                  AVGMAX = -99999.
                  AVGMIN = -99999.
                  SDX = -99999.
                  SDN = -99999.
                  EXTRMAX = -99999.
                  EXTRMIN = -99999.
                  MAXAGAIN = ' '
                  MINAGAIN = ' '
                  YEARMAX = -999
                  YEARMIN = -999
              END IF
              WORK(9:18) = PREVID
              WRITE(WORK(19:24),'(I3.3,1X,I2)') OUTELEM(IELEM),IMONTH
              CALL LOCATE(23,55,IERR)
              CALL WRTSTR(WORK, 24,3,0)
              CALL WRTNML(IMONTH,IELEM,ISTART,ISTOP)
              WTNMLFLG = .TRUE.
C 
C  Determine the earliest begin-date and the latest end-date for each
C  element.  This information is used in the computation of the annual
C  extreme values.
C
              IF (NYEAR1(IMONTH,IELEM).GT.NA(IELEM)) THEN
                  NA(IELEM) = NYEAR1(IMONTH,IELEM)
                  IBGN(IELEM) = BGNYEAR(IMONTH,IELEM)
              END IF
              IF (NYEAR(IMONTH,IELEM).LT.NB(IELEM)) THEN
                  NB(IELEM) = NYEAR(IMONTH,IELEM)
                  IEND(IELEM) = ENDYEAR(IMONTH,IELEM)
              END IF
              
  100      CONTINUE
  
C
C       Compute annual Normal record
C
           IF (.NOT.OKANNUAL) THEN
               GOTO 800
           END IF
C
C Compute the annual normal from the 12 monthly normals

           RNORMAL(13) = 0.
           DO 500 I = 1,MAXMONTHS                  
              RNORMAL(13) = RNORMAL(13) + RNORMAL(I) 
              IF (NMLFLAG(I,IELEM).EQ.'P') THEN
                  NMLFLAG(13,IELEM) = 'P'
              END IF
  500      CONTINUE
           IF (NORMTYP(IELEM).EQ.'M') THEN
               RNORMAL(13) = RNORMAL(13) / FLOAT(MAXMONTHS)
           END IF

C
C  The following loop sums the 12 monthly values for each year, yielding
C  an annual value.  The thirty annual values are then used to compute the
C  percentiles/quintiles and extreme values for the annual normal.  This method
C  yields statistics that are meaningless, so this code has been commented
C  out. A section of code follows this which is a cut down version for
C  finding the extreme values only.
C
C           DO 200 I = 1 ,MAXYEARS
C               BITVAL(I) = 0
C  200      CONTINUE
C           MISSYR = 0
C           EXTRMAX = -99999.
C           EXTRMIN = 99999.
C           DO 300 J1 = NA(IELEM),NB(IELEM)
C              YRSUM(J1) = 0.
C              YRMAX(J1) = -99999.
C              YRMIN(J1) = 99999.
C              MISSYR = 0
C              DO 350 J2 = 1,MAXMONTHS
C                 IF (OUT(J1,J2,IELEM).EQ.-99999.) THEN
C                    YRSUM(J1) = -99999.
C                    YRMAX(J1) = -99999.
C                    YRMIN(J1) = -99999.
C                   MISSYR = MISSYR + 1
C                    GOTO 300
C                 ELSE
C                    YRSUM(J1) = YRSUM(J1) + OUT(J1,J2,IELEM)
C                    IF (NORMTYP(IELEM).EQ.'M') THEN
C                       IF (XMAX(J1,J2,IELEM).GT.YRMAX(J1)) THEN
C                          YRMAX(J1) = XMAX(J1,J2,IELEM)
C                       END IF
C                       IF (XMIN(J1,J2,IELEM).LT.YRMIN(J1)) THEN
C                          YRMIN(J1) = XMIN(J1,J2,IELEM)
C                       END IF
C                    END IF
C                 END IF
C  350         CONTINUE
C              IF (NORMTYP(IELEM).EQ.'M') THEN
C                  YRSUM(J1) = YRSUM(J1) / 12.
C              ELSE 
C                  YRMAX(J1) = YRSUM(J1)
C                  YRMIN(J1) = 0.
C              END IF
C              TMEAN(J1-NA(IELEM)+1-MISSYR) = YRSUM(J1)
C              TMAX(J1-NA(IELEM)+1-MISSYR) = YRMAX(J1)
C              TMIN(J1-NA(IELEM)+1-MISSYR) = YRMIN(J1)
C              CALL EXTREME(YRMAX(J1),EXTRMAX,MAXAGAIN,YEARMAX
C     +          ,IBGN(IELEM),J1,NA(IELEM),.TRUE.)
C              CALL EXTREME(YRMIN(J1),EXTRMIN,MINAGAIN,YEARMIN
C     +           ,IBGN(IELEM),J1,NA(IELEM),.FALSE.)
C              BITVAL(J1) = 1
C  300      CONTINUE
C           IF (EXTRMIN.EQ.99999.) THEN
C              EXTRMIN = -99999.
C           END IF
C          NUMYEAR = NB(IELEM) - NA(IELEM) + 1 - MISSYR
C           IF (INELEM(IELEM).EQ.005
C     +       .AND.NORMTYP(IELEM).EQ.'T') THEN
C                  KEY = 'QT'
C           ELSE IF (NORMTYP(IELEM).EQ.'M') THEN
C                  KEY = 'SD'
C           ELSE IF (NORMTYP(IELEM).EQ.'T') THEN
C                  KEY = 'PT'
C           ELSE IF (NORMTYP(IELEM).EQ.'C') THEN
C                 KEY = 'PT'
C           ELSE
C                  KEY = '  '
C           END IF
C           CALL SDPQ(TMEAN,NUMYEAR,AVGMEAN,SDM,PQM,KEY)
C           CALL SDPQ(TMAX,NUMYEAR,AVGMAX,SDX,PQX,KEY)
C           CALL SDPQ(TMIN,NUMYEAR,AVGMIN,SDN,PQN,KEY)
C
C
C
C   The following code is a cut-down version of the statisics above.
C   This code only finds the extreme values for the annual normal
C
           EXTRMAX = -99999.
           EXTRMIN = 99999.
           DO 300 J1 = NA(IELEM),NB(IELEM)
              YRMAX(J1) = -99999.
              YRMIN(J1) = 99999.
              DO 350 J2 = 1,MAXMONTHS
                 IF (OUT(J1,J2,IELEM).NE.-99999.) THEN
                    IF (NORMTYP(IELEM).EQ.'M'.OR.
     +                    (NORMTYP(IELEM).EQ.'T'.AND.OUT(J1,J2,IELEM)
     +                     .NE.0)) THEN
C                       IF (XMAX(J1,J2,IELEM).GT.YRMAX(J1)) THEN
C                          YRMAX(J1) = XMAX(J1,J2,IELEM)
C                       END IF
C                       IF (XMIN(J1,J2,IELEM).LT.YRMIN(J1)) THEN
C                          YRMIN(J1) = XMIN(J1,J2,IELEM)
C                       END IF
                       YRMAX(J1) = AMAX1(YRMAX(J1),OUT(J1,J2,IELEM))
                       YRMIN(J1) = AMAX1(YRMIN(J1),OUT(J1,J2,IELEM))
                    END IF
                 END IF
  350         CONTINUE
              CALL EXTREME(YRMAX(J1),EXTRMAX,MAXAGAIN,YEARMAX
     +           ,IBGN(IELEM),J1,NA(IELEM),.TRUE.)
              CALL EXTREME(YRMIN(J1),EXTRMIN,MINAGAIN,YEARMIN
     +            ,IBGN(IELEM),J1,NA(IELEM),.FALSE.)
  300      CONTINUE
           IF (EXTRMIN.EQ.99999.) THEN
              EXTRMIN = -99999.
            END IF

C
C WRITE ANNUAL NORMAL RECORD
C
C Make annual statistics missing
C
           DO 700 I = 1,5
              PQX(I) = -99999.
              PQM(I) = -99999.
              PQN(I) = -99999.
  700      CONTINUE
           AVGMEAN = -99999.
           AVGMAX = -99999.
           AVGMIN = -99999.
           SDM = -99999.         
           SDX = -99999.
           SDN = -99999.
           IF (NORMTYP(IELEM).EQ.'C') THEN
              YEARMAX = -999
              YEARMIN = -999
              MAXAGAIN = ' '
              MINAGAIN = ' '
           END IF
           DO 750 I = 1,MAXYEARS
              BITVAL(I) = 9
  750      CONTINUE
C
           WORK(9:18) = PREVID
           WRITE(WORK(19:24),'(I3.3,1X,I2)') OUTELEM(IELEM),13
           CALL LOCATE(23,55,IERR)
           CALL WRTSTR(WORK,24,3,0)
           CALL WRTNML(13,IELEM,ISTART,ISTOP)
           WTNMLFLG = .TRUE.

  800 CONTINUE
      CALL LOCATE(23,55,IERR)
      CALL WRTSTR(BLANK,24,0,0)
      RETURN
      END
      
C******************************************************************************
$PAGE

      SUBROUTINE INITVALS

      PARAMETER (MAXDAYS=31,MAXMONTHS=12,MAXYEARS=30,MAXELEMS=30)

$INCLUDE:'NORMAL.INC'

C
C  Reset values --------------
C
      DO 100 I = 1,MAXELEMS
         DO 150 J = 1,MAXMONTHS
              NYEAR(J,I) = 0
              NYEAR1(J,I) = 0
              NTEST(J,I) = 0
              BGNYEAR(J,I) = 9999 
              ENDYEAR(J,I) = 1900
              NMLFLAG(J,I) = DSTATUS(I)
  150    CONTINUE
         NDX(I) = 0
         NMLFLAG(13,I) = DSTATUS (I)
  100 CONTINUE
      DO 200 I = 1,MAXDAYS
         TEMP(I) = 0.
  200 CONTINUE
      LATEYR = 1901
      NUMOUT = 0

      RETURN
      END
C******************************************************************************
$page
      SUBROUTINE WRTNML(IMONTH,IELEM,JSTART,JSTOP)
C
C   THIS ROUTINE WRITES A NML FORMAT DATAEASE RECORD AS A BINARY FILE.
C   THE VARIABLES WITHIN THE RECORD ARE EQUIVALENCED TO THE APPROPRIATE
C   POSITION WITHIN INREC.                      
C
C   NOTE:  NUMERIC VARIABLES ARE EQUIVALENCED TO THE CHARACTER VARIABLE INREC.
C          SOME OF THE NUMERIC VARIABLES START ON ODD-BYTE BOUNDARIES.  EVEN
C          THOUGH THE DOCUMENTATION FOR MICROSOFT 5.0 FORTRAN SPECIFIES THAT
C          NON-CHARACTER ENTITIES MUST START ON EVEN-BYTE BOUNDARIES, THIS IS
C          NOT AN ACTUAL FORTRAN REQUIREMENT.  THIS HAS BEEN VERIFIED WITH
C          MICROSOFT TECHNICAL SUPPORT.  THE CURRENT EQUIVALENCES WORK AS
C          WRITTEN.  (5-20-91)
C
      PARAMETER (MAXDAYS=31,MAXMONTHS=12,MAXYEARS=30,MAXELEMS=30)
C
C       .. DUMMY ARGUMENTS
      INTEGER*2 IMONTH,IELEM,JSTART,JSTOP
C
      CHARACTER*1 INREC(165)
C
      CHARACTER*8 STNID
      CHARACTER*4 YEAR,YEAR1,MXYR,MNYR
      CHARACTER*3 ELEM
      CHARACTER*1 EMAXFG,EMINFG
      CHARACTER*2 OMONTH
      REAL*4 NORM,SDEMEAN,EMAX,EMIN,MNMYMX,MNMYMN,SDEMAX,SDEMIN
     +       ,P1(5),P2(5),P3(5)
      INTEGER*2 HEADER(2)
      INTEGER*1 TYPNML,NMLFG
C      
$INCLUDE:'NORMAL.INC'
C
C
C       ** EQUIVALENCE THE INPUT VARIABLES TO THE INPUT RECORD STRING        
C          
      EQUIVALENCE (HEADER,INREC(1)),(STNID,INREC(11)),(YEAR,INREC(19)),
     +          (YEAR1,INREC(23)),(OMONTH,INREC(27)),
     +          (ELEM,INREC(29)),(NORM,INREC(32)),(TYPNML,INREC(36)),
     +          (NMLFG,INREC(37)),(SDEMEAN,INREC(38)),(EMAX,INREC(42)),
     +          (EMAXFG,INREC(46)),(MXYR,INREC(47)),(EMIN,INREC(51)),
     +          (EMINFG,INREC(55)), (MNYR,INREC(56)),   (P1,INREC(60)),
     +          (MNMYMX,INREC(80)), (SDEMAX,INREC(84)), (P2,INREC(88)),
     +          (MNMYMN,INREC(108)),(SDEMIN,INREC(112)),(P3,INREC(116))
C
C   WRITE THE INFORMATION INTO THE DATEASE RECORD HEADER AND THEN
C   WRITE THE VALUES INTO THE DATAEASE RECORD             
C
      HEADER(1) = 12
      HEADER(2) = 0
C         
      STNID = PREVID
      WRITE(YEAR,'(I4)') JSTART 
      WRITE(YEAR1,'(I4)') JSTOP 
      WRITE(OMONTH,'(I2)') IMONTH
      WRITE(ELEM,'(I3.3)') OUTELEM(IELEM)
      NORM = RNORMAL(IMONTH)
      IF (NORMTYP(IELEM).EQ.'M') THEN
          TYPNML = INT1(1)
      ELSE IF (NORMTYP(IELEM).EQ.'C') THEN
          TYPNML = INT1(3)
      ELSE 
          TYPNML = INT1(2)
      END IF
      IF (NMLFLAG(IMONTH,IELEM).EQ.'P') THEN
          NMLFG = INT1(2)
      ELSE
          NMLFG = INT1(1)
      END IF
      SDEMEAN = SDM
      EMAX = EXTRMAX
      EMAXFG = MAXAGAIN
      WRITE(MXYR,'(I4)') YEARMAX
      EMIN = EXTRMIN
      EMINFG = MINAGAIN
      WRITE(MNYR,'(I4)') YEARMIN
      MNMYMX = AVGMAX
      SDEMAX = SDX
      MNMYMN = AVGMIN
      SDEMIN = SDN
C      
      DO 200 I=1,5
          P1(I) = PQM(I)
          P2(I) = PQX(I)
          P3(I) = PQN(I)
 200  CONTINUE

      DO 230 I=1,MAXYEARS
         IPOS = 136 + (I-1)
         INREC(IPOS) = BITVAL(I)
230   CONTINUE

      WRITE(51) INREC
      RETURN
      END
