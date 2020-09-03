$STORAGE:2

      PROGRAM ELEMEXTR
C
C   PROGRAM TO READ MLY,10D,DLY,SYN,HLY,OR 15M CLIMATE DATA FROM DATAEASE
C      FORMS AND COMPUTE MEANS AND EXTREMES FOR EACH ELEMENT READ.
C      THE STATISTICS ARE WRITTEN TO ELEMEXTR.DAT FOR IMPORT
C      BACK INTO THE DATAEASE FORM "STN ELEMENT EXTREMES".
C
      PARAMETER (MAXELEM = 90,MAXMONTHS=12,MAXVLS=96)
 
      CHARACTER*80 MESSAGE
      CHARACTER*64 FILNAME
      CHARACTER*8 STNID,STNWANTED,PREVID,PERIOD
      CHARACTER*3 RECTYPE,TYPEDEF(7)
      CHARACTER*2 RTNFLAG
      CHARACTER*1 FLAG1(MAXVLS),OUTTYPE
      CHARACTER*1 DATASOURCE,RTNCODE
      INTEGER*2 DDSID,YEAR,MONTH,DAY,NUMYEARS(MAXELEM,MAXMONTHS)
     +         ,MAXYEAR(MAXELEM,MAXMONTHS),MINYEAR(MAXELEM,MAXMONTHS)
     +         ,MXDAY(MAXELEM,MAXMONTHS),MINDAY(MAXELEM,MAXMONTHS)
     +         ,LOWYEAR(MAXELEM,MAXMONTHS), HIGHYEAR(MAXELEM,MAXMONTHS)
     +         ,ELEM(MAXELEM),PREVMON(MAXELEM),PREVYEAR(MAXELEM)
     +         ,LROW,LCOL,HROW,HCOL,FRMWID,FRMHT
C       .. REVISION JML 3/11/94 -- CHANGE C*3 TO I*2     
      INTEGER*2 MINFLAG(MAXELEM,MAXMONTHS),MAXFLAG(MAXELEM,MAXMONTHS)
      INTEGER*4 RECCOUNT,NRECCOUNT
      INTEGER*4 VALCOUNT(MAXELEM,MAXMONTHS),I1
      REAL*4  MAXVALUE(MAXELEM,MAXMONTHS),MINVALUE(MAXELEM,MAXMONTHS)
     +         ,MAXCHANGE(MAXELEM,MAXMONTHS),ENDRECVAL(MAXELEM)
     +         ,VALUE(MAXVLS),CHANGE
      REAL*8    MEANVALUE(MAXELEM,MAXMONTHS)
     +         ,SUMSQUARES(MAXELEM,MAXMONTHS)
     +         ,VARIANCE, STNDRDDEV
      CHARACTER*7 OUTCNT(2) 
      CHARACTER*8 CURRTIM,NXTTIM,PREVTIM
      LOGICAL DATAOK
      COMMON /SCRTCH/ MAXVALUE,MINVALUE,MAXCHANGE,MEANVALUE,SUMSQUARES
     +               ,MAXYEAR,MINYEAR,MXDAY,MINDAY
C
      DATA RECCOUNT/0/, NRECCOUNT/0/ ,OUTCNT/'       ','      '/
     +    ,DATAOK /.FALSE./
      DATA TYPEDEF/'MLY','10D','DLY','SYN','HLY','15M','U-A'/
C      
      OUTCNT(2)(1:1)=CHAR(0)
C
C     (OPEN THE OUTPUT FILE NO MATTER WHAT.  THAT WAY IT WILL NOT HAVE
C      OLD DATA IN IT IF THE PROGRAM IS NOT RUN.) 
C
   10 CONTINUE
      OPEN(51,FILE='Q:EXTREMES.DAT',STATUS='UNKNOWN',IOSTAT=IOCHK)
      IF(IOCHK.NE.0) THEN
         CALL OPENMSG('Q:EXTREMES.DAT        ','ELEMEXTR    ',IOCHK)
         GO TO 10
      END IF   
      ENDFILE 51
      REWIND 51
      CALL SETMOD(3,IERR)
      CALL WRTMSG(24,243,14,0,0,' ',0)
      CALL WRTMSG(22,244,14,0,0,' ',0)
      STNWANTED = '       '
      CALL LOCATE(5,15,IERR)
      CALL WRTSTR('STATION-ID ',11,14,0)
      CALL WRTSTR(STNWANTED,8,15,1)
      CALL WRTFNC(9)
   20 CONTINUE
      CALL LOCATE(5,26,IERR)
      CALL GETSTR(0,STNWANTED,8,15,1,RTNFLAG)
      IF (RTNFLAG.EQ.'1F') THEN
         FILNAME = 'P:\HELP\ELEMEXTR.HLP'
         CALL DSPWIN(FILNAME)
         GO TO 20
      ELSE IF (RTNFLAG.EQ.'1S') THEN
         CALL GETSTN(STNWANTED)
         CALL LOCATE(5,26,IERR)
         CALL WRTSTR(STNWANTED,8,15,1)
         IF (STNWANTED.EQ.'        ') THEN
            GO TO 20
         END IF
      ELSE IF (RTNFLAG.EQ.'4F') THEN
         CALL LOCATE(24,0,IERR)
         STOP 2
      ELSE IF (RTNFLAG.NE.'  '.AND.RTNFLAG.NE.'2F'.AND.
     +         RTNFLAG.NE.'RE') THEN
         CALL BEEP
         GO TO 20
      END IF
      CALL LOCATE(8,2,IERR)
      CALL DATATYPE(2,1,RECTYPE,NUMVAL,RTNCODE)
      IF (RTNCODE.NE.'0') THEN
         GO TO 20
      END IF 
      IF (RECTYPE.EQ.'U-A') THEN
         CALL WRTMSG(4,245,12,1,0,' ',0)
         GO TO 20
      ELSE
         DO 30 I=1,6
            IF (RECTYPE.EQ.TYPEDEF(I)) GO TO 32
   30    CONTINUE            
   32    CONTINUE
         ITYPE = I
         WRITE(OUTTYPE,'(I1)') ITYPE
      END IF
C
      LROW=7
      LCOL=38
      FRMHT=13
      FRMWID=42
      CALL WRTFNC(14)
      CALL LOCATE(LROW,LCOL,IERR)
      CALL DSPFRM('ELEMEXTR',FRMHT,FRMWID,1,LROW,LCOL,RTNCODE)
   40 CALL GETCHAR(0,RTNFLAG)
      IF (RTNFLAG.EQ.'4F'.OR. RTNCODE.NE.'0') THEN
         CALL LOCATE(24,0,IERR)
         STOP 2
      ELSE IF (RTNFLAG.NE.'2F') THEN
         GO TO 40
      ENDIF   
      HROW=LROW+FRMHT-1
      HCOL=LCOL+FRMWID-1
      CALL SCROLL(1,FRMHT,LROW,LCOL,HROW,HCOL)
C
      CALL LOCATE(8,40,IERR)
      CALL DATASRC(40,DATASOURCE,RTNCODE)
      IF (RTNCODE.NE.'0') THEN
         CALL LOCATE(24,0,IERR)
         STOP 2
      END IF 
C
C   OPEN THE INPUT FILE 
C
      CALL OPENINPUT(RECTYPE,DATASOURCE)
C
C  WRITE THE RUNNING TOTAL LINE
C
      CALL CLRMSG(1)
      CALL LOCATE(23,0,IERR)
      CALL WRTSTR('Records Read -          Records processed - '
     +             ,44,14,0)
      PREVID  = 'ZZZZZZZ'
      PREVTIM = 'ZZZZZZZ'
      NXTTIM  = 'ZZZZZZZ'
      YEAR  = 0
      MONTH = 0
      DAY   = 0
      DO 50 I = 1,MAXELEM
         PREVMON(I) = 0
         PREVYEAR(I) = 0
         ENDRECVAL(I) = -99999.
50    CONTINUE
C
C   DO THE PROCESSING -------------------------------------------------
C
      DO 400 I1=1,999999
$INCLUDE: 'READAREC.INC'
         NRECCOUNT = NRECCOUNT + 1
         CALL LOCATE(23,15,IERR)
         WRITE(OUTCNT,'(I7)') NRECCOUNT
         CALL CWRITE(OUTCNT,12,IERR)
         IF (RECTYPE.EQ.'MLY'.OR.RECTYPE.EQ.'10D') THEN
            INMONTH = 0
         ELSE
            INMONTH = MONTH
         ENDIF      
         WRITE(CURRTIM,'(I4.4,2I2.2)') YEAR,INMONTH,DAY
         IF (STNID.NE.PREVID.OR.RTNCODE.NE.'0') THEN
            IF (PREVID.NE.'ZZZZZZZ') THEN
C
C         HAVE READ THRU PERIOD OF RECORD FOR A STATION - 
C         WRITE THE RESULTS FOR THAT STATION
C
               DO 300 I=1,NUMELEM
                  JELEM = ELEM(I)
                  DO 250 J=1,MAXMONTHS
                     IF (VALCOUNT(I,J).GT.0) THEN
                        WRITE(MESSAGE,120) PREVID,JELEM,J
  120                   FORMAT(A8,' WRITING ELEMENT=',I3.3,
     +                     ' MONTH=',I2)
                        CALL LOCATE(24,0,IERR)
                        CALL WRTSTR(MESSAGE,72,14,0)
                        IF (VALCOUNT(I,J).GT.1) THEN
                           VARIANCE = (SUMSQUARES(I,J) - 
     +                            ((MEANVALUE(I,J) ** 2)
     +                            / VALCOUNT(I,J)))/(VALCOUNT(I,J)-1)
                           IF (VARIANCE.GE.0) THEN
                              STNDRDDEV = SQRT(VARIANCE)
                           ELSE
                              STNDRDDEV = 0.0
                           END IF 
                        ELSE
                              STNDRDDEV = 0.0
                        ENDIF    
                        MEANVALUE(I,J) = MEANVALUE(I,J) / 
     +                         DBLE(VALCOUNT(I,J))
                        WRITE(51,200) PREVID,OUTTYPE,
     +                     JELEM,J,LOWYEAR(I,J),HIGHYEAR(I,J),
     +                     NUMYEARS(I,J),MAXVALUE(I,J),
     +                     MAXYEAR(I,J),J,MXDAY(I,J),MAXFLAG(I,J),
     +                     MINVALUE(I,J),MINYEAR(I,J),J,MINDAY(I,J),
     +                     MINFLAG(I,J),MEANVALUE(I,J),STNDRDDEV,
     +                     MAXCHANGE(I,J)
200                     FORMAT(A8,',',A1,',',I3.3,',,',I2.2,',1
     +                    ,',I4.4,',',I4.4,',',I3.3,',',2(F9.2,',',
     +                    I4,2I2.2,',',I2.2,','),F10.3,',',F10.4,',',
     +                    F9.2)
                        DATAOK = .TRUE.
                     END IF
  250            CONTINUE
  300          CONTINUE
            END IF
C
C         RESET ALL OF THE COUNTERS AND ACCUMULATING VALUES
C
            NUMELEM = 0
            PREVID = STNID
            PREVTIM = 'ZZZZZZZ'
            NXTTIM  = 'ZZZZZZZ'
            DO 345 I=1,MAXELEM
               ELEM(I) = -999
               ENDRECVAL(I) = -99999.
               PREVYEAR(I) = 0
               DO 340 J=1,MAXMONTHS
                  PREVMON(J) = 0
                  NUMYEARS(I,J) = 0
                  MAXVALUE(I,J) = 0
                  VALCOUNT(I,J) = 0
                  SUMSQUARES(I,J) = 0.0
                  MAXCHANGE(I,J) = 0
                  LOWYEAR(I,J) = 9999
                  HIGHYEAR(I,J) = -9999      
                  MAXVALUE(I,J) = -99999.
                  MINVALUE(I,J) =  99999.
                  MEANVALUE(I,J) =0.0
  340          CONTINUE
  345       CONTINUE
         END IF
         IF (RTNCODE.NE.'0') THEN
            GO TO 401
         ELSE IF (STNWANTED.NE.'ALL    ') THEN
            IF (STNID.LT.STNWANTED) THEN
               GO TO 400
            ELSE IF (STNID.GT.STNWANTED) THEN
               GO TO 401
            END IF
         END IF
C
C          ** PROCESS CURRENT STATION
C         
         RECCOUNT = RECCOUNT + 1
         CALL LOCATE(23,44,IERR)
         WRITE(OUTCNT,'(I7)') RECCOUNT
         CALL CWRITE(OUTCNT,12,IERR)
C
C          ** CALCULATE AND SAVE TIMES USED TO DETERMINE METHOD OF
C             CALCULATING RATE OF CHANGE FOR FIRST VALUE IN RECORD
         IF (CURRTIM.NE.PREVTIM) THEN
            IF (PREVTIM.NE.'ZZZZZZZ') THEN
               NXTTIM=PREVTIM
               CALL GTNXTTIM(ITYPE,NXTTIM)
            ENDIF   
            PREVTIM = CURRTIM
         ENDIF   
C
C      SKIP ELEMENTS THAT SHOULD NOT BE TREATED STATISTICALLY
C
         IF (RECTYPE.EQ.'DLY') THEN
            IF ((IELEM.GE.29.AND.IELEM.LE.42).OR.IELEM.EQ.59.OR.
     +             IELEM.EQ.61) THEN
               GO TO 400
            END IF
         ELSE IF (RECTYPE.EQ.'MLY') THEN
            IF (IELEM.EQ.205.OR.IELEM.EQ.207.OR.IELEM.EQ.211.OR.IELEM.
     +            EQ.213.OR.IELEM.EQ.220.OR.IELEM.EQ.222.OR.(IELEM.GE.
     +            229.AND.IELEM.LE.248).OR.IELEM.EQ.253.OR.IELEM.EQ.
     +            255.OR.IELEM.EQ.260.OR.IELEM.EQ.262.OR.IELEM.EQ.264)
     +             THEN
               GO TO 400
            END IF
         ELSE IF (RECTYPE.EQ.'10D') THEN
            IF (IELEM.EQ.405.OR.IELEM.EQ.407.OR.IELEM.EQ.411.OR.IELEM.
     +            EQ.413.OR.IELEM.EQ.420.OR.IELEM.EQ.422.OR.(IELEM.GE.
     +            429.AND.IELEM.LE.448).OR.IELEM.EQ.453.OR.IELEM.EQ.
     +            455.OR.IELEM.EQ.460.OR.IELEM.EQ.462.OR.IELEM.EQ.464)
     +             THEN
               GO TO 400
            END IF
         ELSE IF (RECTYPE.NE.'15M') THEN
            IF (IELEM.EQ.112.OR.(IELEM.GE.116.AND.IELEM.LE.131).OR.
     +            (IELEM.GE.140.AND.IELEM.LE.171)) THEN
               GO TO 400
            END IF
         END IF
C
C     DETERMINE THE RELATIVE NUMBER OF THE CURRENT ELEMENT
C
         IF (NUMELEM.EQ.0) THEN
            NUMELEM = 1
            NELEM = 1
            ELEM(NUMELEM) = IELEM
         ELSE
            DO 190 I6 = 1,NUMELEM
               IF (IELEM.EQ.ELEM(I6)) THEN
                  NELEM = I6
                  GO TO 195
               END IF
  190       CONTINUE
            NUMELEM = NUMELEM + 1
            NELEM = NUMELEM
            ELEM(NUMELEM) = IELEM
  195       CONTINUE
         END IF
         IF (RECTYPE.NE.'MLY'.AND.RECTYPE.NE.'10D') THEN
C             .. DLY,SYN,HLY,OR 15M DATA
            IF (YEAR.GT.HIGHYEAR(NELEM,MONTH))
     +                                HIGHYEAR(NELEM,MONTH) = YEAR
            IF (YEAR.LT.LOWYEAR(NELEM,MONTH))
     +                                 LOWYEAR(NELEM,MONTH) = YEAR
            IF (MONTH.NE.PREVMON(NELEM).OR.YEAR.NE.PREVYEAR(NELEM)) THEN
               NUMYEARS(NELEM,MONTH) = NUMYEARS(NELEM,MONTH) + 1
            END IF
            PREVMON(NELEM) = MONTH
            PREVYEAR(NELEM) = YEAR
         END IF
         DO 390 J=1,NUMVAL
            IF (VALUE(J).NE.-99999.) THEN
               IF (RECTYPE.EQ.'MLY') THEN
                  MONTH = J
                  IF (YEAR.GT.HIGHYEAR(NELEM,MONTH))
     +                                HIGHYEAR(NELEM,MONTH) = YEAR
                  IF (YEAR.LT.LOWYEAR(NELEM,MONTH))
     +                                 LOWYEAR(NELEM,MONTH) = YEAR
                  NUMYEARS(NELEM,MONTH) = NUMYEARS(NELEM,MONTH) + 1
               ELSE IF (RECTYPE.EQ.'10D') THEN
                  MONTH = ((J-1) / 3) + 1
                  IF (YEAR.GT.HIGHYEAR(NELEM,MONTH))
     +                                HIGHYEAR(NELEM,MONTH) = YEAR
                  IF (YEAR.LT.LOWYEAR(NELEM,MONTH))
     +                                 LOWYEAR(NELEM,MONTH) = YEAR

                  IF(MONTH.NE.PREVMON(NELEM).OR.YEAR.NE.PREVYEAR(NELEM))
     +                                                            THEN
                     NUMYEARS(NELEM,MONTH) = NUMYEARS(NELEM,MONTH) + 1
                  END IF
                  PREVMON(NELEM) = MONTH
                  PREVYEAR(NELEM) = YEAR
               END IF
               IF (VALUE(J).EQ.MAXVALUE(NELEM,MONTH))
     +            MAXFLAG(NELEM,MONTH) = 2
               IF (VALUE(J).EQ.MINVALUE(NELEM,MONTH)) 
     +            MINFLAG(NELEM,MONTH) = 2
               IF (VALUE(J).GT.MAXVALUE(NELEM,MONTH)) THEN
                  MAXVALUE(NELEM,MONTH) = VALUE(J)
                  MAXYEAR(NELEM,MONTH) = YEAR
                  IF (RECTYPE.EQ.'DLY') THEN
                     MXDAY(NELEM,MONTH) = J
                  ELSE IF (RECTYPE.NE.'MLY'.AND.RECTYPE.NE.'10D') THEN
                     MXDAY(NELEM,MONTH) = DAY
                  END IF
                  MAXFLAG(NELEM,MONTH) = 1
              END IF
              IF (VALUE(J).LT.MINVALUE(NELEM,MONTH)) THEN
                 MINVALUE(NELEM,MONTH) = VALUE(J)
                 MINYEAR(NELEM,MONTH) = YEAR
                 IF (RECTYPE.EQ.'DLY') THEN
                    MINDAY(NELEM,MONTH) = J
                 ELSE IF (RECTYPE.NE.'MLY'.AND.RECTYPE.NE.'10D') THEN
                    MINDAY(NELEM,MONTH) = DAY
                 END IF
                 MINFLAG(NELEM,MONTH) = 1
              END IF
              MEANVALUE(NELEM,MONTH) = MEANVALUE(NELEM,MONTH) + VALUE(J)
              VALCOUNT(NELEM,MONTH) = VALCOUNT(NELEM,MONTH) + 1
              SUMSQUARES(NELEM,MONTH) = SUMSQUARES(NELEM,MONTH) +
     +           VALUE(J) ** 2
C               .. RATE OF CHANGE     
              IF (J.GT.1) THEN
                 OLDVAL = VALUE(J-1)
              ELSE IF (CURRTIM.EQ.NXTTIM) THEN
                 OLDVAL = ENDRECVAL(NELEM)
              ELSE 
                 OLDVAL = -99999.   
              ENDIF    
              IF (OLDVAL.NE.-99999.) THEN
                 CHANGE = ABS(VALUE(J) - OLDVAL)
                 IF (CHANGE.GT.MAXCHANGE(NELEM,MONTH)) 
     +                         MAXCHANGE(NELEM,MONTH) = CHANGE
              END IF
           END IF
  390    CONTINUE
C  
C        ** SAVE THE LAST VALUE IN THE CURRENT RECORD TO CALCULATE THE
C           FIRST RATE OF CHANGE FOR THE NEXT RECORD
      IF (RECTYPE.EQ.'DLY') THEN
         CALL IDAYMON(YEAR,MONTH,LSTIDX)
      ELSE
         LSTIDX = NUMVAL
      ENDIF    
      ENDRECVAL(NELEM) = VALUE(LSTIDX)
C      NXTTIM = CURRTIM
C      CALL GTNXTTIM(ITYPE,NXTTIM)
  400 CONTINUE
C
C   END OF DATA - FINISHED
C
  401 CONTINUE
      CLOSE(51)
      CALL LOCATE(24,79,IERR)
      WRITE(*,2010) RECCOUNT
 2010 FORMAT(///,' Processing Complete',/
     +       ,2x,I7,' Records Processed')
      IF (DATAOK) THEN
         STOP ' '
      ELSE
         CALL WRTMSG(5,100,12,1,1,' ',0)
         CALL LOCATE(24,79,IERR)
         STOP 2
      END IF
      END
