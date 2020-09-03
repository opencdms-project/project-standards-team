$STORAGE:2
C
C     PROGRAM INVENTOR
C
C   PROGRAM TO PRODUCE AN INVENTORY OF A SPECIFIC RANGE OF 15M, HOURLY,     
C       3-HOURLY, DAILY, OR UPPER AIR DATA.    
C       THE PROGRAM ASKS THE USER FOR THE STATION AND DATE RANGE TO 
C       BE CONSIDERED. 
C
C     NOTE:  THE DATA-TYPE CODE (DLY,HLY...) MUST BE INCLUDED ON THE
C            COMMAND LINE CALLING THIS ROUTINE
C            EXAMPLE: "INVENTOR DLY"
C
      INTERFACE TO SUBROUTINE CMDLIN(ADDRES,LENGTH,RESULT)
      INTEGER*4 ADDRES[VALUE],LENGTH[VALUE]
      CHARACTER*1 RESULT
      END
      INTERFACE TO INTEGER*2 FUNCTION SYSTEM [C]
     +        (STRING[REFERENCE])
      CHARACTER*1 STRING
      END

C----------------------------------------------------------------------
      PROGRAM INVENTOR
C
C  PROGRAM CONTROL VARIABLES
C
      PARAMETER (MAXELEM=40,NUMMONTHS=12,MAXDAYS=31,NUMHOURS=24)
C
      CHARACTER*64 FILNAME,RESULT
      CHARACTER*8 STARTSTN,ENDSTN,PREVSTN,PERIOD
      INTEGER*4 STARTYRMO, ENDYRMO,YRMON,IREC,PSP,PSPNCHR,OFFSET
      INTEGER*4 RECCOUNT,NRECCOUNT
      INTEGER*2 ELEM(MAXELEM),ELEMNUM,DDSID,PREVYEAR,PREDDSID   
      CHARACTER*1 DATASOURCE,RTNCODE
      CHARACTER*3 RECTYPE
      CHARACTER*7 OUTCNT(2)
      LOGICAL ELEMREAD(MAXELEM),NTEMP,NWIND
C
C  INPUT DATA VARIABLES
C    
      CHARACTER*8 STNID 
      CHARACTER*1 FLAG1(96)
      REAL*4 VALUE(96),PRESSURE,HEIGHT,TEMP,DEWPTDEP,WNDDIR,WNDSPEED
      CHARACTER*1 FLAGS(6)
      INTEGER*2 IELEM,DAY,MONTH,YEAR,HOUR
C
C  OUTPUT INVENTORY VARIABLES
C
      INTEGER*2 COUNT(MAXELEM,NUMMONTHS,MAXDAYS)
C
C   COMMON BLOCKS
C
      COMMON /INV1/ COUNT,ELEM,NUMELEM,PREDDSID,PREVYEAR
     +              ,ELEMREAD,RECTYPE,PREVSTN
      DATA OUTCNT /'       ','       '/
      OUTCNT(2) = CHAR(0)
C
C   OPEN THE OUTPUT FILE NO MATTER WHAT.  THAT WAY IT WILL NOT HAVE 
C     OLD DATA IN IT IF THE PROGRAM IS NOT RUN.
C
      OPEN(51,FILE='Q:INVEN.DAT',STATUS='UNKNOWN',FORM='FORMATTED')
      ENDFILE 51
      REWIND 51
C
C   DEFINE THE HELP FILE AND SET UP THE SCREEN ETC
C
      FILNAME = 'P:\HELP\INVENTOR.HLP'
      CALL SETMOD(3,IERR)
C
C   FOLLOWING STATEMENT REQUIRED FOR FORTRAN 3.3 - NOT ALLOWED IN FTN 4
c      EXTERNAL INVENTOR
C
C   LOCATE SEGMENTED ADDRESS OF THE BEGINNING OF THIS PROGRAM
C
      OFFSET = #00100000
      PSP = LOCFAR(INVENTOR)
C
C   COMPUTE THE BEGINNING OF THE PROGRAM SEGMENT PREFIX (PSP)
C
      PSP = (PSP - MOD(PSP,#10000)) - OFFSET 
C
C   LOCATE POSITION OF COMMAND PARAMTERS WITHIN THE PSP
C
      PSPNCHR = PSP + #80
      PSP = PSP + #81
C
C   PASS THE ADDRESS OF THE COMMAND PARAMTERS TO CMDLIN WHICH DECODES
C      THE COMMAND AND RETURNS IT AS RESULT.
C
      CALL CMDLIN(PSP,PSPNCHR,RESULT)
C
C   PULL THE COMMAND (REC-TYPE) OUT OF THE RESULT
C
      RECTYPE = RESULT(1:3)
C
      CALL SETMOD(3,IERR)
C
C   READ THE CONTROL DATA TYPE FROM THE COMMAND LINE
C
      IF (RECTYPE.EQ.'DLY') THEN
         NUMLOOP = 31   
      ELSE IF (RECTYPE.EQ.'SYN') THEN
         NUMLOOP = 8    
      ELSE IF (RECTYPE.EQ.'HLY') THEN
         NUMLOOP = 24   
      ELSE IF (RECTYPE.EQ.'15M') THEN
         NUMLOOP = 96   
      ELSE IF (RECTYPE.EQ.'U-A') THEN
         NUMLOOP = 1
      ELSE
         CALL WRTMSG(2,298,12,0,1,'INVENTOR DLY',12)
         STOP 
      END IF   
      IF (RECTYPE.EQ.'DLY') THEN
         DAY = 1
      END IF
C
C   GET THE SELECTION LIMITS
C
   20 CONTINUE
      CALL CLS
      CALL LOCATE(1,0,IERR)
      CALL GETLIMIT(STARTSTN,ENDSTN,STARTYRMO,ENDYRMO,FILNAME,RTNCODE)
      IF (RTNCODE.NE.'0') THEN
         CALL LOCATE(23,0,IERR)
         STOP ' '   
      END IF
C
      CALL DATASRC(41,DATASOURCE,RTNCODE)
      IF (RTNCODE.NE.'0') THEN
         GO TO 20
      END IF
C   
C   OPEN THE INPUT AND OUTPUT FILES
C
      CALL OPENINPUT(RECTYPE,DATASOURCE)
C
C   INITIALIZE
C
      DO 180 I1 = 1,MAXELEM
         ELEMREAD(I1) = .FALSE.
         ELEM(I1) = 9999
         DO 180 J1=1,NUMMONTHS  
            DO 180 K1=1,MAXDAYS   
               COUNT(I1,J1,K1) = 0
  180 CONTINUE
      NRECCOUNT = 0          
      RECCOUNT = 0
      PREVYEAR = 9999
      PREVSTN = 'ZZZZZZZ'
      PREDDSID = 0
      NUMELEM = 0
C
C  WRITE THE RUNNING TOTAL LINE
C
      CALL CLRMSG(1)
      CALL LOCATE(24,0,IERR)
      CALL WRTSTR('Records Read -          Records processed - '
     +             ,44,14,0)

C-----------------------------------------------------------------------
C                   PROCESS THE DATA RECORDS                           |
C-----------------------------------------------------------------------
      DO 500 IREC=1,999999  
$INCLUDE:'READAREC.INC'
         IF (RTNCODE.NE.'0') THEN
            GO TO 501
         END IF
         NRECCOUNT = NRECCOUNT + 1
         CALL LOCATE(24,15,IERR)
         WRITE(OUTCNT,'(I7)') NRECCOUNT
         CALL CWRITE(OUTCNT,12,IERR)
C
C     SKIP RECORDS OUTSIDE THE LIMITS REQUESTED
C
         IF (STNID.GT.ENDSTN.OR.(STNID.EQ.ENDSTN.AND.YRMON.GT.ENDYRMO))
     +         THEN   
             GO TO 501
         END IF
         IF (STNID.LT.STARTSTN.OR.YRMON.LT.STARTYRMO.OR.
     +        YRMON.GT.ENDYRMO)
     +        THEN
            GO TO 500
         END IF
C
C     AT A CHANGE OF YEAR OR A CHANGE OF STATION WRITE OUT THE INVENTORY
C      INFORMATION BEING HELD
C
         IF (STNID.NE.PREVSTN.OR.YEAR.NE.PREVYEAR) THEN
            IF (PREVYEAR.NE.9999) THEN
               CALL INVWRITE
            END IF
            PREVSTN = STNID
            PREVYEAR = YEAR
            PREDDSID = DDSID 
         END IF
C
C     IF NOT U-A DETERMINE THE RELATIVE NUMBER OF THE CURRENT ELEMENT
C
         IF (RECTYPE.NE.'U-A') THEN
            IF (NUMELEM.EQ.0) THEN
               NUMELEM = 1
               ELEMNUM = 1
               ELEM(NUMELEM) = IELEM
            ELSE
               DO 190 I1 = 1,NUMELEM
                  ELEMNUM = I1
                  IF (IELEM.EQ.ELEM(I1)) THEN
                     GO TO 195
                  END IF
  190          CONTINUE
               NUMELEM = NUMELEM + 1
               ELEMNUM = NUMELEM
               ELEM(NUMELEM) = IELEM
  195          CONTINUE
            END IF
            ELEMREAD(ELEMNUM) = .TRUE.
         END IF  
C
C      ADD THE VALUES IN THE CURRENT RECORD TO THE INVENTORY COUNTER
C
         RECCOUNT = RECCOUNT + 1
         CALL LOCATE(24,44,IERR)
         WRITE(OUTCNT,'(I7)') RECCOUNT
         CALL CWRITE(OUTCNT,12,IERR)
         IF (RECTYPE.EQ.'U-A') THEN
            IF (LVLNUM.EQ.1) THEN
               NTEMP = .FALSE.
               NWIND = .FALSE.
            ELSE IF (LVLNUM.EQ.NUMLVL) THEN
               IF (NTEMP) THEN
                  COUNT(1,MONTH,1) = COUNT(1,MONTH,1) + 1
               END IF
               IF (NWIND) THEN
                  COUNT(2,MONTH,1) = COUNT(2,MONTH,1) + 1
               END IF
            END IF   
            IF (FLAGS(3).NE.'M') THEN
               NTEMP = .TRUE.
            END IF
            IF (FLAGS(5).NE.'M') THEN
               NWIND = .TRUE.
            END IF
         ELSE
            DO 200 I1 = 1,NUMLOOP
               IF (VALUE(I1).NE.-99999) THEN
                  COUNT(ELEMNUM,MONTH,DAY)=COUNT(ELEMNUM,MONTH,DAY) + 1 
               END IF
  200       CONTINUE
         END IF
C
  500 CONTINUE         
  501 CONTINUE
      IF (RECCOUNT.GT.0) THEN
         CALL INVWRITE
      END IF
C-----------------------------------------------------------------------
 2000 CONTINUE
      CLOSE(51)
      WRITE(*,2010) RECCOUNT,NRECCOUNT-RECCOUNT
 2010 FORMAT(/,'  PROCESSING COMPLETE',/
     +        ,2X,I7,' RECORDS PROCESSED',/
     +        ,2X,I7,' RECORDS SKIPPED',/)
      STOP ' '
      END

C======================================================================

      SUBROUTINE INVWRITE
C
C   THIS INTERNAL SUBROUTINE WRITES THE INVENTORY INFORMATION ALREADY
C     COMPUTED AND RE-INITIALIZES ALL INVENTORY COUNTERS.
C
      PARAMETER (MAXELEM=40,NUMMONTHS=12,MAXDAYS=31,NUMHOURS=24)
C
      CHARACTER*8 PREVSTN
      INTEGER*2 COUNT(MAXELEM,NUMMONTHS,MAXDAYS)
     +         ,ELEM(MAXELEM),PREDDSID,PREVYEAR
      CHARACTER*3 RECTYPE
      LOGICAL ELEMREAD(MAXELEM)

      COMMON /INV1/ COUNT,ELEM,NUMELEM,PREDDSID,PREVYEAR
     +             ,ELEMREAD,RECTYPE,PREVSTN
C
      CHARACTER*1 INVEN(NUMMONTHS,32)
      INTEGER*2 NUMDAYS(NUMMONTHS)
C
      NUMDAYS(1) = 31
      IF (MOD(PREVYEAR,4).EQ.0.AND.PREVYEAR.NE.1900) THEN
         NUMDAYS(2) = 29
      ELSE
         NUMDAYS(2) = 28
      END IF
      NUMDAYS(3) = 31
      NUMDAYS(4) = 30
      NUMDAYS(5) = 31
      NUMDAYS(6) = 30
      NUMDAYS(7) = 31
      NUMDAYS(8) = 31
      NUMDAYS(9) = 30
      NUMDAYS(10) = 31
      NUMDAYS(11) = 30
      NUMDAYS(12) = 31
C
C   SET THE INVENTORY INFORMATION FOR UPPER AIR DATA
C
      IF (RECTYPE.EQ.'U-A') THEN
         DO 30 J1 = 1,NUMMONTHS
            DO 30 K1 = 1,4
               INVEN(J1,K1) = ' '
   30    CONTINUE
         DO 50 J1 = 1,NUMMONTHS
            DO 50 K1 = 1,2
               IF (COUNT(K1,J1,1).GT.0) THEN
                  I3 = COUNT(K1,J1,1)/10
                  I4 = MOD(COUNT(K1,J1,1),10)
                  I1 = (K1-1) * 2
                  WRITE(INVEN(J1,I1+1),'(I1)') I3
                  WRITE(INVEN(J1,I1+2),'(I1)') I4
                  COUNT(K1,J1,1) = 0
               END IF
  50     CONTINUE
         WRITE(51,80)PREDDSID,PREVSTN,PREVYEAR,
     +           ((INVEN(J1,K1),K1=1,4),J1=1,NUMMONTHS)
  80     FORMAT(I3.3,','A8,',',I4.4,24(',',2A1))
C
C   OR SET THE INVENTORY INFORMATION FOR THE OTHER DATA SETS
C
      ELSE
         DO 500 I1 = 1,NUMELEM
            IF (ELEMREAD(I1)) THEN
               ELEMREAD(I1) = .FALSE.
C
C          SET THE OUTPUT HOLDER TO ALL SPACES
C
               DO 150 J1 = 1,NUMMONTHS
                  DO 150 K1 = 1,32
                     INVEN(J1,K1) = ' '
  150          CONTINUE
C
C          CONVERT THE INVENTORY COUNTER TO THE OUTPUT CODES
C
               DO 200 J1 = 1,NUMMONTHS
                  IF (RECTYPE.EQ.'DLY') THEN
                     IF (COUNT(I1,J1,1).GT.0) THEN
                        I3 = COUNT(I1,J1,1)/10
                        I4 = MOD(COUNT(I1,J1,1),10)
                        WRITE(INVEN(J1,1),'(I1)') I3
                        WRITE(INVEN(J1,2),'(I1)') I4
                        COUNT(I1,J1,1) = 0
                     END IF
                  ELSE
C                  (HOURLY, SYNOPTIC OR 15 MINUTE DATA)
                     DO 180 K1 = 1,NUMDAYS(J1)
                        IF (COUNT(I1,J1,K1).LE.3) THEN
                           ICOUNT = COUNT(I1,J1,K1)
                        ELSE IF (COUNT(I1,J1,K1).GE.24) THEN
                           ICOUNT = 9
                        ELSE IF (COUNT(I1,J1,K1).GE.3.AND.
     +                        COUNT(I1,J1,K1).LT.6) THEN
                           ICOUNT = 3
                        ELSE IF (COUNT(I1,J1,K1).GE.6.AND.
     +                        COUNT(I1,J1,K1).LT.8) THEN
                           ICOUNT = 4
                        ELSE IF (COUNT(I1,J1,K1).GE.8.AND.
     +                        COUNT(I1,J1,K1).LT.12) THEN
                           ICOUNT = 5
                        ELSE IF (COUNT(I1,J1,K1).GE.12.AND.
     +                        COUNT(I1,J1,K1).LT.16) THEN
                           ICOUNT = 6
                        ELSE IF (COUNT(I1,J1,K1).GE.16.AND.
     +                        COUNT(I1,J1,K1).LT.20) THEN
                           ICOUNT = 7
                        ELSE IF (COUNT(I1,J1,K1).GE.20) THEN
                           ICOUNT = 8
                        END IF
                        WRITE(INVEN(J1,K1),'(I1)') ICOUNT
                        COUNT(I1,J1,K1) = 0
C
C                         ** SET THE APPROPRIATE ZERO VALUES TO SPACES
C
                        IF (K1.LT.30.AND.MOD(K1,2).EQ.0) THEN
C                        
C                            .. K1 POINTS TO SECOND CHARACTER IN A TWO
C                               CHARACTER FIELD.  SET CONSECUTIVE ZEROS TO
C                               BLANK STARTING WITH THE LEFTMOST CHARACTER.
                           IF (INVEN(J1,K1-1).EQ.'0') THEN
                              INVEN(J1,K1-1) = ' '
                              IF (INVEN(J1,K1).EQ.'0') THEN
                                 INVEN(J1,K1) = ' '
                              END IF
                           END IF
                        END IF   
  180                CONTINUE
C                        
C                            .. LAST FIELD CONTAINS THREE CHARACTERS.  SET 
C                               CONSECUTIVE ZEROS TO BLANK STARTING WITH 
C                               THE LEFTMOST CHARACTER.
                     IF (INVEN(J1,29).EQ.'0') THEN
                        INVEN(J1,29) = ' '
                        IF (INVEN(J1,30).EQ.'0') THEN
                           INVEN(J1,30) = ' '
                           IF (INVEN(J1,31).EQ.'0') THEN
                              INVEN(J1,31) = ' '
                           ENDIF 
                        ENDIF 
                     ENDIF 
C                     IF (INVEN(J1,31).EQ.'0'.OR.INVEN(J1,31).EQ.' ')THEN
C                        IF (INVEN(J1,29).EQ.'0' .AND.
C     +                     INVEN(J1,30).EQ.'0') THEN
C                            .. DAYS/MONTH = 31  29-30-31 = 0-0-0
C                               DAYS/MONTH = 30  29-30-31 = 0-0-BL
C                           INVEN(J1,29) = ' '
C                           INVEN(J1,30) = ' '
C                           INVEN(J1,31) = ' '
C                        ELSE IF (INVEN(J1,29).EQ.'0') THEN
C                            .. DAYS/MONTH = 31  29-30-31 = 0-(>0)-0
C                               DAYS/MONTH = 30  29-30-31 = 0-(>0)-BL
C                               DAYS/MONTH = 29  29-30-31 = 0-BL-BL
C                           INVEN(J1,29) = ' '
C                        END IF
C                     END IF    
                  END IF
  200          CONTINUE      
C
C           WRITE THE CODED INVENTORY INFO TO FILE 51 IN DATAEASE IMPORT
C              FORMAT
C
               IF (RECTYPE.EQ.'DLY') THEN
                  WRITE(51,250) PREDDSID,PREVSTN,ELEM(I1),PREVYEAR,
     +              ((INVEN(J1,K1),K1=1,2),J1=1,NUMMONTHS)
  250             FORMAT(I3.3,',',A8,',',I3.3,',,',I4.4,12(',',2A1))
               ELSE
                  WRITE(51,260) PREDDSID,PREVSTN,ELEM(I1),PREVYEAR,
     +              ((INVEN(J1,K1),K1=1,32),J1=1,NUMMONTHS)
  260             FORMAT(I3.3,',',A8,',',I3.3,',,',I4.4
     +                  ,12(',',14(2A1,','),4A1))
               END IF
            END IF
  500    CONTINUE
C
      END IF 
      RETURN
      END       
