$STORAGE:2
C     PROGRAM MRGTWF
C
C****************************************************************************
C
C       **  DEFINE THE INTERFACE TO THE C ROUTINE "SYSTEM"
C
      INTERFACE TO INTEGER*2 FUNCTION SYSTEM [C]
     +        (STRING[REFERENCE])
      CHARACTER*1 STRING
      END
C      
C****************************************************************************
      PROGRAM MRGTWF
C      
      CHARACTER*1 RTNCODE
      CHARACTER*64 FILNAME
      LOGICAL STOPFLG
C      
      FILNAME = 'P:\HELP\MRGTWF.HLP'
C
C       ** GET ACTION FROM MENU:  
C               0=EXIT   1=LIST TWF RECORDS   2=MERGE TWF RECORDS
C
      STOPFLG=.FALSE.
   10 CONTINUE
      CALL CLS
      CALL LOCATE(5,10,IERR)
      CALL GETMNU('LST/MRG TWF ',FILNAME,ICHOICE)
      IF (ICHOICE.EQ.0) THEN
         STOPFLG = .TRUE.
      ELSE IF (ICHOICE.EQ.1) THEN   
         WRITE(*,*) 'LIST TWF RECORDS -- NOT WORKING'
      ELSE
         CALL READMRG(RTNCODE)
         STOPFLG = RTNCODE.NE.'0'
      ENDIF
      IF (.NOT.STOPFLG) GO TO 10
C
   50 CONTINUE   
      STOP ' '
      END         
         
      SUBROUTINE READMRG(RTNCODE)         
C      
$INCLUDE: 'VAL1.INC'
$INCLUDE: 'INDEX.INC'
      CHARACTER*1 RTNCODE
      CHARACTER*8 INSTN
      CHARACTER*13 INKEY
      CHARACTER*21 IDKEY
C
      CHARACTER*78 MSGTXT
      CHARACTER*15 RECPRT(3)
      CHARACTER*20 IDXSRC,TWFSRC
      CHARACTER*8  INOUTFIL(3)
      CHARACTER*6  OUTCNT
      CHARACTER*2 RTNFLAG,HOURLBL(24)
      CHARACTER*3 RECTYPE
      INTEGER*2 DSETID
      INTEGER*2 SYSTEM
      LOGICAL FRMFLG
C
      PARAMETER (NTYPD=7)
      CHARACTER*3  TBLRECTYP(NTYPD)
      DATA TBLRECTYP/'MLY','10D','DLY','SYN','HLY','15M','U-A'/
C
C
   10 CONTINUE   
      RTNCODE = '0'      
      CALL CLS
      CALL LOCATE(0,0,IERR)
      CALL GETFRM('MRGTWFIO','  ',INOUTFIL,8,RTNFLAG)
      IF (RTNFLAG.EQ.'4F') GO TO 250
      FRMFLG=.FALSE.
C
C       ** OPEN THE SETUP FILE 
C
      RECTYPE = INOUTFIL(2)(1:3)
      READ(INOUTFIL(3),'(I3)') DSETID
      DO 20 I=1,NTYPD
         ITYPE = I
         IF (RECTYPE.EQ.TBLRECTYP(I)) GO TO 21
   20 CONTINUE   
      CALL WRTMSG(5,175,12,1,1,RECTYPE,3)
      FRMFLG = .TRUE.
   21 CONTINUE   
      CALL GETSET(DSETID,ITYPE,RECTYPE,HOURLBL,RTNCODE)
      IF (RTNCODE.NE.'0') THEN
         FRMFLG = .TRUE.
      END IF
      IF (FRMFLG) GO TO 10
C
C       ** OPEN THE SOURCE .IDX AND .TWF FILES      
C
      LGTH = LNG(INOUTFIL(1))
      IDXSRC = 'P:\DATA\'
      IDXSRC(9:) = INOUTFIL(1)(1:LGTH)//'.IDX'
      OPEN (69,FILE=IDXSRC,STATUS='OLD',ACCESS='DIRECT',
     +         MODE='READ',
     +         FORM='UNFORMATTED',RECL=25,IOSTAT=IOCHK)
      IF (IOCHK.NE.0) THEN
         WRITE(MSGTXT,'(A20,I5)') IDXSRC,IOCHK
         CALL WRTMSG(4,157,12,1,0,MSGTXT,25)
         RTNCODE = '1'
         GO TO 210            
      END IF   
C
      TWFSRC = 'P:\DATA\'
      TWFSRC(9:) = INOUTFIL(1)(1:LGTH)// '.TWF'
      OPEN (70,FILE=TWFSRC,STATUS='OLD',ACCESS='DIRECT',
     +         MODE='READ',
     +         FORM='UNFORMATTED',RECL=RLNGTH,IOSTAT=IOCHK)
      IF (IOCHK.NE.0) THEN
         WRITE(MSGTXT,'(A20,I5)') TWFSRC,IOCHK
         CALL WRTMSG(4,157,12,1,0,MSGTXT,25)
         RTNCODE = '1'
         GO TO 210            
      END IF  
C
C       ** OPEN THE OUTPUT .IDX AND .TWF FILES
C
      IDXNAM = 'P:\DATA\'
      WRITE(IDXNAM(9:),'(A3,I3.3,A4)') RECTYPE,DSETID,'.IDX'
      FILNAM = IDXNAM(1:14)// '.TWF'
      CALL OPENFILES(2)
C      
      OUTNAM = IDXNAM(1:14)//'.BDX'
      WRITE(MSGTXT,500)IDXNAM,OUTNAM
  600 FORMAT('COPY ',A,1X,A)
      LGTH = LNG(MSGTXT)+1
      MSGTXT(LGTH:LGTH) = CHAR(0)    
      I = SYSTEM(MSGTXT)
C      
      OUTNAM = FILNAM(1:14)//'.BWF'
      MSGTXT = ' '      
      WRITE(MSGTXT,500)IDXNAM,OUTNAM
      LGTH = LNG(MSGTXT)+1
      MSGTXT(LGTH:LGTH) = CHAR(0)    
      I = SYSTEM(MSGTXT)
      MSGTXT = ' '      
C      
C       ** WRITE THE MESSAGE LINE FOR THE NUMBER OF RECORDS THAT ARE
C          READ, WRITTEN, AND DUPLICATE
C 
      MSGTXT = ' '
      CALL GETMSG(430,MSGTXT)
      CALL GETMSG(999,MSGTXT)
      CALL PARSE1(MSGTXT,78,3,15,RECPRT,RTNCODE)
      MSGTXT = ' '
      MSGTXT(2:)=RECPRT(1)
      NCPRT1=LNG(MSGTXT)+1
      MSGTXT(NCPRT1+10:)=RECPRT(2)
      NCPRT2=LNG(MSGTXT)+1
      MSGTXT(NCPRT2+10:)=RECPRT(3)
      NCPRT3=LNG(MSGTXT)+1
      CALL CLRMSG(1)
      CALL LOCATE(24,0,IERR)
      CALL WRTSTR(MSGTXT,NCPRT3,14,0)
C
C       ** READ THE FIRST RECORD OF THE SOURCE .IDX FILE
C
      READ(69,REC=1) DELKEY,BGNIDX,NUMIDX
      IF (NUMIDX.EQ.1) THEN
         CALL WRTMSG(10,300,12,1,1,' ',0)
         RTNCODE = '1'
         GO TO 210            
      END IF
C
C       ** MAIN LOOP - READ THE SOURCE .TWF AND MERGE WITH THE OUTPUT .TWF  
C
      KNTRD  = 0
      KNTDUP = 0
      KNTWT  = 0
      DO 200 I = BGNIDX,NUMIDX
         READ(69,REC=I) DELKEY,INSTN,INKEY,RECNUM
         IF (DELKEY.LT.2.) THEN
             GO TO 200
         END IF
         KNTRD = KNTRD+1
         READ(70,REC=RECNUM) IDKEY,((VALARRAY(I1,J1)
     +      ,I1=1,NUMELEM),J1=1,NUMLINE),(((FLAGARRAY(K,L,M)
     +      ,M=1,2),K=1,NUMELEM),L=1,NUMLINE)
         CALL MRGREC(IDKEY,KNTWT,KNTDUP,RTNCODE)
         IF (RTNCODE.NE.'0') GO TO 210
C
         OUTCNT(6:6) = CHAR(0)
         WRITE(OUTCNT(1:5),500) KNTRD
  500    FORMAT(I4,1X)       
         CALL LOCATE(24,NCPRT1,IERR)         
         CALL CWRITE(OUTCNT,12,IERR)
         WRITE(OUTCNT(1:5),500) KNTWT
         CALL LOCATE(24,NCPRT2,IERR)         
         CALL CWRITE(OUTCNT,12,IERR)
         WRITE(OUTCNT(1:5),500) KNTDUP
         CALL LOCATE(24,NCPRT3,IERR)         
         CALL CWRITE(OUTCNT,12,IERR)
C
  200 CONTINUE
  210 CONTINUE
C
C       ** CLOSE FILES
C
      CLOSE(69)
      CLOSE(70)
      CLOSE(19)
      CLOSE(20)
C      
  250 CONTINUE
      RETURN
      END
***********************************************************************
      SUBROUTINE MRGREC(IDKEY,KNTWT,KNTDUP,RTNCODE)
C
      CHARACTER*21 IDKEY,CHOICE(2)
      CHARACTER*1 RTNCODE
      CHARACTER*2 RTNFLAG
      LOGICAL WTDATA
C
      IRFRM = 14
      IEROW = 24 - IRFRM + 1      
C
      CALL BINDATA(IDKEY,RTNCODE)
C
C          ** IF RTNCODE = 2 THEN ID NOT FOUND SO IT'S NEW (OK).
C             IF RTNCODE = 0 THEN THE PROGRAM HAS ENCOUNTERED A DUPLICATE ID
C             ASK THE USER TO CONTINUE OR QUIT.  ANY OTHER RTNCODE INDICATES
C             AN ERROR.
C
      WTDATA = .TRUE.
      IF (RTNCODE .NE.'2') THEN
         IF (RTNCODE .EQ. '0') THEN
            KNTDUP = KNTDUP + 1
            CHOICE(1) = IDKEY
            CALL LOCATE(IRFRM,0,IERR)
            CALL GETFRM('MRGDUPID ',' ',CHOICE,1,RTNFLAG)
            DO 10 IROW=2,IEROW
               CALL CLRMSG(IROW)
   10       CONTINUE             
            READ(CHOICE(2),'(I1)') ICHOICE           
C             .. 1=OVERWRITE   2=SKIP RECORD   3=STOP
            IF (ICHOICE.EQ.2) THEN
               WTDATA = .FALSE.
            ELSE IF (ICHOICE.EQ.3) THEN
               WTDATA = .FALSE.
               RTNCODE = '1'
            ENDIF                        
         ELSE
            CALL WRTMSG(5,51,12,1,1,RTNCODE,1)
            WTDATA = .FALSE.
         END IF
      ELSE
         RTNCODE='0'   
      END IF
      IF (WTDATA) THEN
         CALL PUTDATA(IDKEY,2,RTNCODE)
         KNTWT = KNTWT + 1
      ENDIF   
C      
      RETURN
      END