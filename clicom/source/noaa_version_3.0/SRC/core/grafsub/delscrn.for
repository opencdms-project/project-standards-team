$STORAGE:2
      SUBROUTINE DELSCRN (GRAFSCRN)
C----------------------------------------------------------------------------
C     ROUTINE TO DELETE A GRAPH SCREEN DEFINITION FILE AND REMOVE ITS
C     ENTRY FROM THE SCREEN DEFINITION INDEX FILE.
C
C     INPUT ARGUMENTS:
C     GRAFSCRN  CHAR  NAME OF THE GRAPHICS SCREEN TO BE DELETED
C
C     OUTPUT ARGUMENTS:  NONE
C----------------------------------------------------------------------------
      CHARACTER*75 INREC
      CHARACTER*32 SCRNFILE
      CHARACTER*8  GRAFSCRN
C      
C  BUILD SCR FILE NAME,  OPEN IT, AND DELETE IT
C
      SCRNFILE = 'O:\DATA\'//GRAFSCRN
      DO 50 I1 = 20,1,-1
         IF (SCRNFILE(I1:I1).NE.' ') THEN
            GO TO 60
         ENDIF
50    CONTINUE
60    CONTINUE
      I1 = I1 + 1
      SCRNFILE(I1:I1+3) = '.SCR'         
      OPEN (51,FILE=SCRNFILE,STATUS='OLD',FORM='FORMATTED'
     +     ,MODE='WRITE',IOSTAT=ICHK)
      IF (ICHK.NE.6416.AND.ICHK.NE.0) THEN
         CALL OPENMSG(SCRNFILE,'DELSCRN     ',ICHK)
      ENDIF
      CLOSE(51,STATUS='DELETE')
C
C   FIND AND REMOVE THE ENTRY FROM THE GRAFSCRN INDEX FILE
C      
      OPEN (51,FILE='O:\DATA\GRAFSCRN.IDX',STATUS='OLD',FORM='BINARY'
     +     ,ACCESS='DIRECT',RECL=75)
      DO 100 I = 1,9999
         READ(51,REC=I,ERR=110) INREC
         IF (INREC(1:8).EQ.GRAFSCRN) THEN
            INREC(1:8)   = '********'              
            INREC(10:41) = 'THIS RECORD DELETED'
            INREC(42:75) = '  '
            WRITE(51,REC=I) INREC
            GO TO 110
         ENDIF
100   CONTINUE
110   CONTINUE
      CLOSE(51)
      RETURN
      END
      