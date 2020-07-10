$STORAGE:2
      SUBROUTINE INITSKT(IDKEY)
C
      CHARACTER*(*) IDKEY
C      
$INCLUDE: 'GRFPARM.INC'
$INCLUDE: 'GRAFVAR.INC'
C
      CHARACTER*18 CURRID
      CHARACTER*12 MONNAME(12)
C
      LEGEND = -1            
C      
      LFTSCALE(1,1) = -4.0      
      LFTSCALE(2,1) = 48.0
      LFTSCALE(2,2) = 400.0
      BTSCALE(1)   = -23.0
      BTSCALE(2)   =  32.1
C      
      VPNDLF = 0.
      VPNDRT = 1.
      VPNDBT = 1.
      VPNDTP = 0.
C      
      GANWLF = .0550
      GANWRT = .8950
      GANWBT = .1550
      GANWTP = .9850
C
C       ** SET TEXT PARAMETERS:  COLOR, FONT, SIZE, LOCATION
C
      TLCLR     = 11
      TLFONT    = 3      
      TLSIZE = .03
      TLASP     = 1.
      TLLOC(1)  = .1762
      TLLOC(2)  = .0900
C        
      STLCLR     = 11
      STLFONT    = 3      
      STLSIZE = .03
      STLASP     = 1.
      STLLOC(1)  = .7505
      STLLOC(2)  = .0900
C        
      LEGCLR     = 14
      LEGFONT    = 3      
      LEGSIZE    = .03
      LEGASP     = 1.
      LEGLOC(1)  = .5555
      LEGLOC(2)  = .0400  
C
C       ** SET PALETTE AND BACKGROUND COLOR
C
      BKGNCLR = 0  
      PALETTE = 1
C 
C          ** READ PALETTE DEFINITIONS
C      
      OPEN(51,FILE='O:\DATA\PALETTE.DEF',STATUS='OLD',FORM='FORMATTED'
     +       ,MODE='READ',IOSTAT=ICHK)
      READ(51,*) ((PALDEF(I,J),I=1,16),J=1,12)
      CLOSE(51)
C      
      COLTYPE(1) = 1
      COLTYPE(2) = 1
      COLTHK(1) = 1
      COLTHK(2) = 1
      COL1CLR(1) = 1
      COL1CLR(2) = 14
C         
      CURRID(1:8)  = IDKEY(1:8)
      CURRID(9:18) = IDKEY(12:21)
      CALL GETMON(MONNAME,12)
      CALL SETTITLE(7,CURRID,MONNAME,GRTITLE,GRSUBTITLE)      
C
      RETURN
      END      
