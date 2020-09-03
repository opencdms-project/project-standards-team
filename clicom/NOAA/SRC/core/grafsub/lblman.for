$STORAGE:2
      SUBROUTINE LBLMAN(I1NODAT)
C------------------------------------------------------------------------------
C     UPPER LEVEL PROGRAM THAT MANAGES THE ROUTINES THAT REVISE THE LABELS OF A
C     GRAPH DEPENDING UPON WHETHER IT IS TIME SERIES, MAP, SKEW-T, OR WINDROSE.
C
C     INPUT ARGUMENT:
C
C     I1NODAT    INT1 ARRAY SWITCH TO PREVENT USE OF DEFAULT VALUES (-9999) AS
C                           A VALID LOCATION FOR THE LEFT/RIGHT AXIS TEXT 
C                           LABELS.  (TIME SERIES ONLY)
C------------------------------------------------------------------------------
$INCLUDE: 'GRFPARM.INC'
$INCLUDE: 'GRAFVAR.INC'
$INCLUDE: 'CURRPLT.INC'
C
      INTEGER*1    I1NODAT(*)
      INTEGER*2    MENU, MENUAX, LIMCHK, HELPLVL
      REAL*4       MARKLOC(2)
      CHARACTER*2  INCHAR   
      CHARACTER*28 TTLTXT
      INTEGER*1 I1OFF
      DATA I1OFF/0/, MARKLOC/1.0,1.0/
C
      XWIN=.1
      YWIN=.8
      IF   (IGRAPH .EQ. 1) THEN
           MENU = 18
      ELSE IF (IGRAPH .EQ. 2) THEN
           MENU = 19
      ELSE IF (IGRAPH .EQ. 3) THEN
           MENU = 17
      ELSE
           MENU = 44
      ENDIF
      HELPLVL=MENU
C      
C---- DISPLAY THE LABEL MANAGER MENU BASED UPON GRAPH TYPE
C
  100 CALL GRAFMNU(1,MENU,XWIN,YWIN,HELPLVL,INCHAR)
      LIMCHK = 1
      IF     (INCHAR .EQ. '1 ') THEN
             IF (GRTITLE.EQ.' ') THEN
                TTLTXT = DATATITLE
             ELSE
                TTLTXT = GRTITLE
             ENDIF      
             IF (TTLTXT.EQ.' ') THEN
                CALL GRAFNOTE(.1,.95,542,202,' ',0,INCHAR)
             ELSE
                CALL CHNGLBL(20,TTLTXT,TLCLR,TLFONT,TLSIZE,TLASP,0,
     +                       TLLOC,PALETTE,PALDEF,LIMCHK) 
             ENDIF
      ELSEIF (INCHAR .EQ. '2 ') THEN
             IF (GRSUBTITLE.EQ.' ') THEN
                TTLTXT = DATASUB
             ELSE
                TTLTXT = GRSUBTITLE
             ENDIF      
             IF (TTLTXT.EQ.' ') THEN
                CALL GRAFNOTE(.1,.95,542,202,' ',0,INCHAR)
             ELSE
                CALL CHNGLBL(21,TTLTXT,STLCLR,STLFONT,STLSIZE,STLASP,0,
     +                       STLLOC,PALETTE,PALDEF,LIMCHK) 
             ENDIF 
      ELSEIF (INCHAR .EQ. '3 ') THEN
             IF (LEGEND.EQ.0) THEN
C---            NO LEGEND DEFINED. RETURN TO MENU   ---
                CALL GRAFNOTE(.1,.95,543,202,' ',0,INCHAR)
             ELSE
                CALL CHNGLGND(22,LEGEND,LEGCLR,LEGFONT,LEGSIZE,LEGASP,
     +          LEGLOC,PALETTE,PALDEF) 
             ENDIF
      ELSEIF (INCHAR .EQ. '4 ') THEN
             IF (FTXT.EQ.' ') THEN
C---            NO FREE TEXT DEFINED. RETURN TO MENU   ---
                CALL GRAFNOTE(.1,.95,542,202,' ',0,INCHAR)
             ELSE
                LIMCHK = 0
                CALL CHNGLBL(23,FTXT,FTXTCLR,FTXTFONT,FTXTSIZE,FTXTASP,
     +          0,FTXTLOC,PALETTE,PALDEF,LIMCHK) 
             ENDIF      
      ELSEIF (INCHAR .EQ. '5 ') THEN
             IF (IGRAPH .EQ. 1) THEN
                TTLTXT=' '
                DO 110 NC=NCA,NCB
                   IF (COLAXIS(NC).EQ.0 .AND. I1NODAT(NC).EQ.I1OFF) THEN
                      IF (LFTTXT(IDPLT).EQ.' ') THEN
                         TTLTXT=COLHDR(NC)
                      ELSE
                         TTLTXT=LFTTXT(IDPLT)
                      ENDIF
                      GO TO 111
                   ENDIF
  110           CONTINUE            
  111           CONTINUE            
                IF (TTLTXT.EQ.' ') THEN
C---               NO TIME SERIES LEFT AXIS TEXT DEFINED. RETURN TO MENU --
                   CALL GRAFNOTE(.1,.95,542,202,' ',0,INCHAR)
                ELSE
                   LIMCHK = 2
                   CALL CHNGLBL(24,TTLTXT,LTXTCLR,LTXTFONT,
     +             LTXTSIZE,LTXTASP,1,LTXTLOC,PALETTE,PALDEF,LIMCHK) 
                ENDIF      
             ELSE
                IF (IGRAPH .EQ. 2) THEN
                   IF (LFTTXT(IDPLT).EQ.' ') THEN
C------               NO MAP LEFT AXIS TEXT DEFINED. RETURN TO MENU   ---
                      CALL GRAFNOTE(.1,.95,542,202,' ',0,INCHAR)
                   ELSE
                      LIMCHK = 2
                      CALL CHNGLBL(24,LFTTXT(IDPLT),LTXTCLR,LTXTFONT,
     +                LTXTSIZE,LTXTASP,1,LTXTLOC,PALETTE,PALDEF,LIMCHK) 
                   ENDIF      
                ELSE
C------            PROCESS WINDROSE LABELS ON OCCURRANCE RINGS
                   LIMCHK = NDECLF(5)
                   CALL CHNGRING(LIMCHK)
                   NDECLF(5) = LIMCHK
                ENDIF
             ENDIF
      ELSEIF (INCHAR .EQ. '6 ') THEN
             IF (IGRAPH .EQ. 1) THEN
                TTLTXT=' '
                DO 120 NC=NCA,NCB
                   IF (COLAXIS(NC).EQ.1 .AND. I1NODAT(NC).EQ.I1OFF) THEN
                      IF (RTTXT(IDPLT).EQ.' ') THEN
                         TTLTXT=COLHDR(NC)
                      ELSE
                         TTLTXT=RTTXT(IDPLT)
                      ENDIF
                      GO TO 121
                   ENDIF
  120           CONTINUE            
  121           CONTINUE       
                IF (TTLTXT.EQ.' ') THEN
C---               NO TIME SERIES RIGHT AXIS TEXT DEFINED. RETURN TO MENU --
                   CALL GRAFNOTE(.1,.95,542,202,' ',0,INCHAR)
                ELSE
                   LIMCHK = 2
                   CALL CHNGLBL(25,TTLTXT,RTXTCLR,RTXTFONT,
     +             RTXTSIZE,RTXTASP,1,RTXTLOC,PALETTE,PALDEF,LIMCHK) 
                ENDIF      
             ELSE 
                IF (BOTTXT.EQ.' ') THEN
C---               NO MAP BOTTOM AXIS TEXT DEFINED. RETURN TO MENU   ---
                   CALL GRAFNOTE(.1,.95,542,202,' ',0,INCHAR)
                ELSE
                   LIMCHK = 1
                   CALL CHNGLBL(26,BOTTXT,BTXTCLR,BTXTFONT,BTXTSIZE,
     +             BTXTASP,0,BTXTLOC,PALETTE,PALDEF,LIMCHK) 
                ENDIF      
             ENDIF
      ELSEIF (INCHAR .EQ. '7 ') THEN
             IF (IGRAPH .EQ. 1) THEN
                IF (BOTTXT.EQ.' ') THEN
C---               NO TIME SERIES BOTTOM AXIS TEXT DEFINED. RETURN TO MENU --
                   CALL GRAFNOTE(.1,.95,542,202,' ',0,INCHAR)
                ELSE
                   LIMCHK = 1
                   CALL CHNGLBL(26,BOTTXT,BTXTCLR,BTXTFONT,BTXTSIZE,
     +             BTXTASP,0,BTXTLOC,PALETTE,PALDEF,LIMCHK) 
                ENDIF      
             ELSE
C------         PROCESS MAP CONTOUR LABEL OPTIONS HERE
                CALL CHNGTXAX(30,AXSCLR,RTXTFONT,RTXTSIZE,RTXTASP,
     +          NDECRT(1),NDM2,NCHRBT)
             ENDIF
      ELSEIF (INCHAR .EQ. '8 ') THEN
C---         PROCESS TIME SERIES & MAP TEXT AXIS OPTIONS 
             IF (IGRAPH .EQ. 1) THEN
                MENUAX = 27
             ELSE
                MENUAX = 28
             ENDIF
             CALL CHNGTXAX(MENUAX,AXSCLR,AXSFONT,ATXTSIZE,ATXTASP,
     +       NDECLF(IDPLT),NDECRT(IDPLT),NCHRBT)
      ELSEIF (INCHAR .EQ. '9 ') THEN
C-----       PROCESS MAP STN MARKER OPTIONS 
             LIMCHK = - (COLAXIS(IDPLT) + 1)
             CALL CHNGLBL(29,TTLTXT,COL2CLR(IDPLT),NDECRT(3),
     +       YMAJRT(1),YMAJRT(2),0,MARKLOC,PALETTE,PALDEF,LIMCHK)
             IF (LIMCHK .GE. 0) THEN
                COLAXIS(IDPLT) = LIMCHK
             ENDIF
             TTLTXT=' '
      ELSEIF (INCHAR .EQ. '4F' .OR. INCHAR .EQ. 'ES') THEN
             RETURN
      ENDIF
      GO TO 100
      END      
