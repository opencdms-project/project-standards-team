C
C
C
C
C
      SUBROUTINE GTRNZ0 (CRDIN,INDEF,TPARIN,CRDIO,IODEF,TPARIO,
     .                   IPFILE,IFLG,ICHK)   
*
      IMPLICIT REAL (A-H,O-Z)
      INTEGER*4 SYSUNT(26),INDEF(3),IODEF(3)
      DIMENSION CRDIN(1),CRDIO(1),TPARIN(1),TPARIO(1),COORD(2)
      COMMON /MAP/ MAPPRJ
      DATA SYSUNT / 0 , 25*2 /
      DATA MAXUNT,MAXSYS / 5 , 25 / 
      DATA ZERO /0.0/  
*
C ******************************************************************
C
C            *****  TRANSFORM POINTS *****
C
c
           ipfile = 6
c
c
C
      IF (ICHK.GT.0) THEN
          CALL  GTRNZ1 (CRDIN,INDEF,TPARIN,CRDIO,IODEF,TPARIO,
     .                   IPFILE,IFLG,ICHK)   
          RETURN
      ENDIF
C ****************************************************************
c ****************************************************************
C
C
      IFLG = 0
c
* CHECK VALIDITY OF CODES FOR UNITS OF MEASURE AND REFERENCE SYSTEMS
c
      IF (INDEF(1).GE.0 .AND. INDEF(1).LE.MAXSYS) GO TO 020  
CCCCC      IF (IPFILE .NE. 0) WRITE (IPFILE,2000) INDEF(1)   
CCCKW  2000 FORMAT (' ILLEGAL SOURCE REFERENCE SYSTEM CODE = ',I6)   
      IFLG = 1   
      RETURN   
  020 IF (IODEF(1).GE.0 .AND. IODEF(1).LE.MAXSYS) GO TO 040
CCCCC      IF (IPFILE .NE. 0) WRITE (IPFILE,2010) IODEF(1) 
CCCKW  2010 FORMAT (' ILLEGAL TARGET REFERENCE SYSTEM CODE = ',I6) 
      IFLG = 2   
      RETURN   
  040 IF (INDEF(3).GE.0 .AND. INDEF(3).LE.MAXUNT) GO TO 060  
CCCCC      IF (IPFILE .NE. 0) WRITE (IPFILE,2020) INDEF(3)  
CCCKW  2020 FORMAT (' ILLEGAL SOURCE UNIT CODE = ',I6)  
      IFLG = 3   
      RETURN    
  060 IF (IODEF(3).GE.0 .AND. IODEF(3).LE.MAXUNT) GO TO 080  
CCCCC      IF (IPFILE .NE. 0) WRITE (IPFILE,2030) IODEF(3)  
CCCKW  2030 FORMAT (' ILLEGAL TARGET UNIT CODE = ',I6)   
      IFLG = 4    
      RETURN   
c
* CHECK CONSISTANCY BETEEN UNITS OF MEASURE AND REFERENCE SYSTEM
c
  080 IUNIT = SYSUNT(INDEF(1) + 1) 
      DO 100 I = 1,2
      CALL UNITZ0 (CRDIN(I),INDEF(3),COORD(I),IUNIT,IPFILE,IFLG)
      IF (IFLG .NE. 0) RETURN    
  100 CONTINUE    
      IUNIT = SYSUNT(IODEF(1) + 1)   
      CALL UNITZ0 (ZERO,IUNIT,CRDIO,IODEF(3),IPFILE,IFLG)  
      IF (IFLG .NE. 0) RETURN   
      IF (INDEF(1).NE.IODEF(1).OR.INDEF(2).NE.IODEF(2)) GO TO 140
      DO 120 I = 1,2   
      CALL UNITZ0 (CRDIN(I),INDEF(3),CRDIO(I),IODEF(3),IPFILE,IFLG)
      IF (IFLG .NE. 0) RETURN   
  120 CONTINUE   
      RETURN    
c
* COMPUTE TRANSFORMED COORDINATES AND ADJUST THEIR UNITS.  
c
  140 IF (INDEF(1) .EQ. 0) GO TO 520    
      IF (INDEF(2).GT.60 .OR. INDEF(1).EQ.1) GO TO 200  
CCCCC      IF (IPFILE .NE. 0) WRITE (IPFILE,2040) INDEF(2) 
CCCKW  2040 FORMAT (' ILLEGAL SOURCE ZONE NUMBER = ',I6)   
      IFLG = 5  
      RETURN    
C ******************************************************************
c
*                      INVERSE TRANSFORMATION. 
c
C ******************************************************************
C
C
 200   continue
c
       if (mapprj.eq.17) goto 217
       if (mapprj.eq.18) goto 218
       IF (MAPPRJ.EQ.25) GOTO 218
              GOTO 218
c
C
  217 CALL IS17Z0 (INDEF(2),TPARIN,IPFILE,IFLG)  
      IF (IFLG .NE. 0) GO TO 500     
      CALL PI17Z0 (COORD,CRDIO,IPFILE,IFLG)     
      GO TO 500   
c
  218 CALL IS18Z0 (INDEF(2),TPARIN,IPFILE,IFLG)  
      if (iflg.ne.0) goto 500
      CALL PI18Z0 (COORD,CRDIO,IPFILE,IFLG)
      GOTO 500
C
  500 IF (IFLG .NE. 0) RETURN   
      IF (IODEF(1) .EQ. 0) GO TO 920  
      COORD(1) = CRDIO(1)   
      COORD(2) = CRDIO(2)  
  520 IF (IODEF(2).GT.60 .OR. IODEF(1).EQ.1) GO TO 540   
CCCCC      IF (IPFILE .NE. 0) WRITE (IPFILE,2050) IODEF(2)  
CCCKW  2050 FORMAT (' ILLEGAL TARGET ZONE NUMBER = ',I6)    
      IFLG = 6    
      RETURN     
c ******************************************************************
c
*                 FORWARD TRANSFORMATION.  
*  
c ******************************************************************
C
 540   CONTINUE
c
       IF (MAPPRJ.EQ.17) GOTO 617
       IF (MAPPRJ.EQ.18) GOTO 618
       IF (MAPPRJ.EQ.25) GOTO 618
             GOTO 618
C
  617 CALL IS17Z0 (IODEF(2),TPARIO,IPFILE,IFLG) 
      IF (IFLG .NE. 0) GO TO 900 
      CALL PF17Z0 (COORD,CRDIO,IPFILE,IFLG) 
      GO TO 900 
c
  618 CALL IS18Z0 (IODEF(2),TPARIO,IPFILE,IFLG) 
      IF (IFLG .NE. 0) GOTO 900
      CALL PF18Z0 (COORD,CRDIO,IPFILE,IFLG)
      GOTO 900
C
  900 IF (IFLG .NE. 0) RETURN 
  920 DO 940 I = 1,2 
      CALL UNITZ0 (CRDIO(I),IUNIT,CRDIO(I),IODEF(3),IPFILE,IFLG)  
      IF (IFLG .NE. 0) RETURN  
  940 CONTINUE 
*   
      RETURN 
      END 

* *****************************************************************
C
      BLOCK DATA    
C
* INITIALIZATION OF ELLIPSOID TO CLARK'S 1866 PARAMETERS.
C
      IMPLICIT REAL (A-Z)   
C
      COMMON /ELLPZ0/ AZ,EZ,ESZ,E0Z,E1Z,E2Z,E3Z,E4Z 
      COMMON /SPHRZ0/ AZZ    
C
      DATA AZ  /0.637820E+07/
      DATA EZ  /0.8227185E-01/
      DATA ESZ /0.6768657E-02/
      DATA E0Z /0.99830568/
      DATA E1Z /0.2542555E-02/
      DATA E2Z /0.2698084E-05/
      DATA E3Z /0.3533088E-07/
      DATA E4Z /0.1003393E+01/
      DATA AZZ /0.6370990E+07/
C
C
      END    
C ******************************************************************
C
      SUBROUTINE DMSLZ0 (ANG,IUNIT,DMS,IPFILE,IFLG) 
* 
* SUBROUTINE TO CONVERT ANGLE TO DISPLAY DMS 
*  
      IMPLICIT REAL (A-H,O-Z) 
      CHARACTER*16 DMS 
      CHARACTER*1 SIGN    
      INTEGER*4 ISEC,IDEG,IMIN   
      DATA ZERO /0.0/ 
*  
* DETERMINE THE SIGN OF ANGLE   
*   
      IFLG = 0   
      SIGN = ' ' 
      IF (ANG .LT. ZERO) SIGN = '-' 
*  
* CONVERT ANGLE TO SECONDS OF ARC                                       GCTP0086
*                                                                       GCTP0087
      IOUNT = 3                                                         GCTP0088
      SEC = ABS(ANG)                                                    GCTP0089
      CALL UNITZ0 (SEC,IUNIT,SEC,IOUNT,IPFILE,IFLG)                     GCTP0090
      IF (IFLG .NE. 0) RETURN                                           GCTP0091
      SEC = SEC - MOD (SEC,0.0001) 
      ISEC = SEC                                                        GCTP0093
*                                                                       GCTP0094
* COMPUTE DEGREES, MINUTES,AND SECONDS PARTS OF ANGLE                   GCTP0095
*                                                                       GCTP0096
      IDEG = ISEC / 3600                                                GCTP0097
      ISEC = MOD (ISEC,3600)                                            GCTP0098
      IMIN = ISEC / 60                                                  GCTP0099
      ISEC = MOD (ISEC,60)                                              GCTP0100
      SEC = SEC - IDEG * 3600 - IMIN * 60 - ISEC                        GCTP0101
*                                                                       GCTP0102
* FORM DMS CHARACTER FIELD                                              GCTP0103
*                                                                       GCTP0104
CCCKW      WRITE (DMS,1000) SIGN,IDEG,IMIN,ISEC,SEC                          GCTP0105
CCCKW  1000 FORMAT (1X,A1,I3.3,2(I3.2),F5.4)                                  GCTP0106
*                                                                       GCTP0107
      RETURN                                                            GCTP0108
      END                                                               GCTP0109
C
C  ********************************************************************
C
      SUBROUTINE UNITZ0 (PARIN,INUNIT,PARIO,IOUNIT,IPFILE,IFLG)         GCTP3577
*                                                                       GCTP3578
* SUBROUTINE TO TRANSFORM UNITS OF MEASURE FOR A PARAMETER              GCTP3579
*                                                                       GCTP3580
* INPUT ......                                                          GCTP3581
* INUNIT * UNIT CODE OF SOURCE                                          GCTP3582
* PARIN  * SOURCE PARAMETER                                             GCTP3583
* IOUNIT * UNIT CODE OF TARGET                                          GCTP3584
* IPFILE * LOGICAL NUMBER OF MESSAGES FILE.                             GCTP3585
*                                                                       GCTP3586
* OUTPUT .....                                                          GCTP3587
* PARIO  * TARGET PARAMETER                                             GCTP3588
* IFLG   * RETURN FLAG                                                  GCTP3589
*                                                                       GCTP3590
      IMPLICIT REAL (A-H,O-Z) 
*                                                                       GCTP3592
      IFLG = 0                                                          GCTP3593
      IF (INUNIT .EQ. IOUNIT) THEN                                      GCTP3594
      PARIO = PARIN                                                     GCTP3595
      RETURN                                                            GCTP3596
      ENDIF                                                             GCTP3597
*                                                                       GCTP3598
* ADJUST FOR PACKED DMS UNITS                                           GCTP3599
*                                                                       GCTP3600
      IF (INUNIT .EQ. 5) THEN                                           GCTP3601
       INUNT = 3                                                        GCTP3602
       CIN = PAKSZ0 (PARIN,IPFILE,IFLG)                                 GCTP3603
       IF (IFLG .NE. 0) RETURN                                          GCTP3604
       ELSE                                                             GCTP3605
       INUNT = INUNIT                                                   GCTP3606
       CIN = PARIN                                                      GCTP3607
       ENDIF                                                            GCTP3608
      IF (IOUNIT .EQ. 5) THEN                                           GCTP3609
       IOUNT = 3                                                        GCTP3610
       ELSE                                                             GCTP3611
       IOUNT = IOUNIT                                                   GCTP3612
       ENDIF                                                            GCTP3613
*                                                                       GCTP3614
* COMPUTE TRANSFORMATION FACTOR                                         GCTP3615
*                                                                       GCTP3616
       CALL UNTFZ0 (INUNT,IOUNT,FACTOR,IPFILE,IFLG)                     GCTP3617
       IF (IFLG .NE. 0) RETURN                                          GCTP3618
       CIO = FACTOR * CIN                                               GCTP3619
*                                                                       GCTP3620
* ADJUST OUTPUT FOR DMS UNITS                                           GCTP3621
*                                                                       GCTP3622
       IF (IOUNIT .EQ. 5) THEN                                          GCTP3623
        PARIO = SPAKZ0 (CIO,IPFILE,IFLG)                                GCTP3624
        IF (IFLG .NE. 0) RETURN                                         GCTP3625
        ELSE                                                            GCTP3626
        PARIO = CIO                                                     GCTP3627
        ENDIF                                                           GCTP3628
*                                                                       GCTP3629
        RETURN                                                          GCTP3630
        END                                                             GCTP3631
C
* **********************************************************************GCTP3636
C
      SUBROUTINE UNTFZ0 (INUNIT,IOUNIT,FACTOR,IPFILE,IFLG)              GCTP3637
*                                                                       GCTP3638
* SUBROUTINE TO DETERMINE CONVERGENCE FACTOR BETWEEN TWO LINEAL UNITS   GCTP3639
*                                                                       GCTP3640
* * INPUT ........                                                      GCTP3641
* * INUNIT * UNIT CODE OF SOURCE.                                       GCTP3642
* * IOUNIT * UNIT CODE OF TARGET.                                       GCTP3643
* * IPFILE * LOGICAL NUMBER OF MESSAGES FILE.                           GCTP3644
*                                                                       GCTP3645
* * OUTPUT .......                                                      GCTP3646
* * FACTOR * CONVERGENCE FACTOR FROM SOURCE TO TARGET.                  GCTP3647
* * IFLG   * RETURN FLAG = 0 , NORMAL RETURN.                           GCTP3648
*                        = I , ABNORMAL RETURN.                         GCTP3649
*                                                                       GCTP3650
      IMPLICIT REAL (A-H,O-Z) 
      DIMENSION FACTRS(5,5) 
      DATA ZERO,MAXUNT /0.0,5/ 
      DATA FACTRS /0.10E01 , 0.00 ,
     .             0.0 , 0.2062648062470963E06 , 
     .             0.5729577951308231E02 ,  
     .             0.0 , 0.10E01 , 
     .             0.3048006096012192 , 0.0 ,
     .             0.0 ,
     .             0.0 , 0.32808333E01 ,
     .             0.10E01 , 0.0 , 
     .             0.00 , 
     .             0.4848136811095360E-5 , 0.0 ,
     .             0.0 , 0.10E01 ,
     .             0.2777777777777778E-3 , 
     .             0.1745329251994330E-1 , 0.0 , 
     .             0.0 , 0.360E04 ,  
     .             0.10E01 /  
*                                                     
      IF (INUNIT.LT.0 .OR. INUNIT.GE.MAXUNT) GO TO 020                  GCTP3670
      IF (IOUNIT.GE.0 .AND. IOUNIT.LT.MAXUNT) GO TO 040                 GCTP3671
  020 CONTINUE 
CCCKW      IF (IPFILE .NE. 0) WRITE (IPFILE,2000) INUNIT,IOUNIT  
CCCKW  2000 FORMAT (' ILLEGAL SOURCE OR TARGET UNIT CODE = ',I6,' / ',I6)     GCTP3673
      IFLG = 11                                                         GCTP3674
      RETURN                                                            GCTP3675
  040 FACTOR = FACTRS(IOUNIT+1 , INUNIT+1)                              GCTP3676
      IF (FACTOR .NE. ZERO) GO TO 060                                   GCTP3677
CCCCC      IF (IPFILE .NE. 0) WRITE (IPFILE,2010) INUNIT,IOUNIT              GCTP3678
CCCKW  2010 FORMAT (' INCONSISTANT UNIT CODES = ',I6,' / ',I6)                GCTP3679
      IFLG = 12                                                         GCTP3680
      RETURN                                                            GCTP3681
  060 IFLG = 0                                                          GCTP3682
      RETURN                                                            GCTP3683
*                                                                       GCTP3684
      END                                                               GCTP36
C
C
C ******************************************************************
C ******************************************************************
      FUNCTION ADJLZ0 (LON)   
C
* FUNCTION TO ADJUST LONGITUDE ANGLE TO MODULE 180 DEGREES.  
C
      IMPLICIT REAL (A-Z) 
      DATA TWO,PI /2.0,3.1415926/
C
  020 ADJLZ0 = LON    
      IF (ABS(LON) .LE. PI) RETURN  
      TWOPI = TWO * PI    
      LON = LON - SIGN (TWOPI,LON) 
      GO TO 020   
C
      END   
C
C ******************************************************************
C ******************************************************************
C ******************************************************************
      FUNCTION PAKSZ0 (ANG,IPFILE,IFLG)   
C
* FUNCTION TO CONVERT DMS PACKED ANGLE INTO SECONDS OF ARC.             GCTP0532
C
      IMPLICIT REAL (A-H,M-Z)  
      DIMENSION CODE(2)                                                 GCTP0535
      DATA CODE /10000.0,100.0/ 
      DATA ZERO,ONE /0.0,1.0/ 
      DATA C1,C2 /3600.0,60.0/ 
*                                                                       GCTP0539
* SEPERATE DEGREE FIELD.                                                GCTP0540
*                                                                       GCTP0541
      IFLG = 0                                                          GCTP0542
      FACTOR = ONE                                                      GCTP0543
      IF (ANG .LT. ZERO) FACTOR = - ONE                                 GCTP0544
      SEC = ABS(ANG) 
      TMP = CODE(1)                                                     GCTP0546
      I = SEC / TMP                                                     GCTP0547
      IF (I .GT. 360) GO TO 020                                         GCTP0548
      DEG = I                                                           GCTP0549
*                                                                       GCTP0550
* SEPERATE MINUTES FIELD.                                               GCTP0551
*                                                                       GCTP0552
      SEC = SEC - DEG * TMP                                             GCTP0553
      TMP = CODE(2)                                                     GCTP0554
      I = SEC / TMP                                                     GCTP0555
      IF (I .GT. 60) GO TO 020                                          GCTP0556
      MIN = I                                                           GCTP0557
*                                                                       GCTP0558
* SEPERATE SECONDS FIELD.                                               GCTP0559
*                                                                       GCTP0560
      SEC = SEC - MIN * TMP                                             GCTP0561
      IF (SEC .GT. C2) GO TO 020                                        GCTP0562
      SEC = FACTOR * (DEG * C1 + MIN * C2 + SEC)                        GCTP0563
      GO TO 040                                                         GCTP0564
*                                                                       GCTP0565
* ERROR DETECTED IN DMS FORM.                                           GCTP0566
*                                                                       GCTP0567
  020 CONTINUE
CCCCC      IF (IPFILE .NE. 0) WRITE (IPFILE,2000) ANG   
CCCKW  2000 FORMAT (' ILLEGAL PACKED DMS FIELD = ',F15.3)                     GCTP0569
      IFLG = 10                                                         GCTP0570
      RETURN                                                            GCTP0571
*                                                                       GCTP0572
  040 PAKSZ0 = SEC                                                      GCTP0573
*                                                                       GCTP0574
      RETURN                                                            GCTP0575
      END                                                               GCTP0576
* **********************************************************************GCTP0578
C
C *****************************************************************
      FUNCTION SPAKZ0 (ANG,IPFILE,IFLG)   
*                                                                       GCTP3510
* FUNCTION TO CONVERT SECONDS OF ARC TO PACKED DMS                      GCTP3511
*                                                                       GCTP3512
      IMPLICIT REAL (A-H,M-Z) 
      DIMENSION CODE(2)                                                 GCTP3514
      DATA CODE /10000.0,100.0/ 
      DATA ZERO,ONE /0.0,1.0/ 
      DATA C1,C2 /3600.0,60.0/ 
*                                                                       GCTP3518
* SEPERATE DEGREE FIELD.                                                GCTP3519
*                                                                       GCTP3520
      IFLG = 0                                                          GCTP3521
      FACTOR = ONE                                                      GCTP3522
      IF (ANG .LT. ZERO) FACTOR = - ONE                                 GCTP3523
      SEC = ABS(ANG) 
      I = SEC / C1                                                      GCTP3525
      IF (I .GT. 360) GO TO 020                                         GCTP3526
      DEG = I                                                           GCTP3527
*                                                                       GCTP3528
* SEPERATE MINUTES FIELD.                                               GCTP3529
*                                                                       GCTP3530
      SEC = SEC - DEG * C1                                              GCTP3531
      I = SEC / C2                                                      GCTP3532
      MIN = I                                                           GCTP3533
*                                                                       GCTP3534
* SEPERATE SECONDS FIELD.                                               GCTP3535
*                                                                       GCTP3536
      SEC = SEC - MIN * C2                                              GCTP3537
      SEC = FACTOR * (DEG * CODE(1) + MIN * CODE(2) + SEC)              GCTP3538
      GO TO 040                                                         GCTP3539
*                                                                       GCTP3540
* ERROR DETECTED IN DMS FORM.                                           GCTP3541
*                                                                       GCTP3542
  020 CONTINUE
CCCCC      IF (IPFILE .NE. 0) WRITE (IPFILE,2000) ANG    
CCCKW  2000 FORMAT (' ANGLE G.T. 360 DEGREES = ',F15.3)                       GCTP3544
      IFLG = 13                                                         GCTP3545
      RETURN                                                            GCTP3546
*                                                                       GCTP3547
  040 SPAKZ0 = SEC                                                      GCTP3548
*                                                                       GCTP3549
      RETURN                                                            GCTP3550
      END                                                               GCTP3551
