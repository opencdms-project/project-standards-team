C
C **********************************************************************
C                        *  EQUIRECTANGULAR   *                        
C **********************************************************************
C 
      SUBROUTINE PJ17Z0 
C 
      IMPLICIT REAL (A-Z) 
      INTEGER*4 SWITCH,I,ZONE,IPFILE,IFLG
      CHARACTER*16 ANGS(2) 
      COMMON /SPHRZ0/ AZZ 
      COMMON /SWTCH/ SWITCH 
      DIMENSION DATA(1),GEOG(1),PROJ(1) 
      DATA HALFPI /1.57079632679489661923/ 
      DATA ZERO   /0.0/ 
C **** PARAMETERS **** A,LON0,X0,Y0,LAT1 *******************************
      DATA  A,LON0,X0,Y0,LAT1 /0.0,0.0,0.0,0.0,0.0/ 
CCCC      DATA SWITCH /0/  
C
C ......................................................................
C      .  INITIALIZATION OF PROJECTION PARAMETERS (ENTRY INPUT)  .      
C ......................................................................
C  
      ENTRY IS17Z0 (ZONE,DATA,IPFILE,IFLG) 
C   
      IFLG = 0   
      IF (SWITCH.NE.0 .AND. SWITCH.EQ.ZONE) RETURN  
      A = DATA(1) 
      IF (A .LE. ZERO) A = AZZ                  
      CALL UNITZ0 (DATA(5),5,LON0,0,IPFILE,IFLG)
      IF (IFLG .NE. 0) RETURN  
      CALL UNITZ0 (DATA(6),5,LAT1,0,IPFILE,IFLG) 
      IF (IFLG .NE. 0) RETURN 
      X0 = DATA(7) 
      Y0 = DATA(8) 
C 
C LIST RESULTS OF PARAMETER INITIALIZATION. 
C 
      CALL DMSLZ0 (LAT1,0,ANGS(1),IPFILE,IFLG)   
      CALL DMSLZ0 (LON0,0,ANGS(2),IPFILE,IFLG)
ccccc     IF (IPFILE .NE. 0) WRITE (IPFILE,2000) A,ANGS,X0,Y0 
CCCKW 2000 FORMAT (' INITIALIZATION PARAMETERS (EQUIRECTANGULAR PROJECTION)'/
CCCKW     .        ' RADIUS OF SPHERE             =',F16.4,' METERS'/ 
CCCKW     .        ' LATITUDE OF TRUE SCALE       =',A16/                  
CCCKW     .        ' LONGITUDE OF C. MERIDIAN     =',A16/                  
CCCKW     .        ' FALSE EASTING                =',F16.4,' METERS'/     
CCCKW     .        ' FALSE NORTHING               =',F16.4,' METERS')       
      SWITCH = ZONE                                                   
      RETURN                                                         
C                                                                    
C ......................................................................
C                      .  FORWARD TRANSFORMATION  .                   
C ......................................................................
C                                                        
      ENTRY PF17Z0 (GEOG,PROJ,IPFILE,IFLG) 
C   
      IFLG = 0               
      IF (SWITCH .NE. 0) GO TO 120
CCCC      IF (IPFILE .NE. 0) WRITE (IPFILE,2010)
CCCKW 2010 FORMAT (' UNINITIALIZED TRANSFORMATION')
      IFLG = 1700  
      RETURN   
  120 LON = ADJLZ0 (GEOG(1) - LON0)  
      PROJ(1) = X0 + A * LON * COS(LAT1) 
      PROJ(2) = Y0 + A * GEOG(2) 
      RETURN 
C 
C ......................................................................
C                      .  INVERSE TRANSFORMATION  .                     
C ......................................................................
C   
      ENTRY PI17Z0 (PROJ,GEOG,IPFILE,IFLG)
C 
      IFLG = 0  
      IF (SWITCH .NE. 0) GO TO 220  
CCCC      IF (IPFILE .NE. 0) WRITE (IPFILE,2010)   
      IFLG = 1700 
      RETURN                                                           
  220 X = PROJ(1) - X0                                               
      Y = PROJ(2) - Y0                                                 
      GEOG(2) = Y / A                         
      IF (ABS(GEOG(2)) .LE. HALFPI) GO TO 240   
CCCC      IF (IPFILE .NE. 0) WRITE (IPFILE,2020) 
CCCC 2020 FORMAT (' IMPROPER PARAMETER')                                 
      IFLG = 1701                                                   
      RETURN                                                      
  240 GEOG(1) = ADJLZ0 (LON0 + X / (A * COS(LAT1) ))   
      RETURN                                                        
      END                                                             
