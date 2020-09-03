C
*                       *  MILLER CYLINDRICAL  *    
C
C
      SUBROUTINE PJ18Z0      
C
      IMPLICIT REAL (A-Z)    
      INTEGER*4 SWITCH,I,ZONE,IPFILE,IFLG   
      CHARACTER*16 ANGS   
      COMMON /SPHRZ0/ AZZ   
      COMMON /SWTCH/ SWITCH   
      DIMENSION DATA(1),GEOG(1),PROJ(1)  
      DATA FORTPI /0.78539816339744833/
      DATA ZERO,ONEQ,TWOH /0.0,1.25,2.5/                          
* **** PARAMETERS **** A,LON0,X0,Y0 *******************************
      DATA  A,LON0,X0,Y0 /0.0,0.0,0.0,0.0/ 
CCCC      DATA SWITCH /0/ 
C
C
*      .  INITIALIZATION OF PROJECTION PARAMETERS (ENTRY INPUT)  .
C ..................................................................
*                                                                       
      ENTRY IS18Z0 (ZONE,DATA,IPFILE,IFLG)                              
*                                                                       
      IFLG = 0                                                          
      IF (SWITCH.NE.0 .AND. SWITCH.EQ.ZONE) RETURN                      
      A = DATA(1)                                                       
      IF (A .LE. ZERO) A = AZZ                                          
      CALL UNITZ0 (DATA(5),5,LON0,0,IPFILE,IFLG)                        
      IF (IFLG .NE. 0) RETURN                                           
      X0 = DATA(7)                                                      
      Y0 = DATA(8)    
C
* LIST RESULTS OF PARAMETER INITIALIZATION. 
C
      CALL DMSLZ0 (LON0,0,ANGS,IPFILE,IFLG)   
CCCC      IF (IPFILE .NE. 0) WRITE (IPFILE,2000) A,ANGS,X0,Y0 
CCCKW 2000 FORMAT (' INITIALIZATION PARAMETERS (MILLER CYLINDRICAL',
CCCKW     .        ' PROJECTION)'/  
CCCKW     .        ' RADIUS OF SPHERE             =',F16.4,' METERS'/
CCCKW     .        ' LONGITUDE OF C. MERIDIAN     =',A16/ 
CCCKW     .        ' FALSE EASTING                =',F16.4,' METERS'/
CCCKW     .        ' FALSE NORTHING               =',F16.4,' METERS')
      SWITCH = ZONE    
      RETURN   
C
C
*                      .  FORWARD TRANSFORMATION  .  
C .................................................................
C
      ENTRY PF18Z0 (GEOG,PROJ,IPFILE,IFLG)   
C
      IFLG = 0    
      IF (SWITCH .NE. 0) GO TO 120   
CCC   IF (IPFILE .NE. 0) WRITE (IPFILE,2010)   
CCCKW 2010 FORMAT (' UNINITIALIZED TRANSFORMATION') 
      IFLG = 1800   
      RETURN    
  120 LON = ADJLZ0 (GEOG(1) - LON0)  
      PROJ(1) = X0 + A * LON  
      PROJ(2) = Y0 + A *  LOG ( TAN (FORTPI + GEOG(2) / TWOH))*ONEQ
      RETURN     
C
C ..................................................................
C                      .  INVERSE TRANSFORMATION  .   
C ..................................................................
C
      ENTRY PI18Z0 (PROJ,GEOG,IPFILE,IFLG)  
C
      IFLG = 0    
      IF (SWITCH .NE. 0) GO TO 220 
C     IF (IPFILE .NE. 0) WRITE (IPFILE,2010)   
      IFLG = 1800 
      RETURN   
  220 X = PROJ(1) - X0   
      Y = PROJ(2) - Y0   
      GEOG(1) = ADJLZ0 (LON0 + X / A) 
      GEOG(2) = TWOH *  ATAN ( EXP (Y / A / ONEQ)) - FORTPI * TWOH
      RETURN   
C
      END    
