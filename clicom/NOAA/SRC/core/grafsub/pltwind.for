$STORAGE:2
      SUBROUTINE PLTWIND(PRES,DIR,SPD,NLVLS)
C------------------------------------------------------------------------------
C     PLOT THE WIND BARBS ALONG THE RIGHT SIDE OF SKEW-T, LOG P DIAGRAM
C
C     INPUT ARGUMENTS:
C
C     PRES  REAL ARRAY  PRESSURE LEVEL (MB ABOVE THE SURFACE)
C     DIR   REAL ARRAY  WIND DIRECTION (DEG FROM NORTH)
C     SPD   REAL ARRAY  WIND SPEED (DESIRED SPEED UNITS)
C     NLVLS INT2        NUMBER OF WIND LEVELS TO DRAW
C
C     OUTPUT ARGUMENTS:  NONE
C------------------------------------------------------------------------------
      REAL PRES(1), DIR(1), SPD(1)
C
C     LOCAL VARIABLES
C
C     X,Y   REAL        X,Y PLOTTING COORDINATE FOR HEAD OF WIND BARB.
C     DU    REAL        SCALING FACTOR FOR SIZE OF BARBS.
C     ROT   REAL        ROTATION FACTOR (0 MEANS NORTH IS VERTICAL).
C
      REAL DR, DR1, SIND, SIND1, COSD, COSD1, STAFF, BARB, SP, ADD
      REAL X, Y, X1, Y1, X2, Y2, X3, Y3, DU, ROT 
      INTEGER*2 N10, N50, NLVLS, I
C
C     CONSTANTS
C
      PARAMETER (RPD=0.0174533)
      ROT=0.
      DU =1.25
      X  =24.8
      DO 800 I=1,NLVLS
         IF (DIR(I).GT.360. .OR. SPD(I).GT.220.) GO TO 800
         IF (DIR(I).LT.  0. .OR. SPD(I).LT.  0. .OR. 
     +      PRES(I).LE.  0.) GO TO 800
         Y=SKEWTY(PRES(I))
C
C---- SET UP NECESSARY ANGLES FOR PLOTTING BARB ALONG STAFF
C
         DR   = DIR(I)*RPD+ROT
         DR1  = (DIR(I)+60.)*RPD+ROT
         SIND = SIN(DR)
         SIND1= SIN(DR1)
         COSD = COS(DR)
         COSD1= COS(DR1)
C
C----SET UP BARB LENGTHS
C
         STAFF= DU*1.4
         BARB = STAFF*0.9
         ADD  = STAFF*0.9
C
C----COUNTERS FOR WIND STAFF LINES/FLAGS
C
         N50  = 0
         N10  = 0
C
C----IF WIND IS CALM, PLOT A '0' AND EXIT
C
         IF (SPD(I).LT.2.5) THEN
            CALL DRWTXT('0',X,Y,0,0.0)
            GO TO 800
         ENDIF
C
C----ADD 2.5 TO ROUND SPEED FOR CONVENTION OF WIND BARB PLOTTING
C
         SP = SPD(I)+2.5
C
C----COUNT HOW MANY FLAGS ARE NEEDED. ONE FLAG IS 50 UNITS OF SPEED
C
         DO 100 II=1,5
            IF (SP.LT.50.)  GO TO 101
            N50=N50+1
            SP =SP-50
  100    CONTINUE
  101    CONTINUE
C
C----COUNT HOW MANY BARBS ARE NEEDED. ONE BARB IS 10 UNITS OF SPEED
C
         DO 200 II=1,5
            IF (SP.LT.10.)  GO TO 201
            N10=N10+1
            SP =SP-10
  200    CONTINUE
  201    CONTINUE
C
C--- DRAW WIND STAFF
C
         X1 = X
         Y1 = Y
         X2 = X1+SIND*STAFF
         Y2 = Y1+COSD*STAFF
         CALL SETLNW(3)
         CALL MOVABS(X1,Y1)
         CALL LNABS (X2,Y2)
C
C----PLOT HALF BARB IF NECESSARY
C
         IF (SP.GE.5.) THEN
            X1=X2+SIND1*(BARB/2.)
            Y1=Y2+COSD1*(BARB/2.)
            CALL MOVABS(X1,Y1)
            CALL LNABS (X2,Y2)
            IF (N50.NE.0 .OR. N10.NE.0)  GO TO 240
            X1=X2+SIND*BARB
            Y1=Y2+COSD*BARB
            CALL MOVABS(X1,Y1)
            CALL LNABS (X2,Y2)
            GO TO 800
         ENDIF
  240    X1=X2
         Y1=Y2
C 
C----PLOT BARBS IF NECESSARY
C
         DO 300 II=1,N10
            X2=X1+SIND*BARB
            Y2=Y1+COSD*BARB
            X3=X2+SIND1*BARB
            Y3=Y2+COSD1*BARB
            CALL MOVABS(X1,Y1)
            CALL LNABS (X2,Y2)
            CALL LNABS (X3,Y3)
            X1=X2
            Y1=Y2
  300    CONTINUE
C
C----PLOT FLAGS IF NECESSARY
C
         DO 400 II=1,N50
            X2=X1+SIND*ADD
            Y2=Y1+COSD*ADD
            X3=X2+SIND1*ADD
            Y3=Y2+COSD1*ADD
            CALL MOVABS(X1,Y1)
            CALL LNABS (X2,Y2)
            CALL LNABS (X3,Y3)
            CALL LNABS (X1,Y1)
            X1=X2
            Y1=Y2
  400    CONTINUE
  800 CONTINUE
      CALL SETLNW(1)
      RETURN
      END
