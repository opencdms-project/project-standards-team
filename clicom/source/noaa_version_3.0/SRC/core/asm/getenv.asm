PAGE ,132
;******************************************************************************
;			PROCEDURE GETENV
;
;	Calling Language: MICROSOFT FORTRAN
;
;	Parameters: CHARACTER*1 BUFF(400)
;
;	Entry Conditions: None.
;
;	DESCRIPTION: THIS ROUTINE USES DOS FUNCTION #98 TO RETREIVE THE 500
;	 	     BYTE DOS ENVIROMENT TABLE FOR A FORTRAN CALLING PROGRAM.
;		     BUFF IS THE ADDRESS OF A 500 BYTE AREA.  IT IS UP TO THE
;		     CALLING PROGRAM TO SEPARATE OUT THE STRINGS BASED ON THE
;		     CHR$(0) SEPARATOR.  THE END OF ALL STRINGS SHOULD HAVE TWO
;		     CONSECUTIVE CHR$(0).
; 
;	      ****THIS ROUTINE WILL NOT WORK ON DOS SYSTEMS BELOW 3.1****
;******************************************************************************

DATA      SEGMENT PUBLIC 'DATA'
DATA      ENDS
DGROUP    GROUP   DATA

STACKF	STRUC				;Define Stack Addresses
   SAVEBP	DW ?
   RTNADR	DD ?
   BUFF		DD ?
STACKF	ENDS

CODE        SEGMENT   'CODE'
            ASSUME CS:CODE,DS:DGROUP,SS:DGROUP
	    
PUBLIC      GETENV
GETENV      PROC FAR

            PUSH      BP  	;SAVE REGISTERS
            MOV       BP,SP
            PUSH      DS
	    PUSH      ES
	    PUSH      DI
	    PUSH      SI
	    
	    
;******************************************************************************
;		Set up for call to DOS Function #98
;******************************************************************************
	    
            MOV      CX,500   		;SET TO MOVE 500 BYTES TO USER BUFF
            MOV      AH,98
            INT      21H    	 	;GET SEGMENT ADDRESS OF PSP
	    
;******************************************************************************
;		Pass table back to the calling program
;******************************************************************************
	    
            MOV      ES,BX
            MOV      DI,44  		;ADDRESS OF ENVIR TABLE IN  BYTES
            MOV      AX,ES:[DI] 	;44 AND 45 OF PSP
            MOV      SI,0
            LES      DX,DWORD PTR [BP].BUFF	 ;POINT TO CALLER BUFF
            MOV      DI,DX
            MOV      DS,AX
            CLD
REP         MOVSB    			;MOVE 400 BYTE ENV TABLE TO USER BUFFER

;******************************************************************************

EXIT:       POP	      SI 		;RESTORE REGISTERS
            POP       DI
	    POP       ES
	    POP       DS
	    POP       BP
	    
	    
	    
            RET       04H 		;RETURN TO CALLING PROGRAM
GETENV      ENDP
CODE        ENDS
END
